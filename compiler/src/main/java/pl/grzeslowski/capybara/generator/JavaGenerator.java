package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.generator.java.*;
import pl.grzeslowski.capybara.linker.LinkedModule;
import pl.grzeslowski.capybara.linker.LinkedProgram;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.lang.String.join;
import static java.util.stream.Collectors.joining;
import static pl.grzeslowski.capybara.generator.java.JavaExpressionEvaluator.evaluateExpression;

public final class JavaGenerator implements Generator {
    private final JavaAstBuilder astBuilder = new JavaAstBuilder();
    private static final String METHOD_DECL_PREFIX = "__method__";

    @Override
    public CompiledProgram generate(LinkedProgram program) {
        var modules = program.modules().stream()
                .map(this::modules)
                .flatMap(List::stream)
                .toList();
        return new CompiledProgram(modules);
    }

    private List<CompiledModule> modules(LinkedModule module) {
        var javaClass = astBuilder.build(module);
        if (!hasTypeOrDataNameConflictWithFile(javaClass)) {
            return List.of(new CompiledModule(relativePath(module), code(javaClass)));
        }

        var ownerInterface = javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface.name().toString().equals(javaClass.name().toString()))
                .findFirst();
        if (ownerInterface.isPresent()) {
            return List.of(new CompiledModule(
                    Path.of(module.path(), javaClass.name() + ".java"),
                    codeNestedInOwnerInterface(javaClass, ownerInterface.get())
            ));
        }

        var compiled = new ArrayList<CompiledModule>();
        for (var declaration : topLevelDeclarations(javaClass)) {
            compiled.add(new CompiledModule(
                    Path.of(module.path(), declaration.name() + ".java"),
                    codeTopLevelDeclaration(javaClass, declaration.code())
            ));
        }
        if (!javaClass.staticMethods().isEmpty()) {
            var utilityClass = new JavaClass(
                    javaClass.annotations(),
                    new JavaType(javaClass.name() + "Module"),
                    javaClass.javaPackage(),
                    javaClass.staticImports(),
                    javaClass.staticMethods(),
                    java.util.Set.of(),
                    java.util.Set.of(),
                    java.util.Set.of()
            );
            compiled.add(new CompiledModule(
                    Path.of(module.path(), utilityClass.name() + ".java"),
                    code(utilityClass)
            ));
        }
        return List.copyOf(compiled);
    }

    private Path relativePath(LinkedModule module) {
        return Path.of(module.path(), module.name() + ".java");
    }

    private String code(JavaClass javaClass) {
        var code = new StringBuilder();

        // package
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");

        // imports
        javaClass.staticImports().stream()
                .sorted()
                .forEach(staticImport -> code.append("import static ").append(staticImport).append(";\n"));
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }

        // generated annotation
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        // class declaration
        code.append("public class ")
                .append(javaClass.name())
                .append("{");

        // interfaces
        code.append('\n');
        javaClass.interfaces()
                .stream()
                .map(this::mapJavaInterface)
                .forEach(code::append);

        // records
        code.append('\n');
        javaClass.records()
                .stream()
                .map(this::mapJavaRecord)
                .forEach(code::append);

        // enums
        code.append('\n');
        javaClass.enums()
                .stream()
                .map(this::mapJavaEnum)
                .forEach(code::append);

        // static methods
        code.append('\n');
        javaClass.staticMethods()
                .stream()
                .map(this::mapJavaMethod)
                .forEach(code::append);
        code.append('\n').append(unsupportedHelperMethod());

        // close object declaration
        code.append("}");

        return code.toString();
    }

    private String codeNestedInOwnerInterface(JavaClass javaClass, JavaInterface ownerInterface) {
        var code = new StringBuilder();
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");
        javaClass.staticImports().stream()
                .sorted()
                .forEach(staticImport -> code.append("import static ").append(staticImport).append(";\n"));
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        code.append(mapJavaOwnerInterfaceHeader(ownerInterface)).append("{\n");
        if (ownerInterface instanceof JavaNormalInterface normalInterface) {
            normalInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
        }
        if (ownerInterface instanceof JavaSealedInterface sealedInterface) {
            sealedInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
        }

        javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface != ownerInterface)
                .map(this::mapJavaInterface)
                .map(this::removePublicModifier)
                .forEach(code::append);
        javaClass.records().stream()
                .map(this::mapJavaRecord)
                .map(this::removePublicModifier)
                .forEach(code::append);
        javaClass.enums().stream()
                .map(this::mapJavaEnum)
                .map(this::removePublicModifier)
                .forEach(code::append);
        javaClass.staticMethods().stream()
                .map(this::mapJavaMethod)
                .forEach(code::append);
        code.append('\n').append(unsupportedHelperMethod());

        code.append("}\n");
        return code.toString();
    }

    private String removePublicModifier(String declaration) {
        if (declaration.startsWith("public ")) {
            return declaration.substring("public ".length());
        }
        return declaration;
    }

    private String mapJavaInterfaceHeader(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> "public interface " + javaNormalInterface.name() + " ";
            case JavaSealedInterface javaSealedInterface -> {
                var permits = join(", ", javaSealedInterface.permits());
                var typeParameters = javaSealedInterface.typeParameters().isEmpty()
                        ? ""
                        : javaSealedInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
                yield "public sealed interface " + javaSealedInterface.name() + typeParameters + " permits " + permits + " ";
            }
        };
    }

    private String mapJavaOwnerInterfaceHeader(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> "public interface " + javaNormalInterface.name() + " ";
            case JavaSealedInterface javaSealedInterface -> {
                var ownerName = javaSealedInterface.name().toString();
                var permits = javaSealedInterface.permits().stream()
                        .map(permit -> ownerName + "." + permit)
                        .collect(joining(", "));
                var typeParameters = javaSealedInterface.typeParameters().isEmpty()
                        ? ""
                        : javaSealedInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
                yield "public sealed interface " + ownerName + typeParameters + " permits " + permits + " ";
            }
        };
    }

    private String codeTopLevelDeclaration(JavaClass javaClass, String declarationCode) {
        var code = new StringBuilder();
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));
        code.append(declarationCode);
        return code.toString();
    }

    private boolean hasTypeOrDataNameConflictWithFile(JavaClass javaClass) {
        var fileName = javaClass.name().toString();
        return javaClass.interfaces().stream().anyMatch(javaInterface -> javaInterface.name().toString().equals(fileName))
               || javaClass.records().stream().anyMatch(record -> record.name().toString().equals(fileName))
               || javaClass.enums().stream().anyMatch(javaEnum -> javaEnum.name().toString().equals(fileName));
    }

    private List<TopLevelDeclaration> topLevelDeclarations(JavaClass javaClass) {
        var declarations = new ArrayList<TopLevelDeclaration>();
        javaClass.interfaces().forEach(javaInterface -> declarations.add(
                new TopLevelDeclaration(javaInterface.name().toString(), mapJavaInterface(javaInterface))
        ));
        javaClass.records().forEach(javaRecord -> declarations.add(
                new TopLevelDeclaration(javaRecord.name().toString(), mapJavaRecord(javaRecord))
        ));
        javaClass.enums().forEach(javaEnum -> declarations.add(
                new TopLevelDeclaration(javaEnum.name().toString(), mapJavaEnum(javaEnum))
        ));
        return List.copyOf(declarations);
    }

    private record TopLevelDeclaration(String name, String code) {
    }

    private String mapJavaRecord(JavaRecord record) {
        var fields = record.fields().stream().map(this::mapJavaRecordField).collect(joining(", "));
        var typeParameters = record.typeParameters().isEmpty()
                ? ""
                : record.typeParameters().stream().collect(joining(", ", "<", ">"));
        var implementInterfaces = record.implementInterfaces().size() > 0
                ? record.implementInterfaces().stream()
                .map(Objects::toString)
                .collect(joining(", ", " implements ", " "))
                : "";
        var staticMethods = record.staticMethods().stream()
                .map(this::mapJavaMethod)
                .collect(joining("\n"));
        var methods = record.methods().stream()
                .map(this::mapJavaRecordMethod)
                .collect(joining("\n"));
        return "public record " + record.name() + typeParameters + "(" + fields + ")" + implementInterfaces + "{"
               + staticMethods + methods + "}\n";
    }

    private String mapJavaRecordField(JavaRecord.JavaRecordField field) {
        return field.type() + " " + field.name();
    }

    private String mapJavaEnum(JavaEnum javaEnum) {
        var implementInterfaces = javaEnum.implementInterfaces().isEmpty()
                ? ""
                : javaEnum.implementInterfaces().stream().map(Objects::toString).collect(joining(", ", " implements ", " "));
        return "public enum " + javaEnum.name() + implementInterfaces + "{INSTANCE}\n";
    }


    private String mapJavaInterface(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> mapJavaNormalInterface(javaNormalInterface);
            case JavaSealedInterface javaSealedInterface -> mapJavaSealedInterface(javaSealedInterface);
        };
    }

    private String mapJavaNormalInterface(JavaNormalInterface javaInterface) {
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        return "public interface " + javaInterface.name() + " {" + methods + "}\n";
    }

    private String mapJavaSealedInterface(JavaSealedInterface javaInterface) {
        var permits = join(", ", javaInterface.permits());
        var typeParameters = javaInterface.typeParameters().isEmpty()
                ? ""
                : javaInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        return "public sealed interface " + javaInterface.name() + typeParameters + " permits " + permits + " {" + methods + "}\n";
    }

    private String mapJavaInterfaceMethod(JavaInterface.JavaInterfaceMethod method) {
        return method.returnType() + " " + method.name() + "();";
    }

    private String mapJavaMethod(JavaMethod method) {
        return mapJavaDoc(method.comments())
               + "public static " + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters())
               + "\n}\n";
    }

    private String mapJavaRecordMethod(JavaMethod method) {
        return mapJavaDoc(method.comments())
               + "public " + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters())
               + "\n}\n";
    }

    private String mapJavaDoc(List<String> comments) {
        if (comments == null || comments.isEmpty()) {
            return "";
        }
        var body = comments.stream()
                .map(line -> " * " + line)
                .collect(joining("\n"));
        return "/**\n" + body + "\n */\n";
    }

    private String mapMethodName(String name) {
        if (name.startsWith(METHOD_DECL_PREFIX)) {
            var idx = name.lastIndexOf("__");
            if (idx >= 0 && idx + 2 < name.length()) {
                return name.substring(idx + 2);
            }
        }
        return name;
    }

    private String mapFunctionParameters(List<JavaMethod.JavaFunctionParameter> parameters) {
        return parameters.stream()
                .map(this::mapFunctionParameter)
                .collect(joining(", "));
    }

    private String mapFunctionParameter(JavaMethod.JavaFunctionParameter parameter) {
        return parameter.type() + " " + parameter.generatedName();
    }

    private String unsupportedHelperMethod() {
        return "private static <T> T __capybaraUnsupported(String message) {\n"
               + "throw new java.lang.UnsupportedOperationException(message);\n"
               + "}\n";
    }
}
