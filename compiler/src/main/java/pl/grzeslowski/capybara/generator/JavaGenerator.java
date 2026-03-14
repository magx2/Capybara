package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.generator.java.*;
import pl.grzeslowski.capybara.linker.LinkedModule;
import pl.grzeslowski.capybara.linker.LinkedProgram;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

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
            return List.of(new CompiledModule(relativePath(javaClass, javaClass.name().toString()), code(javaClass)));
        }

        var ownerInterface = javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface.name().toString().equals(javaClass.name().toString()))
                .findFirst();
        if (ownerInterface.isPresent()) {
            return List.of(new CompiledModule(
                    relativePath(javaClass, javaClass.name().toString()),
                    codeNestedInOwnerInterface(javaClass, ownerInterface.get())
            ));
        }

        var compiled = new ArrayList<CompiledModule>();
        for (var declaration : topLevelDeclarations(javaClass)) {
            compiled.add(new CompiledModule(
                    relativePath(javaClass, declaration.name()),
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
                    relativePath(javaClass, utilityClass.name().toString()),
                    code(utilityClass)
            ));
        }
        return List.copyOf(compiled);
    }

    private Path relativePath(JavaClass javaClass, String simpleName) {
        var packageName = javaClass.javaPackage().toString();
        if (packageName.isBlank()) {
            return Path.of(simpleName + ".java");
        }
        return Path.of(packageName.replace('.', '/'), simpleName + ".java");
    }

    private String code(JavaClass javaClass) {
        var code = new StringBuilder();

        // package
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");

        // imports
        appendImports(code, javaClass.staticImports());
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
        appendImports(code, javaClass.staticImports());
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        code.append(mapJavaOwnerInterfaceHeader(ownerInterface)).append("{\n");
        if (ownerInterface instanceof JavaNormalInterface normalInterface) {
            normalInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
            normalInterface.defaultMethods().stream()
                    .map(this::mapJavaInterfaceDefaultMethod)
                    .forEach(code::append);
        }
        if (ownerInterface instanceof JavaSealedInterface sealedInterface) {
            sealedInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
            sealedInterface.defaultMethods().stream()
                    .map(this::mapJavaInterfaceDefaultMethod)
                    .forEach(code::append);
        }

        javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface != ownerInterface)
                .map(this::mapJavaInterface)
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.records().stream()
                .map(this::mapJavaRecord)
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.enums().stream()
                .map(this::mapJavaEnum)
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.staticMethods().stream()
                .map(this::mapJavaMethod)
                .forEach(code::append);
        code.append('\n').append(unsupportedHelperMethod());

        code.append("}\n");
        return code.toString();
    }

    private String removeVisibilityModifier(String declaration) {
        if (declaration.startsWith("public ")) {
            return declaration.substring("public ".length());
        }
        if (declaration.startsWith("private ")) {
            return declaration.substring("private ".length());
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

    private void appendImports(StringBuilder code, Set<String> staticImports) {
        var classImports = staticImports.stream()
                .map(this::extractClassNameFromStaticImport)
                .filter(className -> !className.isBlank())
                .distinct()
                .sorted()
                .toList();
        classImports.forEach(classImport -> code.append("import ").append(classImport).append(";\n"));
        staticImports.stream()
                .sorted()
                .forEach(staticImport -> code.append("import static ").append(staticImport).append(";\n"));
    }

    private String extractClassNameFromStaticImport(String staticImport) {
        if (staticImport.endsWith(".*")) {
            return staticImport.substring(0, staticImport.length() - 2);
        }
        var lastDot = staticImport.lastIndexOf('.');
        if (lastDot <= 0) {
            return "";
        }
        return staticImport.substring(0, lastDot);
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
        var toStringMethod = mapJavaRecordToString(record);
        var visibility = record.isPrivate() ? "private" : "public";
        return visibility + " record " + record.name() + typeParameters + "(" + fields + ")" + implementInterfaces + "{"
               + staticMethods + methods + toStringMethod + "}\n";
    }

    private String mapJavaRecordField(JavaRecord.JavaRecordField field) {
        return field.type() + " " + field.name();
    }

    private String mapJavaRecordToString(JavaRecord record) {
        if (record.fields().isEmpty()) {
            return "@Override public java.lang.String toString() { return \"" + record.name() + " { }\"; }\n";
        }
        var body = new StringBuilder();
        body.append("@Override public java.lang.String toString() { return \"")
                .append(record.name())
                .append(" { \"");
        for (int i = 0; i < record.fields().size(); i++) {
            var field = record.fields().get(i);
            if (i > 0) {
                body.append(" + \", \"");
            }
            var displayFieldName = mapRecordFieldDisplayName(record, field);
            body.append(" + \"\\\"")
                    .append(displayFieldName)
                    .append("\\\": \" + ")
                    .append(mapRecordFieldToStringValue(field));
        }
        body.append(" + \" }\"; }\n");
        body.append("private static java.lang.String __capybaraToStringValue(java.lang.Object value) {\n");
        body.append("if (value == null) { return \"null\"; }\n");
        body.append("if (value instanceof java.lang.String __capybaraStringValue) {\n");
        body.append("return \"\\\"\" + __capybaraStringValue.replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\") + \"\\\"\";\n");
        body.append("}\n");
        body.append("if (value instanceof java.lang.Enum<?> __capybaraEnumValue) {\n");
        body.append("return \"INSTANCE\".equals(__capybaraEnumValue.name())\n");
        body.append("? __capybaraEnumValue.getDeclaringClass().getSimpleName()\n");
        body.append(": __capybaraEnumValue.name();\n");
        body.append("}\n");
        body.append("if (value instanceof java.util.Map<?, ?> __capybaraMapValue) {\n");
        body.append("return __capybaraMapValue.entrySet().stream()\n");
        body.append(".map(__capybaraEntry -> java.lang.String.valueOf(__capybaraEntry.getKey()) + \"=\" + __capybaraToStringValue(__capybaraEntry.getValue()))\n");
        body.append(".collect(java.util.stream.Collectors.joining(\",\", \"{\", \"}\"));\n");
        body.append("}\n");
        body.append("if (value instanceof java.util.Collection<?> __capybaraCollectionValue) {\n");
        body.append("return __capybaraCollectionValue.stream()\n");
        body.append(".map(__capybaraItem -> __capybaraToStringValue(__capybaraItem))\n");
        body.append(".collect(java.util.stream.Collectors.joining(\",\", \"[\", \"]\"));\n");
        body.append("}\n");
        body.append("if (value instanceof java.util.Map.Entry<?, ?> __capybaraEntryValue) {\n");
        body.append("return java.lang.String.valueOf(__capybaraEntryValue.getKey()) + \"=\" + __capybaraToStringValue(__capybaraEntryValue.getValue());\n");
        body.append("}\n");
        body.append("return java.lang.String.valueOf(value);\n");
        body.append("}\n");
        return body.toString();
    }

    private String mapRecordFieldDisplayName(JavaRecord record, JavaRecord.JavaRecordField field) {
        if ("Error".equals(record.name().toString())
            && "ex".equals(field.name())
            && "pl.grzeslowski.capybara.CapybaraException".equals(field.type().toString())) {
            return "message";
        }
        return field.name();
    }

    private String mapRecordFieldToStringValue(JavaRecord.JavaRecordField field) {
        return "__capybaraToStringValue(" + field.name() + ")";
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
        var defaultMethods = javaInterface.defaultMethods().stream()
                .map(this::mapJavaInterfaceDefaultMethod)
                .collect(joining());
        return "public interface " + javaInterface.name() + " {" + methods + defaultMethods + "}\n";
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
        var defaultMethods = javaInterface.defaultMethods().stream()
                .map(this::mapJavaInterfaceDefaultMethod)
                .collect(joining());
        return "public sealed interface " + javaInterface.name() + typeParameters + " permits " + permits + " {" + methods + defaultMethods + "}\n";
    }

    private String mapJavaInterfaceMethod(JavaInterface.JavaInterfaceMethod method) {
        return method.returnType() + " " + method.name() + "();";
    }

    private String mapJavaInterfaceDefaultMethod(JavaMethod method) {
        var prefix = method.isPrivate() ? "private" : "default";
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + prefix + " " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters())
               + "\n}\n";
    }

    private String mapJavaMethod(JavaMethod method) {
        if (method.programMain()) {
            return mapJavaProgramMainMethod(method);
        }
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + (method.isPrivate() ? "private" : "public") + " static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters())
               + "\n}\n";
    }

    private String mapJavaRecordMethod(JavaMethod method) {
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + (method.isPrivate() ? "private" : "public") + " " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(method.expression(), method.parameters())
               + "\n}\n";
    }

    private String mapJavaProgramMainMethod(JavaMethod method) {
        var rewrittenParameters = method.parameters().isEmpty()
                ? method.parameters()
                : List.of(new JavaMethod.JavaFunctionParameter(
                        method.parameters().getFirst().type(),
                        method.parameters().getFirst().sourceName(),
                        "__capybaraArgsList"
                ));
        var programComputation = evaluateExpression(method.expression(), rewrittenParameters)
                .replaceFirst("\\s*return\\s+", "var program = ");
        var programType = normalizeProgramTypeReference(method.returnType().toString());
        var successType = programType + ".Success";
        var failedType = programType + ".Failed";
        return mapJavaDoc(method.comments())
               + "public static void main(java.lang.String[] args) {\n"
               + "var __capybaraArgsList = java.util.List.of(args);\n"
               + programComputation + "\n"
               + "switch (program) {\n"
               + "case " + successType + "(var __capybaraResults) -> {\n"
               + "__capybaraResults.forEach(System.out::println);\n"
               + "}\n"
               + "case " + failedType + "(var __capybaraExitCode, var __capybaraErrors) -> {\n"
               + "__capybaraErrors.forEach(System.err::println);\n"
               + "System.exit(__capybaraExitCode);\n"
               + "}\n"
               + "default -> throw new java.lang.IllegalStateException(\"Unexpected value: \" + program);\n"
               + "}\n"
               + "}\n";
    }

    private String normalizeProgramTypeReference(String typeName) {
        var parts = typeName.split("\\.");
        if (parts.length >= 2 && parts[parts.length - 1].equals(parts[parts.length - 2])) {
            return String.join(".", java.util.Arrays.copyOf(parts, parts.length - 1));
        }
        return typeName;
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
