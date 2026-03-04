package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.generator.java.*;
import pl.grzeslowski.capybara.linker.LinkedModule;
import pl.grzeslowski.capybara.linker.LinkedProgram;

import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

import static java.lang.String.join;
import static java.util.stream.Collectors.joining;
import static pl.grzeslowski.capybara.generator.java.JavaExpressionEvaluator.evaluateExpression;

public final class JavaGenerator implements Generator {
    private final JavaAstBuilder astBuilder = new JavaAstBuilder();

    @Override
    public CompiledProgram generate(LinkedProgram program) {
        var modules = program.modules().stream().map(this::module).toList();
        return new CompiledProgram(modules);
    }

    private CompiledModule module(LinkedModule module) {
        return new CompiledModule(
                relativePath(module),
                code(module)
        );
    }

    private Path relativePath(LinkedModule module) {
        return Path.of(module.path(), module.name() + ".java");
    }


    private String code(LinkedModule module) {
        return code(astBuilder.build(module));
    }

    private String code(JavaClass javaClass) {
        var code = new StringBuilder();

        // package
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");

        // imports
        // TODO: imports

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

        // static methods
        code.append('\n');
        javaClass.staticMethods()
                .stream()
                .map(this::mapJavaMethod)
                .forEach(code::append);

        // close object declaration
        code.append("}");

        return code.toString();
    }

    private String mapJavaRecord(JavaRecord record) {
        var fields = record.fields().stream().map(this::mapJavaRecordField).collect(joining(", "));
        var implementInterfaces = record.implementInterfaces().size() > 0
                ? record.implementInterfaces().stream()
                .map(Objects::toString)
                .collect(joining(", ", " implements ", " "))
                : "";
        var staticMethods = record.staticMethods().size() > 0 ? "// todo implement static methods" : "";
        var methods = record.methods().size() > 0 ? "// todo implement methods" : "";
        return "public record " + record.name() + "(" + fields + ")" + implementInterfaces + "{" + staticMethods + "" + methods + "}\n";
    }

    private String mapJavaRecordField(JavaRecord.JavaRecordField field) {
        return field.type() + " " + field.name();
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
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        return "public sealed interface " + javaInterface.name() + " permits " + permits + " {" + methods + "}\n";
    }

    private String mapJavaInterfaceMethod(JavaInterface.JavaInterfaceMethod method) {
        return method.returnType() + " " + method.name() + "();";
    }

    private String mapJavaMethod(JavaMethod method) {
        return "public static " + method.returnType() + " " + method.name()
               + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + "return " + evaluateExpression(method.expression().expression()) + ";"
               + "\n}\n";
    }

    private String mapFunctionParameters(List<JavaMethod.JavaFunctionParameter> parameters) {
        return parameters.stream()
                .map(this::mapFunctionParameter)
                .collect(joining(", "));
    }

    private String mapFunctionParameter(JavaMethod.JavaFunctionParameter parameter) {
        return parameter.type() + " " + parameter.name();
    }
}
