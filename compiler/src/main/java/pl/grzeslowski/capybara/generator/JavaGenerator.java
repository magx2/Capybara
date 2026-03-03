package pl.grzeslowski.capybara.generator;

import pl.grzeslowski.capybara.linker.*;

import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

public final class JavaGenerator implements Generator {
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
        var code = new StringBuilder();

        // package
        code.append("package ")
                .append(module.path()
                        // Linux and macOS
                        .replaceAll("/", ".")
                        // windows
                        .replaceAll("\\\\", "."))
                .append(";\n\n");

        // imports
        // TODO: imports

        // generated annotation
        code.append("@javax.annotation.processing.Generated(date = \"")
                .append(ZonedDateTime.now())
                .append("\", value = \"Capybara Compiler\")\n");

        // class declaration
        code.append("public class ")
                .append(module.name())
                .append("{\n");

        // types
        var genericDataTypes = module.types().values();

        var parentTypes = genericDataTypes.stream()
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .collect(Collectors.toSet());

        var dataTypes = genericDataTypes
                .stream()
                .filter(LinkedDataType.class::isInstance)
                .map(LinkedDataType.class::cast)
                .filter(ldt -> parentTypes.stream().noneMatch(pt -> pt.subTypes().contains(ldt)))
                .collect(Collectors.toSet());

        Stream.concat(parentTypes.stream(), dataTypes.stream())
                .map(this::type)
                .forEach(code::append);

        // functions
        module.functions().stream().map(this::function).forEach(code::append);

        // close object declaration
        code.append('}');

        return code.toString();
    }

    private String function(LinkedFunction function) {
        return "public static " + type(function.returnType()) + " " + function.name()
               + "(" + functionParameters(function.parameters()) + ") {\n"
               + expression(function.expression())
               + "\n}\n";
    }

    private String expression(LinkedExpression expression) {
        return "throw new java.lang.UnsupportedOperationException(\"WIP\");";
    }

    private String functionParameters(List<LinkedFunction.LinkedFunctionParameter> parameters) {
        return parameters.stream()
                .map(this::functionParameter)
                .collect(joining(", "));
    }

    private String functionParameter(LinkedFunction.LinkedFunctionParameter parameter) {
        return type(parameter.type()) + " " + parameter.name();
    }

    private String type(GenericDataType type) {
        return switch (type) {
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType(linkedDataParentType);
            case LinkedDataType linkedDataType -> linkedDataType(linkedDataType);
        };
    }

    private String linkedDataParentType(LinkedDataParentType type) {
        var interfaceCode = "public sealed interface " + type.name() + " permits " + type.subTypes().stream().map(LinkedDataType::name).collect(joining(", "))
                            + " {\n"
                            + interfaceFields(type.fields()) + "\n"
                            + "}\n";

        var subTypes = type.subTypes()
                .stream()
                .map(ldt -> linkedDataType(ldt, type))
                .collect(joining("\n"));

        return interfaceCode + "\n" + subTypes;
    }

    private String linkedDataType(LinkedDataType type, LinkedDataParentType parent) {
        var allFields = Stream.concat(parent.fields().stream(), type.fields().stream())
                .distinct()
                .toList();
        return "public record " + type.name() + "(" + fields(allFields) + ") implements " + parent.name() + " {\n}\n";
    }

    private String interfaceFields(List<LinkedDataType.LinkedField> fields) {
        return fields.stream().map(this::interfaceField).collect(joining("\n"));
    }

    private String interfaceField(LinkedDataType.LinkedField field) {
        return type(field.type()) + " " + field.name() + "();";
    }

    private String linkedDataType(LinkedDataType type) {
        return "public record " + type.name() + "(" + fields(type.fields()) + ") {\n}\n";
    }

    private String fields(List<LinkedDataType.LinkedField> fields) {
        return fields.stream()
                .map(this::field)
                .collect(joining(", "));
    }

    private String field(LinkedDataType.LinkedField field) {
        return type(field.type()) + " " + field.name();
    }

    private String type(LinkedType type) {
        return switch (type) {
            case GenericDataType genericDataType -> genericDataType(genericDataType);
            case PrimitiveLinkedType primitiveLinkedType -> primitiveLinkedType(primitiveLinkedType);
        };
    }

    private String genericDataType(GenericDataType type) {
        return type.name();
    }

    private String primitiveLinkedType(PrimitiveLinkedType type) {
        return switch (type) {
            case INT -> "int";
            case STRING -> "java.lang.String";
            case BOOL -> "boolean";
            case FLOAT -> "float";
        };
    }
}
