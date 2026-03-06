package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.LinkedFunction.LinkedFunctionParameter;
import pl.grzeslowski.capybara.linker.expression.CapybaraExpressionLinker;
import pl.grzeslowski.capybara.parser.*;

import java.util.*;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;
import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;

public class CapybaraLinker {
    public static final CapybaraLinker INSTANCE = new CapybaraLinker();

    public ValueOrError<LinkedProgram> link(Program program) {
        return program.modules()
                .stream()
                .map(this::linkModule)
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(LinkedProgram::new);
    }

    private ValueOrError<LinkedModule> linkModule(Module module) {
        return types(module)
                .flatMap(dataTypes ->
                        linkFunctions(findFunctions(module.functional().definitions()), dataTypes)
                                .map(functions -> new LinkedModule(module.name(), module.path(), dataTypes, Set.copyOf(functions))));
    }

    private List<Function> findFunctions(Set<Definition> definitions) {
        return definitions.stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .toList();
    }

    private ValueOrError<List<LinkedFunction>> linkFunctions(List<Function> functions, Map<String, GenericDataType> dataTypes) {
        return functions.stream()
                .map(f -> linkFunction(f, dataTypes))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunction> linkFunction(Function function, Map<String, GenericDataType> dataTypes) {
        var linked = linkParameters(function.parameters(), dataTypes)
                .flatMap(parameters ->
                        new CapybaraExpressionLinker(parameters, dataTypes).linkExpression(function.expression())
                                .flatMap(ex ->
                                        function.returnType()
                                                .map(type -> {
                                                    // todo check if this type matches ex.type
                                                    return linkType(type, dataTypes);
                                                })
                                                .orElseGet(() -> ValueOrError.success(ex.type()))
                                                .map(rtype ->
                                                        new LinkedFunction(
                                                                function.name(),
                                                                rtype,
                                                                parameters,
                                                                ex))));
        return withPosition(linked, function.position());
    }

    private ValueOrError<List<LinkedFunctionParameter>> linkParameters(List<Parameter> parameters, Map<String, GenericDataType> dataTypes) {
        return parameters.stream()
                .map(p -> linkParameter(p, dataTypes))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunctionParameter> linkParameter(Parameter parameter, Map<String, GenericDataType> dataTypes) {
        return withPosition(
                linkType(parameter.type(), dataTypes)
                        .map(type -> new LinkedFunctionParameter(parameter.name(), type)),
                parameter.position()
        );
    }

    private ValueOrError<Map<String, GenericDataType>> types(Module module) {
        var dataDeclarationsOrError = castList(module, DataDeclaration.class)
                .stream()
                .map(this::linkDataDeclaration)
                .collect(new ValueOrErrorCollectionCollector<>());

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();

        var typeDeclarationsOrError = castList(module, TypeDeclaration.class)
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(typeDeclaration, dataDeclarations))
                .collect(new ValueOrErrorCollectionCollector<>());

        if (typeDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var typeDeclarations = ((ValueOrError.Value<List<LinkedDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(typeDeclarations);
        var map = set.stream().collect(toMap(GenericDataType::name, identity()));
        return ValueOrError.success(map);
    }

    private ValueOrError<LinkedDataType> linkDataDeclaration(DataDeclaration dataDeclaration) {
        var genericTypes = Set.copyOf(dataDeclaration.typeParameters());
        var linked = dataDeclaration.fields()
                .stream()
                .map(field -> linkField(field, genericTypes))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(fields -> new LinkedDataType(dataDeclaration.name(), fields, dataDeclaration.typeParameters()));
        return withPosition(linked, dataDeclaration.position());
    }

    private ValueOrError<LinkedDataType.LinkedField> linkField(DataDeclaration.DataField type, Set<String> genericTypes) {
        if (type.type() instanceof DataType dataType && genericTypes.contains(dataType.name())) {
            return ValueOrError.success(new LinkedDataType.LinkedField(type.name(), new LinkedGenericTypeParameter(dataType.name())));
        }
        return linkType(type.type())
                .map(t -> new LinkedDataType.LinkedField(type.name(), t));
    }

    private ValueOrError<LinkedDataParentType> linkTypeDeclaration(TypeDeclaration typeDeclaration, List<LinkedDataType> dataDeclarations) {
        var linked = findSubtypes(typeDeclaration.subTypes(), dataDeclarations)
                .flatMap(subTypes -> linkedDataParentType(typeDeclaration, subTypes));
        return withPosition(linked, typeDeclaration.position());
    }

    private ValueOrError<LinkedDataParentType> linkedDataParentType(TypeDeclaration typeDeclaration, List<LinkedDataType> subTypes) {
        var genericTypes = Set.copyOf(typeDeclaration.typeParameters());
        return typeDeclaration.fields()
                .stream()
                .map(field -> linkField(field, genericTypes))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(fields -> new LinkedDataParentType(typeDeclaration.name(),
                        fields,
                        subTypes,
                        typeDeclaration.typeParameters()));
    }

    private ValueOrError<List<LinkedDataType>> findSubtypes(List<String> rawSubTypes, List<LinkedDataType> dataDeclarations) {
        var dataTypesMap = dataDeclarations.stream().collect(toMap(LinkedDataType::name, identity()));
        return rawSubTypes.stream()
                .map(key -> {
                    var dataType = dataTypesMap.get(key);
                    if (dataType == null) {
                        return ValueOrError.<LinkedDataType>error("Type " + key + " not found");
                    }
                    return ValueOrError.success(dataType);
                })
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private static <T> ValueOrError<T> withPosition(ValueOrError<T> valueOrError, Optional<pl.grzeslowski.capybara.parser.SourcePosition> position) {
        if (valueOrError instanceof ValueOrError.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new ValueOrError.Error<>(error.errors()
                    .stream()
                    .map(ValueOrError.Error.SingleError::message)
                    .map(msg -> "line %d, column %d: %s".formatted(pos.line(), pos.column(), msg))
                    .map(ValueOrError.Error.SingleError::new)
                    .toList());
        }
        return valueOrError;
    }

    private static <T> List<T> castList(Module module, Class<T> clazz) {
        return module.functional().definitions()
                .stream()
                .filter(clazz::isInstance)
                .map(clazz::cast)
                .toList();
    }
}
