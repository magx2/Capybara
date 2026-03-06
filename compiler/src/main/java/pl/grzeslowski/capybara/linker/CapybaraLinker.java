package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.LinkedFunction.LinkedFunctionParameter;
import pl.grzeslowski.capybara.linker.expression.CapybaraExpressionLinker;
import pl.grzeslowski.capybara.parser.*;

import java.util.*;
import java.util.stream.Stream;

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
                .flatMap(dataTypes -> {
                    var functions = findFunctions(module.functional().definitions());
                    return linkFunctionSignatures(functions, dataTypes)
                            .flatMap(signatures ->
                                    linkFunctions(functions, dataTypes, signatures)
                                            .map(linkedFunctions -> new LinkedModule(module.name(), module.path(), dataTypes, Set.copyOf(linkedFunctions))));
                });
    }

    private List<Function> findFunctions(Set<Definition> definitions) {
        return definitions.stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .toList();
    }

    private ValueOrError<List<LinkedFunction>> linkFunctions(
            List<Function> functions,
            Map<String, GenericDataType> dataTypes,
            List<CapybaraExpressionLinker.FunctionSignature> signatures
    ) {
        return functions.stream()
                .map(f -> linkFunction(f, dataTypes, signatures))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunction> linkFunction(
            Function function,
            Map<String, GenericDataType> dataTypes,
            List<CapybaraExpressionLinker.FunctionSignature> signatures
    ) {
        var linked = linkParameters(function.parameters(), dataTypes)
                .flatMap(parameters ->
                        new CapybaraExpressionLinker(parameters, dataTypes, signatures).linkExpression(function.expression())
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

    private ValueOrError<List<CapybaraExpressionLinker.FunctionSignature>> linkFunctionSignatures(
            List<Function> functions,
            Map<String, GenericDataType> dataTypes
    ) {
        return functions.stream()
                .map(function -> linkParameters(function.parameters(), dataTypes)
                        .flatMap(parameters -> function.returnType()
                                .map(type -> linkType(type, dataTypes))
                                .orElseGet(() -> ValueOrError.success(PrimitiveLinkedType.ANY))
                                .map(returnType -> new CapybaraExpressionLinker.FunctionSignature(
                                        function.name(),
                                        parameters.stream().map(LinkedFunctionParameter::type).toList(),
                                        returnType
                                ))))
                .collect(new ValueOrErrorCollectionCollector<>());
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
        var dataDeclarationsOrError = linkDataDeclarations(castList(module, DataDeclaration.class));
        var singlesDeclarationsOrError = castList(module, SingleDeclaration.class)
                .stream()
                .map(this::linkSingleDeclaration)
                .collect(new ValueOrErrorCollectionCollector<>());

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();
        if (singlesDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var singleDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) singlesDeclarationsOrError).value();

        var typeDeclarationsOrError = castList(module, TypeDeclaration.class)
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(typeDeclaration, Stream.concat(dataDeclarations.stream(), singleDeclarations.stream()).toList()))
                .collect(new ValueOrErrorCollectionCollector<>());

        if (typeDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var typeDeclarations = ((ValueOrError.Value<List<LinkedDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(singleDeclarations);
        set.addAll(typeDeclarations);
        var map = set.stream().collect(toMap(GenericDataType::name, identity()));
        return ValueOrError.success(map);
    }

    private ValueOrError<List<LinkedDataType>> linkDataDeclarations(List<DataDeclaration> dataDeclarations) {
        var declarationsByName = dataDeclarations.stream()
                .collect(toMap(DataDeclaration::name, identity(), (first, second) -> first));
        var cache = new HashMap<String, ValueOrError<LinkedDataType>>();
        return dataDeclarations.stream()
                .map(dataDeclaration -> linkDataDeclaration(dataDeclaration, declarationsByName, cache, new HashSet<>()))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedDataType> linkDataDeclaration(
            DataDeclaration dataDeclaration,
            Map<String, DataDeclaration> declarationsByName,
            Map<String, ValueOrError<LinkedDataType>> cache,
            Set<String> visiting
    ) {
        var cached = cache.get(dataDeclaration.name());
        if (cached != null) {
            return cached;
        }
        if (!visiting.add(dataDeclaration.name())) {
            return withPosition(
                    ValueOrError.error("Circular data extension detected for `" + dataDeclaration.name() + "`"),
                    dataDeclaration.position()
            );
        }

        var genericTypes = Set.copyOf(dataDeclaration.typeParameters());
        var inheritedFields = dataDeclaration.extendsTypes().stream()
                .map(parentName -> {
                    var parent = declarationsByName.get(parentName);
                    if (parent == null) {
                        return ValueOrError.<List<LinkedDataType.LinkedField>>error(
                                "Extended data type `" + parentName + "` not found"
                        );
                    }
                    return linkDataDeclaration(parent, declarationsByName, cache, visiting)
                            .map(LinkedDataType::fields);
                })
                .collect(new ValueOrErrorCollectionCollector<>());
        var ownFields = dataDeclaration.fields().stream()
                .map(field -> linkField(field, genericTypes))
                .collect(new ValueOrErrorCollectionCollector<>());
        var linked = ValueOrError.join(
                        (List<List<LinkedDataType.LinkedField>> inherited, List<LinkedDataType.LinkedField> own) -> {
                            var fields = inherited.stream()
                                    .flatMap(Collection::stream)
                                    .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
                            fields.addAll(own);
                            return new LinkedDataType(
                                    dataDeclaration.name(),
                                    List.copyOf(fields),
                                    dataDeclaration.typeParameters(),
                                    dataDeclaration.extendsTypes(),
                                    false
                            );
                        },
                        inheritedFields,
                        ownFields
                );
        visiting.remove(dataDeclaration.name());
        var withPosition = withPosition(linked, dataDeclaration.position());
        cache.put(dataDeclaration.name(), withPosition);
        return withPosition;
    }

    private ValueOrError<LinkedDataType> linkSingleDeclaration(SingleDeclaration singleDeclaration) {
        return ValueOrError.success(new LinkedDataType(singleDeclaration.name(), List.of(), List.of(), List.of(), true));
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
        var dataTypesMap = dataDeclarations.stream().collect(toMap(LinkedDataType::name, identity(), (first, second) -> first));
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
