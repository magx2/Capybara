package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.LinkedFunction.LinkedFunctionParameter;
import pl.grzeslowski.capybara.linker.expression.CapybaraExpressionLinker;
import pl.grzeslowski.capybara.parser.*;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;
import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;

public class CapybaraLinker {
    public static final CapybaraLinker INSTANCE = new CapybaraLinker();

    public ValueOrError<LinkedProgram> link(Program program) {
        return program.modules()
                .stream()
                .map(this::linkModule)
                .reduce(
                        ValueOrError.<List<LinkedModule>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join)
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
                .reduce(
                        ValueOrError.<List<LinkedFunction>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private ValueOrError<LinkedFunction> linkFunction(Function function, Map<String, GenericDataType> dataTypes) {
        return linkType(function.returnType(), dataTypes)
                .flatMap(rtype ->
                        linkParameters(function.parameters(), dataTypes)
                                .flatMap(parameters ->
                                        new CapybaraExpressionLinker(parameters, dataTypes).linkExpression(function.expression())
                                                .map(ex ->
                                                        new LinkedFunction(
                                                                function.name(),
                                                                rtype,
                                                                parameters,
                                                                ex))));
    }

    private ValueOrError<List<LinkedFunctionParameter>> linkParameters(List<Parameter> parameters, Map<String, GenericDataType> dataTypes) {
        return parameters.stream()
                .map(p -> linkParameter(p, dataTypes))
                .reduce(
                        ValueOrError.<List<LinkedFunctionParameter>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private ValueOrError<LinkedFunctionParameter> linkParameter(Parameter parameter, Map<String, GenericDataType> dataTypes) {
        return linkType(parameter.type(), dataTypes)
                .map(type -> new LinkedFunctionParameter(parameter.name(), type));
    }

    private ValueOrError<Map<String, GenericDataType>> types(Module module) {
        var dataDeclarationsOrError = castList(module, DataDeclaration.class)
                .stream()
                .map(this::linkDataDeclaration)
                .reduce(
                        ValueOrError.<List<LinkedDataType>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();

        var typeDeclarationsOrError = castList(module, TypeDeclaration.class)
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(typeDeclaration, dataDeclarations))
                .reduce(
                        ValueOrError.<List<LinkedDataParentType>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);

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
        return dataDeclaration.fields()
                .stream()
                .map(this::linkField)
                .reduce(
                        ValueOrError.<List<LinkedDataType.LinkedField>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join)
                .map(fields -> new LinkedDataType(dataDeclaration.name(), fields));
    }

    private ValueOrError<LinkedDataType.LinkedField> linkField(DataDeclaration.DataField type) {
        return linkType(type.type())
                .map(t -> new LinkedDataType.LinkedField(type.name(), t));
    }

    private ValueOrError<LinkedDataParentType> linkTypeDeclaration(TypeDeclaration typeDeclaration, List<LinkedDataType> dataDeclarations) {
        return findSubtypes(typeDeclaration.subTypes(), dataDeclarations)
                .flatMap(subTypes -> linkedDataParentType(typeDeclaration, subTypes));
    }

    private ValueOrError<LinkedDataParentType> linkedDataParentType(TypeDeclaration typeDeclaration, List<LinkedDataType> subTypes) {
        return typeDeclaration.fields()
                .stream()
                .map(this::linkField)
                .reduce(
                        ValueOrError.<List<LinkedDataType.LinkedField>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join)
                .map(fields -> new LinkedDataParentType(typeDeclaration.name(),
                        fields,
                        subTypes));
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
                .reduce(
                        ValueOrError.<List<LinkedDataType>>success(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private static <T> List<T> castList(Module module, Class<T> clazz) {
        return module.functional().definitions()
                .stream()
                .filter(clazz::isInstance)
                .map(clazz::cast)
                .toList();
    }
}
