package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.LinkedFunction.LinkedFunctionParameter;
import pl.grzeslowski.capybara.parser.*;

import java.util.*;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;

public class CapybaraLinker {
    public static final CapybaraLinker INSTANCE = new CapybaraLinker();

    public ValueOrError<LinkedProgram> link(Program program) {
        return program.modules()
                .stream()
                .map(this::linkModule)
                .reduce(
                        (ValueOrError<List<LinkedModule>>) new ValueOrError.Value<List<LinkedModule>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join)
                .map(LinkedProgram::new);
    }

    private ValueOrError<LinkedModule> linkModule(Module module) {
        return types(module)
                .flatMap(dataTypes ->
                        linkFunctions(findFunctions(module.functional().definitions()), dataTypes)
                                .map(functions -> new LinkedModule(dataTypes, Set.copyOf(functions))));
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
                        (ValueOrError<List<LinkedFunction>>) new ValueOrError.Value<List<LinkedFunction>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private ValueOrError<LinkedFunction> linkFunction(Function function, Map<String, GenericDataType> dataTypes) {
        var linkedTypeOrError = linkType(function.returnType(), dataTypes);
        var linkedParametersOrError = linkParameters(function.parameters(), dataTypes);
        var linkedExpressionOrError = linkExpression(function.expression(), dataTypes);

        record TypeParameters(LinkedType type, List<LinkedFunctionParameter> parameters) {
        }
        var typeParameters = ValueOrError.join(
                TypeParameters::new,
                linkedTypeOrError,
                linkedParametersOrError);

        record TypeParametersExpression(LinkedType type, List<LinkedFunctionParameter> parameters,
                                        LinkedExpression expression) {
            public TypeParametersExpression(TypeParameters tp, LinkedExpression expression) {
                this(tp.type, tp.parameters, expression);
            }
        }
        var typeParametersExpression = ValueOrError.join(
                TypeParametersExpression::new,
                typeParameters,
                linkedExpressionOrError);

        return typeParametersExpression.map(tpe -> new LinkedFunction(function.name(), tpe.type, tpe.parameters, tpe.expression()));
    }

    private ValueOrError<LinkedExpression> linkExpression(Expression expression, Map<String, GenericDataType> dataTypes) {
        return new ValueOrError.Value<>(new LinkedExpression(expression));
    }

    private ValueOrError<List<LinkedFunctionParameter>> linkParameters(List<Parameter> parameters, Map<String, GenericDataType> dataTypes) {
        return parameters.stream()
                .map(p -> linkParameter(p, dataTypes))
                .reduce(
                        (ValueOrError<List<LinkedFunctionParameter>>) new ValueOrError.Value<List<LinkedFunctionParameter>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private ValueOrError<LinkedFunctionParameter> linkParameter(Parameter parameter, Map<String, GenericDataType> dataTypes) {
        return linkType(parameter.type(), dataTypes)
                .map(type -> new LinkedFunctionParameter(parameter.name(), type));
    }

    private ValueOrError<? extends LinkedType> linkType(Type type, Map<String, GenericDataType> dataTypes) {
        return switch (type) {
            case PrimitiveType primitiveType -> new ValueOrError.Value<>(linkPrimitiveType(primitiveType));
            case DataType dataType -> linkDataType(dataType, dataTypes);
        };
    }

    @Deprecated
    private ValueOrError<? extends LinkedType> linkType(Type type) {
        // TODO proper mapping of types
        return linkType(type, Map.of());
    }

    private ValueOrError<? extends LinkedType> linkDataType(DataType dataType, Map<String, GenericDataType> dataTypes) {
        if (dataTypes.containsKey(dataType.name())) {
            return new ValueOrError.Value<>(dataTypes.get(dataType.name()));
        }

        return new ValueOrError.Error<>("Data type \"" + dataType.name() + "\" not found");
    }

    private LinkedType linkPrimitiveType(PrimitiveType primitiveType) {
        return switch (primitiveType) {
            case INT -> PrimitiveLinkedType.INT;
            case STRING -> PrimitiveLinkedType.STRING;
            case BOOL -> PrimitiveLinkedType.BOOL;
            case FLOAT -> PrimitiveLinkedType.FLOAT;
        };
    }

    private ValueOrError<Map<String, GenericDataType>> types(Module module) {
        var dataDeclarationsOrError = castList(module, DataDeclaration.class)
                .stream()
                .map(this::linkDataDeclaration)
                .reduce(
                        (ValueOrError<List<LinkedDataType>>) new ValueOrError.Value<List<LinkedDataType>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();

        var typeDeclarationsOrError = castList(module, TypeDeclaration.class)
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(typeDeclaration, dataDeclarations))
                .reduce(
                        (ValueOrError<List<LinkedDataParentType>>) new ValueOrError.Value<List<LinkedDataParentType>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);

        if (typeDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var typeDeclarations = ((ValueOrError.Value<List<LinkedDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(typeDeclarations);
        var map = set.stream().collect(toMap(GenericDataType::name, identity()));
        return new ValueOrError.Value<>(map);
    }

    private ValueOrError<LinkedDataType> linkDataDeclaration(DataDeclaration dataDeclaration) {
        return dataDeclaration.fields()
                .stream()
                .map(this::linkField)
                .reduce(
                        (ValueOrError<List<LinkedDataType.LinkedField>>) new ValueOrError.Value<List<LinkedDataType.LinkedField>>(List.of()),
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
                        (ValueOrError<List<LinkedDataType.LinkedField>>) new ValueOrError.Value<List<LinkedDataType.LinkedField>>(List.of()),
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
                        return new ValueOrError.Error<LinkedDataType>("Type " + key + " not found");
                    }
                    return new ValueOrError.Value<>(dataType);
                })
                .reduce(
                        (ValueOrError<List<LinkedDataType>>) new ValueOrError.Value<List<LinkedDataType>>(List.of()),
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
