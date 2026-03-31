package dev.capylang.compiler.expression;

import dev.capylang.compiler.CollectionLinkedType.CompiledDict;
import dev.capylang.compiler.CollectionLinkedType.CompiledList;
import dev.capylang.compiler.CollectionLinkedType.CompiledSet;
import dev.capylang.compiler.*;
import dev.capylang.compiler.parser.*;

import java.math.BigInteger;
import java.util.*;
import java.util.logging.Logger;

import static dev.capylang.compiler.CapybaraTypeCompiler.linkType;
import static dev.capylang.compiler.PrimitiveLinkedType.*;
import static dev.capylang.compiler.expression.CapybaraTypeFinder.findHigherType;

public class CapybaraExpressionCompiler {
    private static final Logger LOG = Logger.getLogger(CapybaraExpressionCompiler.class.getName());
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String METHOD_INVOKE_PREFIX = "__invoke__";
    private static final String DICT_PIPE_ARGS_SEPARATOR = "::";
    private static final String TUPLE_PIPE_ARGS_SEPARATOR = ";;";
    private final List<CompiledFunction.CompiledFunctionParameter> parameters;
    private final Map<String, GenericDataType> dataTypes;
    private final List<FunctionSignature> functionSignatures;
    private final Map<String, List<FunctionSignature>> functionSignaturesByModule;
    private final Map<String, String> moduleClassNameByModuleName;

    public CapybaraExpressionCompiler(
            List<CompiledFunction.CompiledFunctionParameter> parameters,
            Map<String, GenericDataType> dataTypes,
            List<FunctionSignature> functionSignatures,
            Map<String, List<FunctionSignature>> functionSignaturesByModule,
            Map<String, String> moduleClassNameByModuleName
    ) {
        this.parameters = parameters;
        this.dataTypes = dataTypes;
        this.functionSignatures = functionSignatures;
        this.functionSignaturesByModule = functionSignaturesByModule;
        this.moduleClassNameByModuleName = moduleClassNameByModuleName;
    }

    public Result<CompiledExpression> linkExpression(Expression expression) {
        return linkExpression(expression, Scope.EMPTY);
    }

    private Result<CompiledExpression> linkExpression(Expression expression, Scope scope) {
        return switch (expression) {
            case BooleanValue booleanValue -> linkBooleanValue(booleanValue, scope);
            case ByteValue byteValue -> linkByteValue(byteValue, scope);
            case DoubleValue doubleValue -> linkDoubleValue(doubleValue, scope);
            case FieldAccess fieldAccess -> linkFieldAccess(fieldAccess, scope);
            case FloatValue floatValue -> linkFloatValue(floatValue, scope);
            case FunctionCall functionCall -> linkFunctionCall(functionCall, scope);
            case FunctionInvoke functionInvoke -> linkFunctionInvoke(functionInvoke, scope);
            case FunctionReference functionReference -> linkFunctionReference(functionReference, scope);
            case IfExpression ifExpression -> linkIfExpression(ifExpression, scope);
            case IndexExpression indexExpression -> linkIndexExpression(indexExpression, scope);
            case InfixExpression infixExpression -> linkInfixExpression(infixExpression, scope);
            case IntValue intValue -> linkIntValue(intValue, scope);
            case LambdaExpression lambdaExpression -> linkStandaloneLambdaExpression(lambdaExpression, scope);
            case ReduceExpression reduceExpression -> withPosition(
                    Result.error("Reduce expression can only be used as the right side of `|>`"),
                    reduceExpression.position()
            );
            case SliceExpression sliceExpression -> linkSliceExpression(sliceExpression, scope);
            case MatchExpression matchExpression -> linkMatchExpression(matchExpression, scope);
            case NewDictExpression newDictExpression -> linkNewDictExpression(newDictExpression, scope);
            case NewListExpression newListExpression -> linkNewListExpression(newListExpression, scope);
            case NewSetExpression newSetExpression -> linkNewSetExpression(newSetExpression, scope);
            case NewData newData -> linkNewData(newData, scope);
            case NothingValue nothingValue -> linkNothingValue(nothingValue);
            case StringValue stringValue -> linkStringValue(stringValue, scope);
            case TupleExpression tupleExpression -> linkTupleExpression(tupleExpression, scope);
            case Value value -> linkValue(value, scope);
            //
            case LetExpression letExpression -> linkLetExpression(letExpression, scope);
            case LongValue longValue -> linkLongValue(longValue, scope);
        };
    }

    private Optional<SourcePosition> lambdaErrorPosition(LambdaExpression lambdaExpression) {
        return lambdaExpression.position().map(position -> {
            if (lambdaExpression.argumentNames().isEmpty() && position.column() > 0) {
                return new SourcePosition(position.line(), position.column() - 1, position.length());
            }
            return position;
        });
    }

    private Result<CompiledExpression> linkStandaloneLambdaExpression(LambdaExpression lambdaExpression, Scope scope) {
        if (!lambdaExpression.argumentNames().isEmpty()) {
            return withPosition(
                    Result.error("Lambda expression with parameters requires expected function type"),
                    lambdaErrorPosition(lambdaExpression)
            );
        }
        return linkExpression(lambdaExpression.expression(), scope)
                .map(expression -> (CompiledExpression) new CompiledLambdaExpression(
                        "__capybaraNoArgs",
                        expression,
                        new CompiledFunctionType(NOTHING, expression.type())
                ));
    }

    private Result<CompiledExpression> linkSliceExpression(SliceExpression expression, Scope scope) {
        return linkExpression(expression.source(), scope)
                .flatMap(source -> {
                    if (!(source.type() instanceof CompiledList)
                        && source.type() != STRING
                        && !(source.type() instanceof CompiledTupleType)) {
                        return withPosition(
                                Result.error("Slice source has to be `list`, `string` or `tuple`, was `" + source.type() + "`"),
                                expression.position()
                        );
                    }
                    return linkSliceBound(expression.start(), scope, "start")
                            .flatMap(start -> linkSliceBound(expression.end(), scope, "end")
                                    .map(end -> (CompiledExpression) new CompiledSliceExpression(
                                            source,
                                            start,
                                            end,
                                            slicedType(source.type(), start, end)
                                    )));
                });
    }

    private Result<CompiledExpression> linkIndexExpression(IndexExpression expression, Scope scope) {
        return linkExpression(expression.source(), scope)
                .flatMap(source -> {
                    if (!(source.type() instanceof CompiledList)
                        && source.type() != STRING
                        && !(source.type() instanceof CompiledTupleType)) {
                        return withPosition(
                                Result.error("Index source has to be `list`, `string` or `tuple`, was `" + source.type() + "`"),
                                expression.position()
                        );
                    }
                    return linkExpression(expression.index(), scope)
                            .flatMap(index -> {
                                if (index.type() != PrimitiveLinkedType.INT) {
                                    return withPosition(
                                            Result.error("Index has to be `int`, was `" + index.type() + "`"),
                                            expression.index().position()
                                    );
                                }
                                var elementType = switch (source.type()) {
                                    case CompiledList linkedList -> Result.success(linkedList.elementType());
                                    case PrimitiveLinkedType primitive when primitive == STRING -> Result.<CompiledType>success(STRING);
                                    case CompiledTupleType tupleType -> tupleElementType(tupleType, index, expression.index().position());
                                    default -> Result.<CompiledType>error("Unsupported index source `" + source.type() + "`");
                                };
                                if (elementType instanceof Result.Error<CompiledType> error) {
                                    return withPosition(new Result.Error<>(error.errors()), expression.position());
                                }
                                var resolvedElementType = ((Result.Success<CompiledType>) elementType).value();
                                if (source.type() instanceof CompiledTupleType) {
                                    return Result.success((CompiledExpression) new CompiledIndexExpression(
                                            source,
                                            index,
                                            resolvedElementType,
                                            resolvedElementType
                                    ));
                                }
                                var optionType = optionTypeFor(resolvedElementType);
                                if (optionType == null) {
                                    return withPosition(Result.error("Option type not found"), expression.position());
                                }
                                return Result.success((CompiledExpression) new CompiledIndexExpression(source, index, resolvedElementType, optionType));
                            });
                });
    }

    private Result<CompiledExpression> linkTupleExpression(TupleExpression expression, Scope scope) {
        return expression.values().stream()
                .map(value -> linkExpression(value, scope))
                .collect(new ResultCollectionCollector<>())
                .map(values -> {
                    var tupleType = new CompiledTupleType(values.stream().map(CompiledExpression::type).toList());
                    return (CompiledExpression) new CompiledTupleExpression(values, tupleType);
                });
    }

    private Result<Optional<CompiledExpression>> linkSliceBound(
            Optional<Expression> bound,
            Scope scope,
            String name
    ) {
        if (bound.isEmpty()) {
            return Result.success(Optional.empty());
        }
        return linkExpression(bound.get(), scope)
                .flatMap(linked -> {
                    if (linked.type() != PrimitiveLinkedType.INT) {
                        return withPosition(
                                Result.error("Slice " + name + " index has to be `int`, was `" + linked.type() + "`"),
                                bound.get().position()
                        );
                    }
                    return Result.success(Optional.of(linked));
                });
    }

    private Result<CompiledExpression> linkBooleanValue(BooleanValue booleanValue, Scope scope) {
        return Result.success(booleanValue.value() ? CompiledBooleanValue.TRUE : CompiledBooleanValue.FALSE);
    }

    private Result<CompiledExpression> linkFieldAccess(FieldAccess fieldAccess, Scope scope) {
        var enumValuesCall = tryLinkEnumValuesFieldAccess(fieldAccess, scope);
        if (enumValuesCall.isPresent()) {
            return enumValuesCall.get();
        }
        return linkExpression(fieldAccess.source(), scope)
                .flatMap(source -> {
                    if (source.type() instanceof CompiledDataParentType linkedDataParentType && linkedDataParentType.enumType()) {
                        if ("order".equals(fieldAccess.field())) {
                            return Result.success(new CompiledFieldAccess(source, "ordinal", PrimitiveLinkedType.INT));
                        }
                        if ("name".equals(fieldAccess.field())) {
                            return Result.success(new CompiledFieldAccess(source, "name", PrimitiveLinkedType.STRING));
                        }
                    }
                    if (source.type() instanceof CompiledDataType linkedDataType && linkedDataType.singleton()) {
                        var enumParent = findEnumParentForValue(linkedDataType.name());
                        if (enumParent != null) {
                            if ("order".equals(fieldAccess.field())) {
                                return Result.success(new CompiledFieldAccess(source, "ordinal", PrimitiveLinkedType.INT));
                            }
                            if ("name".equals(fieldAccess.field())) {
                                return Result.success(new CompiledFieldAccess(source, "name", PrimitiveLinkedType.STRING));
                            }
                        }
                    }
                    if (source.type() instanceof CompiledList
                        || source.type() instanceof CompiledSet
                        || source.type() instanceof CompiledDict
                        || source.type() == STRING) {
                        if ("size".equals(fieldAccess.field())) {
                            var javaField = source.type() == STRING ? "length" : "size";
                            return Result.success(new CompiledFieldAccess(source, javaField, PrimitiveLinkedType.INT));
                        }
                        if ("is_empty".equals(fieldAccess.field())) {
                            return Result.success(new CompiledFieldAccess(source, "isEmpty", PrimitiveLinkedType.BOOL));
                        }
                        return withPosition(
                                Result.error("Field `" + fieldAccess.field() + "` not found in type `" + source.type() + "`"),
                                fieldAccess.position()
                        );
                    }
                    if (!(source.type() instanceof GenericDataType dataType)) {
                        return withPosition(
                                Result.error("Field access requires data type, was `" + source.type() + "`"),
                                fieldAccess.position()
                        );
                    }
                    return dataType.fields().stream()
                            .filter(field -> field.name().equals(fieldAccess.field()))
                            .findFirst()
                            .<Result<CompiledExpression>>map(field ->
                                    Result.success(new CompiledFieldAccess(
                                            source,
                                            field.name(),
                                            resolveFieldType(dataType, field)
                                    )))
                            .orElseGet(() -> withPosition(
                                    Result.error("Field `" + fieldAccess.field() + "` not found in type `" + dataType.name() + "`"),
                                    fieldAccess.position()
                            ));
                });
    }

    private CompiledType resolveFieldType(GenericDataType dataType, CompiledDataType.CompiledField field) {
        var actualTypeDescriptors = switch (dataType) {
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
        };
        if (actualTypeDescriptors.isEmpty()) {
            return field.type();
        }

        var resolvedDataType = resolveDataTypeByName(dataType.name());
        var declaredTypeParameters = switch (resolvedDataType) {
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            default -> actualTypeDescriptors;
        };
        if (declaredTypeParameters.isEmpty()) {
            return field.type();
        }

        var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
        var max = Math.min(declaredTypeParameters.size(), actualTypeDescriptors.size());
        for (var i = 0; i < max; i++) {
            var typeParameterName = declaredTypeParameters.get(i);
            parseLinkedTypeDescriptor(actualTypeDescriptors.get(i))
                    .ifPresent(type -> substitutions.put(typeParameterName, type));
        }
        if (substitutions.isEmpty()) {
            return field.type();
        }
        return substituteTypeParameters(field.type(), substitutions);
    }

    private Result<CompiledExpression> linkFloatValue(FloatValue floatValue, Scope scope) {
        try {
            var value = floatValue.floatValue();
            var normalized = value.endsWith("f") || value.endsWith("F")
                    ? value.substring(0, value.length() - 1)
                    : value;
            var parsed = Float.parseFloat(normalized);
            if (!Float.isFinite(parsed)) {
                return withPosition(Result.error("Float literal out of range: `" + value + "`"), floatValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(Result.error("Invalid float literal: `" + floatValue.floatValue() + "`"), floatValue.position());
        }
        return Result.success(new CompiledFloatValue(floatValue.floatValue()));
    }

    private Result<CompiledExpression> linkByteValue(ByteValue byteValue, Scope scope) {
        try {
            var raw = byteValue.byteValue();
            var digits = raw.substring(2);
            var parsed = new BigInteger(digits, 16);
            if (parsed.compareTo(BigInteger.ZERO) < 0 || parsed.compareTo(BigInteger.valueOf(255)) > 0) {
                return withPosition(Result.error("Byte literal out of range: `" + raw + "`"), byteValue.position());
            }
        } catch (RuntimeException e) {
            return withPosition(Result.error("Invalid byte literal: `" + byteValue.byteValue() + "`"), byteValue.position());
        }
        return Result.success(new CompiledByteValue(byteValue.byteValue()));
    }

    private Result<CompiledExpression> linkDoubleValue(DoubleValue doubleValue, Scope scope) {
        try {
            var raw = doubleValue.doubleValue();
            var normalized = raw.endsWith("d") || raw.endsWith("D")
                    ? raw.substring(0, raw.length() - 1)
                    : raw;
            var parsed = Double.parseDouble(normalized);
            if (!Double.isFinite(parsed)) {
                return withPosition(Result.error("Double literal out of range: `" + raw + "`"), doubleValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(Result.error("Invalid double literal: `" + doubleValue.doubleValue() + "`"), doubleValue.position());
        }
        return Result.success(new CompiledDoubleValue(doubleValue.doubleValue()));
    }

    private Result<CompiledExpression> linkFunctionCall(FunctionCall functionCall, Scope scope) {
        if (functionCall.moduleName().isPresent()) {
            return resolveQualifiedFunctionCall(functionCall, scope);
        }
        var functionVariable = resolveFunctionVariable(functionCall.name(), scope);
        if (functionVariable.isPresent()) {
            return resolveFunctionInvoke(functionCall, scope, functionVariable.get());
        }
        return resolveGlobalFunctionCall(functionCall, scope);
    }

    private Result<CompiledExpression> linkFunctionInvoke(FunctionInvoke functionInvoke, Scope scope) {
        return linkExpression(functionInvoke.function(), scope)
                .flatMap(function -> resolveFunctionInvoke(functionInvoke, scope, function));
    }

    private Result<CompiledExpression> resolveQualifiedFunctionCall(FunctionCall functionCall, Scope scope) {
        var rawModuleName = functionCall.moduleName().orElseThrow();
        var moduleName = normalizeQualifiedModuleName(rawModuleName);
        var enumQualified = resolveEnumQualifiedFunctionCall(functionCall, scope, moduleName);
        if (enumQualified.isPresent()) {
            return enumQualified.get();
        }
        var resolvedModule = resolveQualifiedModule(moduleName);
        if (resolvedModule == null) {
            return withPosition(
                    Result.error("Unknown module `" + rawModuleName + "` in call `" + rawModuleName + "." + functionCall.name() + "`"),
                    functionCall.position()
            );
        }
        var signatures = resolvedModule.signatures();

        var candidates = signatures.stream()
                .filter(signature -> signature.name().equals(functionCall.name()))
                .filter(signature -> signature.parameterTypes().size() == functionCall.arguments().size())
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    Result.error(
                            "Module `" + moduleName + "` has no function `" + functionCall.name() + "` with "
                            + functionCall.arguments().size() + " argument(s)"
                    ),
                    functionCall.position()
            );
        }

        ResolvedFunctionCall best = null;
        Result.Error.SingleError deepestError = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (maybeResolved instanceof Result.Error<CoercedArguments> error) {
                deepestError = preferDeeperError(deepestError, firstError(error));
                continue;
            }
            if (!(maybeResolved instanceof Result.Success<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            if (deepestError != null) {
                return new Result.Error<>(deepestError);
            }
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof Result.Success<CompiledExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return withPosition(
                    Result.error(
                            "No matching function `" + moduleName + "." + functionCall.name() + "` for argument types " + actualTypes
                    ),
                    functionCall.position()
            );
        }

        return Result.success(new CompiledFunctionCall(
                resolvedModule.javaModuleName() + "." + functionCall.name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private Optional<Result<CompiledExpression>> resolveEnumQualifiedFunctionCall(
            FunctionCall functionCall,
            Scope scope,
            String normalizedModuleName
    ) {
        var enumType = findEnumTypeByName(normalizedModuleName);
        if (enumType == null) {
            return Optional.empty();
        }
        if (!"parse".equals(functionCall.name()) || functionCall.arguments().size() != 1) {
            return Optional.of(withPosition(
                    Result.error("Enum `" + enumType.name() + "` supports only `parse(string)` and `parse(int)`"),
                    functionCall.position()
            ));
        }
        var linkedArgument = linkExpression(functionCall.arguments().getFirst(), scope);
        if (linkedArgument instanceof Result.Error<CompiledExpression> error) {
            return Optional.of(new Result.Error<>(error.errors()));
        }
        var argument = ((Result.Success<CompiledExpression>) linkedArgument).value();
        if (argument.type() != STRING && argument.type() != INT) {
            return Optional.of(withPosition(
                    Result.error("Enum `parse` expects `string` or `int`, got `" + argument.type() + "`"),
                    functionCall.arguments().getFirst().position()
            ));
        }
        var resultType = resultTypeFor(enumType);
        if (resultType == null) {
            return Optional.of(withPosition(Result.error("Result type not found"), functionCall.position()));
        }
        return Optional.of(Result.success(new CompiledFunctionCall(
                enumType.name() + ".parse",
                List.of(argument),
                resultType
        )));
    }

    private ResolvedModule resolveQualifiedModule(String normalizedModuleName) {
        var direct = functionSignaturesByModule.get(normalizedModuleName);
        if (direct != null) {
            return new ResolvedModule(
                    moduleClassNameByModuleName.getOrDefault(normalizedModuleName, normalizedModuleName),
                    direct
            );
        }
        var lastSlash = normalizedModuleName.lastIndexOf('/');
        if (lastSlash >= 0 && lastSlash < normalizedModuleName.length() - 1) {
            var moduleName = normalizedModuleName.substring(lastSlash + 1);
            var signatures = functionSignaturesByModule.get(moduleName);
            if (signatures != null) {
                return new ResolvedModule(moduleClassNameByModuleName.getOrDefault(moduleName, moduleName), signatures);
            }
        }
        var lastDot = normalizedModuleName.lastIndexOf('.');
        if (lastDot >= 0 && lastDot < normalizedModuleName.length() - 1) {
            var moduleName = normalizedModuleName.substring(lastDot + 1);
            var signatures = functionSignaturesByModule.get(moduleName);
            if (signatures != null) {
                return new ResolvedModule(moduleClassNameByModuleName.getOrDefault(moduleName, moduleName), signatures);
            }
        }
        return null;
    }

    private String normalizeQualifiedModuleName(String moduleName) {
        var normalized = moduleName.replace('\\', '/');
        if (normalized.startsWith("/")) {
            normalized = normalized.substring(1);
            return normalized.replace("/", ".");
        }
        return normalized;
    }

    private Result<CompiledExpression> resolveGlobalFunctionCall(FunctionCall functionCall, Scope scope) {
        if (functionCall.name().startsWith(METHOD_INVOKE_PREFIX)) {
            return resolveMethodInvokeCall(functionCall, scope);
        }
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionCall.name()))
                .filter(signature -> signature.parameterTypes().size() == functionCall.arguments().size())
                .toList();
        if (candidates.isEmpty()) {
            if (functionCall.arguments().isEmpty()) {
                var singleton = findSingletonDataType(functionCall.name());
                if (singleton.isPresent()) {
                    return Result.success(new CompiledNewData(singleton.get(), List.of()));
                }
            }
            return functionCall.arguments()
                    .stream()
                    .map((Expression expression) -> linkExpression(expression, scope))
                    .collect(new ResultCollectionCollector<>())
                    .map(args -> (CompiledExpression) new CompiledFunctionCall(functionCall.name(), args, ANY));
        }

        ResolvedFunctionCall best = null;
        Result.Error.SingleError deepestError = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (maybeResolved instanceof Result.Error<CoercedArguments> error) {
                deepestError = preferDeeperError(deepestError, firstError(error));
                continue;
            }
            if (!(maybeResolved instanceof Result.Success<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            if (deepestError != null) {
                return new Result.Error<>(deepestError);
            }
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof Result.Success<CompiledExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return withPosition(
                    Result.error(
                            "No matching function `" + functionCall.name() + "` for argument types " + actualTypes
                    ),
                    functionCall.position()
            );
        }
        return Result.success(new CompiledFunctionCall(
                functionCall.name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private Optional<CompiledDataType> findSingletonDataType(String constructorName) {
        var direct = dataTypes.values().stream()
                .filter(CompiledDataType.class::isInstance)
                .map(CompiledDataType.class::cast)
                .filter(CompiledDataType::singleton)
                .filter(dataType -> typeNameMatches(dataType.name(), constructorName))
                .findFirst();
        if (direct.isPresent()) {
            return direct;
        }
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(CompiledDataParentType::enumType)
                .flatMap(parentType -> parentType.subTypes().stream())
                .filter(dataType -> typeNameMatches(dataType.name(), constructorName))
                .findFirst();
    }

    private Optional<Result<CompiledExpression>> tryLinkEnumValuesFieldAccess(FieldAccess fieldAccess, Scope scope) {
        if (!(fieldAccess.source() instanceof FunctionCall functionCall)
            || functionCall.moduleName().isPresent()
            || !functionCall.arguments().isEmpty()
            || !"values".equals(fieldAccess.field())) {
            return Optional.empty();
        }
        var enumType = findEnumTypeByName(functionCall.name());
        if (enumType == null) {
            return Optional.empty();
        }
        var setType = new CompiledSet(enumType);
        return Optional.of(Result.success(new CompiledFunctionCall(enumType.name() + ".valuesSet", List.of(), setType)));
    }

    private CompiledDataParentType findEnumParentForValue(String valueName) {
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(CompiledDataParentType::enumType)
                .filter(parentType -> parentType.subTypes().stream().anyMatch(value -> typeNameMatches(value.name(), valueName)))
                .findFirst()
                .orElse(null);
    }

    private CompiledDataParentType findEnumTypeByName(String enumName) {
        var normalizedLookup = enumName.replace('/', '.');
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(CompiledDataParentType::enumType)
                .filter(parentType ->
                        typeNameMatches(parentType.name(), enumName)
                        || typeNameMatches(parentType.name(), normalizedLookup)
                        || normalizeQualifiedTypeName(parentType.name()).replace('/', '.').endsWith("." + normalizedLookup)
                        || normalizeQualifiedTypeName(parentType.name()).replace('/', '.').endsWith(normalizedLookup))
                .findFirst()
                .orElse(null);
    }

    private boolean typeNameMatches(String typeName, String constructorName) {
        if (typeName.equals(constructorName)) {
            return true;
        }
        var normalizedType = normalizeQualifiedTypeName(typeName);
        var normalizedConstructor = normalizeQualifiedTypeName(constructorName);
        return normalizedType.equals(normalizedConstructor)
               || normalizedType.endsWith("/" + constructorName)
               || normalizedType.endsWith("." + constructorName)
               || normalizedConstructor.endsWith("/" + typeName)
               || normalizedConstructor.endsWith("." + typeName);
    }

    private String normalizeQualifiedTypeName(String typeName) {
        var normalized = typeName.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private Result<CompiledExpression> resolveMethodInvokeCall(FunctionCall functionCall, Scope scope) {
        var methodName = functionCall.name().substring(METHOD_INVOKE_PREFIX.length());
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().startsWith(METHOD_DECL_PREFIX))
                .filter(signature -> signature.name().endsWith("__" + methodName))
                .filter(signature -> signature.parameterTypes().size() == functionCall.arguments().size())
                .toList();
        if (candidates.isEmpty()) {
            return resolveBuiltinMethodInvoke(functionCall, scope, methodName)
                    .orElseGet(() -> withPosition(
                            Result.error("No method `" + methodName + "` with " + functionCall.arguments().size() + " argument(s)"),
                            functionCall.position()
                    ));
        }

        ResolvedFunctionCall best = null;
        Result.Error.SingleError deepestError = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (maybeResolved instanceof Result.Error<CoercedArguments> error) {
                deepestError = preferDeeperError(deepestError, firstError(error));
                continue;
            }
            if (!(maybeResolved instanceof Result.Success<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            var builtin = resolveBuiltinMethodInvoke(functionCall, scope, methodName);
            if (builtin.isPresent()) {
                return builtin.get();
            }
            if (deepestError != null) {
                return new Result.Error<>(deepestError);
            }
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof Result.Success<CompiledExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return withPosition(
                    Result.error("No matching method `" + methodName + "` for argument types " + actualTypes),
                    functionCall.position()
            );
        }
        return Result.success(new CompiledFunctionCall(
                best.signature().name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private CompiledType resolveReturnType(FunctionSignature signature, List<CompiledExpression> arguments) {
        if (signature.parameterTypes().isEmpty() || arguments.isEmpty()) {
            return signature.returnType();
        }
        var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
        var count = Math.min(signature.parameterTypes().size(), arguments.size());
        for (var i = 0; i < count; i++) {
            var expectedType = signature.parameterTypes().get(i);
            var argument = arguments.get(i);
            collectTypeSubstitutions(expectedType, argument.type(), substitutions);
            if (expectedType instanceof CompiledFunctionType expectedFunction
                && argument instanceof CompiledLambdaExpression lambdaExpression) {
                collectLambdaReturnTypeSubstitutions(expectedFunction, lambdaExpression, substitutions);
            }
        }
        if (substitutions.isEmpty()) {
            return signature.returnType();
        }
        return substituteTypeParameters(signature.returnType(), substitutions);
    }

    private void collectLambdaReturnTypeSubstitutions(
            CompiledFunctionType expectedFunction,
            CompiledLambdaExpression lambdaExpression,
            Map<String, CompiledType> substitutions
    ) {
        var lambdaReturnType = lambdaExpression.expression().type();
        if (lambdaReturnType instanceof CompiledGenericTypeParameter || !isResolvedTypeForInference(lambdaReturnType)) {
            return;
        }
        collectTypeSubstitutions(expectedFunction.returnType(), lambdaReturnType, substitutions);
    }

    private void collectTypeSubstitutions(CompiledType expected, CompiledType actual, Map<String, CompiledType> substitutions) {
        if (expected instanceof CompiledGenericTypeParameter genericTypeParameter) {
            substitutions.putIfAbsent(genericTypeParameter.name(), actual);
            return;
        }
        if (expected instanceof CompiledList expectedList && actual instanceof CompiledList actualList) {
            collectTypeSubstitutions(expectedList.elementType(), actualList.elementType(), substitutions);
            return;
        }
        if (expected instanceof CompiledSet expectedSet && actual instanceof CompiledSet actualSet) {
            collectTypeSubstitutions(expectedSet.elementType(), actualSet.elementType(), substitutions);
            return;
        }
        if (expected instanceof CompiledDict expectedDict && actual instanceof CompiledDict actualDict) {
            collectTypeSubstitutions(expectedDict.valueType(), actualDict.valueType(), substitutions);
            return;
        }
        if (expected instanceof CompiledFunctionType expectedFunction && actual instanceof CompiledFunctionType actualFunction) {
            collectTypeSubstitutions(expectedFunction.argumentType(), actualFunction.argumentType(), substitutions);
            collectTypeSubstitutions(expectedFunction.returnType(), actualFunction.returnType(), substitutions);
            return;
        }
        if (expected instanceof CompiledTupleType expectedTuple && actual instanceof CompiledTupleType actualTuple) {
            var count = Math.min(expectedTuple.elementTypes().size(), actualTuple.elementTypes().size());
            for (var i = 0; i < count; i++) {
                collectTypeSubstitutions(expectedTuple.elementTypes().get(i), actualTuple.elementTypes().get(i), substitutions);
            }
            return;
        }
        if (expected instanceof CompiledDataParentType expectedParent && actual instanceof CompiledDataType actualData) {
            if (!isSubtypeOfParent(actualData, expectedParent)) {
                return;
            }
            var expectedTypeParameters = expectedParent.typeParameters();
            var actualTypeParameters = actualData.typeParameters();
            var count = Math.min(expectedTypeParameters.size(), actualTypeParameters.size());
            for (var i = 0; i < count; i++) {
                collectTypeSubstitutionsFromDescriptors(
                        expectedTypeParameters.get(i),
                        actualTypeParameters.get(i),
                        substitutions
                );
            }
            return;
        }
        if (expected instanceof GenericDataType expectedData && actual instanceof GenericDataType actualData) {
            if (!sameRawTypeName(expectedData.name(), actualData.name())) {
                return;
            }
            var expectedTypeParameters = switch (expectedData) {
                case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
                case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            };
            var actualTypeParameters = switch (actualData) {
                case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
                case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            };
            var count = Math.min(expectedTypeParameters.size(), actualTypeParameters.size());
            for (var i = 0; i < count; i++) {
                collectTypeSubstitutionsFromDescriptors(
                        expectedTypeParameters.get(i),
                        actualTypeParameters.get(i),
                        substitutions
                );
            }
        }
    }

    private void collectTypeSubstitutionsFromDescriptors(
            String expectedDescriptor,
            String actualDescriptor,
            Map<String, CompiledType> substitutions
    ) {
        var maybeActual = parseLinkedTypeDescriptor(actualDescriptor);
        if (maybeActual.isEmpty()) {
            return;
        }
        var maybeExpected = parseLinkedTypeDescriptor(expectedDescriptor);
        if (maybeExpected.isPresent()) {
            collectTypeSubstitutions(maybeExpected.get(), maybeActual.get(), substitutions);
            return;
        }
        var expectedName = expectedDescriptor == null ? "" : expectedDescriptor.trim();
        if (!expectedName.isEmpty()) {
            substitutions.putIfAbsent(expectedName, maybeActual.get());
        }
    }

    private Optional<Result<CompiledExpression>> resolveBuiltinMethodInvoke(FunctionCall functionCall, Scope scope, String methodName) {
        var supportsBoolTwoStrings = "contains".equals(methodName)
                || "starts_with".equals(methodName)
                || "end_with".equals(methodName);
        var supportsStringThreeArgs = "replace".equals(methodName);
        var supportsTrim = "trim".equals(methodName);
        var supportsIsEmpty = "is_empty".equals(methodName);
        var supportsToInt = "to_int".equals(methodName);
        var supportsToLong = "to_long".equals(methodName);
        var supportsToDouble = "to_double".equals(methodName);
        var supportsToFloat = "to_float".equals(methodName);
        var supportsToBool = "to_bool".equals(methodName);
        var supportsSingleString = supportsTrim || supportsIsEmpty
                                   || supportsToInt || supportsToLong || supportsToDouble || supportsToFloat || supportsToBool;
        if ((!supportsBoolTwoStrings && !supportsStringThreeArgs && !supportsSingleString)
                || (supportsBoolTwoStrings && functionCall.arguments().size() != 2)
                || (supportsStringThreeArgs && functionCall.arguments().size() != 3)
                || (supportsSingleString && functionCall.arguments().size() != 1)) {
            return Optional.empty();
        }
        var linkedArguments = functionCall.arguments().stream()
                .map(argument -> linkExpression(argument, scope))
                .collect(new ResultCollectionCollector<>());
        if (!(linkedArguments instanceof Result.Success<java.util.List<CompiledExpression>> value)) {
            if (linkedArguments instanceof Result.Error<java.util.List<CompiledExpression>> error) {
                return Optional.of(new Result.Error<>(error.errors()));
            }
            return Optional.empty();
        }
        var args = value.value();
        if (supportsBoolTwoStrings || supportsStringThreeArgs) {
            var allStrings = args.stream().allMatch(argument -> argument.type() == STRING);
            if (!allStrings) {
                return Optional.empty();
            }
            var returnType = supportsBoolTwoStrings ? BOOL : STRING;
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__" + methodName,
                    args,
                    returnType
            )));
        }
        if (args.get(0).type() != STRING) {
            return Optional.empty();
        }
        if (supportsIsEmpty) {
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__is_empty",
                    args,
                    BOOL
            )));
        }
        if (supportsToInt) {
            var resultType = resultTypeFor(INT);
            if (resultType == null) {
                return Optional.of(withPosition(Result.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_int",
                    args,
                    resultType
            )));
        }
        if (supportsToLong) {
            var resultType = resultTypeFor(LONG);
            if (resultType == null) {
                return Optional.of(withPosition(Result.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_long",
                    args,
                    resultType
            )));
        }
        if (supportsToDouble) {
            var resultType = resultTypeFor(DOUBLE);
            if (resultType == null) {
                return Optional.of(withPosition(Result.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_double",
                    args,
                    resultType
            )));
        }
        if (supportsToFloat) {
            var resultType = resultTypeFor(FLOAT);
            if (resultType == null) {
                return Optional.of(withPosition(Result.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_float",
                    args,
                    resultType
            )));
        }
        if (supportsToBool) {
            var resultType = resultTypeFor(BOOL);
            if (resultType == null) {
                return Optional.of(withPosition(Result.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(Result.success(new CompiledFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_bool",
                    args,
                    resultType
            )));
        }
        return Optional.of(Result.success(new CompiledFunctionCall(
                METHOD_DECL_PREFIX + "String__trim",
                args,
                STRING
        )));
    }

    private Optional<CompiledVariable> resolveFunctionVariable(String name, Scope scope) {
        if (scope.localValues().get(name) instanceof CompiledFunctionType functionType) {
            return Optional.of(new CompiledVariable(name, functionType));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(name))
                .filter(parameter -> parameter.type() instanceof CompiledFunctionType)
                .findFirst()
                .map(parameter -> new CompiledVariable(parameter.name(), parameter.type()));
    }

    private Result<CompiledExpression> resolveFunctionInvoke(
            FunctionCall functionCall,
            Scope scope,
            CompiledVariable function
    ) {
        if (!(function.type() instanceof CompiledFunctionType functionType)) {
            return withPosition(Result.error("Variable `" + function.name() + "` is not callable"), functionCall.position());
        }
        if (functionCall.arguments().isEmpty()) {
            if (functionType.argumentType().equals(NOTHING)) {
                return Result.success(new CompiledFunctionInvoke(function, List.of(), functionType.returnType()));
            }
            return withPosition(
                    Result.error("Function variable `" + function.name() + "` requires at least one argument"),
                    functionCall.position()
            );
        }
        var currentType = (CompiledType) functionType;
        var coercedArguments = new java.util.ArrayList<CompiledExpression>(functionCall.arguments().size());
        for (var argument : functionCall.arguments()) {
            if (!(currentType instanceof CompiledFunctionType currentFunctionType)) {
                return withPosition(
                        Result.error("Function variable `" + function.name() + "` called with too many arguments"),
                        functionCall.position()
                );
            }
            var linked = linkArgumentForExpectedType(argument, scope, currentFunctionType.argumentType());
            if (linked instanceof Result.Error<CoercedArgument> error) {
                return withPosition(new Result.Error<>(error.errors()), argument.position());
            }
            var coerced = ((Result.Success<CoercedArgument>) linked).value();
            coercedArguments.add(coerced.expression());
            currentType = currentFunctionType.returnType();
        }

        return Result.success(new CompiledFunctionInvoke(function, List.copyOf(coercedArguments), currentType));
    }

    private Result<CompiledExpression> resolveFunctionInvoke(
            FunctionInvoke functionInvoke,
            Scope scope,
            CompiledExpression function
    ) {
        if (!(function.type() instanceof CompiledFunctionType functionType)) {
            return withPosition(Result.error("Expression is not callable, was `" + function.type() + "`"), functionInvoke.position());
        }
        if (functionInvoke.arguments().isEmpty()) {
            if (functionType.argumentType().equals(NOTHING)) {
                return Result.success(new CompiledFunctionInvoke(function, List.of(), functionType.returnType()));
            }
            return withPosition(
                    Result.error("Callable expression requires at least one argument"),
                    functionInvoke.position()
            );
        }
        var currentType = (CompiledType) functionType;
        var coercedArguments = new java.util.ArrayList<CompiledExpression>(functionInvoke.arguments().size());
        for (var argument : functionInvoke.arguments()) {
            if (!(currentType instanceof CompiledFunctionType currentFunctionType)) {
                return withPosition(
                        Result.error("Callable expression invoked with too many arguments"),
                        functionInvoke.position()
                );
            }
            var linked = linkArgumentForExpectedType(argument, scope, currentFunctionType.argumentType());
            if (linked instanceof Result.Error<CoercedArgument> error) {
                return withPosition(new Result.Error<>(error.errors()), argument.position());
            }
            var coerced = ((Result.Success<CoercedArgument>) linked).value();
            coercedArguments.add(coerced.expression());
            currentType = currentFunctionType.returnType();
        }

        return Result.success(new CompiledFunctionInvoke(function, List.copyOf(coercedArguments), currentType));
    }

    private Result<CoercedArguments> linkArgumentsForExpectedTypes(
            List<Expression> arguments,
            Scope scope,
            List<CompiledType> expectedTypes
    ) {
        var coerced = new java.util.ArrayList<CompiledExpression>(arguments.size());
        var coercions = 0;
        var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
        for (var i = 0; i < arguments.size(); i++) {
            var argument = arguments.get(i);
            var expected = substituteTypeParameters(expectedTypes.get(i), substitutions);
            var maybeCoerced = linkArgumentForExpectedType(argument, scope, expected);
            if (maybeCoerced instanceof Result.Error<CoercedArgument> error) {
                return new Result.Error<>(error.errors());
            }
            var value = ((Result.Success<CoercedArgument>) maybeCoerced).value();
            coerced.add(value.expression());
            coercions += value.coercions();
            collectTypeSubstitutions(expected, value.expression().type(), substitutions);
            if (expected instanceof CompiledFunctionType expectedFunction
                && value.expression() instanceof CompiledLambdaExpression lambdaExpression) {
                collectLambdaReturnTypeSubstitutions(expectedFunction, lambdaExpression, substitutions);
            }
        }
        return Result.success(new CoercedArguments(List.copyOf(coerced), coercions));
    }

    private Result<CoercedArgument> linkArgumentForExpectedType(Expression argument, Scope scope, CompiledType expected) {
        if (argument instanceof TupleExpression tupleExpression
            && expected instanceof CompiledTupleType expectedTupleType) {
            if (tupleExpression.values().size() != expectedTupleType.elementTypes().size()) {
                return withPosition(
                        Result.error("Expected `" + expected + "`, got tuple with "
                                           + tupleExpression.values().size() + " element(s)"),
                        argument.position()
                );
            }
            var linkedElements = new java.util.ArrayList<CompiledExpression>(tupleExpression.values().size());
            var coercions = 0;
            for (var i = 0; i < tupleExpression.values().size(); i++) {
                var elementExpression = tupleExpression.values().get(i);
                var elementExpectedType = expectedTupleType.elementTypes().get(i);
                var maybeLinkedElement = linkArgumentForExpectedType(elementExpression, scope, elementExpectedType);
                if (maybeLinkedElement instanceof Result.Error<CoercedArgument>) {
                    return (Result<CoercedArgument>) maybeLinkedElement;
                }
                var linkedElement = ((Result.Success<CoercedArgument>) maybeLinkedElement).value();
                linkedElements.add(linkedElement.expression());
                coercions += linkedElement.coercions();
            }
            return Result.success(new CoercedArgument(
                    new CompiledTupleExpression(
                            List.copyOf(linkedElements),
                            new CompiledTupleType(
                                    linkedElements.stream()
                                            .map(CompiledExpression::type)
                                            .toList()
                            )
                    ),
                    coercions
            ));
        }
        if (argument instanceof LambdaExpression lambdaExpression) {
            if (expected instanceof CompiledFunctionType functionType) {
                return linkLambdaExpression(lambdaExpression, scope, functionType)
                        .map(linkedLambda -> new CoercedArgument(linkedLambda, 0));
            }
            return withPosition(
                    Result.error("Lambda expression can only be used where function type is expected"),
                    argument.position()
            );
        }
        if (argument instanceof FunctionReference functionReference) {
            if (expected instanceof CompiledFunctionType functionType) {
                return linkFunctionReference(functionReference, functionType)
                        .map(linkedReference -> new CoercedArgument(linkedReference, 0));
            }
            return withPosition(
                    Result.error("Function reference can only be used where function type is expected"),
                    argument.position()
            );
        }
        if (argument instanceof NewListExpression newListExpression && expected instanceof CompiledList expectedList) {
            return linkNewListExpression(newListExpression, scope, expectedList.elementType())
                    .flatMap(linkedArgument -> {
                        var maybeCoerced = coerceArgument(linkedArgument, expected);
                        if (maybeCoerced == null) {
                            return withPosition(
                                    Result.error("Expected `" + expected + "`, got `" + linkedArgument.type() + "`"),
                                    argument.position()
                            );
                        }
                        return Result.success(maybeCoerced);
                    });
        }
        if (argument instanceof NewSetExpression newSetExpression && expected instanceof CompiledSet expectedSet) {
            return linkNewSetExpression(newSetExpression, scope, expectedSet.elementType())
                    .flatMap(linkedArgument -> {
                        var maybeCoerced = coerceArgument(linkedArgument, expected);
                        if (maybeCoerced == null) {
                            return withPosition(
                                    Result.error("Expected `" + expected + "`, got `" + linkedArgument.type() + "`"),
                                    argument.position()
                            );
                        }
                        return Result.success(maybeCoerced);
                    });
        }
        if (argument instanceof NewDictExpression newDictExpression && expected instanceof CompiledDict expectedDict) {
            return linkNewDictExpression(newDictExpression, scope, expectedDict.valueType())
                    .flatMap(linkedArgument -> {
                        var maybeCoerced = coerceArgument(linkedArgument, expected);
                        if (maybeCoerced == null) {
                            return withPosition(
                                    Result.error("Expected `" + expected + "`, got `" + linkedArgument.type() + "`"),
                                    argument.position()
                            );
                        }
                        return Result.success(maybeCoerced);
                    });
        }

        return linkExpression(argument, scope)
                .flatMap(linkedArgument -> {
                    var maybeCoerced = coerceArgument(linkedArgument, expected);
                    if (maybeCoerced == null) {
                        return withPosition(
                                Result.error("Expected `" + expected + "`, got `" + linkedArgument.type() + "`"),
                                argument.position()
                        );
                    }
                    return Result.success(maybeCoerced);
                });
    }

    private Result<CompiledExpression> linkFunctionReference(FunctionReference functionReference, CompiledFunctionType expectedType) {
        var expectedShape = flattenFunctionType(expectedType);
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .filter(signature -> signature.parameterTypes().size() == expectedShape.parameterTypes().size())
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    Result.error("Function `" + functionReference.name() + "` with "
                                       + expectedShape.parameterTypes().size() + " argument(s) not found"),
                    functionReference.position()
            );
        }

        ResolvedFunctionReference best = null;
        for (var candidate : candidates) {
            var resolved = resolveFunctionReferenceCandidate(candidate, expectedShape);
            if (resolved == null) {
                continue;
            }
            if (best == null || resolved.coercions() < best.coercions()) {
                best = resolved;
            }
        }
        if (best == null) {
            return withPosition(
                    Result.error("Function reference `" + functionReference.name() + "` is not compatible with `" + expectedType + "`"),
                    functionReference.position()
            );
        }
        return Result.success(best.expression());
    }

    private Result<CompiledExpression> linkFunctionReference(FunctionReference functionReference, Scope scope) {
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    Result.error("Function `" + functionReference.name() + "` not found"),
                    functionReference.position()
            );
        }
        if (candidates.size() > 1) {
            return withPosition(
                    Result.error("Function reference `" + functionReference.name() + "` is ambiguous. Add expected function type."),
                    functionReference.position()
            );
        }
        return Result.success(toFunctionReferenceLambda(candidates.getFirst()));
    }

    private CompiledExpression toFunctionReferenceLambda(FunctionSignature candidate) {
        var argumentNames = new java.util.ArrayList<String>(candidate.parameterTypes().size());
        var callArguments = new java.util.ArrayList<CompiledExpression>(candidate.parameterTypes().size());
        for (int i = 0; i < candidate.parameterTypes().size(); i++) {
            var argumentName = "arg" + i;
            argumentNames.add(argumentName);
            callArguments.add(new CompiledVariable(argumentName, candidate.parameterTypes().get(i)));
        }

        CompiledExpression expression = new CompiledFunctionCall(candidate.name(), List.copyOf(callArguments), candidate.returnType());
        var nestedType = candidate.returnType();
        for (int i = argumentNames.size() - 1; i >= 0; i--) {
            var functionType = new CompiledFunctionType(candidate.parameterTypes().get(i), nestedType);
            expression = new CompiledLambdaExpression(argumentNames.get(i), expression, functionType);
            nestedType = functionType;
        }
        return expression;
    }

    private ResolvedFunctionReference resolveFunctionReferenceCandidate(FunctionSignature candidate, FunctionShape expectedShape) {
        var argumentNames = new java.util.ArrayList<String>(expectedShape.parameterTypes().size());
        var callArguments = new java.util.ArrayList<CompiledExpression>(expectedShape.parameterTypes().size());
        var coercions = 0;

        for (int i = 0; i < expectedShape.parameterTypes().size(); i++) {
            var argumentName = "arg" + i;
            argumentNames.add(argumentName);
            var argumentVariable = new CompiledVariable(argumentName, expectedShape.parameterTypes().get(i));
            var coerced = coerceArgument(argumentVariable, candidate.parameterTypes().get(i));
            if (coerced == null) {
                return null;
            }
            callArguments.add(coerced.expression());
            coercions += coerced.coercions();
        }

        CompiledExpression expression = new CompiledFunctionCall(candidate.name(), List.copyOf(callArguments), candidate.returnType());
        var returnCoerced = coerceArgument(expression, expectedShape.returnType());
        if (returnCoerced == null) {
            if (candidate.returnType() != ANY) {
                return null;
            }
            // First linking pass can expose unknown return type (ANY); keep expected return so relinking can refine it.
            expression = new CompiledFunctionCall(candidate.name(), List.copyOf(callArguments), expectedShape.returnType());
            coercions += 1;
        } else {
            expression = returnCoerced.expression();
            coercions += returnCoerced.coercions();
        }

        var nestedType = expectedShape.returnType();
        for (int i = argumentNames.size() - 1; i >= 0; i--) {
            var functionType = new CompiledFunctionType(expectedShape.parameterTypes().get(i), nestedType);
            expression = new CompiledLambdaExpression(argumentNames.get(i), expression, functionType);
            nestedType = functionType;
        }

        return new ResolvedFunctionReference(expression, coercions);
    }

    private Result<CompiledLambdaExpression> linkLambdaExpression(
            LambdaExpression lambdaExpression,
            Scope scope,
            CompiledFunctionType expectedType
    ) {
        var argumentNames = lambdaExpression.argumentNames();
        var noArgsLambda = argumentNames.isEmpty();
        if (noArgsLambda && !expectedType.argumentType().equals(NOTHING)) {
            return withPosition(
                    Result.error("Lambda expects 0 argument(s), but target function type is `" + expectedType + "`"),
                    lambdaExpression.position()
            );
        }

        var argumentTypes = new java.util.ArrayList<CompiledType>(argumentNames.size());
        var returnType = expectedReturnTypeForLambda(expectedType, argumentNames.size(), noArgsLambda)
                .orElse(null);
        if (returnType == null) {
            return withPosition(
                    Result.error("Lambda expects " + argumentNames.size() + " argument(s), but target function type is `" + expectedType + "`"),
                    lambdaExpression.position()
            );
        }
        var currentType = (CompiledType) expectedType;
        for (int idx = 0; idx < argumentNames.size(); idx++) {
            var currentFunctionType = (CompiledFunctionType) currentType;
            argumentTypes.add(currentFunctionType.argumentType());
            currentType = currentFunctionType.returnType();
        }

        var lambdaScope = scope;
        for (int idx = 0; idx < argumentNames.size(); idx++) {
            lambdaScope = addLambdaBinding(lambdaScope, argumentNames.get(idx), argumentTypes.get(idx));
        }

        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .flatMap(linkedBody -> {
                    var maybeCoerced = coerceArgument(linkedBody, returnType);
                    if (maybeCoerced == null) {
                        return withPosition(
                                Result.error(
                                        "Lambda has to return `" + returnType
                                        + "`, got `" + linkedBody.type() + "`"
                                ),
                                lambdaExpression.position()
                        );
                    }

                    CompiledExpression nested = maybeCoerced.expression();
                    CompiledType nestedType = returnType;
                    if (!(nested.type() instanceof CompiledGenericTypeParameter)
                        && isResolvedTypeForInference(nested.type())) {
                        // Preserve the concrete linked return type whenever coercion succeeded so
                        // generic method inference can substitute placeholders like `Y`.
                        nestedType = nested.type();
                    } else if (returnType instanceof CompiledGenericTypeParameter
                               && !(nested.type() instanceof CompiledGenericTypeParameter)) {
                        nestedType = nested.type();
                    }
                    for (int idx = argumentNames.size() - 1; idx >= 0; idx--) {
                        var functionType = new CompiledFunctionType(argumentTypes.get(idx), nestedType);
                        nested = new CompiledLambdaExpression(argumentNames.get(idx), nested, functionType);
                        nestedType = functionType;
                    }
                    if (noArgsLambda) {
                        nested = new CompiledLambdaExpression("__capybaraNoArgs", nested, new CompiledFunctionType(NOTHING, nestedType));
                    }
                    return Result.success((CompiledLambdaExpression) nested);
                });
    }

    private Optional<CompiledType> expectedReturnTypeForLambda(CompiledFunctionType expectedType, int argumentCount, boolean noArgsLambda) {
        if (noArgsLambda) {
            return expectedType.argumentType().equals(NOTHING)
                    ? Optional.of(expectedType.returnType())
                    : Optional.empty();
        }
        CompiledType current = expectedType;
        for (int idx = 0; idx < argumentCount; idx++) {
            if (!(current instanceof CompiledFunctionType currentFunctionType)) {
                return Optional.empty();
            }
            current = currentFunctionType.returnType();
        }
        return Optional.of(current);
    }

    private static FunctionShape flattenFunctionType(CompiledFunctionType functionType) {
        var parameterTypes = new java.util.ArrayList<CompiledType>();
        CompiledType current = functionType;
        while (current instanceof CompiledFunctionType linkedFunctionType) {
            parameterTypes.add(linkedFunctionType.argumentType());
            current = linkedFunctionType.returnType();
        }
        return new FunctionShape(List.copyOf(parameterTypes), current);
    }

    private CoercedArgument coerceArgument(CompiledExpression argument, CompiledType expected) {
        if (argument.type().equals(expected)) {
            return new CoercedArgument(argument, 0);
        }
        if (expected instanceof CompiledGenericTypeParameter) {
            return new CoercedArgument(argument, 1);
        }
        if (argument.type() instanceof CompiledGenericTypeParameter) {
            return new CoercedArgument(argument, 1);
        }
        if (expected == ANY) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledTupleType expectedTuple
            && argument.type() instanceof CompiledTupleType argumentTuple
            && areTupleTypesCompatible(argumentTuple, expectedTuple)) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledList expectedList
            && argument.type() instanceof CompiledList argumentList
            && isTypeCompatible(argumentList.elementType(), expectedList.elementType())) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledSet expectedSet
            && argument.type() instanceof CompiledSet argumentSet
            && isTypeCompatible(argumentSet.elementType(), expectedSet.elementType())) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledDict expectedDict
            && argument.type() instanceof CompiledDict argumentDict
            && isTypeCompatible(argumentDict.valueType(), expectedDict.valueType())) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledFunctionType expectedFunction
            && argument.type() instanceof CompiledFunctionType argumentFunction
            && areFunctionTypesCompatible(argumentFunction, expectedFunction)) {
            return new CoercedArgument(argument, 1);
        }
        if (!(expected instanceof CompiledFunctionType)
            && argument.type() instanceof CompiledFunctionType argumentFunction
            && argumentFunction.argumentType().equals(NOTHING)) {
            var invoked = new CompiledFunctionInvoke(argument, List.of(), argumentFunction.returnType());
            var invokedCoerced = coerceArgument(invoked, expected);
            if (invokedCoerced != null) {
                return new CoercedArgument(invokedCoerced.expression(), invokedCoerced.coercions() + 1);
            }
        }
        if (expected instanceof CompiledList expectedList
            && argument instanceof CompiledNewList linkedNewList
            && linkedNewList.values().isEmpty()) {
            return new CoercedArgument(new CompiledNewList(List.of(), new CompiledList(expectedList.elementType())), 1);
        }
        if (expected instanceof CompiledSet expectedSet
            && argument instanceof CompiledNewSet linkedNewSet
            && linkedNewSet.values().isEmpty()) {
            return new CoercedArgument(new CompiledNewSet(List.of(), new CompiledSet(expectedSet.elementType())), 1);
        }
        if (expected instanceof CompiledDict expectedDict
            && argument instanceof CompiledNewSet linkedNewSet
            && linkedNewSet.values().isEmpty()) {
            return new CoercedArgument(new CompiledNewDict(List.of(), new CompiledDict(expectedDict.valueType())), 1);
        }
        if (expected instanceof CompiledDict expectedDict
            && argument instanceof CompiledNewDict linkedNewDict
            && linkedNewDict.entries().isEmpty()) {
            return new CoercedArgument(new CompiledNewDict(List.of(), new CompiledDict(expectedDict.valueType())), 1);
        }
        if (argument.type() == NOTHING) {
            return new CoercedArgument(argument, 0);
        }
        if (expected == PrimitiveLinkedType.DATA
            && argument.type() instanceof GenericDataType) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledDataParentType expectedParent
            && argument.type() instanceof CompiledDataParentType argumentParent
            && sameRawTypeName(expectedParent.name(), argumentParent.name())
            && areTypeParameterDescriptorsCompatible(argumentParent.typeParameters(), expectedParent.typeParameters())) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledDataType expectedData
            && argument.type() instanceof CompiledDataType argumentData
            && sameRawTypeName(expectedData.name(), argumentData.name())
            && areTypeParameterDescriptorsCompatible(argumentData.typeParameters(), expectedData.typeParameters())) {
            return new CoercedArgument(argument, 1);
        }
        if (argument.type() == ANY) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledDataType expectedDataType
            && argument.type() instanceof CompiledDataType argumentDataType
            && isSubtype(argumentDataType, expectedDataType.name(), new java.util.HashSet<>())) {
            var assignments = expectedDataType.fields().stream()
                    .map(field -> new CompiledNewData.FieldAssignment(
                            field.name(),
                            new CompiledFieldAccess(argument, field.name(), field.type())
                    ))
                    .toList();
            return new CoercedArgument(new CompiledNewData(expectedDataType, assignments), 1);
        }
        if (expected instanceof CompiledDataParentType expectedParentType
            && argument.type() instanceof CompiledDataType argumentDataType
            && isSubtypeOfParent(argumentDataType, expectedParentType)
            && (argumentDataType.typeParameters().isEmpty()
                || areTypeParameterDescriptorsCompatible(argumentDataType.typeParameters(), expectedParentType.typeParameters()))) {
            return new CoercedArgument(argument, 1);
        }
        return null;
    }

    private boolean areTupleTypesCompatible(CompiledTupleType actual, CompiledTupleType expected) {
        if (actual.elementTypes().size() != expected.elementTypes().size()) {
            return false;
        }
        for (var i = 0; i < actual.elementTypes().size(); i++) {
            if (!isTypeCompatible(actual.elementTypes().get(i), expected.elementTypes().get(i))) {
                return false;
            }
        }
        return true;
    }

    private boolean areFunctionTypesCompatible(CompiledFunctionType actual, CompiledFunctionType expected) {
        return isTypeCompatible(actual.argumentType(), expected.argumentType())
               && isTypeCompatible(actual.returnType(), expected.returnType());
    }

    private boolean isTypeCompatible(CompiledType actual, CompiledType expected) {
        if (actual.equals(expected)) {
            return true;
        }
        if (expected == ANY || actual == ANY || actual == NOTHING) {
            return true;
        }
        if (expected instanceof CompiledGenericTypeParameter || actual instanceof CompiledGenericTypeParameter) {
            return true;
        }
        if (expected instanceof CompiledList expectedList && actual instanceof CompiledList actualList) {
            return isTypeCompatible(actualList.elementType(), expectedList.elementType());
        }
        if (expected instanceof CompiledSet expectedSet && actual instanceof CompiledSet actualSet) {
            return isTypeCompatible(actualSet.elementType(), expectedSet.elementType());
        }
        if (expected instanceof CompiledDict expectedDict && actual instanceof CompiledDict actualDict) {
            return isTypeCompatible(actualDict.valueType(), expectedDict.valueType());
        }
        if (expected instanceof CompiledTupleType expectedTuple && actual instanceof CompiledTupleType actualTuple) {
            return areTupleTypesCompatible(actualTuple, expectedTuple);
        }
        if (expected instanceof CompiledFunctionType expectedFunction && actual instanceof CompiledFunctionType actualFunction) {
            return areFunctionTypesCompatible(actualFunction, expectedFunction);
        }
        if (expected instanceof GenericDataType expectedData && actual instanceof GenericDataType actualData) {
            if (!sameRawTypeName(expectedData.name(), actualData.name())) {
                return false;
            }
            var expectedTypeParameters = switch (expectedData) {
                case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
                case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            };
            var actualTypeParameters = switch (actualData) {
                case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
                case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            };
            return areTypeParameterDescriptorsCompatible(actualTypeParameters, expectedTypeParameters);
        }
        return false;
    }

    private boolean areTypeParameterDescriptorsCompatible(List<String> actualTypeParameters, List<String> expectedTypeParameters) {
        if (expectedTypeParameters.isEmpty()) {
            return true;
        }
        if (actualTypeParameters.size() != expectedTypeParameters.size()) {
            return false;
        }
        for (int i = 0; i < expectedTypeParameters.size(); i++) {
            if (!isTypeDescriptorCompatible(actualTypeParameters.get(i), expectedTypeParameters.get(i))) {
                return false;
            }
        }
        return true;
    }

    private boolean isTypeDescriptorCompatible(String actualDescriptor, String expectedDescriptor) {
        var actual = normalizeTypeDescriptor(actualDescriptor);
        var expected = normalizeTypeDescriptor(expectedDescriptor);
        if (actual.equals(expected)) {
            return true;
        }
        if (expected.matches("[A-Z]") || actual.matches("[A-Z]")) {
            return true;
        }
        var expectedPrimitive = PrimitiveType.find(expected).map(this::toPrimitiveLinkedType);
        var actualPrimitive = PrimitiveType.find(actual).map(this::toPrimitiveLinkedType);
        if (expectedPrimitive.isPresent() && actualPrimitive.isPresent()) {
            if (expectedPrimitive.get() == actualPrimitive.get()) {
                return true;
            }
            if (isNumericPrimitive(expectedPrimitive.get()) && isNumericPrimitive(actualPrimitive.get())) {
                return isAssignableNumericPrimitive(expectedPrimitive.get(), actualPrimitive.get());
            }
            return false;
        }
        var parsedActual = parseLinkedTypeDescriptor(actual);
        var parsedExpected = parseLinkedTypeDescriptor(expected);
        if (parsedActual.isPresent() && parsedExpected.isPresent()) {
            return isTypeCompatible(parsedActual.get(), parsedExpected.get());
        }
        return sameRawTypeName(actual, expected);
    }

    private String normalizeTypeDescriptor(String descriptor) {
        return descriptor == null ? "" : descriptor.replaceAll("\\s+", "").replace("->", "=>");
    }

    private PrimitiveLinkedType toPrimitiveLinkedType(PrimitiveType primitiveType) {
        return switch (primitiveType) {
            case BYTE -> BYTE;
            case INT -> INT;
            case LONG -> LONG;
            case DOUBLE -> DOUBLE;
            case BOOL -> BOOL;
            case STRING -> STRING;
            case FLOAT -> FLOAT;
            case NOTHING -> NOTHING;
            case ANY -> ANY;
            case DATA -> DATA;
        };
    }

    private boolean isSubtype(CompiledDataType candidate, String expectedTypeName, java.util.Set<String> visited) {
        if (!visited.add(candidate.name())) {
            return false;
        }
        if (candidate.extendedTypes().contains(expectedTypeName)) {
            return true;
        }
        return candidate.extendedTypes().stream()
                .map(dataTypes::get)
                .filter(CompiledDataType.class::isInstance)
                .map(CompiledDataType.class::cast)
                .anyMatch(parent -> parent.name().equals(expectedTypeName) || isSubtype(parent, expectedTypeName, visited));
    }

    private boolean isSubtypeOfParent(CompiledDataType candidate, CompiledDataParentType expectedParentType) {
        if (expectedParentType.subTypes().stream()
                .anyMatch(subType -> sameRawTypeName(subType.name(), candidate.name()))) {
            return true;
        }
        if (!expectedParentType.subTypes().isEmpty()) {
            return false;
        }
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parentType -> sameRawTypeName(parentType.name(), expectedParentType.name()))
                .flatMap(parentType -> parentType.subTypes().stream())
                .anyMatch(subType -> sameRawTypeName(subType.name(), candidate.name()));
    }

    private static boolean sameRawTypeName(String left, String right) {
        var normalizedLeft = normalizeTypeAlias(left);
        var normalizedRight = normalizeTypeAlias(right);
        return normalizedLeft.equals(normalizedRight)
               || simpleRawTypeName(normalizedLeft).equals(simpleRawTypeName(normalizedRight));
    }

    private static String normalizeTypeAlias(String typeName) {
        var withoutGenerics = stripGenericSuffix(typeName);
        return withoutGenerics
                .replace("/capy/lang/Option", "/cap/lang/Option")
                .replace(".capy.lang.Option", ".cap.lang.Option");
    }

    private static String stripGenericSuffix(String typeName) {
        var idx = typeName.indexOf('[');
        return idx > 0 ? typeName.substring(0, idx) : typeName;
    }

    private static String simpleRawTypeName(String typeName) {
        var slash = typeName.lastIndexOf('/');
        var dot = typeName.lastIndexOf('.');
        var idx = Math.max(slash, dot);
        return idx >= 0 ? typeName.substring(idx + 1) : typeName;
    }

    private Result<CompiledExpression> linkIfExpression(IfExpression ifExpression, Scope scope) {
        return linkExpression(ifExpression.condition(), scope)
                .flatMap(c -> {
                    if (!isBooleanConvertibleType(c.type())) {
                        return withPosition(
                                Result.error("condition in if statement has to have type `" + BOOL + "`, was `" + c.type() + "`"),
                                ifExpression.condition().position()
                        );
                    }
                    return linkExpression(ifExpression.thenBranch(), scope)
                            .flatMap(t ->
                                    linkExpression(ifExpression.elseBranch(), scope)
                                            .map(e -> new CompiledIfExpression(c, t, e, mergeBranchTypes(t.type(), e.type()))));
                });
    }

    private Result<CompiledExpression> linkInfixExpression(InfixExpression expression, Scope scope) {
        var normalizedExpression = normalizePipeAssociativity(expression);
        if (normalizedExpression != expression) {
            return linkInfixExpression(normalizedExpression, scope);
        }
        var minLiteral = tryLinkMinLiteralExpression(expression);
        if (minLiteral.isPresent()) {
            return minLiteral.get();
        }
        if (expression.operator() == InfixOperator.PIPE) {
            return linkExpression(expression.left(), scope)
                    .flatMap(left -> {
                        if (left.type() instanceof GenericDataType) {
                            var methodCall = resolveMethodInfixCall(
                                    expression.operator().symbol(),
                                    left,
                                    expression.right(),
                                    scope,
                                    expression.position()
                            );
                            if (methodCall instanceof Result.Success<CompiledExpression> value) {
                                return value;
                            }
                            if (methodCall instanceof Result.Error<CompiledExpression> error
                                && !error.errors().isEmpty()) {
                                return methodCall;
                            }
                        }
                        if (isPipeMapExpression(expression)) {
                            return linkPipeExpression(expression, scope);
                        }
                        return linkExpression(expression.right(), scope)
                                .flatMap(right ->
                                        getLinkedInfixExpression(left, expression.operator(), right, expression.position())
                                                .map(linked -> (CompiledExpression) linked));
                    });
        }
        if (expression.operator() == InfixOperator.PIPE_MINUS) {
            return linkPipeFilterOutExpression(expression, scope);
        }
        if (expression.operator() == InfixOperator.PIPE_FLATMAP) {
            return linkExpression(expression.left(), scope)
                    .flatMap(left -> {
                        if (left.type() instanceof GenericDataType) {
                            var methodCall = resolveMethodInfixCall(
                                    expression.operator().symbol(),
                                    left,
                                    expression.right(),
                                    scope,
                                    expression.position()
                            );
                            if (methodCall instanceof Result.Success<CompiledExpression> value) {
                                return value;
                            }
                            if (methodCall instanceof Result.Error<CompiledExpression> error
                                && !error.errors().isEmpty()) {
                                return methodCall;
                            }
                        }
                        return linkPipeFlatMapExpression(expression, scope);
                    });
        }
        if (expression.operator() == InfixOperator.PIPE_REDUCE) {
            return linkExpression(expression.left(), scope)
                    .flatMap(left -> {
                        if (left.type() instanceof GenericDataType) {
                            var methodCall = resolveMethodInfixCall(
                                    expression.operator().symbol(),
                                    left,
                                    expression.right(),
                                    scope,
                                    expression.position()
                            );
                            if (methodCall instanceof Result.Success<CompiledExpression> value) {
                                return value;
                            }
                            if (methodCall instanceof Result.Error<CompiledExpression> error
                                && !error.errors().isEmpty()) {
                                return methodCall;
                            }
                        }
                        return linkPipeReduceExpression(expression, scope);
                    });
        }
        if (expression.operator() == InfixOperator.PIPE_ANY) {
            return linkPipeAnyAllExpression(expression, scope, true);
        }
        if (expression.operator() == InfixOperator.PIPE_ALL) {
            return linkPipeAnyAllExpression(expression, scope, false);
        }
        return linkExpression(expression.left(), scope)
                .flatMap(left ->
                        linkExpression(expression.right(), scope)
                                .flatMap(right -> {
                                    if (left.type() instanceof GenericDataType) {
                                        var methodCall = resolveMethodInfixCall(expression.operator().symbol(), left, right, expression.position());
                                        if (methodCall instanceof Result.Success<CompiledExpression> value) {
                                            return value;
                                        }
                                    }
                                    return getLinkedInfixExpression(left, expression.operator(), right, expression.position())
                                            .map(linked -> (CompiledExpression) linked);
                                }));
    }

    private InfixExpression normalizePipeAssociativity(InfixExpression expression) {
        if (expression.operator() == InfixOperator.PIPE
            && expression.left() instanceof InfixExpression leftInfix
            && leftInfix.operator() == InfixOperator.PIPE
            && leftInfix.right() instanceof LambdaExpression outerLambda
            && expression.right() instanceof LambdaExpression innerLambda
            && outerLambda.argumentNames().size() == 1) {
            var outerArgument = outerLambda.argumentNames().get(0);
            if (!containsValueReference(innerLambda.expression(), outerArgument)) {
                return expression;
            }
            var nestedBody = new InfixExpression(
                    outerLambda.expression(),
                    InfixOperator.PIPE,
                    innerLambda,
                    expression.position()
            );
            var nestedLambda = new LambdaExpression(
                    outerLambda.argumentNames(),
                    nestedBody,
                    outerLambda.position()
            );
            return new InfixExpression(
                    leftInfix.left(),
                    InfixOperator.PIPE,
                    nestedLambda,
                    expression.position()
            );
        }
        if (!isPipeOperator(expression.operator()) || !(expression.right() instanceof InfixExpression rightInfix)) {
            return expression;
        }
        if (rightInfix.left() instanceof LambdaExpression) {
            return expression;
        }
        if (!isPipeOperator(rightInfix.operator())
            || !(rightInfix.left() instanceof LambdaExpression
                 || rightInfix.left() instanceof FunctionReference
                 || rightInfix.left() instanceof ReduceExpression)) {
            return expression;
        }
        var leftAssociated = new InfixExpression(
                expression.left(),
                expression.operator(),
                rightInfix.left(),
                expression.position()
        );
        return new InfixExpression(
                leftAssociated,
                rightInfix.operator(),
                rightInfix.right(),
                rightInfix.position()
        );
    }

    private boolean containsValueReference(Expression expression, String variableName) {
        return switch (expression) {
            case Value value -> value.name().equals(variableName);
            case FieldAccess fieldAccess -> containsValueReference(fieldAccess.source(), variableName);
            case FunctionCall functionCall -> functionCall.arguments().stream()
                    .anyMatch(argument -> containsValueReference(argument, variableName));
            case FunctionInvoke functionInvoke ->
                    containsValueReference(functionInvoke.function(), variableName)
                    || functionInvoke.arguments().stream().anyMatch(argument -> containsValueReference(argument, variableName));
            case InfixExpression infixExpression ->
                    containsValueReference(infixExpression.left(), variableName)
                    || containsValueReference(infixExpression.right(), variableName);
            case IfExpression ifExpression ->
                    containsValueReference(ifExpression.condition(), variableName)
                    || containsValueReference(ifExpression.thenBranch(), variableName)
                    || containsValueReference(ifExpression.elseBranch(), variableName);
            case IndexExpression indexExpression ->
                    containsValueReference(indexExpression.source(), variableName)
                    || containsValueReference(indexExpression.index(), variableName);
            case SliceExpression sliceExpression ->
                    containsValueReference(sliceExpression.source(), variableName)
                    || sliceExpression.start().map(start -> containsValueReference(start, variableName)).orElse(false)
                    || sliceExpression.end().map(end -> containsValueReference(end, variableName)).orElse(false);
            case LambdaExpression lambdaExpression -> {
                if (lambdaExpression.argumentNames().contains(variableName)) {
                    yield false;
                }
                yield containsValueReference(lambdaExpression.expression(), variableName);
            }
            case ReduceExpression reduceExpression -> {
                if (reduceExpression.accumulatorName().equals(variableName)
                    || reduceExpression.valueName().equals(variableName)
                    || reduceExpression.keyName().map(variableName::equals).orElse(false)) {
                    yield false;
                }
                yield containsValueReference(reduceExpression.initialValue(), variableName)
                      || containsValueReference(reduceExpression.reducerExpression(), variableName);
            }
            case LetExpression letExpression -> {
                var valueContains = containsValueReference(letExpression.value(), variableName);
                if (letExpression.name().equals(variableName)) {
                    yield valueContains;
                }
                yield valueContains || containsValueReference(letExpression.rest(), variableName);
            }
            case MatchExpression matchExpression ->
                    containsValueReference(matchExpression.matchWith(), variableName)
                    || matchExpression.cases().stream()
                    .anyMatch(matchCase -> containsValueReference(matchCase.expression(), variableName));
            case NewData newData ->
                    newData.assignments().stream().anyMatch(assignment -> containsValueReference(assignment.value(), variableName))
                    || newData.positionalArguments().stream().anyMatch(argument -> containsValueReference(argument, variableName))
                    || newData.spreads().stream().anyMatch(spread -> containsValueReference(spread, variableName));
            case NewDictExpression newDictExpression -> newDictExpression.entries().stream()
                    .anyMatch(entry -> containsValueReference(entry.key(), variableName)
                                       || containsValueReference(entry.value(), variableName));
            case NewListExpression newListExpression -> newListExpression.values().stream()
                    .anyMatch(value -> containsValueReference(value, variableName));
            case NewSetExpression newSetExpression -> newSetExpression.values().stream()
                    .anyMatch(value -> containsValueReference(value, variableName));
            case TupleExpression tupleExpression -> tupleExpression.values().stream()
                    .anyMatch(value -> containsValueReference(value, variableName));
            default -> false;
        };
    }

    private static boolean isPipeOperator(InfixOperator operator) {
        return operator == InfixOperator.PIPE
               || operator == InfixOperator.PIPE_MINUS
               || operator == InfixOperator.PIPE_FLATMAP
               || operator == InfixOperator.PIPE_REDUCE
               || operator == InfixOperator.PIPE_ANY
               || operator == InfixOperator.PIPE_ALL;
    }

    private Result<CompiledExpression> resolveMethodInfixCall(
            String operatorSymbol,
            CompiledExpression left,
            CompiledExpression right,
            Optional<SourcePosition> position
    ) {
        var ownerNames = methodOwnerCandidates(left.type());
        var candidates = functionSignatures.stream()
                .filter(signature -> matchesMethodOwner(signature.name(), ownerNames, operatorSymbol))
                .filter(signature -> signature.parameterTypes().size() == 2)
                .toList();
        if (candidates.isEmpty()) {
            return Result.error(List.of());
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var receiverParameterType = candidate.parameterTypes().get(0);
            var maybeReceiver = coerceArgument(left, receiverParameterType);
            if (maybeReceiver == null) {
                continue;
            }
            var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
            collectTypeSubstitutions(receiverParameterType, maybeReceiver.expression().type(), substitutions);
            var expectedRightType = substituteTypeParameters(candidate.parameterTypes().get(1), substitutions);
            var maybeArgument = coerceArgument(right, expectedRightType);
            if (maybeArgument == null) {
                continue;
            }
            var arguments = List.of(maybeReceiver.expression(), maybeArgument.expression());
            var coercions = maybeReceiver.coercions() + maybeArgument.coercions();
            if (best == null || coercions < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, arguments, coercions);
            }
        }
        if (best == null) {
            return withPosition(
                    Result.error(
                            "No matching method `" + operatorSymbol + "` for argument types ["
                            + left.type().name() + ", " + right.type().name() + "]"
                    ),
                    position
            );
        }
        var returnType = resolveMethodReturnType(operatorSymbol, best.signature(), best.arguments());
        return Result.success(new CompiledFunctionCall(
                best.signature().name(),
                best.arguments(),
                returnType
        ));
    }

    private Result<CompiledExpression> resolveMethodInfixCall(
            String operatorSymbol,
            CompiledExpression left,
            Expression right,
            Scope scope,
            Optional<SourcePosition> position
    ) {
        var ownerNames = methodOwnerCandidates(left.type());
        var candidates = functionSignatures.stream()
                .filter(signature -> matchesMethodOwner(signature.name(), ownerNames, operatorSymbol))
                .filter(signature -> signature.parameterTypes().size() == 2)
                .toList();
        if (candidates.isEmpty()) {
            return Result.error(List.of());
        }

        ResolvedFunctionCall best = null;
        Result.Error.SingleError deepestError = null;
        for (var candidate : candidates) {
            var receiverParameterType = candidate.parameterTypes().get(0);
            var maybeReceiver = coerceArgument(left, receiverParameterType);
            if (maybeReceiver == null) {
                continue;
            }
            var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
            collectTypeSubstitutions(receiverParameterType, maybeReceiver.expression().type(), substitutions);
            var expectedRightType = substituteTypeParameters(candidate.parameterTypes().get(1), substitutions);
            var maybeArgument = linkArgumentForExpectedType(right, scope, expectedRightType);
            if (maybeArgument instanceof Result.Error<CoercedArgument> error) {
                deepestError = preferDeeperError(deepestError, firstError(error));
                continue;
            }
            var coercedArgument = ((Result.Success<CoercedArgument>) maybeArgument).value();
            var arguments = List.of(maybeReceiver.expression(), coercedArgument.expression());
            var coercions = maybeReceiver.coercions() + coercedArgument.coercions();
            if (best == null || coercions < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, arguments, coercions);
            }
        }
        if (best == null) {
            if (deepestError != null) {
                return new Result.Error<>(deepestError);
            }
            return withPosition(
                    Result.error(
                            "No matching method `" + operatorSymbol + "` for argument types ["
                            + left.type().name() + ", " + right + "]"
                    ),
                    position
            );
        }
        var returnType = resolveMethodReturnType(operatorSymbol, best.signature(), best.arguments());
        return Result.success(new CompiledFunctionCall(
                best.signature().name(),
                best.arguments(),
                returnType
        ));
    }

    private CompiledType resolveMethodReturnType(
            String operatorSymbol,
            FunctionSignature signature,
            List<CompiledExpression> arguments
    ) {
        var resolved = resolveReturnType(signature, arguments);
        if (!"|".equals(operatorSymbol) || arguments.size() < 2) {
            return resolved;
        }
        var mapper = arguments.get(1);
        if (!(mapper instanceof CompiledLambdaExpression lambdaExpression)) {
            return resolved;
        }
        if (!(signature.returnType() instanceof GenericDataType signatureReturnType)) {
            return resolved;
        }
        if (!(lambdaExpression.expression().type() instanceof GenericDataType lambdaReturnType)) {
            return resolved;
        }
        if (!sameRawTypeName(signatureReturnType.name(), lambdaReturnType.name())
            || !isResolvedTypeForInference(lambdaReturnType)) {
            return resolved;
        }
        return lambdaReturnType;
    }

    private static Result.Error.SingleError firstError(Result.Error<?> error) {
        if (error.errors().isEmpty()) {
            return null;
        }
        return error.errors().first();
    }

    private static Result.Error.SingleError preferDeeperError(
            Result.Error.SingleError current,
            Result.Error.SingleError candidate
    ) {
        if (candidate == null) {
            return current;
        }
        if (current == null) {
            return candidate;
        }
        if (candidate.line() != current.line()) {
            return candidate.line() > current.line() ? candidate : current;
        }
        if (candidate.column() != current.column()) {
            return candidate.column() > current.column() ? candidate : current;
        }
        return candidate.message().length() >= current.message().length() ? candidate : current;
    }

    private Set<String> methodOwnerCandidates(CompiledType receiverType) {
        var ownerNames = new LinkedHashSet<String>();
        var receiverBase = baseTypeName(receiverType.name());
        var receiverSimple = simpleTypeName(receiverBase);
        ownerNames.add(receiverBase);
        dataTypes.keySet().stream()
                .map(this::baseTypeName)
                .filter(name -> simpleTypeName(name).equals(receiverSimple))
                .forEach(ownerNames::add);
        if (receiverType instanceof CompiledDataType receiverDataType) {
            var receiverDataSimple = simpleTypeName(baseTypeName(receiverDataType.name()));
            dataTypes.values().stream()
                    .filter(CompiledDataParentType.class::isInstance)
                    .map(CompiledDataParentType.class::cast)
                    .filter(parentType -> parentType.subTypes().stream()
                            .anyMatch(subType -> simpleTypeName(baseTypeName(subType.name())).equals(receiverDataSimple)))
                    .map(CompiledDataParentType::name)
                    .map(this::baseTypeName)
                    .forEach(ownerNames::add);
            dataTypes.entrySet().stream()
                    .filter(entry -> entry.getValue() instanceof CompiledDataParentType parentType
                            && parentType.subTypes().stream()
                                    .anyMatch(subType -> simpleTypeName(baseTypeName(subType.name())).equals(receiverDataSimple)))
                    .map(Map.Entry::getKey)
                    .map(this::baseTypeName)
                    .forEach(ownerNames::add);
        }
        return Set.copyOf(ownerNames);
    }

    private boolean matchesMethodOwner(String signatureName, Set<String> ownerNames, String operatorSymbol) {
        if (!signatureName.startsWith(METHOD_DECL_PREFIX)) {
            return false;
        }
        var separator = signatureName.indexOf("__", METHOD_DECL_PREFIX.length());
        if (separator < 0 || separator + 2 > signatureName.length()) {
            return false;
        }
        var ownerName = baseTypeName(signatureName.substring(METHOD_DECL_PREFIX.length(), separator));
        var methodName = signatureName.substring(separator + 2);
        return methodName.equals(operatorSymbol) && ownerNames.contains(ownerName);
    }

    private String baseTypeName(String typeName) {
        var genericStart = typeName.indexOf('[');
        return genericStart > 0 ? typeName.substring(0, genericStart) : typeName;
    }

    private String simpleTypeName(String typeName) {
        var normalized = baseTypeName(typeName);
        var slash = normalized.lastIndexOf('/');
        var dot = normalized.lastIndexOf('.');
        var index = Math.max(slash, dot);
        return index >= 0 ? normalized.substring(index + 1) : normalized;
    }

    private Result<CompiledExpression> linkPipeExpression(InfixExpression expression, Scope scope) {
        var reAssociated = reAssociatePipeChain(expression);
        if (reAssociated != expression) {
            return linkInfixExpression(reAssociated, scope);
        }
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    if (isOptionType(left.type())) {
                        return linkOptionPipeExpression(expression, scope, left, optionElementType(left));
                    }
                    var elementType = switch (left.type()) {
                        case CompiledList linkedList -> linkedList.elementType();
                        case CompiledSet linkedSet -> linkedSet.elementType();
                        case CompiledDict linkedDict -> linkedDict.valueType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (elementType == null) {
                        return linkScalarPipeExpression(expression, scope, left);
                    }
                    return linkCollectionPipeExpression(expression, scope, left, elementType);
                });
    }

    private Result<CompiledExpression> linkOptionPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledType elementType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, elementType)
                        .map(linked -> (CompiledExpression) new CompiledPipeExpression(
                                left,
                                linked.argumentName(),
                                linked.expression(),
                                left.type()
                        ));
            }
            return withPosition(
                    Result.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        return linkPipeLambdaArguments(scope, lambdaExpression, elementType, "|")
                .flatMap(lambdaBinding -> linkExpression(lambdaExpression.expression(), lambdaBinding.scope())
                .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                        left,
                        lambdaBinding.argumentName(),
                        mapper,
                        left.type()
                )));
    }

    private Result<CompiledExpression> linkCollectionPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledType elementType
    ) {
        if (left.type() instanceof CompiledDict dictType) {
            return linkDictPipeExpression(expression, scope, left, dictType);
        }
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, elementType)
                        .map(linked -> (CompiledExpression) new CompiledPipeExpression(
                                left,
                                linked.argumentName(),
                                linked.expression(),
                                left.type() instanceof CompiledSet
                                        ? new CompiledSet(linked.expression().type())
                                        : new CompiledList(linked.expression().type())
                        ));
            }
            return withPosition(
                    Result.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        return linkPipeLambdaArguments(scope, lambdaExpression, elementType, "|")
                .flatMap(lambdaBinding -> linkExpression(lambdaExpression.expression(), lambdaBinding.scope())
                .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                        left,
                        lambdaBinding.argumentName(),
                        mapper,
                        left.type() instanceof CompiledSet
                                ? new CompiledSet(mapper.type())
                                : new CompiledList(mapper.type())
                )));
    }

    private Result<CompiledExpression> linkDictPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledDict dictType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, dictType.valueType())
                        .map(linked -> (CompiledExpression) new CompiledPipeExpression(
                                left,
                                linked.argumentName(),
                                linked.expression(),
                                new CompiledDict(linked.expression().type())
                        ));
            }
            return withPosition(
                    Result.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var valueName = argumentNames.get(0);
            var lambdaScope = addLambdaBinding(scope, valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                            left,
                            valueName,
                            mapper,
                            new CompiledDict(mapper.type())
                    ));
        }
        if (argumentNames.size() == 2) {
            var keyName = argumentNames.get(0);
            var valueName = argumentNames.get(1);
            var lambdaScope = addLambdaBinding(
                    addLambdaBinding(scope, keyName, STRING),
                    valueName,
                    dictType.valueType()
            );
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                            left,
                            encodeDictPipeArguments(keyName, valueName),
                            mapper,
                            new CompiledDict(mapper.type())
                    ));
        }
        return withPosition(
                Result.error("Right side lambda of `|` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private Result<CompiledExpression> linkScalarPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, left.type())
                        .map(linked -> (CompiledExpression) new CompiledLetExpression(
                                linked.argumentName(),
                                left,
                                linked.expression()
                        ));
            }
            return withPosition(
                    Result.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                .orElse(null);
        if (lambdaArgumentName == null) {
            return withPosition(
                    Result.error("Right side lambda of `|` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = addLambdaBinding(scope, lambdaArgumentName, left.type());
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .map(mapper -> (CompiledExpression) new CompiledLetExpression(
                        lambdaArgumentName,
                        left,
                        mapper
                ));
    }

    private Result<PipeMapper> resolvePipeFunctionReference(FunctionReference functionReference, CompiledType inputType) {
        var argumentName = "it";
        var argument = new CompiledVariable(argumentName, inputType);
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .filter(signature -> signature.parameterTypes().size() == 1)
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    Result.error("Function `" + functionReference.name() + "` with one argument not found"),
                    functionReference.position()
            );
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var coerced = coerceArgument(argument, candidate.parameterTypes().get(0));
            if (coerced == null) {
                continue;
            }
            var resolved = new ResolvedFunctionCall(
                    candidate,
                    List.of(coerced.expression()),
                    coerced.coercions()
            );
            if (best == null || resolved.coercions() < best.coercions()) {
                best = resolved;
            }
        }
        if (best == null) {
            return withPosition(
                    Result.error("Function reference `" + functionReference.name() + "` is not compatible with `" + inputType + "`"),
                    functionReference.position()
            );
        }
        return Result.success(new PipeMapper(
                argumentName,
                new CompiledFunctionCall(
                        functionReference.name(),
                        best.arguments(),
                        resolveReturnType(best.signature(), best.arguments())
                )
        ));
    }

    private Result<CompiledExpression> linkPipeReduceExpression(InfixExpression expression, Scope scope) {
        var reAssociated = reAssociatePipeChain(expression);
        if (reAssociated != expression) {
            return linkInfixExpression(reAssociated, scope);
        }
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = left.type() == STRING ? STRING : collectionElementType(left.type());
                    if (elementType == null) {
                        return withPosition(
                                Result.error("Left side of `|>` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (left.type() instanceof CompiledDict dictType
                        && expression.right() instanceof LambdaExpression lambdaExpression) {
                        var argumentNames = lambdaExpression.argumentNames();
                        if (argumentNames.size() == 1) {
                            var valueName = argumentNames.get(0);
                            var lambdaScope = addLambdaBinding(scope, valueName, dictType.valueType());
                            return linkExpression(lambdaExpression.expression(), lambdaScope)
                                    .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                                            left,
                                            valueName,
                                            mapper,
                                            new CompiledSet(mapper.type())
                                    ));
                        }
                        if (argumentNames.size() == 2) {
                            var keyName = argumentNames.get(0);
                            var valueName = argumentNames.get(1);
                            var lambdaScope = addLambdaBinding(
                                    addLambdaBinding(scope, keyName, STRING),
                                    valueName,
                                    dictType.valueType()
                            );
                            return linkExpression(lambdaExpression.expression(), lambdaScope)
                                    .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                                            left,
                                            encodeDictPipeArguments(keyName, valueName),
                                            mapper,
                                            new CompiledSet(mapper.type())
                                    ));
                        }
                        return withPosition(
                                Result.error("Right side lambda of `|>` for dict has to have one or two arguments"),
                                lambdaExpression.position()
                        );
                    }
                    if (!(expression.right() instanceof ReduceExpression reduceExpression)) {
                        return withPosition(
                                Result.error("Right side of `|>` has to be a reduce expression"),
                                expression.right().position()
                        );
                    }
                    return linkExpression(reduceExpression.initialValue(), scope)
                            .flatMap(initial -> {
                                var reduceScope = scope;
                                if (reduceExpression.accumulatorName().contains("::") && reduceExpression.keyName().isPresent()) {
                                    return withPosition(
                                            Result.error("Reducer with four arguments is not supported for `dict`. Use `|>` mapper and then a standard reduce."),
                                            reduceExpression.position()
                                    );
                                } else {
                                    reduceScope = reduceScope.add(
                                            reduceExpression.accumulatorName(),
                                            widenReduceAccumulatorBindingType(initial.type())
                                    );
                                }
                                if (reduceExpression.keyName().isPresent()) {
                                    if (!(left.type() instanceof CompiledDict)) {
                                        return withPosition(
                                                Result.error("Reducer in `|>` with key argument can only be used for `dict`"),
                                                reduceExpression.position()
                                        );
                                    }
                                    reduceScope = reduceScope.add(reduceExpression.keyName().orElseThrow(), STRING);
                                }
                                reduceScope = reduceScope.add(reduceExpression.valueName(), elementType);
                                return linkExpression(reduceExpression.reducerExpression(), reduceScope)
                                        .flatMap(reducer -> {
                                            var resolvedInitial = initial;
                                            var accumulatorType = initial.type();
                                            var coercedReducer = coerceArgument(reducer, accumulatorType);
                                            if (coercedReducer == null) {
                                                var coercedInitial = coerceArgument(initial, reducer.type());
                                                if (coercedInitial != null) {
                                                    resolvedInitial = coercedInitial.expression();
                                                    accumulatorType = reducer.type();
                                                    coercedReducer = coerceArgument(reducer, accumulatorType);
                                                }
                                            }
                                            if (coercedReducer == null) {
                                                var sharedAccumulatorType = findSharedReduceAccumulatorType(initial, reducer);
                                                if (sharedAccumulatorType != null) {
                                                    var coercedInitial = coerceArgument(initial, sharedAccumulatorType);
                                                    var sharedCoercedReducer = coerceArgument(reducer, sharedAccumulatorType);
                                                    if (coercedInitial != null && sharedCoercedReducer != null) {
                                                        resolvedInitial = coercedInitial.expression();
                                                        accumulatorType = sharedAccumulatorType;
                                                        coercedReducer = sharedCoercedReducer;
                                                    }
                                                }
                                            }
                                            if (coercedReducer == null) {
                                                return withPosition(
                                                        Result.error(
                                                                "Reducer in `|>` has to return `" + initial.type() + "`, was `" + reducer.type() + "`"
                                                        ),
                                                        reduceExpression.position()
                                                );
                                            }
                                            var inferredReduceType = inferReduceResultType(accumulatorType, reducer.type());
                                            return Result.success((CompiledExpression) new CompiledPipeReduceExpression(
                                                    left,
                                                    resolvedInitial,
                                                    reduceExpression.accumulatorName(),
                                                    reduceExpression.keyName(),
                                                    reduceExpression.valueName(),
                                                    coercedReducer.expression(),
                                                    inferredReduceType
                                            ));
                                        });
                            });
                });
    }

    private CompiledType findSharedReduceAccumulatorType(CompiledExpression initial, CompiledExpression reducer) {
        if (!(initial.type() instanceof CompiledDataType initialData) || !(reducer.type() instanceof CompiledDataType reducerData)) {
            return null;
        }
        for (var initialParentDescriptor : initialData.extendedTypes()) {
            var initialParentName = rawTypeName(initialParentDescriptor);
            for (var reducerParentDescriptor : reducerData.extendedTypes()) {
                var reducerParentName = rawTypeName(reducerParentDescriptor);
                if (!sameRawTypeName(initialParentName, reducerParentName)) {
                    continue;
                }
                var shared = resolveDataTypeByName(initialParentName);
                if (shared != null
                    && coerceArgument(initial, shared) != null
                    && coerceArgument(reducer, shared) != null) {
                    return shared;
                }
            }
        }
        for (var type : dataTypes.values()) {
            if (!(type instanceof CompiledDataParentType parentType)) {
                continue;
            }
            var hasInitial = parentType.subTypes().stream()
                    .anyMatch(subType -> sameRawTypeName(subType.name(), initialData.name()));
            if (!hasInitial) {
                continue;
            }
            var hasReducer = parentType.subTypes().stream()
                    .anyMatch(subType -> sameRawTypeName(subType.name(), reducerData.name()));
            if (!hasReducer) {
                continue;
            }
            if (coerceArgument(initial, parentType) != null && coerceArgument(reducer, parentType) != null) {
                return parentType;
            }
        }
        return null;
    }

    private CompiledType widenReduceAccumulatorBindingType(CompiledType initialType) {
        if (!(initialType instanceof CompiledDataType initialData)) {
            return initialType;
        }
        var sharedParents = dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parent -> isSubtypeOfParent(initialData, parent))
                .toList();
        if (sharedParents.size() == 1) {
            return canonicalizeParentAlias(sharedParents.getFirst());
        }
        if (!sharedParents.isEmpty()) {
            var uniqueByRawName = sharedParents.stream()
                    .collect(java.util.stream.Collectors.toMap(
                            parent -> simpleRawTypeName(normalizeTypeAlias(parent.name())),
                            parent -> parent,
                            this::preferQualifiedParent,
                            java.util.LinkedHashMap::new
                    ));
            if (uniqueByRawName.size() == 1) {
                return canonicalizeParentAlias(uniqueByRawName.values().iterator().next());
            }
        }
        return initialType;
    }

    private String rawTypeName(String descriptor) {
        var genericStart = descriptor.indexOf('[');
        return genericStart >= 0 ? descriptor.substring(0, genericStart) : descriptor;
    }

    private CompiledType inferReduceResultType(CompiledType initialType, CompiledType reducerType) {
        if (initialType instanceof CompiledList initialList
            && initialList.elementType() == ANY
            && reducerType instanceof CompiledList reducerList
            && reducerList.elementType() != ANY) {
            return reducerType;
        }
        if (initialType instanceof CompiledSet initialSet
            && initialSet.elementType() == ANY
            && reducerType instanceof CompiledSet reducerSet
            && reducerSet.elementType() != ANY) {
            return reducerType;
        }
        if (initialType instanceof CompiledDict initialDict
            && initialDict.valueType() == ANY
            && reducerType instanceof CompiledDict reducerDict
            && reducerDict.valueType() != ANY) {
            return reducerType;
        }
        return initialType;
    }

    private Result<CompiledExpression> linkPipeFlatMapExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = collectionElementType(left.type());
                    if (elementType == null) {
                        return withPosition(
                                Result.error("Left side of `|*` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        return withPosition(
                                Result.error("Right side of `|*` has to be a lambda expression"),
                                expression.right().position()
                        );
                    }
                    return linkPipeLambdaArguments(scope, lambdaExpression, elementType, "|*")
                            .flatMap(lambdaBinding -> linkExpression(lambdaExpression.expression(), lambdaBinding.scope())
                            .flatMap(mapper -> {
                                var mappedElementType = collectionElementType(mapper.type());
                                if (mappedElementType == null) {
                                    return withPosition(
                                            Result.error("Lambda in `|*` has to return collection type, was `" + mapper.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return Result.success((CompiledExpression) new CompiledPipeFlatMapExpression(
                                        left,
                                        lambdaBinding.argumentName(),
                                        mapper,
                                        left.type() instanceof CompiledSet
                                                ? new CompiledSet(mappedElementType)
                                                : new CompiledList(mappedElementType)
                                ));
                            }));
                });
    }

    private Result<CompiledExpression> linkPipeFilterOutExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    if (left.type() instanceof CompiledDict dictType) {
                        return linkDictPipeFilterOutExpression(expression, scope, left, dictType);
                    }
                    if (isOptionType(left.type())) {
                        return linkOptionPipeFilterOutExpression(expression, scope, left, optionElementType(left), left.type());
                    }
                    var elementType = switch (left.type()) {
                        case CompiledList linkedList -> linkedList.elementType();
                        case CompiledSet linkedSet -> linkedSet.elementType();
                        case CompiledDict linkedDict -> linkedDict.valueType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (elementType == null) {
                        if (left.type() instanceof GenericDataType) {
                            var optionType = findOptionType();
                            if (optionType == null) {
                                return withPosition(
                                        Result.error("`|-` on data/type requires `Option` type to be available"),
                                        expression.left().position()
                                );
                            }
                            return linkOptionPipeFilterOutExpression(expression, scope, left, left.type(), optionType);
                        }
                        return withPosition(
                                Result.error("Left side of `|-` has to be a collection or data/type, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        if (expression.right() instanceof FunctionReference functionReference) {
                            return resolvePipeFunctionReference(functionReference, elementType)
                                    .flatMap(linked -> {
                                        if (linked.expression().type() != BOOL) {
                                            return withPosition(
                                                    Result.error("Function reference in `|-` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                                    functionReference.position()
                                            );
                                        }
                                        return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                                                left,
                                                linked.argumentName(),
                                                linked.expression(),
                                                left.type() instanceof CompiledSet
                                                        ? new CompiledSet(elementType)
                                                        : new CompiledList(elementType)
                                        ));
                                    });
                        }
                        return withPosition(
                            Result.error("Right side of `|-` has to be a lambda expression or function reference"),
                            expression.right().position()
                        );
                    }
                    return linkPipeLambdaArguments(scope, lambdaExpression, elementType, "|-")
                            .flatMap(lambdaBinding -> linkExpression(lambdaExpression.expression(), lambdaBinding.scope())
                            .flatMap(predicate -> {
                                if (predicate.type() != BOOL) {
                                    return withPosition(
                                            Result.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                                        left,
                                        lambdaBinding.argumentName(),
                                        predicate,
                                        left.type() instanceof CompiledSet
                                                ? new CompiledSet(elementType)
                                                : new CompiledList(elementType)
                                ));
                            }));
                });
    }

    private Result<CompiledExpression> linkPipeAnyAllExpression(
            InfixExpression expression,
            Scope scope,
            boolean any
    ) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    if (left.type() instanceof CompiledDict dictType) {
                        return linkDictPipeAnyAllExpression(expression, scope, left, dictType, any);
                    }
                    var elementType = switch (left.type()) {
                        case CompiledList linkedList -> linkedList.elementType();
                        case CompiledSet linkedSet -> linkedSet.elementType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (elementType == null) {
                        return withPosition(
                                Result.error("Left side of `" + expression.operator().symbol() + "` has to be a collection or string, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        if (expression.right() instanceof FunctionReference functionReference) {
                            return resolvePipeFunctionReference(functionReference, elementType)
                                    .flatMap(linked -> {
                                        if (linked.expression().type() != BOOL) {
                                            return withPosition(
                                                    Result.error("Function reference in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                                    functionReference.position()
                                            );
                                        }
                                        return Result.success(
                                                any
                                                        ? (CompiledExpression) new CompiledPipeAnyExpression(left, linked.argumentName(), linked.expression(), BOOL)
                                                        : (CompiledExpression) new CompiledPipeAllExpression(left, linked.argumentName(), linked.expression(), BOOL)
                                        );
                                    });
                        }
                        return withPosition(
                                Result.error("Right side of `" + expression.operator().symbol() + "` has to be a lambda expression or function reference"),
                                expression.right().position()
                        );
                    }
                    return linkPipeLambdaArguments(scope, lambdaExpression, elementType, expression.operator().symbol())
                            .flatMap(lambdaBinding -> linkExpression(lambdaExpression.expression(), lambdaBinding.scope())
                            .flatMap(predicate -> {
                                if (predicate.type() != BOOL) {
                                    return withPosition(
                                            Result.error("Lambda in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + predicate.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return Result.success(
                                        any
                                                ? (CompiledExpression) new CompiledPipeAnyExpression(left, lambdaBinding.argumentName(), predicate, BOOL)
                                                : (CompiledExpression) new CompiledPipeAllExpression(left, lambdaBinding.argumentName(), predicate, BOOL)
                                );
                            }));
                });
    }

    private Result<CompiledExpression> linkDictPipeAnyAllExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledDict dictType,
            boolean any
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, dictType.valueType())
                        .flatMap(linked -> {
                            if (linked.expression().type() != BOOL) {
                                return withPosition(
                                        Result.error("Function reference in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                        functionReference.position()
                                );
                            }
                            return Result.success(
                                    any
                                            ? (CompiledExpression) new CompiledPipeAnyExpression(left, linked.argumentName(), linked.expression(), BOOL)
                                            : (CompiledExpression) new CompiledPipeAllExpression(left, linked.argumentName(), linked.expression(), BOOL)
                            );
                        });
            }
            return withPosition(
                    Result.error("Right side of `" + expression.operator().symbol() + "` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var valueName = argumentNames.get(0);
            var lambdaScope = addLambdaBinding(scope, valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    Result.error("Lambda in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        return Result.success(
                                any
                                        ? (CompiledExpression) new CompiledPipeAnyExpression(left, valueName, predicate, BOOL)
                                        : (CompiledExpression) new CompiledPipeAllExpression(left, valueName, predicate, BOOL)
                        );
                    });
        }
        if (argumentNames.size() == 2) {
            var keyName = argumentNames.get(0);
            var valueName = argumentNames.get(1);
            var lambdaScope = addLambdaBinding(
                    addLambdaBinding(scope, keyName, STRING),
                    valueName,
                    dictType.valueType()
            );
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    Result.error("Lambda in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        var encodedArgName = encodeDictPipeArguments(keyName, valueName);
                        return Result.success(
                                any
                                        ? (CompiledExpression) new CompiledPipeAnyExpression(left, encodedArgName, predicate, BOOL)
                                        : (CompiledExpression) new CompiledPipeAllExpression(left, encodedArgName, predicate, BOOL)
                        );
                    });
        }
        return withPosition(
                Result.error("Right side lambda of `" + expression.operator().symbol() + "` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private Result<CompiledExpression> linkDictPipeFilterOutExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledDict dictType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, dictType.valueType())
                        .flatMap(linked -> {
                            if (linked.expression().type() != BOOL) {
                                return withPosition(
                                        Result.error("Function reference in `|-` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                        functionReference.position()
                                );
                            }
                            return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                                    left,
                                    linked.argumentName(),
                                    linked.expression(),
                                    new CompiledDict(dictType.valueType())
                            ));
                        });
            }
            return withPosition(
                    Result.error("Right side of `|-` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var valueName = argumentNames.get(0);
            var lambdaScope = addLambdaBinding(scope, valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    Result.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                                left,
                                valueName,
                                predicate,
                                new CompiledDict(dictType.valueType())
                        ));
                    });
        }
        if (argumentNames.size() == 2) {
            var keyName = argumentNames.get(0);
            var valueName = argumentNames.get(1);
            var lambdaScope = addLambdaBinding(
                    addLambdaBinding(scope, keyName, STRING),
                    valueName,
                    dictType.valueType()
            );
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    Result.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                                left,
                                encodeDictPipeArguments(keyName, valueName),
                                predicate,
                                new CompiledDict(dictType.valueType())
                        ));
                    });
        }
        return withPosition(
                Result.error("Right side lambda of `|-` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private Result<CompiledExpression> linkOptionPipeFilterOutExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledType elementType,
            CompiledType optionType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, elementType)
                        .flatMap(linked -> {
                            if (linked.expression().type() != BOOL) {
                                return withPosition(
                                        Result.error("Lambda in `|-` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                        expression.right().position()
                                );
                            }
                            return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                                    left,
                                    linked.argumentName(),
                                    linked.expression(),
                                    optionType
                            ));
                        });
            }
            return withPosition(
                    Result.error("Right side of `|-` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                .orElse(null);
        if (lambdaArgumentName == null) {
            return withPosition(
                    Result.error("Right side lambda of `|-` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = addLambdaBinding(scope, lambdaArgumentName, elementType);
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .flatMap(predicate -> {
                    if (predicate.type() != BOOL) {
                        return withPosition(
                                Result.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                lambdaExpression.position()
                        );
                    }
                    return Result.success((CompiledExpression) new CompiledPipeFilterOutExpression(
                            left,
                            lambdaArgumentName,
                            predicate,
                            optionType
                    ));
                });
    }

    private static Result<CompiledInfixExpression> getLinkedInfixExpression(
            CompiledExpression left,
            InfixOperator operator,
            CompiledExpression right,
            Optional<SourcePosition> position
    ) {
        CompiledType type = switch (operator) {
            case PLUS -> findPlusType(left.type(), right.type());
            case MINUS -> findMinusType(left.type(), right.type());
            case MUL, DIV, MOD, POWER -> findMathType(left.type(), right.type());
            case BITWISE_AND, BITWISE_NAND, BITWISE_OR, BITWISE_XOR -> findBitwiseType(left.type(), right.type());
            case BITWISE_NOT -> findBitwiseNotType(left.type());
            // bool operators
            case GT, LT, EQUAL, NOTEQUAL, LE, GE -> BOOL;
            case AND, PIPE -> findLogicalType(left.type(), right.type());
            case QUESTION -> findQuestionType(left.type(), right.type());
            case PIPE_MINUS, PIPE_FLATMAP, PIPE_REDUCE, PIPE_ANY, PIPE_ALL -> null;
        };
        if (type == null) {
            var op = operator.symbol();
            return withPosition(Result.error("Cannot apply `" + op + "` to `" + left.type() + "` and `" + right.type() + "`"), position);
        }
        return Result.success(new CompiledInfixExpression(left, operator, right, type));
    }

    private Optional<Result<CompiledExpression>> tryLinkMinLiteralExpression(InfixExpression expression) {
        if (expression.operator() != InfixOperator.MINUS) {
            return Optional.empty();
        }
        if (!(expression.left() instanceof IntValue leftInt) || !"0".equals(leftInt.intValue())) {
            if (!(expression.left() instanceof LongValue leftLong) || !"0".equals(stripLongSuffix(leftLong.longValue()))) {
                return Optional.empty();
            }
        }

        if (expression.right() instanceof IntValue intValue) {
            var parsed = parseBigInteger(intValue.intValue());
            if (parsed != null
                && BigInteger.valueOf(Integer.MAX_VALUE).add(BigInteger.ONE).equals(parsed)) {
                return Optional.of(Result.success(new CompiledIntValue(Integer.toString(Integer.MIN_VALUE))));
            }
            return Optional.empty();
        }

        if (expression.right() instanceof LongValue longValue) {
            var normalized = stripLongSuffix(longValue.longValue());
            var parsed = parseBigInteger(normalized);
            if (parsed != null
                && BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE).equals(parsed)) {
                return Optional.of(Result.success(new CompiledLongValue(Long.toString(Long.MIN_VALUE) + "L")));
            }
        }
        return Optional.empty();
    }

    private BigInteger parseBigInteger(String value) {
        try {
            return new BigInteger(value, 10);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static String stripLongSuffix(String value) {
        return value.endsWith("l") || value.endsWith("L")
                ? value.substring(0, value.length() - 1)
                : value;
    }

    private static CompiledType findPlusType(CompiledType left, CompiledType right) {
        if (left == STRING && isDataLikeType(right)) {
            return STRING;
        }
        if (right == STRING && isDataLikeType(left)) {
            return STRING;
        }
        if (left instanceof CompiledList leftList) {
            if (right instanceof CompiledList rightList) {
                return new CompiledList(findHigherType(leftList.elementType(), rightList.elementType()));
            }
            return new CompiledList(findHigherType(leftList.elementType(), right));
        }
        if (left instanceof CompiledSet leftSet) {
            if (right instanceof CompiledSet rightSet) {
                return new CompiledSet(findHigherType(leftSet.elementType(), rightSet.elementType()));
            }
            return new CompiledSet(findHigherType(leftSet.elementType(), right));
        }
        if (left instanceof CompiledDict leftDict) {
            if (right instanceof CompiledDict rightDict) {
                return new CompiledDict(findHigherType(leftDict.valueType(), rightDict.valueType()));
            }
            if (right instanceof CompiledTupleType tupleType
                && tupleType.elementTypes().size() == 2
                && tupleType.elementTypes().getFirst() == STRING) {
                return new CompiledDict(findHigherType(leftDict.valueType(), tupleType.elementTypes().get(1)));
            }
            return null;
        }
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findPlusPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return findHigherType(left, right);
    }

    private static CompiledType findMinusType(CompiledType left, CompiledType right) {
        if (left instanceof CompiledList leftList) {
            if (right instanceof CompiledList rightList) {
                return new CompiledList(findHigherType(leftList.elementType(), rightList.elementType()));
            }
            return new CompiledList(findHigherType(leftList.elementType(), right));
        }
        if (left instanceof CompiledSet leftSet) {
            if (right instanceof CompiledSet rightSet) {
                return new CompiledSet(findHigherType(leftSet.elementType(), rightSet.elementType()));
            }
            return new CompiledSet(findHigherType(leftSet.elementType(), right));
        }
        if (left instanceof CompiledDict leftDict) {
            if (right instanceof CompiledDict rightDict) {
                return new CompiledDict(findHigherType(leftDict.valueType(), rightDict.valueType()));
            }
            if (right == STRING) {
                return new CompiledDict(leftDict.valueType());
            }
            return null;
        }
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findMathPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return findMathType(left, right);
    }

    private static CompiledType findMathType(CompiledType left, CompiledType right) {
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findMathPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return findHigherType(left, right);
    }

    private static CompiledType findBitwiseType(CompiledType left, CompiledType right) {
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findBitwisePrimitiveType(leftPrimitive, rightPrimitive);
        }
        return null;
    }

    private static CompiledType findBitwiseNotType(CompiledType left) {
        if (left instanceof PrimitiveLinkedType leftPrimitive) {
            return switch (leftPrimitive) {
                case INT -> leftPrimitive;
                default -> null;
            };
        }
        return null;
    }

    private static CompiledType findLogicalType(CompiledType left, CompiledType right) {
        if (isBooleanConvertibleType(left) && isBooleanConvertibleType(right)) {
            return BOOL;
        }
        return null;
    }

    private static boolean isBooleanConvertibleType(CompiledType type) {
        if (type == BOOL) {
            return true;
        }
        if (type == STRING) {
            return true;
        }
        if (type instanceof CompiledList || type instanceof CompiledSet || type instanceof CompiledDict) {
            return true;
        }
        return type instanceof PrimitiveLinkedType primitive && isNumericPrimitive(primitive);
    }

    private static CompiledType findPlusPrimitiveType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (left == STRING) {
            return isNumericPrimitive(right)
                   || right == STRING
                   || right == BOOL
                   || right == PrimitiveLinkedType.DATA
                   || right == PrimitiveLinkedType.ANY ? STRING : null;
        }
        if (right == STRING) {
            return isNumericPrimitive(left)
                   || left == BOOL
                   || left == PrimitiveLinkedType.DATA
                   || left == PrimitiveLinkedType.ANY ? STRING : null;
        }
        if (left == BOOL || right == BOOL) {
            return null;
        }
        if (isNumericPrimitive(left) && isNumericPrimitive(right)) {
            return promoteNumeric(left, right);
        }
        return null;
    }

    private static boolean isDataLikeType(CompiledType type) {
        return type == PrimitiveLinkedType.DATA || type instanceof GenericDataType;
    }

    private static CompiledType findMathPrimitiveType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (left == BOOL || right == BOOL) {
            return null;
        }
        if (left == STRING) {
            return isNumericPrimitive(right) ? STRING : null;
        }
        if (right == STRING) {
            return null;
        }
        if (isNumericPrimitive(left) && isNumericPrimitive(right)) {
            return promoteNumeric(left, right);
        }
        return null;
    }

    private static boolean isNumericPrimitive(PrimitiveLinkedType type) {
        return switch (type) {
            case BYTE, INT, LONG, FLOAT, DOUBLE -> true;
            default -> false;
        };
    }

    private static CompiledType findBitwisePrimitiveType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (left == PrimitiveLinkedType.INT && right == PrimitiveLinkedType.INT) {
            return PrimitiveLinkedType.INT;
        }
        return null;
    }

    private static boolean isBitwisePrimitive(PrimitiveLinkedType type) {
        return switch (type) {
            case INT -> true;
            default -> false;
        };
    }

    private static PrimitiveLinkedType promoteNumeric(PrimitiveLinkedType left, PrimitiveLinkedType right) {
        if (left == PrimitiveLinkedType.DOUBLE || right == PrimitiveLinkedType.DOUBLE) {
            return PrimitiveLinkedType.DOUBLE;
        }
        if (left == PrimitiveLinkedType.FLOAT || right == PrimitiveLinkedType.FLOAT) {
            return PrimitiveLinkedType.FLOAT;
        }
        if (left == PrimitiveLinkedType.LONG || right == PrimitiveLinkedType.LONG) {
            return PrimitiveLinkedType.LONG;
        }
        if (left == PrimitiveLinkedType.INT || right == PrimitiveLinkedType.INT) {
            return PrimitiveLinkedType.INT;
        }
        return PrimitiveLinkedType.BYTE;
    }

    private static CompiledType findQuestionType(CompiledType left, CompiledType right) {
        if (left instanceof CompiledList leftList) {
            if (right instanceof CompiledList) {
                return null;
            }
            var elementType = findHigherType(leftList.elementType(), right);
            return elementType == ANY ? null : BOOL;
        }
        if (left instanceof CompiledSet leftSet) {
            if (right instanceof CompiledSet) {
                return null;
            }
            var elementType = findHigherType(leftSet.elementType(), right);
            return elementType == ANY ? null : BOOL;
        }
        if (left instanceof CompiledDict) {
            return right == STRING ? BOOL : null;
        }
        if (left == STRING) {
            return right == STRING ? BOOL : null;
        }
        return null;
    }

    private static CompiledType collectionElementType(CompiledType type) {
        return switch (type) {
            case CompiledList linkedList -> linkedList.elementType();
            case CompiledSet linkedSet -> linkedSet.elementType();
            case CompiledDict linkedDict -> linkedDict.valueType();
            case CompiledTupleType tupleType -> tupleType.elementTypes().isEmpty() ? ANY : tupleType.elementTypes().getFirst();
            default -> null;
        };
    }

    private CompiledType slicedType(
            CompiledType sourceType,
            Optional<CompiledExpression> start,
            Optional<CompiledExpression> end
    ) {
        if (!(sourceType instanceof CompiledTupleType tupleType)) {
            return sourceType;
        }
        var size = tupleType.elementTypes().size();
        var startIndex = start.map(this::intLiteralValue).map(idx -> normalizeTupleIndex(idx, size)).orElse(0);
        var endIndex = end.map(this::intLiteralValue).map(idx -> normalizeTupleIndex(idx, size)).orElse(size);
        var normalizedStart = Math.max(0, Math.min(size, startIndex));
        var normalizedEnd = Math.max(normalizedStart, Math.min(size, endIndex));
        return new CompiledTupleType(tupleType.elementTypes().subList(normalizedStart, normalizedEnd));
    }

    private Result<CompiledType> tupleElementType(
            CompiledTupleType tupleType,
            CompiledExpression index,
            Optional<SourcePosition> position
    ) {
        var size = tupleType.elementTypes().size();
        var indexValue = intLiteralValue(index);
        var normalized = normalizeTupleIndex(indexValue, size);
        if (normalized < 0 || normalized >= size) {
            return withPosition(
                    Result.error("tuple index `" + indexValue + "` out of bounds for tuple size `" + size + "`"),
                    position
            );
        }
        return Result.success(tupleType.elementTypes().get(normalized));
    }

    private int intLiteralValue(CompiledExpression expression) {
        if (expression instanceof CompiledIntValue intValue) {
            return Integer.parseInt(intValue.intValue());
        }
        throw new IllegalStateException("Expected int literal expression, got: " + expression);
    }

    private int normalizeTupleIndex(int index, int size) {
        return index < 0 ? size + index : index;
    }

    private CompiledDataParentType findOptionType() {
        return dataTypes.entrySet().stream()
                .filter(entry -> isOptionTypeKey(entry.getKey()))
                .map(Map.Entry::getValue)
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .findFirst()
                .orElseGet(() -> dataTypes.values().stream()
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast)
                        .filter(type -> "Option".equals(type.name()))
                        .findFirst()
                        .orElse(null));
    }

    private CompiledDataParentType optionTypeFor(CompiledType elementType) {
        var optionType = findOptionType();
        if (optionType == null) {
            return null;
        }
        var typeParameters = optionType.typeParameters().isEmpty()
                ? List.<String>of()
                : List.of(linkedTypeDescriptor(elementType));
        return new CompiledDataParentType(optionType.name(), optionType.fields(), optionType.subTypes(), typeParameters);
    }

    private CompiledDataParentType findResultType() {
        return dataTypes.entrySet().stream()
                .filter(entry -> isResultTypeKey(entry.getKey()))
                .map(Map.Entry::getValue)
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .findFirst()
                .orElseGet(() -> dataTypes.values().stream()
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast)
                        .filter(type -> "Result".equals(type.name()))
                        .findFirst()
                        .orElse(null));
    }

    private CompiledDataParentType resultTypeFor(CompiledType elementType) {
        var resultType = findResultType();
        if (resultType == null) {
            return null;
        }
        var typeParameters = resultType.typeParameters().isEmpty()
                ? List.<String>of()
                : List.of(linkedTypeDescriptor(elementType));
        return new CompiledDataParentType(resultType.name(), resultType.fields(), resultType.subTypes(), typeParameters);
    }

    private boolean isOptionType(CompiledType type) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return false;
        }
        var normalized = normalizeTypeName(genericDataType.name());
        return "Option".equals(genericDataType.name())
               || normalized.endsWith("/Option.Option")
               || normalized.endsWith("/Option");
    }

    private CompiledType optionElementType(CompiledExpression expression) {
        if (expression instanceof CompiledPipeExpression pipeExpression && isOptionType(pipeExpression.type())) {
            return pipeExpression.mapper().type();
        }
        if (expression instanceof CompiledPipeFilterOutExpression filterOutExpression && isOptionType(filterOutExpression.type())) {
            return optionElementType(filterOutExpression.source());
        }
        if (isOptionType(expression.type())) {
            return ANY;
        }
        return expression.type();
    }

    private boolean isOptionTypeKey(String key) {
        var normalized = normalizeTypeName(key);
        return normalized.endsWith("/capy/lang/Option.Option")
               || normalized.endsWith("/cap/lang/Option.Option")
               || normalized.endsWith("/Option.Option");
    }

    private boolean isResultTypeKey(String key) {
        var normalized = normalizeTypeName(key);
        return normalized.endsWith("/capy/lang/Result.Result")
               || normalized.endsWith("/cap/lang/Result.Result")
               || normalized.endsWith("/Result.Result");
    }

    private String normalizeTypeName(String name) {
        var normalized = name.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private Optional<String> singleLambdaArgument(LambdaExpression lambdaExpression) {
        if (lambdaExpression.argumentNames().size() != 1) {
            return Optional.empty();
        }
        return Optional.of(lambdaExpression.argumentNames().get(0));
    }

    private Scope addLambdaBinding(Scope scope, String name, CompiledType type) {
        return "_".equals(name) ? scope : scope.add(name, type);
    }

    private static String encodeDictPipeArguments(String keyName, String valueName) {
        return keyName + DICT_PIPE_ARGS_SEPARATOR + valueName;
    }

    private static String encodeTuplePipeArguments(List<String> argumentNames) {
        return String.join(TUPLE_PIPE_ARGS_SEPARATOR, argumentNames);
    }

    private Result<PipeLambdaBinding> linkPipeLambdaArguments(
            Scope scope,
            LambdaExpression lambdaExpression,
            CompiledType elementType,
            String operator
    ) {
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var argumentName = argumentNames.getFirst();
            return Result.success(new PipeLambdaBinding(argumentName, addLambdaBinding(scope, argumentName, elementType)));
        }
        if (argumentNames.isEmpty()) {
            return withPosition(
                    Result.error("Right side lambda of `" + operator + "` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        if (!(elementType instanceof CompiledTupleType tupleType)) {
            return withPosition(
                    Result.error("Right side lambda of `" + operator + "` can use tuple destructuring only for tuple elements"),
                    lambdaExpression.position()
            );
        }
        if (tupleType.elementTypes().size() != argumentNames.size()) {
            return withPosition(
                    Result.error("Tuple destructuring in `" + operator + "` expects "
                                 + tupleType.elementTypes().size()
                                 + " arguments, got "
                                 + argumentNames.size()),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = scope;
        for (int i = 0; i < argumentNames.size(); i++) {
            lambdaScope = addLambdaBinding(lambdaScope, argumentNames.get(i), tupleType.elementTypes().get(i));
        }
        return Result.success(new PipeLambdaBinding(encodeTuplePipeArguments(argumentNames), lambdaScope));
    }

    private record PipeLambdaBinding(String argumentName, Scope scope) {
    }

    private Result<CompiledExpression> linkIntValue(IntValue intValue, Scope scope) {
        try {
            var parsed = new BigInteger(intValue.intValue(), 10);
            if (parsed.compareTo(BigInteger.valueOf(Integer.MIN_VALUE)) < 0
                || parsed.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
                return withPosition(Result.error("Int literal out of range: `" + intValue.intValue() + "`"), intValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(Result.error("Invalid int literal: `" + intValue.intValue() + "`"), intValue.position());
        }
        return Result.success(new CompiledIntValue(intValue.intValue()));
    }

    private Result<CompiledExpression> linkLongValue(LongValue longValue, Scope scope) {
        try {
            var raw = longValue.longValue();
            var normalized = raw.endsWith("l") || raw.endsWith("L")
                    ? raw.substring(0, raw.length() - 1)
                    : raw;
            var parsed = new BigInteger(normalized, 10);
            if (parsed.compareTo(BigInteger.valueOf(Long.MIN_VALUE)) < 0
                || parsed.compareTo(BigInteger.valueOf(Long.MAX_VALUE)) > 0) {
                return withPosition(Result.error("Long literal out of range: `" + raw + "`"), longValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(Result.error("Invalid long literal: `" + longValue.longValue() + "`"), longValue.position());
        }
        return Result.success(new CompiledLongValue(longValue.longValue()));
    }

    private Result<CompiledExpression> linkMatchExpression(MatchExpression matchExpression, Scope scope) {
        return linkExpression(matchExpression.matchWith(), scope)
                .flatMap(matchWith -> matchExpression.cases().stream()
                        .map(matchCase -> linkMatchCase(matchCase, matchWith, scope))
                        .collect(new ResultCollectionCollector<>())
                        .flatMap(cases -> validateMatchExhaustiveness(matchExpression, matchWith.type(), cases)
                                .map(ignored -> {
                            var matchType = cases.stream()
                                    .map(CompiledMatchExpression.MatchCase::expression)
                                    .map(CompiledExpression::type)
                                    .reduce(this::mergeBranchTypes)
                                    .orElse(ANY);
                            return (CompiledExpression) new CompiledMatchExpression(matchWith, cases, matchType);
                        })));
    }

    private CompiledType mergeBranchTypes(CompiledType left, CompiledType right) {
        if (left.equals(right)) {
            return left;
        }
        if (left instanceof CompiledDataParentType leftParent && right instanceof CompiledDataType rightData) {
            if (isSubtypeOfParent(rightData, leftParent)) {
                return canonicalizeParentAlias(leftParent);
            }
        }
        if (right instanceof CompiledDataParentType rightParent && left instanceof CompiledDataType leftData) {
            if (isSubtypeOfParent(leftData, rightParent)) {
                return canonicalizeParentAlias(rightParent);
            }
        }
        if (left instanceof CompiledDataType leftData && right instanceof CompiledDataType rightData) {
            var sharedParents = dataTypes.values().stream()
                    .filter(CompiledDataParentType.class::isInstance)
                    .map(CompiledDataParentType.class::cast)
                    .filter(parent -> isSubtypeOfParent(leftData, parent) && isSubtypeOfParent(rightData, parent))
                    .toList();
            if (sharedParents.size() == 1) {
                return specializeSharedParent(sharedParents.getFirst(), leftData, rightData);
            }
            if (!sharedParents.isEmpty()) {
                var uniqueByRawName = sharedParents.stream()
                        .collect(java.util.stream.Collectors.toMap(
                                parent -> simpleRawTypeName(normalizeTypeAlias(parent.name())),
                                parent -> parent,
                                (first, second) -> preferQualifiedParent(first, second),
                                java.util.LinkedHashMap::new
                        ));
                if (uniqueByRawName.size() == 1) {
                    return specializeSharedParent(
                            canonicalizeParentAlias(uniqueByRawName.values().iterator().next()),
                            leftData,
                            rightData
                    );
                }
            }
        }
        return findHigherType(left, right);
    }

    private CompiledDataParentType specializeSharedParent(
            CompiledDataParentType parent,
            CompiledDataType leftData,
            CompiledDataType rightData
    ) {
        if (parent.typeParameters().isEmpty()) {
            return canonicalizeParentAlias(parent);
        }
        var specializedDescriptors = mergeSharedParentTypeParameters(parent, leftData, rightData);
        if (specializedDescriptors.isEmpty()) {
            return canonicalizeParentAlias(parent);
        }
        return new CompiledDataParentType(
                canonicalizeParentAlias(parent).name(),
                parent.fields(),
                parent.subTypes(),
                specializedDescriptors,
                parent.enumType()
        );
    }

    private List<String> mergeSharedParentTypeParameters(
            CompiledDataParentType parent,
            CompiledDataType leftData,
            CompiledDataType rightData
    ) {
        var count = parent.typeParameters().size();
        if (count == 0) {
            return List.of();
        }
        var leftParameters = leftData.typeParameters();
        var rightParameters = rightData.typeParameters();
        if (leftParameters.isEmpty() && rightParameters.isEmpty()) {
            return List.of();
        }

        var merged = new ArrayList<String>(count);
        for (var i = 0; i < count; i++) {
            var leftDescriptor = i < leftParameters.size() ? leftParameters.get(i) : null;
            var rightDescriptor = i < rightParameters.size() ? rightParameters.get(i) : null;
            if (leftDescriptor != null && rightDescriptor != null) {
                if (leftDescriptor.equals(rightDescriptor)) {
                    merged.add(leftDescriptor);
                    continue;
                }
                return List.of();
            }
            if (leftDescriptor != null) {
                merged.add(leftDescriptor);
                continue;
            }
            if (rightDescriptor != null) {
                merged.add(rightDescriptor);
                continue;
            }
            return List.of();
        }
        return merged;
    }

    private CompiledDataParentType preferQualifiedParent(CompiledDataParentType first, CompiledDataParentType second) {
        var firstQualified = first.name().contains("/") || first.name().contains(".");
        var secondQualified = second.name().contains("/") || second.name().contains(".");
        if (firstQualified == secondQualified) {
            return first;
        }
        return secondQualified ? second : first;
    }

    private CompiledDataParentType canonicalizeParentAlias(CompiledDataParentType parent) {
        if (parent.name().contains("/") || parent.name().contains(".")) {
            return parent;
        }
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(candidate -> sameRawTypeName(candidate.name(), parent.name()))
                .filter(candidate -> candidate.name().contains("/") || candidate.name().contains("."))
                .max(java.util.Comparator.comparingInt(candidate -> candidate.name().length()))
                .orElse(parent);
    }

    private Result<Void> validateMatchExhaustiveness(
            MatchExpression matchExpression,
            CompiledType matchType,
            List<CompiledMatchExpression.MatchCase> cases
    ) {
        if (cases.stream().anyMatch(matchCase ->
                matchCase.pattern() instanceof CompiledMatchExpression.WildcardPattern
                || matchCase.pattern() instanceof CompiledMatchExpression.WildcardBindingPattern)) {
            return Result.success(null);
        }
        var requiredConstructors = requiredConstructorsForMatch(matchType);
        if (requiredConstructors.isEmpty()) {
            return withPosition(
                    Result.error("`match` is not exhaustive. Use wildcard `| _ -> ...`."),
                    matchExpression.position()
            );
        }
        var coveredConstructors = coveredConstructors(cases, requiredConstructors);
        var missing = requiredConstructors.stream()
                .filter(name -> !coveredConstructors.contains(name))
                .toList();
        if (missing.isEmpty()) {
            return Result.success(null);
        }
        var missingText = missing.stream()
                .map(name -> "`" + name + "`")
                .collect(java.util.stream.Collectors.joining(", "));
        return withPosition(
                Result.error("`match` is not exhaustive. Use wildcard `| _ -> ...` or add missing branches:" + missingText + "."),
                matchExpression.position()
        );
    }

    private java.util.LinkedHashSet<String> requiredConstructorsForMatch(CompiledType matchType) {
        if (matchType instanceof CompiledDataParentType parentType) {
            return parentType.subTypes().stream()
                    .map(CompiledDataType::name)
                    .collect(java.util.stream.Collectors.toCollection(java.util.LinkedHashSet::new));
        }
        if (matchType instanceof CompiledDataType dataType) {
            var single = new java.util.LinkedHashSet<String>();
            single.add(dataType.name());
            return single;
        }
        return new java.util.LinkedHashSet<>();
    }

    private java.util.LinkedHashSet<String> coveredConstructors(
            List<CompiledMatchExpression.MatchCase> cases,
            java.util.Set<String> requiredConstructors
    ) {
        var covered = new java.util.LinkedHashSet<String>();
        for (var matchCase : cases) {
            var pattern = matchCase.pattern();
            if (pattern instanceof CompiledMatchExpression.VariablePattern variablePattern) {
                if (requiredConstructors.contains(variablePattern.name())) {
                    covered.add(variablePattern.name());
                }
                continue;
            }
            if (pattern instanceof CompiledMatchExpression.ConstructorPattern constructorPattern) {
                covered.add(constructorPattern.constructorName());
                continue;
            }
            if (pattern instanceof CompiledMatchExpression.TypedPattern typedPattern) {
                if (typedPattern.type() instanceof CompiledDataType dataType) {
                    covered.add(dataType.name());
                } else if (typedPattern.type() instanceof CompiledDataParentType parentType) {
                    parentType.subTypes().stream()
                            .map(CompiledDataType::name)
                            .forEach(covered::add);
                }
            }
        }
        return covered;
    }

    private Result<CompiledMatchExpression.MatchCase> linkMatchCase(
            MatchExpression.MatchCase matchCase,
            CompiledExpression matchWith,
            Scope scope
    ) {
        return linkPattern(matchCase.pattern(), matchWith.type(), scope)
                .flatMap(patternAndScope -> {
                    var caseScope = patternAndScope.scope();
                    if (patternAndScope.pattern() instanceof CompiledMatchExpression.TypedPattern typedPattern
                        && matchWith instanceof CompiledVariable matchedVariable) {
                        // Flow typing: inside a typed match branch, treat the matched variable as narrowed too.
                        caseScope = caseScope.add(matchedVariable.name(), typedPattern.type());
                    }
                    return linkExpression(matchCase.expression(), caseScope)
                            .map(expression -> new CompiledMatchExpression.MatchCase(patternAndScope.pattern(), expression));
                });
    }

    private InfixExpression reAssociatePipeChain(InfixExpression expression) {
        if (!(expression.right() instanceof InfixExpression rightInfix)
            || !isPipeOperator(expression.operator())
            || !isPipeOperator(rightInfix.operator())) {
            return expression;
        }
        if (!(rightInfix.left() instanceof LambdaExpression
              || rightInfix.left() instanceof FunctionReference
              || rightInfix.left() instanceof ReduceExpression)) {
            return expression;
        }
        var leftAssociated = new InfixExpression(
                expression.left(),
                expression.operator(),
                rightInfix.left(),
                expression.position()
        );
        return new InfixExpression(
                leftAssociated,
                rightInfix.operator(),
                rightInfix.right(),
                rightInfix.position()
        );
    }

    private Result<PatternAndScope> linkPattern(MatchExpression.Pattern pattern, CompiledType matchType, Scope scope) {
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern -> validateLiteralPattern(intPattern, matchType, scope, PrimitiveLinkedType.INT);
            case MatchExpression.StringPattern stringPattern -> validateLiteralPattern(stringPattern, matchType, scope, PrimitiveLinkedType.STRING);
            case MatchExpression.BoolPattern boolPattern -> validateLiteralPattern(boolPattern, matchType, scope, PrimitiveLinkedType.BOOL);
            case MatchExpression.FloatPattern floatPattern -> validateLiteralPattern(floatPattern, matchType, scope, PrimitiveLinkedType.FLOAT);
            case MatchExpression.TypedPattern typedPattern -> linkTypedPattern(typedPattern, matchType, scope);
            case MatchExpression.VariablePattern variablePattern -> linkVariablePattern(variablePattern, matchType, scope);
            case MatchExpression.WildcardPattern wildcardPattern ->
                    Result.success(new PatternAndScope(CompiledMatchExpression.WildcardPattern.WILDCARD, scope));
            case MatchExpression.WildcardBindingPattern wildcardBindingPattern ->
                    Result.success(new PatternAndScope(
                            new CompiledMatchExpression.WildcardBindingPattern(wildcardBindingPattern.name()),
                            scope.add(wildcardBindingPattern.name(), matchType)
                    ));
            case MatchExpression.ConstructorPattern constructorPattern -> linkConstructorPattern(constructorPattern, matchType, scope);
        };
    }

    private Result<PatternAndScope> validateLiteralPattern(
            MatchExpression.Pattern pattern,
            CompiledType matchType,
            Scope scope,
            PrimitiveLinkedType expected
    ) {
        if (matchType != expected) {
            return Result.error("Cannot match `" + matchType + "` with literal of type `" + expected + "`");
        }
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern ->
                    Result.success(new PatternAndScope(new CompiledMatchExpression.IntPattern(intPattern.value()), scope));
            case MatchExpression.StringPattern stringPattern ->
                    Result.success(new PatternAndScope(new CompiledMatchExpression.StringPattern(stringPattern.value()), scope));
            case MatchExpression.BoolPattern boolPattern ->
                    Result.success(new PatternAndScope(new CompiledMatchExpression.BoolPattern(boolPattern.value()), scope));
            case MatchExpression.FloatPattern floatPattern ->
                    Result.success(new PatternAndScope(new CompiledMatchExpression.FloatPattern(floatPattern.value()), scope));
            default -> throw new IllegalStateException("Unexpected literal pattern: " + pattern);
        };
    }

    private Result<PatternAndScope> linkVariablePattern(
            MatchExpression.VariablePattern variablePattern,
            CompiledType matchType,
            Scope scope
    ) {
        if (matchType instanceof CompiledDataParentType parentType) {
            return findSubtype(variablePattern.name(), parentType)
                    .map(ignored -> new PatternAndScope(new CompiledMatchExpression.VariablePattern(variablePattern.name()), scope));
        }
        if (matchType instanceof CompiledDataType dataType && dataType.name().equals(variablePattern.name())) {
            return Result.success(new PatternAndScope(new CompiledMatchExpression.VariablePattern(variablePattern.name()), scope));
        }
        return Result.error("Cannot match `" + matchType + "` with constructor `" + variablePattern.name() + "`");
    }

    private Result<PatternAndScope> linkTypedPattern(
            MatchExpression.TypedPattern typedPattern,
            CompiledType matchType,
            Scope scope
    ) {
        return linkTypeInScope(typedPattern.type(), scope)
                .flatMap(patternType -> {
                    if (!isTypedPatternCompatible(matchType, patternType)) {
                        return Result.error("Cannot match `" + matchType + "` with typed pattern `" + patternType + "`");
                    }
                    return Result.success(new PatternAndScope(
                            new CompiledMatchExpression.TypedPattern(patternType, typedPattern.name()),
                            scope.add(typedPattern.name(), patternType)
                    ));
                });
    }

    private boolean isTypedPatternCompatible(CompiledType matchType, CompiledType patternType) {
        if (matchType == ANY || patternType == ANY) {
            return true;
        }
        if (patternType == PrimitiveLinkedType.DATA && matchType instanceof GenericDataType) {
            return true;
        }
        if (matchType == PrimitiveLinkedType.DATA && patternType instanceof GenericDataType) {
            return true;
        }
        if (matchType.equals(patternType)) {
            return true;
        }
        if (patternType instanceof CompiledDataType patternDataType
            && matchType instanceof CompiledDataParentType parentType) {
            return parentType.subTypes().stream().anyMatch(subType -> subType.name().equals(patternDataType.name()));
        }
        if (patternType instanceof CompiledDataParentType patternParentType
            && matchType instanceof CompiledDataType matchDataType) {
            return isSubtypeOfParent(matchDataType, patternParentType);
        }
        return false;
    }

    private Result<PatternAndScope> linkConstructorPattern(
            MatchExpression.ConstructorPattern constructorPattern,
            CompiledType matchType,
            Scope scope
    ) {
        return findConstructorType(constructorPattern.constructorName(), matchType)
                .flatMap(constructorType -> {
                    var resolvedFields = resolveConstructorFields(constructorType, matchType);
                    if (resolvedFields.size() != constructorPattern.fieldPatterns().size()) {
                        return Result.error("Constructor `" + constructorPattern.constructorName() + "` expects "
                                                  + resolvedFields.size() + " argument(s), got "
                                                  + constructorPattern.fieldPatterns().size());
                    }
                    var updatedScope = scope;
                    var linkedFieldPatterns = new java.util.ArrayList<CompiledMatchExpression.Pattern>(constructorPattern.fieldPatterns().size());
                    for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
                        var fieldPattern = constructorPattern.fieldPatterns().get(i);
                        if (fieldPattern instanceof MatchExpression.VariablePattern variablePattern) {
                            updatedScope = updatedScope.add(variablePattern.name(), resolvedFields.get(i).type());
                            linkedFieldPatterns.add(new CompiledMatchExpression.VariablePattern(variablePattern.name()));
                            continue;
                        }
                        var linkedPatternAndScope = linkPattern(fieldPattern, resolvedFields.get(i).type(), updatedScope);
                        if (linkedPatternAndScope instanceof Result.Error<PatternAndScope> error) {
                            return new Result.Error<>(error.errors());
                        }
                        var patternAndScope = ((Result.Success<PatternAndScope>) linkedPatternAndScope).value();
                        linkedFieldPatterns.add(patternAndScope.pattern());
                        updatedScope = patternAndScope.scope();
                    }
                    return Result.success(new PatternAndScope(
                            new CompiledMatchExpression.ConstructorPattern(
                                    constructorPattern.constructorName(),
                                    List.copyOf(linkedFieldPatterns)
                            ),
                            updatedScope
                    ));
                });
    }

    private List<CompiledDataType.CompiledField> resolveConstructorFields(CompiledDataType constructorType, CompiledType matchType) {
        if (!(matchType instanceof CompiledDataParentType parentType)) {
            return constructorType.fields();
        }
        var instantiatedParentFields = resolveFieldsFromInstantiatedParent(constructorType.name(), parentType);
        if (instantiatedParentFields != null) {
            return instantiatedParentFields;
        }
        if (constructorType.fields().stream().map(CompiledDataType.CompiledField::type).noneMatch(this::containsGenericTypeParameter)) {
            return constructorType.fields();
        }
        var rawConstructorType = resolveRawConstructorType(constructorType);
        if (rawConstructorType.typeParameters().isEmpty() || parentType.typeParameters().isEmpty()) {
            if (parentType.typeParameters().isEmpty()) {
                return rawConstructorType.fields();
            }
        }
        var substitutions = new java.util.HashMap<String, CompiledType>();
        var inferredTypeParameters = inferGenericParameterNames(rawConstructorType.fields());
        var constructorTypeParameters = inferredTypeParameters.isEmpty()
                ? rawConstructorType.typeParameters()
                : inferredTypeParameters;
        var actualTypeDescriptors = shouldUseParentTypeDescriptors(constructorType, rawConstructorType)
                ? parentType.typeParameters()
                : constructorType.typeParameters();
        var max = Math.min(constructorTypeParameters.size(), actualTypeDescriptors.size());
        for (int i = 0; i < max; i++) {
            var typeParameterName = constructorTypeParameters.get(i);
            parseLinkedTypeDescriptor(actualTypeDescriptors.get(i))
                    .ifPresent(type -> substitutions.put(typeParameterName, type));
        }
        if (substitutions.isEmpty()) {
            return rawConstructorType.fields();
        }
        return rawConstructorType.fields().stream()
                .map(field -> new CompiledDataType.CompiledField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                .toList();
    }

    private CompiledDataType resolveRawConstructorType(CompiledDataType constructorType) {
        var resolvedType = resolveDataTypeByName(constructorType.name());
        return resolvedType instanceof CompiledDataType linkedDataType ? linkedDataType : constructorType;
    }

    private List<CompiledDataType.CompiledField> resolveFieldsFromInstantiatedParent(String constructorName, CompiledDataParentType parentType) {
        var rawParentType = resolveDataTypeByName(parentType.name());
        if (!(rawParentType instanceof CompiledDataParentType linkedParentType) || linkedParentType.typeParameters().isEmpty()) {
            return null;
        }
        var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
        var max = Math.min(linkedParentType.typeParameters().size(), parentType.typeParameters().size());
        for (var i = 0; i < max; i++) {
            var descriptor = parentType.typeParameters().get(i);
            var parsed = parseLinkedTypeDescriptor(descriptor);
            if (parsed.isEmpty()) {
                return null;
            }
            substitutions.put(linkedParentType.typeParameters().get(i), parsed.orElseThrow());
        }
        var instantiatedParentType = (CompiledDataParentType) substituteTypeParameters(linkedParentType, substitutions);
        return instantiatedParentType.subTypes().stream()
                .filter(subType -> subType.name().equals(constructorName))
                .findFirst()
                .map(CompiledDataType::fields)
                .orElse(null);
    }

    private boolean shouldUseParentTypeDescriptors(CompiledDataType constructorType, CompiledDataType rawConstructorType) {
        if (constructorType.typeParameters().isEmpty()) {
            return true;
        }
        if (rawConstructorType.typeParameters().isEmpty()) {
            return constructorType.typeParameters().stream().allMatch(this::isGenericDescriptorName);
        }
        return constructorType.typeParameters().equals(rawConstructorType.typeParameters());
    }

    private boolean isGenericDescriptorName(String descriptor) {
        return parseLinkedTypeDescriptor(descriptor)
                .map(CompiledGenericTypeParameter.class::isInstance)
                .orElseGet(() -> Character.isUpperCase(descriptor.charAt(0)));
    }

    private boolean containsGenericTypeParameter(CompiledType type) {
        return switch (type) {
            case CompiledGenericTypeParameter ignored -> true;
            case CompiledList linkedList -> containsGenericTypeParameter(linkedList.elementType());
            case CompiledSet linkedSet -> containsGenericTypeParameter(linkedSet.elementType());
            case CompiledDict linkedDict -> containsGenericTypeParameter(linkedDict.valueType());
            case CompiledFunctionType functionType ->
                    containsGenericTypeParameter(functionType.argumentType()) || containsGenericTypeParameter(functionType.returnType());
            case CompiledTupleType tupleType -> tupleType.elementTypes().stream().anyMatch(this::containsGenericTypeParameter);
            case CompiledDataType linkedDataType -> linkedDataType.fields().stream()
                    .map(CompiledDataType.CompiledField::type)
                    .anyMatch(this::containsGenericTypeParameter);
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.fields().stream()
                    .map(CompiledDataType.CompiledField::type)
                    .anyMatch(this::containsGenericTypeParameter);
            default -> false;
        };
    }

    private List<String> inferGenericParameterNames(List<CompiledDataType.CompiledField> fields) {
        var names = new java.util.LinkedHashSet<String>();
        fields.forEach(field -> collectGenericParameterNames(field.type(), names));
        return List.copyOf(names);
    }

    private void collectGenericParameterNames(CompiledType type, java.util.Set<String> names) {
        switch (type) {
            case CompiledGenericTypeParameter genericTypeParameter -> names.add(genericTypeParameter.name());
            case CompiledList linkedList -> collectGenericParameterNames(linkedList.elementType(), names);
            case CompiledSet linkedSet -> collectGenericParameterNames(linkedSet.elementType(), names);
            case CompiledDict linkedDict -> collectGenericParameterNames(linkedDict.valueType(), names);
            case CompiledFunctionType functionType -> {
                collectGenericParameterNames(functionType.argumentType(), names);
                collectGenericParameterNames(functionType.returnType(), names);
            }
            case CompiledTupleType linkedTupleType ->
                    linkedTupleType.elementTypes().forEach(elementType -> collectGenericParameterNames(elementType, names));
            case CompiledDataType linkedDataType -> {
                linkedDataType.fields().forEach(field -> collectGenericParameterNames(field.type(), names));
                linkedDataType.typeParameters().forEach(typeDescriptor ->
                        parseLinkedTypeDescriptor(typeDescriptor)
                                .ifPresent(parsedType -> collectGenericParameterNames(parsedType, names)));
            }
            case CompiledDataParentType linkedDataParentType -> {
                linkedDataParentType.fields().forEach(field -> collectGenericParameterNames(field.type(), names));
                linkedDataParentType.typeParameters().forEach(typeDescriptor ->
                        parseLinkedTypeDescriptor(typeDescriptor)
                                .ifPresent(parsedType -> collectGenericParameterNames(parsedType, names)));
            }
            default -> {
            }
        }
    }

    private CompiledType substituteTypeParameters(CompiledType type, Map<String, CompiledType> substitutions) {
        if (type instanceof CompiledGenericTypeParameter genericTypeParameter) {
            return substitutions.getOrDefault(genericTypeParameter.name(), type);
        }
        return switch (type) {
            case CompiledList linkedList -> new CompiledList(substituteTypeParameters(linkedList.elementType(), substitutions));
            case CompiledSet linkedSet -> new CompiledSet(substituteTypeParameters(linkedSet.elementType(), substitutions));
            case CompiledDict linkedDict -> new CompiledDict(substituteTypeParameters(linkedDict.valueType(), substitutions));
            case CompiledFunctionType functionType -> new CompiledFunctionType(
                    substituteTypeParameters(functionType.argumentType(), substitutions),
                    substituteTypeParameters(functionType.returnType(), substitutions)
            );
            case CompiledTupleType linkedTupleType -> new CompiledTupleType(
                    linkedTupleType.elementTypes().stream()
                            .map(elementType -> substituteTypeParameters(elementType, substitutions))
                            .toList()
            );
            case CompiledDataType linkedDataType -> new CompiledDataType(
                    linkedDataType.name(),
                    linkedDataType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    substituteTypeParameters(field.type(), substitutions)
                            ))
                            .toList(),
                    linkedDataType.typeParameters().stream()
                            .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                            .toList(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.singleton()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    linkedDataParentType.name(),
                    linkedDataParentType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    substituteTypeParameters(field.type(), substitutions)
                            ))
                            .toList(),
                    linkedDataParentType.subTypes().stream()
                            .map(subType -> (CompiledDataType) substituteTypeParameters(subType, substitutions))
                            .toList(),
                    linkedDataParentType.typeParameters().stream()
                            .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                            .toList(),
                    linkedDataParentType.enumType()
            );
            default -> type;
        };
    }

    private String substituteTypeDescriptor(String descriptor, Map<String, CompiledType> substitutions) {
        if (descriptor == null || descriptor.isBlank()) {
            return descriptor;
        }
        var direct = substitutions.get(descriptor.trim());
        if (direct != null) {
            return linkedTypeDescriptor(direct);
        }
        var parsed = parseLinkedTypeDescriptor(descriptor.trim());
        if (parsed.isPresent()) {
            return linkedTypeDescriptor(substituteTypeParameters(parsed.get(), substitutions));
        }
        return descriptor;
    }

    private Optional<CompiledType> parseLinkedTypeDescriptor(String descriptor) {
        var normalizedDescriptor = descriptor == null ? "" : descriptor.trim();
        if (normalizedDescriptor.isEmpty()) {
            return Optional.empty();
        }
        if (normalizedDescriptor.startsWith("(") && normalizedDescriptor.endsWith(")")) {
            var inner = normalizedDescriptor.substring(1, normalizedDescriptor.length() - 1).trim();
            var arrowIndex = findTopLevelFunctionArrow(inner);
            if (arrowIndex > 0) {
                var argumentDescriptor = inner.substring(0, arrowIndex).trim();
                var returnDescriptor = inner.substring(arrowIndex + 2).trim();
                var argumentType = parseLinkedTypeDescriptor(argumentDescriptor);
                var returnType = parseLinkedTypeDescriptor(returnDescriptor);
                if (argumentType.isPresent() && returnType.isPresent()) {
                    return Optional.of(new CompiledFunctionType(argumentType.orElseThrow(), returnType.orElseThrow()));
                }
                return Optional.empty();
            }
        }
        if (normalizedDescriptor.startsWith("list[") && normalizedDescriptor.endsWith("]")) {
            return parseLinkedTypeDescriptor(normalizedDescriptor.substring(5, normalizedDescriptor.length() - 1))
                    .map(CompiledList::new);
        }
        if (normalizedDescriptor.startsWith("set[") && normalizedDescriptor.endsWith("]")) {
            return parseLinkedTypeDescriptor(normalizedDescriptor.substring(4, normalizedDescriptor.length() - 1))
                    .map(CompiledSet::new);
        }
        if (normalizedDescriptor.startsWith("dict[") && normalizedDescriptor.endsWith("]")) {
            return parseLinkedTypeDescriptor(normalizedDescriptor.substring(5, normalizedDescriptor.length() - 1))
                    .map(CompiledDict::new);
        }
        if (normalizedDescriptor.startsWith("tuple[") && normalizedDescriptor.endsWith("]")) {
            var inner = normalizedDescriptor.substring(6, normalizedDescriptor.length() - 1);
            var elementTypes = splitTopLevelTypeDescriptors(inner).stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .toList();
            if (elementTypes.stream().anyMatch(Optional::isEmpty)) {
                return Optional.empty();
            }
            return Optional.of(new CompiledTupleType(elementTypes.stream().map(Optional::orElseThrow).toList()));
        }
        var genericTypeStart = normalizedDescriptor.indexOf('[');
        if (genericTypeStart > 0 && normalizedDescriptor.endsWith("]")) {
            var baseName = normalizedDescriptor.substring(0, genericTypeStart).trim();
            var argsRaw = normalizedDescriptor.substring(genericTypeStart + 1, normalizedDescriptor.length() - 1);
            var baseType = resolveDataTypeByName(baseName);
            if (baseType == null) {
                return Optional.empty();
            }
            var parsedTypeArguments = splitTopLevelTypeDescriptors(argsRaw).stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .toList();
            if (parsedTypeArguments.stream().anyMatch(Optional::isEmpty)) {
                return Optional.empty();
            }
            var typeArguments = parsedTypeArguments.stream().map(Optional::orElseThrow).toList();
            var typeArgumentDescriptors = typeArguments.stream().map(this::linkedTypeDescriptor).toList();
            return Optional.of(switch (baseType) {
                case CompiledDataParentType parentType -> {
                    if (parentType.typeParameters().isEmpty()) {
                        yield new CompiledDataParentType(
                                parentType.name(),
                                parentType.fields(),
                                parentType.subTypes(),
                                typeArgumentDescriptors,
                                parentType.enumType()
                        );
                    }
                    var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
                    var max = Math.min(parentType.typeParameters().size(), typeArguments.size());
                    for (var i = 0; i < max; i++) {
                        substitutions.put(parentType.typeParameters().get(i), typeArguments.get(i));
                    }
                    var substituted = (CompiledDataParentType) substituteTypeParameters(parentType, substitutions);
                    yield new CompiledDataParentType(
                            substituted.name(),
                            substituted.fields(),
                            substituted.subTypes(),
                            typeArgumentDescriptors,
                            substituted.enumType()
                    );
                }
                case CompiledDataType dataType -> {
                    if (dataType.typeParameters().isEmpty()) {
                        yield new CompiledDataType(
                                dataType.name(),
                                dataType.fields(),
                                typeArgumentDescriptors,
                                dataType.extendedTypes(),
                                dataType.singleton()
                        );
                    }
                    var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
                    var max = Math.min(dataType.typeParameters().size(), typeArguments.size());
                    for (var i = 0; i < max; i++) {
                        substitutions.put(dataType.typeParameters().get(i), typeArguments.get(i));
                    }
                    var substitutedFields = dataType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    substituteTypeParameters(field.type(), substitutions)
                            ))
                            .toList();
                    yield new CompiledDataType(
                            dataType.name(),
                            substitutedFields,
                            typeArgumentDescriptors,
                            dataType.extendedTypes(),
                            dataType.singleton()
                    );
                }
            });
        }
        try {
            return Optional.of(PrimitiveLinkedType.valueOf(normalizedDescriptor.toUpperCase(java.util.Locale.ROOT)));
        } catch (IllegalArgumentException ignored) {
            // Not a primitive descriptor.
        }
        if (normalizedDescriptor.length() == 1
            && Character.isUpperCase(normalizedDescriptor.charAt(0))) {
            return Optional.of(new CompiledGenericTypeParameter(normalizedDescriptor));
        }
        var direct = resolveDataTypeByName(normalizedDescriptor);
        if (direct != null) {
            return Optional.of(direct);
        }
        return Optional.empty();
    }

    private GenericDataType resolveDataTypeByName(String typeName) {
        var direct = dataTypes.get(typeName);
        if (direct != null) {
            return direct;
        }
        return dataTypes.entrySet().stream()
                .filter(entry -> entry.getKey().endsWith("." + typeName) || entry.getKey().endsWith("/" + typeName))
                .map(Map.Entry::getValue)
                .findFirst()
                .orElse(null);
    }

    private List<String> splitTopLevelTypeDescriptors(String text) {
        var values = new java.util.ArrayList<String>();
        var bracketDepth = 0;
        var parenDepth = 0;
        var current = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            var ch = text.charAt(i);
            if (ch == '[') {
                bracketDepth++;
                current.append(ch);
                continue;
            }
            if (ch == ']') {
                bracketDepth = Math.max(0, bracketDepth - 1);
                current.append(ch);
                continue;
            }
            if (ch == '(') {
                parenDepth++;
                current.append(ch);
                continue;
            }
            if (ch == ')') {
                parenDepth = Math.max(0, parenDepth - 1);
                current.append(ch);
                continue;
            }
            if (ch == ',' && bracketDepth == 0 && parenDepth == 0) {
                var token = current.toString().trim();
                if (!token.isEmpty()) {
                    values.add(token);
                }
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        var token = current.toString().trim();
        if (!token.isEmpty()) {
            values.add(token);
        }
        return List.copyOf(values);
    }

    private int findTopLevelFunctionArrow(String text) {
        var bracketDepth = 0;
        var parenDepth = 0;
        for (int i = 0; i < text.length() - 1; i++) {
            var ch = text.charAt(i);
            if (ch == '[') {
                bracketDepth++;
                continue;
            }
            if (ch == ']') {
                bracketDepth = Math.max(0, bracketDepth - 1);
                continue;
            }
            if (ch == '(') {
                parenDepth++;
                continue;
            }
            if (ch == ')') {
                parenDepth = Math.max(0, parenDepth - 1);
                continue;
            }
            if (ch == '=' && text.charAt(i + 1) == '>' && bracketDepth == 0 && parenDepth == 0) {
                return i;
            }
        }
        return -1;
    }

    private String linkedTypeDescriptor(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase();
            case CompiledList linkedList -> "list[" + linkedTypeDescriptor(linkedList.elementType()) + "]";
            case CompiledSet linkedSet -> "set[" + linkedTypeDescriptor(linkedSet.elementType()) + "]";
            case CompiledDict linkedDict -> "dict[" + linkedTypeDescriptor(linkedDict.valueType()) + "]";
            case CompiledTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(this::linkedTypeDescriptor)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledFunctionType linkedFunctionType ->
                    "(" + linkedTypeDescriptor(linkedFunctionType.argumentType()) + " => " + linkedTypeDescriptor(linkedFunctionType.returnType()) + ")";
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? linkedDataType.name()
                    : linkedDataType.name() + "[" + String.join(", ", linkedDataType.typeParameters()) + "]";
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? linkedDataParentType.name()
                    : linkedDataParentType.name() + "[" + String.join(", ", linkedDataParentType.typeParameters()) + "]";
            case CompiledGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private Result<CompiledDataType> findConstructorType(String constructorName, CompiledType matchType) {
        if (matchType instanceof CompiledDataType linkedDataType) {
            if (linkedDataType.name().equals(constructorName)) {
                return Result.success(linkedDataType);
            }
            return Result.error("Type `" + linkedDataType.name() + "` cannot match constructor `" + constructorName + "`");
        }
        if (matchType instanceof CompiledDataParentType parentType) {
            return findSubtype(constructorName, parentType);
        }
        return Result.error("Type `" + matchType + "` is not matchable with constructor `" + constructorName + "`");
    }

    private Result<CompiledDataType> findSubtype(String constructorName, CompiledDataParentType parentType) {
        return parentType.subTypes().stream()
                .filter(subType -> subType.name().equals(constructorName))
                .findFirst()
                .map(Result::success)
                .orElseGet(() -> Result.error("Constructor `" + constructorName + "` not found in type `" + parentType.name() + "`"));
    }

    private Result<CompiledExpression> linkNewListExpression(NewListExpression expression, Scope scope) {
        return linkNewListExpression(expression, scope, null);
    }

    private Result<CompiledExpression> linkNewListExpression(NewListExpression expression, Scope scope, CompiledType expectedElementType) {
        return expression.values().stream()
                .map(value -> expectedElementType == null
                        ? linkExpression(value, scope)
                        : linkArgumentForExpectedType(value, scope, expectedElementType)
                        .map(CoercedArgument::expression))
                .collect(new ResultCollectionCollector<>())
                .map(values -> {
                    var elementType = expectedElementType == null
                            ? values.stream()
                                    .map(CompiledExpression::type)
                                    .reduce(CapybaraTypeFinder::findHigherType)
                                    .orElse(ANY)
                            : expectedElementType;
                    return (CompiledExpression) new CompiledNewList(values, new CompiledList(elementType));
                });
    }

    private Result<CompiledExpression> linkNewSetExpression(NewSetExpression expression, Scope scope) {
        return linkNewSetExpression(expression, scope, null);
    }

    private Result<CompiledExpression> linkNewSetExpression(NewSetExpression expression, Scope scope, CompiledType expectedElementType) {
        return expression.values().stream()
                .map(value -> expectedElementType == null
                        ? linkExpression(value, scope)
                        : linkArgumentForExpectedType(value, scope, expectedElementType)
                        .map(CoercedArgument::expression))
                .collect(new ResultCollectionCollector<>())
                .map(values -> {
                    var elementType = expectedElementType == null
                            ? values.stream()
                                    .map(CompiledExpression::type)
                                    .reduce(CapybaraTypeFinder::findHigherType)
                                    .orElse(ANY)
                            : expectedElementType;
                    return (CompiledExpression) new CompiledNewSet(values, new CompiledSet(elementType));
                });
    }

    private Result<CompiledExpression> linkNewDictExpression(NewDictExpression expression, Scope scope) {
        return linkNewDictExpression(expression, scope, null);
    }

    private Result<CompiledExpression> linkNewDictExpression(
            NewDictExpression expression,
            Scope scope,
            CompiledType expectedValueType
    ) {
        var entries = new java.util.ArrayList<CompiledNewDict.Entry>();
        for (var entry : expression.entries()) {
            var key = linkExpression(entry.key(), scope);
            if (key instanceof Result.Error<CompiledExpression> error) {
                return new Result.Error<>(error.errors());
            }
            var value = expectedValueType == null
                    ? linkExpression(entry.value(), scope)
                    : linkArgumentForExpectedType(entry.value(), scope, expectedValueType)
                    .map(CoercedArgument::expression);
            if (value instanceof Result.Error<CompiledExpression> error) {
                return new Result.Error<>(error.errors());
            }
            var linkedEntry = new CompiledNewDict.Entry(
                    ((Result.Success<CompiledExpression>) key).value(),
                    ((Result.Success<CompiledExpression>) value).value()
            );
            if (linkedEntry.key().type() != STRING) {
                return withPosition(
                        Result.error("dict keys must be of type `STRING`"),
                        entry.key().position().or(() -> expression.position())
                );
            }
            entries.add(linkedEntry);
        }

        var valueType = expectedValueType == null
                ? entries.stream()
                        .map(CompiledNewDict.Entry::value)
                        .map(CompiledExpression::type)
                        .reduce(CapybaraTypeFinder::findHigherType)
                        .orElse(ANY)
                : expectedValueType;
        return Result.success((CompiledExpression) new CompiledNewDict(List.copyOf(entries), new CompiledDict(valueType)));
    }

    private Result<CompiledExpression> linkNewData(NewData newData, Scope scope) {
        return linkTypeInScope(newData.type(), scope)
                .flatMap(type -> linkSpreadAssignments(newData.spreads(), scope)
                        .flatMap(spreadAssignments ->
                                linkFieldAssignment(newData.assignments(), scope, type)
                                        .flatMap(assignments -> linkPositionalAssignments(
                                                type,
                                                spreadAssignments,
                                                assignments,
                                                newData.positionalArguments(),
                                                scope,
                                                newData
                                        ).flatMap(positionalAssignments -> {
                                            var allAssignments = new java.util.ArrayList<CompiledNewData.FieldAssignment>(
                                                    spreadAssignments.size() + assignments.size() + positionalAssignments.size()
                                            );
                                            allAssignments.addAll(spreadAssignments);
                                            allAssignments.addAll(assignments);
                                            allAssignments.addAll(positionalAssignments);
                                            var validatedAssignments = validateRequiredAssignments(type, allAssignments, newData);
                                            if (validatedAssignments instanceof Result.Error<List<CompiledNewData.FieldAssignment>> error) {
                                                return new Result.Error<CompiledExpression>(error.errors());
                                            }
                                            return coerceAssignmentsForType(
                                                    type,
                                                    ((Result.Success<List<CompiledNewData.FieldAssignment>>) validatedAssignments).value(),
                                                    newData
                                            ).flatMap(coercedAssignments -> {
                                                var resolvedType = inferDataTypeFromAssignments(type, coercedAssignments);
                                                return coerceAssignmentsForType(
                                                        resolvedType,
                                                        List.copyOf(coercedAssignments),
                                                        newData
                                                ).map(resolvedAssignments -> (CompiledExpression) new CompiledNewData(
                                                        resolvedType,
                                                        List.copyOf(resolvedAssignments)
                                                ));
                                            });
                                        }))));
    }

    private CompiledType inferDataTypeFromAssignments(CompiledType type, List<CompiledNewData.FieldAssignment> assignments) {
        if (!(type instanceof GenericDataType genericDataType) || !hasGenericTypeParameters(type)) {
            return type;
        }
        var fieldsByName = genericDataType.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        CompiledDataType.CompiledField::name,
                        CompiledDataType.CompiledField::type,
                        (a, b) -> a
                ));
        var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
        for (var assignment : assignments) {
            var expectedType = fieldsByName.get(assignment.name());
            if (expectedType == null) {
                continue;
            }
            collectTypeSubstitutions(expectedType, assignment.value().type(), substitutions);
        }
        substitutions.entrySet().removeIf(entry -> !isConcreteResolvedType(entry.getValue()));
        if (substitutions.isEmpty()) {
            return type;
        }
        return substituteTypeParameters(type, substitutions);
    }

    private boolean hasGenericTypeParameters(CompiledType type) {
        if (type instanceof CompiledDataType linkedDataType) {
            return !linkedDataType.typeParameters().isEmpty();
        }
        if (type instanceof CompiledDataParentType linkedDataParentType) {
            return !linkedDataParentType.typeParameters().isEmpty();
        }
        return false;
    }

    private boolean isResolvedTypeForInference(CompiledType type) {
        return switch (type) {
            case CompiledGenericTypeParameter ignored -> false;
            case PrimitiveLinkedType primitive -> primitive != ANY;
            case CompiledList linkedList -> isResolvedTypeForInference(linkedList.elementType());
            case CompiledSet linkedSet -> isResolvedTypeForInference(linkedSet.elementType());
            case CompiledDict linkedDict -> isResolvedTypeForInference(linkedDict.valueType());
            case CompiledTupleType linkedTupleType -> linkedTupleType.elementTypes().stream().allMatch(this::isResolvedTypeForInference);
            case CompiledFunctionType linkedFunctionType ->
                    isResolvedTypeForInference(linkedFunctionType.argumentType())
                    && isResolvedTypeForInference(linkedFunctionType.returnType());
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters().stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .allMatch(maybeType -> maybeType.map(this::isResolvedTypeForInference).orElse(false));
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .allMatch(maybeType -> maybeType.map(this::isResolvedTypeForInference).orElse(false));
        };
    }

    private boolean isConcreteResolvedType(CompiledType type) {
        return switch (type) {
            case CompiledGenericTypeParameter ignored -> false;
            case PrimitiveLinkedType primitive -> primitive != ANY;
            case CompiledList linkedList -> isConcreteResolvedType(linkedList.elementType());
            case CompiledSet linkedSet -> isConcreteResolvedType(linkedSet.elementType());
            case CompiledDict linkedDict -> isConcreteResolvedType(linkedDict.valueType());
            case CompiledTupleType linkedTupleType -> linkedTupleType.elementTypes().stream().allMatch(this::isConcreteResolvedType);
            case CompiledFunctionType linkedFunctionType ->
                    isConcreteResolvedType(linkedFunctionType.argumentType())
                    && isConcreteResolvedType(linkedFunctionType.returnType());
            case CompiledDataType linkedDataType -> {
                if (linkedDataType.singleton() && linkedDataType.typeParameters().isEmpty()) {
                    yield true;
                }
                if (linkedDataType.typeParameters().isEmpty()) {
                    yield false;
                }
                yield linkedDataType.typeParameters().stream().allMatch(this::isSingletonDataDescriptor);
            }
            case CompiledDataParentType ignored -> false;
        };
    }

    private boolean isSingletonDataDescriptor(String descriptor) {
        return parseLinkedTypeDescriptor(descriptor)
                .filter(CompiledDataType.class::isInstance)
                .map(CompiledDataType.class::cast)
                .filter(linkedDataType -> linkedDataType.singleton() && linkedDataType.typeParameters().isEmpty())
                .isPresent();
    }

    private Result<List<CompiledNewData.FieldAssignment>> linkSpreadAssignments(List<Expression> spreads, Scope scope) {
        var assignments = new java.util.ArrayList<CompiledNewData.FieldAssignment>();
        for (var spread : spreads) {
            var linkedSpread = linkExpression(spread, scope);
            if (linkedSpread instanceof Result.Error<CompiledExpression> error) {
                return new Result.Error<>(error.errors());
            }
            var spreadExpression = ((Result.Success<CompiledExpression>) linkedSpread).value();
            if (!(spreadExpression.type() instanceof GenericDataType dataType)) {
                return withPosition(
                        Result.error("Spread assignment requires data type, was `" + spreadExpression.type() + "`"),
                        spread.position()
                );
            }
            for (var field : dataType.fields()) {
                assignments.add(new CompiledNewData.FieldAssignment(
                        field.name(),
                        new CompiledFieldAccess(spreadExpression, field.name(), field.type())
                ));
            }
        }
        return Result.success(List.copyOf(assignments));
    }

    private Result<List<CompiledNewData.FieldAssignment>> linkFieldAssignment(
            List<NewData.FieldAssignment> assignments,
            Scope scope,
            CompiledType type
    ) {
        var expectedByField = fieldTypesByName(type);
        return assignments.stream()
                .map(assignment -> {
                    var expected = expectedByField.get(assignment.name());
                    if (expected == null || !requiresExpectedTypeLinking(assignment.value(), expected)) {
                        return linkExpression(assignment.value(), scope)
                                .map(ex -> new CompiledNewData.FieldAssignment(assignment.name(), ex));
                    }
                    return linkArgumentForExpectedType(assignment.value(), scope, expected)
                            .map(coerced -> new CompiledNewData.FieldAssignment(assignment.name(), coerced.expression()));
                })
                .collect(new ResultCollectionCollector<>());
    }

    private Result<List<CompiledNewData.FieldAssignment>> linkPositionalAssignments(
            CompiledType type,
            List<CompiledNewData.FieldAssignment> spreadAssignments,
            List<CompiledNewData.FieldAssignment> namedAssignments,
            List<Expression> positionalArguments,
            Scope scope,
            NewData source
    ) {
        if (positionalArguments.isEmpty()) {
            return Result.success(List.of());
        }
        if (!(type instanceof GenericDataType genericDataType)) {
            return withPosition(
                    Result.error("Positional data arguments require data type, was `" + type + "`"),
                    source.position()
            );
        }

        var assignedNames = new java.util.LinkedHashSet<String>();
        spreadAssignments.stream().map(CompiledNewData.FieldAssignment::name).forEach(assignedNames::add);
        namedAssignments.stream().map(CompiledNewData.FieldAssignment::name).forEach(assignedNames::add);
        var availableFields = genericDataType.fields().stream()
                .filter(field -> !assignedNames.contains(field.name()))
                .toList();

        if (positionalArguments.size() > availableFields.size()) {
            return withPosition(
                    Result.error(
                            "Too many positional arguments for `" + genericDataType.name() + "`: expected at most "
                            + availableFields.size() + ", got " + positionalArguments.size()
                    ),
                    source.position()
            );
        }

        var mapped = new java.util.ArrayList<CompiledNewData.FieldAssignment>(positionalArguments.size());
        for (var i = 0; i < positionalArguments.size(); i++) {
            var field = availableFields.get(i);
            CompiledExpression expression;
            if (requiresExpectedTypeLinking(positionalArguments.get(i), field.type())) {
                var linked = linkArgumentForExpectedType(positionalArguments.get(i), scope, field.type());
                if (linked instanceof Result.Error<CoercedArgument> error) {
                    return new Result.Error<>(error.errors());
                }
                expression = ((Result.Success<CoercedArgument>) linked).value().expression();
            } else {
                var linked = linkExpression(positionalArguments.get(i), scope);
                if (linked instanceof Result.Error<CompiledExpression> error) {
                    return new Result.Error<>(error.errors());
                }
                expression = ((Result.Success<CompiledExpression>) linked).value();
            }
            mapped.add(new CompiledNewData.FieldAssignment(
                    field.name(),
                    expression
            ));
        }
        return Result.success(List.copyOf(mapped));
    }

    private boolean requiresExpectedTypeLinking(Expression expression, CompiledType expectedType) {
        if (expression instanceof LambdaExpression || expression instanceof FunctionReference) {
            return true;
        }
        if (expression instanceof NewListExpression && expectedType instanceof CompiledList) {
            return true;
        }
        if (expression instanceof NewSetExpression && expectedType instanceof CompiledSet) {
            return true;
        }
        return expression instanceof NewDictExpression && expectedType instanceof CompiledDict;
    }

    private Map<String, CompiledType> fieldTypesByName(CompiledType type) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return Map.of();
        }
        return genericDataType.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        CompiledDataType.CompiledField::name,
                        CompiledDataType.CompiledField::type,
                        (first, second) -> first
                ));
    }

    private Result<List<CompiledNewData.FieldAssignment>> validateRequiredAssignments(
            CompiledType type,
            List<CompiledNewData.FieldAssignment> assignments,
            NewData source
    ) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return Result.success(List.copyOf(assignments));
        }
        var assignedNames = assignments.stream()
                .map(CompiledNewData.FieldAssignment::name)
                .collect(java.util.stream.Collectors.toSet());
        var missingField = genericDataType.fields().stream()
                .map(CompiledDataType.CompiledField::name)
                .filter(fieldName -> !assignedNames.contains(fieldName))
                .findFirst();
        if (missingField.isPresent()) {
            return withPosition(
                    Result.error("Missing assignment for field `" + missingField.get() + "` in `" + genericDataType.name() + "`"),
                    source.position()
            );
        }
        return Result.success(List.copyOf(assignments));
    }

    private Result<List<CompiledNewData.FieldAssignment>> coerceAssignmentsForType(
            CompiledType type,
            List<CompiledNewData.FieldAssignment> assignments,
            NewData source
    ) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return Result.success(assignments);
        }
        var fieldsByName = genericDataType.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        CompiledDataType.CompiledField::name,
                        CompiledDataType.CompiledField::type,
                        (a, b) -> a
                ));
        var coercedAssignments = new java.util.ArrayList<CompiledNewData.FieldAssignment>(assignments.size());
        for (var assignment : assignments) {
            var expectedType = fieldsByName.get(assignment.name());
            if (expectedType == null) {
                coercedAssignments.add(assignment);
                continue;
            }
            var coerced = coerceArgument(assignment.value(), expectedType);
            if (coerced == null) {
                if (isHardFieldTypeMismatch(expectedType, assignment.value().type())) {
                    var assignmentPosition = findAssignmentValuePosition(source, assignment.name()).or(source::position);
                    return withPosition(
                            Result.error(
                                    "Expected `" + renderTypeForError(expectedType)
                                    + "`, but got `" + renderTypeForError(assignment.value().type()) + "`"
                            ),
                            assignmentPosition
                    );
                }
                coercedAssignments.add(assignment);
                continue;
            }
            coercedAssignments.add(new CompiledNewData.FieldAssignment(assignment.name(), coerced.expression()));
        }
        return Result.success(List.copyOf(coercedAssignments));
    }

    private Optional<SourcePosition> findAssignmentValuePosition(NewData source, String fieldName) {
        return source.assignments().stream()
                .filter(assignment -> assignment.name().equals(fieldName))
                .map(NewData.FieldAssignment::value)
                .map(Expression::position)
                .flatMap(Optional::stream)
                .findFirst();
    }

    private boolean isHardFieldTypeMismatch(CompiledType expected, CompiledType actual) {
        if (expected.equals(actual) || expected == ANY || actual == ANY || actual == NOTHING) {
            return false;
        }

        if (expected instanceof CompiledList) {
            return !(actual instanceof CompiledList);
        }
        if (expected instanceof CompiledSet) {
            return !(actual instanceof CompiledSet);
        }
        if (expected instanceof CompiledDict) {
            return !(actual instanceof CompiledDict);
        }
        if (expected instanceof CompiledTupleType) {
            return !(actual instanceof CompiledTupleType);
        }
        if (expected instanceof CompiledFunctionType) {
            return !(actual instanceof CompiledFunctionType);
        }

        if (expected instanceof PrimitiveLinkedType expectedPrimitive) {
            if (actual instanceof PrimitiveLinkedType actualPrimitive) {
                if (isNumericPrimitive(expectedPrimitive) && isNumericPrimitive(actualPrimitive)) {
                    return !isAssignableNumericPrimitive(expectedPrimitive, actualPrimitive);
                }
                return expectedPrimitive != actualPrimitive;
            }
            return true;
        }

        if (expected instanceof GenericDataType) {
            return actual instanceof PrimitiveLinkedType;
        }

        return false;
    }

    private boolean isAssignableNumericPrimitive(PrimitiveLinkedType expected, PrimitiveLinkedType actual) {
        return numericRank(actual) <= numericRank(expected);
    }

    private int numericRank(PrimitiveLinkedType type) {
        return switch (type) {
            case BYTE -> 1;
            case INT -> 2;
            case LONG -> 3;
            case FLOAT -> 4;
            case DOUBLE -> 5;
            default -> Integer.MAX_VALUE;
        };
    }

    private String renderTypeForError(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType.FLOAT -> "double";
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase(java.util.Locale.ROOT);
            default -> type.toString();
        };
    }

    private Result<CompiledExpression> linkStringValue(StringValue value, Scope scope) {
        return Result.success(new CompiledStringValue(value.stringValue()));
    }

    private Result<CompiledExpression> linkNothingValue(NothingValue value) {
        return Result.success(new CompiledNothingValue(value.position(), "Encountered `???`"));
    }

    private Result<CompiledExpression> linkValue(Value value, Scope scope) {
        if (scope.localValues().containsKey(value.name())) {
            // found local value
            // has to check if there were a rewrite
            String finalName;
//            if (scope.variableNameToUniqueName().containsKey(value.name())) {
//                finalName = scope.variableNameToUniqueName().get(value.name());
//            } else {
            finalName = value.name();
//            }
            return Result.success(new CompiledVariable(finalName, scope.localValues().get(value.name())));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(value.name()))
                .findAny()
                .map(p -> new CompiledVariable(p.name(), p.type()))
                .map(Result::<CompiledExpression>success)
                .orElseGet(() -> withPosition(
                        Result.error("Variable " + value.name() + " not found"),
                        value.position()
                ));
    }

    private Result<CompiledExpression> linkLetExpression(LetExpression expression, Scope scope) {
        return linkExpression(expression.value(), scope)
                .flatMap(value -> expression.declaredType()
                        .map(declaredType -> linkTypeInScope(declaredType, scope)
                                .flatMap(linkedDeclaredType -> {
                                    var coerced = coerceArgument(value, linkedDeclaredType);
                                    if (coerced == null) {
                                        return withPosition(
                                                Result.error(
                                                        "Cannot assign let `" + expression.name() + "` of type `"
                                                        + linkedDeclaredType + "` from `" + value.type() + "`"
                                                ),
                                                expression.position()
                                        );
                                    }
                                    var typedValue = coerced.expression();
                                    return linkExpression(expression.rest(), scope.add(expression.name(), linkedDeclaredType))
                                            .map(rest -> new CompiledLetExpression(
                                                    expression.name(),
                                                    typedValue,
                                                    rest
                                            ));
                                }))
                        .orElseGet(() ->
                                linkExpression(expression.rest(), scope.add(expression.name(), value.type()))
                                        .map(rest ->
                                                new CompiledLetExpression(
                                                        expression.name(),
                                                        value,
                                                        rest
                                                ))));
    }

    private Result<CompiledType> linkTypeInScope(Type type, Scope scope) {
        var knownTypes = new java.util.LinkedHashMap<String, GenericDataType>(dataTypes);
        var genericTypeNames = new java.util.LinkedHashSet<String>();
        parameters.forEach(parameter -> collectGenericParameterNames(parameter.type(), genericTypeNames));
        scope.localValues().values().forEach(localType -> collectGenericParameterNames(localType, genericTypeNames));
        genericTypeNames.forEach(typeName -> knownTypes.putIfAbsent(
                typeName,
                new CompiledDataParentType(typeName, List.of(), List.of(), List.of())
        ));
        return linkType(type, knownTypes)
                .map(linkedType -> replaceScopeGenericPlaceholders(linkedType, genericTypeNames));
    }

    private CompiledType replaceScopeGenericPlaceholders(CompiledType type, java.util.Set<String> genericTypeNames) {
        if (type instanceof CompiledDataParentType parentType
            && parentType.fields().isEmpty()
            && parentType.subTypes().isEmpty()
            && parentType.typeParameters().isEmpty()
            && genericTypeNames.contains(parentType.name())) {
            return new CompiledGenericTypeParameter(parentType.name());
        }
        return switch (type) {
            case CompiledList linkedList -> new CompiledList(
                    replaceScopeGenericPlaceholders(linkedList.elementType(), genericTypeNames)
            );
            case CompiledSet linkedSet -> new CompiledSet(
                    replaceScopeGenericPlaceholders(linkedSet.elementType(), genericTypeNames)
            );
            case CompiledDict linkedDict -> new CompiledDict(
                    replaceScopeGenericPlaceholders(linkedDict.valueType(), genericTypeNames)
            );
            case CompiledFunctionType functionType -> new CompiledFunctionType(
                    replaceScopeGenericPlaceholders(functionType.argumentType(), genericTypeNames),
                    replaceScopeGenericPlaceholders(functionType.returnType(), genericTypeNames)
            );
            case CompiledTupleType tupleType -> new CompiledTupleType(
                    tupleType.elementTypes().stream()
                            .map(elementType -> replaceScopeGenericPlaceholders(elementType, genericTypeNames))
                            .toList()
            );
            case CompiledDataType linkedDataType -> new CompiledDataType(
                    linkedDataType.name(),
                    linkedDataType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    replaceScopeGenericPlaceholders(field.type(), genericTypeNames)
                            ))
                            .toList(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.singleton()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    linkedDataParentType.name(),
                    linkedDataParentType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(
                                    field.name(),
                                    replaceScopeGenericPlaceholders(field.type(), genericTypeNames)
                            ))
                            .toList(),
                    linkedDataParentType.subTypes().stream()
                            .map(subType -> (CompiledDataType) replaceScopeGenericPlaceholders(subType, genericTypeNames))
                            .toList(),
                    linkedDataParentType.typeParameters(),
                    linkedDataParentType.enumType()
            );
            default -> type;
        };
    }

    private boolean isPipeMapExpression(InfixExpression expression) {
        return expression.right() instanceof LambdaExpression
               || expression.right() instanceof FunctionReference;
    }

    private static <T> Result<T> withPosition(Result<T> valueOrError, Optional<SourcePosition> position) {
        if (valueOrError instanceof Result.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new Result.Error<>(error.errors()
                    .stream()
                    .map(singleError -> new Result.Error.SingleError(
                            singleError.line() > 0 ? singleError.line() : pos.line(),
                            singleError.line() > 0 ? singleError.column() : pos.column(),
                            singleError.file(),
                            singleError.message()))
                    .toList());
        }
        return valueOrError;
    }

    private record PatternAndScope(CompiledMatchExpression.Pattern pattern, Scope scope) {
    }

    public record FunctionSignature(String name, List<CompiledType> parameterTypes, CompiledType returnType, Visibility visibility) {
        public FunctionSignature(String name, List<CompiledType> parameterTypes, CompiledType returnType) {
            this(name, parameterTypes, returnType, null);
        }
    }
    private record CoercedArgument(CompiledExpression expression, int coercions) {
    }

    private record CoercedArguments(List<CompiledExpression> arguments, int coercions) {
    }

    private record ResolvedFunctionCall(FunctionSignature signature, List<CompiledExpression> arguments, int coercions) {
    }

    private record PipeMapper(String argumentName, CompiledExpression expression) {
    }

    private record FunctionShape(List<CompiledType> parameterTypes, CompiledType returnType) {
    }

    private record ResolvedFunctionReference(CompiledExpression expression, int coercions) {
    }

    private record ResolvedModule(String javaModuleName, List<FunctionSignature> signatures) {
    }
}






