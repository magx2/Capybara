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

    public static final class LinkCache {
        private final Map<String, GenericDataType> fallbackDataTypesBySuffix;
        private final Map<String, GenericDataType> resolvedDataTypesByNameCache = new HashMap<>();
        private final Map<String, Optional<CompiledType>> parsedTypeDescriptorCache = new HashMap<>();
        private final Map<String, List<String>> splitTopLevelTypeDescriptorCache = new HashMap<>();
        private final IdentityHashMap<List<FunctionSignature>, SignatureIndex> signatureIndexes = new IdentityHashMap<>();
        private final Map<String, Set<String>> methodOwnerCandidatesBySimpleType;
        private final Map<String, Set<String>> subtypeParentOwnerCandidatesBySimpleType;
        private final CompiledDataParentType optionType;
        private final CompiledDataParentType resultType;

        public LinkCache(Map<String, GenericDataType> dataTypes) {
            this.fallbackDataTypesBySuffix = buildFallbackDataTypesBySuffix(dataTypes);
            this.methodOwnerCandidatesBySimpleType = buildMethodOwnerCandidatesBySimpleType(dataTypes);
            this.subtypeParentOwnerCandidatesBySimpleType = buildSubtypeParentOwnerCandidatesBySimpleType(dataTypes);
            this.optionType = findParentType(dataTypes, CapybaraExpressionCompiler::isOptionTypeKeyStatic, "Option");
            this.resultType = findParentType(dataTypes, CapybaraExpressionCompiler::isResultTypeKeyStatic, "Result");
        }
    }

    private record SignatureKey(String name, int arity) {
    }

    private record SignatureIndex(
            Map<SignatureKey, List<FunctionSignature>> functionsByNameAndArity,
            Map<String, List<FunctionSignature>> functionsByName,
            Map<SignatureKey, List<FunctionSignature>> methodsByNameAndArity
    ) {
    }

    private final List<CompiledFunction.CompiledFunctionParameter> parameters;
    private final Map<String, GenericDataType> dataTypes;
    private final LinkCache linkCache;
    private final List<FunctionSignature> functionSignatures;
    private final Map<String, List<FunctionSignature>> functionSignaturesByModule;
    private final Map<String, String> moduleClassNameByModuleName;
    private final ConstructorRegistry constructorRegistry;
    private final Optional<String> currentConstructorTypeName;

    public CapybaraExpressionCompiler(
            List<CompiledFunction.CompiledFunctionParameter> parameters,
            Map<String, GenericDataType> dataTypes,
            List<FunctionSignature> functionSignatures,
            Map<String, List<FunctionSignature>> functionSignaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            ConstructorRegistry constructorRegistry
    ) {
        this(parameters, dataTypes, functionSignatures, functionSignaturesByModule, moduleClassNameByModuleName, constructorRegistry, new LinkCache(dataTypes));
    }

    public CapybaraExpressionCompiler(
            List<CompiledFunction.CompiledFunctionParameter> parameters,
            Map<String, GenericDataType> dataTypes,
            List<FunctionSignature> functionSignatures,
            Map<String, List<FunctionSignature>> functionSignaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            ConstructorRegistry constructorRegistry,
            LinkCache linkCache
    ) {
        this(parameters, dataTypes, functionSignatures, functionSignaturesByModule, moduleClassNameByModuleName, constructorRegistry, linkCache, Optional.empty());
    }

    public CapybaraExpressionCompiler(
            List<CompiledFunction.CompiledFunctionParameter> parameters,
            Map<String, GenericDataType> dataTypes,
            List<FunctionSignature> functionSignatures,
            Map<String, List<FunctionSignature>> functionSignaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            ConstructorRegistry constructorRegistry,
            LinkCache linkCache,
            Optional<String> currentConstructorTypeName
    ) {
        this(parameters, dataTypes, functionSignatures, functionSignaturesByModule, moduleClassNameByModuleName, constructorRegistry, linkCache, currentConstructorTypeName, true);
    }

    private CapybaraExpressionCompiler(
            List<CompiledFunction.CompiledFunctionParameter> parameters,
            Map<String, GenericDataType> dataTypes,
            List<FunctionSignature> functionSignatures,
            Map<String, List<FunctionSignature>> functionSignaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            ConstructorRegistry constructorRegistry,
            LinkCache linkCache,
            Optional<String> currentConstructorTypeName,
            boolean ignored
    ) {
        this.parameters = parameters;
        this.dataTypes = dataTypes;
        this.linkCache = linkCache;
        this.functionSignatures = functionSignatures;
        this.functionSignaturesByModule = functionSignaturesByModule;
        this.moduleClassNameByModuleName = moduleClassNameByModuleName;
        this.constructorRegistry = constructorRegistry;
        this.currentConstructorTypeName = currentConstructorTypeName;
    }

    public Result<CompiledExpression> linkExpression(Expression expression) {
        return linkExpression(expression, Scope.EMPTY);
    }

    public Result<CompiledExpression> linkExpressionForExpectedType(Expression expression, CompiledType expectedType) {
        return linkArgumentForExpectedType(expression, Scope.EMPTY, expectedType)
                .map(CoercedArgument::expression);
    }

    private Result<CompiledExpression> linkExpression(Expression expression, Scope scope) {
        return switch (expression) {
            case BooleanValue booleanValue -> linkBooleanValue(booleanValue, scope);
            case ByteValue byteValue -> linkByteValue(byteValue, scope);
            case DoubleValue doubleValue -> linkDoubleValue(doubleValue, scope);
            case FieldAccess fieldAccess -> linkFieldAccess(fieldAccess, scope);
            case FloatValue floatValue -> linkFloatValue(floatValue, scope);
            case ConstructorData constructorData -> linkConstructorData(constructorData, scope);
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
            case WithExpression withExpression -> linkWithExpression(withExpression, scope);
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

    private CapybaraExpressionCompiler insideConstructor(String dataTypeName) {
        return new CapybaraExpressionCompiler(
                parameters,
                dataTypes,
                functionSignatures,
                functionSignaturesByModule,
                moduleClassNameByModuleName,
                constructorRegistry,
                linkCache,
                Optional.of(dataTypeName)
        );
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
        return linkFunctionCall(functionCall, scope, Optional.empty());
    }

    private Result<CompiledExpression> linkFunctionCall(
            FunctionCall functionCall,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        if (functionCall.moduleName().isPresent()) {
            return resolveQualifiedFunctionCall(functionCall, scope, expectedType);
        }
        var functionVariable = resolveFunctionVariable(functionCall.name(), scope);
        if (functionVariable.isPresent()) {
            return resolveFunctionInvoke(functionCall, scope, functionVariable.get());
        }
        return resolveGlobalFunctionCall(functionCall, scope, expectedType);
    }

    private Result<CompiledExpression> linkFunctionInvoke(FunctionInvoke functionInvoke, Scope scope) {
        return linkExpression(functionInvoke.function(), scope)
                .flatMap(function -> resolveFunctionInvoke(functionInvoke, scope, function));
    }

    private Result<CompiledExpression> resolveQualifiedFunctionCall(
            FunctionCall functionCall,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
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

        var candidates = functionsByNameAndArity(signatures, functionCall.name(), functionCall.arguments().size());
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
            var unsafeCollectionError = findUnsafeCollectionArgumentError(functionCall.arguments(), resolved.arguments(), candidate.parameterTypes());
            if (unsafeCollectionError != null) {
                deepestError = preferDeeperError(deepestError, unsafeCollectionError);
                continue;
            }
            var returnType = resolveReturnType(candidate, resolved.arguments());
            if (expectedType.isPresent() && !canCoerceToExpectedType(returnType, expectedType.orElseThrow())) {
                continue;
            }
            if (isBetterResolvedCall(candidate, resolved.coercions(), best)) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions(), returnType);
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
                best.returnType()
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

    private Result<CompiledExpression> resolveGlobalFunctionCall(
            FunctionCall functionCall,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        if (functionCall.name().startsWith(METHOD_INVOKE_PREFIX)) {
            return resolveMethodInvokeCall(functionCall, scope, expectedType);
        }
        var candidates = functionsByNameAndArity(functionSignatures, functionCall.name(), functionCall.arguments().size());
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
            var unsafeCollectionError = findUnsafeCollectionArgumentError(functionCall.arguments(), resolved.arguments(), candidate.parameterTypes());
            if (unsafeCollectionError != null) {
                deepestError = preferDeeperError(deepestError, unsafeCollectionError);
                continue;
            }
            var returnType = resolveReturnType(candidate, resolved.arguments());
            if (expectedType.isPresent() && !canCoerceToExpectedType(returnType, expectedType.orElseThrow())) {
                continue;
            }
            if (isBetterResolvedCall(candidate, resolved.coercions(), best)) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions(), returnType);
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
                best.returnType()
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

    private List<FunctionSignature> functionsByNameAndArity(
            List<FunctionSignature> signatures,
            String functionName,
            int arity
    ) {
        return signatureIndex(signatures).functionsByNameAndArity()
                .getOrDefault(new SignatureKey(functionName, arity), List.of());
    }

    private List<FunctionSignature> functionsByName(
            List<FunctionSignature> signatures,
            String functionName
    ) {
        return signatureIndex(signatures).functionsByName()
                .getOrDefault(functionName, List.of());
    }

    private List<FunctionSignature> methodsByNameAndArity(
            List<FunctionSignature> signatures,
            String methodName,
            int arity
    ) {
        return signatureIndex(signatures).methodsByNameAndArity()
                .getOrDefault(new SignatureKey(methodName, arity), List.of());
    }

    private SignatureIndex signatureIndex(List<FunctionSignature> signatures) {
        return linkCache.signatureIndexes.computeIfAbsent(signatures, this::buildSignatureIndex);
    }

    private SignatureIndex buildSignatureIndex(List<FunctionSignature> signatures) {
        var mutableFunctionsByNameAndArity = new LinkedHashMap<SignatureKey, ArrayList<FunctionSignature>>();
        var mutableFunctionsByName = new LinkedHashMap<String, ArrayList<FunctionSignature>>();
        var mutableMethodsByNameAndArity = new LinkedHashMap<SignatureKey, ArrayList<FunctionSignature>>();
        for (var signature : signatures) {
            var arity = signature.parameterTypes().size();
            if (signature.name().startsWith(METHOD_DECL_PREFIX)) {
                var separator = signature.name().indexOf("__", METHOD_DECL_PREFIX.length());
                if (separator >= 0 && separator + 2 <= signature.name().length()) {
                    var methodName = signature.name().substring(separator + 2);
                    mutableMethodsByNameAndArity.computeIfAbsent(
                            new SignatureKey(methodName, arity),
                            ignored -> new ArrayList<>()
                    ).add(signature);
                }
                continue;
            }
            mutableFunctionsByNameAndArity.computeIfAbsent(
                    new SignatureKey(signature.name(), arity),
                    ignored -> new ArrayList<>()
            ).add(signature);
            mutableFunctionsByName.computeIfAbsent(signature.name(), ignored -> new ArrayList<>()).add(signature);
        }
        var functionsByNameAndArity = new LinkedHashMap<SignatureKey, List<FunctionSignature>>();
        mutableFunctionsByNameAndArity.forEach((key, value) -> functionsByNameAndArity.put(key, List.copyOf(value)));
        var functionsByName = new LinkedHashMap<String, List<FunctionSignature>>();
        mutableFunctionsByName.forEach((key, value) -> functionsByName.put(key, List.copyOf(value)));
        var methodsByNameAndArity = new LinkedHashMap<SignatureKey, List<FunctionSignature>>();
        mutableMethodsByNameAndArity.forEach((key, value) -> methodsByNameAndArity.put(key, List.copyOf(value)));
        return new SignatureIndex(
                Map.copyOf(functionsByNameAndArity),
                Map.copyOf(functionsByName),
                Map.copyOf(methodsByNameAndArity)
        );
    }

    private Result<CompiledExpression> resolveMethodInvokeCall(FunctionCall functionCall, Scope scope) {
        return resolveMethodInvokeCall(functionCall, scope, Optional.empty());
    }

    private Result<CompiledExpression> resolveMethodInvokeCall(
            FunctionCall functionCall,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        var methodName = functionCall.name().substring(METHOD_INVOKE_PREFIX.length());
        var candidates = methodsByNameAndArity(functionSignatures, methodName, functionCall.arguments().size());
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
            var returnType = resolveReturnType(candidate, resolved.arguments());
            if (expectedType.isPresent() && !canCoerceToExpectedType(returnType, expectedType.orElseThrow())) {
                continue;
            }
            if (isBetterResolvedCall(candidate, resolved.coercions(), best)) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions(), returnType);
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
                best.returnType()
        ));
    }

    private Result.Error.SingleError findUnsafeCollectionArgumentError(
            List<Expression> rawArguments,
            List<CompiledExpression> resolvedArguments,
            List<CompiledType> expectedTypes
    ) {
        for (var i = 0; i < Math.min(rawArguments.size(), Math.min(resolvedArguments.size(), expectedTypes.size())); i++) {
            var actual = resolvedArguments.get(i).type();
            var expected = expectedTypes.get(i);
            if (!isUnsafeCollectionNarrowing(actual, expected)) {
                continue;
            }
            var position = rawArguments.get(i).position();
            var message = "Expected `" + expected + "`, got `" + actual + "`";
            return withPosition(Result.error(message), position) instanceof Result.Error<?> error
                    ? error.errors().first()
                    : null;
        }
        return null;
    }

    private boolean isUnsafeCollectionNarrowing(CompiledType actual, CompiledType expected) {
        if (actual instanceof CompiledList actualList && expected instanceof CompiledList expectedList) {
            return isUnsafeCollectionElementNarrowing(actualList.elementType(), expectedList.elementType());
        }
        if (actual instanceof CompiledSet actualSet && expected instanceof CompiledSet expectedSet) {
            return isUnsafeCollectionElementNarrowing(actualSet.elementType(), expectedSet.elementType());
        }
        if (actual instanceof CompiledDict actualDict && expected instanceof CompiledDict expectedDict) {
            return isUnsafeCollectionElementNarrowing(actualDict.valueType(), expectedDict.valueType());
        }
        return false;
    }

    private boolean isUnsafeCollectionElementNarrowing(CompiledType actualElement, CompiledType expectedElement) {
        if (expectedElement == ANY || expectedElement == DATA) {
            return false;
        }
        return actualElement == ANY || actualElement == DATA;
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
            var matchedExtendedParent = actualData.extendedTypes().stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .flatMap(Optional::stream)
                    .filter(GenericDataType.class::isInstance)
                    .map(GenericDataType.class::cast)
                    .filter(parentType -> sameRawTypeName(parentType.name(), expectedParent.name()))
                    .findFirst();
            if (matchedExtendedParent.isEmpty()) {
                matchedExtendedParent = instantiateExtendedParentFromSubtypeFields(actualData, expectedParent);
            }
            if (matchedExtendedParent.isEmpty()) {
                matchedExtendedParent = specializeParentFromSubtypeFields(actualData, expectedParent);
            }
            if (matchedExtendedParent.isPresent()) {
                collectTypeSubstitutions(expectedParent, matchedExtendedParent.orElseThrow(), substitutions);
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

    private Optional<GenericDataType> instantiateExtendedParentFromSubtypeFields(
            CompiledDataType actualData,
            CompiledDataParentType expectedParent
    ) {
        var resolvedType = resolveDataTypeByName(actualData.name());
        if (!(resolvedType instanceof CompiledDataType rawDataType)) {
            return Optional.empty();
        }
        var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
        var actualFieldsByName = actualData.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        CompiledDataType.CompiledField::name,
                        CompiledDataType.CompiledField::type,
                        (first, second) -> first,
                        java.util.LinkedHashMap::new
                ));
        for (var rawField : rawDataType.fields()) {
            var actualFieldType = actualFieldsByName.get(rawField.name());
            if (actualFieldType == null) {
                continue;
            }
            collectTypeSubstitutions(rawField.type(), actualFieldType, substitutions);
        }
        if (substitutions.isEmpty()) {
            return Optional.empty();
        }
        return rawDataType.extendedTypes().stream()
                .map(this::parseLinkedTypeDescriptor)
                .flatMap(Optional::stream)
                .map(parentType -> substituteTypeParameters(parentType, substitutions))
                .filter(GenericDataType.class::isInstance)
                .map(GenericDataType.class::cast)
                .filter(parentType -> sameRawTypeName(parentType.name(), expectedParent.name()))
                .findFirst();
    }

    private Optional<GenericDataType> specializeParentFromSubtypeFields(
            CompiledDataType actualData,
            CompiledDataParentType expectedParent
    ) {
        var canonicalParent = dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parentType -> sameRawTypeName(parentType.name(), expectedParent.name()))
                .findFirst()
                .orElse(expectedParent);
        var rawSubtype = canonicalParent.subTypes().stream()
                .filter(subType -> sameRawTypeName(subType.name(), actualData.name()))
                .findFirst();
        if (rawSubtype.isEmpty()) {
            return Optional.empty();
        }
        var subtypeSubstitutions = new java.util.LinkedHashMap<String, CompiledType>();
        var actualFieldsByName = actualData.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        CompiledDataType.CompiledField::name,
                        CompiledDataType.CompiledField::type,
                        (first, second) -> first,
                        java.util.LinkedHashMap::new
                ));
        for (var rawField : rawSubtype.orElseThrow().fields()) {
            var actualFieldType = actualFieldsByName.get(rawField.name());
            if (actualFieldType == null) {
                continue;
            }
            collectTypeSubstitutions(rawField.type(), actualFieldType, subtypeSubstitutions);
        }
        if (subtypeSubstitutions.isEmpty()) {
            return Optional.empty();
        }
        var parentSubstitutions = new java.util.LinkedHashMap<String, CompiledType>();
        var max = Math.min(canonicalParent.typeParameters().size(), rawSubtype.orElseThrow().typeParameters().size());
        for (var i = 0; i < max; i++) {
            var rawSubtypeTypeParameter = rawSubtype.orElseThrow().typeParameters().get(i);
            var concreteType = subtypeSubstitutions.get(rawSubtypeTypeParameter);
            if (concreteType != null) {
                parentSubstitutions.put(canonicalParent.typeParameters().get(i), concreteType);
            }
        }
        if (parentSubstitutions.isEmpty()) {
            return Optional.empty();
        }
        var specializedParent = substituteTypeParameters(canonicalParent, parentSubstitutions);
        return specializedParent instanceof GenericDataType genericDataType
                ? Optional.of(genericDataType)
                : Optional.empty();
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
        var rewrittenReduceMethod = rewriteReduceMethodInvoke(functionCall, scope, methodName);
        if (rewrittenReduceMethod.isPresent()) {
            return rewrittenReduceMethod;
        }
        var optionAlias = resolveBuiltinOptionMethodInvoke(functionCall, scope, methodName);
        if (optionAlias.isPresent()) {
            return optionAlias;
        }
        var pipeAlias = resolveBuiltinPipeAliasMethodInvoke(functionCall, scope, methodName);
        if (pipeAlias.isPresent()) {
            return pipeAlias;
        }
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

    private Optional<Result<CompiledExpression>> rewriteReduceMethodInvoke(
            FunctionCall functionCall,
            Scope scope,
            String methodName
    ) {
        if (!"reduce".equals(methodName) && !"reduce_left".equals(methodName)) {
            return Optional.empty();
        }
        if (functionCall.arguments().size() != 2 || !(functionCall.arguments().get(1) instanceof ReduceExpression reduceExpression)) {
            return Optional.empty();
        }
        if (methodsByNameAndArity(functionSignatures, methodName, 3).isEmpty()) {
            return Optional.empty();
        }
        var lambdaArguments = new java.util.ArrayList<String>();
        lambdaArguments.add(reduceExpression.accumulatorName());
        reduceExpression.keyName().ifPresent(lambdaArguments::add);
        lambdaArguments.add(reduceExpression.valueName());
        var rewritten = new FunctionCall(
                functionCall.moduleName(),
                functionCall.name(),
                List.of(
                        functionCall.arguments().getFirst(),
                        reduceExpression.initialValue(),
                        new LambdaExpression(lambdaArguments, reduceExpression.reducerExpression(), reduceExpression.position())
                ),
                functionCall.position()
        );
        return Optional.of(resolveMethodInvokeCall(rewritten, scope));
    }

    private Optional<Result<CompiledExpression>> resolveBuiltinOptionMethodInvoke(
            FunctionCall functionCall,
            Scope scope,
            String methodName
    ) {
        if (functionCall.arguments().isEmpty()) {
            return Optional.empty();
        }
        var linkedReceiver = linkExpression(functionCall.arguments().getFirst(), scope);
        if (linkedReceiver instanceof Result.Error<CompiledExpression> error) {
            return Optional.of(new Result.Error<>(error.errors()));
        }
        if (!(linkedReceiver instanceof Result.Success<CompiledExpression> receiverValue)) {
            return Optional.empty();
        }
        if (resolveSpecializedOptionType(receiverValue.value().type()).isEmpty()) {
            return Optional.empty();
        }
        return switch (methodName) {
            case "flat_map" -> functionCall.arguments().size() == 2
                    ? Optional.of(linkBuiltinOptionFlatMapMethodInvoke(functionCall, scope))
                    : Optional.empty();
            case "reduce", "reduce_left" -> (functionCall.arguments().size() == 2 || functionCall.arguments().size() == 3)
                    ? Optional.of(linkBuiltinOptionReduceMethodInvoke(functionCall, scope))
                    : Optional.empty();
            default -> Optional.empty();
        };
    }

    private Optional<Result<CompiledExpression>> resolveBuiltinPipeAliasMethodInvoke(
            FunctionCall functionCall,
            Scope scope,
            String methodName
    ) {
        var arguments = functionCall.arguments();
        return switch (methodName) {
            case "map" -> arguments.size() == 2
                    ? Optional.of(linkBuiltinPipeAliasMethod(scope, arguments.get(0), InfixOperator.PIPE, arguments.get(1), functionCall.position()))
                    : Optional.empty();
            case "filter", "|-" -> arguments.size() == 2
                    ? Optional.of(linkBuiltinPipeAliasMethod(scope, arguments.get(0), InfixOperator.PIPE_MINUS, arguments.get(1), functionCall.position()))
                    : Optional.empty();
            case "flat_map" -> arguments.size() == 2
                    ? Optional.of(linkBuiltinPipeAliasMethod(scope, arguments.get(0), InfixOperator.PIPE_FLATMAP, arguments.get(1), functionCall.position()))
                    : Optional.empty();
            case "reduce" -> (arguments.size() == 2 || arguments.size() == 3)
                    ? Optional.of(linkBuiltinReduceMethodInvoke(functionCall, scope))
                    : Optional.empty();
            case "reduce_left", "|l>" -> (arguments.size() == 2 || arguments.size() == 3)
                    ? Optional.of(linkBuiltinReduceMethodInvoke(functionCall, scope))
                    : Optional.empty();
            default -> Optional.empty();
        };
    }

    private Result<CompiledExpression> linkBuiltinPipeAliasMethod(
            Scope scope,
            Expression source,
            InfixOperator operator,
            Expression mapper,
            Optional<SourcePosition> position
    ) {
        return linkInfixExpression(new InfixExpression(source, operator, mapper, position), scope);
    }

    private Result<CompiledExpression> linkBuiltinReduceMethodInvoke(FunctionCall functionCall, Scope scope) {
        var source = functionCall.arguments().getFirst();
        ReduceExpression reduceExpression;
        if (functionCall.arguments().size() == 2 && functionCall.arguments().get(1) instanceof ReduceExpression parsedReduceExpression) {
            reduceExpression = parsedReduceExpression;
        } else {
            var initialValue = functionCall.arguments().get(1);
            var reducer = functionCall.arguments().get(2);
            if (!(reducer instanceof LambdaExpression lambdaExpression)) {
                return withPosition(
                        Result.error("Reducer in `.reduce(...)` has to be a lambda expression"),
                        reducer.position()
                );
            }

            var reducerArguments = lambdaExpression.argumentNames();
            if (reducerArguments.size() != 2 && reducerArguments.size() != 3) {
                return withPosition(
                        Result.error("Reducer in `.reduce(...)` has to have two or three arguments"),
                        lambdaExpression.position()
                );
            }

            reduceExpression = new ReduceExpression(
                    initialValue,
                    reducerArguments.get(0),
                    reducerArguments.size() == 3 ? Optional.of(reducerArguments.get(1)) : Optional.empty(),
                    reducerArguments.get(reducerArguments.size() - 1),
                    lambdaExpression.expression(),
                    lambdaExpression.position()
            );
        }
        return linkInfixExpression(
                new InfixExpression(source, InfixOperator.PIPE_REDUCE, reduceExpression, functionCall.position()),
                scope
        );
    }

    private Result<CompiledExpression> linkBuiltinOptionFlatMapMethodInvoke(FunctionCall functionCall, Scope scope) {
        var position = functionCall.position();
        var source = functionCall.arguments().get(0);
        var mapper = functionCall.arguments().get(1);
        Expression someExpression;
        String valueName;
        if (mapper instanceof LambdaExpression lambdaExpression) {
            valueName = singleLambdaArgument(lambdaExpression).orElse(null);
            if (valueName == null) {
                return withPosition(
                        Result.error("Mapper in `.flat_map(...)` has to have exactly one argument"),
                        lambdaExpression.position()
                );
            }
            someExpression = lambdaExpression.expression();
        } else if (mapper instanceof FunctionReference functionReference) {
            valueName = "__option_value";
            someExpression = new FunctionInvoke(
                    functionReference,
                    List.of(new Value(valueName, functionReference.position())),
                    functionReference.position()
            );
        } else {
            return withPosition(
                    Result.error("Mapper in `.flat_map(...)` has to be a lambda expression or function reference"),
                    mapper.position()
            );
        }
        var matchExpression = new MatchExpression(
                source,
                List.of(
                        new MatchExpression.MatchCase(
                                new MatchExpression.ConstructorPattern("Some", List.of(new MatchExpression.VariablePattern(valueName))),
                                Optional.empty(),
                                someExpression
                        ),
                        new MatchExpression.MatchCase(
                                new MatchExpression.VariablePattern("None"),
                                Optional.empty(),
                                noneExpression(position)
                        )
                ),
                position
        );
        return linkExpression(matchExpression, scope);
    }

    private Result<CompiledExpression> linkBuiltinOptionReduceMethodInvoke(FunctionCall functionCall, Scope scope) {
        var position = functionCall.position();
        var source = functionCall.arguments().get(0);
        ReduceExpression reduceExpression;
        if (functionCall.arguments().size() == 2 && functionCall.arguments().get(1) instanceof ReduceExpression parsedReduceExpression) {
            reduceExpression = parsedReduceExpression;
        } else {
            var initial = functionCall.arguments().get(1);
            var reducer = functionCall.arguments().get(2);
            if (!(reducer instanceof LambdaExpression lambdaExpression)) {
                return withPosition(
                        Result.error("Reducer in `.reduce(...)` has to be a lambda expression"),
                        reducer.position()
                );
            }
            if (lambdaExpression.argumentNames().size() != 2) {
                return withPosition(
                        Result.error("Reducer in `.reduce(...)` has to have exactly two arguments"),
                        lambdaExpression.position()
                );
            }
            reduceExpression = new ReduceExpression(
                    initial,
                    lambdaExpression.argumentNames().getFirst(),
                    Optional.empty(),
                    lambdaExpression.argumentNames().get(1),
                    lambdaExpression.expression(),
                    lambdaExpression.position()
            );
        }
        var matchExpression = new MatchExpression(
                source,
                List.of(
                        new MatchExpression.MatchCase(
                                new MatchExpression.ConstructorPattern("Some", List.of(new MatchExpression.VariablePattern(reduceExpression.valueName()))),
                                Optional.empty(),
                                new LetExpression(
                                        reduceExpression.accumulatorName(),
                                        Optional.empty(),
                                        LetExpression.Kind.ASSIGN,
                                        reduceExpression.initialValue(),
                                        reduceExpression.reducerExpression(),
                                        reduceExpression.position()
                                )
                        ),
                        new MatchExpression.MatchCase(
                                new MatchExpression.VariablePattern("None"),
                                Optional.empty(),
                                reduceExpression.initialValue()
                        )
                ),
                position
        );
        return linkExpression(matchExpression, scope);
    }

    private NewData noneExpression(Optional<SourcePosition> position) {
        return new NewData(new DataType("None"), List.of(), List.of(), List.of(), position);
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
        if (resolveSpecializedResultType(expected).isPresent()) {
            return linkArgumentForExpectedResultType(argument, scope, expected);
        }
        if (argument instanceof IfExpression ifExpression) {
            return linkIfExpression(ifExpression, scope, expected)
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
        if (argument instanceof MatchExpression matchExpression) {
            return linkMatchExpression(matchExpression, scope, expected)
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
        if (argument instanceof LetExpression letExpression) {
            return linkLetExpression(letExpression, scope, Optional.of(expected))
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
        if (argument instanceof FunctionCall functionCall) {
            return linkFunctionCall(functionCall, scope, Optional.of(expected))
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
        if (argument instanceof InfixExpression infixExpression
            && infixExpression.operator() == InfixOperator.PIPE
            && (expected instanceof CompiledList || expected instanceof CompiledSet || expected instanceof CompiledDict)) {
            return linkPipeExpression(infixExpression, scope, expected)
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

    private Result<CoercedArgument> linkArgumentForExpectedResultType(
            Expression argument,
            Scope scope,
            CompiledType expected
    ) {
        var expectedResultParent = resolveSpecializedResultType(expected);
        if (expectedResultParent.isEmpty()) {
            return withPosition(Result.error("Expected `Result[...]`, got `" + expected + "`"), argument.position());
        }
        var linkedArgument = switch (argument) {
            case IfExpression ifExpression -> linkIfExpression(ifExpression, scope, expected);
            case MatchExpression matchExpression -> linkMatchExpression(matchExpression, scope, expected);
            case LetExpression letExpression -> linkLetExpression(letExpression, scope, Optional.of(expected));
            default -> linkExpression(argument, scope);
        };
        return linkedArgument.flatMap(linked -> {
            var maybeCoerced = coerceArgument(linked, expected);
            if (maybeCoerced != null) {
                return Result.success(maybeCoerced);
            }
            var wrapped = ensureResultExpression(linked, expectedResultParent.orElseThrow(), argument.position());
            if (wrapped instanceof Result.Error<CompiledExpression> error) {
                return new Result.Error<>(error.errors());
            }
            var resultCompatibleExpression = ((Result.Success<CompiledExpression>) wrapped).value();
            maybeCoerced = coerceArgument(resultCompatibleExpression, expected);
            if (maybeCoerced == null) {
                return withPosition(
                        Result.error("Expected `" + expected + "`, got `" + linked.type() + "`"),
                        argument.position()
                );
            }
            return Result.success(maybeCoerced);
        });
    }

    private Result<CompiledExpression> linkFunctionReference(FunctionReference functionReference, CompiledFunctionType expectedType) {
        var expectedShape = flattenFunctionType(expectedType);
        var candidates = functionsByNameAndArity(functionSignatures, functionReference.name(), expectedShape.parameterTypes().size());
        if (candidates.isEmpty()) {
            return withPosition(
                    Result.error("Function `" + functionReference.name() + "` with "
                                       + expectedShape.parameterTypes().size() + " argument(s) not found"),
                    functionReference.position()
            );
        }

        ResolvedFunctionReference best = null;
        FunctionSignature bestCandidate = null;
        for (var candidate : candidates) {
            var resolved = resolveFunctionReferenceCandidate(candidate, expectedShape);
            if (resolved == null) {
                continue;
            }
            if (best == null
                || resolved.coercions() < best.coercions()
                || (resolved.coercions() == best.coercions() && bestCandidate != null && isSignatureMoreSpecific(candidate, bestCandidate))) {
                best = resolved;
                bestCandidate = candidate;
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
        var candidates = functionsByName(functionSignatures, functionReference.name());
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
        if (expected instanceof CompiledList expectedList
            && argument.type() instanceof CompiledList argumentList
            && argumentList.elementType() != ANY
            && (isTypeCompatible(argumentList.elementType(), expectedList.elementType())
                || canCoerceToExpectedType(argumentList.elementType(), expectedList.elementType()))) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledSet expectedSet
            && argument.type() instanceof CompiledSet argumentSet
            && argumentSet.elementType() != ANY
            && (isTypeCompatible(argumentSet.elementType(), expectedSet.elementType())
                || canCoerceToExpectedType(argumentSet.elementType(), expectedSet.elementType()))) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof CompiledDict expectedDict
            && argument.type() instanceof CompiledDict argumentDict
            && argumentDict.valueType() != ANY
            && (isTypeCompatible(argumentDict.valueType(), expectedDict.valueType())
                || canCoerceToExpectedType(argumentDict.valueType(), expectedDict.valueType()))) {
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
            && isSubtypeOfParent(argumentDataType, expectedParentType)) {
            var specializedParent = argumentDataType.extendedTypes().stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .flatMap(Optional::stream)
                    .filter(GenericDataType.class::isInstance)
                    .map(GenericDataType.class::cast)
                    .filter(parentType -> sameRawTypeName(parentType.name(), expectedParentType.name()))
                    .findFirst()
                    .or(() -> specializeParentFromSubtypeFields(argumentDataType, expectedParentType))
                    .or(() -> instantiateExtendedParentFromSubtypeFields(argumentDataType, expectedParentType));
            if (specializedParent.isPresent() && isTypeCompatible(specializedParent.orElseThrow(), expectedParentType)) {
                return new CoercedArgument(argument, 1);
            }
            if (argumentDataType.typeParameters().isEmpty()
                || areTypeParameterDescriptorsCompatible(argumentDataType.typeParameters(), expectedParentType.typeParameters())) {
                return new CoercedArgument(argument, 1);
            }
        }
        return null;
    }

    private boolean canCoerceToExpectedType(CompiledType actualType, CompiledType expectedType) {
        return coerceArgument(new CompiledVariable("__expected__", actualType), expectedType) != null;
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
        if (!(actual instanceof GenericDataType) && !(expected instanceof GenericDataType) && actual.equals(expected)) {
            return true;
        }
        if (expected instanceof CompiledList expectedList && actual instanceof CompiledList actualList) {
            if (actualList.elementType() == ANY && expectedList.elementType() != ANY) {
                return false;
            }
            return isTypeCompatible(actualList.elementType(), expectedList.elementType());
        }
        if (expected instanceof CompiledSet expectedSet && actual instanceof CompiledSet actualSet) {
            if (actualSet.elementType() == ANY && expectedSet.elementType() != ANY) {
                return false;
            }
            return isTypeCompatible(actualSet.elementType(), expectedSet.elementType());
        }
        if (expected instanceof CompiledDict expectedDict && actual instanceof CompiledDict actualDict) {
            if (actualDict.valueType() == ANY && expectedDict.valueType() != ANY) {
                return false;
            }
            return isTypeCompatible(actualDict.valueType(), expectedDict.valueType());
        }
        if (expected == ANY || actual == ANY || actual == NOTHING) {
            return true;
        }
        if (expected instanceof CompiledGenericTypeParameter || actual instanceof CompiledGenericTypeParameter) {
            return true;
        }
        if (expected instanceof CompiledTupleType expectedTuple && actual instanceof CompiledTupleType actualTuple) {
            return areTupleTypesCompatible(actualTuple, expectedTuple);
        }
        if (expected instanceof CompiledFunctionType expectedFunction && actual instanceof CompiledFunctionType actualFunction) {
            return areFunctionTypesCompatible(actualFunction, expectedFunction);
        }
        if (expected instanceof GenericDataType expectedData && actual instanceof GenericDataType actualData) {
            if (sameRawTypeName(expectedData.name(), actualData.name())) {
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
            if (expectedData instanceof CompiledDataParentType expectedParent && actualData instanceof CompiledDataType actualSubtype) {
                if (!isSubtypeOfParent(actualSubtype, expectedParent)) {
                    return false;
                }
                var specializedParent = actualSubtype.extendedTypes().stream()
                        .map(this::parseLinkedTypeDescriptor)
                        .flatMap(Optional::stream)
                        .filter(GenericDataType.class::isInstance)
                        .map(GenericDataType.class::cast)
                        .filter(parentType -> sameRawTypeName(parentType.name(), expectedParent.name()))
                        .findFirst()
                        .or(() -> specializeParentFromSubtypeFields(actualSubtype, expectedParent))
                        .or(() -> instantiateExtendedParentFromSubtypeFields(actualSubtype, expectedParent));
                if (specializedParent.isPresent()) {
                    return isTypeCompatible(specializedParent.orElseThrow(), expectedParent);
                }
                return !containsGenericTypeParameter(actualSubtype);
            }
            return false;
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
                .replace(".capy.lang.Option", ".cap.lang.Option")
                .replace("/capy/lang/Result", "/cap/lang/Result")
                .replace(".capy.lang.Result", ".cap.lang.Result");
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

    private Result<CompiledExpression> linkIfExpression(IfExpression ifExpression, Scope scope, CompiledType expectedType) {
        return linkExpression(ifExpression.condition(), scope)
                .flatMap(c -> {
                    if (!isBooleanConvertibleType(c.type())) {
                        return withPosition(
                                Result.error("condition in if statement has to have type `" + BOOL + "`, was `" + c.type() + "`"),
                                ifExpression.condition().position()
                        );
                    }
                    return linkArgumentForExpectedType(ifExpression.thenBranch(), scope, expectedType)
                            .flatMap(t ->
                                    linkArgumentForExpectedType(ifExpression.elseBranch(), scope, expectedType)
                                            .map(e -> new CompiledIfExpression(
                                                    c,
                                                    t.expression(),
                                                    e.expression(),
                                                    mergeBranchTypes(t.expression().type(), e.expression().type())
                                            )));
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
            case WithExpression withExpression ->
                    containsValueReference(withExpression.source(), variableName)
                    || withExpression.assignments().stream()
                    .anyMatch(assignment -> containsValueReference(assignment.value(), variableName));
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
        var candidates = methodsByNameAndArity(functionSignatures, operatorSymbol, 2).stream()
                .filter(signature -> matchesMethodOwner(signature.name(), ownerNames, operatorSymbol))
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
            var returnType = resolveMethodReturnType(operatorSymbol, candidate, arguments);
            if (best == null
                || coercions < best.coercions()
                || (coercions == best.coercions() && isSignatureMoreSpecific(candidate, best.signature()))) {
                best = new ResolvedFunctionCall(candidate, arguments, coercions, returnType);
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
        return Result.success(new CompiledFunctionCall(
                best.signature().name(),
                best.arguments(),
                best.returnType()
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
        var candidates = methodsByNameAndArity(functionSignatures, operatorSymbol, 2).stream()
                .filter(signature -> matchesMethodOwner(signature.name(), ownerNames, operatorSymbol))
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
            var returnType = resolveMethodReturnType(operatorSymbol, candidate, arguments);
            if (best == null
                || coercions < best.coercions()
                || (coercions == best.coercions() && isSignatureMoreSpecific(candidate, best.signature()))) {
                best = new ResolvedFunctionCall(candidate, arguments, coercions, returnType);
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
        return Result.success(new CompiledFunctionCall(
                best.signature().name(),
                best.arguments(),
                best.returnType()
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
        if (signatureReturnType instanceof CompiledDataParentType signatureParentType
            && lambdaReturnType instanceof CompiledDataType lambdaReturnData) {
            if (signatureParentType.typeParameters().size() == 1
                && lambdaExpression.expression() instanceof CompiledNewData lambdaNewData) {
                var valueAssignment = lambdaNewData.assignments().stream()
                        .filter(assignment -> "value".equals(assignment.name()))
                        .findFirst();
                if (valueAssignment.isPresent()) {
                    var specializedParent = substituteTypeParameters(
                            signatureParentType,
                            java.util.Map.of(signatureParentType.typeParameters().getFirst(), valueAssignment.orElseThrow().value().type())
                    );
                    if (!specializedParent.equals(signatureParentType)) {
                        return specializedParent;
                    }
                }
            }
            var specializedParent = specializeParentFromSubtypeFields(lambdaReturnData, signatureParentType)
                    .or(() -> instantiateExtendedParentFromSubtypeFields(lambdaReturnData, signatureParentType));
            if (specializedParent.isPresent()) {
                return specializedParent.orElseThrow();
            }
            if (isSubtypeOfParent(lambdaReturnData, signatureParentType)
                && isResolvedTypeForInference(lambdaReturnType)) {
                var substitutions = new java.util.LinkedHashMap<String, CompiledType>();
                collectTypeSubstitutions(signatureReturnType, lambdaReturnType, substitutions);
                var specialized = substituteTypeParameters(signatureReturnType, substitutions);
                if (!specialized.equals(signatureReturnType)) {
                    return specialized;
                }
            }
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

    private boolean isBetterResolvedCall(FunctionSignature candidate, int coercions, ResolvedFunctionCall currentBest) {
        if (currentBest == null) {
            return true;
        }
        if (coercions < currentBest.coercions()) {
            return true;
        }
        return coercions == currentBest.coercions()
               && isSignatureMoreSpecific(candidate, currentBest.signature());
    }

    private boolean isSignatureMoreSpecific(FunctionSignature candidate, FunctionSignature currentBest) {
        if (candidate.parameterTypes().size() != currentBest.parameterTypes().size()) {
            return false;
        }
        var strictlyMoreSpecific = false;
        for (var i = 0; i < candidate.parameterTypes().size(); i++) {
            var candidateParam = candidate.parameterTypes().get(i);
            var currentBestParam = currentBest.parameterTypes().get(i);
            if (candidateParam.equals(currentBestParam)) {
                continue;
            }
            if (coerceArgument(new CompiledVariable("__specificity__", candidateParam), currentBestParam) == null) {
                return false;
            }
            var candidateIsBroad = isBroadSpecificityType(candidateParam);
            var currentBestIsBroad = isBroadSpecificityType(currentBestParam);
            if (candidateIsBroad && !currentBestIsBroad) {
                return false;
            }
            if (!candidateIsBroad && currentBestIsBroad) {
                strictlyMoreSpecific = true;
                continue;
            }
            if (!candidateIsBroad && !currentBestIsBroad) {
                strictlyMoreSpecific = true;
            }
        }
        return strictlyMoreSpecific;
    }

    private boolean isBroadSpecificityType(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive == ANY || primitive == DATA;
            case CompiledGenericTypeParameter ignored -> true;
            case CompiledDataType ignored -> false;
            case CompiledDataParentType ignored -> false;
            case CompiledList ignored -> false;
            case CompiledSet ignored -> false;
            case CompiledDict ignored -> false;
            case CompiledTupleType ignored -> false;
            case CompiledFunctionType ignored -> false;
        };
    }

    private Set<String> methodOwnerCandidates(CompiledType receiverType) {
        var ownerNames = new LinkedHashSet<String>();
        var receiverBase = baseTypeName(receiverType.name());
        var receiverSimple = simpleTypeName(receiverBase);
        ownerNames.add(receiverBase);
        ownerNames.addAll(linkCache.methodOwnerCandidatesBySimpleType.getOrDefault(receiverSimple, Set.of()));
        if (receiverType instanceof CompiledDataType) {
            ownerNames.addAll(linkCache.subtypeParentOwnerCandidatesBySimpleType.getOrDefault(receiverSimple, Set.of()));
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
                    var specializedOptionType = resolveSpecializedOptionType(left.type()).orElse(null);
                    if (specializedOptionType != null) {
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

    private Result<CompiledExpression> linkPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledType expectedType
    ) {
        var reAssociated = reAssociatePipeChain(expression);
        if (reAssociated != expression) {
            return linkArgumentForExpectedType(reAssociated, scope, expectedType)
                    .map(CoercedArgument::expression);
        }
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var specializedOptionType = resolveSpecializedOptionType(left.type()).orElse(null);
                    if (specializedOptionType != null) {
                        return linkOptionPipeExpression(expression, scope, left, optionElementType(left));
                    }
                    var sourceElementType = switch (left.type()) {
                        case CompiledList linkedList -> linkedList.elementType();
                        case CompiledSet linkedSet -> linkedSet.elementType();
                        case CompiledDict linkedDict -> linkedDict.valueType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (sourceElementType == null) {
                        return linkScalarPipeExpression(expression, scope, left);
                    }
                    var expectedElementType = expectedPipeElementType(left.type(), expectedType);
                    return linkCollectionPipeExpression(expression, scope, left, sourceElementType, expectedElementType);
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
        return linkCollectionPipeExpression(expression, scope, left, elementType, Optional.empty());
    }

    private Result<CompiledExpression> linkCollectionPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledType elementType,
            Optional<CompiledType> expectedResultElementType
    ) {
        if (left.type() instanceof CompiledDict dictType) {
            return linkDictPipeExpression(expression, scope, left, dictType, expectedResultElementType);
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
                .flatMap(lambdaBinding -> expectedResultElementType
                        .<Result<CompiledExpression>>map(type -> linkArgumentForExpectedType(lambdaExpression.expression(), lambdaBinding.scope(), type)
                                .map(CoercedArgument::expression))
                        .orElseGet(() -> linkExpression(lambdaExpression.expression(), lambdaBinding.scope()))
                .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                        left,
                        lambdaBinding.argumentName(),
                        mapper,
                        left.type() instanceof CompiledSet
                                ? new CompiledSet(expectedResultElementType.orElse(mapper.type()))
                                : new CompiledList(expectedResultElementType.orElse(mapper.type()))
                )));
    }

    private Result<CompiledExpression> linkDictPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledDict dictType
    ) {
        return linkDictPipeExpression(expression, scope, left, dictType, Optional.empty());
    }

    private Result<CompiledExpression> linkDictPipeExpression(
            InfixExpression expression,
            Scope scope,
            CompiledExpression left,
            CompiledDict dictType,
            Optional<CompiledType> expectedValueType
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
            return expectedValueType
                    .<Result<CompiledExpression>>map(type -> linkArgumentForExpectedType(lambdaExpression.expression(), lambdaScope, type)
                            .map(CoercedArgument::expression))
                    .orElseGet(() -> linkExpression(lambdaExpression.expression(), lambdaScope))
                    .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                            left,
                            valueName,
                            mapper,
                            new CompiledDict(expectedValueType.orElse(mapper.type()))
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
            return expectedValueType
                    .<Result<CompiledExpression>>map(type -> linkArgumentForExpectedType(lambdaExpression.expression(), lambdaScope, type)
                            .map(CoercedArgument::expression))
                    .orElseGet(() -> linkExpression(lambdaExpression.expression(), lambdaScope))
                    .map(mapper -> (CompiledExpression) new CompiledPipeExpression(
                            left,
                            encodeDictPipeArguments(keyName, valueName),
                            mapper,
                            new CompiledDict(expectedValueType.orElse(mapper.type()))
                    ));
        }
        return withPosition(
                Result.error("Right side lambda of `|` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private Optional<CompiledType> expectedPipeElementType(CompiledType sourceType, CompiledType expectedType) {
        if (sourceType instanceof CompiledList && expectedType instanceof CompiledList expectedList) {
            return Optional.of(expectedList.elementType());
        }
        if (sourceType instanceof CompiledSet && expectedType instanceof CompiledSet expectedSet) {
            return Optional.of(expectedSet.elementType());
        }
        if (sourceType instanceof CompiledDict && expectedType instanceof CompiledDict expectedDict) {
            return Optional.of(expectedDict.valueType());
        }
        return Optional.empty();
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
                                Optional.empty(),
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
                        Optional.empty(),
                        mapper
                ));
    }

    private Result<PipeMapper> resolvePipeFunctionReference(FunctionReference functionReference, CompiledType inputType) {
        var argumentName = "it";
        var argument = new CompiledVariable(argumentName, inputType);
        var candidates = functionsByNameAndArity(functionSignatures, functionReference.name(), 1);
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
                    coerced.coercions(),
                    resolveReturnType(candidate, List.of(coerced.expression()))
            );
            if (isBetterResolvedCall(candidate, resolved.coercions(), best)) {
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
                    var specializedOptionType = resolveSpecializedOptionType(left.type()).orElse(null);
                    if (specializedOptionType != null) {
                        return linkOptionPipeFilterOutExpression(expression, scope, left, optionElementType(left), specializedOptionType);
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
                            var optionType = resolveSpecializedOptionType(left.type()).orElseGet(() -> optionTypeFor(left.type()));
                            if (optionType == null) {
                                return withPosition(
                                        Result.error("`|-` on data/type requires `Option` type to be available"),
                                        expression.left().position()
                                );
                            }
                            return linkOptionPipeFilterOutExpression(expression, scope, left, optionElementType(left), optionType);
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
                                                left.type() == STRING
                                                        ? STRING
                                                        : left.type() instanceof CompiledSet
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
                                        left.type() == STRING
                                                ? STRING
                                                : left.type() instanceof CompiledSet
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

    private Result<CompiledInfixExpression> getLinkedInfixExpression(
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

    private CompiledType findPlusType(CompiledType left, CompiledType right) {
        if (left instanceof CompiledList leftList) {
            if (right instanceof CompiledList rightList) {
                return new CompiledList(mergeCollectionElementType(leftList.elementType(), rightList.elementType()));
            }
            return new CompiledList(mergeCollectionElementType(leftList.elementType(), right));
        }
        if (left instanceof CompiledSet leftSet) {
            if (right instanceof CompiledSet rightSet) {
                return new CompiledSet(mergeCollectionElementType(leftSet.elementType(), rightSet.elementType()));
            }
            return new CompiledSet(mergeCollectionElementType(leftSet.elementType(), right));
        }
        if (left instanceof CompiledDict leftDict) {
            if (right instanceof CompiledDict rightDict) {
                return new CompiledDict(mergeCollectionElementType(leftDict.valueType(), rightDict.valueType()));
            }
            if (right instanceof CompiledTupleType tupleType
                && tupleType.elementTypes().size() == 2
                && tupleType.elementTypes().getFirst() == STRING) {
                return new CompiledDict(mergeCollectionElementType(leftDict.valueType(), tupleType.elementTypes().get(1)));
            }
            return null;
        }
        if (left == STRING && right != NOTHING) {
            return STRING;
        }
        if (right == STRING && left != NOTHING) {
            return STRING;
        }
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findPlusPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return null;
    }

    private CompiledType findMinusType(CompiledType left, CompiledType right) {
        if (left instanceof CompiledList leftList) {
            if (right instanceof CompiledList rightList) {
                return new CompiledList(mergeCollectionElementType(leftList.elementType(), rightList.elementType()));
            }
            return new CompiledList(mergeCollectionElementType(leftList.elementType(), right));
        }
        if (left instanceof CompiledSet leftSet) {
            if (right instanceof CompiledSet rightSet) {
                return new CompiledSet(mergeCollectionElementType(leftSet.elementType(), rightSet.elementType()));
            }
            return new CompiledSet(mergeCollectionElementType(leftSet.elementType(), right));
        }
        if (left instanceof CompiledDict leftDict) {
            if (right instanceof CompiledDict rightDict) {
                return new CompiledDict(mergeCollectionElementType(leftDict.valueType(), rightDict.valueType()));
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
        return null;
    }

    private CompiledType mergeCollectionElementType(CompiledType left, CompiledType right) {
        if (left == ANY) {
            return right;
        }
        if (right == ANY) {
            return left;
        }
        var sharedDataParent = findSharedCollectionParentType(left, right);
        if (sharedDataParent != null) {
            return sharedDataParent;
        }
        return findHigherType(left, right);
    }

    private CompiledType findSharedCollectionParentType(CompiledType left, CompiledType right) {
        if (left instanceof CompiledDataParentType leftParent
            && right instanceof CompiledDataType rightData
            && isSubtypeOfParent(rightData, leftParent)) {
            return leftParent;
        }
        if (right instanceof CompiledDataParentType rightParent
            && left instanceof CompiledDataType leftData
            && isSubtypeOfParent(leftData, rightParent)) {
            return rightParent;
        }
        if (left instanceof CompiledDataParentType leftParent
            && right instanceof CompiledDataParentType rightParent
            && sameRawTypeName(leftParent.name(), rightParent.name())
            && areTypeParameterDescriptorsCompatible(rightParent.typeParameters(), leftParent.typeParameters())) {
            return leftParent;
        }
        if (!(left instanceof CompiledDataType leftData) || !(right instanceof CompiledDataType rightData)) {
            return null;
        }
        for (var leftParentDescriptor : leftData.extendedTypes()) {
            var leftParentName = rawTypeName(leftParentDescriptor);
            for (var rightParentDescriptor : rightData.extendedTypes()) {
                var rightParentName = rawTypeName(rightParentDescriptor);
                if (!sameRawTypeName(leftParentName, rightParentName)) {
                    continue;
                }
                var shared = resolveDataTypeByName(leftParentName);
                if (shared != null) {
                    return shared;
                }
            }
        }
        for (var type : dataTypes.values()) {
            if (!(type instanceof CompiledDataParentType parentType)) {
                continue;
            }
            var hasLeft = parentType.subTypes().stream()
                    .anyMatch(subType -> sameRawTypeName(subType.name(), leftData.name()));
            if (!hasLeft) {
                continue;
            }
            var hasRight = parentType.subTypes().stream()
                    .anyMatch(subType -> sameRawTypeName(subType.name(), rightData.name()));
            if (hasRight) {
                return parentType;
            }
        }
        return null;
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
        return linkCache.optionType;
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
        return linkCache.resultType;
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

    private boolean isResultType(CompiledType type) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return false;
        }
        var normalized = normalizeTypeName(genericDataType.name());
        return "Result".equals(genericDataType.name())
               || normalized.endsWith("/Result.Result")
               || normalized.endsWith("/Result");
    }

    private Optional<CompiledDataParentType> resolveSpecializedResultType(CompiledType type) {
        if (type instanceof CompiledDataParentType resultParent && isResultType(resultParent)) {
            return Optional.of(resultParent);
        }
        if (!(type instanceof CompiledDataType resultSubtype)) {
            return Optional.empty();
        }
        var resultType = findResultType();
        if (resultType == null || !isSubtypeOfParent(resultSubtype, resultType)) {
            return Optional.empty();
        }
        return resultSubtype.extendedTypes().stream()
                .map(this::parseLinkedTypeDescriptor)
                .flatMap(Optional::stream)
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parentType -> sameRawTypeName(parentType.name(), resultType.name()))
                .findFirst()
                .or(() -> specializeParentFromSubtypeFields(resultSubtype, resultType)
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast))
                .or(() -> instantiateExtendedParentFromSubtypeFields(resultSubtype, resultType)
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast));
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

    private Optional<CompiledDataParentType> resolveSpecializedOptionType(CompiledType type) {
        if (type instanceof CompiledDataParentType optionParent && isOptionType(optionParent)) {
            return Optional.of(optionParent);
        }
        if (!(type instanceof CompiledDataType optionSubtype)) {
            return Optional.empty();
        }
        var optionType = findOptionType();
        if (optionType == null || !isSubtypeOfParent(optionSubtype, optionType)) {
            return Optional.empty();
        }
        return optionSubtype.extendedTypes().stream()
                .map(this::parseLinkedTypeDescriptor)
                .flatMap(Optional::stream)
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parentType -> sameRawTypeName(parentType.name(), optionType.name()))
                .findFirst()
                .or(() -> specializeParentFromSubtypeFields(optionSubtype, optionType)
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast))
                .or(() -> instantiateExtendedParentFromSubtypeFields(optionSubtype, optionType)
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast));
    }

    private CompiledType optionElementType(CompiledExpression expression) {
        if (expression instanceof CompiledPipeExpression pipeExpression
            && resolveSpecializedOptionType(pipeExpression.type()).isPresent()) {
            return pipeExpression.mapper().type();
        }
        if (expression instanceof CompiledPipeFilterOutExpression filterOutExpression
            && resolveSpecializedOptionType(filterOutExpression.type()).isPresent()) {
            return optionElementType(filterOutExpression.source());
        }
        var optionType = resolveSpecializedOptionType(expression.type()).orElse(null);
        if (optionType != null && !optionType.typeParameters().isEmpty()) {
            return parseLinkedTypeDescriptor(optionType.typeParameters().getFirst()).orElse(ANY);
        }
        if (optionType != null) {
            return ANY;
        }
        return expression.type();
    }
    private boolean isOptionTypeKey(String key) {
        return isOptionTypeKeyStatic(key);
    }

    private boolean isResultTypeKey(String key) {
        return isResultTypeKeyStatic(key);
    }

    private String normalizeTypeName(String name) {
        return normalizeTypeNameStatic(name);
    }

    private static boolean isOptionTypeKeyStatic(String key) {
        var normalized = normalizeTypeNameStatic(key);
        return normalized.endsWith("/capy/lang/Option.Option")
               || normalized.endsWith("/cap/lang/Option.Option")
               || normalized.endsWith("/Option.Option");
    }

    private static boolean isResultTypeKeyStatic(String key) {
        var normalized = normalizeTypeNameStatic(key);
        return normalized.endsWith("/capy/lang/Result.Result")
               || normalized.endsWith("/cap/lang/Result.Result")
               || normalized.endsWith("/Result.Result");
    }

    private static String normalizeTypeNameStatic(String name) {
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
                        .map(this::orderMatchCases)
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

    private Result<CompiledExpression> linkMatchExpression(
            MatchExpression matchExpression,
            Scope scope,
            CompiledType expectedType
    ) {
        return linkExpression(matchExpression.matchWith(), scope)
                .flatMap(matchWith -> matchExpression.cases().stream()
                        .map(matchCase -> linkMatchCase(matchCase, matchWith, scope, Optional.of(expectedType)))
                        .collect(new ResultCollectionCollector<>())
                        .map(this::orderMatchCases)
                        .flatMap(cases -> validateMatchExhaustiveness(matchExpression, matchWith.type(), cases)
                                .map(ignored -> (CompiledExpression) new CompiledMatchExpression(matchWith, cases, expectedType))));
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
        if (matchType == PrimitiveLinkedType.BOOL) {
            return validateBoolMatchExhaustiveness(matchExpression, cases);
        }
        var requiredConstructors = requiredConstructorsForMatch(matchType);
        if (requiredConstructors.isEmpty()) {
            return withPosition(
                    Result.error("`match` is not exhaustive. Use wildcard `case _ -> ...`."),
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
                Result.error("`match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:" + missingText + "."),
                matchExpression.position()
        );
    }

    private Result<Void> validateBoolMatchExhaustiveness(
            MatchExpression matchExpression,
            List<CompiledMatchExpression.MatchCase> cases
    ) {
        var coveredBooleanValues = new java.util.LinkedHashSet<String>();
        for (var matchCase : cases) {
            if (matchCase.guard().isPresent()) {
                continue;
            }
            var pattern = matchCase.pattern();
            if (pattern instanceof CompiledMatchExpression.VariablePattern) {
                return Result.success(null);
            }
            if (pattern instanceof CompiledMatchExpression.TypedPattern typedPattern
                && typedPattern.type() == PrimitiveLinkedType.BOOL) {
                return Result.success(null);
            }
            if (pattern instanceof CompiledMatchExpression.BoolPattern boolPattern) {
                coveredBooleanValues.add(boolPattern.value().toLowerCase(java.util.Locale.ROOT));
            }
        }
        if (coveredBooleanValues.size() == 2) {
            return Result.success(null);
        }
        var missing = new java.util.ArrayList<String>();
        if (!coveredBooleanValues.contains("true")) {
            missing.add("`true`");
        }
        if (!coveredBooleanValues.contains("false")) {
            missing.add("`false`");
        }
        var missingText = String.join(", ", missing);
        return withPosition(
                Result.error("`match` is not exhaustive. Use wildcard `case _ -> ...` or add missing branches:" + missingText + "."),
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
            if (matchCase.guard().isPresent()) {
                continue;
            }
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
        return linkMatchCase(matchCase, matchWith, scope, Optional.empty());
    }

    private Result<CompiledMatchExpression.MatchCase> linkMatchCase(
            MatchExpression.MatchCase matchCase,
            CompiledExpression matchWith,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        return linkPattern(matchCase.pattern(), matchWith.type(), scope)
                .flatMap(patternAndScope -> {
                    var branchScope = patternAndScope.scope();
                    if (patternAndScope.pattern() instanceof CompiledMatchExpression.TypedPattern typedPattern
                        && matchWith instanceof CompiledVariable matchedVariable) {
                        // Flow typing: inside a typed match branch, treat the matched variable as narrowed too.
                        branchScope = branchScope.add(matchedVariable.name(), typedPattern.type());
                    }
                    var caseScope = branchScope;
                    return linkGuard(matchCase.guard(), caseScope)
                            .flatMap(guard -> expectedType
                                    .<Result<CompiledExpression>>map(type -> linkArgumentForExpectedType(matchCase.expression(), caseScope, type)
                                            .map(CoercedArgument::expression))
                                    .orElseGet(() -> linkExpression(matchCase.expression(), caseScope))
                                    .map(expression -> new CompiledMatchExpression.MatchCase(patternAndScope.pattern(), guard, expression)));
                });
    }

    private Result<Optional<CompiledExpression>> linkGuard(Optional<Expression> guard, Scope scope) {
        if (guard.isEmpty()) {
            return Result.success(Optional.empty());
        }
        return linkExpression(guard.get(), scope)
                .flatMap(linkedGuard -> {
                    if (linkedGuard.type() != PrimitiveLinkedType.BOOL) {
                        return withPosition(
                                Result.error("`when` guard has to be `bool`, was `" + linkedGuard.type() + "`"),
                                guard.get().position()
                        );
                    }
                    return Result.success(Optional.of(linkedGuard));
                });
    }

    private List<CompiledMatchExpression.MatchCase> orderMatchCases(List<CompiledMatchExpression.MatchCase> cases) {
        var guarded = new ArrayList<CompiledMatchExpression.MatchCase>();
        var unguarded = new ArrayList<CompiledMatchExpression.MatchCase>();
        for (var matchCase : cases) {
            if (matchCase.guard().isPresent()) {
                guarded.add(matchCase);
            } else {
                unguarded.add(matchCase);
            }
        }
        guarded.addAll(unguarded);
        return List.copyOf(guarded);
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
            case MatchExpression.LongPattern longPattern -> validateLiteralPattern(longPattern, matchType, scope, PrimitiveLinkedType.LONG);
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
            case MatchExpression.LongPattern longPattern ->
                    Result.success(new PatternAndScope(new CompiledMatchExpression.LongPattern(longPattern.value()), scope));
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
                    var effectivePatternType = patternType;
                    if (patternType instanceof CompiledDataType patternDataType
                        && matchType instanceof CompiledDataParentType parentType) {
                        var subtype = findSubtype(patternDataType.name(), parentType);
                        if (subtype instanceof Result.Success<CompiledDataType> value) {
                            effectivePatternType = value.value();
                        }
                    }
                    return Result.success(new PatternAndScope(
                            new CompiledMatchExpression.TypedPattern(effectivePatternType, typedPattern.name()),
                            scope.add(typedPattern.name(), effectivePatternType)
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
                    linkedDataType.extendedTypes().stream()
                            .map(typeDescriptor -> substituteTypeDescriptor(typeDescriptor, substitutions))
                            .toList(),
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
        var cached = linkCache.parsedTypeDescriptorCache.get(normalizedDescriptor);
        if (cached != null) {
            return cached;
        }
        var parsed = parseLinkedTypeDescriptorUncached(normalizedDescriptor);
        linkCache.parsedTypeDescriptorCache.put(normalizedDescriptor, parsed);
        return parsed;
    }

    private Optional<CompiledType> parseLinkedTypeDescriptorUncached(String normalizedDescriptor) {
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
        var cached = linkCache.resolvedDataTypesByNameCache.get(typeName);
        if (cached != null || linkCache.resolvedDataTypesByNameCache.containsKey(typeName)) {
            return cached;
        }
        var direct = dataTypes.get(typeName);
        if (direct != null) {
            linkCache.resolvedDataTypesByNameCache.put(typeName, direct);
            return direct;
        }
        var resolved = linkCache.fallbackDataTypesBySuffix.get(typeName);
        linkCache.resolvedDataTypesByNameCache.put(typeName, resolved);
        return resolved;
    }

    private static Map<String, GenericDataType> buildFallbackDataTypesBySuffix(Map<String, GenericDataType> availableDataTypes) {
        var fallbackBySuffix = new LinkedHashMap<String, GenericDataType>();
        for (var entry : availableDataTypes.entrySet()) {
            var key = entry.getKey();
            for (int i = 0; i < key.length(); i++) {
                var ch = key.charAt(i);
                if (ch == '.' || ch == '/') {
                    var suffix = key.substring(i + 1);
                    if (!suffix.isEmpty()) {
                        fallbackBySuffix.putIfAbsent(suffix, entry.getValue());
                    }
                }
            }
        }
        return Map.copyOf(fallbackBySuffix);
    }

    private static Map<String, Set<String>> buildMethodOwnerCandidatesBySimpleType(Map<String, GenericDataType> availableDataTypes) {
        var ownersBySimpleType = new LinkedHashMap<String, LinkedHashSet<String>>();
        for (var key : availableDataTypes.keySet()) {
            var baseName = baseTypeNameStatic(key);
            ownersBySimpleType.computeIfAbsent(simpleTypeNameStatic(baseName), ignored -> new LinkedHashSet<>()).add(baseName);
        }
        var immutable = new LinkedHashMap<String, Set<String>>();
        ownersBySimpleType.forEach((name, values) -> immutable.put(name, Set.copyOf(values)));
        return Map.copyOf(immutable);
    }

    private static Map<String, Set<String>> buildSubtypeParentOwnerCandidatesBySimpleType(Map<String, GenericDataType> availableDataTypes) {
        var ownersBySimpleType = new LinkedHashMap<String, LinkedHashSet<String>>();
        for (var entry : availableDataTypes.entrySet()) {
            if (!(entry.getValue() instanceof CompiledDataParentType parentType)) {
                continue;
            }
            var parentBaseName = baseTypeNameStatic(entry.getKey());
            for (var subType : parentType.subTypes()) {
                var subtypeSimple = simpleTypeNameStatic(baseTypeNameStatic(subType.name()));
                var owners = ownersBySimpleType.computeIfAbsent(subtypeSimple, ignored -> new LinkedHashSet<>());
                owners.add(baseTypeNameStatic(parentType.name()));
                owners.add(parentBaseName);
            }
        }
        var immutable = new LinkedHashMap<String, Set<String>>();
        ownersBySimpleType.forEach((name, values) -> immutable.put(name, Set.copyOf(values)));
        return Map.copyOf(immutable);
    }

    private static CompiledDataParentType findParentType(
            Map<String, GenericDataType> availableDataTypes,
            java.util.function.Predicate<String> qualifiedKeyMatcher,
            String simpleName
    ) {
        return availableDataTypes.entrySet().stream()
                .filter(entry -> qualifiedKeyMatcher.test(entry.getKey()))
                .map(Map.Entry::getValue)
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .findFirst()
                .orElseGet(() -> availableDataTypes.values().stream()
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast)
                        .filter(type -> simpleName.equals(type.name()))
                        .findFirst()
                        .orElse(null));
    }

    private static String baseTypeNameStatic(String typeName) {
        var genericStart = typeName.indexOf('[');
        return genericStart > 0 ? typeName.substring(0, genericStart) : typeName;
    }

    private static String simpleTypeNameStatic(String typeName) {
        var normalized = baseTypeNameStatic(typeName);
        var slash = normalized.lastIndexOf('/');
        var dot = normalized.lastIndexOf('.');
        var index = Math.max(slash, dot);
        return index >= 0 ? normalized.substring(index + 1) : normalized;
    }

    private List<String> splitTopLevelTypeDescriptors(String text) {
        var cached = linkCache.splitTopLevelTypeDescriptorCache.get(text);
        if (cached != null) {
            return cached;
        }
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
        var split = List.copyOf(values);
        linkCache.splitTopLevelTypeDescriptorCache.put(text, split);
        return split;
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
                    var elementType = resolveCollectionElementType(values, expectedElementType);
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
                    var elementType = resolveCollectionElementType(values, expectedElementType);
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

        var valueType = resolveCollectionElementType(
                entries.stream().map(CompiledNewDict.Entry::value).toList(),
                expectedValueType
        );
        return Result.success((CompiledExpression) new CompiledNewDict(List.copyOf(entries), new CompiledDict(valueType)));
    }

    private CompiledType resolveCollectionElementType(
            List<? extends CompiledExpression> values,
            CompiledType expectedElementType
    ) {
        if (expectedElementType == null) {
            return values.stream()
                    .map(CompiledExpression::type)
                    .reduce(CapybaraTypeFinder::findHigherType)
                    .orElse(ANY);
        }
        if (!containsGenericTypeParameter(expectedElementType)) {
            return expectedElementType;
        }
        return values.stream()
                .map(CompiledExpression::type)
                .filter(type -> !(type instanceof CompiledGenericTypeParameter))
                .reduce(CapybaraTypeFinder::findHigherType)
                .orElse(expectedElementType);
    }

    private Result<CompiledExpression> linkWithExpression(WithExpression expression, Scope scope) {
        return linkExpression(expression.source(), scope).flatMap(source -> {
            if (!(source.type() instanceof GenericDataType genericDataType)) {
                return withPosition(
                        Result.error("`.with(...)` requires data/type receiver, was `" + source.type() + "`"),
                        expression.position()
                );
            }
            if (expression.assignments().isEmpty()) {
                return withPosition(Result.error("`.with(...)` requires at least one named assignment"), expression.position());
            }
            var duplicate = firstDuplicateAssignment(expression.assignments());
            if (duplicate.isPresent()) {
                return withPosition(Result.error("Field `" + duplicate.get() + "` is assigned more than once"), expression.position());
            }

            var visibleFieldTypes = fieldTypesByName(genericDataType);
            for (var assignment : expression.assignments()) {
                if (!visibleFieldTypes.containsKey(assignment.name())) {
                    return withPosition(
                            Result.error("Field `" + assignment.name() + "` not found in type `" + genericDataType.name() + "`"),
                            assignment.value().position().or(() -> expression.position())
                    );
                }
            }

            var linkedUpdates = new java.util.LinkedHashMap<String, CompiledExpression>();
            for (var assignment : expression.assignments()) {
                var expectedType = visibleFieldTypes.get(assignment.name());
                var linked = linkArgumentForExpectedType(assignment.value(), scope, expectedType)
                        .map(CoercedArgument::expression);
                if (linked instanceof Result.Error<CompiledExpression> error) {
                    return new Result.Error<>(error.errors());
                }
                linkedUpdates.put(assignment.name(), ((Result.Success<CompiledExpression>) linked).value());
            }

            return switch (genericDataType) {
                case CompiledDataType dataType -> linkWithForData(source, dataType, linkedUpdates, expression.position());
                case CompiledDataParentType parentType -> linkWithForParent(source, parentType, linkedUpdates, expression.position());
            };
        });
    }

    private Optional<String> firstDuplicateAssignment(List<NewData.FieldAssignment> assignments) {
        var names = new java.util.LinkedHashSet<String>();
        for (var assignment : assignments) {
            if (!names.add(assignment.name())) {
                return Optional.of(assignment.name());
            }
        }
        return Optional.empty();
    }

    private Result<CompiledExpression> linkWithForData(
            CompiledExpression source,
            CompiledDataType dataType,
            Map<String, CompiledExpression> linkedUpdates,
            Optional<SourcePosition> position
    ) {
        var assignments = new java.util.ArrayList<CompiledNewData.FieldAssignment>(dataType.fields().size());
        for (var field : dataType.fields()) {
            var fieldType = resolveFieldType(dataType, field);
            var updated = linkedUpdates.get(field.name());
            if (updated != null) {
                var coerced = coerceExpressionToType(updated, fieldType);
                if (coerced.isEmpty()) {
                    return withPosition(
                            Result.error("Cannot coerce assigned value for field `" + field.name() + "` to `" + fieldType + "`"),
                            position
                    );
                }
                assignments.add(new CompiledNewData.FieldAssignment(field.name(), coerced.orElseThrow()));
            } else {
                assignments.add(new CompiledNewData.FieldAssignment(
                        field.name(),
                        new CompiledFieldAccess(source, field.name(), fieldType)
                ));
            }
        }
        if (hasProtectedConstructorPipeline(dataType)) {
            return applyProtectedConstructorIfNeeded(new CompiledNewData(dataType, List.copyOf(assignments)), position);
        }
        var args = new java.util.ArrayList<CompiledExpression>(assignments.size() + 1);
        args.add(source);
        args.addAll(assignments.stream().map(CompiledNewData.FieldAssignment::value).toList());
        return Result.success((CompiledExpression) new CompiledFunctionCall(
                METHOD_DECL_PREFIX + dataType.name() + "__with",
                List.copyOf(args),
                source.type()
        ));
    }

    private Result<CompiledExpression> linkWithForParent(
            CompiledExpression source,
            CompiledDataParentType parentType,
            Map<String, CompiledExpression> linkedUpdates,
            Optional<SourcePosition> position
    ) {
        var cases = new java.util.ArrayList<CompiledMatchExpression.MatchCase>(parentType.subTypes().size());
        for (var subtype : parentType.subTypes()) {
            var subtypeFields = resolveConstructorFields(subtype, parentType);
            var fieldPatterns = new java.util.ArrayList<CompiledMatchExpression.Pattern>(subtypeFields.size());
            var constructorAssignments = new java.util.ArrayList<CompiledNewData.FieldAssignment>(subtypeFields.size());
            for (var i = 0; i < subtypeFields.size(); i++) {
                var field = subtypeFields.get(i);
                var variableName = "__with_" + field.name() + "_" + i;
                fieldPatterns.add(new CompiledMatchExpression.VariablePattern(variableName));
                var updated = linkedUpdates.get(field.name());
                if (updated != null) {
                    var coerced = coerceExpressionToType(updated, field.type());
                    if (coerced.isEmpty()) {
                        return withPosition(
                                Result.error("Cannot coerce assigned value for field `" + field.name() + "` to `" + field.type() + "`"),
                                position
                        );
                    }
                    constructorAssignments.add(new CompiledNewData.FieldAssignment(field.name(), coerced.orElseThrow()));
                } else {
                    constructorAssignments.add(new CompiledNewData.FieldAssignment(
                            field.name(),
                            new CompiledVariable(variableName, field.type())
                        ));
                }
            }
            cases.add(new CompiledMatchExpression.MatchCase(
                    new CompiledMatchExpression.ConstructorPattern(subtype.name(), List.copyOf(fieldPatterns)),
                    Optional.empty(),
                    new CompiledNewData(subtype, List.copyOf(constructorAssignments))
            ));
        }
        return Result.success((CompiledExpression) new CompiledMatchExpression(
                source,
                List.copyOf(cases),
                parentType
        ));
    }

    private Optional<CompiledExpression> coerceExpressionToType(CompiledExpression expression, CompiledType expectedType) {
        var coerced = coerceArgument(expression, expectedType);
        if (coerced == null) {
            return Optional.empty();
        }
        return Optional.of(coerced.expression());
    }

    private Result<CompiledExpression> linkConstructorData(ConstructorData constructorData, Scope scope) {
        if (currentConstructorTypeName.isEmpty()) {
            return withPosition(
                    Result.error("`* { ... }` can only be used inside `with constructor`"),
                    constructorData.position()
            );
        }
        return rawLinkNewData(
                new NewData(
                        new DataType(currentConstructorTypeName.orElseThrow()),
                        constructorData.assignments(),
                        constructorData.positionalArguments(),
                        constructorData.spreads(),
                        constructorData.position()
                ),
                scope
        );
    }

    private Result<CompiledExpression> linkNewData(NewData newData, Scope scope) {
        return rawLinkNewData(newData, scope)
                .flatMap(expression -> {
                    if (!(expression instanceof CompiledNewData compiledNewData)) {
                        return Result.success(expression);
                    }
                    return applyProtectedConstructorIfNeeded(compiledNewData, newData.position());
                });
    }

    private Result<CompiledExpression> rawLinkNewData(NewData newData, Scope scope) {
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

    private Result<CompiledExpression> applyProtectedConstructorIfNeeded(
            CompiledNewData newData,
            Optional<SourcePosition> position
    ) {
        if (!(newData.type() instanceof CompiledDataType dataType)) {
            return Result.success(newData);
        }
        var directConstructor = directConstructorFor(dataType);
        var parentConstructors = parentConstructorsFor(dataType);
        if ((directConstructor == null || directConstructor.typeConstructor()) && parentConstructors.isEmpty()) {
            return Result.success(newData);
        }
        return applyConstructorPipeline(
                dataType,
                orderedAssignmentsByName(newData, dataType),
                parentConstructors,
                directConstructor != null && !directConstructor.typeConstructor() ? Optional.of(directConstructor) : Optional.empty(),
                position,
                0
        );
    }

    private boolean hasProtectedConstructorPipeline(CompiledDataType dataType) {
        return directConstructorFor(dataType) != null || !parentConstructorsFor(dataType).isEmpty();
    }

    private ProtectedConstructorRef directConstructorFor(CompiledDataType dataType) {
        var direct = constructorRegistry.constructorsByType().get(dataType.name());
        if (direct != null) {
            return direct;
        }
        return constructorRegistry.constructorsByType().entrySet().stream()
                .filter(entry -> sameRawTypeName(entry.getKey(), dataType.name()))
                .map(Map.Entry::getValue)
                .findFirst()
                .orElse(null);
    }

    private List<ProtectedConstructorRef> parentConstructorsFor(CompiledDataType dataType) {
        var direct = constructorRegistry.parentTypeConstructorsByDataType().get(dataType.name());
        if (direct != null) {
            return direct;
        }
        return constructorRegistry.parentTypeConstructorsByDataType().entrySet().stream()
                .filter(entry -> sameRawTypeName(entry.getKey(), dataType.name()))
                .map(Map.Entry::getValue)
                .findFirst()
                .orElse(List.of());
    }

    private Result<CompiledExpression> applyConstructorPipeline(
            CompiledDataType dataType,
            LinkedHashMap<String, CompiledExpression> fieldValues,
            List<ProtectedConstructorRef> parentConstructors,
            Optional<ProtectedConstructorRef> directConstructor,
            Optional<SourcePosition> position,
            int index
    ) {
        if (index >= parentConstructors.size()) {
            return applyDirectConstructorOrRaw(dataType, fieldValues, directConstructor, position);
        }
        var parentConstructor = parentConstructors.get(index);
        var stateType = resolveConstructorTargetType(parentConstructor, position);
        if (stateType instanceof Result.Error<CompiledDataType> error) {
            return new Result.Error<>(error.errors());
        }
        var linkedStateType = ((Result.Success<CompiledDataType>) stateType).value();
        var constructorCall = constructorCall(parentConstructor, linkedStateType, fieldValues, position);
        if (constructorCall instanceof Result.Error<CompiledExpression> error) {
            return new Result.Error<>(error.errors());
        }
        var callExpression = ((Result.Success<CompiledExpression>) constructorCall).value();
        var resultType = resolveSpecializedResultType(callExpression.type());
        if (resultType.isEmpty()) {
            return applyConstructorPipeline(
                    dataType,
                    updatedFieldValues(fieldValues, linkedStateType, callExpression),
                    parentConstructors,
                    directConstructor,
                    position,
                    index + 1
            );
        }

        var successVariable = "__constructor_state_" + index;
        var messageVariable = "__constructor_error_" + index;
        var successExpression = applyConstructorPipeline(
                dataType,
                updatedFieldValues(fieldValues, linkedStateType, new CompiledVariable(successVariable, linkedStateType)),
                parentConstructors,
                directConstructor,
                position,
                index + 1
        );
        if (successExpression instanceof Result.Error<CompiledExpression> error) {
            return new Result.Error<>(error.errors());
        }
        var finalResultType = resultParentForExpression(((Result.Success<CompiledExpression>) successExpression).value(), position);
        if (finalResultType instanceof Result.Error<CompiledDataParentType> error) {
            return new Result.Error<>(error.errors());
        }
        var resultParent = ((Result.Success<CompiledDataParentType>) finalResultType).value();
        var successResult = ensureResultExpression(((Result.Success<CompiledExpression>) successExpression).value(), resultParent, position);
        if (successResult instanceof Result.Error<CompiledExpression> error) {
            return new Result.Error<>(error.errors());
        }
        var errorResult = errorResultExpression(resultParent, messageVariable, position);
        return Result.success(new CompiledMatchExpression(
                callExpression,
                List.of(
                        new CompiledMatchExpression.MatchCase(
                                new CompiledMatchExpression.ConstructorPattern(
                                        "Success",
                                        List.of(new CompiledMatchExpression.VariablePattern(successVariable))
                                ),
                                Optional.empty(),
                                ((Result.Success<CompiledExpression>) successResult).value()
                        ),
                        new CompiledMatchExpression.MatchCase(
                                new CompiledMatchExpression.ConstructorPattern(
                                        "Error",
                                        List.of(new CompiledMatchExpression.VariablePattern(messageVariable))
                                ),
                                Optional.empty(),
                                errorResult
                        )
                ),
                resultParent
        ));
    }

    private Result<CompiledExpression> applyDirectConstructorOrRaw(
            CompiledDataType dataType,
            LinkedHashMap<String, CompiledExpression> fieldValues,
            Optional<ProtectedConstructorRef> directConstructor,
            Optional<SourcePosition> position
    ) {
        if (directConstructor.isEmpty()) {
            return Result.success(rawConstructedData(dataType, fieldValues));
        }
        return constructorCall(directConstructor.orElseThrow(), dataType, fieldValues, position);
    }

    private Result<CompiledDataType> resolveConstructorTargetType(
            ProtectedConstructorRef constructorRef,
            Optional<SourcePosition> position
    ) {
        var type = resolveDataTypeByName(constructorRef.internalTargetTypeName());
        if (type instanceof CompiledDataType dataType) {
            return Result.success(dataType);
        }
        return withPosition(
                Result.error("Constructor target `" + constructorRef.internalTargetTypeName() + "` not found"),
                position
        );
    }

    private Result<CompiledExpression> constructorCall(
            ProtectedConstructorRef constructorRef,
            CompiledDataType targetType,
            LinkedHashMap<String, CompiledExpression> fieldValues,
            Optional<SourcePosition> position
    ) {
        var resolvedModule = resolveQualifiedModule(constructorRef.ownerModuleName());
        if (resolvedModule == null) {
            return withPosition(
                    Result.error("Unknown module `" + constructorRef.ownerModuleName() + "` for constructor `" + constructorRef.functionName() + "`"),
                    position
            );
        }
        var signatures = functionsByNameAndArity(
                resolvedModule.signatures(),
                constructorRef.functionName(),
                targetType.fields().size()
        );
        if (signatures.isEmpty()) {
            return withPosition(
                    Result.error("Constructor `" + constructorRef.functionName() + "` not found for `" + constructorRef.targetTypeName() + "`"),
                    position
            );
        }
        var arguments = orderedAssignments(fieldValues, targetType.fields());
        var signature = signatures.getFirst();
        var resolvedReturnType = resolveReturnType(signature, arguments);
        if (resolvedReturnType == PrimitiveLinkedType.ANY) {
            resolvedReturnType = constructorRef.resultReturning()
                    ? resultTypeFor(targetType)
                    : targetType;
        }
        return Result.success(new CompiledFunctionCall(
                resolvedModule.javaModuleName() + "." + constructorRef.functionName(),
                arguments,
                resolvedReturnType
        ));
    }

    private LinkedHashMap<String, CompiledExpression> orderedAssignmentsByName(CompiledNewData newData, CompiledDataType dataType) {
        var byName = new LinkedHashMap<String, CompiledExpression>();
        var orderedValues = orderedAssignments(newData, dataType);
        for (var i = 0; i < dataType.fields().size(); i++) {
            byName.put(dataType.fields().get(i).name(), orderedValues.get(i));
        }
        return byName;
    }

    private List<CompiledExpression> orderedAssignments(
            LinkedHashMap<String, CompiledExpression> fieldValues,
            List<CompiledDataType.CompiledField> fields
    ) {
        return fields.stream()
                .map(CompiledDataType.CompiledField::name)
                .map(fieldValues::get)
                .toList();
    }

    private LinkedHashMap<String, CompiledExpression> updatedFieldValues(
            LinkedHashMap<String, CompiledExpression> originalValues,
            CompiledDataType stateType,
            CompiledExpression source
    ) {
        var updated = new LinkedHashMap<>(originalValues);
        for (var field : stateType.fields()) {
            updated.put(field.name(), new CompiledFieldAccess(source, field.name(), field.type()));
        }
        return updated;
    }

    private CompiledExpression rawConstructedData(
            CompiledDataType dataType,
            LinkedHashMap<String, CompiledExpression> fieldValues
    ) {
        return new CompiledNewData(
                dataType,
                dataType.fields().stream()
                        .map(field -> new CompiledNewData.FieldAssignment(field.name(), fieldValues.get(field.name())))
                        .toList()
        );
    }

    private Result<CompiledExpression> ensureResultExpression(
            CompiledExpression expression,
            CompiledDataParentType resultParent,
            Optional<SourcePosition> position
    ) {
        if (resolveSpecializedResultType(expression.type()).isPresent()) {
            return Result.success(expression);
        }
        var successType = findSubtype("Success", resultParent);
        if (successType instanceof Result.Error<CompiledDataType> error) {
            return new Result.Error<>(error.errors());
        }
        var linkedSuccessType = ((Result.Success<CompiledDataType>) successType).value();
        var valueField = linkedSuccessType.fields().stream()
                .filter(field -> field.name().equals("value"))
                .findFirst();
        if (valueField.isEmpty()) {
            return withPosition(Result.error("`Result.Success` is missing `value` field"), position);
        }
        var coerced = coerceExpressionToType(expression, valueField.orElseThrow().type());
        if (coerced.isEmpty()) {
            return withPosition(Result.error("Cannot coerce constructor result to `" + valueField.orElseThrow().type() + "`"), position);
        }
        return Result.success(new CompiledNewData(
                linkedSuccessType,
                List.of(new CompiledNewData.FieldAssignment("value", coerced.orElseThrow()))
        ));
    }

    private Result<CompiledDataParentType> resultParentForExpression(
            CompiledExpression expression,
            Optional<SourcePosition> position
    ) {
        var existing = resolveSpecializedResultType(expression.type());
        if (existing.isPresent()) {
            return Result.success(existing.orElseThrow());
        }
        var wrapped = resultTypeFor(expression.type());
        if (wrapped != null) {
            return Result.success(wrapped);
        }
        return withPosition(Result.error("Cannot wrap constructor result `" + expression.type() + "` into `Result`"), position);
    }

    private CompiledExpression errorResultExpression(
            CompiledDataParentType resultParent,
            String messageVariable,
            Optional<SourcePosition> position
    ) {
        var errorType = findSubtype("Error", resultParent);
        if (errorType instanceof Result.Error<CompiledDataType>) {
            throw new IllegalStateException("Result.Error constructor not found at " + position);
        }
        return new CompiledNewData(
                ((Result.Success<CompiledDataType>) errorType).value(),
                List.of(new CompiledNewData.FieldAssignment(
                        "message",
                        new CompiledVariable(messageVariable, PrimitiveLinkedType.STRING)
                ))
        );
    }

    private List<CompiledExpression> orderedAssignments(CompiledNewData newData, CompiledDataType dataType) {
        var byName = new java.util.LinkedHashMap<String, CompiledExpression>();
        for (var assignment : newData.assignments()) {
            byName.put(assignment.name(), assignment.value());
        }
        return dataType.fields().stream()
                .map(CompiledDataType.CompiledField::name)
                .map(byName::get)
                .toList();
    }

    private CompiledType inferDataTypeFromAssignments(CompiledType type, List<CompiledNewData.FieldAssignment> assignments) {
        if (!(type instanceof GenericDataType genericDataType) || !containsGenericTypeParameter(type)) {
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
                if (containsGenericTypeParameter(linkedDataType)) {
                    yield false;
                }
                if (linkedDataType.typeParameters().isEmpty()) {
                    yield true;
                }
                yield linkedDataType.typeParameters().stream()
                        .map(this::parseLinkedTypeDescriptor)
                        .allMatch(maybeType -> maybeType.map(this::isConcreteResolvedType).orElse(false));
            }
            case CompiledDataParentType linkedDataParentType -> {
                if (containsGenericTypeParameter(linkedDataParentType)) {
                    yield false;
                }
                if (linkedDataParentType.typeParameters().isEmpty()) {
                    yield true;
                }
                yield linkedDataParentType.typeParameters().stream()
                        .map(this::parseLinkedTypeDescriptor)
                        .allMatch(maybeType -> maybeType.map(this::isConcreteResolvedType).orElse(false));
            }
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
                        field -> resolveFieldType(genericDataType, field),
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
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase(java.util.Locale.ROOT);
            case CollectionLinkedType.CompiledList listType ->
                    "list[" + renderTypeForError(listType.elementType()) + "]";
            case CollectionLinkedType.CompiledSet setType ->
                    "set[" + renderTypeForError(setType.elementType()) + "]";
            case CollectionLinkedType.CompiledDict dictType ->
                    "dict[" + renderTypeForError(dictType.valueType()) + "]";
            case CompiledTupleType tupleType -> "tuple[" + tupleType.elementTypes().stream()
                    .map(this::renderTypeForError)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledFunctionType functionType ->
                    renderTypeForError(functionType.argumentType()) + "=>" + renderTypeForError(functionType.returnType());
            case CompiledGenericTypeParameter genericTypeParameter -> genericTypeParameter.name();
            case CompiledDataType dataType -> renderNamedTypeForError(dataType.name(), dataType.typeParameters());
            case CompiledDataParentType dataParentType -> renderNamedTypeForError(dataParentType.name(), dataParentType.typeParameters());
            default -> type.toString();
        };
    }

    private String renderNamedTypeForError(String name, List<String> typeParameters) {
        var displayName = stripQualifiedTypeName(name);
        if (typeParameters.isEmpty()) {
            return displayName;
        }
        return displayName + "[" + typeParameters.stream()
                .map(this::renderTypeDescriptorForError)
                .collect(java.util.stream.Collectors.joining(", ")) + "]";
    }

    private String renderTypeDescriptorForError(String descriptor) {
        return parseLinkedTypeDescriptor(descriptor)
                .map(this::renderTypeForError)
                .orElseGet(() -> stripQualifiedTypeName(descriptor));
    }

    private String stripQualifiedTypeName(String typeName) {
        var normalized = typeName == null ? "" : typeName.trim();
        var slash = normalized.lastIndexOf('/');
        if (slash >= 0 && slash + 1 < normalized.length()) {
            normalized = normalized.substring(slash + 1);
        }
        var dot = normalized.lastIndexOf('.');
        if (dot >= 0 && dot + 1 < normalized.length()) {
            normalized = normalized.substring(dot + 1);
        }
        return normalized;
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
        return linkLetExpression(expression, scope, Optional.empty());
    }

    private Result<CompiledExpression> linkLetExpression(
            LetExpression expression,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        if (expression.kind() == LetExpression.Kind.RESULT_BIND) {
            return linkResultBindLetExpression(expression, scope, expectedType);
        }
        if (expression.declaredType().isPresent()) {
            return linkTypeInScope(expression.declaredType().orElseThrow(), scope)
                    .flatMap(linkedDeclaredType -> linkArgumentForExpectedType(expression.value(), scope, linkedDeclaredType)
                            .flatMap(coercedValue -> linkLetRestExpression(
                                            expression.rest(),
                                            scope.add(expression.name(), linkedDeclaredType),
                                            expectedType
                                    )
                                    .map(rest -> new CompiledLetExpression(
                                            expression.name(),
                                            coercedValue.expression(),
                                            Optional.of(linkedDeclaredType),
                                            rest
                                    ))));
        }

        return linkExpression(expression.value(), scope)
                .flatMap(value -> {
                    var inferredDeclarationType = inferImplicitLetDeclarationType(
                            expression.name(),
                            value.type(),
                            expression.rest(),
                            scope
                    );
                    var letType = inferredDeclarationType.orElse(value.type());
                    return linkLetRestExpression(expression.rest(), scope.add(expression.name(), letType), expectedType)
                            .map(rest ->
                                    new CompiledLetExpression(
                                            expression.name(),
                                            value,
                                            inferredDeclarationType,
                                            rest
                                    ));
                });
    }

    private Result<CompiledExpression> linkResultBindLetExpression(
            LetExpression expression,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        if (expression.declaredType().isPresent()) {
            return linkTypeInScope(expression.declaredType().orElseThrow(), scope)
                    .flatMap(declaredType -> {
                        var expectedResultType = resultTypeFor(declaredType);
                        var linkedSource = linkResultBindSourceExpression(
                                expression.value(),
                                scope,
                                Optional.ofNullable(expectedResultType)
                        );
                        return linkedSource.flatMap(resultExpression -> {
                            var resultParent = resolveSpecializedResultType(resultExpression.type());
                            if (resultParent.isEmpty()) {
                                return withPosition(
                                        Result.error("`<-` can only be used with `Result[...]`, got `" + resultExpression.type() + "`"),
                                        expression.value().position().or(expression::position)
                                );
                            }
                            var successTypeResult = findSubtype("Success", resultParent.orElseThrow());
                            if (successTypeResult instanceof Result.Error<CompiledDataType> error) {
                                return new Result.Error<>(error.errors());
                            }
                            var successType = ((Result.Success<CompiledDataType>) successTypeResult).value();
                            var successValueField = successType.fields().stream()
                                    .filter(field -> field.name().equals("value"))
                                    .findFirst();
                            if (successValueField.isEmpty()) {
                                return withPosition(Result.error("`Result.Success` is missing `value` field"), expression.position());
                            }
                            return linkResultBindContinuation(
                                    expression,
                                    scope,
                                    resultExpression,
                                    resultParent.orElseThrow(),
                                    successValueField.orElseThrow().type(),
                                    declaredType,
                                    expectedType
                            );
                        });
                    });
        }
        return linkResultBindSourceExpression(expression.value(), scope)
                .flatMap(resultExpression -> {
                    var resultParent = resolveSpecializedResultType(resultExpression.type());
                    if (resultParent.isEmpty()) {
                        return withPosition(
                                Result.error("`<-` can only be used with `Result[...]`, got `" + resultExpression.type() + "`"),
                                expression.value().position().or(expression::position)
                        );
                    }
                    var successTypeResult = findSubtype("Success", resultParent.orElseThrow());
                    if (successTypeResult instanceof Result.Error<CompiledDataType> error) {
                        return new Result.Error<>(error.errors());
                    }
                    var successType = ((Result.Success<CompiledDataType>) successTypeResult).value();
                    var successValueField = successType.fields().stream()
                            .filter(field -> field.name().equals("value"))
                            .findFirst();
                    if (successValueField.isEmpty()) {
                        return withPosition(Result.error("`Result.Success` is missing `value` field"), expression.position());
                    }
                    return linkResultBindContinuation(
                            expression,
                            scope,
                            resultExpression,
                            resultParent.orElseThrow(),
                            successValueField.orElseThrow().type(),
                            successValueField.orElseThrow().type(),
                            expectedType
                    );
                });
    }

    private Result<CompiledExpression> linkResultBindSourceExpression(Expression expression, Scope scope) {
        return linkExpression(expression, scope)
                .flatMap(linked -> {
                    if (resolveSpecializedResultType(linked.type()).isPresent()) {
                        return Result.success(linked);
                    }
                    if (expression instanceof NewData newData) {
                        var linkedType = linkTypeInScope(newData.type(), scope);
                        if (linkedType instanceof Result.Success<CompiledType> success
                            && success.value() instanceof CompiledDataType dataType) {
                            var expectedResultType = resultTypeFor(dataType);
                            if (expectedResultType != null) {
                                return linkArgumentForExpectedType(expression, scope, expectedResultType)
                                        .map(coerced -> {
                                            var resolvedExpression = coerced.expression();
                                            if (resolvedExpression instanceof CompiledFunctionCall functionCall
                                                && resolveSpecializedResultType(functionCall.type()).isEmpty()) {
                                                return (CompiledExpression) new CompiledFunctionCall(
                                                        functionCall.name(),
                                                        functionCall.arguments(),
                                                        expectedResultType
                                                );
                                            }
                                            return resolvedExpression;
                                        });
                            }
                        }
                    }
                    return Result.success(linked);
                });
    }

    private Result<CompiledExpression> linkResultBindSourceExpression(
            Expression expression,
            Scope scope,
            Optional<CompiledType> expectedResultType
    ) {
        if (expectedResultType.isPresent()
            && (expression instanceof LetExpression
                || expression instanceof MatchExpression
                || expression instanceof IfExpression)) {
            return linkArgumentForExpectedType(expression, scope, expectedResultType.orElseThrow())
                    .map(CoercedArgument::expression);
        }
        return linkResultBindSourceExpression(expression, scope);
    }

    private Result<CompiledExpression> linkResultBindContinuation(
            LetExpression expression,
            Scope scope,
            CompiledExpression resultExpression,
            CompiledDataParentType resultParent,
            CompiledType payloadType,
            CompiledType letType,
            Optional<CompiledType> expectedType
    ) {
        var rawPayloadName = "__result_bind_value_" + scope.localValues().size();
        var boundPayload = new CompiledVariable(rawPayloadName, payloadType);
        var coercedPayload = coerceExpressionToType(boundPayload, letType);
        if (coercedPayload.isEmpty()) {
            return withPosition(
                    Result.error("Expected `" + letType + "`, got `" + payloadType + "`"),
                    expression.position()
            );
        }
        return linkResultBindRestExpression(expression.rest(), scope.add(expression.name(), letType), expectedType)
                .flatMap(rest -> {
                    var successContinuation = new CompiledLetExpression(
                            expression.name(),
                            coercedPayload.orElseThrow(),
                            expression.declaredType().isPresent() ? Optional.of(letType) : Optional.empty(),
                            rest
                    );
                    var successResultType = resultParentForExpression(rest, expression.position());
                    if (successResultType instanceof Result.Error<CompiledDataParentType> error) {
                        return new Result.Error<>(error.errors());
                    }
                    var wrappedSuccess = ensureResultExpression(
                            successContinuation,
                            ((Result.Success<CompiledDataParentType>) successResultType).value(),
                            expression.position()
                    );
                    if (wrappedSuccess instanceof Result.Error<CompiledExpression> error) {
                        return new Result.Error<>(error.errors());
                    }
                    var messageVariable = "__result_bind_error_" + scope.localValues().size();
                    return Result.success((CompiledExpression) new CompiledMatchExpression(
                            resultExpression,
                            List.of(
                                    new CompiledMatchExpression.MatchCase(
                                            new CompiledMatchExpression.ConstructorPattern(
                                                    "Success",
                                                    List.of(new CompiledMatchExpression.VariablePattern(rawPayloadName))
                                            ),
                                            Optional.empty(),
                                            ((Result.Success<CompiledExpression>) wrappedSuccess).value()
                                    ),
                                    new CompiledMatchExpression.MatchCase(
                                            new CompiledMatchExpression.ConstructorPattern(
                                                    "Error",
                                                    List.of(new CompiledMatchExpression.VariablePattern(messageVariable))
                                            ),
                                            Optional.empty(),
                                            errorResultExpression(
                                                    ((Result.Success<CompiledDataParentType>) successResultType).value(),
                                                    messageVariable,
                                                    expression.position()
                                            )
                                    )
                            ),
                            ((Result.Success<CompiledDataParentType>) successResultType).value()
                    ));
                });
    }

    private Result<CompiledExpression> linkLetRestExpression(
            Expression expression,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        return expectedType
                .<Result<CompiledExpression>>map(type -> linkArgumentForExpectedType(expression, scope, type)
                        .map(CoercedArgument::expression))
                .orElseGet(() -> linkExpression(expression, scope));
    }

    private Result<CompiledExpression> linkResultBindRestExpression(
            Expression expression,
            Scope scope,
            Optional<CompiledType> expectedType
    ) {
        if (expectedType.isPresent()
            && (expression instanceof LetExpression
                || expression instanceof MatchExpression
                || expression instanceof IfExpression)) {
            return linkArgumentForExpectedType(expression, scope, expectedType.orElseThrow())
                    .map(CoercedArgument::expression);
        }
        return linkExpression(expression, scope);
    }

    private Optional<CompiledType> inferImplicitLetDeclarationType(
            String variableName,
            CompiledType type,
            Expression rest,
            Scope scope
    ) {
        if (!(type instanceof CompiledDataType dataType) || !dataType.typeParameters().isEmpty()) {
            return Optional.empty();
        }
        if (containsGenericTypeParameter(dataType)) {
            return Optional.empty();
        }
        var genericParent = dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parentType -> parentType.subTypes().stream()
                        .anyMatch(subType -> sameRawTypeName(subType.name(), dataType.name())))
                .filter(parentType -> !parentType.typeParameters().isEmpty())
                .findFirst();
        if (genericParent.isEmpty()) {
            return Optional.empty();
        }
        var inferred = inferGenericBindingsFromRest(variableName, genericParent.orElseThrow(), rest, scope);
        if (inferred.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(substituteTypeParameters(genericParent.orElseThrow(), inferred));
    }

    private Map<String, CompiledType> inferGenericBindingsFromRest(
            String variableName,
            CompiledDataParentType parentType,
            Expression expression,
            Scope scope
    ) {
        var bindings = new LinkedHashMap<String, CompiledType>();
        collectGenericBindingsFromExpression(variableName, parentType, expression, scope, bindings);
        return bindings;
    }

    private void collectGenericBindingsFromExpression(
            String variableName,
            CompiledDataParentType parentType,
            Expression expression,
            Scope scope,
            Map<String, CompiledType> bindings
    ) {
        switch (expression) {
            case FunctionCall functionCall -> {
                collectBindingsFromFunctionCall(variableName, parentType, functionCall, scope, bindings);
                functionCall.arguments().forEach(argument ->
                        collectGenericBindingsFromExpression(variableName, parentType, argument, scope, bindings));
            }
            case LetExpression letExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, letExpression.value(), scope, bindings);
                var nestedScope = scope;
                var linkedValue = linkExpression(letExpression.value(), scope);
                if (linkedValue instanceof Result.Success<CompiledExpression> success) {
                    var letType = success.value().type();
                    if (letExpression.kind() == LetExpression.Kind.RESULT_BIND) {
                        letType = resolveSpecializedResultType(success.value().type())
                                .flatMap(resultType -> {
                                    var successType = findSubtype("Success", resultType);
                                    if (!(successType instanceof Result.Success<CompiledDataType> successDataType)) {
                                        return Optional.<CompiledType>empty();
                                    }
                                    return successDataType.value().fields().stream()
                                            .filter(field -> field.name().equals("value"))
                                            .map(CompiledDataType.CompiledField::type)
                                            .findFirst();
                                })
                                .orElse(letType);
                    }
                    nestedScope = nestedScope.add(letExpression.name(), letType);
                }
                collectGenericBindingsFromExpression(variableName, parentType, letExpression.rest(), nestedScope, bindings);
            }
            case IfExpression ifExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, ifExpression.condition(), scope, bindings);
                collectGenericBindingsFromExpression(variableName, parentType, ifExpression.thenBranch(), scope, bindings);
                collectGenericBindingsFromExpression(variableName, parentType, ifExpression.elseBranch(), scope, bindings);
            }
            case InfixExpression infixExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, infixExpression.left(), scope, bindings);
                collectGenericBindingsFromExpression(variableName, parentType, infixExpression.right(), scope, bindings);
            }
            case MatchExpression matchExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, matchExpression.matchWith(), scope, bindings);
                matchExpression.cases().forEach(matchCase ->
                        collectGenericBindingsFromExpression(variableName, parentType, matchCase.expression(), scope, bindings));
            }
            case FunctionInvoke functionInvoke -> {
                collectGenericBindingsFromExpression(variableName, parentType, functionInvoke.function(), scope, bindings);
                functionInvoke.arguments().forEach(argument ->
                        collectGenericBindingsFromExpression(variableName, parentType, argument, scope, bindings));
            }
            case FieldAccess fieldAccess ->
                    collectGenericBindingsFromExpression(variableName, parentType, fieldAccess.source(), scope, bindings);
            case TupleExpression tupleExpression ->
                    tupleExpression.values().forEach(value ->
                            collectGenericBindingsFromExpression(variableName, parentType, value, scope, bindings));
            case NewListExpression newListExpression ->
                    newListExpression.values().forEach(value ->
                            collectGenericBindingsFromExpression(variableName, parentType, value, scope, bindings));
            case NewSetExpression newSetExpression ->
                    newSetExpression.values().forEach(value ->
                            collectGenericBindingsFromExpression(variableName, parentType, value, scope, bindings));
            case NewDictExpression newDictExpression -> {
                newDictExpression.entries().forEach(entry -> {
                    collectGenericBindingsFromExpression(variableName, parentType, entry.key(), scope, bindings);
                    collectGenericBindingsFromExpression(variableName, parentType, entry.value(), scope, bindings);
                });
            }
            case NewData newData -> newData.assignments().forEach(assignment ->
                    collectGenericBindingsFromExpression(variableName, parentType, assignment.value(), scope, bindings));
            case WithExpression withExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, withExpression.source(), scope, bindings);
                withExpression.assignments().forEach(assignment ->
                        collectGenericBindingsFromExpression(variableName, parentType, assignment.value(), scope, bindings));
            }
            case SliceExpression sliceExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, sliceExpression.source(), scope, bindings);
                sliceExpression.start().ifPresent(value ->
                        collectGenericBindingsFromExpression(variableName, parentType, value, scope, bindings));
                sliceExpression.end().ifPresent(value ->
                        collectGenericBindingsFromExpression(variableName, parentType, value, scope, bindings));
            }
            case IndexExpression indexExpression -> {
                collectGenericBindingsFromExpression(variableName, parentType, indexExpression.source(), scope, bindings);
                collectGenericBindingsFromExpression(variableName, parentType, indexExpression.index(), scope, bindings);
            }
            case LambdaExpression lambdaExpression ->
                    collectGenericBindingsFromExpression(variableName, parentType, lambdaExpression.expression(), scope, bindings);
            default -> {
            }
        }
    }

    private void collectBindingsFromFunctionCall(
            String variableName,
            CompiledDataParentType parentType,
            FunctionCall functionCall,
            Scope scope,
            Map<String, CompiledType> bindings
    ) {
        if (functionCall.arguments().isEmpty() || !containsValueReference(functionCall.arguments().get(0), variableName)) {
            return;
        }
        functionSignatures.stream()
                .filter(signature -> matchesInferenceSignature(functionCall, signature, parentType))
                .findFirst()
                .ifPresent(signature -> {
                    for (var i = 1; i < functionCall.arguments().size(); i++) {
                        if (containsValueReference(functionCall.arguments().get(i), variableName)) {
                            continue;
                        }
                        var linkedArgument = linkExpression(functionCall.arguments().get(i), scope);
                        if (linkedArgument instanceof Result.Success<CompiledExpression> success) {
                            collectTypeBindings(signature.parameterTypes().get(i), success.value().type(), bindings);
                        }
                    }
                });
    }

    private boolean matchesInferenceSignature(
            FunctionCall functionCall,
            FunctionSignature signature,
            CompiledDataParentType parentType
    ) {
        if (signature.parameterTypes().size() != functionCall.arguments().size()) {
            return false;
        }
        if (!(signature.parameterTypes().getFirst() instanceof CompiledDataParentType signatureParent)
            || !sameRawTypeName(signatureParent.name(), parentType.name())) {
            return false;
        }
        if (signature.name().equals(functionCall.name())) {
            return true;
        }
        if (!functionCall.name().startsWith(METHOD_INVOKE_PREFIX)) {
            return false;
        }
        var methodName = functionCall.name().substring(METHOD_INVOKE_PREFIX.length());
        return signature.name().startsWith(METHOD_DECL_PREFIX)
               && signature.name().endsWith("__" + methodName);
    }

    private void collectTypeBindings(
            CompiledType expected,
            CompiledType actual,
            Map<String, CompiledType> bindings
    ) {
        switch (expected) {
            case CompiledGenericTypeParameter genericTypeParameter -> bindings.putIfAbsent(genericTypeParameter.name(), actual);
            case CompiledList expectedList when actual instanceof CompiledList actualList ->
                    collectTypeBindings(expectedList.elementType(), actualList.elementType(), bindings);
            case CompiledSet expectedSet when actual instanceof CompiledSet actualSet ->
                    collectTypeBindings(expectedSet.elementType(), actualSet.elementType(), bindings);
            case CompiledDict expectedDict when actual instanceof CompiledDict actualDict ->
                    collectTypeBindings(expectedDict.valueType(), actualDict.valueType(), bindings);
            case CompiledTupleType expectedTuple when actual instanceof CompiledTupleType actualTuple -> {
                var max = Math.min(expectedTuple.elementTypes().size(), actualTuple.elementTypes().size());
                for (var i = 0; i < max; i++) {
                    collectTypeBindings(expectedTuple.elementTypes().get(i), actualTuple.elementTypes().get(i), bindings);
                }
            }
            case CompiledFunctionType expectedFunction when actual instanceof CompiledFunctionType actualFunction -> {
                collectTypeBindings(expectedFunction.argumentType(), actualFunction.argumentType(), bindings);
                collectTypeBindings(expectedFunction.returnType(), actualFunction.returnType(), bindings);
            }
            case CompiledDataParentType expectedParent -> collectParentTypeBindings(expectedParent, actual, bindings);
            case CompiledDataType expectedData when actual instanceof CompiledDataType actualData -> {
                var max = Math.min(expectedData.fields().size(), actualData.fields().size());
                for (var i = 0; i < max; i++) {
                    collectTypeBindings(expectedData.fields().get(i).type(), actualData.fields().get(i).type(), bindings);
                }
            }
            default -> {
            }
        }
    }

    private void collectParentTypeBindings(
            CompiledDataParentType expectedParent,
            CompiledType actual,
            Map<String, CompiledType> bindings
    ) {
        if (actual instanceof CompiledDataParentType actualParent && sameRawTypeName(expectedParent.name(), actualParent.name())) {
            var max = Math.min(expectedParent.typeParameters().size(), actualParent.typeParameters().size());
            for (var i = 0; i < max; i++) {
                var expectedType = parseLinkedTypeDescriptor(expectedParent.typeParameters().get(i));
                var actualType = parseLinkedTypeDescriptor(actualParent.typeParameters().get(i));
                if (expectedType.isPresent() && actualType.isPresent()) {
                    collectTypeBindings(expectedType.orElseThrow(), actualType.orElseThrow(), bindings);
                }
            }
            return;
        }
        if (!(actual instanceof CompiledDataType actualData) || !isSubtypeOfParent(actualData, expectedParent)) {
            return;
        }
        var max = Math.min(expectedParent.typeParameters().size(), actualData.typeParameters().size());
        for (var i = 0; i < max; i++) {
            var expectedType = parseLinkedTypeDescriptor(expectedParent.typeParameters().get(i));
            var actualType = parseLinkedTypeDescriptor(actualData.typeParameters().get(i));
            if (expectedType.isPresent() && actualType.isPresent()) {
                collectTypeBindings(expectedType.orElseThrow(), actualType.orElseThrow(), bindings);
            }
        }
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

    public record ConstructorRegistry(
            Map<String, ProtectedConstructorRef> constructorsByType,
            Map<String, List<ProtectedConstructorRef>> parentTypeConstructorsByDataType
    ) {
        public ConstructorRegistry {
            parentTypeConstructorsByDataType = parentTypeConstructorsByDataType.entrySet().stream()
                    .collect(java.util.stream.Collectors.toUnmodifiableMap(
                            Map.Entry::getKey,
                            entry -> List.copyOf(entry.getValue())
                    ));
            constructorsByType = Map.copyOf(constructorsByType);
        }
    }

    public record ProtectedConstructorRef(
            String ownerModuleName,
            String functionName,
            String targetTypeName,
            String internalTargetTypeName,
            boolean typeConstructor,
            boolean resultReturning
    ) {
    }
    private record CoercedArgument(CompiledExpression expression, int coercions) {
    }

    private record CoercedArguments(List<CompiledExpression> arguments, int coercions) {
    }

    private record ResolvedFunctionCall(
            FunctionSignature signature,
            List<CompiledExpression> arguments,
            int coercions,
            CompiledType returnType
    ) {
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
