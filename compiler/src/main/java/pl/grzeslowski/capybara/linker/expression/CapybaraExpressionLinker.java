package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.parser.*;

import java.math.BigInteger;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;

import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.*;
import static pl.grzeslowski.capybara.linker.expression.CapybaraTypeFinder.findHigherType;

public class CapybaraExpressionLinker {
    private static final Logger LOG = Logger.getLogger(CapybaraExpressionLinker.class.getName());
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String METHOD_INVOKE_PREFIX = "__invoke__";
    private static final String DICT_PIPE_ARGS_SEPARATOR = "::";
    private final List<LinkedFunction.LinkedFunctionParameter> parameters;
    private final Map<String, GenericDataType> dataTypes;
    private final List<FunctionSignature> functionSignatures;
    private final Map<String, List<FunctionSignature>> functionSignaturesByModule;
    private final Map<String, String> moduleClassNameByModuleName;

    public CapybaraExpressionLinker(
            List<LinkedFunction.LinkedFunctionParameter> parameters,
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

    public ValueOrError<LinkedExpression> linkExpression(Expression expression) {
        return linkExpression(expression, Scope.EMPTY);
    }

    private ValueOrError<LinkedExpression> linkExpression(Expression expression, Scope scope) {
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
            case LambdaExpression lambdaExpression -> withPosition(
                    ValueOrError.error("Lambda expression can only be used as the right side of `|`, `|-`, `|*`, `|all?` or `|any?`"),
                    lambdaExpression.position()
            );
            case ReduceExpression reduceExpression -> withPosition(
                    ValueOrError.error("Reduce expression can only be used as the right side of `|>`"),
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

    private ValueOrError<LinkedExpression> linkSliceExpression(SliceExpression expression, Scope scope) {
        return linkExpression(expression.source(), scope)
                .flatMap(source -> {
                    if (!(source.type() instanceof LinkedList)
                        && source.type() != STRING
                        && !(source.type() instanceof LinkedTupleType)) {
                        return withPosition(
                                ValueOrError.error("Slice source has to be `list`, `string` or `tuple`, was `" + source.type() + "`"),
                                expression.position()
                        );
                    }
                    return linkSliceBound(expression.start(), scope, "start")
                            .flatMap(start -> linkSliceBound(expression.end(), scope, "end")
                                    .map(end -> (LinkedExpression) new LinkedSliceExpression(
                                            source,
                                            start,
                                            end,
                                            slicedType(source.type(), start, end)
                                    )));
                });
    }

    private ValueOrError<LinkedExpression> linkIndexExpression(IndexExpression expression, Scope scope) {
        return linkExpression(expression.source(), scope)
                .flatMap(source -> {
                    if (!(source.type() instanceof LinkedList)
                        && source.type() != STRING
                        && !(source.type() instanceof LinkedTupleType)) {
                        return withPosition(
                                ValueOrError.error("Index source has to be `list`, `string` or `tuple`, was `" + source.type() + "`"),
                                expression.position()
                        );
                    }
                    return linkExpression(expression.index(), scope)
                            .flatMap(index -> {
                                if (index.type() != PrimitiveLinkedType.INT) {
                                    return withPosition(
                                            ValueOrError.error("Index has to be `int`, was `" + index.type() + "`"),
                                            expression.index().position()
                                    );
                                }
                                var elementType = switch (source.type()) {
                                    case LinkedList linkedList -> ValueOrError.success(linkedList.elementType());
                                    case PrimitiveLinkedType primitive when primitive == STRING -> ValueOrError.<LinkedType>success(STRING);
                                    case LinkedTupleType tupleType -> tupleElementType(tupleType, index, expression.index().position());
                                    default -> ValueOrError.<LinkedType>error("Unsupported index source `" + source.type() + "`");
                                };
                                if (elementType instanceof ValueOrError.Error<LinkedType> error) {
                                    return withPosition(new ValueOrError.Error<>(error.errors()), expression.position());
                                }
                                var resolvedElementType = ((ValueOrError.Value<LinkedType>) elementType).value();
                                if (source.type() instanceof LinkedTupleType) {
                                    return ValueOrError.success((LinkedExpression) new LinkedIndexExpression(
                                            source,
                                            index,
                                            resolvedElementType,
                                            resolvedElementType
                                    ));
                                }
                                var optionType = optionTypeFor(resolvedElementType);
                                if (optionType == null) {
                                    return withPosition(ValueOrError.error("Option type not found"), expression.position());
                                }
                                return ValueOrError.success((LinkedExpression) new LinkedIndexExpression(source, index, resolvedElementType, optionType));
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkTupleExpression(TupleExpression expression, Scope scope) {
        return expression.values().stream()
                .map(value -> linkExpression(value, scope))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(values -> {
                    var tupleType = new LinkedTupleType(values.stream().map(LinkedExpression::type).toList());
                    return (LinkedExpression) new LinkedTupleExpression(values, tupleType);
                });
    }

    private ValueOrError<Optional<LinkedExpression>> linkSliceBound(
            Optional<Expression> bound,
            Scope scope,
            String name
    ) {
        if (bound.isEmpty()) {
            return ValueOrError.success(Optional.empty());
        }
        return linkExpression(bound.get(), scope)
                .flatMap(linked -> {
                    if (linked.type() != PrimitiveLinkedType.INT) {
                        return withPosition(
                                ValueOrError.error("Slice " + name + " index has to be `int`, was `" + linked.type() + "`"),
                                bound.get().position()
                        );
                    }
                    return ValueOrError.success(Optional.of(linked));
                });
    }

    private ValueOrError<LinkedExpression> linkBooleanValue(BooleanValue booleanValue, Scope scope) {
        return ValueOrError.success(booleanValue.value() ? LinkedBooleanValue.TRUE : LinkedBooleanValue.FALSE);
    }

    private ValueOrError<LinkedExpression> linkFieldAccess(FieldAccess fieldAccess, Scope scope) {
        return linkExpression(fieldAccess.source(), scope)
                .flatMap(source -> {
                    if (source.type() instanceof LinkedList
                        || source.type() instanceof LinkedSet
                        || source.type() instanceof LinkedDict
                        || source.type() == STRING) {
                        if ("size".equals(fieldAccess.field())) {
                            var javaField = source.type() == STRING ? "length" : "size";
                            return ValueOrError.success(new LinkedFieldAccess(source, javaField, PrimitiveLinkedType.INT));
                        }
                        if (source.type() == STRING && "is_empty".equals(fieldAccess.field())) {
                            return ValueOrError.success(new LinkedFieldAccess(source, "isEmpty", PrimitiveLinkedType.BOOL));
                        }
                        return withPosition(
                                ValueOrError.error("Field `" + fieldAccess.field() + "` not found in type `" + source.type() + "`"),
                                fieldAccess.position()
                        );
                    }
                    if (!(source.type() instanceof GenericDataType dataType)) {
                        return withPosition(
                                ValueOrError.error("Field access requires data type, was `" + source.type() + "`"),
                                fieldAccess.position()
                        );
                    }
                    return dataType.fields().stream()
                            .filter(field -> field.name().equals(fieldAccess.field()))
                            .findFirst()
                            .<ValueOrError<LinkedExpression>>map(field ->
                                    ValueOrError.success(new LinkedFieldAccess(
                                            source,
                                            field.name(),
                                            resolveFieldType(dataType, field)
                                    )))
                            .orElseGet(() -> withPosition(
                                    ValueOrError.error("Field `" + fieldAccess.field() + "` not found in type `" + dataType.name() + "`"),
                                    fieldAccess.position()
                            ));
                });
    }

    private LinkedType resolveFieldType(GenericDataType dataType, LinkedDataType.LinkedField field) {
        var actualTypeDescriptors = switch (dataType) {
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters();
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
        };
        if (actualTypeDescriptors.isEmpty()) {
            return field.type();
        }

        var resolvedDataType = resolveDataTypeByName(dataType.name());
        var declaredTypeParameters = switch (resolvedDataType) {
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters();
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            default -> actualTypeDescriptors;
        };
        if (declaredTypeParameters.isEmpty()) {
            return field.type();
        }

        var substitutions = new java.util.LinkedHashMap<String, LinkedType>();
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

    private ValueOrError<LinkedExpression> linkFloatValue(FloatValue floatValue, Scope scope) {
        try {
            var value = floatValue.floatValue();
            var normalized = value.endsWith("f") || value.endsWith("F")
                    ? value.substring(0, value.length() - 1)
                    : value;
            var parsed = Float.parseFloat(normalized);
            if (!Float.isFinite(parsed)) {
                return withPosition(ValueOrError.error("Float literal out of range: `" + value + "`"), floatValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(ValueOrError.error("Invalid float literal: `" + floatValue.floatValue() + "`"), floatValue.position());
        }
        return ValueOrError.success(new LinkedFloatValue(floatValue.floatValue()));
    }

    private ValueOrError<LinkedExpression> linkByteValue(ByteValue byteValue, Scope scope) {
        try {
            var raw = byteValue.byteValue();
            var digits = raw.substring(2);
            var parsed = new BigInteger(digits, 16);
            if (parsed.compareTo(BigInteger.ZERO) < 0 || parsed.compareTo(BigInteger.valueOf(255)) > 0) {
                return withPosition(ValueOrError.error("Byte literal out of range: `" + raw + "`"), byteValue.position());
            }
        } catch (RuntimeException e) {
            return withPosition(ValueOrError.error("Invalid byte literal: `" + byteValue.byteValue() + "`"), byteValue.position());
        }
        return ValueOrError.success(new LinkedByteValue(byteValue.byteValue()));
    }

    private ValueOrError<LinkedExpression> linkDoubleValue(DoubleValue doubleValue, Scope scope) {
        try {
            var raw = doubleValue.doubleValue();
            var normalized = raw.endsWith("d") || raw.endsWith("D")
                    ? raw.substring(0, raw.length() - 1)
                    : raw;
            var parsed = Double.parseDouble(normalized);
            if (!Double.isFinite(parsed)) {
                return withPosition(ValueOrError.error("Double literal out of range: `" + raw + "`"), doubleValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(ValueOrError.error("Invalid double literal: `" + doubleValue.doubleValue() + "`"), doubleValue.position());
        }
        return ValueOrError.success(new LinkedDoubleValue(doubleValue.doubleValue()));
    }

    private ValueOrError<LinkedExpression> linkFunctionCall(FunctionCall functionCall, Scope scope) {
        if (functionCall.moduleName().isPresent()) {
            return resolveQualifiedFunctionCall(functionCall, scope);
        }
        var functionVariable = resolveFunctionVariable(functionCall.name(), scope);
        if (functionVariable.isPresent()) {
            return resolveFunctionInvoke(functionCall, scope, functionVariable.get());
        }
        return resolveGlobalFunctionCall(functionCall, scope);
    }

    private ValueOrError<LinkedExpression> linkFunctionInvoke(FunctionInvoke functionInvoke, Scope scope) {
        return linkExpression(functionInvoke.function(), scope)
                .flatMap(function -> resolveFunctionInvoke(functionInvoke, scope, function));
    }

    private ValueOrError<LinkedExpression> resolveQualifiedFunctionCall(FunctionCall functionCall, Scope scope) {
        var rawModuleName = functionCall.moduleName().orElseThrow();
        var moduleName = normalizeQualifiedModuleName(rawModuleName);
        var resolvedModule = resolveQualifiedModule(moduleName);
        if (resolvedModule == null) {
            return withPosition(
                    ValueOrError.error("Unknown module `" + rawModuleName + "` in call `" + rawModuleName + "." + functionCall.name() + "`"),
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
                    ValueOrError.error(
                            "Module `" + moduleName + "` has no function `" + functionCall.name() + "` with "
                            + functionCall.arguments().size() + " argument(s)"
                    ),
                    functionCall.position()
            );
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (!(maybeResolved instanceof ValueOrError.Value<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof ValueOrError.Value<LinkedExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return withPosition(
                    ValueOrError.error(
                            "No matching function `" + moduleName + "." + functionCall.name() + "` for argument types " + actualTypes
                    ),
                    functionCall.position()
            );
        }

        return ValueOrError.success(new LinkedFunctionCall(
                resolvedModule.javaModuleName() + "." + functionCall.name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
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

    private ValueOrError<LinkedExpression> resolveGlobalFunctionCall(FunctionCall functionCall, Scope scope) {
        if (functionCall.name().startsWith(METHOD_INVOKE_PREFIX)) {
            return resolveMethodInvokeCall(functionCall, scope);
        }
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionCall.name()))
                .filter(signature -> signature.parameterTypes().size() == functionCall.arguments().size())
                .toList();
        if (candidates.isEmpty()) {
            return functionCall.arguments()
                    .stream()
                    .map((Expression expression) -> linkExpression(expression, scope))
                    .collect(new ValueOrErrorCollectionCollector<>())
                    .map(args -> (LinkedExpression) new LinkedFunctionCall(functionCall.name(), args, ANY));
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (!(maybeResolved instanceof ValueOrError.Value<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof ValueOrError.Value<LinkedExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return withPosition(
                    ValueOrError.error(
                            "No matching function `" + functionCall.name() + "` for argument types " + actualTypes
                    ),
                    functionCall.position()
            );
        }
        return ValueOrError.success(new LinkedFunctionCall(
                functionCall.name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private ValueOrError<LinkedExpression> resolveMethodInvokeCall(FunctionCall functionCall, Scope scope) {
        var methodName = functionCall.name().substring(METHOD_INVOKE_PREFIX.length());
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().startsWith(METHOD_DECL_PREFIX))
                .filter(signature -> signature.name().endsWith("__" + methodName))
                .filter(signature -> signature.parameterTypes().size() == functionCall.arguments().size())
                .toList();
        if (candidates.isEmpty()) {
            return resolveBuiltinMethodInvoke(functionCall, scope, methodName)
                    .orElseGet(() -> withPosition(
                            ValueOrError.error("No method `" + methodName + "` with " + functionCall.arguments().size() + " argument(s)"),
                            functionCall.position()
                    ));
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (!(maybeResolved instanceof ValueOrError.Value<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof ValueOrError.Value<LinkedExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return resolveBuiltinMethodInvoke(functionCall, scope, methodName)
                    .orElseGet(() -> withPosition(
                            ValueOrError.error("No matching method `" + methodName + "` for argument types " + actualTypes),
                            functionCall.position()
                    ));
        }
        return ValueOrError.success(new LinkedFunctionCall(
                best.signature().name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private LinkedType resolveReturnType(FunctionSignature signature, List<LinkedExpression> arguments) {
        if (signature.parameterTypes().isEmpty() || arguments.isEmpty()) {
            return signature.returnType();
        }
        var substitutions = new java.util.LinkedHashMap<String, LinkedType>();
        var count = Math.min(signature.parameterTypes().size(), arguments.size());
        for (var i = 0; i < count; i++) {
            collectTypeSubstitutions(signature.parameterTypes().get(i), arguments.get(i).type(), substitutions);
        }
        if (substitutions.isEmpty()) {
            return signature.returnType();
        }
        return substituteTypeParameters(signature.returnType(), substitutions);
    }

    private void collectTypeSubstitutions(LinkedType expected, LinkedType actual, Map<String, LinkedType> substitutions) {
        if (expected instanceof LinkedGenericTypeParameter genericTypeParameter) {
            substitutions.putIfAbsent(genericTypeParameter.name(), actual);
            return;
        }
        if (expected instanceof LinkedList expectedList && actual instanceof LinkedList actualList) {
            collectTypeSubstitutions(expectedList.elementType(), actualList.elementType(), substitutions);
            return;
        }
        if (expected instanceof LinkedSet expectedSet && actual instanceof LinkedSet actualSet) {
            collectTypeSubstitutions(expectedSet.elementType(), actualSet.elementType(), substitutions);
            return;
        }
        if (expected instanceof LinkedDict expectedDict && actual instanceof LinkedDict actualDict) {
            collectTypeSubstitutions(expectedDict.valueType(), actualDict.valueType(), substitutions);
            return;
        }
        if (expected instanceof LinkedFunctionType expectedFunction && actual instanceof LinkedFunctionType actualFunction) {
            collectTypeSubstitutions(expectedFunction.argumentType(), actualFunction.argumentType(), substitutions);
            collectTypeSubstitutions(expectedFunction.returnType(), actualFunction.returnType(), substitutions);
            return;
        }
        if (expected instanceof LinkedTupleType expectedTuple && actual instanceof LinkedTupleType actualTuple) {
            var count = Math.min(expectedTuple.elementTypes().size(), actualTuple.elementTypes().size());
            for (var i = 0; i < count; i++) {
                collectTypeSubstitutions(expectedTuple.elementTypes().get(i), actualTuple.elementTypes().get(i), substitutions);
            }
            return;
        }
        if (expected instanceof LinkedDataParentType expectedParent && actual instanceof LinkedDataType actualData) {
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
                case LinkedDataType linkedDataType -> linkedDataType.typeParameters();
                case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            };
            var actualTypeParameters = switch (actualData) {
                case LinkedDataType linkedDataType -> linkedDataType.typeParameters();
                case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
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
            Map<String, LinkedType> substitutions
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

    private Optional<ValueOrError<LinkedExpression>> resolveBuiltinMethodInvoke(FunctionCall functionCall, Scope scope, String methodName) {
        var supportsTwoStrings = "contains".equals(methodName)
                || "starts_with".equals(methodName)
                || "end_with".equals(methodName);
        var supportsTrim = "trim".equals(methodName);
        var supportsIsEmpty = "is_empty".equals(methodName);
        var supportsToInt = "to_int".equals(methodName);
        var supportsToLong = "to_long".equals(methodName);
        var supportsToDouble = "to_double".equals(methodName);
        var supportsToFloat = "to_float".equals(methodName);
        var supportsToBool = "to_bool".equals(methodName);
        var supportsSingleString = supportsTrim || supportsIsEmpty
                                   || supportsToInt || supportsToLong || supportsToDouble || supportsToFloat || supportsToBool;
        if ((!supportsTwoStrings && !supportsSingleString)
                || (supportsTwoStrings && functionCall.arguments().size() != 2)
                || (supportsSingleString && functionCall.arguments().size() != 1)) {
            return Optional.empty();
        }
        var linkedArguments = functionCall.arguments().stream()
                .map(argument -> linkExpression(argument, scope))
                .collect(new ValueOrErrorCollectionCollector<>());
        if (!(linkedArguments instanceof ValueOrError.Value<java.util.List<LinkedExpression>> value)) {
            if (linkedArguments instanceof ValueOrError.Error<java.util.List<LinkedExpression>> error) {
                return Optional.of(new ValueOrError.Error<>(error.errors()));
            }
            return Optional.empty();
        }
        var args = value.value();
        if (supportsTwoStrings) {
            if (args.get(0).type() != STRING || args.get(1).type() != STRING) {
                return Optional.empty();
            }
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__" + methodName,
                    args,
                    BOOL
            )));
        }
        if (args.get(0).type() != STRING) {
            return Optional.empty();
        }
        if (supportsIsEmpty) {
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__is_empty",
                    args,
                    BOOL
            )));
        }
        if (supportsToInt) {
            var resultType = resultTypeFor(INT);
            if (resultType == null) {
                return Optional.of(withPosition(ValueOrError.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_int",
                    args,
                    resultType
            )));
        }
        if (supportsToLong) {
            var resultType = resultTypeFor(LONG);
            if (resultType == null) {
                return Optional.of(withPosition(ValueOrError.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_long",
                    args,
                    resultType
            )));
        }
        if (supportsToDouble) {
            var resultType = resultTypeFor(DOUBLE);
            if (resultType == null) {
                return Optional.of(withPosition(ValueOrError.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_double",
                    args,
                    resultType
            )));
        }
        if (supportsToFloat) {
            var resultType = resultTypeFor(FLOAT);
            if (resultType == null) {
                return Optional.of(withPosition(ValueOrError.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_float",
                    args,
                    resultType
            )));
        }
        if (supportsToBool) {
            var resultType = resultTypeFor(BOOL);
            if (resultType == null) {
                return Optional.of(withPosition(ValueOrError.error("Result type not found"), functionCall.position()));
            }
            return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                    METHOD_DECL_PREFIX + "String__to_bool",
                    args,
                    resultType
            )));
        }
        return Optional.of(ValueOrError.success(new LinkedFunctionCall(
                METHOD_DECL_PREFIX + "String__trim",
                args,
                STRING
        )));
    }

    private Optional<LinkedVariable> resolveFunctionVariable(String name, Scope scope) {
        if (scope.localValues().get(name) instanceof LinkedFunctionType functionType) {
            return Optional.of(new LinkedVariable(name, functionType));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(name))
                .filter(parameter -> parameter.type() instanceof LinkedFunctionType)
                .findFirst()
                .map(parameter -> new LinkedVariable(parameter.name(), parameter.type()));
    }

    private ValueOrError<LinkedExpression> resolveFunctionInvoke(
            FunctionCall functionCall,
            Scope scope,
            LinkedVariable function
    ) {
        if (!(function.type() instanceof LinkedFunctionType functionType)) {
            return withPosition(ValueOrError.error("Variable `" + function.name() + "` is not callable"), functionCall.position());
        }
        if (functionCall.arguments().isEmpty()) {
            return withPosition(
                    ValueOrError.error("Function variable `" + function.name() + "` requires at least one argument"),
                    functionCall.position()
            );
        }
        var currentType = (LinkedType) functionType;
        var coercedArguments = new java.util.ArrayList<LinkedExpression>(functionCall.arguments().size());
        for (var argument : functionCall.arguments()) {
            if (!(currentType instanceof LinkedFunctionType currentFunctionType)) {
                return withPosition(
                        ValueOrError.error("Function variable `" + function.name() + "` called with too many arguments"),
                        functionCall.position()
                );
            }
            var linked = linkArgumentForExpectedType(argument, scope, currentFunctionType.argumentType());
            if (linked instanceof ValueOrError.Error<CoercedArgument> error) {
                return withPosition(new ValueOrError.Error<>(error.errors()), argument.position());
            }
            var coerced = ((ValueOrError.Value<CoercedArgument>) linked).value();
            coercedArguments.add(coerced.expression());
            currentType = currentFunctionType.returnType();
        }

        return ValueOrError.success(new LinkedFunctionInvoke(function, List.copyOf(coercedArguments), currentType));
    }

    private ValueOrError<LinkedExpression> resolveFunctionInvoke(
            FunctionInvoke functionInvoke,
            Scope scope,
            LinkedExpression function
    ) {
        if (!(function.type() instanceof LinkedFunctionType functionType)) {
            return withPosition(ValueOrError.error("Expression is not callable, was `" + function.type() + "`"), functionInvoke.position());
        }
        if (functionInvoke.arguments().isEmpty()) {
            return withPosition(
                    ValueOrError.error("Callable expression requires at least one argument"),
                    functionInvoke.position()
            );
        }
        var currentType = (LinkedType) functionType;
        var coercedArguments = new java.util.ArrayList<LinkedExpression>(functionInvoke.arguments().size());
        for (var argument : functionInvoke.arguments()) {
            if (!(currentType instanceof LinkedFunctionType currentFunctionType)) {
                return withPosition(
                        ValueOrError.error("Callable expression invoked with too many arguments"),
                        functionInvoke.position()
                );
            }
            var linked = linkArgumentForExpectedType(argument, scope, currentFunctionType.argumentType());
            if (linked instanceof ValueOrError.Error<CoercedArgument> error) {
                return withPosition(new ValueOrError.Error<>(error.errors()), argument.position());
            }
            var coerced = ((ValueOrError.Value<CoercedArgument>) linked).value();
            coercedArguments.add(coerced.expression());
            currentType = currentFunctionType.returnType();
        }

        return ValueOrError.success(new LinkedFunctionInvoke(function, List.copyOf(coercedArguments), currentType));
    }

    private ValueOrError<CoercedArguments> linkArgumentsForExpectedTypes(
            List<Expression> arguments,
            Scope scope,
            List<LinkedType> expectedTypes
    ) {
        var coerced = new java.util.ArrayList<LinkedExpression>(arguments.size());
        var coercions = 0;
        for (var i = 0; i < arguments.size(); i++) {
            var argument = arguments.get(i);
            var expected = expectedTypes.get(i);
            var maybeCoerced = linkArgumentForExpectedType(argument, scope, expected);
            if (maybeCoerced instanceof ValueOrError.Error<CoercedArgument>) {
                return ValueOrError.error(List.of());
            }
            var value = ((ValueOrError.Value<CoercedArgument>) maybeCoerced).value();
            coerced.add(value.expression());
            coercions += value.coercions();
        }
        return ValueOrError.success(new CoercedArguments(List.copyOf(coerced), coercions));
    }

    private ValueOrError<CoercedArgument> linkArgumentForExpectedType(Expression argument, Scope scope, LinkedType expected) {
        if (argument instanceof TupleExpression tupleExpression
            && expected instanceof LinkedTupleType expectedTupleType) {
            if (tupleExpression.values().size() != expectedTupleType.elementTypes().size()) {
                return withPosition(
                        ValueOrError.error("Expected `" + expected + "`, got tuple with "
                                           + tupleExpression.values().size() + " element(s)"),
                        argument.position()
                );
            }
            var linkedElements = new java.util.ArrayList<LinkedExpression>(tupleExpression.values().size());
            var coercions = 0;
            for (var i = 0; i < tupleExpression.values().size(); i++) {
                var elementExpression = tupleExpression.values().get(i);
                var elementExpectedType = expectedTupleType.elementTypes().get(i);
                var maybeLinkedElement = linkArgumentForExpectedType(elementExpression, scope, elementExpectedType);
                if (maybeLinkedElement instanceof ValueOrError.Error<CoercedArgument>) {
                    return (ValueOrError<CoercedArgument>) maybeLinkedElement;
                }
                var linkedElement = ((ValueOrError.Value<CoercedArgument>) maybeLinkedElement).value();
                linkedElements.add(linkedElement.expression());
                coercions += linkedElement.coercions();
            }
            return ValueOrError.success(new CoercedArgument(
                    new LinkedTupleExpression(
                            List.copyOf(linkedElements),
                            new LinkedTupleType(
                                    linkedElements.stream()
                                            .map(LinkedExpression::type)
                                            .toList()
                            )
                    ),
                    coercions
            ));
        }
        if (argument instanceof LambdaExpression lambdaExpression) {
            if (expected instanceof LinkedFunctionType functionType) {
                return linkLambdaExpression(lambdaExpression, scope, functionType)
                        .map(linkedLambda -> new CoercedArgument(linkedLambda, 0));
            }
            return withPosition(
                    ValueOrError.error("Lambda expression can only be used where function type is expected"),
                    argument.position()
            );
        }
        if (argument instanceof FunctionReference functionReference) {
            if (expected instanceof LinkedFunctionType functionType) {
                return linkFunctionReference(functionReference, functionType)
                        .map(linkedReference -> new CoercedArgument(linkedReference, 0));
            }
            return withPosition(
                    ValueOrError.error("Function reference can only be used where function type is expected"),
                    argument.position()
            );
        }

        return linkExpression(argument, scope)
                .flatMap(linkedArgument -> {
                    var maybeCoerced = coerceArgument(linkedArgument, expected);
                    if (maybeCoerced == null) {
                        return withPosition(
                                ValueOrError.error("Expected `" + expected + "`, got `" + linkedArgument.type() + "`"),
                                argument.position()
                        );
                    }
                    return ValueOrError.success(maybeCoerced);
                });
    }

    private ValueOrError<LinkedExpression> linkFunctionReference(FunctionReference functionReference, LinkedFunctionType expectedType) {
        var expectedShape = flattenFunctionType(expectedType);
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .filter(signature -> signature.parameterTypes().size() == expectedShape.parameterTypes().size())
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    ValueOrError.error("Function `" + functionReference.name() + "` with "
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
                    ValueOrError.error("Function reference `" + functionReference.name() + "` is not compatible with `" + expectedType + "`"),
                    functionReference.position()
            );
        }
        return ValueOrError.success(best.expression());
    }

    private ValueOrError<LinkedExpression> linkFunctionReference(FunctionReference functionReference, Scope scope) {
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    ValueOrError.error("Function `" + functionReference.name() + "` not found"),
                    functionReference.position()
            );
        }
        if (candidates.size() > 1) {
            return withPosition(
                    ValueOrError.error("Function reference `" + functionReference.name() + "` is ambiguous. Add expected function type."),
                    functionReference.position()
            );
        }
        return ValueOrError.success(toFunctionReferenceLambda(candidates.getFirst()));
    }

    private LinkedExpression toFunctionReferenceLambda(FunctionSignature candidate) {
        var argumentNames = new java.util.ArrayList<String>(candidate.parameterTypes().size());
        var callArguments = new java.util.ArrayList<LinkedExpression>(candidate.parameterTypes().size());
        for (int i = 0; i < candidate.parameterTypes().size(); i++) {
            var argumentName = "arg" + i;
            argumentNames.add(argumentName);
            callArguments.add(new LinkedVariable(argumentName, candidate.parameterTypes().get(i)));
        }

        LinkedExpression expression = new LinkedFunctionCall(candidate.name(), List.copyOf(callArguments), candidate.returnType());
        var nestedType = candidate.returnType();
        for (int i = argumentNames.size() - 1; i >= 0; i--) {
            var functionType = new LinkedFunctionType(candidate.parameterTypes().get(i), nestedType);
            expression = new LinkedLambdaExpression(argumentNames.get(i), expression, functionType);
            nestedType = functionType;
        }
        return expression;
    }

    private ResolvedFunctionReference resolveFunctionReferenceCandidate(FunctionSignature candidate, FunctionShape expectedShape) {
        var argumentNames = new java.util.ArrayList<String>(expectedShape.parameterTypes().size());
        var callArguments = new java.util.ArrayList<LinkedExpression>(expectedShape.parameterTypes().size());
        var coercions = 0;

        for (int i = 0; i < expectedShape.parameterTypes().size(); i++) {
            var argumentName = "arg" + i;
            argumentNames.add(argumentName);
            var argumentVariable = new LinkedVariable(argumentName, expectedShape.parameterTypes().get(i));
            var coerced = coerceArgument(argumentVariable, candidate.parameterTypes().get(i));
            if (coerced == null) {
                return null;
            }
            callArguments.add(coerced.expression());
            coercions += coerced.coercions();
        }

        LinkedExpression expression = new LinkedFunctionCall(candidate.name(), List.copyOf(callArguments), candidate.returnType());
        var returnCoerced = coerceArgument(expression, expectedShape.returnType());
        if (returnCoerced == null) {
            if (candidate.returnType() != ANY) {
                return null;
            }
            // First linking pass can expose unknown return type (ANY); keep expected return so relinking can refine it.
            expression = new LinkedFunctionCall(candidate.name(), List.copyOf(callArguments), expectedShape.returnType());
            coercions += 1;
        } else {
            expression = returnCoerced.expression();
            coercions += returnCoerced.coercions();
        }

        var nestedType = expectedShape.returnType();
        for (int i = argumentNames.size() - 1; i >= 0; i--) {
            var functionType = new LinkedFunctionType(expectedShape.parameterTypes().get(i), nestedType);
            expression = new LinkedLambdaExpression(argumentNames.get(i), expression, functionType);
            nestedType = functionType;
        }

        return new ResolvedFunctionReference(expression, coercions);
    }

    private ValueOrError<LinkedLambdaExpression> linkLambdaExpression(
            LambdaExpression lambdaExpression,
            Scope scope,
            LinkedFunctionType expectedType
    ) {
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.isEmpty()) {
            return withPosition(ValueOrError.error("Lambda has to have at least one argument"), lambdaExpression.position());
        }

        var argumentTypes = new java.util.ArrayList<LinkedType>(argumentNames.size());
        var returnType = expectedReturnTypeForLambda(expectedType, argumentNames.size())
                .orElse(null);
        if (returnType == null) {
            return withPosition(
                    ValueOrError.error("Lambda expects " + argumentNames.size() + " argument(s), but target function type is `" + expectedType + "`"),
                    lambdaExpression.position()
            );
        }
        var currentType = (LinkedType) expectedType;
        for (int idx = 0; idx < argumentNames.size(); idx++) {
            var currentFunctionType = (LinkedFunctionType) currentType;
            argumentTypes.add(currentFunctionType.argumentType());
            currentType = currentFunctionType.returnType();
        }

        var lambdaScope = scope;
        for (int idx = 0; idx < argumentNames.size(); idx++) {
            lambdaScope = lambdaScope.add(argumentNames.get(idx), argumentTypes.get(idx));
        }

        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .flatMap(linkedBody -> {
                    var maybeCoerced = coerceArgument(linkedBody, returnType);
                    if (maybeCoerced == null) {
                        return withPosition(
                                ValueOrError.error(
                                        "Lambda has to return `" + returnType
                                        + "`, got `" + linkedBody.type() + "`"
                                ),
                                lambdaExpression.position()
                        );
                    }

                    LinkedExpression nested = maybeCoerced.expression();
                    LinkedType nestedType = returnType;
                    if (returnType instanceof LinkedGenericTypeParameter
                        && !(nested.type() instanceof LinkedGenericTypeParameter)) {
                        nestedType = nested.type();
                    }
                    for (int idx = argumentNames.size() - 1; idx >= 0; idx--) {
                        var functionType = new LinkedFunctionType(argumentTypes.get(idx), nestedType);
                        nested = new LinkedLambdaExpression(argumentNames.get(idx), nested, functionType);
                        nestedType = functionType;
                    }
                    return ValueOrError.success((LinkedLambdaExpression) nested);
                });
    }

    private Optional<LinkedType> expectedReturnTypeForLambda(LinkedFunctionType expectedType, int argumentCount) {
        LinkedType current = expectedType;
        for (int idx = 0; idx < argumentCount; idx++) {
            if (!(current instanceof LinkedFunctionType currentFunctionType)) {
                return Optional.empty();
            }
            current = currentFunctionType.returnType();
        }
        return Optional.of(current);
    }

    private static FunctionShape flattenFunctionType(LinkedFunctionType functionType) {
        var parameterTypes = new java.util.ArrayList<LinkedType>();
        LinkedType current = functionType;
        while (current instanceof LinkedFunctionType linkedFunctionType) {
            parameterTypes.add(linkedFunctionType.argumentType());
            current = linkedFunctionType.returnType();
        }
        return new FunctionShape(List.copyOf(parameterTypes), current);
    }

    private CoercedArgument coerceArgument(LinkedExpression argument, LinkedType expected) {
        if (argument.type().equals(expected)) {
            return new CoercedArgument(argument, 0);
        }
        if (expected instanceof LinkedGenericTypeParameter) {
            return new CoercedArgument(argument, 1);
        }
        if (argument.type() instanceof LinkedGenericTypeParameter) {
            return new CoercedArgument(argument, 1);
        }
        if (expected == ANY) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof LinkedTupleType expectedTuple
            && argument.type() instanceof LinkedTupleType argumentTuple
            && areTupleTypesCompatible(argumentTuple, expectedTuple)) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof LinkedFunctionType expectedFunction
            && argument.type() instanceof LinkedFunctionType argumentFunction
            && areFunctionTypesCompatible(argumentFunction, expectedFunction)) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof LinkedList expectedList
            && argument instanceof LinkedNewList linkedNewList
            && linkedNewList.values().isEmpty()) {
            return new CoercedArgument(new LinkedNewList(List.of(), new LinkedList(expectedList.elementType())), 1);
        }
        if (expected instanceof LinkedSet expectedSet
            && argument instanceof LinkedNewSet linkedNewSet
            && linkedNewSet.values().isEmpty()) {
            return new CoercedArgument(new LinkedNewSet(List.of(), new LinkedSet(expectedSet.elementType())), 1);
        }
        if (expected instanceof LinkedDict expectedDict
            && argument instanceof LinkedNewSet linkedNewSet
            && linkedNewSet.values().isEmpty()) {
            return new CoercedArgument(new LinkedNewDict(List.of(), new LinkedDict(expectedDict.valueType())), 1);
        }
        if (expected instanceof LinkedDict expectedDict
            && argument instanceof LinkedNewDict linkedNewDict
            && linkedNewDict.entries().isEmpty()) {
            return new CoercedArgument(new LinkedNewDict(List.of(), new LinkedDict(expectedDict.valueType())), 1);
        }
        if (argument.type() == NOTHING) {
            return new CoercedArgument(argument, 0);
        }
        if (expected == PrimitiveLinkedType.DATA
            && argument.type() instanceof GenericDataType) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof LinkedDataParentType expectedParent
            && argument.type() instanceof LinkedDataParentType argumentParent
            && sameRawTypeName(expectedParent.name(), argumentParent.name())) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof LinkedDataType expectedData
            && argument.type() instanceof LinkedDataType argumentData
            && sameRawTypeName(expectedData.name(), argumentData.name())) {
            return new CoercedArgument(argument, 1);
        }
        if (argument.type() == ANY) {
            return new CoercedArgument(argument, 1);
        }
        if (expected instanceof LinkedDataType expectedDataType
            && argument.type() instanceof LinkedDataType argumentDataType
            && isSubtype(argumentDataType, expectedDataType.name(), new java.util.HashSet<>())) {
            var assignments = expectedDataType.fields().stream()
                    .map(field -> new LinkedNewData.FieldAssignment(
                            field.name(),
                            new LinkedFieldAccess(argument, field.name(), field.type())
                    ))
                    .toList();
            return new CoercedArgument(new LinkedNewData(expectedDataType, assignments), 1);
        }
        if (expected instanceof LinkedDataParentType expectedParentType
            && argument.type() instanceof LinkedDataType argumentDataType
            && isSubtypeOfParent(argumentDataType, expectedParentType)) {
            return new CoercedArgument(argument, 1);
        }
        return null;
    }

    private boolean areTupleTypesCompatible(LinkedTupleType actual, LinkedTupleType expected) {
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

    private boolean areFunctionTypesCompatible(LinkedFunctionType actual, LinkedFunctionType expected) {
        return isTypeCompatible(actual.argumentType(), expected.argumentType())
               && isTypeCompatible(actual.returnType(), expected.returnType());
    }

    private boolean isTypeCompatible(LinkedType actual, LinkedType expected) {
        if (actual.equals(expected)) {
            return true;
        }
        if (expected == ANY || actual == ANY || actual == NOTHING) {
            return true;
        }
        if (expected instanceof LinkedGenericTypeParameter || actual instanceof LinkedGenericTypeParameter) {
            return true;
        }
        if (expected instanceof LinkedList expectedList && actual instanceof LinkedList actualList) {
            return isTypeCompatible(actualList.elementType(), expectedList.elementType());
        }
        if (expected instanceof LinkedSet expectedSet && actual instanceof LinkedSet actualSet) {
            return isTypeCompatible(actualSet.elementType(), expectedSet.elementType());
        }
        if (expected instanceof LinkedDict expectedDict && actual instanceof LinkedDict actualDict) {
            return isTypeCompatible(actualDict.valueType(), expectedDict.valueType());
        }
        if (expected instanceof LinkedTupleType expectedTuple && actual instanceof LinkedTupleType actualTuple) {
            return areTupleTypesCompatible(actualTuple, expectedTuple);
        }
        if (expected instanceof LinkedFunctionType expectedFunction && actual instanceof LinkedFunctionType actualFunction) {
            return areFunctionTypesCompatible(actualFunction, expectedFunction);
        }
        return false;
    }

    private boolean isSubtype(LinkedDataType candidate, String expectedTypeName, java.util.Set<String> visited) {
        if (!visited.add(candidate.name())) {
            return false;
        }
        if (candidate.extendedTypes().contains(expectedTypeName)) {
            return true;
        }
        return candidate.extendedTypes().stream()
                .map(dataTypes::get)
                .filter(LinkedDataType.class::isInstance)
                .map(LinkedDataType.class::cast)
                .anyMatch(parent -> parent.name().equals(expectedTypeName) || isSubtype(parent, expectedTypeName, visited));
    }

    private boolean isSubtypeOfParent(LinkedDataType candidate, LinkedDataParentType expectedParentType) {
        if (expectedParentType.subTypes().stream()
                .anyMatch(subType -> sameRawTypeName(subType.name(), candidate.name()))) {
            return true;
        }
        if (!expectedParentType.subTypes().isEmpty()) {
            return false;
        }
        return dataTypes.values().stream()
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
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

    private ValueOrError<LinkedExpression> linkIfExpression(IfExpression ifExpression, Scope scope) {
        return linkExpression(ifExpression.condition(), scope)
                .flatMap(c -> {
                    if (!isBooleanConvertibleType(c.type())) {
                        return withPosition(
                                ValueOrError.error("condition in if statement has to have type `" + BOOL + "`, was `" + c.type() + "`"),
                                ifExpression.condition().position()
                        );
                    }
                    return linkExpression(ifExpression.thenBranch(), scope)
                            .flatMap(t ->
                                    linkExpression(ifExpression.elseBranch(), scope)
                                            .map(e -> new LinkedIfExpression(c, t, e, findHigherType(t.type(), e.type()))));
                });
    }

    private ValueOrError<LinkedExpression> linkInfixExpression(InfixExpression expression, Scope scope) {
        var normalizedExpression = normalizePipeAssociativity(expression);
        if (normalizedExpression != expression) {
            return linkInfixExpression(normalizedExpression, scope);
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
                            if (methodCall instanceof ValueOrError.Value<LinkedExpression> value) {
                                return value;
                            }
                            if (methodCall instanceof ValueOrError.Error<LinkedExpression> error
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
                                                .map(linked -> (LinkedExpression) linked));
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
                            if (methodCall instanceof ValueOrError.Value<LinkedExpression> value) {
                                return value;
                            }
                            if (methodCall instanceof ValueOrError.Error<LinkedExpression> error
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
                            if (methodCall instanceof ValueOrError.Value<LinkedExpression> value) {
                                return value;
                            }
                            if (methodCall instanceof ValueOrError.Error<LinkedExpression> error
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
                                        if (methodCall instanceof ValueOrError.Value<LinkedExpression> value) {
                                            return value;
                                        }
                                    }
                                    return getLinkedInfixExpression(left, expression.operator(), right, expression.position())
                                            .map(linked -> (LinkedExpression) linked);
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

    private ValueOrError<LinkedExpression> resolveMethodInfixCall(
            String operatorSymbol,
            LinkedExpression left,
            LinkedExpression right,
            Optional<pl.grzeslowski.capybara.parser.SourcePosition> position
    ) {
        var ownerNames = methodOwnerCandidates(left.type());
        var candidates = functionSignatures.stream()
                .filter(signature -> matchesMethodOwner(signature.name(), ownerNames, operatorSymbol))
                .filter(signature -> signature.parameterTypes().size() == 2)
                .toList();
        if (candidates.isEmpty()) {
            return ValueOrError.error(List.of());
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var receiverParameterType = candidate.parameterTypes().get(0);
            var maybeReceiver = coerceArgument(left, receiverParameterType);
            if (maybeReceiver == null) {
                continue;
            }
            var substitutions = new java.util.LinkedHashMap<String, LinkedType>();
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
                    ValueOrError.error(
                            "No matching method `" + operatorSymbol + "` for argument types ["
                            + left.type().name() + ", " + right.type().name() + "]"
                    ),
                    position
            );
        }
        return ValueOrError.success(new LinkedFunctionCall(
                best.signature().name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private ValueOrError<LinkedExpression> resolveMethodInfixCall(
            String operatorSymbol,
            LinkedExpression left,
            Expression right,
            Scope scope,
            Optional<pl.grzeslowski.capybara.parser.SourcePosition> position
    ) {
        var ownerNames = methodOwnerCandidates(left.type());
        var candidates = functionSignatures.stream()
                .filter(signature -> matchesMethodOwner(signature.name(), ownerNames, operatorSymbol))
                .filter(signature -> signature.parameterTypes().size() == 2)
                .toList();
        if (candidates.isEmpty()) {
            return ValueOrError.error(List.of());
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var receiverParameterType = candidate.parameterTypes().get(0);
            var maybeReceiver = coerceArgument(left, receiverParameterType);
            if (maybeReceiver == null) {
                continue;
            }
            var substitutions = new java.util.LinkedHashMap<String, LinkedType>();
            collectTypeSubstitutions(receiverParameterType, maybeReceiver.expression().type(), substitutions);
            var expectedRightType = substituteTypeParameters(candidate.parameterTypes().get(1), substitutions);
            var maybeArgument = linkArgumentForExpectedType(right, scope, expectedRightType);
            if (maybeArgument instanceof ValueOrError.Error<CoercedArgument>) {
                continue;
            }
            var coercedArgument = ((ValueOrError.Value<CoercedArgument>) maybeArgument).value();
            var arguments = List.of(maybeReceiver.expression(), coercedArgument.expression());
            var coercions = maybeReceiver.coercions() + coercedArgument.coercions();
            if (best == null || coercions < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, arguments, coercions);
            }
        }
        if (best == null) {
            return withPosition(
                    ValueOrError.error(
                            "No matching method `" + operatorSymbol + "` for argument types ["
                            + left.type().name() + ", " + right + "]"
                    ),
                    position
            );
        }
        return ValueOrError.success(new LinkedFunctionCall(
                best.signature().name(),
                best.arguments(),
                resolveReturnType(best.signature(), best.arguments())
        ));
    }

    private Set<String> methodOwnerCandidates(LinkedType receiverType) {
        var ownerNames = new LinkedHashSet<String>();
        var receiverBase = baseTypeName(receiverType.name());
        var receiverSimple = simpleTypeName(receiverBase);
        ownerNames.add(receiverBase);
        dataTypes.keySet().stream()
                .map(this::baseTypeName)
                .filter(name -> simpleTypeName(name).equals(receiverSimple))
                .forEach(ownerNames::add);
        if (receiverType instanceof LinkedDataType receiverDataType) {
            var receiverDataSimple = simpleTypeName(baseTypeName(receiverDataType.name()));
            dataTypes.values().stream()
                    .filter(LinkedDataParentType.class::isInstance)
                    .map(LinkedDataParentType.class::cast)
                    .filter(parentType -> parentType.subTypes().stream()
                            .anyMatch(subType -> simpleTypeName(baseTypeName(subType.name())).equals(receiverDataSimple)))
                    .map(LinkedDataParentType::name)
                    .map(this::baseTypeName)
                    .forEach(ownerNames::add);
            dataTypes.entrySet().stream()
                    .filter(entry -> entry.getValue() instanceof LinkedDataParentType parentType
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

    private ValueOrError<LinkedExpression> linkPipeExpression(InfixExpression expression, Scope scope) {
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
                        case LinkedList linkedList -> linkedList.elementType();
                        case LinkedSet linkedSet -> linkedSet.elementType();
                        case LinkedDict linkedDict -> linkedDict.valueType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (elementType == null) {
                        return linkScalarPipeExpression(expression, scope, left);
                    }
                    return linkCollectionPipeExpression(expression, scope, left, elementType);
                });
    }

    private ValueOrError<LinkedExpression> linkOptionPipeExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left,
            LinkedType elementType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, elementType)
                        .map(linked -> (LinkedExpression) new LinkedPipeExpression(
                                left,
                                linked.argumentName(),
                                linked.expression(),
                                left.type()
                        ));
            }
            return withPosition(
                    ValueOrError.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                .orElse(null);
        if (lambdaArgumentName == null) {
            return withPosition(
                    ValueOrError.error("Right side lambda of `|` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = scope.add(lambdaArgumentName, elementType);
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                        left,
                        lambdaArgumentName,
                        mapper,
                        left.type()
                ));
    }

    private ValueOrError<LinkedExpression> linkCollectionPipeExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left,
            LinkedType elementType
    ) {
        if (left.type() instanceof LinkedDict dictType) {
            return linkDictPipeExpression(expression, scope, left, dictType);
        }
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, elementType)
                        .map(linked -> (LinkedExpression) new LinkedPipeExpression(
                                left,
                                linked.argumentName(),
                                linked.expression(),
                                left.type() instanceof LinkedSet
                                        ? new LinkedSet(linked.expression().type())
                                        : new LinkedList(linked.expression().type())
                        ));
            }
            return withPosition(
                    ValueOrError.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                .orElse(null);
        if (lambdaArgumentName == null) {
            return withPosition(
                    ValueOrError.error("Right side lambda of `|` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = scope.add(lambdaArgumentName, elementType);
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                        left,
                        lambdaArgumentName,
                        mapper,
                        left.type() instanceof LinkedSet
                                ? new LinkedSet(mapper.type())
                                : new LinkedList(mapper.type())
                ));
    }

    private ValueOrError<LinkedExpression> linkDictPipeExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left,
            LinkedDict dictType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, dictType.valueType())
                        .map(linked -> (LinkedExpression) new LinkedPipeExpression(
                                left,
                                linked.argumentName(),
                                linked.expression(),
                                new LinkedDict(linked.expression().type())
                        ));
            }
            return withPosition(
                    ValueOrError.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var valueName = argumentNames.get(0);
            var lambdaScope = scope.add(valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                            left,
                            valueName,
                            mapper,
                            new LinkedDict(mapper.type())
                    ));
        }
        if (argumentNames.size() == 2) {
            var keyName = argumentNames.get(0);
            var valueName = argumentNames.get(1);
            var lambdaScope = scope
                    .add(keyName, STRING)
                    .add(valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                            left,
                            encodeDictPipeArguments(keyName, valueName),
                            mapper,
                            new LinkedDict(mapper.type())
                    ));
        }
        return withPosition(
                ValueOrError.error("Right side lambda of `|` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private ValueOrError<LinkedExpression> linkScalarPipeExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, left.type())
                        .map(linked -> (LinkedExpression) new LinkedLetExpression(
                                linked.argumentName(),
                                left,
                                linked.expression()
                        ));
            }
            return withPosition(
                    ValueOrError.error("Right side of `|` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                .orElse(null);
        if (lambdaArgumentName == null) {
            return withPosition(
                    ValueOrError.error("Right side lambda of `|` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = scope.add(lambdaArgumentName, left.type());
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .map(mapper -> (LinkedExpression) new LinkedLetExpression(
                        lambdaArgumentName,
                        left,
                        mapper
                ));
    }

    private ValueOrError<PipeMapper> resolvePipeFunctionReference(FunctionReference functionReference, LinkedType inputType) {
        var argumentName = "it";
        var argument = new LinkedVariable(argumentName, inputType);
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .filter(signature -> signature.parameterTypes().size() == 1)
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    ValueOrError.error("Function `" + functionReference.name() + "` with one argument not found"),
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
                    ValueOrError.error("Function reference `" + functionReference.name() + "` is not compatible with `" + inputType + "`"),
                    functionReference.position()
            );
        }
        return ValueOrError.success(new PipeMapper(
                argumentName,
                new LinkedFunctionCall(
                        functionReference.name(),
                        best.arguments(),
                        resolveReturnType(best.signature(), best.arguments())
                )
        ));
    }

    private ValueOrError<LinkedExpression> linkPipeReduceExpression(InfixExpression expression, Scope scope) {
        var reAssociated = reAssociatePipeChain(expression);
        if (reAssociated != expression) {
            return linkInfixExpression(reAssociated, scope);
        }
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = left.type() == STRING ? STRING : collectionElementType(left.type());
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `|>` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (left.type() instanceof LinkedDict dictType
                        && expression.right() instanceof LambdaExpression lambdaExpression) {
                        var argumentNames = lambdaExpression.argumentNames();
                        if (argumentNames.size() == 1) {
                            var valueName = argumentNames.get(0);
                            var lambdaScope = scope.add(valueName, dictType.valueType());
                            return linkExpression(lambdaExpression.expression(), lambdaScope)
                                    .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                                            left,
                                            valueName,
                                            mapper,
                                            new LinkedSet(mapper.type())
                                    ));
                        }
                        if (argumentNames.size() == 2) {
                            var keyName = argumentNames.get(0);
                            var valueName = argumentNames.get(1);
                            var lambdaScope = scope
                                    .add(keyName, STRING)
                                    .add(valueName, dictType.valueType());
                            return linkExpression(lambdaExpression.expression(), lambdaScope)
                                    .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                                            left,
                                            encodeDictPipeArguments(keyName, valueName),
                                            mapper,
                                            new LinkedSet(mapper.type())
                                    ));
                        }
                        return withPosition(
                                ValueOrError.error("Right side lambda of `|>` for dict has to have one or two arguments"),
                                lambdaExpression.position()
                        );
                    }
                    if (!(expression.right() instanceof ReduceExpression reduceExpression)) {
                        return withPosition(
                                ValueOrError.error("Right side of `|>` has to be a reduce expression"),
                                expression.right().position()
                        );
                    }
                    return linkExpression(reduceExpression.initialValue(), scope)
                            .flatMap(initial -> {
                                var reduceScope = scope;
                                if (reduceExpression.accumulatorName().contains("::") && reduceExpression.keyName().isPresent()) {
                                    return withPosition(
                                            ValueOrError.error("Reducer with four arguments is not supported for `dict`. Use `|>` mapper and then a standard reduce."),
                                            reduceExpression.position()
                                    );
                                } else {
                                    reduceScope = reduceScope.add(reduceExpression.accumulatorName(), initial.type());
                                }
                                if (reduceExpression.keyName().isPresent()) {
                                    if (!(left.type() instanceof LinkedDict)) {
                                        return withPosition(
                                                ValueOrError.error("Reducer in `|>` with key argument can only be used for `dict`"),
                                                reduceExpression.position()
                                        );
                                    }
                                    reduceScope = reduceScope.add(reduceExpression.keyName().orElseThrow(), STRING);
                                }
                                reduceScope = reduceScope.add(reduceExpression.valueName(), elementType);
                                return linkExpression(reduceExpression.reducerExpression(), reduceScope)
                                        .flatMap(reducer -> {
                                            if (!reducer.type().equals(initial.type())) {
                                                return withPosition(
                                                        ValueOrError.error(
                                                                "Reducer in `|>` has to return `" + initial.type() + "`, was `" + reducer.type() + "`"
                                                        ),
                                                        reduceExpression.position()
                                                );
                                            }
                                            return ValueOrError.success((LinkedExpression) new LinkedPipeReduceExpression(
                                                    left,
                                                    initial,
                                                    reduceExpression.accumulatorName(),
                                                    reduceExpression.keyName(),
                                                    reduceExpression.valueName(),
                                                    reducer,
                                                    initial.type()
                                            ));
                                        });
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkPipeFlatMapExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = collectionElementType(left.type());
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `|*` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        return withPosition(
                                ValueOrError.error("Right side of `|*` has to be a lambda expression"),
                                expression.right().position()
                        );
                    }
                    var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                            .orElse(null);
                    if (lambdaArgumentName == null) {
                        return withPosition(
                                ValueOrError.error("Right side lambda of `|*` has to have exactly one argument"),
                                lambdaExpression.position()
                        );
                    }
                    var lambdaScope = scope.add(lambdaArgumentName, elementType);
                    return linkExpression(lambdaExpression.expression(), lambdaScope)
                            .flatMap(mapper -> {
                                var mappedElementType = collectionElementType(mapper.type());
                                if (mappedElementType == null) {
                                    return withPosition(
                                            ValueOrError.error("Lambda in `|*` has to return collection type, was `" + mapper.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return ValueOrError.success((LinkedExpression) new LinkedPipeFlatMapExpression(
                                        left,
                                        lambdaArgumentName,
                                        mapper,
                                        left.type() instanceof LinkedSet
                                                ? new LinkedSet(mappedElementType)
                                                : new LinkedList(mappedElementType)
                                ));
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkPipeFilterOutExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    if (left.type() instanceof LinkedDict dictType) {
                        return linkDictPipeFilterOutExpression(expression, scope, left, dictType);
                    }
                    if (isOptionType(left.type())) {
                        return linkOptionPipeFilterOutExpression(expression, scope, left, optionElementType(left), left.type());
                    }
                    var elementType = switch (left.type()) {
                        case LinkedList linkedList -> linkedList.elementType();
                        case LinkedSet linkedSet -> linkedSet.elementType();
                        case LinkedDict linkedDict -> linkedDict.valueType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (elementType == null) {
                        if (left.type() instanceof GenericDataType) {
                            var optionType = findOptionType();
                            if (optionType == null) {
                                return withPosition(
                                        ValueOrError.error("`|-` on data/type requires `Option` type to be available"),
                                        expression.left().position()
                                );
                            }
                            return linkOptionPipeFilterOutExpression(expression, scope, left, left.type(), optionType);
                        }
                        return withPosition(
                                ValueOrError.error("Left side of `|-` has to be a collection or data/type, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        if (expression.right() instanceof FunctionReference functionReference) {
                            return resolvePipeFunctionReference(functionReference, elementType)
                                    .flatMap(linked -> {
                                        if (linked.expression().type() != BOOL) {
                                            return withPosition(
                                                    ValueOrError.error("Function reference in `|-` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                                    functionReference.position()
                                            );
                                        }
                                        return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                                left,
                                                linked.argumentName(),
                                                linked.expression(),
                                                left.type() instanceof LinkedSet
                                                        ? new LinkedSet(elementType)
                                                        : new LinkedList(elementType)
                                        ));
                                    });
                        }
                        return withPosition(
                            ValueOrError.error("Right side of `|-` has to be a lambda expression or function reference"),
                            expression.right().position()
                        );
                    }
                    var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                            .orElse(null);
                    if (lambdaArgumentName == null) {
                        return withPosition(
                                ValueOrError.error("Right side lambda of `|-` has to have exactly one argument"),
                                lambdaExpression.position()
                        );
                    }
                    var lambdaScope = scope.add(lambdaArgumentName, elementType);
                    return linkExpression(lambdaExpression.expression(), lambdaScope)
                            .flatMap(predicate -> {
                                if (predicate.type() != BOOL) {
                                    return withPosition(
                                            ValueOrError.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                        left,
                                        lambdaArgumentName,
                                        predicate,
                                        left.type() instanceof LinkedSet
                                                ? new LinkedSet(elementType)
                                                : new LinkedList(elementType)
                                ));
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkPipeAnyAllExpression(
            InfixExpression expression,
            Scope scope,
            boolean any
    ) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    if (left.type() instanceof LinkedDict dictType) {
                        return linkDictPipeAnyAllExpression(expression, scope, left, dictType, any);
                    }
                    var elementType = switch (left.type()) {
                        case LinkedList linkedList -> linkedList.elementType();
                        case LinkedSet linkedSet -> linkedSet.elementType();
                        case PrimitiveLinkedType primitive when primitive == STRING -> STRING;
                        default -> null;
                    };
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `" + expression.operator().symbol() + "` has to be a collection or string, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        if (expression.right() instanceof FunctionReference functionReference) {
                            return resolvePipeFunctionReference(functionReference, elementType)
                                    .flatMap(linked -> {
                                        if (linked.expression().type() != BOOL) {
                                            return withPosition(
                                                    ValueOrError.error("Function reference in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                                    functionReference.position()
                                            );
                                        }
                                        return ValueOrError.success(
                                                any
                                                        ? (LinkedExpression) new LinkedPipeAnyExpression(left, linked.argumentName(), linked.expression(), BOOL)
                                                        : (LinkedExpression) new LinkedPipeAllExpression(left, linked.argumentName(), linked.expression(), BOOL)
                                        );
                                    });
                        }
                        return withPosition(
                                ValueOrError.error("Right side of `" + expression.operator().symbol() + "` has to be a lambda expression or function reference"),
                                expression.right().position()
                        );
                    }
                    var lambdaArgumentName = singleLambdaArgument(lambdaExpression).orElse(null);
                    if (lambdaArgumentName == null) {
                        return withPosition(
                                ValueOrError.error("Right side lambda of `" + expression.operator().symbol() + "` has to have exactly one argument"),
                                lambdaExpression.position()
                        );
                    }
                    var lambdaScope = scope.add(lambdaArgumentName, elementType);
                    return linkExpression(lambdaExpression.expression(), lambdaScope)
                            .flatMap(predicate -> {
                                if (predicate.type() != BOOL) {
                                    return withPosition(
                                            ValueOrError.error("Lambda in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + predicate.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return ValueOrError.success(
                                        any
                                                ? (LinkedExpression) new LinkedPipeAnyExpression(left, lambdaArgumentName, predicate, BOOL)
                                                : (LinkedExpression) new LinkedPipeAllExpression(left, lambdaArgumentName, predicate, BOOL)
                                );
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkDictPipeAnyAllExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left,
            LinkedDict dictType,
            boolean any
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, dictType.valueType())
                        .flatMap(linked -> {
                            if (linked.expression().type() != BOOL) {
                                return withPosition(
                                        ValueOrError.error("Function reference in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                        functionReference.position()
                                );
                            }
                            return ValueOrError.success(
                                    any
                                            ? (LinkedExpression) new LinkedPipeAnyExpression(left, linked.argumentName(), linked.expression(), BOOL)
                                            : (LinkedExpression) new LinkedPipeAllExpression(left, linked.argumentName(), linked.expression(), BOOL)
                            );
                        });
            }
            return withPosition(
                    ValueOrError.error("Right side of `" + expression.operator().symbol() + "` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var valueName = argumentNames.get(0);
            var lambdaScope = scope.add(valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    ValueOrError.error("Lambda in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        return ValueOrError.success(
                                any
                                        ? (LinkedExpression) new LinkedPipeAnyExpression(left, valueName, predicate, BOOL)
                                        : (LinkedExpression) new LinkedPipeAllExpression(left, valueName, predicate, BOOL)
                        );
                    });
        }
        if (argumentNames.size() == 2) {
            var keyName = argumentNames.get(0);
            var valueName = argumentNames.get(1);
            var lambdaScope = scope
                    .add(keyName, STRING)
                    .add(valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    ValueOrError.error("Lambda in `" + expression.operator().symbol() + "` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        var encodedArgName = encodeDictPipeArguments(keyName, valueName);
                        return ValueOrError.success(
                                any
                                        ? (LinkedExpression) new LinkedPipeAnyExpression(left, encodedArgName, predicate, BOOL)
                                        : (LinkedExpression) new LinkedPipeAllExpression(left, encodedArgName, predicate, BOOL)
                        );
                    });
        }
        return withPosition(
                ValueOrError.error("Right side lambda of `" + expression.operator().symbol() + "` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private ValueOrError<LinkedExpression> linkDictPipeFilterOutExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left,
            LinkedDict dictType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, dictType.valueType())
                        .flatMap(linked -> {
                            if (linked.expression().type() != BOOL) {
                                return withPosition(
                                        ValueOrError.error("Function reference in `|-` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                        functionReference.position()
                                );
                            }
                            return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                    left,
                                    linked.argumentName(),
                                    linked.expression(),
                                    new LinkedDict(dictType.valueType())
                            ));
                        });
            }
            return withPosition(
                    ValueOrError.error("Right side of `|-` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var argumentNames = lambdaExpression.argumentNames();
        if (argumentNames.size() == 1) {
            var valueName = argumentNames.get(0);
            var lambdaScope = scope.add(valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    ValueOrError.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                left,
                                valueName,
                                predicate,
                                new LinkedDict(dictType.valueType())
                        ));
                    });
        }
        if (argumentNames.size() == 2) {
            var keyName = argumentNames.get(0);
            var valueName = argumentNames.get(1);
            var lambdaScope = scope
                    .add(keyName, STRING)
                    .add(valueName, dictType.valueType());
            return linkExpression(lambdaExpression.expression(), lambdaScope)
                    .flatMap(predicate -> {
                        if (predicate.type() != BOOL) {
                            return withPosition(
                                    ValueOrError.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                    lambdaExpression.position()
                            );
                        }
                        return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                left,
                                encodeDictPipeArguments(keyName, valueName),
                                predicate,
                                new LinkedDict(dictType.valueType())
                        ));
                    });
        }
        return withPosition(
                ValueOrError.error("Right side lambda of `|-` for dict has to have one or two arguments"),
                lambdaExpression.position()
        );
    }

    private ValueOrError<LinkedExpression> linkOptionPipeFilterOutExpression(
            InfixExpression expression,
            Scope scope,
            LinkedExpression left,
            LinkedType elementType,
            LinkedType optionType
    ) {
        if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
            if (expression.right() instanceof FunctionReference functionReference) {
                return resolvePipeFunctionReference(functionReference, elementType)
                        .flatMap(linked -> {
                            if (linked.expression().type() != BOOL) {
                                return withPosition(
                                        ValueOrError.error("Lambda in `|-` has to return `BOOL`, was `" + linked.expression().type() + "`"),
                                        expression.right().position()
                                );
                            }
                            return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                    left,
                                    linked.argumentName(),
                                    linked.expression(),
                                    optionType
                            ));
                        });
            }
            return withPosition(
                    ValueOrError.error("Right side of `|-` has to be a lambda expression or function reference"),
                    expression.right().position()
            );
        }
        var lambdaArgumentName = singleLambdaArgument(lambdaExpression)
                .orElse(null);
        if (lambdaArgumentName == null) {
            return withPosition(
                    ValueOrError.error("Right side lambda of `|-` has to have exactly one argument"),
                    lambdaExpression.position()
            );
        }
        var lambdaScope = scope.add(lambdaArgumentName, elementType);
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .flatMap(predicate -> {
                    if (predicate.type() != BOOL) {
                        return withPosition(
                                ValueOrError.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                lambdaExpression.position()
                        );
                    }
                    return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                            left,
                            lambdaArgumentName,
                            predicate,
                            optionType
                    ));
                });
    }

    private static ValueOrError<LinkedInfixExpression> getLinkedInfixExpression(
            LinkedExpression left,
            InfixOperator operator,
            LinkedExpression right,
            Optional<pl.grzeslowski.capybara.parser.SourcePosition> position
    ) {
        LinkedType type = switch (operator) {
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
            return withPosition(ValueOrError.error("Cannot apply `" + op + "` to `" + left.type() + "` and `" + right.type() + "`"), position);
        }
        return ValueOrError.success(new LinkedInfixExpression(left, operator, right, type));
    }

    private static LinkedType findPlusType(LinkedType left, LinkedType right) {
        if (left == STRING && isDataLikeType(right)) {
            return STRING;
        }
        if (right == STRING && isDataLikeType(left)) {
            return STRING;
        }
        if (left instanceof LinkedList leftList) {
            if (right instanceof LinkedList rightList) {
                return new LinkedList(findHigherType(leftList.elementType(), rightList.elementType()));
            }
            return new LinkedList(findHigherType(leftList.elementType(), right));
        }
        if (left instanceof LinkedSet leftSet) {
            if (right instanceof LinkedSet rightSet) {
                return new LinkedSet(findHigherType(leftSet.elementType(), rightSet.elementType()));
            }
            return new LinkedSet(findHigherType(leftSet.elementType(), right));
        }
        if (left instanceof LinkedDict leftDict) {
            if (right instanceof LinkedDict rightDict) {
                return new LinkedDict(findHigherType(leftDict.valueType(), rightDict.valueType()));
            }
            return null;
        }
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findPlusPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return findHigherType(left, right);
    }

    private static LinkedType findMinusType(LinkedType left, LinkedType right) {
        if (left instanceof LinkedList leftList) {
            if (right instanceof LinkedList rightList) {
                return new LinkedList(findHigherType(leftList.elementType(), rightList.elementType()));
            }
            return new LinkedList(findHigherType(leftList.elementType(), right));
        }
        if (left instanceof LinkedSet leftSet) {
            if (right instanceof LinkedSet rightSet) {
                return new LinkedSet(findHigherType(leftSet.elementType(), rightSet.elementType()));
            }
            return new LinkedSet(findHigherType(leftSet.elementType(), right));
        }
        if (left instanceof LinkedDict leftDict) {
            if (right instanceof LinkedDict rightDict) {
                return new LinkedDict(findHigherType(leftDict.valueType(), rightDict.valueType()));
            }
            if (right == STRING) {
                return new LinkedDict(leftDict.valueType());
            }
            return null;
        }
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findMathPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return findMathType(left, right);
    }

    private static LinkedType findMathType(LinkedType left, LinkedType right) {
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findMathPrimitiveType(leftPrimitive, rightPrimitive);
        }
        return findHigherType(left, right);
    }

    private static LinkedType findBitwiseType(LinkedType left, LinkedType right) {
        if (left instanceof PrimitiveLinkedType leftPrimitive && right instanceof PrimitiveLinkedType rightPrimitive) {
            return findBitwisePrimitiveType(leftPrimitive, rightPrimitive);
        }
        return null;
    }

    private static LinkedType findBitwiseNotType(LinkedType left) {
        if (left instanceof PrimitiveLinkedType leftPrimitive) {
            return switch (leftPrimitive) {
                case INT -> leftPrimitive;
                default -> null;
            };
        }
        return null;
    }

    private static LinkedType findLogicalType(LinkedType left, LinkedType right) {
        if (isBooleanConvertibleType(left) && isBooleanConvertibleType(right)) {
            return BOOL;
        }
        return null;
    }

    private static boolean isBooleanConvertibleType(LinkedType type) {
        if (type == BOOL) {
            return true;
        }
        if (type == STRING) {
            return true;
        }
        if (type instanceof LinkedList || type instanceof LinkedSet || type instanceof LinkedDict) {
            return true;
        }
        return type instanceof PrimitiveLinkedType primitive && isNumericPrimitive(primitive);
    }

    private static LinkedType findPlusPrimitiveType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
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

    private static boolean isDataLikeType(LinkedType type) {
        return type == PrimitiveLinkedType.DATA || type instanceof GenericDataType;
    }

    private static LinkedType findMathPrimitiveType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
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

    private static LinkedType findBitwisePrimitiveType(PrimitiveLinkedType left, PrimitiveLinkedType right) {
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

    private static LinkedType findQuestionType(LinkedType left, LinkedType right) {
        if (left instanceof LinkedList leftList) {
            if (right instanceof LinkedList) {
                return null;
            }
            var elementType = findHigherType(leftList.elementType(), right);
            return elementType == ANY ? null : BOOL;
        }
        if (left instanceof LinkedSet leftSet) {
            if (right instanceof LinkedSet) {
                return null;
            }
            var elementType = findHigherType(leftSet.elementType(), right);
            return elementType == ANY ? null : BOOL;
        }
        if (left instanceof LinkedDict) {
            return right == STRING ? BOOL : null;
        }
        if (left == STRING) {
            return right == STRING ? BOOL : null;
        }
        return null;
    }

    private static LinkedType collectionElementType(LinkedType type) {
        return switch (type) {
            case LinkedList linkedList -> linkedList.elementType();
            case LinkedSet linkedSet -> linkedSet.elementType();
            case LinkedDict linkedDict -> linkedDict.valueType();
            case LinkedTupleType tupleType -> tupleType.elementTypes().isEmpty() ? ANY : tupleType.elementTypes().getFirst();
            default -> null;
        };
    }

    private LinkedType slicedType(
            LinkedType sourceType,
            Optional<LinkedExpression> start,
            Optional<LinkedExpression> end
    ) {
        if (!(sourceType instanceof LinkedTupleType tupleType)) {
            return sourceType;
        }
        var size = tupleType.elementTypes().size();
        var startIndex = start.map(this::intLiteralValue).map(idx -> normalizeTupleIndex(idx, size)).orElse(0);
        var endIndex = end.map(this::intLiteralValue).map(idx -> normalizeTupleIndex(idx, size)).orElse(size);
        var normalizedStart = Math.max(0, Math.min(size, startIndex));
        var normalizedEnd = Math.max(normalizedStart, Math.min(size, endIndex));
        return new LinkedTupleType(tupleType.elementTypes().subList(normalizedStart, normalizedEnd));
    }

    private ValueOrError<LinkedType> tupleElementType(
            LinkedTupleType tupleType,
            LinkedExpression index,
            Optional<pl.grzeslowski.capybara.parser.SourcePosition> position
    ) {
        var size = tupleType.elementTypes().size();
        var indexValue = intLiteralValue(index);
        var normalized = normalizeTupleIndex(indexValue, size);
        if (normalized < 0 || normalized >= size) {
            return withPosition(
                    ValueOrError.error("tuple index `" + indexValue + "` out of bounds for tuple size `" + size + "`"),
                    position
            );
        }
        return ValueOrError.success(tupleType.elementTypes().get(normalized));
    }

    private int intLiteralValue(LinkedExpression expression) {
        if (expression instanceof LinkedIntValue intValue) {
            return Integer.parseInt(intValue.intValue());
        }
        throw new IllegalStateException("Expected int literal expression, got: " + expression);
    }

    private int normalizeTupleIndex(int index, int size) {
        return index < 0 ? size + index : index;
    }

    private LinkedDataParentType findOptionType() {
        return dataTypes.entrySet().stream()
                .filter(entry -> isOptionTypeKey(entry.getKey()))
                .map(Map.Entry::getValue)
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .findFirst()
                .orElseGet(() -> dataTypes.values().stream()
                        .filter(LinkedDataParentType.class::isInstance)
                        .map(LinkedDataParentType.class::cast)
                        .filter(type -> "Option".equals(type.name()))
                        .findFirst()
                        .orElse(null));
    }

    private LinkedDataParentType optionTypeFor(LinkedType elementType) {
        var optionType = findOptionType();
        if (optionType == null) {
            return null;
        }
        var typeParameters = optionType.typeParameters().isEmpty()
                ? List.<String>of()
                : List.of(linkedTypeDescriptor(elementType));
        return new LinkedDataParentType(optionType.name(), optionType.fields(), optionType.subTypes(), typeParameters);
    }

    private LinkedDataParentType findResultType() {
        return dataTypes.entrySet().stream()
                .filter(entry -> isResultTypeKey(entry.getKey()))
                .map(Map.Entry::getValue)
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .findFirst()
                .orElseGet(() -> dataTypes.values().stream()
                        .filter(LinkedDataParentType.class::isInstance)
                        .map(LinkedDataParentType.class::cast)
                        .filter(type -> "Result".equals(type.name()))
                        .findFirst()
                        .orElse(null));
    }

    private LinkedDataParentType resultTypeFor(LinkedType elementType) {
        var resultType = findResultType();
        if (resultType == null) {
            return null;
        }
        var typeParameters = resultType.typeParameters().isEmpty()
                ? List.<String>of()
                : List.of(linkedTypeDescriptor(elementType));
        return new LinkedDataParentType(resultType.name(), resultType.fields(), resultType.subTypes(), typeParameters);
    }

    private boolean isOptionType(LinkedType type) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return false;
        }
        var normalized = normalizeTypeName(genericDataType.name());
        return "Option".equals(genericDataType.name())
               || normalized.endsWith("/Option.Option")
               || normalized.endsWith("/Option");
    }

    private LinkedType optionElementType(LinkedExpression expression) {
        if (expression instanceof LinkedPipeExpression pipeExpression && isOptionType(pipeExpression.type())) {
            return pipeExpression.mapper().type();
        }
        if (expression instanceof LinkedPipeFilterOutExpression filterOutExpression && isOptionType(filterOutExpression.type())) {
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

    private static String encodeDictPipeArguments(String keyName, String valueName) {
        return keyName + DICT_PIPE_ARGS_SEPARATOR + valueName;
    }

    private ValueOrError<LinkedExpression> linkIntValue(IntValue intValue, Scope scope) {
        try {
            var parsed = new BigInteger(intValue.intValue(), 10);
            if (parsed.compareTo(BigInteger.valueOf(Integer.MIN_VALUE)) < 0
                || parsed.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
                return withPosition(ValueOrError.error("Int literal out of range: `" + intValue.intValue() + "`"), intValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(ValueOrError.error("Invalid int literal: `" + intValue.intValue() + "`"), intValue.position());
        }
        return ValueOrError.success(new LinkedIntValue(intValue.intValue()));
    }

    private ValueOrError<LinkedExpression> linkLongValue(LongValue longValue, Scope scope) {
        try {
            var raw = longValue.longValue();
            var normalized = raw.endsWith("l") || raw.endsWith("L")
                    ? raw.substring(0, raw.length() - 1)
                    : raw;
            var parsed = new BigInteger(normalized, 10);
            if (parsed.compareTo(BigInteger.valueOf(Long.MIN_VALUE)) < 0
                || parsed.compareTo(BigInteger.valueOf(Long.MAX_VALUE)) > 0) {
                return withPosition(ValueOrError.error("Long literal out of range: `" + raw + "`"), longValue.position());
            }
        } catch (NumberFormatException e) {
            return withPosition(ValueOrError.error("Invalid long literal: `" + longValue.longValue() + "`"), longValue.position());
        }
        return ValueOrError.success(new LinkedLongValue(longValue.longValue()));
    }

    private ValueOrError<LinkedExpression> linkMatchExpression(MatchExpression matchExpression, Scope scope) {
        return linkExpression(matchExpression.matchWith(), scope)
                .flatMap(matchWith -> matchExpression.cases().stream()
                        .map(matchCase -> linkMatchCase(matchCase, matchWith, scope))
                        .collect(new ValueOrErrorCollectionCollector<>())
                        .flatMap(cases -> validateMatchExhaustiveness(matchExpression, matchWith.type(), cases)
                                .map(ignored -> {
                            var matchType = cases.stream()
                                    .map(LinkedMatchExpression.MatchCase::expression)
                                    .map(LinkedExpression::type)
                                    .reduce(CapybaraTypeFinder::findHigherType)
                                    .orElse(ANY);
                            return (LinkedExpression) new LinkedMatchExpression(matchWith, cases, matchType);
                        })));
    }

    private ValueOrError<Void> validateMatchExhaustiveness(
            MatchExpression matchExpression,
            LinkedType matchType,
            List<LinkedMatchExpression.MatchCase> cases
    ) {
        if (cases.stream().anyMatch(matchCase ->
                matchCase.pattern() instanceof LinkedMatchExpression.WildcardPattern
                || matchCase.pattern() instanceof LinkedMatchExpression.WildcardBindingPattern)) {
            return ValueOrError.success(null);
        }
        var requiredConstructors = requiredConstructorsForMatch(matchType);
        if (requiredConstructors.isEmpty()) {
            return withPosition(
                    ValueOrError.error("`match` is not exhaustive. Use wildcard `| _ -> ...`."),
                    matchExpression.position()
            );
        }
        var coveredConstructors = coveredConstructors(cases, requiredConstructors);
        var missing = requiredConstructors.stream()
                .filter(name -> !coveredConstructors.contains(name))
                .toList();
        if (missing.isEmpty()) {
            return ValueOrError.success(null);
        }
        var missingText = missing.stream()
                .map(name -> "`" + name + "`")
                .collect(java.util.stream.Collectors.joining(", "));
        return withPosition(
                ValueOrError.error("`match` is not exhaustive. Use wildcard `| _ -> ...` or add missing branches:" + missingText + "."),
                matchExpression.position()
        );
    }

    private java.util.LinkedHashSet<String> requiredConstructorsForMatch(LinkedType matchType) {
        if (matchType instanceof LinkedDataParentType parentType) {
            return parentType.subTypes().stream()
                    .map(LinkedDataType::name)
                    .collect(java.util.stream.Collectors.toCollection(java.util.LinkedHashSet::new));
        }
        if (matchType instanceof LinkedDataType dataType) {
            var single = new java.util.LinkedHashSet<String>();
            single.add(dataType.name());
            return single;
        }
        return new java.util.LinkedHashSet<>();
    }

    private java.util.LinkedHashSet<String> coveredConstructors(
            List<LinkedMatchExpression.MatchCase> cases,
            java.util.Set<String> requiredConstructors
    ) {
        var covered = new java.util.LinkedHashSet<String>();
        for (var matchCase : cases) {
            var pattern = matchCase.pattern();
            if (pattern instanceof LinkedMatchExpression.VariablePattern variablePattern) {
                if (requiredConstructors.contains(variablePattern.name())) {
                    covered.add(variablePattern.name());
                }
                continue;
            }
            if (pattern instanceof LinkedMatchExpression.ConstructorPattern constructorPattern) {
                covered.add(constructorPattern.constructorName());
                continue;
            }
            if (pattern instanceof LinkedMatchExpression.TypedPattern typedPattern) {
                if (typedPattern.type() instanceof LinkedDataType dataType) {
                    covered.add(dataType.name());
                } else if (typedPattern.type() instanceof LinkedDataParentType parentType) {
                    parentType.subTypes().stream()
                            .map(LinkedDataType::name)
                            .forEach(covered::add);
                }
            }
        }
        return covered;
    }

    private ValueOrError<LinkedMatchExpression.MatchCase> linkMatchCase(
            MatchExpression.MatchCase matchCase,
            LinkedExpression matchWith,
            Scope scope
    ) {
        return linkPattern(matchCase.pattern(), matchWith.type(), scope)
                .flatMap(patternAndScope -> {
                    var caseScope = patternAndScope.scope();
                    if (patternAndScope.pattern() instanceof LinkedMatchExpression.TypedPattern typedPattern
                        && matchWith instanceof LinkedVariable matchedVariable) {
                        // Flow typing: inside a typed match branch, treat the matched variable as narrowed too.
                        caseScope = caseScope.add(matchedVariable.name(), typedPattern.type());
                    }
                    return linkExpression(matchCase.expression(), caseScope)
                            .map(expression -> new LinkedMatchExpression.MatchCase(patternAndScope.pattern(), expression));
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

    private ValueOrError<PatternAndScope> linkPattern(MatchExpression.Pattern pattern, LinkedType matchType, Scope scope) {
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern -> validateLiteralPattern(intPattern, matchType, scope, PrimitiveLinkedType.INT);
            case MatchExpression.StringPattern stringPattern -> validateLiteralPattern(stringPattern, matchType, scope, PrimitiveLinkedType.STRING);
            case MatchExpression.BoolPattern boolPattern -> validateLiteralPattern(boolPattern, matchType, scope, PrimitiveLinkedType.BOOL);
            case MatchExpression.FloatPattern floatPattern -> validateLiteralPattern(floatPattern, matchType, scope, PrimitiveLinkedType.FLOAT);
            case MatchExpression.TypedPattern typedPattern -> linkTypedPattern(typedPattern, matchType, scope);
            case MatchExpression.VariablePattern variablePattern -> linkVariablePattern(variablePattern, matchType, scope);
            case MatchExpression.WildcardPattern wildcardPattern ->
                    ValueOrError.success(new PatternAndScope(LinkedMatchExpression.WildcardPattern.WILDCARD, scope));
            case MatchExpression.WildcardBindingPattern wildcardBindingPattern ->
                    ValueOrError.success(new PatternAndScope(
                            new LinkedMatchExpression.WildcardBindingPattern(wildcardBindingPattern.name()),
                            scope.add(wildcardBindingPattern.name(), matchType)
                    ));
            case MatchExpression.ConstructorPattern constructorPattern -> linkConstructorPattern(constructorPattern, matchType, scope);
        };
    }

    private ValueOrError<PatternAndScope> validateLiteralPattern(
            MatchExpression.Pattern pattern,
            LinkedType matchType,
            Scope scope,
            PrimitiveLinkedType expected
    ) {
        if (matchType != expected) {
            return ValueOrError.error("Cannot match `" + matchType + "` with literal of type `" + expected + "`");
        }
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.IntPattern(intPattern.value()), scope));
            case MatchExpression.StringPattern stringPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.StringPattern(stringPattern.value()), scope));
            case MatchExpression.BoolPattern boolPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.BoolPattern(boolPattern.value()), scope));
            case MatchExpression.FloatPattern floatPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.FloatPattern(floatPattern.value()), scope));
            default -> throw new IllegalStateException("Unexpected literal pattern: " + pattern);
        };
    }

    private ValueOrError<PatternAndScope> linkVariablePattern(
            MatchExpression.VariablePattern variablePattern,
            LinkedType matchType,
            Scope scope
    ) {
        if (matchType instanceof LinkedDataParentType parentType) {
            return findSubtype(variablePattern.name(), parentType)
                    .map(ignored -> new PatternAndScope(new LinkedMatchExpression.VariablePattern(variablePattern.name()), scope));
        }
        if (matchType instanceof LinkedDataType dataType && dataType.name().equals(variablePattern.name())) {
            return ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.VariablePattern(variablePattern.name()), scope));
        }
        return ValueOrError.error("Cannot match `" + matchType + "` with constructor `" + variablePattern.name() + "`");
    }

    private ValueOrError<PatternAndScope> linkTypedPattern(
            MatchExpression.TypedPattern typedPattern,
            LinkedType matchType,
            Scope scope
    ) {
        return linkType(typedPattern.type(), dataTypes)
                .flatMap(patternType -> {
                    if (!isTypedPatternCompatible(matchType, patternType)) {
                        return ValueOrError.error("Cannot match `" + matchType + "` with typed pattern `" + patternType + "`");
                    }
                    return ValueOrError.success(new PatternAndScope(
                            new LinkedMatchExpression.TypedPattern(patternType, typedPattern.name()),
                            scope.add(typedPattern.name(), patternType)
                    ));
                });
    }

    private boolean isTypedPatternCompatible(LinkedType matchType, LinkedType patternType) {
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
        if (patternType instanceof LinkedDataType patternDataType
            && matchType instanceof LinkedDataParentType parentType) {
            return parentType.subTypes().stream().anyMatch(subType -> subType.name().equals(patternDataType.name()));
        }
        if (patternType instanceof LinkedDataParentType patternParentType
            && matchType instanceof LinkedDataType matchDataType) {
            return isSubtypeOfParent(matchDataType, patternParentType);
        }
        return false;
    }

    private ValueOrError<PatternAndScope> linkConstructorPattern(
            MatchExpression.ConstructorPattern constructorPattern,
            LinkedType matchType,
            Scope scope
    ) {
        return findConstructorType(constructorPattern.constructorName(), matchType)
                .flatMap(constructorType -> {
                    var resolvedFields = resolveConstructorFields(constructorType, matchType);
                    if (resolvedFields.size() != constructorPattern.fieldPatterns().size()) {
                        return ValueOrError.error("Constructor `" + constructorPattern.constructorName() + "` expects "
                                                  + resolvedFields.size() + " argument(s), got "
                                                  + constructorPattern.fieldPatterns().size());
                    }
                    var updatedScope = scope;
                    var linkedFieldPatterns = new java.util.ArrayList<LinkedMatchExpression.Pattern>(constructorPattern.fieldPatterns().size());
                    for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
                        var fieldPattern = constructorPattern.fieldPatterns().get(i);
                        if (fieldPattern instanceof MatchExpression.VariablePattern variablePattern) {
                            updatedScope = updatedScope.add(variablePattern.name(), resolvedFields.get(i).type());
                            linkedFieldPatterns.add(new LinkedMatchExpression.VariablePattern(variablePattern.name()));
                            continue;
                        }
                        var linkedPatternAndScope = linkPattern(fieldPattern, resolvedFields.get(i).type(), updatedScope);
                        if (linkedPatternAndScope instanceof ValueOrError.Error<PatternAndScope> error) {
                            return new ValueOrError.Error<>(error.errors());
                        }
                        var patternAndScope = ((ValueOrError.Value<PatternAndScope>) linkedPatternAndScope).value();
                        linkedFieldPatterns.add(patternAndScope.pattern());
                        updatedScope = patternAndScope.scope();
                    }
                    return ValueOrError.success(new PatternAndScope(
                            new LinkedMatchExpression.ConstructorPattern(
                                    constructorPattern.constructorName(),
                                    List.copyOf(linkedFieldPatterns)
                            ),
                            updatedScope
                    ));
                });
    }

    private List<LinkedDataType.LinkedField> resolveConstructorFields(LinkedDataType constructorType, LinkedType matchType) {
        if (!(matchType instanceof LinkedDataParentType parentType)) {
            return constructorType.fields();
        }
        if (constructorType.typeParameters().isEmpty() || parentType.typeParameters().isEmpty()) {
            if (parentType.typeParameters().isEmpty()) {
                return constructorType.fields();
            }
        }
        var substitutions = new java.util.HashMap<String, LinkedType>();
        var constructorTypeParameters = constructorType.typeParameters().isEmpty()
                ? inferGenericParameterNames(constructorType.fields())
                : constructorType.typeParameters();
        var max = Math.min(constructorTypeParameters.size(), parentType.typeParameters().size());
        for (int i = 0; i < max; i++) {
            var typeParameterName = constructorTypeParameters.get(i);
            parseLinkedTypeDescriptor(parentType.typeParameters().get(i))
                    .ifPresent(type -> substitutions.put(typeParameterName, type));
        }
        if (substitutions.isEmpty()) {
            return constructorType.fields();
        }
        return constructorType.fields().stream()
                .map(field -> new LinkedDataType.LinkedField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                .toList();
    }

    private List<String> inferGenericParameterNames(List<LinkedDataType.LinkedField> fields) {
        var names = new java.util.LinkedHashSet<String>();
        fields.forEach(field -> collectGenericParameterNames(field.type(), names));
        return List.copyOf(names);
    }

    private void collectGenericParameterNames(LinkedType type, java.util.Set<String> names) {
        switch (type) {
            case LinkedGenericTypeParameter genericTypeParameter -> names.add(genericTypeParameter.name());
            case LinkedList linkedList -> collectGenericParameterNames(linkedList.elementType(), names);
            case LinkedSet linkedSet -> collectGenericParameterNames(linkedSet.elementType(), names);
            case LinkedDict linkedDict -> collectGenericParameterNames(linkedDict.valueType(), names);
            case LinkedFunctionType functionType -> {
                collectGenericParameterNames(functionType.argumentType(), names);
                collectGenericParameterNames(functionType.returnType(), names);
            }
            case LinkedTupleType linkedTupleType ->
                    linkedTupleType.elementTypes().forEach(elementType -> collectGenericParameterNames(elementType, names));
            default -> {
            }
        }
    }

    private LinkedType substituteTypeParameters(LinkedType type, Map<String, LinkedType> substitutions) {
        if (type instanceof LinkedGenericTypeParameter genericTypeParameter) {
            return substitutions.getOrDefault(genericTypeParameter.name(), type);
        }
        return switch (type) {
            case LinkedList linkedList -> new LinkedList(substituteTypeParameters(linkedList.elementType(), substitutions));
            case LinkedSet linkedSet -> new LinkedSet(substituteTypeParameters(linkedSet.elementType(), substitutions));
            case LinkedDict linkedDict -> new LinkedDict(substituteTypeParameters(linkedDict.valueType(), substitutions));
            case LinkedFunctionType functionType -> new LinkedFunctionType(
                    substituteTypeParameters(functionType.argumentType(), substitutions),
                    substituteTypeParameters(functionType.returnType(), substitutions)
            );
            case LinkedTupleType linkedTupleType -> new LinkedTupleType(
                    linkedTupleType.elementTypes().stream()
                            .map(elementType -> substituteTypeParameters(elementType, substitutions))
                            .toList()
            );
            default -> type;
        };
    }

    private Optional<LinkedType> parseLinkedTypeDescriptor(String descriptor) {
        var normalizedDescriptor = descriptor == null ? "" : descriptor.trim();
        if (normalizedDescriptor.isEmpty()) {
            return Optional.empty();
        }
        if (normalizedDescriptor.startsWith("list[") && normalizedDescriptor.endsWith("]")) {
            return parseLinkedTypeDescriptor(normalizedDescriptor.substring(5, normalizedDescriptor.length() - 1))
                    .map(LinkedList::new);
        }
        if (normalizedDescriptor.startsWith("set[") && normalizedDescriptor.endsWith("]")) {
            return parseLinkedTypeDescriptor(normalizedDescriptor.substring(4, normalizedDescriptor.length() - 1))
                    .map(LinkedSet::new);
        }
        if (normalizedDescriptor.startsWith("dict[") && normalizedDescriptor.endsWith("]")) {
            return parseLinkedTypeDescriptor(normalizedDescriptor.substring(5, normalizedDescriptor.length() - 1))
                    .map(LinkedDict::new);
        }
        if (normalizedDescriptor.startsWith("tuple[") && normalizedDescriptor.endsWith("]")) {
            var inner = normalizedDescriptor.substring(6, normalizedDescriptor.length() - 1);
            var elementTypes = splitTopLevelTypeDescriptors(inner).stream()
                    .map(this::parseLinkedTypeDescriptor)
                    .toList();
            if (elementTypes.stream().anyMatch(Optional::isEmpty)) {
                return Optional.empty();
            }
            return Optional.of(new LinkedTupleType(elementTypes.stream().map(Optional::orElseThrow).toList()));
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
                case LinkedDataParentType parentType -> new LinkedDataParentType(
                        parentType.name(),
                        parentType.fields(),
                        parentType.subTypes(),
                        typeArgumentDescriptors
                );
                case LinkedDataType dataType -> {
                    if (dataType.typeParameters().isEmpty()) {
                        yield new LinkedDataType(
                                dataType.name(),
                                dataType.fields(),
                                typeArgumentDescriptors,
                                dataType.extendedTypes(),
                                dataType.singleton()
                        );
                    }
                    var substitutions = new java.util.LinkedHashMap<String, LinkedType>();
                    var max = Math.min(dataType.typeParameters().size(), typeArguments.size());
                    for (var i = 0; i < max; i++) {
                        substitutions.put(dataType.typeParameters().get(i), typeArguments.get(i));
                    }
                    var substitutedFields = dataType.fields().stream()
                            .map(field -> new LinkedDataType.LinkedField(
                                    field.name(),
                                    substituteTypeParameters(field.type(), substitutions)
                            ))
                            .toList();
                    yield new LinkedDataType(
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
        var depth = 0;
        var current = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            var ch = text.charAt(i);
            if (ch == '[') {
                depth++;
                current.append(ch);
                continue;
            }
            if (ch == ']') {
                depth = Math.max(0, depth - 1);
                current.append(ch);
                continue;
            }
            if (ch == ',' && depth == 0) {
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

    private String linkedTypeDescriptor(LinkedType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase();
            case LinkedList linkedList -> "list[" + linkedTypeDescriptor(linkedList.elementType()) + "]";
            case LinkedSet linkedSet -> "set[" + linkedTypeDescriptor(linkedSet.elementType()) + "]";
            case LinkedDict linkedDict -> "dict[" + linkedTypeDescriptor(linkedDict.valueType()) + "]";
            case LinkedTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(this::linkedTypeDescriptor)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case LinkedFunctionType linkedFunctionType ->
                    "(" + linkedTypeDescriptor(linkedFunctionType.argumentType()) + " => " + linkedTypeDescriptor(linkedFunctionType.returnType()) + ")";
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? linkedDataType.name()
                    : linkedDataType.name() + "[" + String.join(", ", linkedDataType.typeParameters()) + "]";
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? linkedDataParentType.name()
                    : linkedDataParentType.name() + "[" + String.join(", ", linkedDataParentType.typeParameters()) + "]";
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private ValueOrError<LinkedDataType> findConstructorType(String constructorName, LinkedType matchType) {
        if (matchType instanceof LinkedDataType linkedDataType) {
            if (linkedDataType.name().equals(constructorName)) {
                return ValueOrError.success(linkedDataType);
            }
            return ValueOrError.error("Type `" + linkedDataType.name() + "` cannot match constructor `" + constructorName + "`");
        }
        if (matchType instanceof LinkedDataParentType parentType) {
            return findSubtype(constructorName, parentType);
        }
        return ValueOrError.error("Type `" + matchType + "` is not matchable with constructor `" + constructorName + "`");
    }

    private ValueOrError<LinkedDataType> findSubtype(String constructorName, LinkedDataParentType parentType) {
        return parentType.subTypes().stream()
                .filter(subType -> subType.name().equals(constructorName))
                .findFirst()
                .map(ValueOrError::success)
                .orElseGet(() -> ValueOrError.error("Constructor `" + constructorName + "` not found in type `" + parentType.name() + "`"));
    }

    private ValueOrError<LinkedExpression> linkNewListExpression(NewListExpression expression, Scope scope) {
        return expression.values().stream()
                .map(value -> linkExpression(value, scope))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(values -> {
                    var elementType = values.stream()
                            .map(LinkedExpression::type)
                            .reduce(CapybaraTypeFinder::findHigherType)
                            .orElse(ANY);
                    return (LinkedExpression) new LinkedNewList(values, new LinkedList(elementType));
                });
    }

    private ValueOrError<LinkedExpression> linkNewSetExpression(NewSetExpression expression, Scope scope) {
        return expression.values().stream()
                .map(value -> linkExpression(value, scope))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(values -> {
                    var elementType = values.stream()
                            .map(LinkedExpression::type)
                            .reduce(CapybaraTypeFinder::findHigherType)
                            .orElse(ANY);
                    return (LinkedExpression) new LinkedNewSet(values, new LinkedSet(elementType));
                });
    }

    private ValueOrError<LinkedExpression> linkNewDictExpression(NewDictExpression expression, Scope scope) {
        var entries = new java.util.ArrayList<LinkedNewDict.Entry>();
        for (var entry : expression.entries()) {
            var key = linkExpression(entry.key(), scope);
            if (key instanceof ValueOrError.Error<LinkedExpression> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            var value = linkExpression(entry.value(), scope);
            if (value instanceof ValueOrError.Error<LinkedExpression> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            var linkedEntry = new LinkedNewDict.Entry(
                    ((ValueOrError.Value<LinkedExpression>) key).value(),
                    ((ValueOrError.Value<LinkedExpression>) value).value()
            );
            if (linkedEntry.key().type() != STRING) {
                return withPosition(
                        ValueOrError.error("dict keys must be of type `STRING`"),
                        entry.key().position().or(() -> expression.position())
                );
            }
            entries.add(linkedEntry);
        }

        var valueType = entries.stream()
                .map(LinkedNewDict.Entry::value)
                .map(LinkedExpression::type)
                .reduce(CapybaraTypeFinder::findHigherType)
                .orElse(ANY);
        return ValueOrError.success((LinkedExpression) new LinkedNewDict(List.copyOf(entries), new LinkedDict(valueType)));
    }

    private ValueOrError<LinkedExpression> linkNewData(NewData newData, Scope scope) {
        return linkType(newData.type(), dataTypes)
                .flatMap(type -> linkSpreadAssignments(newData.spreads(), scope)
                        .flatMap(spreadAssignments ->
                                linkFieldAssignment(newData.assignments(), scope)
                                        .flatMap(assignments ->
                                                linkPositionalArguments(newData.positionalArguments(), scope)
                                        .flatMap(positionalArguments -> {
                                            var allAssignments = new java.util.ArrayList<LinkedNewData.FieldAssignment>(
                                                    spreadAssignments.size() + assignments.size() + positionalArguments.size()
                                            );
                                            allAssignments.addAll(spreadAssignments);
                                            allAssignments.addAll(assignments);
                                            var positionalAssignments = mapPositionalArguments(type, allAssignments, positionalArguments, newData);
                                            if (positionalAssignments instanceof ValueOrError.Error<List<LinkedNewData.FieldAssignment>> error) {
                                                return new ValueOrError.Error<LinkedExpression>(error.errors());
                                            }
                                            allAssignments.addAll(((ValueOrError.Value<List<LinkedNewData.FieldAssignment>>) positionalAssignments).value());
                                            var validatedAssignments = validateRequiredAssignments(type, allAssignments, newData);
                                            if (validatedAssignments instanceof ValueOrError.Error<List<LinkedNewData.FieldAssignment>> error) {
                                                return new ValueOrError.Error<LinkedExpression>(error.errors());
                                            }
                                            return coerceAssignmentsForType(
                                                    type,
                                                    ((ValueOrError.Value<List<LinkedNewData.FieldAssignment>>) validatedAssignments).value(),
                                                    newData
                                            )
                                                    .map(coercedAssignments -> (LinkedExpression) new LinkedNewData(
                                                            type,
                                                            List.copyOf(coercedAssignments)
                                                    ));
                                        }))));
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> linkSpreadAssignments(List<Expression> spreads, Scope scope) {
        var assignments = new java.util.ArrayList<LinkedNewData.FieldAssignment>();
        for (var spread : spreads) {
            var linkedSpread = linkExpression(spread, scope);
            if (linkedSpread instanceof ValueOrError.Error<LinkedExpression> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            var spreadExpression = ((ValueOrError.Value<LinkedExpression>) linkedSpread).value();
            if (!(spreadExpression.type() instanceof GenericDataType dataType)) {
                return withPosition(
                        ValueOrError.error("Spread assignment requires data type, was `" + spreadExpression.type() + "`"),
                        spread.position()
                );
            }
            for (var field : dataType.fields()) {
                assignments.add(new LinkedNewData.FieldAssignment(
                        field.name(),
                        new LinkedFieldAccess(spreadExpression, field.name(), field.type())
                ));
            }
        }
        return ValueOrError.success(List.copyOf(assignments));
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> linkFieldAssignment(List<NewData.FieldAssignment> assignments, Scope scope) {
        return assignments.stream()
                .map(a -> linkExpression(a.value(), scope)
                        .map(ex -> new LinkedNewData.FieldAssignment(a.name(), ex)))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<List<LinkedExpression>> linkPositionalArguments(List<Expression> positionalArguments, Scope scope) {
        return positionalArguments.stream()
                .map(argument -> linkExpression(argument, scope))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> mapPositionalArguments(
            LinkedType type,
            List<LinkedNewData.FieldAssignment> existingAssignments,
            List<LinkedExpression> positionalArguments,
            NewData source
    ) {
        if (positionalArguments.isEmpty()) {
            return ValueOrError.success(List.of());
        }
        if (!(type instanceof GenericDataType genericDataType)) {
            return withPosition(
                    ValueOrError.error("Positional data arguments require data type, was `" + type + "`"),
                    source.position()
            );
        }

        var assignedNames = existingAssignments.stream()
                .map(LinkedNewData.FieldAssignment::name)
                .collect(java.util.stream.Collectors.toSet());
        var availableFields = genericDataType.fields().stream()
                .filter(field -> !assignedNames.contains(field.name()))
                .toList();

        if (positionalArguments.size() > availableFields.size()) {
            return withPosition(
                    ValueOrError.error(
                            "Too many positional arguments for `" + genericDataType.name() + "`: expected at most "
                            + availableFields.size() + ", got " + positionalArguments.size()
                    ),
                    source.position()
            );
        }

        var mapped = new java.util.ArrayList<LinkedNewData.FieldAssignment>(positionalArguments.size());
        for (var i = 0; i < positionalArguments.size(); i++) {
            mapped.add(new LinkedNewData.FieldAssignment(
                    availableFields.get(i).name(),
                    positionalArguments.get(i)
            ));
        }
        return ValueOrError.success(List.copyOf(mapped));
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> validateRequiredAssignments(
            LinkedType type,
            List<LinkedNewData.FieldAssignment> assignments,
            NewData source
    ) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return ValueOrError.success(List.copyOf(assignments));
        }
        var assignedNames = assignments.stream()
                .map(LinkedNewData.FieldAssignment::name)
                .collect(java.util.stream.Collectors.toSet());
        var missingField = genericDataType.fields().stream()
                .map(LinkedDataType.LinkedField::name)
                .filter(fieldName -> !assignedNames.contains(fieldName))
                .findFirst();
        if (missingField.isPresent()) {
            return withPosition(
                    ValueOrError.error("Missing assignment for field `" + missingField.get() + "` in `" + genericDataType.name() + "`"),
                    source.position()
            );
        }
        return ValueOrError.success(List.copyOf(assignments));
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> coerceAssignmentsForType(
            LinkedType type,
            List<LinkedNewData.FieldAssignment> assignments,
            NewData source
    ) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return ValueOrError.success(assignments);
        }
        var fieldsByName = genericDataType.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        LinkedDataType.LinkedField::name,
                        LinkedDataType.LinkedField::type,
                        (a, b) -> a
                ));
        var coercedAssignments = new java.util.ArrayList<LinkedNewData.FieldAssignment>(assignments.size());
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
                            ValueOrError.error(
                                    "Expected `" + renderTypeForError(expectedType)
                                    + "`, but got `" + renderTypeForError(assignment.value().type()) + "`"
                            ),
                            assignmentPosition
                    );
                }
                coercedAssignments.add(assignment);
                continue;
            }
            coercedAssignments.add(new LinkedNewData.FieldAssignment(assignment.name(), coerced.expression()));
        }
        return ValueOrError.success(List.copyOf(coercedAssignments));
    }

    private Optional<SourcePosition> findAssignmentValuePosition(NewData source, String fieldName) {
        return source.assignments().stream()
                .filter(assignment -> assignment.name().equals(fieldName))
                .map(NewData.FieldAssignment::value)
                .map(Expression::position)
                .flatMap(Optional::stream)
                .findFirst();
    }

    private boolean isHardFieldTypeMismatch(LinkedType expected, LinkedType actual) {
        if (expected.equals(actual) || expected == ANY || actual == ANY || actual == NOTHING) {
            return false;
        }

        if (expected instanceof LinkedList) {
            return !(actual instanceof LinkedList);
        }
        if (expected instanceof LinkedSet) {
            return !(actual instanceof LinkedSet);
        }
        if (expected instanceof LinkedDict) {
            return !(actual instanceof LinkedDict);
        }
        if (expected instanceof LinkedTupleType) {
            return !(actual instanceof LinkedTupleType);
        }
        if (expected instanceof LinkedFunctionType) {
            return !(actual instanceof LinkedFunctionType);
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

    private String renderTypeForError(LinkedType type) {
        return switch (type) {
            case PrimitiveLinkedType.FLOAT -> "double";
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase(java.util.Locale.ROOT);
            default -> type.toString();
        };
    }

    private ValueOrError<LinkedExpression> linkStringValue(StringValue value, Scope scope) {
        return ValueOrError.success(new LinkedStringValue(value.stringValue()));
    }

    private ValueOrError<LinkedExpression> linkNothingValue(NothingValue value) {
        return ValueOrError.success(new LinkedNothingValue(value.position(), "Encountered `???`"));
    }

    private ValueOrError<LinkedExpression> linkValue(Value value, Scope scope) {
        if (scope.localValues().containsKey(value.name())) {
            // found local value
            // has to check if there were a rewrite
            String finalName;
//            if (scope.variableNameToUniqueName().containsKey(value.name())) {
//                finalName = scope.variableNameToUniqueName().get(value.name());
//            } else {
            finalName = value.name();
//            }
            return ValueOrError.success(new LinkedVariable(finalName, scope.localValues().get(value.name())));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(value.name()))
                .findAny()
                .map(p -> new LinkedVariable(p.name(), p.type()))
                .map(ValueOrError::<LinkedExpression>success)
                .orElseGet(() -> withPosition(
                        ValueOrError.error("Variable " + value.name() + " not found"),
                        value.position()
                ));
    }

    private ValueOrError<LinkedExpression> linkLetExpression(LetExpression expression, Scope scope) {
        return linkExpression(expression.value(), scope)
                .flatMap(value -> expression.declaredType()
                        .map(declaredType -> linkType(declaredType, dataTypes)
                                .flatMap(linkedDeclaredType -> {
                                    var coerced = coerceArgument(value, linkedDeclaredType);
                                    if (coerced == null) {
                                        return withPosition(
                                                ValueOrError.error(
                                                        "Cannot assign let `" + expression.name() + "` of type `"
                                                        + linkedDeclaredType + "` from `" + value.type() + "`"
                                                ),
                                                expression.position()
                                        );
                                    }
                                    var typedValue = coerced.expression();
                                    return linkExpression(expression.rest(), scope.add(expression.name(), linkedDeclaredType))
                                            .map(rest -> new LinkedLetExpression(
                                                    expression.name(),
                                                    typedValue,
                                                    rest
                                            ));
                                }))
                        .orElseGet(() ->
                                linkExpression(expression.rest(), scope.add(expression.name(), value.type()))
                                        .map(rest ->
                                                new LinkedLetExpression(
                                                        expression.name(),
                                                        value,
                                                        rest
                                                ))));
    }

    private boolean isPipeMapExpression(InfixExpression expression) {
        return expression.right() instanceof LambdaExpression
               || expression.right() instanceof FunctionReference;
    }

    private static <T> ValueOrError<T> withPosition(ValueOrError<T> valueOrError, Optional<pl.grzeslowski.capybara.parser.SourcePosition> position) {
        if (valueOrError instanceof ValueOrError.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new ValueOrError.Error<>(error.errors()
                    .stream()
                    .map(singleError -> new ValueOrError.Error.SingleError(
                            singleError.line() > 0 ? singleError.line() : pos.line(),
                            singleError.line() > 0 ? singleError.column() : pos.column(),
                            singleError.file(),
                            singleError.message()))
                    .toList());
        }
        return valueOrError;
    }

    private record PatternAndScope(LinkedMatchExpression.Pattern pattern, Scope scope) {
    }

    public record FunctionSignature(String name, List<LinkedType> parameterTypes, LinkedType returnType) {
    }

    private record CoercedArgument(LinkedExpression expression, int coercions) {
    }

    private record CoercedArguments(List<LinkedExpression> arguments, int coercions) {
    }

    private record ResolvedFunctionCall(FunctionSignature signature, List<LinkedExpression> arguments, int coercions) {
    }

    private record PipeMapper(String argumentName, LinkedExpression expression) {
    }

    private record FunctionShape(List<LinkedType> parameterTypes, LinkedType returnType) {
    }

    private record ResolvedFunctionReference(LinkedExpression expression, int coercions) {
    }

    private record ResolvedModule(String javaModuleName, List<FunctionSignature> signatures) {
    }
}

