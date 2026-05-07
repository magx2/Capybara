package dev.capylang.generator.java;

import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.expression.*;
import dev.capylang.compiler.parser.InfixOperator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.logging.Logger;

import static java.lang.System.lineSeparator;

@SuppressWarnings("SwitchStatementWithTooFewBranches")
public class JavaExpressionEvaluator {
    private static final ThreadLocal<java.util.Map<String, String>> FUNCTION_NAME_OVERRIDES =
            ThreadLocal.withInitial(java.util.Map::of);

    public static void setFunctionNameOverrides(java.util.Map<String, String> functionNameOverrides) {
        FUNCTION_NAME_OVERRIDES.set(java.util.Map.copyOf(functionNameOverrides));
    }
    private static final java.util.concurrent.atomic.AtomicLong OPTION_CASE_VAR_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final java.util.concurrent.atomic.AtomicLong MATCH_BINDING_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final java.util.concurrent.atomic.AtomicLong MATCH_SELECTOR_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final java.util.concurrent.atomic.AtomicLong STRING_PARSE_VAR_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final java.util.concurrent.atomic.AtomicLong TUPLE_LET_VAR_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final java.util.concurrent.atomic.AtomicLong TAIL_ARGUMENT_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final Logger log = Logger.getLogger(JavaExpressionEvaluator.class.getName());
    private static final ConcurrentMap<String, String> JAVA_CAST_TYPE_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, List<String>> TOP_LEVEL_DESCRIPTOR_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, String> NORMALIZED_TYPE_REFERENCE_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentMap<String, String> NORMALIZED_QUALIFIED_TYPE_NAME_CACHE = new ConcurrentHashMap<>();
    private static final String DICT_PIPE_ARGS_SEPARATOR = "::";
    private static final String TUPLE_PIPE_ARGS_SEPARATOR = ";;";
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final java.util.regex.Pattern CONST_NAME_PATTERN = java.util.regex.Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");
    private static final java.util.Set<String> JAVA_KEYWORDS = java.util.Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );

    public static String evaluateExpression(CompiledExpression expression) {
        log.fine(() -> "evaluateExpression: " + expression.getClass().getSimpleName() + " -> " + expression);
        var scope = evaluateExpression(expression, Scope.EMPTY);
        return expressionToJava(scope);
    }

    public static String evaluateExpression(CompiledExpression expression, List<JavaMethod.JavaFunctionParameter> parameters) {
        return evaluateExpression(expression, parameters, null);
    }

    public static String evaluateExpression(
            CompiledExpression expression,
            List<JavaMethod.JavaFunctionParameter> parameters,
            String moduleHelperClass
    ) {
        log.fine(() -> "evaluateExpression: " + expression.getClass().getSimpleName() + " -> " + expression);
        var scope = initialScope(parameters, moduleHelperClass);
        var evaluatedScope = evaluateExpression(expression, scope);
        return expressionToJava(evaluatedScope);
    }

    public static String evaluateTailRecursiveExpression(
            CompiledExpression expression,
            List<JavaMethod.JavaFunctionParameter> parameters,
            List<String> selfCallNames,
            CompiledType sourceReturnType,
            List<CompiledType> sourceParameterTypes,
            String moduleHelperClass
    ) {
        log.fine(() -> "evaluateTailRecursiveExpression: " + expression.getClass().getSimpleName() + " -> " + expression);
        var context = new TailRecursiveContext(selfCallNames, parameters, sourceReturnType, sourceParameterTypes);
        var code = new StringBuilder("while (true) {\n");
        appendTailRecursiveStatement(code, expression, initialScope(parameters, moduleHelperClass), context);
        code.append("}");
        return code.toString();
    }

    private static Scope initialScope(List<JavaMethod.JavaFunctionParameter> parameters, String moduleHelperClass) {
        var scope = moduleHelperClass == null ? Scope.EMPTY : Scope.EMPTY.withModuleHelperClass(moduleHelperClass);
        for (var parameter : parameters) {
            scope = scope.addLocalValue(parameter.sourceName())
                    .addValueOverride(parameter.sourceName(), parameter.generatedName());
        }
        return scope;
    }

    private static String expressionToJava(Scope scope) {
        var statements = scope.getStatements();
        var sb = new StringBuilder();
        for (int idx = 0; idx <= statements.size() - 1; idx++) {
            sb.append(statements.get(idx)).append(';').append(lineSeparator());
        }
        sb.append("return ").append(scope.getExpression()).append(';');
        return sb.toString();
    }

    private static void appendTailRecursiveStatement(
            StringBuilder code,
            CompiledExpression expression,
            Scope scope,
            TailRecursiveContext context
    ) {
        if (expression instanceof CompiledFunctionCall functionCall && isTailRecursiveSelfCall(functionCall, context)) {
            appendTailRecursiveSelfCall(code, functionCall, scope, context);
            return;
        }
        if (expression instanceof CompiledIfExpression ifExpression) {
            appendTailRecursiveIf(code, ifExpression, scope, context);
            return;
        }
        if (expression instanceof CompiledLetExpression letExpression) {
            appendTailRecursiveLet(code, letExpression, scope, context);
            return;
        }
        if (expression instanceof CompiledMatchExpression matchExpression) {
            appendTailRecursiveMatch(code, matchExpression, scope, context);
            return;
        }

        var evaluated = evaluateExpression(expression, scope).popExpression();
        appendStatements(code, newStatements(scope, evaluated.scope()));
        code.append("return ")
                .append(coerceTailReturnExpression(context.returnType(), evaluated.expression()))
                .append(";\n");
    }

    private static void appendTailRecursiveSelfCall(
            StringBuilder code,
            CompiledFunctionCall functionCall,
            Scope scope,
            TailRecursiveContext context
    ) {
        var current = scope;
        var arguments = new ArrayList<String>(functionCall.arguments().size());
        for (var argument : functionCall.arguments()) {
            var evaluatedArgument = evaluateExpression(argument, current).popExpression();
            appendStatements(code, newStatements(current, evaluatedArgument.scope()));
            current = evaluatedArgument.scope();
            arguments.add(evaluatedArgument.expression());
        }

        var temporaryNames = new ArrayList<String>(arguments.size());
        for (int i = 0; i < arguments.size(); i++) {
            var temporaryName = "__capybaraTailArg" + TAIL_ARGUMENT_COUNTER.incrementAndGet();
            temporaryNames.add(temporaryName);
            var parameterType = context.parameterTypes().get(i);
            var argument = functionCall.arguments().get(i);
            code.append(javaCastType(parameterType))
                    .append(" ")
                    .append(temporaryName)
                    .append(" = ")
                    .append(coerceExpressionForExpectedType(parameterType, argument.type(), arguments.get(i)))
                    .append(";\n");
        }

        for (int i = 0; i < temporaryNames.size(); i++) {
            code.append(context.parameters().get(i).generatedName())
                    .append(" = ")
                    .append(temporaryNames.get(i))
                    .append(";\n");
        }
        code.append("continue;\n");
    }

    private static void appendTailRecursiveIf(
            StringBuilder code,
            CompiledIfExpression expression,
            Scope scope,
            TailRecursiveContext context
    ) {
        var condition = evaluateExpression(expression.condition(), scope).popExpression();
        appendStatements(code, newStatements(scope, condition.scope()));
        code.append("if (")
                .append(toBooleanExpression(condition.expression(), expression.condition().type()))
                .append(") {\n");
        appendTailRecursiveStatement(code, expression.thenBranch(), condition.scope(), context);
        code.append("} else {\n");
        appendTailRecursiveStatement(code, expression.elseBranch(), condition.scope(), context);
        code.append("}\n");
    }

    private static void appendTailRecursiveLet(
            StringBuilder code,
            CompiledLetExpression let,
            Scope scope,
            TailRecursiveContext context
    ) {
        var value = evaluateExpression(let.value(), scope).popExpression();
        appendStatements(code, newStatements(scope, value.scope()));
        var letDeclarationType = let.declaredType().orElse(let.value().type());
        var coercedValueExpression = coerceExpressionForExpectedType(letDeclarationType, let.value().type(), value.expression());
        var scopeExpression = shouldUseTypedLetDeclaration(let.value(), let.declaredType().isPresent())
                ? value.scope().declareTypedValue(let.name(), javaLocalDeclarationType(letDeclarationType), coercedValueExpression, let.rest())
                : value.scope().declareValue(let.name(), coercedValueExpression, let.rest());
        appendStatements(code, newStatements(value.scope(), scopeExpression.scope()));
        appendTailRecursiveStatement(code, scopeExpression.expression(), scopeExpression.scope(), context);
    }

    private static void appendTailRecursiveMatch(
            StringBuilder code,
            CompiledMatchExpression matchExpression,
            Scope scope,
            TailRecursiveContext context
    ) {
        var matchSelectorName = "__matchValue" + MATCH_SELECTOR_COUNTER.incrementAndGet();
        var optionMatch = isOptionType(matchExpression.matchWith().type());
        var matchWithExSc = evaluateExpression(matchExpression.matchWith(), scope).popExpression();
        appendStatements(code, newStatements(scope, matchWithExSc.scope()));
        var selectorExpression = castMatchSelectorExpression(matchWithExSc.expression(), matchExpression.matchWith().type(), optionMatch);
        var declaredValue = matchWithExSc.scope().declareValue(
                matchSelectorName,
                selectorExpression,
                new CompiledVariable(matchSelectorName, matchExpression.matchWith().type())
        );
        appendStatements(code, newStatements(matchWithExSc.scope(), declaredValue.scope()));
        var current = declaredValue.scope();
        var switchTarget = current.findValueOverride(matchSelectorName).orElse(matchSelectorName);

        code.append("switch (").append(switchTarget).append(") {\n");
        for (var matchCase : matchExpression.cases()) {
            var preparedCase = prepareMatchCase(matchExpression, matchCase, current, switchTarget, optionMatch);
            code.append(preparedCase.casePattern()).append(" -> {\n");
            appendTailRecursiveStatement(code, matchCase.expression(), preparedCase.branchScope(), context);
            code.append("}\n");
        }
        var hasWildcard = matchExpression.cases().stream()
                .map(CompiledMatchExpression.MatchCase::pattern)
                .anyMatch(pattern ->
                        pattern instanceof CompiledMatchExpression.WildcardPattern
                        || pattern instanceof CompiledMatchExpression.WildcardBindingPattern);
        if (optionMatch && !hasWildcard) {
            code.append("case java.lang.Object __capybaraUnexpected -> throw new java.lang.IllegalStateException(\"Unexpected value: \" + ")
                    .append(switchTarget)
                    .append(");\n");
        }
        if (matchExpression.matchWith().type() == dev.capylang.compiler.PrimitiveLinkedType.BOOL && !hasWildcard) {
            code.append("default -> throw new java.lang.IllegalStateException(\"Unexpected bool value: \" + ")
                    .append(switchTarget)
                    .append(");\n");
        }
        code.append("}\n");
    }

    private static boolean isTailRecursiveSelfCall(CompiledFunctionCall functionCall, TailRecursiveContext context) {
        if (!context.selfCallNames().contains(functionCall.name())) {
            return false;
        }
        if (functionCall.arguments().size() != context.parameterTypes().size()) {
            return false;
        }
        for (int i = 0; i < functionCall.arguments().size(); i++) {
            if (!functionCall.arguments().get(i).type().equals(context.parameterTypes().get(i))) {
                return false;
            }
        }
        return true;
    }

    private static void appendStatements(StringBuilder code, List<String> statements) {
        for (var statement : statements) {
            code.append(statement).append(";\n");
        }
    }

    private static List<String> newStatements(Scope before, Scope after) {
        return after.getStatements().subList(before.getStatements().size(), after.getStatements().size());
    }

    private record TailRecursiveContext(
            List<String> selfCallNames,
            List<JavaMethod.JavaFunctionParameter> parameters,
            CompiledType returnType,
            List<CompiledType> parameterTypes
    ) {
    }

    private static Scope evaluateExpression(CompiledExpression expression, Scope scope) {
        return switch (expression) {
            case CompiledBooleanValue booleanValue -> evaluateBooleanValue(booleanValue, scope);
            case CompiledByteValue byteValue -> evaluateByteValue(byteValue, scope);
            case CompiledDoubleValue doubleValue -> evaluateDoubleValue(doubleValue, scope);
            case CompiledEffectBindExpression effectBindExpression -> evaluateEffectBindExpression(effectBindExpression, scope);
            case CompiledEffectExpression effectExpression -> evaluateEffectExpression(effectExpression, scope);
            case CompiledFieldAccess fieldAccess -> evaluateFieldAccess(fieldAccess, scope);
            case CompiledFloatValue floatValue -> evaluateFloatValue(floatValue, scope);
            case CompiledFunctionCall functionCall -> evaluateFunctionCall(functionCall, scope);
            case CompiledFunctionInvoke functionInvoke -> evaluateFunctionInvoke(functionInvoke, scope);
            case CompiledIfExpression ifExpression -> evaluateIfExpression(ifExpression, scope);
            case CompiledIndexExpression indexExpression -> evaluateIndexExpression(indexExpression, scope);
            case CompiledInfixExpression infixExpression -> evaluateInfixExpression(infixExpression, scope);
            case CompiledIntValue intValue -> evaluateIntValue(intValue, scope);
            case CompiledLambdaExpression lambdaExpression -> evaluateLambdaExpression(lambdaExpression, scope);
            case CompiledLetExpression letExpression -> evaluateLetExpression(letExpression, scope);
            case CompiledLongValue longValue -> evaluateLongValue(longValue, scope);
            case CompiledMatchExpression matchExpression -> evaluateMatchExpression(matchExpression, scope);
            case CompiledNothingValue nothingValue -> evaluateNothingValue(nothingValue, scope);
            case CompiledNumericWidening numericWidening -> evaluateNumericWidening(numericWidening, scope);
            case CompiledPipeAllExpression pipeAllExpression -> evaluatePipeMatchExpression(pipeAllExpression.source(), pipeAllExpression.argumentName(), pipeAllExpression.predicate(), scope, "allMatch");
            case CompiledPipeAnyExpression pipeAnyExpression -> evaluatePipeMatchExpression(pipeAnyExpression.source(), pipeAnyExpression.argumentName(), pipeAnyExpression.predicate(), scope, "anyMatch");
            case CompiledPipeFlatMapExpression pipeFlatMapExpression -> evaluatePipeFlatMapExpression(pipeFlatMapExpression, scope);
            case CompiledPipeFilterOutExpression pipeFilterOutExpression -> evaluatePipeFilterOutExpression(pipeFilterOutExpression, scope);
            case CompiledPipeExpression pipeExpression -> evaluatePipeExpression(pipeExpression, scope);
            case CompiledPipeReduceExpression pipeReduceExpression -> evaluatePipeReduceExpression(pipeReduceExpression, scope);
            case CompiledSliceExpression sliceExpression -> evaluateSliceExpression(sliceExpression, scope);
            case CompiledTupleExpression tupleExpression -> evaluateTupleExpression(tupleExpression, scope);
            case CompiledNewDict newDict -> evaluateNewDict(newDict, scope);
            case CompiledNewList newList -> evaluateNewList(newList, scope);
            case CompiledNewSet newSet -> evaluateNewSet(newSet, scope);
            case CompiledNewData newData -> evaluateNewData(newData, scope);
            case CompiledStringValue stringValue -> evaluateStringValue(stringValue, scope);
            case CompiledVariable variable -> evaluateVariable(variable, scope);
        };
    }

    private static Scope evaluateBooleanValue(CompiledBooleanValue booleanValue, Scope scope) {
        return scope.addExpression(booleanValue.toString());
    }

    private static Scope evaluateByteValue(CompiledByteValue byteValue, Scope scope) {
        return scope.addExpression("((byte) " + byteValue.byteValue() + ")");
    }

    private static Scope evaluateDoubleValue(CompiledDoubleValue doubleValue, Scope scope) {
        return scope.addExpression(doubleValue.doubleValue());
    }

    private static Scope evaluateFloatValue(CompiledFloatValue floatValue, Scope scope) {
        return scope.addExpression(ensureFloatSuffix(floatValue.floatValue()));
    }

    private static Scope evaluateFieldAccess(CompiledFieldAccess fieldAccess, Scope scope) {
        var source = evaluateExpression(fieldAccess.source(), scope).popExpression();
        var expression = isResultErrorMessageFieldAccess(fieldAccess)
                ? "((" + source.expression() + ").ex() == null ? null : (" + source.expression() + ").ex().getMessage())"
                : "(" + source.expression() + ")." + fieldAccess.field() + "()";
        if (requiresGenericFieldCast(fieldAccess)) {
            var castType = javaCastType(fieldAccess.type());
            if (!"java.lang.Object".equals(castType)) {
                expression = "((" + castType + ") (" + expression + "))";
            }
        }
        return source.scope().addExpression(expression);
    }

    private static boolean isResultErrorMessageFieldAccess(CompiledFieldAccess fieldAccess) {
        if (!"message".equals(fieldAccess.field())) {
            return false;
        }
        if (!(fieldAccess.source().type() instanceof CompiledDataType linkedDataType)) {
            return false;
        }
        var normalized = normalizeQualifiedTypeName(linkedDataType.name());
        return normalized.endsWith("/Result.Error")
               || normalized.endsWith(".Result.Error")
               || "Error".equals(linkedDataType.name());
    }

    private static boolean requiresGenericFieldCast(CompiledFieldAccess fieldAccess) {
        if (fieldAccess.type() instanceof dev.capylang.compiler.CompiledGenericTypeParameter) {
            return true;
        }
        return switch (fieldAccess.source().type()) {
            case dev.capylang.compiler.CompiledDataType linkedDataType ->
                    !linkedDataType.typeParameters().isEmpty();
            case dev.capylang.compiler.CompiledDataParentType linkedDataParentType ->
                    !linkedDataParentType.typeParameters().isEmpty();
            default -> false;
        };
    }

    private static Scope evaluateFunctionCall(CompiledFunctionCall functionCall, Scope scope) {
        var current = scope;
        var args = new ArrayList<String>(functionCall.arguments().size());
        for (var argument : functionCall.arguments()) {
            var argumentScope = evaluateExpression(argument, current).popExpression();
            current = argumentScope.scope();
            args.add(argumentScope.expression());
        }

        if (functionCall.name().startsWith(METHOD_DECL_PREFIX)) {
            if (args.isEmpty()) {
                throw new IllegalStateException("Method call requires receiver argument: " + functionCall.name());
            }
            var idx = functionCall.name().lastIndexOf("__");
            var methodName = idx >= 0 && idx + 2 < functionCall.name().length()
                    ? functionCall.name().substring(idx + 2)
                    : functionCall.name();
            if ("end_with".equals(methodName)) {
                methodName = "ends_with";
            }
            var normalizedMethodName = emittedMethodName(functionCall);
            var receiver = args.get(0);
            var typedReceiver = maybeCastGenericMethodReceiver(functionCall, receiver, normalizedMethodName);
            if (functionCall.name().contains("Long__to_int")
                || functionCall.name().contains("Float__to_int")
                || functionCall.name().contains("Double__to_int")) {
                return current.addExpression("((int) " + receiver + ")");
            }
            if (functionCall.name().contains("Float__to_long")
                || functionCall.name().contains("Double__to_long")) {
                return current.addExpression("((long) " + receiver + ")");
            }
            if ("to_int".equals(methodName)) {
                return current.addExpression(buildNumericStringParseResult(
                        functionCall.type(),
                        "java.lang.Integer.parseInt(" + receiver + ")",
                        receiver,
                        "int"
                ));
            }
            if ("to_long".equals(methodName)) {
                return current.addExpression(buildNumericStringParseResult(
                        functionCall.type(),
                        "java.lang.Long.parseLong(" + receiver + ")",
                        receiver,
                        "long"
                ));
            }
            if ("to_double".equals(methodName)) {
                return current.addExpression(buildNumericStringParseResult(
                        functionCall.type(),
                        "java.lang.Double.parseDouble(" + receiver + ")",
                        receiver,
                        "double"
                ));
            }
            if ("to_float".equals(methodName)) {
                return current.addExpression(buildNumericStringParseResult(
                        functionCall.type(),
                        "java.lang.Float.parseFloat(" + receiver + ")",
                        receiver,
                        "float"
                ));
            }
            if ("to_bool".equals(methodName)) {
                return current.addExpression(buildBoolStringParseResult(functionCall.type(), receiver));
            }
            var invokeArgs = args.size() > 1
                    ? java.util.stream.IntStream.range(1, args.size())
                            .mapToObj(i -> coercePrimitiveCallArgument(functionCall.arguments().get(i).type(), args.get(i)))
                            .collect(java.util.stream.Collectors.joining(", "))
                    : "";
            return current.addExpression(typedReceiver + "." + normalizedMethodName + "(" + invokeArgs + ")");
        }

        var callArgs = java.util.stream.IntStream.range(0, args.size())
                .mapToObj(i -> coercePrimitiveCallArgument(functionCall.arguments().get(i).type(), args.get(i)))
                .toList();
        var expression = switch (functionCall.name()) {
            case "sqrt" -> {
                if (args.size() != 1) {
                    throw new IllegalStateException("sqrt expects exactly one argument");
                }
                yield "((float) java.lang.Math.sqrt(" + callArgs.get(0) + "))";
            }
            default -> isConstCall(functionCall)
                    ? normalizeFunctionCallTarget(functionCall, scope)
                    : normalizeFunctionCallTarget(functionCall, scope) + "(" + String.join(", ", callArgs) + ")";
        };
        return current.addExpression(expression);
    }

    private static boolean isConstCall(CompiledFunctionCall functionCall) {
        if (!functionCall.arguments().isEmpty()) {
            return false;
        }
        var name = functionCall.name();
        if (name.contains("__local_const_")) {
            return true;
        }
        var lastDot = name.lastIndexOf('.');
        var simpleName = lastDot >= 0 ? name.substring(lastDot + 1) : name;
        return CONST_NAME_PATTERN.matcher(simpleName).matches();
    }

    private static Scope evaluateFunctionInvoke(CompiledFunctionInvoke functionInvoke, Scope scope) {
        var functionExSc = evaluateExpression(functionInvoke.function(), scope).popExpression();
        var current = functionExSc.scope();
        var call = new StringBuilder(functionExSc.expression());
        if (functionInvoke.arguments().isEmpty()
            && functionInvoke.function().type() instanceof dev.capylang.compiler.CompiledFunctionType functionType
            && functionType.argumentType() == dev.capylang.compiler.PrimitiveLinkedType.NOTHING) {
            call.append(".get()");
            return current.addExpression(call.toString());
        }
        for (var argument : functionInvoke.arguments()) {
            var argumentScope = evaluateExpression(argument, current).popExpression();
            current = argumentScope.scope();
            call.append(".apply(").append(argumentScope.expression()).append(")");
        }
        return current.addExpression(call.toString());
    }

    private static Scope evaluateIfExpression(CompiledIfExpression expression, Scope scope) {
        var condition = evaluateExpression(expression.condition(), scope).popExpression();
        var then = evaluateExpression(expression.thenBranch(), condition.scope()).popExpression();
        var elseExSc = evaluateExpression(expression.elseBranch(), then.scope()).popExpression();

        var conditionBoolean = toBooleanExpression(condition.expression(), expression.condition().type());
        return elseExSc.scope()
                .addExpression("((%s) ? (%s) : (%s))".formatted(
                        conditionBoolean,
                        then.expression(),
                        elseExSc.expression()));
    }

    private static Scope evaluateInfixExpression(CompiledInfixExpression infixExpression, Scope scope) {
        var left = evaluateExpression(infixExpression.left(), scope).popExpression();
        var right = evaluateExpression(infixExpression.right(), left.scope()).popExpression();

        var operator = infixExpression.operator();
        var expression = switch (operator) {
            case POWER -> evaluatePowerExpression(infixExpression, left.expression(), right.expression());
            case PLUS -> {
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList) {
                    yield evaluateListAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet) {
                    yield evaluateSetAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
                    yield evaluateDictAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.type() == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
                    yield stringConcatOperand(left.expression(), infixExpression.left().type())
                          + operator.symbol()
                          + stringConcatOperand(right.expression(), infixExpression.right().type());
                }
                yield left.expression() + operator.symbol() + right.expression();
            }
            case MINUS -> {
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList) {
                    yield evaluateListRemoveExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet) {
                    yield evaluateSetRemoveExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
                    yield evaluateDictRemoveExpression(infixExpression, left.expression(), right.expression());
                }
                if (isStringLeftNumericRight(infixExpression)) {
                    yield left.expression() + "+" + right.expression();
                }
                yield left.expression() + operator.symbol() + right.expression();
            }
            case MUL, DIV, MOD -> {
                if (isStringLeftNumericRight(infixExpression)) {
                    yield left.expression() + "+" + right.expression();
                }
                yield left.expression() + operator.symbol() + right.expression();
            }
            case QUESTION -> {
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList) {
                    yield left.expression() + ".contains(" + right.expression() + ")";
                }
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet) {
                    yield left.expression() + ".contains(" + right.expression() + ")";
                }
                if (infixExpression.left().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
                    yield left.expression() + ".containsKey(" + right.expression() + ")";
                }
                if (infixExpression.left().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING
                    && infixExpression.right().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
                    yield left.expression() + ".contains(" + right.expression() + ")";
                }
                yield left.expression() + operator.symbol() + right.expression();
            }
            case BITWISE_AND, BITWISE_OR, BITWISE_XOR ->
                    left.expression() + operator.javaSymbol() + right.expression();
            case BITWISE_NAND ->
                    "~(" + left.expression() + operator.javaSymbol() + right.expression() + ")";
            case BITWISE_NOT ->
                    operator.javaSymbol() + left.expression();
            case AND, PIPE -> toBooleanExpression(left.expression(), infixExpression.left().type())
                            + operator.javaSymbol()
                            + toBooleanExpression(right.expression(), infixExpression.right().type());
            case EQUAL, NOTEQUAL -> {
                if (isBooleanCoercionComparison(infixExpression)) {
                    yield toBooleanExpression(left.expression(), infixExpression.left().type())
                          + operator.javaSymbol()
                          + toBooleanExpression(right.expression(), infixExpression.right().type());
                }
                if (isStringComparison(infixExpression)) {
                    var equalsExpression = "java.util.Objects.equals(" + left.expression() + ", " + right.expression() + ")";
                    yield operator == InfixOperator.EQUAL
                            ? equalsExpression
                            : "!(" + equalsExpression + ")";
                }
                if (!isPrimitiveComparison(infixExpression.left().type(), infixExpression.right().type())) {
                    var equalsExpression = "java.util.Objects.equals(" + left.expression() + ", " + right.expression() + ")";
                    yield operator == InfixOperator.EQUAL
                            ? equalsExpression
                            : "!(" + equalsExpression + ")";
                }
                yield left.expression() + operator.javaSymbol() + right.expression();
            }
            default -> left.expression() + operator.javaSymbol() + right.expression();
        };

        return right.scope().addExpression('(' + castIfNeeded(infixExpression.type(), expression) + ')');
    }

    private static String evaluatePowerExpression(CompiledInfixExpression infixExpression, String left, String right) {
        if (isStringLeftNumericRight(infixExpression)) {
            return left + "+" + right;
        }
        if (infixExpression.type() == dev.capylang.compiler.PrimitiveLinkedType.INT
            && infixExpression.left().type() == dev.capylang.compiler.PrimitiveLinkedType.INT
            && infixExpression.right().type() == dev.capylang.compiler.PrimitiveLinkedType.INT) {
            return "dev.capylang.CapybaraUtil.power(" + left + ", " + right + ")";
        }
        return switch (infixExpression.type()) {
            case dev.capylang.compiler.PrimitiveLinkedType.BYTE -> "((byte) java.lang.Math.pow(" + left + ", " + right + "))";
            case dev.capylang.compiler.PrimitiveLinkedType.INT -> "((int) java.lang.Math.pow(" + left + ", " + right + "))";
            case dev.capylang.compiler.PrimitiveLinkedType.LONG -> "((long) java.lang.Math.pow(" + left + ", " + right + "))";
            case dev.capylang.compiler.PrimitiveLinkedType.FLOAT -> "((float) java.lang.Math.pow(" + left + ", " + right + "))";
            case dev.capylang.compiler.PrimitiveLinkedType.DOUBLE -> "java.lang.Math.pow(" + left + ", " + right + ")";
            default -> "java.lang.Math.pow(" + left + ", " + right + ")";
        };
    }

    private static String castIfNeeded(dev.capylang.compiler.CompiledType type, String expression) {
        if (type == dev.capylang.compiler.PrimitiveLinkedType.BYTE) {
            return "((byte) (" + expression + "))";
        }
        return expression;
    }

    private static boolean isStringLeftNumericRight(CompiledInfixExpression infixExpression) {
        if (infixExpression.left().type() != dev.capylang.compiler.PrimitiveLinkedType.STRING) {
            return false;
        }
        return switch (infixExpression.right().type()) {
            case dev.capylang.compiler.PrimitiveLinkedType.BYTE,
                 dev.capylang.compiler.PrimitiveLinkedType.INT,
                 dev.capylang.compiler.PrimitiveLinkedType.LONG,
                 dev.capylang.compiler.PrimitiveLinkedType.FLOAT,
                 dev.capylang.compiler.PrimitiveLinkedType.DOUBLE -> true;
            default -> false;
        };
    }

    private static boolean isBooleanCoercionComparison(CompiledInfixExpression infixExpression) {
        var leftType = infixExpression.left().type();
        var rightType = infixExpression.right().type();
        var leftIsBool = leftType == dev.capylang.compiler.PrimitiveLinkedType.BOOL;
        var rightIsBool = rightType == dev.capylang.compiler.PrimitiveLinkedType.BOOL;
        return (leftIsBool && isBooleanConvertibleType(rightType))
               || (rightIsBool && isBooleanConvertibleType(leftType));
    }

    private static boolean isStringComparison(CompiledInfixExpression infixExpression) {
        return infixExpression.left().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING
               || infixExpression.right().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING;
    }

    private static boolean isBooleanConvertibleType(dev.capylang.compiler.CompiledType type) {
        if (type == dev.capylang.compiler.PrimitiveLinkedType.BOOL) {
            return true;
        }
        if (type == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
            return true;
        }
        if (type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList
            || type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet
            || type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
            return true;
        }
        return type == dev.capylang.compiler.PrimitiveLinkedType.BYTE
               || type == dev.capylang.compiler.PrimitiveLinkedType.INT
               || type == dev.capylang.compiler.PrimitiveLinkedType.LONG
               || type == dev.capylang.compiler.PrimitiveLinkedType.FLOAT
               || type == dev.capylang.compiler.PrimitiveLinkedType.DOUBLE;
    }

    private static boolean isPrimitiveComparison(dev.capylang.compiler.CompiledType leftType,
                                                 dev.capylang.compiler.CompiledType rightType) {
        return isNumericOrBoolType(leftType) && isNumericOrBoolType(rightType);
    }

    private static boolean isNumericOrBoolType(dev.capylang.compiler.CompiledType type) {
        return type == dev.capylang.compiler.PrimitiveLinkedType.BYTE
               || type == dev.capylang.compiler.PrimitiveLinkedType.INT
               || type == dev.capylang.compiler.PrimitiveLinkedType.LONG
               || type == dev.capylang.compiler.PrimitiveLinkedType.FLOAT
               || type == dev.capylang.compiler.PrimitiveLinkedType.DOUBLE
               || type == dev.capylang.compiler.PrimitiveLinkedType.BOOL;
    }

    private static String toBooleanExpression(String expression, dev.capylang.compiler.CompiledType type) {
        if (type == dev.capylang.compiler.PrimitiveLinkedType.BOOL) {
            return expression;
        }
        if (type == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
            return "(!(" + expression + ").isEmpty())";
        }
        if (type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList
            || type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet
            || type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
            return "(!(" + expression + ").isEmpty())";
        }
        if (type == dev.capylang.compiler.PrimitiveLinkedType.BYTE
            || type == dev.capylang.compiler.PrimitiveLinkedType.INT) {
            return "((" + expression + ") != 0)";
        }
        if (type == dev.capylang.compiler.PrimitiveLinkedType.LONG) {
            return "((" + expression + ") != 0L)";
        }
        if (type == dev.capylang.compiler.PrimitiveLinkedType.FLOAT) {
            return "((" + expression + ") != 0f)";
        }
        if (type == dev.capylang.compiler.PrimitiveLinkedType.DOUBLE) {
            return "((" + expression + ") != 0d)";
        }
        return "(" + expression + ")";
    }

    private static String evaluateListAppendExpression(CompiledInfixExpression infixExpression, String left, String right) {
        var resultElementType = infixExpression.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList listType
                ? javaStreamElementType(listType.elementType())
                : "java.lang.Object";
        var concat = "java.util.stream.Stream.<" + resultElementType + ">concat";
        if (infixExpression.right().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList) {
            return concat + "((" + left + ").stream(), (" + right + ").stream()).toList()";
        }
        return concat + "((" + left + ").stream(), java.util.stream.Stream.of(" + right + ")).toList()";
    }

    private static String javaStreamElementType(dev.capylang.compiler.CompiledType type) {
        if (type instanceof dev.capylang.compiler.CompiledDataParentType parentType
            && !parentType.typeParameters().isEmpty()) {
            var mappedTypeParameters = parentType.typeParameters().stream()
                    .map(JavaExpressionEvaluator::javaCastTypeFromDescriptor)
                    .toList();
            return normalizeJavaTypeReference(parentType.name()) + "<" + String.join(", ", mappedTypeParameters) + ">";
        }
        return javaCastType(type);
    }

    private static String evaluateSetAppendExpression(CompiledInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet) {
            return "java.util.stream.Stream.concat(" + left + ".stream(), " + right + ".stream())"
                   + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
        }
        return "java.util.stream.Stream.concat(" + left + ".stream(), java.util.stream.Stream.of(" + right + "))"
               + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
    }

    private static String evaluateDictAppendExpression(CompiledInfixExpression infixExpression, String left, String right) {
        var valueCastType = dictValueCastType(infixExpression.type(), infixExpression.left().type(), infixExpression.right().type());
        if (infixExpression.right().type() instanceof dev.capylang.compiler.CompiledTupleType) {
            var tupleExpression = "((java.util.List<?>) (" + right + "))";
            return "java.util.stream.Stream.concat(" + left + ".entrySet().stream(), "
                   + "java.util.stream.Stream.of(java.util.Map.entry("
                   + "((java.lang.String) " + tupleExpression + ".get(0)), "
                   + "((" + valueCastType + ") " + tupleExpression + ".get(1)))))"
                   + ".collect(java.util.stream.Collectors.toMap("
                   + "entry -> ((java.lang.String) ((java.util.Map.Entry<?, ?>) entry).getKey()), "
                   + "entry -> ((" + valueCastType + ") ((java.util.Map.Entry<?, ?>) entry).getValue()), "
                   + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))";
        }
        return "java.util.stream.Stream.concat(" + left + ".entrySet().stream(), " + right + ".entrySet().stream())"
               + ".collect(java.util.stream.Collectors.toMap("
               + "entry -> ((java.lang.String) ((java.util.Map.Entry<?, ?>) entry).getKey()), "
               + "entry -> ((" + valueCastType + ") ((java.util.Map.Entry<?, ?>) entry).getValue()), "
               + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))";
    }

    private static String stringConcatOperand(String expression, dev.capylang.compiler.CompiledType type) {
        if (type == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
            return "(" + expression + ")";
        }
        if (isOptionType(type)) {
            return "(" + expression + ".map(java.lang.String::valueOf).orElse(\"\"))";
        }
        return "(java.lang.String.valueOf(" + expression + "))";
    }

    private static String evaluateListRemoveExpression(CompiledInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList) {
            return left + ".stream().filter(v -> !" + right + ".contains(v)).toList()";
        }
        return left + ".stream().filter(v -> !java.util.Objects.equals(v, " + right + ")).toList()";
    }

    private static String evaluateSetRemoveExpression(CompiledInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet) {
            return left + ".stream().filter(v -> !" + right + ".contains(v))"
                   + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
        }
        return left + ".stream().filter(v -> !java.util.Objects.equals(v, " + right + "))"
               + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
    }

    private static String evaluateDictRemoveExpression(CompiledInfixExpression infixExpression, String left, String right) {
        var valueCastType = dictValueCastType(infixExpression.type(), infixExpression.left().type(), infixExpression.right().type());
        if (infixExpression.right().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
            return left + ".entrySet().stream().filter(entry -> !" + right + ".containsKey(entry.getKey()))"
                   + ".collect(java.util.stream.Collectors.toMap("
                   + "entry -> ((java.lang.String) ((java.util.Map.Entry<?, ?>) entry).getKey()), "
                   + "entry -> ((" + valueCastType + ") ((java.util.Map.Entry<?, ?>) entry).getValue()), "
                   + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))";
        }
        return left + ".entrySet().stream().filter(entry -> !java.util.Objects.equals(entry.getKey(), " + right + "))"
               + ".collect(java.util.stream.Collectors.toMap("
               + "entry -> ((java.lang.String) ((java.util.Map.Entry<?, ?>) entry).getKey()), "
               + "entry -> ((" + valueCastType + ") ((java.util.Map.Entry<?, ?>) entry).getValue()), "
               + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))";
    }

    private static String dictValueCastType(dev.capylang.compiler.CompiledType type) {
        if (type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict linkedDict) {
            return javaCastType(linkedDict.valueType());
        }
        return "java.lang.Object";
    }

    private static String dictValueCastType(
            dev.capylang.compiler.CompiledType resultType,
            dev.capylang.compiler.CompiledType leftType,
            dev.capylang.compiler.CompiledType rightType
    ) {
        var fromResult = dictValueCastType(resultType);
        if (!"java.lang.Object".equals(fromResult)) {
            return fromResult;
        }
        var fromLeft = dictValueCastType(leftType);
        if (!"java.lang.Object".equals(fromLeft)) {
            return fromLeft;
        }
        var fromRight = dictValueCastType(rightType);
        if (!"java.lang.Object".equals(fromRight)) {
            return fromRight;
        }
        return "java.lang.Object";
    }

    private static Scope evaluateIntValue(CompiledIntValue intValue, Scope scope) {
        return scope.addExpression(intValue.intValue());
    }

    private static Scope evaluateLongValue(CompiledLongValue longValue, Scope scope) {
        return scope.addExpression(longValue.longValue());
    }

    private static Scope evaluateNumericWidening(CompiledNumericWidening numericWidening, Scope scope) {
        var source = evaluateExpression(numericWidening.expression(), scope).popExpression();
        return source.scope().addExpression(coerceExpressionForExpectedType(
                numericWidening.type(),
                numericWidening.expression().type(),
                source.expression()
        ));
    }

    private static Scope evaluateLambdaExpression(CompiledLambdaExpression lambdaExpression, Scope scope) {
        if (lambdaExpression.functionType().argumentType() == dev.capylang.compiler.PrimitiveLinkedType.NOTHING) {
            var bodyExSc = evaluateExpression(lambdaExpression.expression(), scope).popExpression();
            var addedStatements = addedStatements(scope, bodyExSc.scope());
            if (addedStatements.isEmpty()) {
                return scope.addExpression("() -> (" + bodyExSc.expression() + ")");
            }
            var statements = String.join("; ", addedStatements);
            return scope.addExpression("() -> { " + statements + "; return (" + bodyExSc.expression() + "); }");
        }
        var baseScope = scope.addLocalValue(lambdaExpression.argumentName());
        var bodyExSc = evaluateExpression(lambdaExpression.expression(), baseScope).popExpression();
        return scope.addExpression(lambdaExpression(
                lambdaExpression.argumentName(),
                baseScope,
                bodyExSc.scope(),
                bodyExSc.expression()
        ));
    }

    private static String replaceIdentifier(String expression, String sourceIdentifier, String targetIdentifier) {
        return expression.replaceAll(
                "(?<![A-Za-z0-9_])" + java.util.regex.Pattern.quote(sourceIdentifier) + "(?![A-Za-z0-9_])",
                java.util.regex.Matcher.quoteReplacement(targetIdentifier)
        );
    }

    private static Scope evaluatePipeExpression(CompiledPipeExpression pipeExpression, Scope scope) {
        if (isOptionType(pipeExpression.type())) {
            return evaluateOptionPipeExpression(pipeExpression, scope);
        }
        if (pipeExpression.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict
            && pipeExpression.source().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
            return evaluateDictPipeExpression(pipeExpression, scope);
        }
        var streamExSc = evaluatePipeExpressionAsStream(pipeExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeExpression.type()));
    }

    private static Scope evaluateDictPipeExpression(CompiledPipeExpression pipeExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
        var entryVar = "__entry";
        var dictArgs = parseDictPipeArguments(pipeExpression.argumentName());

        Scope mapperBaseScope;
        if (dictArgs.length == 2) {
            mapperBaseScope = sourceExSc.scope()
                    .addValueOverride(dictArgs[0], entryVar + ".getKey()")
                    .addValueOverride(dictArgs[1], entryVar + ".getValue()");
        } else {
            mapperBaseScope = sourceExSc.scope()
                    .addValueOverride(pipeExpression.argumentName(), entryVar + ".getValue()");
        }

        var mapperExSc = evaluateExpression(pipeExpression.mapper(), mapperBaseScope).popExpression();
        var mapperLambda = lambdaExpression(entryVar, mapperBaseScope, mapperExSc.scope(), mapperExSc.expression());
        return mapperBaseScope.withoutValueOverrides().addExpression(
                sourceExSc.expression()
                + ".entrySet().stream().collect(java.util.stream.Collectors.toMap("
                + entryVar + " -> " + entryVar + ".getKey(), "
                + mapperLambda + ", "
                + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))"
        );
    }

    private static Scope evaluatePipeFlatMapExpression(CompiledPipeFlatMapExpression pipeFlatMapExpression, Scope scope) {
        var streamExSc = evaluatePipeFlatMapExpressionAsStream(pipeFlatMapExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeFlatMapExpression.type()));
    }

    private static Scope evaluatePipeFilterOutExpression(CompiledPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        if (isOptionType(pipeFilterOutExpression.type())) {
            return evaluateOptionPipeFilterOutExpression(pipeFilterOutExpression, scope);
        }
        if (pipeFilterOutExpression.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict
            && pipeFilterOutExpression.source().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
            return evaluateDictPipeFilterOutExpression(pipeFilterOutExpression, scope);
        }
        var streamExSc = evaluatePipeFilterOutExpressionAsStream(pipeFilterOutExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeFilterOutExpression.type()));
    }

    private static Scope evaluatePipeMatchExpression(
            CompiledExpression source,
            String argumentName,
            CompiledExpression predicate,
            Scope scope,
            String matchMethod
    ) {
        if (source.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict
            && argumentName.contains(DICT_PIPE_ARGS_SEPARATOR)) {
            var sourceExSc = evaluateExpression(source, scope).popExpression();
            var entryVar = "__entry";
            var dictArgs = parseDictPipeArguments(argumentName);
            var predicateBaseScope = sourceExSc.scope()
                    .addValueOverride(dictArgs[0], entryVar + ".getKey()")
                    .addValueOverride(dictArgs[1], entryVar + ".getValue()");
            var predicateExSc = evaluateExpression(predicate, predicateBaseScope).popExpression();
            var predicateLambda = lambdaExpression(
                    entryVar,
                    predicateBaseScope,
                    predicateExSc.scope(),
                    predicateExSc.expression()
            );
            return predicateBaseScope.withoutValueOverrides().addExpression(
                    sourceExSc.expression()
                    + ".entrySet().stream()." + matchMethod + "(" + predicateLambda + ")"
            );
        }

        var sourceStreamExSc = evaluateSourceAsStream(source, scope);
        var predicateBinding = bindPipeLambdaArgument(
                sourceStreamExSc.scope(),
                argumentName,
                streamElementType(source.type()).orElse(dev.capylang.compiler.PrimitiveLinkedType.ANY)
        );
        var predicateBaseScope = predicateBinding.scope();
        var predicateExSc = evaluateExpression(
                predicate,
                predicateBaseScope
        ).popExpression();
        var predicateLambda = lambdaExpression(
                predicateBinding.lambdaArgumentName(),
                predicateBaseScope,
                predicateExSc.scope(),
                predicateExSc.expression()
        );
        return sourceStreamExSc.scope().withoutValueOverrides().addExpression(
                sourceStreamExSc.streamExpression() + "." + matchMethod + "(" + predicateLambda + ")"
        );
    }

    private static Scope evaluateDictPipeFilterOutExpression(CompiledPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeFilterOutExpression.source(), scope).popExpression();
        var entryVar = "__entry";
        var dictArgs = parseDictPipeArguments(pipeFilterOutExpression.argumentName());

        Scope predicateBaseScope;
        if (dictArgs.length == 2) {
            predicateBaseScope = sourceExSc.scope()
                    .addValueOverride(dictArgs[0], entryVar + ".getKey()")
                    .addValueOverride(dictArgs[1], entryVar + ".getValue()");
        } else {
            predicateBaseScope = sourceExSc.scope()
                    .addValueOverride(pipeFilterOutExpression.argumentName(), entryVar + ".getValue()");
        }
        var predicateExSc = evaluateExpression(pipeFilterOutExpression.predicate(), predicateBaseScope).popExpression();
        var predicateLambda = lambdaExpression(
                entryVar,
                predicateBaseScope,
                predicateExSc.scope(),
                "!(" + predicateExSc.expression() + ")"
        );
        return predicateBaseScope.withoutValueOverrides().addExpression(
                sourceExSc.expression()
                + ".entrySet().stream()"
                + ".filter(" + predicateLambda + ")"
                + ".collect(java.util.stream.Collectors.toMap("
                + entryVar + " -> " + entryVar + ".getKey(), "
                + entryVar + " -> " + entryVar + ".getValue(), "
                + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))"
        );
    }

    private static Scope evaluateOptionPipeExpression(CompiledPipeExpression pipeExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
        var mapperBinding = bindPipeLambdaArgument(
                sourceExSc.scope(),
                pipeExpression.argumentName(),
                optionElementType(pipeExpression.source().type()).orElse(dev.capylang.compiler.PrimitiveLinkedType.ANY)
        );
        var mapperBaseScope = mapperBinding.scope();
        var mapperExSc = evaluateExpression(
                pipeExpression.mapper(),
                mapperBaseScope
        ).popExpression();
        var mapperLambda = lambdaExpression(
                mapperBinding.lambdaArgumentName(),
                mapperBaseScope,
                mapperExSc.scope(),
                mapperExSc.expression()
        );
        return sourceExSc.scope().withoutValueOverrides().addExpression(
                sourceExSc.expression()
                + ".map(" + mapperLambda + ")"
        );
    }

    private static Scope evaluateOptionPipeFilterOutExpression(CompiledPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeFilterOutExpression.source(), scope).popExpression();
        var predicateBinding = bindPipeLambdaArgument(
                sourceExSc.scope(),
                pipeFilterOutExpression.argumentName(),
                optionElementType(pipeFilterOutExpression.source().type()).orElse(dev.capylang.compiler.PrimitiveLinkedType.ANY)
        );
        var predicateBaseScope = predicateBinding.scope();
        var predicateExSc = evaluateExpression(
                pipeFilterOutExpression.predicate(),
                predicateBaseScope
        ).popExpression();

        var sourceExpression = isOptionType(pipeFilterOutExpression.source().type())
                               || isOptionConstructor(pipeFilterOutExpression.source())
                ? sourceExSc.expression()
                : "java.util.Optional.of(" + sourceExSc.expression() + ")";

        var predicateLambda = lambdaExpression(
                predicateBinding.lambdaArgumentName(),
                predicateBaseScope,
                predicateExSc.scope(),
                "!(" + predicateExSc.expression() + ")"
        );
        return sourceExSc.scope().withoutValueOverrides().addExpression(
                sourceExpression
                + ".filter(" + predicateLambda + ")"
        );
    }

    private static Scope evaluatePipeReduceExpression(CompiledPipeReduceExpression pipeReduceExpression, Scope scope) {
        if (pipeReduceExpression.source().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict
            && pipeReduceExpression.keyName().isPresent()) {
            var sourceExSc = evaluateExpression(pipeReduceExpression.source(), scope).popExpression();
            var initialExSc = evaluateExpression(pipeReduceExpression.initialValue(), sourceExSc.scope()).popExpression();
            var entryVar = "__entry";
            var keyName = pipeReduceExpression.keyName().orElseThrow();
            if (pipeReduceExpression.accumulatorName().contains("::")
                && pipeReduceExpression.initialValue().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
                var dictArgs = parseDictPipeArguments(pipeReduceExpression.accumulatorName());
                var leftKeyName = dictArgs[0];
                var leftValueName = dictArgs[1];
                var leftStateVar = "__capybaraLeftState";
                var rightStateVar = "__capybaraRightState";
                var leftAccumulatedVar = "__capybaraLeftAccumulated";
                var combinedValueVar = "__capybaraCombinedValue";
                var reducedStateVar = "__capybaraReducedState";
                var reducedValueVar = "__capybaraReducedValue";
                var reduceResultVar = "__capybaraReduceResult";
                var stateEntryType = "java.util.Map.Entry<java.lang.String, java.util.Map.Entry<java.lang.Object, java.lang.String>>";
                var reducerExSc = evaluateExpression(
                        pipeReduceExpression.reducerExpression(),
                        initialExSc.scope()
                                .addValueOverride(leftKeyName, leftStateVar + ".getKey()")
                                .addValueOverride(leftValueName, leftStateVar + ".getValue().getKey()")
                                .addValueOverride(keyName, rightStateVar + ".getKey()")
                                .addValueOverride(pipeReduceExpression.valueName(), rightStateVar + ".getValue().getKey()")
                ).popExpression();

                return reducerExSc.scope().addExpression(
                        "((java.lang.String)("
                        + sourceExSc.expression()
                        + ".entrySet().stream()"
                        + ".map(__capybaraEntry -> java.util.Map.entry(__capybaraEntry.getKey(), "
                        + "java.util.Map.entry((java.lang.Object)__capybaraEntry.getValue(), \"\")))"
                        + ".reduce((" + stateEntryType + " " + leftStateVar + ", " + stateEntryType + " " + rightStateVar + ") -> {"
                        + "var " + leftAccumulatedVar + " = " + leftStateVar + ".getValue().getValue();"
                        + "var " + reduceResultVar + " = (" + reducerExSc.expression() + ");"
                        + "var " + combinedValueVar + " = " + leftAccumulatedVar + " + " + reduceResultVar + ";"
                        + "return java.util.Map.entry(" + rightStateVar + ".getKey(), "
                        + "java.util.Map.entry(" + rightStateVar + ".getValue().getKey(), " + combinedValueVar + "));"
                        + "})"
                        + ".map(" + reducedStateVar + " -> " + reducedStateVar + ".getValue().getValue())"
                        + ".map(" + reducedValueVar + " -> (" + initialExSc.expression() + " + " + reducedValueVar + "))"
                        + ".orElse(" + initialExSc.expression() + ")"
                        + "))"
                );
            }
            if (pipeReduceExpression.accumulatorName().contains("::")) {
                var dictArgs = parseDictPipeArguments(pipeReduceExpression.accumulatorName());
                var leftKeyName = dictArgs[0];
                var leftValueName = dictArgs[1];
                var leftStateVar = "__capybaraLeftState";
                var rightEntryVar = "__capybaraRightEntry";
                var dictValueType = ((dev.capylang.compiler.CollectionLinkedType.CompiledDict) pipeReduceExpression.source().type())
                        .valueType();
                var leftKeyExpression = valueFieldOrFallback(
                        dictValueType,
                        leftStateVar + ".getValue()",
                        stripNumericSuffix(leftKeyName),
                        leftStateVar + ".getKey()"
                );
                var leftValueExpression = valueFieldOrFallback(
                        dictValueType,
                        leftStateVar + ".getValue()",
                        stripNumericSuffix(leftValueName),
                        leftStateVar + ".getValue()"
                );
                var rightKeyExpression = valueFieldOrFallback(
                        dictValueType,
                        rightEntryVar + ".getValue()",
                        stripNumericSuffix(keyName),
                        rightEntryVar + ".getKey()"
                );
                var rightValueExpression = valueFieldOrFallback(
                        dictValueType,
                        rightEntryVar + ".getValue()",
                        stripNumericSuffix(pipeReduceExpression.valueName()),
                        rightEntryVar + ".getValue()"
                );
                var reducerExSc = evaluateExpression(
                        pipeReduceExpression.reducerExpression(),
                        initialExSc.scope()
                                .addValueOverride(leftKeyName, leftKeyExpression)
                                .addValueOverride(leftValueName, leftValueExpression)
                                .addValueOverride(keyName, rightKeyExpression)
                                .addValueOverride(pipeReduceExpression.valueName(), rightValueExpression)
                ).popExpression();
                return reducerExSc.scope().addExpression(
                        sourceExSc.expression()
                        + ".entrySet().stream()"
                        + ".reduce("
                        + "java.util.Map.entry(\"\", " + initialExSc.expression() + ")"
                        + ", (" + leftStateVar + ", " + rightEntryVar + ") -> "
                        + "java.util.Map.entry(" + rightEntryVar + ".getKey(), (" + reducerExSc.expression() + "))"
                        + ", (left, right) -> right)"
                        + ".getValue()"
                );
            }
            if (pipeReduceExpression.initialValue().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
                var perEntryReducerExSc = evaluateExpression(
                        pipeReduceExpression.reducerExpression(),
                        initialExSc.scope()
                                .addValueOverride(pipeReduceExpression.accumulatorName(), "\"\"")
                                .addValueOverride(keyName, entryVar + ".getKey()")
                                .addValueOverride(pipeReduceExpression.valueName(), entryVar + ".getValue()")
                ).popExpression();
                var reducedValueName = "__capybaraReducedValue";
                var maybeTrimLeadingComma = "\"\"".equals(initialExSc.expression())
                        ? ""
                        : ".map(" + reducedValueName + " -> ("
                          + reducedValueName + ".startsWith(\", \") ? " + reducedValueName + ".substring(2) : ("
                          + reducedValueName + ".startsWith(\",\") ? " + reducedValueName + ".substring(1) : " + reducedValueName + ")))";
                var maybeMapPrefix = "\"\"".equals(initialExSc.expression())
                        ? ""
                        : ".map(" + reducedValueName + " -> (" + initialExSc.expression() + "+" + reducedValueName + "))";
                return perEntryReducerExSc.scope().addExpression(
                        sourceExSc.expression()
                        + ".entrySet().stream()"
                        + ".map(" + entryVar + " -> (" + perEntryReducerExSc.expression() + "))"
                        + ".reduce((left, right) -> ((left+\", \")+right))"
                        + maybeTrimLeadingComma
                        + maybeMapPrefix
                        + ".orElseGet(() -> " + initialExSc.expression() + ")"
                );
            }
            var reducerExSc = evaluateExpression(
                    pipeReduceExpression.reducerExpression(),
                    initialExSc.scope()
                            .addLocalValue(pipeReduceExpression.accumulatorName())
                            .addValueOverride(keyName, entryVar + ".getKey()")
                            .addValueOverride(pipeReduceExpression.valueName(), entryVar + ".getValue()")
            ).popExpression();
            var reduceInitialExpression = reduceInitialExpression(pipeReduceExpression.initialValue(), pipeReduceExpression.type(), initialExSc.expression());
            var javaAccumulatorName = resolveJavaLocalIdentifier(reducerExSc.scope(), pipeReduceExpression.accumulatorName());
            var javaReducerExpression = pipeReduceExpression.accumulatorName().equals(javaAccumulatorName)
                    ? reducerExSc.expression()
                    : replaceIdentifier(reducerExSc.expression(), pipeReduceExpression.accumulatorName(), javaAccumulatorName);
            return reducerExSc.scope().addExpression(
                    sourceExSc.expression()
                    + ".entrySet().stream().reduce("
                    + reduceInitialExpression
                    + ", (" + javaAccumulatorName
                    + ", " + entryVar
                    + ") -> (" + javaReducerExpression + ")"
                    + ", (left, right) -> left)"
            );
        }

        var sourceStreamExSc = evaluateSourceAsStream(pipeReduceExpression.source(), scope);
        var initialExSc = evaluateExpression(pipeReduceExpression.initialValue(), sourceStreamExSc.scope()).popExpression();
        var reducerExSc = evaluateExpression(
                pipeReduceExpression.reducerExpression(),
                initialExSc.scope()
                        .addLocalValue(pipeReduceExpression.accumulatorName())
                        .addLocalValue(pipeReduceExpression.valueName())
        ).popExpression();
        var reduceInitialExpression = reduceInitialExpression(pipeReduceExpression.initialValue(), pipeReduceExpression.type(), initialExSc.expression());
        var javaAccumulatorName = resolveJavaLocalIdentifier(reducerExSc.scope(), pipeReduceExpression.accumulatorName());
        var javaValueName = resolveJavaLocalIdentifier(reducerExSc.scope(), pipeReduceExpression.valueName());
        var javaReducerExpression = reducerExSc.expression();
        if (!pipeReduceExpression.accumulatorName().equals(javaAccumulatorName)) {
            javaReducerExpression = replaceIdentifier(javaReducerExpression, pipeReduceExpression.accumulatorName(), javaAccumulatorName);
        }
        if (!pipeReduceExpression.valueName().equals(javaValueName)) {
            javaReducerExpression = replaceIdentifier(javaReducerExpression, pipeReduceExpression.valueName(), javaValueName);
        }

        var maybeElementType = streamElementType(pipeReduceExpression.source().type());
        if (maybeElementType.isPresent() && maybeElementType.get().equals(pipeReduceExpression.initialValue().type())) {
            var reducedValueName = "__capybaraReducedValue";
            var maybeMapPrefix = pipeReduceExpression.initialValue().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING
                    && !"\"\"".equals(initialExSc.expression())
                    ? ".map(" + reducedValueName + " -> (" + reduceInitialExpression + "+" + reducedValueName + "))"
                    : "";
            return reducerExSc.scope().addExpression(
                    sourceStreamExSc.streamExpression()
                    + ".reduce("
                    + "(" + javaAccumulatorName
                    + ", " + javaValueName
                    + ") -> (" + javaReducerExpression + "))"
                    + maybeMapPrefix
                    + ".orElse("
                    + reduceInitialExpression
                    + ")"
            );
        }

        return reducerExSc.scope().addExpression(
                sourceStreamExSc.streamExpression()
                + ".reduce("
                + reduceInitialExpression
                + ", (" + javaAccumulatorName
                + ", " + javaValueName
                + ") -> (" + javaReducerExpression + ")"
                + ", (left, right) -> left)"
        );
    }

    private static java.util.Optional<dev.capylang.compiler.CompiledType> streamElementType(dev.capylang.compiler.CompiledType sourceType) {
        return switch (sourceType) {
            case dev.capylang.compiler.CollectionLinkedType.CompiledList linkedList ->
                    java.util.Optional.of(linkedList.elementType());
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet linkedSet ->
                    java.util.Optional.of(linkedSet.elementType());
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict linkedDict ->
                    java.util.Optional.of(linkedDict.valueType());
            case dev.capylang.compiler.PrimitiveLinkedType primitive when primitive == dev.capylang.compiler.PrimitiveLinkedType.STRING ->
                    java.util.Optional.of(dev.capylang.compiler.PrimitiveLinkedType.STRING);
            default -> java.util.Optional.empty();
        };
    }

    private static String reduceInitialExpression(
            CompiledExpression initialValue,
            dev.capylang.compiler.CompiledType resultType,
            String expression
    ) {
        if (initialValue.type().equals(resultType)) {
            return expression;
        }
        var typedEmptyLiteral = typedEmptyCollectionLiteral(initialValue, resultType);
        if (typedEmptyLiteral != null) {
            return typedEmptyLiteral;
        }
        return "((" + javaCastTypeForMatchCase(resultType) + ") (" + expression + "))";
    }

    private static String javaCastTypeForMatchCase(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.CompiledDataType dataType when isResultErrorDataType(dataType) ->
                    resultErrorJavaTypeReference(dataType.name());
            default -> javaCastType(type);
        };
    }

    private static String typedEmptyCollectionLiteral(
            CompiledExpression expression,
            dev.capylang.compiler.CompiledType type
    ) {
        if (expression instanceof CompiledNewList newList && newList.values().isEmpty()
            && type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList listType
            && listType.elementType() != dev.capylang.compiler.PrimitiveLinkedType.ANY) {
            return "java.util.List.<" + javaStreamElementType(listType.elementType()) + ">of()";
        }
        if (expression instanceof CompiledNewSet newSet && newSet.values().isEmpty()
            && type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet setType
            && setType.elementType() != dev.capylang.compiler.PrimitiveLinkedType.ANY) {
            return "java.util.Set.<" + javaCastType(setType.elementType()) + ">of()";
        }
        if (expression instanceof CompiledNewDict newDict && newDict.entries().isEmpty()
            && type instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict dictType
            && dictType.valueType() != dev.capylang.compiler.PrimitiveLinkedType.ANY) {
            return "java.util.Map.<java.lang.String, " + javaCastType(dictType.valueType()) + ">of()";
        }
        return null;
    }

    private static StreamExpressionScope evaluatePipeExpressionAsStream(CompiledPipeExpression pipeExpression, Scope scope) {
        if (pipeExpression.source().type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict
            && pipeExpression.argumentName().contains("::")) {
            var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
            var dictArgs = parseDictPipeArguments(pipeExpression.argumentName());
            var keyName = dictArgs[0];
            var valueName = dictArgs[1];
            var entryVar = "__entry";
            var mapperBaseScope = sourceExSc.scope()
                    .addValueOverride(keyName, entryVar + ".getKey()")
                    .addValueOverride(valueName, entryVar + ".getValue()");
            var mapperExSc = evaluateExpression(
                    pipeExpression.mapper(),
                    mapperBaseScope
            ).popExpression();
            var mapperLambda = lambdaExpression(entryVar, mapperBaseScope, mapperExSc.scope(), mapperExSc.expression());
            return new StreamExpressionScope(
                    sourceExSc.expression() + ".entrySet().stream().map(" + mapperLambda + ")",
                    mapperBaseScope.withoutValueOverrides()
            );
        }

        var sourceStreamExSc = evaluateSourceAsStream(pipeExpression.source(), scope);
        var mapperBinding = bindPipeLambdaArgument(
                sourceStreamExSc.scope(),
                pipeExpression.argumentName(),
                streamElementType(pipeExpression.source().type()).orElse(dev.capylang.compiler.PrimitiveLinkedType.ANY)
        );
        var mapperBaseScope = mapperBinding.scope();
        var mapperExSc = evaluateExpression(
                pipeExpression.mapper(),
                mapperBaseScope
        ).popExpression();
        var mapperExpression = mapperExSc.expression();
        if (pipeExpression.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList listType
            && listType.elementType() == dev.capylang.compiler.PrimitiveLinkedType.ANY) {
            mapperExpression = "(java.lang.Object) (" + mapperExpression + ")";
        }
        if (pipeExpression.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledSet setType
            && setType.elementType() == dev.capylang.compiler.PrimitiveLinkedType.ANY) {
            mapperExpression = "(java.lang.Object) (" + mapperExpression + ")";
        }
        var mapperLambda = lambdaExpression(
                mapperBinding.lambdaArgumentName(),
                mapperBaseScope,
                mapperExSc.scope(),
                mapperExpression
        );
        return new StreamExpressionScope(
                sourceStreamExSc.streamExpression()
                + ".map(" + mapperLambda + ")",
                sourceStreamExSc.scope().withoutValueOverrides()
        );
    }

    private static StreamExpressionScope evaluatePipeFilterOutExpressionAsStream(CompiledPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceStreamExSc = evaluateSourceAsStream(pipeFilterOutExpression.source(), scope);
        var predicateBinding = bindPipeLambdaArgument(
                sourceStreamExSc.scope(),
                pipeFilterOutExpression.argumentName(),
                streamElementType(pipeFilterOutExpression.source().type()).orElse(dev.capylang.compiler.PrimitiveLinkedType.ANY)
        );
        var predicateBaseScope = predicateBinding.scope();
        var predicateExSc = evaluateExpression(
                pipeFilterOutExpression.predicate(),
                predicateBaseScope
        ).popExpression();
        var predicateLambda = lambdaExpressionNoOuterParens(
                predicateBinding.lambdaArgumentName(),
                predicateBaseScope,
                predicateExSc.scope(),
                "!(" + predicateExSc.expression() + ")"
        );
        return new StreamExpressionScope(
                sourceStreamExSc.streamExpression()
                + ".filter(" + predicateLambda + ")",
                sourceStreamExSc.scope().withoutValueOverrides()
        );
    }

    private static StreamExpressionScope evaluatePipeFlatMapExpressionAsStream(CompiledPipeFlatMapExpression pipeFlatMapExpression, Scope scope) {
        var sourceStreamExSc = evaluateSourceAsStream(pipeFlatMapExpression.source(), scope);
        var mapperBinding = bindPipeLambdaArgument(
                sourceStreamExSc.scope(),
                pipeFlatMapExpression.argumentName(),
                streamElementType(pipeFlatMapExpression.source().type()).orElse(dev.capylang.compiler.PrimitiveLinkedType.ANY)
        );
        var mapperBaseScope = mapperBinding.scope();
        var mapperExSc = evaluateExpression(
                pipeFlatMapExpression.mapper(),
                mapperBaseScope
        ).popExpression();

        var streamExtractor = switch (pipeFlatMapExpression.mapper().type()) {
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict ignored -> ".values().stream()";
            default -> ".stream()";
        };
        var mapperLambda = lambdaExpressionNoOuterParens(
                mapperBinding.lambdaArgumentName(),
                mapperBaseScope,
                mapperExSc.scope(),
                "(" + mapperExSc.expression() + ")" + streamExtractor
        );
        return new StreamExpressionScope(
                sourceStreamExSc.streamExpression()
                + ".flatMap(" + mapperLambda + ")",
                sourceStreamExSc.scope().withoutValueOverrides()
        );
    }

    private static StreamExpressionScope evaluateSourceAsStream(CompiledExpression source, Scope scope) {
        if (source instanceof CompiledPipeExpression pipeExpression) {
            return evaluatePipeExpressionAsStream(pipeExpression, scope);
        }
        if (source instanceof CompiledPipeFilterOutExpression pipeFilterOutExpression) {
            return evaluatePipeFilterOutExpressionAsStream(pipeFilterOutExpression, scope);
        }
        if (source instanceof CompiledPipeFlatMapExpression pipeFlatMapExpression) {
            return evaluatePipeFlatMapExpressionAsStream(pipeFlatMapExpression, scope);
        }

        var sourceExSc = evaluateExpression(source, scope).popExpression();
        var sourceExpression = sourceExSc.expression();
        if (source.type() == dev.capylang.compiler.PrimitiveLinkedType.STRING) {
            return new StreamExpressionScope(
                    sourceExpression + ".chars().mapToObj(__capybaraChar -> java.lang.String.valueOf((char) __capybaraChar))",
                    sourceExSc.scope()
            );
        }
        if (source.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict) {
            sourceExpression = sourceExpression + ".values()";
        }
        return new StreamExpressionScope(sourceExpression + ".stream()", sourceExSc.scope());
    }

    private record StreamExpressionScope(String streamExpression, Scope scope) {
    }

    private static String lambdaExpression(
            String argumentName,
            Scope baseScope,
            Scope evaluatedScope,
            String expression
    ) {
        var javaArgumentName = resolveJavaLocalIdentifier(baseScope, argumentName);
        var bodyExpression = argumentName.equals(javaArgumentName)
                ? expression
                : replaceIdentifier(expression, argumentName, javaArgumentName);
        var addedStatements = addedStatements(baseScope, evaluatedScope);
        if (addedStatements.isEmpty()) {
            return javaArgumentName + " -> (" + bodyExpression + ")";
        }
        var statements = String.join("; ", addedStatements);
        return javaArgumentName + " -> { " + statements + "; return (" + bodyExpression + "); }";
    }

    private static String lambdaExpressionNoOuterParens(
            String argumentName,
            Scope baseScope,
            Scope evaluatedScope,
            String expression
    ) {
        var javaArgumentName = resolveJavaLocalIdentifier(baseScope, argumentName);
        var bodyExpression = argumentName.equals(javaArgumentName)
                ? expression
                : replaceIdentifier(expression, argumentName, javaArgumentName);
        var addedStatements = addedStatements(baseScope, evaluatedScope);
        if (addedStatements.isEmpty()) {
            return javaArgumentName + " -> " + bodyExpression;
        }
        var statements = String.join("; ", addedStatements);
        return javaArgumentName + " -> { " + statements + "; return " + bodyExpression + "; }";
    }

    private static java.util.List<String> addedStatements(Scope baseScope, Scope evaluatedScope) {
        var baseStatements = baseScope.getStatements();
        var evaluatedStatements = evaluatedScope.getStatements();
        if (evaluatedStatements.size() <= baseStatements.size()) {
            return java.util.List.of();
        }
        return evaluatedStatements.subList(baseStatements.size(), evaluatedStatements.size());
    }

    private static String resolveJavaLocalIdentifier(Scope scope, String identifier) {
        return scope.findValueOverride(identifier).orElseGet(() -> normalizeJavaLocalIdentifier(identifier));
    }

    private static Scope evaluateEffectExpression(CompiledEffectExpression effectExpression, Scope scope) {
        var bodyExSc = evaluateExpression(effectExpression.body(), scope).popExpression();
        var addedStatements = addedStatements(scope, bodyExSc.scope());
        if (addedStatements.isEmpty()) {
            return scope.addExpression("capy.lang.Effect.delay(() -> (" + bodyExSc.expression() + "))");
        }
        var statements = String.join("; ", addedStatements);
        return scope.addExpression("capy.lang.Effect.delay(() -> { " + statements + "; return (" + bodyExSc.expression() + "); })");
    }

    private static Scope evaluateEffectBindExpression(CompiledEffectBindExpression bind, Scope scope) {
        var sourceExSc = evaluateExpression(bind.source(), scope).popExpression();
        var unsafeRunSource = "(" + sourceExSc.expression() + ").unsafeRun()";
        var payloadExpression = coerceExpressionForExpectedType(bind.letType(), bind.payloadType(), unsafeRunSource);
        var scopeExpression = sourceExSc.scope().declareTypedValue(
                bind.name(),
                javaLocalDeclarationType(bind.letType()),
                payloadExpression,
                bind.rest()
        );
        var restExSc = evaluateExpression(scopeExpression.expression(), scopeExpression.scope()).popExpression();
        var addedStatements = addedStatements(scope, restExSc.scope());
        var statements = String.join("; ", addedStatements);
        return scope.addExpression(
                "capy.lang.Effect.delay(() -> { "
                + statements
                + "; return (("
                + restExSc.expression()
                + ").unsafeRun()); })"
        );
    }

    private static String terminalCollect(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet ignored ->
                    ".collect(java.util.stream.Collectors.toSet())";
            case dev.capylang.compiler.PrimitiveLinkedType primitive
                    when primitive == dev.capylang.compiler.PrimitiveLinkedType.STRING ->
                    ".collect(java.util.stream.Collectors.joining())";
            default -> ".toList()";
        };
    }

    private static Scope evaluateLetExpression(CompiledLetExpression let, Scope scope) {
        var valueScope = evaluateExpression(let.value(), scope);
        var valueExSc = valueScope.popExpression();
        var tupleArgs = parseTuplePipeArguments(let.name());
        if (tupleArgs.length > 0 && let.value().type() instanceof dev.capylang.compiler.CompiledTupleType tupleType) {
            var tupleVarName = "__capybaraTupleLet" + TUPLE_LET_VAR_COUNTER.incrementAndGet();
            var tupleScope = valueExSc.scope().addStatement("var " + tupleVarName + " = " + valueExSc.expression());
            var size = Math.min(tupleArgs.length, tupleType.elementTypes().size());
            for (int i = 0; i < size; i++) {
                var tupleArg = tupleArgs[i];
                if ("_".equals(tupleArg) || tupleArg.isBlank()) {
                    continue;
                }
                tupleScope = tupleScope.addValueOverride(tupleArg, tupleElementAccessExpression(tupleVarName, tupleType.elementTypes().get(i), i));
            }
            return evaluateExpression(let.rest(), tupleScope);
        }
        var letDeclarationType = let.declaredType().orElse(let.value().type());
        var coercedValueExpression = coerceExpressionForExpectedType(letDeclarationType, let.value().type(), valueExSc.expression());
        var scopeExpression = shouldUseTypedLetDeclaration(let.value(), let.declaredType().isPresent())
                ? valueExSc.scope().declareTypedValue(let.name(), javaLocalDeclarationType(letDeclarationType), coercedValueExpression, let.rest())
                : valueExSc.scope().declareValue(let.name(), coercedValueExpression, let.rest());
        return evaluateExpression(scopeExpression.expression(), scopeExpression.scope());
    }

    private static boolean shouldUseTypedLetDeclaration(CompiledExpression expression, boolean hasDeclaredType) {
        return hasDeclaredType
               || (expression instanceof CompiledNewList linkedNewList && linkedNewList.values().isEmpty())
               || (expression instanceof CompiledNewSet linkedNewSet && linkedNewSet.values().isEmpty())
               || (expression instanceof CompiledNewDict linkedNewDict && linkedNewDict.entries().isEmpty())
               || expression.type() instanceof dev.capylang.compiler.CompiledFunctionType;
    }

    private static Scope evaluateSliceExpression(CompiledSliceExpression expression, Scope scope) {
        var sourceExSc = evaluateExpression(expression.source(), scope).popExpression();
        var current = sourceExSc.scope();

        Optional<String> start = Optional.empty();
        if (expression.start().isPresent()) {
            var startExSc = evaluateExpression(expression.start().orElseThrow(), current).popExpression();
            current = startExSc.scope();
            start = Optional.of(startExSc.expression());
        }

        Optional<String> end = Optional.empty();
        if (expression.end().isPresent()) {
            var endExSc = evaluateExpression(expression.end().orElseThrow(), current).popExpression();
            current = endExSc.scope();
            end = Optional.of(endExSc.expression());
        }

        var source = sourceExSc.expression();
        var isString = expression.type() == dev.capylang.compiler.PrimitiveLinkedType.STRING;
        var sizeExpression = "(" + source + ")." + (isString ? "length()" : "size()");
        var startExpression = start
                .map(idx -> normalizeSliceIndex(idx, sizeExpression))
                .orElse("0");
        var endExpression = end
                .map(idx -> normalizeSliceIndex(idx, sizeExpression))
                .orElse(sizeExpression);

        var slice = isString
                ? source + ".substring(" + startExpression + ", " + endExpression + ")"
                : source + ".subList(" + startExpression + ", " + endExpression + ")";
        if (expression.type() instanceof dev.capylang.compiler.CompiledTupleType) {
            slice = "new java.util.ArrayList<java.lang.Object>(" + slice + ")";
        }
        return current.addExpression(slice);
    }

    private static Scope evaluateIndexExpression(CompiledIndexExpression expression, Scope scope) {
        var sourceExSc = evaluateExpression(expression.source(), scope).popExpression();
        var indexExSc = evaluateExpression(expression.index(), sourceExSc.scope()).popExpression();
        var source = sourceExSc.expression();
        var index = indexExSc.expression();
        var isString = expression.source().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING;
        var sizeExpression = "(" + source + ")." + (isString ? "length()" : "size()");
        var normalizedIndex = normalizeSliceIndex(index, sizeExpression);
        if (expression.source().type() instanceof dev.capylang.compiler.CompiledTupleType) {
            var castType = javaCastType(expression.type());
            return indexExSc.scope().addExpression("((" + castType + ") (" + source + ").get(" + normalizedIndex + "))");
        }
        var inRange = "(" + normalizedIndex + " >= 0 && " + normalizedIndex + " < " + sizeExpression + ")";
        var value = isString
                ? "java.lang.String.valueOf((" + source + ").charAt(" + normalizedIndex + "))"
                : "(" + source + ").get(" + normalizedIndex + ")";
        return indexExSc.scope().addExpression("(" + inRange + " ? java.util.Optional.of(" + value + ") : java.util.Optional.empty())");
    }

    private static Scope evaluateTupleExpression(CompiledTupleExpression expression, Scope scope) {
        var current = scope;
        var values = new ArrayList<String>(expression.values().size());
        for (var value : expression.values()) {
            var valueExSc = evaluateExpression(value, current).popExpression();
            current = valueExSc.scope();
            var renderedValue = valueExSc.expression();
            if (value.type() instanceof dev.capylang.compiler.CompiledFunctionType) {
                renderedValue = "((" + javaCastTypeForLambdaLiteral(value.type()) + ") " + renderedValue + ")";
            }
            values.add(renderedValue);
        }
        return current.addExpression("java.util.List.of(" + String.join(", ", values) + ")");
    }

    private static String javaCastTypeForLambdaLiteral(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.CompiledFunctionType linkedFunctionType ->
                    linkedFunctionType.argumentType() == dev.capylang.compiler.PrimitiveLinkedType.NOTHING
                            ? "java.util.function.Supplier<" + javaCastTypeForLambdaLiteral(linkedFunctionType.returnType()) + ">"
                            : "java.util.function.Function<"
                              + javaCastTypeForLambdaLiteral(linkedFunctionType.argumentType())
                              + ", "
                              + javaCastTypeForLambdaLiteral(linkedFunctionType.returnType())
                              + ">";
            case dev.capylang.compiler.CollectionLinkedType.CompiledList linkedList ->
                    "java.util.List<" + javaCastTypeForLambdaLiteral(linkedList.elementType()) + ">";
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet linkedSet ->
                    "java.util.Set<" + javaCastTypeForLambdaLiteral(linkedSet.elementType()) + ">";
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict linkedDict ->
                    "java.util.Map<java.lang.String, " + javaCastTypeForLambdaLiteral(linkedDict.valueType()) + ">";
            case dev.capylang.compiler.CompiledTupleType ignored -> "java.util.List<?>";
            case dev.capylang.compiler.CompiledGenericTypeParameter ignored -> "java.lang.Object";
            default -> javaCastType(type);
        };
    }

    private static String normalizeSliceIndex(String indexExpression, String sizeExpression) {
        return "((" + indexExpression + ") < 0 ? (" + sizeExpression + " + (" + indexExpression + ")) : (" + indexExpression + "))";
    }

    private static String javaCastType(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.PrimitiveLinkedType primitive -> switch (primitive) {
                case BYTE -> "java.lang.Byte";
                case INT -> "java.lang.Integer";
                case LONG -> "java.lang.Long";
                case DOUBLE -> "java.lang.Double";
                case STRING -> "java.lang.String";
                case BOOL -> "java.lang.Boolean";
                case FLOAT -> "java.lang.Float";
                case NOTHING, ANY, DATA -> "java.lang.Object";
            };
            case dev.capylang.compiler.CollectionLinkedType.CompiledList linkedList ->
                    "java.util.List<" + javaCastType(linkedList.elementType()) + ">";
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet linkedSet ->
                    "java.util.Set<" + javaCastType(linkedSet.elementType()) + ">";
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict linkedDict ->
                    "java.util.Map<java.lang.String, " + javaCastType(linkedDict.valueType()) + ">";
            case dev.capylang.compiler.CompiledTupleType ignored -> "java.util.List<?>";
            case dev.capylang.compiler.CompiledFunctionType linkedFunctionType ->
                    linkedFunctionType.argumentType() == dev.capylang.compiler.PrimitiveLinkedType.NOTHING
                            ? "java.util.function.Supplier<" + javaCastType(linkedFunctionType.returnType()) + ">"
                            : "java.util.function.Function<"
                              + javaCastType(linkedFunctionType.argumentType())
                              + ", "
                              + javaCastType(linkedFunctionType.returnType())
                              + ">";
            case dev.capylang.compiler.CompiledDataType linkedDataType ->
                    isOptionSomeTypeName(linkedDataType.name()) || isOptionNoneTypeName(linkedDataType.name())
                            ? "java.util.Optional"
                            : normalizeJavaTypeReference(linkedDataType.name());
            case dev.capylang.compiler.CompiledDataParentType linkedDataParentType ->
                    normalizeJavaTypeReference(linkedDataParentType.name());
            case dev.capylang.compiler.CompiledGenericTypeParameter genericTypeParameter ->
                    genericTypeParameter.name();
            default -> "java.lang.Object";
        };
    }

    private static String javaLocalDeclarationType(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.PrimitiveLinkedType primitive -> switch (primitive) {
                case BYTE -> "byte";
                case INT -> "int";
                case LONG -> "long";
                case DOUBLE -> "double";
                case STRING -> "java.lang.String";
                case BOOL -> "boolean";
                case FLOAT -> "float";
                case NOTHING, ANY, DATA -> "java.lang.Object";
            };
            default -> javaCastType(type);
        };
    }

    private static String coercePrimitiveCallArgument(dev.capylang.compiler.CompiledType type, String expression) {
        if (type instanceof dev.capylang.compiler.PrimitiveLinkedType primitive) {
            return switch (primitive) {
                case BYTE -> "((byte) " + expression + ")";
                case INT -> "((int) " + expression + ")";
                case LONG -> "((long) " + expression + ")";
                case DOUBLE -> "((double) " + expression + ")";
                case BOOL -> "((boolean) " + expression + ")";
                case FLOAT -> "((float) " + expression + ")";
                case STRING, NOTHING, ANY, DATA -> expression;
            };
        }
        return expression;
    }

    private static String coerceExpressionForExpectedType(
            dev.capylang.compiler.CompiledType expectedType,
            dev.capylang.compiler.CompiledType actualType,
            String expression
    ) {
        if (expectedType instanceof dev.capylang.compiler.PrimitiveLinkedType expectedPrimitive
            && actualType instanceof dev.capylang.compiler.PrimitiveLinkedType actualPrimitive
            && isImplicitNumericWidening(expectedPrimitive, actualPrimitive)) {
            return "((" + javaPrimitiveType(expectedPrimitive) + ") " + expression + ")";
        }
        return expression;
    }

    private static String coerceTailReturnExpression(dev.capylang.compiler.CompiledType expectedType, String expression) {
        if (shouldCastTailReturnToExpectedType(expectedType)) {
            return "((" + javaCastTypeForMatchCase(expectedType) + ") (" + expression + "))";
        }
        return expression;
    }

    private static boolean shouldCastTailReturnToExpectedType(dev.capylang.compiler.CompiledType expectedType) {
        return switch (expectedType) {
            case dev.capylang.compiler.CompiledDataType ignored -> true;
            case dev.capylang.compiler.CompiledDataParentType ignored -> true;
            case dev.capylang.compiler.CollectionLinkedType.CompiledList ignored -> true;
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet ignored -> true;
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict ignored -> true;
            case dev.capylang.compiler.CompiledTupleType ignored -> true;
            case dev.capylang.compiler.CompiledFunctionType ignored -> true;
            default -> false;
        };
    }

    private static boolean isImplicitNumericWidening(
            dev.capylang.compiler.PrimitiveLinkedType expected,
            dev.capylang.compiler.PrimitiveLinkedType actual
    ) {
        if (expected == actual) {
            return false;
        }
        return (actual == dev.capylang.compiler.PrimitiveLinkedType.INT
                && (expected == dev.capylang.compiler.PrimitiveLinkedType.LONG
                    || expected == dev.capylang.compiler.PrimitiveLinkedType.FLOAT
                    || expected == dev.capylang.compiler.PrimitiveLinkedType.DOUBLE))
               || (actual == dev.capylang.compiler.PrimitiveLinkedType.LONG
                   && (expected == dev.capylang.compiler.PrimitiveLinkedType.FLOAT
                       || expected == dev.capylang.compiler.PrimitiveLinkedType.DOUBLE))
               || (actual == dev.capylang.compiler.PrimitiveLinkedType.FLOAT
                   && expected == dev.capylang.compiler.PrimitiveLinkedType.DOUBLE);
    }

    private static String javaPrimitiveType(dev.capylang.compiler.PrimitiveLinkedType type) {
        return switch (type) {
            case BYTE -> "byte";
            case INT -> "int";
            case LONG -> "long";
            case FLOAT -> "float";
            case DOUBLE -> "double";
            case BOOL -> "boolean";
            default -> throw new IllegalArgumentException("Unsupported primitive cast target: " + type);
        };
    }

    private static Scope evaluateMatchExpression(CompiledMatchExpression matchExpression, Scope scope) {
        var matchSelectorName = "__matchValue" + MATCH_SELECTOR_COUNTER.incrementAndGet();
        var optionMatch = isOptionType(matchExpression.matchWith().type());
        var matchWithExSc = evaluateExpression(matchExpression.matchWith(), scope).popExpression();
        var selectorExpression = castMatchSelectorExpression(matchWithExSc.expression(), matchExpression.matchWith().type(), optionMatch);
        var declaredValue = matchWithExSc.scope().declareValue(
                matchSelectorName,
                selectorExpression,
                new CompiledVariable(matchSelectorName, matchExpression.matchWith().type())
        );
        var current = declaredValue.scope();
        var switchTarget = current.findValueOverride(matchSelectorName).orElse(matchSelectorName);
        var cases = new ArrayList<String>(matchExpression.cases().size());

        for (int caseIndex = 0; caseIndex < matchExpression.cases().size(); caseIndex++) {
            var matchCase = matchExpression.cases().get(caseIndex);
            var preparedCase = prepareMatchCase(matchExpression, matchCase, current, switchTarget, optionMatch);
            var expressionScope = evaluateExpression(matchCase.expression(), preparedCase.branchScope()).popExpression();
            var caseExpression = expressionScope.expression();
            if (optionMatch) {
                caseExpression = castMatchCaseExpression(caseExpression, matchExpression.type());
                if (isDirectOptionSomeVariableCase(matchCase)) {
                    var inferredType = inferConcreteCaseType(matchExpression, caseIndex);
                    if (inferredType != null) {
                        caseExpression = "((" + javaPatternType(inferredType) + ") (" + caseExpression + "))";
                    }
                }
            }
            if (matchCase.expression().type() instanceof CompiledDataParentType) {
                caseExpression = "((" + javaCastType(matchCase.expression().type()) + ") (" + caseExpression + "))";
            }
            var caseStatements = expressionScope.scope().getStatements()
                    .subList(preparedCase.branchScope().getStatements().size(), expressionScope.scope().getStatements().size());
            var caseStatementsCode = String.join(" ", caseStatements.stream()
                    .map(statement -> statement + ";")
                    .toList());
            var caseRule = caseStatements.isEmpty()
                    ? preparedCase.casePattern() + " -> (" + caseExpression + ");"
                    : preparedCase.casePattern()
                      + " -> { " + caseStatementsCode + " yield (" + caseExpression + "); }";
            cases.add(caseRule);
        }

        var hasWildcard = matchExpression.cases().stream()
                .map(CompiledMatchExpression.MatchCase::pattern)
                .anyMatch(pattern ->
                        pattern instanceof CompiledMatchExpression.WildcardPattern
                        || pattern instanceof CompiledMatchExpression.WildcardBindingPattern);
        var hasDefaultCase = cases.stream().anyMatch(caseRule -> caseRule.startsWith("default"));
        if (optionMatch && !hasWildcard && !hasDefaultCase) {
            cases.add("case java.lang.Object __capybaraUnexpected -> throw new java.lang.IllegalStateException(\"Unexpected value: \" + " + switchTarget + ");");
        }
        if (matchExpression.matchWith().type() == dev.capylang.compiler.PrimitiveLinkedType.BOOL
            && !hasWildcard
            && !hasDefaultCase) {
            cases.add("default -> throw new java.lang.IllegalStateException(\"Unexpected bool value: \" + " + switchTarget + ");");
        }

        return current.addExpression("switch (" + switchTarget + ") { " + String.join(" ", cases) + " }");
    }

    private static PreparedMatchCase prepareMatchCase(
            CompiledMatchExpression matchExpression,
            CompiledMatchExpression.MatchCase matchCase,
            Scope current,
            String switchTarget,
            boolean optionMatch
    ) {
        var branchScope = current;
        var optionCaseVar = "__capybaraOptionCase" + OPTION_CASE_VAR_COUNTER.incrementAndGet();
        var caseBindingNames = new java.util.HashMap<String, String>();
        if (matchCase.pattern() instanceof CompiledMatchExpression.ConstructorPattern constructorPattern) {
            var bindingNames = constructorPatternBindingNames(constructorPattern, caseBindingNames);
            var bindingCastTypes = constructorBindingCastTypes(matchExpression.matchWith().type(), constructorPattern);
            for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
                var fieldPattern = constructorPattern.fieldPatterns().get(i);
                var bindingName = bindingNames.get(i);
                if (fieldPattern instanceof CompiledMatchExpression.VariablePattern variablePattern) {
                    branchScope = branchScope.addLocalValue(variablePattern.name());
                    branchScope = branchScope.addValueOverride(variablePattern.name(), bindingName);
                    var castType = bindingCastTypes.get(i);
                    if (castType != null && !"java.lang.Object".equals(castType)) {
                        branchScope = branchScope.addValueOverride(bindingName, "((" + castType + ") " + bindingName + ")");
                        branchScope = branchScope.addValueOverride(variablePattern.name(), "((" + castType + ") " + bindingName + ")");
                    }
                }
                if (fieldPattern instanceof CompiledMatchExpression.TypedPattern typedPattern) {
                    branchScope = branchScope.addLocalValue(typedPattern.name());
                    var typedValue = "((" + javaPatternType(typedPattern.type()) + ") " + bindingName + ")";
                    branchScope = branchScope.addValueOverride(bindingName, typedValue);
                    branchScope = branchScope.addValueOverride(typedPattern.name(), typedValue);
                }
            }
            if (optionMatch && isOptionSomePattern(constructorPattern.constructorName()) && constructorPattern.fieldPatterns().size() == 1) {
                var firstPattern = constructorPattern.fieldPatterns().getFirst();
                if (firstPattern instanceof CompiledMatchExpression.VariablePattern variablePattern) {
                    var castType = optionPayloadCastType(matchExpression.matchWith().type());
                    if (castType == null) {
                        castType = bindingCastTypes.isEmpty() ? null : bindingCastTypes.getFirst();
                    }
                    var valueExpression = optionSomeBindingExpression(switchTarget + ".orElse(null)");
                    if (castType != null && !"java.lang.Object".equals(castType)) {
                        valueExpression = "((" + castType + ") " + valueExpression + ")";
                    }
                    branchScope = branchScope.addValueOverride(variablePattern.name(), valueExpression);
                }
                if (firstPattern instanceof CompiledMatchExpression.TypedPattern typedPattern) {
                    var castType = optionPayloadCastType(matchExpression.matchWith().type());
                    if (castType == null) {
                        castType = javaPatternType(typedPattern.type());
                    }
                    branchScope = branchScope.addValueOverride(
                            typedPattern.name(),
                            "((" + castType + ") " + optionSomeBindingExpression(switchTarget + ".orElse(null)") + ")"
                    );
                }
            }
            if (isResultErrorConstructor(matchExpression.matchWith().type(), constructorPattern)
                && constructorPattern.fieldPatterns().size() == 1
                && constructorPattern.fieldPatterns().getFirst() instanceof CompiledMatchExpression.VariablePattern variablePattern) {
                var valueName = variablePattern.name();
                var generatedName = caseBindingNames.getOrDefault(valueName, valueName);
                branchScope = branchScope.addValueOverride(valueName, "((" + generatedName + ") == null ? null : " + generatedName + ".getMessage())");
            }
        }
        if (matchCase.pattern() instanceof CompiledMatchExpression.TypedPattern typedPattern) {
            var generatedName = caseBindingNames.computeIfAbsent(
                    typedPattern.name(),
                    ignored -> "__capybaraMatchBinding" + MATCH_BINDING_COUNTER.incrementAndGet()
            );
            var typedPatternValue = generatedName;
            if (optionMatch && isOptionSomePattern(typedPattern.type().name())) {
                var castType = optionPayloadCastType(matchExpression.matchWith().type());
                var payloadExpression = optionSomeBindingExpression(generatedName + ".orElse(null)");
                if (castType != null && !"java.lang.Object".equals(castType)) {
                    payloadExpression = "((" + castType + ") " + payloadExpression + ")";
                }
                typedPatternValue = "new "
                                    + normalizeJavaTypeReference(stripGenericSuffix(typedPattern.type().name()))
                                    + "<>("
                                    + payloadExpression
                                    + ")";
            } else if (optionMatch && isOptionNonePattern(typedPattern.type().name())) {
                typedPatternValue = normalizeJavaTypeReference(stripGenericSuffix(typedPattern.type().name())) + ".INSTANCE";
            }
            branchScope = branchScope
                    .addLocalValue(typedPattern.name())
                    .addValueOverride(typedPattern.name(), typedPatternValue);
        }
        if (matchCase.pattern() instanceof CompiledMatchExpression.TypedPattern typedPattern
            && matchExpression.matchWith() instanceof CompiledVariable matchedVariable) {
            var generatedName = caseBindingNames.getOrDefault(typedPattern.name(), typedPattern.name());
            branchScope = branchScope.addValueOverride(matchedVariable.name(), generatedName);
        }
        if (matchCase.pattern() instanceof CompiledMatchExpression.WildcardBindingPattern wildcardBindingPattern) {
            var generatedName = caseBindingNames.computeIfAbsent(
                    wildcardBindingPattern.name(),
                    ignored -> "__capybaraMatchBinding" + MATCH_BINDING_COUNTER.incrementAndGet()
            );
            branchScope = branchScope
                    .addLocalValue(wildcardBindingPattern.name())
                    .addValueOverride(wildcardBindingPattern.name(), generatedName);
        }
        var casePattern = matchCasePattern(matchCase.pattern(), matchExpression.matchWith().type(), optionCaseVar, caseBindingNames);
        if (matchCase.guard().isPresent()) {
            var guardScope = evaluateExpression(matchCase.guard().orElseThrow(), branchScope).popExpression();
            var guardStatements = guardScope.scope().getStatements()
                    .subList(branchScope.getStatements().size(), guardScope.scope().getStatements().size());
            var guardExpression = wrapGuardExpression(guardStatements, guardScope.expression());
            casePattern = appendCaseGuard(casePattern, guardExpression);
        }
        return new PreparedMatchCase(branchScope, casePattern);
    }

    private record PreparedMatchCase(Scope branchScope, String casePattern) {
    }

    private static String appendCaseGuard(String casePattern, String guardExpression) {
        if (casePattern.startsWith("default")) {
            return casePattern;
        }
        var whenIndex = casePattern.indexOf(" when ");
        if (whenIndex >= 0) {
            return casePattern + " && (" + guardExpression + ")";
        }
        return casePattern + " when " + guardExpression;
    }

    private static String wrapGuardExpression(List<String> guardStatements, String guardExpression) {
        if (guardStatements.isEmpty()) {
            return "(" + guardExpression + ")";
        }
        var statements = String.join(" ", guardStatements.stream().map(statement -> statement + ";").toList());
        return "((java.util.function.Supplier<java.lang.Boolean>) () -> { " + statements + " return (" + guardExpression + "); }).get()";
    }

    private static List<String> constructorBindingCastTypes(
            dev.capylang.compiler.CompiledType matchType,
            CompiledMatchExpression.ConstructorPattern constructorPattern
    ) {
        var constructorType = resolveConstructorType(matchType, constructorPattern.constructorName());
        if (constructorType == null) {
            return java.util.Collections.nCopies(constructorPattern.fieldPatterns().size(), null);
        }
        var genericCasts = resolveGenericTypeCasts(matchType, constructorType);
        return constructorType.fields().stream()
                .limit(constructorPattern.fieldPatterns().size())
                .map(field -> {
                    var fieldType = field.type();
                    if (fieldType instanceof dev.capylang.compiler.CompiledFunctionType) {
                        return null;
                    }
                    if (fieldType instanceof dev.capylang.compiler.CompiledGenericTypeParameter genericTypeParameter) {
                        var resolvedGenericCast = sanitizePatternCastType(genericCasts.get(genericTypeParameter.name()));
                        return resolvedGenericCast != null ? resolvedGenericCast : genericTypeParameter.name();
                    }
                    if (fieldType instanceof dev.capylang.compiler.CompiledDataParentType parentType
                        && parentType.subTypes().isEmpty()
                        && parentType.name().matches("[A-Z]")) {
                        var resolvedGenericCast = sanitizePatternCastType(genericCasts.get(parentType.name()));
                        return resolvedGenericCast != null ? resolvedGenericCast : parentType.name();
                    }
                    return sanitizePatternCastType(javaCastType(fieldType));
                })
                .toList();
    }

    private static String sanitizePatternCastType(String castType) {
        if (castType == null || "java.lang.Object".equals(castType)) {
            return null;
        }
        if (castType.matches(".*(^|[<,\\s])[A-Z]([>,\\s]|$).*")) {
            return null;
        }
        return castType;
    }

    private static CompiledDataType resolveConstructorType(
            dev.capylang.compiler.CompiledType matchType,
            String constructorName
    ) {
        if (matchType instanceof CompiledDataType linkedDataType) {
            return typeNameMatches(linkedDataType.name(), constructorName) ? linkedDataType : null;
        }
        if (matchType instanceof CompiledDataParentType parentType) {
            return parentType.subTypes().stream()
                    .filter(subType -> typeNameMatches(subType.name(), constructorName))
                    .findFirst()
                    .orElse(null);
        }
        return null;
    }

    private static boolean typeNameMatches(String typeName, String constructorName) {
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

    private static boolean isResultErrorConstructor(
            dev.capylang.compiler.CompiledType matchType,
            CompiledMatchExpression.ConstructorPattern constructorPattern
    ) {
        var constructorType = resolveConstructorType(matchType, constructorPattern.constructorName());
        if (constructorType == null) {
            return false;
        }
        var normalized = normalizeQualifiedTypeName(constructorType.name());
        return normalized.endsWith("/Result.Error")
               || normalized.endsWith(".Result.Error")
               || "Error".equals(constructorType.name());
    }

    private static String optionPayloadCastType(dev.capylang.compiler.CompiledType matchType) {
        if (!(matchType instanceof CompiledDataParentType parentType) || parentType.typeParameters().isEmpty()) {
            return null;
        }
        var normalized = normalizeQualifiedTypeName(parentType.name());
        if (!normalized.endsWith("/Option") && !normalized.endsWith(".Option") && !"Option".equals(parentType.name())) {
            return null;
        }
        return sanitizePatternCastType(javaCastTypeFromDescriptor(parentType.typeParameters().getFirst()));
    }

    private static java.util.Map<String, String> resolveGenericTypeCasts(
            dev.capylang.compiler.CompiledType matchType,
            CompiledDataType constructorType
    ) {
        if (!(matchType instanceof CompiledDataParentType parentType)
            || constructorType.typeParameters().isEmpty()
            || parentType.typeParameters().isEmpty()) {
            return java.util.Map.of();
        }
        var resolved = new java.util.HashMap<String, String>();
        var max = Math.min(constructorType.typeParameters().size(), parentType.typeParameters().size());
        for (int i = 0; i < max; i++) {
            resolved.put(constructorType.typeParameters().get(i), javaCastTypeFromDescriptor(parentType.typeParameters().get(i)));
        }
        return java.util.Map.copyOf(resolved);
    }

    private static String javaCastTypeFromDescriptor(String descriptor) {
        var normalized = descriptor == null ? "" : descriptor.trim();
        if (normalized.isEmpty()) {
            return "java.lang.Object";
        }
        var cached = JAVA_CAST_TYPE_CACHE.get(normalized);
        if (cached != null) {
            return cached;
        }
        var javaType = computeJavaCastTypeFromDescriptor(normalized);
        JAVA_CAST_TYPE_CACHE.put(normalized, javaType);
        return javaType;
    }

    private static String computeJavaCastTypeFromDescriptor(String normalized) {
        return switch (normalized.toLowerCase(java.util.Locale.ROOT)) {
            case "byte" -> "java.lang.Byte";
            case "int" -> "java.lang.Integer";
            case "long" -> "java.lang.Long";
            case "float" -> "java.lang.Float";
            case "double" -> "java.lang.Double";
            case "string" -> "java.lang.String";
            case "bool" -> "java.lang.Boolean";
            case "any", "nothing", "data" -> "java.lang.Object";
            default -> {
                if (normalized.matches("[A-Z]")) {
                    yield "java.lang.Object";
                }
                if (normalized.startsWith("list[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("list[".length(), normalized.length() - 1);
                    yield "java.util.List<" + javaCastTypeFromDescriptor(inner) + ">";
                }
                if (normalized.startsWith("set[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("set[".length(), normalized.length() - 1);
                    yield "java.util.Set<" + javaCastTypeFromDescriptor(inner) + ">";
                }
                if (normalized.startsWith("dict[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("dict[".length(), normalized.length() - 1);
                    yield "java.util.Map<java.lang.String, " + javaCastTypeFromDescriptor(inner) + ">";
                }
                if (normalized.startsWith("tuple[") && normalized.endsWith("]")) {
                    yield "java.util.List<java.lang.Object>";
                }
                var genericStart = normalized.indexOf('[');
                if (genericStart > 0 && normalized.endsWith("]")) {
                    var rawType = normalized.substring(0, genericStart).trim();
                    var argsDescriptor = normalized.substring(genericStart + 1, normalized.length() - 1);
                    var rawArgs = splitTopLevelDescriptors(argsDescriptor);
                    var javaArgs = rawArgs.stream()
                            .map(JavaExpressionEvaluator::javaCastTypeFromDescriptor)
                            .toList();
                    if (isOptionSomeTypeName(rawType) || isOptionNoneTypeName(rawType)) {
                        var optionalElementType = javaArgs.isEmpty() ? "java.lang.Object" : javaArgs.getFirst();
                        yield "java.util.Optional<" + optionalElementType + ">";
                    }
                    var javaRaw = normalizeJavaTypeReference(rawType);
                    if (rawArgs.stream().anyMatch(arg -> arg.matches("[A-Z]"))) {
                        yield javaRaw;
                    }
                    yield javaRaw + "<" + String.join(", ", javaArgs) + ">";
                }
                if ("Error".equals(normalized)
                    || normalizeQualifiedTypeName(normalized).endsWith("/Result.Error")) {
                    yield resultErrorJavaTypeReference(normalized);
                }
                yield normalizeJavaTypeReference(normalized);
            }
        };
    }

    private static String maybeCastGenericMethodReceiver(
            CompiledFunctionCall functionCall,
            String receiverExpression,
            String methodName
    ) {
        if (!(functionCall.arguments().get(0).type() instanceof dev.capylang.compiler.CompiledDataParentType parentType)) {
            return receiverExpression;
        }
        var normalizedName = normalizeQualifiedTypeName(parentType.name());
        var isResultType = "Result".equals(parentType.name())
                           || normalizedName.endsWith("/Result")
                           || normalizedName.endsWith("/Result.Result");
        if (!isResultType || parentType.typeParameters().isEmpty()) {
            return receiverExpression;
        }
        if ("or".equals(methodName)) {
            return "((" + resultParentRawJavaTypeReference(parentType) + ") (" + receiverExpression + "))";
        }
        var needsTypedReceiver = switch (methodName) {
            case "pipe", "pipeStar", "pipeGreater", "or", "orElse" -> true;
            default -> false;
        };
        if (!needsTypedReceiver) {
            return receiverExpression;
        }
        var castType = resultParentJavaTypeReference(parentType);
        if (hasUnknownTypeDescriptor(parentType.typeParameters())) {
            castType = resultParentRawJavaTypeReference(parentType) + "<?>";
        }
        return "((" + castType + ") (" + receiverExpression + "))";
    }

    private static boolean hasUnknownTypeDescriptor(List<String> descriptors) {
        return descriptors.stream().anyMatch(JavaExpressionEvaluator::isUnknownTypeDescriptor);
    }

    private static boolean isUnknownTypeDescriptor(String descriptor) {
        var normalized = descriptor == null ? "" : descriptor.trim();
        if (normalized.isEmpty()) {
            return true;
        }
        if ("any".equalsIgnoreCase(normalized) || "nothing".equalsIgnoreCase(normalized) || "data".equalsIgnoreCase(normalized)) {
            return true;
        }
        return normalized.matches("[A-Z]");
    }

    private static boolean hasUnresolvedTypeDescriptor(String descriptor) {
        var normalized = descriptor == null ? "" : descriptor.trim();
        if (isUnknownTypeDescriptor(normalized)) {
            return true;
        }
        var genericStart = normalized.indexOf('[');
        if (genericStart > 0 && normalized.endsWith("]")) {
            var argsDescriptor = normalized.substring(genericStart + 1, normalized.length() - 1);
            return splitTopLevelDescriptors(argsDescriptor).stream().anyMatch(JavaExpressionEvaluator::hasUnresolvedTypeDescriptor);
        }
        return false;
    }

    private static List<String> splitTopLevelDescriptors(String descriptors) {
        return TOP_LEVEL_DESCRIPTOR_CACHE.computeIfAbsent(descriptors, JavaExpressionEvaluator::computeSplitTopLevelDescriptors);
    }

    private static List<String> computeSplitTopLevelDescriptors(String descriptors) {
        var result = new ArrayList<String>();
        var depth = 0;
        var start = 0;
        for (int i = 0; i < descriptors.length(); i++) {
            var ch = descriptors.charAt(i);
            if (ch == '[') {
                depth++;
            } else if (ch == ']') {
                depth = Math.max(0, depth - 1);
            } else if (ch == ',' && depth == 0) {
                var part = descriptors.substring(start, i).trim();
                if (!part.isEmpty()) {
                    result.add(part);
                }
                start = i + 1;
            }
        }
        var tail = descriptors.substring(start).trim();
        if (!tail.isEmpty()) {
            result.add(tail);
        }
        return List.copyOf(result);
    }

    private static String castMatchSelectorExpression(
            String expression,
            dev.capylang.compiler.CompiledType selectorType,
            boolean optionMatch
    ) {
        if (optionMatch
            || selectorType == dev.capylang.compiler.PrimitiveLinkedType.ANY
            || selectorType == dev.capylang.compiler.PrimitiveLinkedType.DATA
            || selectorType == dev.capylang.compiler.PrimitiveLinkedType.NOTHING
            || selectorType instanceof dev.capylang.compiler.CompiledGenericTypeParameter
            || selectorType instanceof dev.capylang.compiler.CompiledDataType
            || selectorType instanceof dev.capylang.compiler.CompiledDataParentType) {
            return expression;
        }
        return "((" + javaPatternType(selectorType) + ") (" + expression + "))";
    }

    private static String matchCasePattern(
            CompiledMatchExpression.Pattern pattern,
            dev.capylang.compiler.CompiledType matchType,
            String optionCaseVar,
            java.util.Map<String, String> caseBindingNames
    ) {
        if (isOptionType(matchType)) {
            return switch (pattern) {
                case CompiledMatchExpression.VariablePattern variablePattern when isOptionSomePattern(variablePattern.name()) ->
                        "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isPresent()";
                case CompiledMatchExpression.VariablePattern variablePattern when isOptionNonePattern(variablePattern.name()) ->
                        "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isEmpty()";
                case CompiledMatchExpression.TypedPattern typedPattern when isOptionSomePattern(typedPattern.type().name()) ->
                        "case java.util.Optional " + caseBindingNames.getOrDefault(typedPattern.name(), typedPattern.name()) + " when " + caseBindingNames.getOrDefault(typedPattern.name(), typedPattern.name()) + ".isPresent()";
                case CompiledMatchExpression.TypedPattern typedPattern when isOptionNonePattern(typedPattern.type().name()) ->
                        "case java.util.Optional " + caseBindingNames.getOrDefault(typedPattern.name(), typedPattern.name()) + " when " + caseBindingNames.getOrDefault(typedPattern.name(), typedPattern.name()) + ".isEmpty()";
                case CompiledMatchExpression.WildcardBindingPattern wildcardBindingPattern ->
                        "case " + javaPatternType(matchType) + " " + caseBindingNames.getOrDefault(wildcardBindingPattern.name(), wildcardBindingPattern.name());
                case CompiledMatchExpression.ConstructorPattern constructorPattern when isOptionSomePattern(constructorPattern.constructorName()) ->
                        optionSomeCasePattern(constructorPattern, optionCaseVar, caseBindingNames);
                case CompiledMatchExpression.ConstructorPattern constructorPattern when isOptionNonePattern(constructorPattern.constructorName()) ->
                        "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isEmpty()";
                default -> "default";
            };
        }
        return switch (pattern) {
            case CompiledMatchExpression.IntPattern intPattern -> "case " + intPattern.value();
            case CompiledMatchExpression.LongPattern longPattern -> {
                var longBindingName = "__capybaraLongLiteral" + MATCH_BINDING_COUNTER.incrementAndGet();
                yield "case java.lang.Long " + longBindingName
                      + " when java.util.Objects.equals(" + longBindingName + ", " + longPattern.value() + ")";
            }
            case CompiledMatchExpression.StringPattern stringPattern -> "case " + stringPattern.value();
            case CompiledMatchExpression.BoolPattern boolPattern -> {
                var boolBindingName = "__capybaraBoolLiteral" + MATCH_BINDING_COUNTER.incrementAndGet();
                yield "case java.lang.Boolean " + boolBindingName
                      + " when java.util.Objects.equals(" + boolBindingName + ", " + boolPattern.value() + ")";
            }
            case CompiledMatchExpression.FloatPattern floatPattern -> "case " + floatPattern.value();
            case CompiledMatchExpression.TypedPattern typedPattern -> {
                var patternBindingName = caseBindingNames.getOrDefault(typedPattern.name(), typedPattern.name());
                if (typedPattern.type() == dev.capylang.compiler.PrimitiveLinkedType.DATA) {
                    yield "case java.lang.Object " + patternBindingName + " when " + dataGuard(patternBindingName);
                }
                if (typedPattern.type() instanceof CompiledDataType typedDataType) {
                    var resolvedTypedType = resolveConstructorType(matchType, typedDataType.name());
                    var patternType = constructorPatternTypeName(matchType, resolvedTypedType, typedDataType.name());
                    yield "case " + patternType + " " + patternBindingName;
                }
                yield "case " + javaPatternType(typedPattern.type()) + " " + patternBindingName;
            }
            case CompiledMatchExpression.VariablePattern variablePattern -> {
                if (matchType instanceof CompiledDataParentType parentType && parentType.enumType()) {
                    yield "case " + variablePattern.name();
                }
                if (matchType instanceof CompiledDataParentType) {
                    var constructorType = resolveConstructorType(matchType, variablePattern.name());
                    if (constructorType != null) {
                        yield "case " + constructorPatternTypeName(matchType, constructorType, variablePattern.name()) + " __ignored";
                    }
                }
                yield "case " + variablePattern.name() + " __ignored";
            }
            case CompiledMatchExpression.WildcardPattern wildcardPattern -> "default";
            case CompiledMatchExpression.WildcardBindingPattern wildcardBindingPattern ->
                    "case " + javaPatternType(matchType) + " " + caseBindingNames.getOrDefault(wildcardBindingPattern.name(), wildcardBindingPattern.name());
            case CompiledMatchExpression.ConstructorPattern constructorPattern -> {
                var constructorType = resolveConstructorType(matchType, constructorPattern.constructorName());
                var patternType = constructorPatternTypeName(matchType, constructorType, constructorPattern.constructorName());
                var bindingNames = constructorPatternBindingNames(constructorPattern, caseBindingNames);
                var constructorCasePattern = "case " + patternType + "("
                                             + bindingNames.stream().map(name -> "var " + name).reduce((a, b) -> a + ", " + b).orElse("")
                                             + ")";
                var guard = constructorPatternGuard(constructorPattern, bindingNames);
                yield guard.map(s -> constructorCasePattern + " when " + s).orElse(constructorCasePattern);
            }
        };
    }

    private static String constructorPatternTypeName(
            dev.capylang.compiler.CompiledType matchType,
            CompiledDataType constructorType,
            String constructorName
    ) {
        if (constructorType == null) {
            return unresolvedConstructorPatternType(constructorName);
        }
        if (constructorType.name().contains(".") || constructorType.name().contains("/")) {
            return normalizeJavaTypeReference(constructorType.name());
        }
        if (matchType instanceof CompiledDataParentType parentType) {
            var normalizedParent = stripGenericSuffix(normalizeQualifiedTypeName(parentType.name()));
            if (normalizedParent.endsWith("/Result")
                || normalizedParent.endsWith(".Result")
                || normalizedParent.endsWith("/Result.Result")
                || normalizedParent.endsWith(".Result.Result")
                || "Result".equals(parentType.name())) {
                return normalizeJavaTypeReference(stripGenericSuffix(parentType.name()))
                       + "."
                       + normalizeJavaClassName(constructorType.name());
            }
        }
        return normalizeJavaClassName(constructorType.name());
    }

    private static String unresolvedConstructorPatternType(String constructorName) {
        if ("Error".equals(constructorName) || "Success".equals(constructorName)) {
            return "capy.lang.Result." + constructorName;
        }
        return normalizeJavaTypeReference(constructorName);
    }

    private static boolean isOptionSomePattern(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return "Some".equals(name)
               || normalized.endsWith("/Option.Some")
               || normalized.endsWith(".Some");
    }

    private static boolean isOptionNonePattern(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return "None".equals(name)
               || normalized.endsWith("/Option.None")
               || normalized.endsWith(".None");
    }

    private static String optionSomeBindingExpression(String rawValueExpression) {
        return rawValueExpression;
    }

    private static boolean isDirectOptionSomeVariableCase(CompiledMatchExpression.MatchCase matchCase) {
        if (!(matchCase.pattern() instanceof CompiledMatchExpression.ConstructorPattern constructorPattern)) {
            return false;
        }
        if (!isOptionSomePattern(constructorPattern.constructorName()) || constructorPattern.fieldPatterns().size() != 1) {
            return false;
        }
        if (!(constructorPattern.fieldPatterns().getFirst() instanceof CompiledMatchExpression.VariablePattern variablePattern)) {
            return false;
        }
        return matchCase.expression() instanceof CompiledVariable variable
               && variable.name().equals(variablePattern.name());
    }

    private static String optionSomeCasePattern(
            CompiledMatchExpression.ConstructorPattern constructorPattern,
            String optionCaseVar,
            java.util.Map<String, String> caseBindingNames
    ) {
        var base = "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isPresent()";
        if (constructorPattern.fieldPatterns().size() != 1) {
            return base;
        }
        var fieldPattern = constructorPattern.fieldPatterns().getFirst();
        var guard = constructorFieldGuardExpression(fieldPattern, optionCaseVar + ".orElse(null)");
        return guard.map(s -> base + " && (" + s + ")").orElse(base);
    }

    private static List<String> constructorPatternBindingNames(
            CompiledMatchExpression.ConstructorPattern constructorPattern,
            java.util.Map<String, String> caseBindingNames
    ) {
        var names = new ArrayList<String>(constructorPattern.fieldPatterns().size());
        for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
            var fieldPattern = constructorPattern.fieldPatterns().get(i);
            if (fieldPattern instanceof CompiledMatchExpression.VariablePattern variablePattern) {
                names.add(caseBindingNames.computeIfAbsent(
                        variablePattern.name(),
                        ignored -> "__capybaraMatchBinding" + MATCH_BINDING_COUNTER.incrementAndGet()
                ));
            } else if (fieldPattern instanceof CompiledMatchExpression.TypedPattern typedPattern) {
                names.add(caseBindingNames.computeIfAbsent(
                        typedPattern.name(),
                        ignored -> "__capybaraMatchBinding" + MATCH_BINDING_COUNTER.incrementAndGet()
                ));
            } else {
                names.add("__capybaraCtorField" + i);
            }
        }
        return names;
    }

    private static java.util.Optional<String> constructorPatternGuard(
            CompiledMatchExpression.ConstructorPattern constructorPattern,
            List<String> bindingNames
    ) {
        var guards = new ArrayList<String>();
        for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
            constructorFieldGuardExpression(constructorPattern.fieldPatterns().get(i), bindingNames.get(i))
                    .ifPresent(guards::add);
        }
        if (guards.isEmpty()) {
            return java.util.Optional.empty();
        }
        return java.util.Optional.of(String.join(" && ", guards));
    }

    private static java.util.Optional<String> constructorFieldGuardExpression(
            CompiledMatchExpression.Pattern pattern,
            String valueExpression
    ) {
        return switch (pattern) {
            case CompiledMatchExpression.IntPattern intPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + intPattern.value() + ")");
            case CompiledMatchExpression.LongPattern longPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + longPattern.value() + ")");
            case CompiledMatchExpression.StringPattern stringPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + stringPattern.value() + ")");
            case CompiledMatchExpression.BoolPattern boolPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + boolPattern.value() + ")");
            case CompiledMatchExpression.FloatPattern floatPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + floatPattern.value() + ")");
            case CompiledMatchExpression.TypedPattern typedPattern ->
                    java.util.Optional.of("(" + valueExpression + " instanceof " + javaPatternType(typedPattern.type()) + ")");
            case CompiledMatchExpression.VariablePattern ignored -> java.util.Optional.empty();
            case CompiledMatchExpression.WildcardPattern ignored -> java.util.Optional.empty();
            case CompiledMatchExpression.WildcardBindingPattern ignored -> java.util.Optional.empty();
            case CompiledMatchExpression.ConstructorPattern ignored ->
                    throw new IllegalStateException("Nested constructor pattern in constructor pattern is not supported in Java generation");
        };
    }

    private static dev.capylang.compiler.CompiledType inferConcreteCaseType(
            CompiledMatchExpression matchExpression,
            int currentCaseIndex
    ) {
        for (int idx = 0; idx < matchExpression.cases().size(); idx++) {
            if (idx == currentCaseIndex) {
                continue;
            }
            var type = matchExpression.cases().get(idx).expression().type();
            if (type instanceof dev.capylang.compiler.CompiledGenericTypeParameter) {
                continue;
            }
            if (type == dev.capylang.compiler.PrimitiveLinkedType.ANY
                || type == dev.capylang.compiler.PrimitiveLinkedType.DATA
                || type == dev.capylang.compiler.PrimitiveLinkedType.NOTHING) {
                continue;
            }
            return type;
        }
        return null;
    }

    private static String dataGuard(String varName) {
        return "!("
               + varName + " instanceof java.lang.Number"
               + " || " + varName + " instanceof java.lang.Boolean"
               + " || " + varName + " instanceof java.lang.String"
               + " || " + varName + " instanceof java.util.List"
               + " || " + varName + " instanceof java.util.Set"
               + " || " + varName + " instanceof java.util.Map"
               + ")";
    }

    private static String castMatchCaseExpression(String expression, dev.capylang.compiler.CompiledType resultType) {
        if (resultType == dev.capylang.compiler.PrimitiveLinkedType.ANY
            || resultType == dev.capylang.compiler.PrimitiveLinkedType.DATA
            || resultType == dev.capylang.compiler.PrimitiveLinkedType.NOTHING
            || resultType instanceof dev.capylang.compiler.CompiledGenericTypeParameter) {
            return expression;
        }
        return "((" + javaCastTypeForMatchCase(resultType) + ") (" + expression + "))";
    }

    private static String javaPatternType(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.PrimitiveLinkedType primitiveType -> switch (primitiveType) {
                case BYTE -> "java.lang.Byte";
                case INT -> "java.lang.Integer";
                case LONG -> "java.lang.Long";
                case DOUBLE -> "java.lang.Double";
                case STRING -> "java.lang.String";
                case BOOL -> "java.lang.Boolean";
                case FLOAT -> "java.lang.Float";
                case NOTHING, ANY, DATA -> "java.lang.Object";
            };
            case dev.capylang.compiler.CompiledDataType dataType -> normalizeJavaTypeReference(dataType.name());
            case dev.capylang.compiler.CompiledDataParentType dataParentType -> normalizeJavaTypeReference(dataParentType.name());
            case dev.capylang.compiler.CollectionLinkedType.CompiledList ignored -> "java.util.List";
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet ignored -> "java.util.Set";
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict ignored -> "java.util.Map";
            case dev.capylang.compiler.CompiledTupleType ignored -> "java.util.List";
            default -> "java.lang.Object";
        };
    }

    private static Scope evaluateNewList(CompiledNewList newList, Scope scope) {
        var current = scope;
        var values = new ArrayList<String>(newList.values().size());
        for (var value : newList.values()) {
            var exSc = evaluateExpression(value, current).popExpression();
            current = exSc.scope();
            values.add(exSc.expression());
        }
        if (newList.values().isEmpty()
            && newList.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledList listType
            && listType.elementType() != dev.capylang.compiler.PrimitiveLinkedType.ANY) {
            return current.addExpression("java.util.List.<" + javaStreamElementType(listType.elementType()) + ">of()");
        }
        return current.addExpression("java.util.List.of(" + String.join(", ", values) + ")");
    }

    private static Scope evaluateNewSet(CompiledNewSet newSet, Scope scope) {
        var current = scope;
        var values = new ArrayList<String>(newSet.values().size());
        for (var value : newSet.values()) {
            var exSc = evaluateExpression(value, current).popExpression();
            current = exSc.scope();
            values.add(exSc.expression());
        }
        return current.addExpression("java.util.Set.of(" + String.join(", ", values) + ")");
    }

    private static Scope evaluateNewDict(CompiledNewDict newDict, Scope scope) {
        var current = scope;
        var entries = new ArrayList<String>(newDict.entries().size());
        for (var entry : newDict.entries()) {
            var keyExSc = evaluateExpression(entry.key(), current).popExpression();
            var valueExSc = evaluateExpression(entry.value(), keyExSc.scope()).popExpression();
            current = valueExSc.scope();
            entries.add("java.util.Map.entry(" + keyExSc.expression() + ", " + valueExSc.expression() + ")");
        }
        if (entries.isEmpty()) {
            if (newDict.type() instanceof dev.capylang.compiler.CollectionLinkedType.CompiledDict linkedDict) {
                var valueType = javaCastType(linkedDict.valueType());
                if (!"java.lang.Object".equals(valueType)) {
                    return current.addExpression(
                            "java.util.Map.<java.lang.String, " + valueType + ">of()"
                    );
                }
            }
            return current.addExpression("java.util.Map.of()");
        }
        return current.addExpression(
                "java.util.stream.Stream.of(" + String.join(", ", entries) + ")"
                + ".collect(java.util.stream.Collectors.toMap("
                + "java.util.Map.Entry::getKey, java.util.Map.Entry::getValue, "
                + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))"
        );
    }

    private static Scope evaluateNewData(CompiledNewData newData, Scope scope) {
        if (!(newData.type() instanceof dev.capylang.compiler.CompiledDataType dataType)) {
            throw new UnsupportedOperationException("Cannot instantiate non-data type: " + newData.type());
        }

        var current = scope;
        var byName = new java.util.HashMap<String, String>();
        for (var assignment : newData.assignments()) {
            var expressionScope = evaluateExpression(assignment.value(), current).popExpression();
            current = expressionScope.scope();
            byName.put(assignment.name(), expressionScope.expression());
        }

        var args = dataType.fields().stream()
                .map(field -> {
                    var value = byName.get(field.name());
                    if (value == null) {
                        throw new IllegalStateException("Missing assignment for field `" + field.name() + "` in `" + dataType.name() + "`");
                    }
                    var assignment = newData.assignments().stream()
                            .filter(it -> it.name().equals(field.name()))
                            .findFirst()
                            .orElseThrow(() -> new IllegalStateException(
                                    "Missing assignment metadata for field `" + field.name() + "` in `" + dataType.name() + "`"));
                    return coerceExpressionForExpectedType(field.type(), assignment.value().type(), value);
                })
                .toList();

        if (isOptionSomeTypeName(dataType.name())) {
            if (args.size() != 1) {
                throw new IllegalStateException("`Some` expects a single `value` argument");
            }
            return current.addExpression("java.util.Optional.of(" + args.get(0) + ")");
        }
        if (isOptionNoneTypeName(dataType.name())) {
            return current.addExpression("java.util.Optional.empty()");
        }
        if (isResultErrorDataType(dataType)) {
            if (args.size() != 1) {
                throw new IllegalStateException("`Result.Error` expects a single `message` argument");
            }
            return current.addExpression("new " + resultErrorJavaTypeReference(dataType.name())
                                         + "(new dev.capylang.CapybaraException(" + args.get(0) + "))");
        }
        if (dataType.singleton()) {
            var javaType = normalizeJavaTypeReference(dataType.name());
            if (isEnumValueType(dataType)) {
                return current.addExpression(javaType);
            }
            return current.addExpression(javaType + ".INSTANCE");
        }

        var javaType = normalizeJavaTypeReference(dataType.name());
        var genericSuffix = dataType.typeParameters().isEmpty() ? "" : "<>";
        return current.addExpression("new " + javaType + genericSuffix + "(" + String.join(", ", args) + ")");
    }

    private static boolean isEnumValueType(CompiledDataType dataType) {
        return dataType.name().equals(dataType.name().toUpperCase(java.util.Locale.ROOT));
    }
    private static Scope evaluateStringValue(CompiledStringValue stringValue, Scope scope) {
        return scope.addExpression(stringValue.toString());
    }

    private static Scope evaluateNothingValue(CompiledNothingValue nothingValue, Scope scope) {
        return scope.addExpression("__capybaraUnsupported(\"" + escapeJavaString(nothingValue.message()) + "\")");
    }

    private static Scope evaluateVariable(CompiledVariable variable, Scope scope) {
        var name = scope.findValueOverride(variable.name()).orElse(variable.name());
        return scope.addExpression(name);
    }

    private static String normalizeJavaMethodName(String name) {
        var leadingUnderscores = countLeadingUnderscores(name);
        var coreName = name.substring(leadingUnderscores);
        var parts = coreName.split("[^A-Za-z0-9]+");
        var result = new StringBuilder();
        var first = true;
        for (var part : parts) {
            if (part.isEmpty()) {
                continue;
            }
            if (first) {
                result.append(Character.toLowerCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
                first = false;
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        if (result.isEmpty()) {
            result.append(encodeSymbolicMethodName(coreName));
        }
        var identifier = "_".repeat(leadingUnderscores) + result;
        if (JAVA_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }

    private static int countLeadingUnderscores(String value) {
        var count = 0;
        while (count < value.length() && value.charAt(count) == '_') {
            count++;
        }
        return count;
    }

    private static String encodeSymbolicMethodName(String raw) {
        var parts = new ArrayList<String>(raw.length());
        for (var i = 0; i < raw.length(); i++) {
            parts.add(symbolName(raw.charAt(i)));
        }
        if (parts.isEmpty()) {
            return "generated";
        }
        var result = new StringBuilder();
        for (var i = 0; i < parts.size(); i++) {
            var part = parts.get(i);
            if (i == 0) {
                result.append(part);
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        return result.toString();
    }

    private static String symbolName(char symbol) {
        return switch (symbol) {
            case '+' -> "plus";
            case '-' -> "minus";
            case '*' -> "star";
            case '/' -> "slash";
            case '\\' -> "backslash";
            case '^' -> "power";
            case '%' -> "mod";
            case '$' -> "dollar";
            case '#' -> "hash";
            case '@' -> "at";
            case '~' -> "tilde";
            case '!' -> "bang";
            case ':' -> "colon";
            case '<' -> "less";
            case '>' -> "greater";
            case '|' -> "pipe";
            default -> "op" + Integer.toHexString(symbol);
        };
    }

    private static String normalizeJavaLocalIdentifier(String identifier) {
        if ("_".equals(identifier)) {
            return "__unused";
        }
        if (identifier.isEmpty()) {
            return "__value";
        }
        var normalized = new StringBuilder(identifier.length());
        for (int i = 0; i < identifier.length(); i++) {
            var ch = identifier.charAt(i);
            normalized.append(Character.isJavaIdentifierPart(ch) ? ch : '_');
        }
        var candidate = normalized.toString();
        if (!Character.isJavaIdentifierStart(candidate.charAt(0))) {
            candidate = "_" + candidate;
        }
        if (JAVA_KEYWORDS.contains(candidate)) {
            return candidate + "_";
        }
        return candidate;
    }

    private static String normalizeFunctionCallTarget(CompiledFunctionCall functionCall, Scope scope) {
        var target = functionCall.name();
        var emittedMethodName = emittedMethodName(functionCall);
        var lastDot = target.lastIndexOf('.');
        if (lastDot < 0) {
            if (scope.moduleHelperClass().isPresent()) {
                return scope.moduleHelperClass().get() + "." + emittedMethodName;
            }
            return emittedMethodName;
        }
        var qualifier = target.substring(0, lastDot);
        return normalizeJavaQualifier(qualifier) + "." + emittedMethodName;
    }

    private static String emittedMethodName(CompiledFunctionCall functionCall) {
        var parameterTypes = functionCall.arguments().stream().map(CompiledExpression::type).toList();
        var key = signatureKey(functionCall.name(), parameterTypes);
        var functionNameOverrides = functionNameOverrides();
        if (functionNameOverrides.containsKey(key)) {
            return functionNameOverrides.get(key);
        }
        var parameterSignature = parameterTypes.stream()
                .map(type -> String.valueOf(type))
                .collect(java.util.stream.Collectors.joining(","));
        var simpleMethodName = simpleMethodName(functionCall.name());
        var overrideBySimpleName = findOverrideBySimpleName(functionCall.name(), parameterSignature);
        if (overrideBySimpleName.isPresent()) {
            return overrideBySimpleName.get();
        }
        var methodName = simpleMethodName;
        if (methodName.contains("__compiled")) {
            return methodName;
        }
        if ("end_with".equals(methodName)) {
            methodName = "ends_with";
        }
        return normalizeJavaMethodName(methodName);
    }

    private static String keyName(String signatureKey) {
        var separator = signatureKey.indexOf('|');
        return separator >= 0 ? signatureKey.substring(0, separator) : signatureKey;
    }

    static Optional<String> findOverrideBySimpleName(String targetName, String parameterSignature) {
        var simpleMethodName = simpleMethodName(targetName);
        var qualifier = qualifierName(targetName);
        var candidates = functionNameOverrides().entrySet().stream()
                .filter(entry -> simpleMethodName(keyName(entry.getKey())).equals(simpleMethodName))
                .filter(entry -> keyParameterSignature(entry.getKey()).equals(parameterSignature))
                .toList();
        if (qualifier != null) {
            var qualifiedMatch = candidates.stream()
                    .filter(entry -> java.util.Objects.equals(qualifierName(keyName(entry.getKey())), qualifier))
                    .map(java.util.Map.Entry::getValue)
                    .findFirst();
            if (qualifiedMatch.isPresent()) {
                return qualifiedMatch;
            }
        }
        return candidates.stream()
                .filter(entry -> qualifierName(keyName(entry.getKey())) == null)
                .map(java.util.Map.Entry::getValue)
                .findFirst();
    }

    private static java.util.Map<String, String> functionNameOverrides() {
        return FUNCTION_NAME_OVERRIDES.get();
    }

    private static String keyParameterSignature(String signatureKey) {
        var separator = signatureKey.indexOf('|');
        return separator >= 0 ? signatureKey.substring(separator + 1) : "";
    }

    private static String qualifierName(String target) {
        var lastDot = target.lastIndexOf('.');
        return lastDot >= 0 ? target.substring(0, lastDot) : null;
    }

    private static String simpleMethodName(String target) {
        var lastDot = target.lastIndexOf('.');
        var methodName = lastDot >= 0 ? target.substring(lastDot + 1) : target;
        if (methodName.startsWith(METHOD_DECL_PREFIX)) {
            var idx = methodName.lastIndexOf("__");
            methodName = idx >= 0 && idx + 2 < methodName.length() ? methodName.substring(idx + 2) : methodName;
        }
        return methodName;
    }

    private static String signatureKey(String name, java.util.List<dev.capylang.compiler.CompiledType> parameterTypes) {
        return name + "|" + parameterTypes.stream().map(type -> String.valueOf(type)).collect(java.util.stream.Collectors.joining(","));
    }

    private static String normalizeJavaQualifier(String qualifier) {
        var parts = qualifier.split("\\.");
        var normalized = new ArrayList<String>(parts.length);
        for (var part : parts) {
            if (part.isBlank()) {
                continue;
            }
            if (Character.isUpperCase(part.charAt(0))) {
                normalized.add(normalizeJavaClassName(part));
            } else {
                normalized.add(normalizeJavaPackageSegment(part));
            }
        }
        return String.join(".", normalized);
    }

    private static String normalizeJavaPackageSegment(String rawName) {
        var parts = rawName.split("[^A-Za-z0-9]+");
        var result = new StringBuilder();
        var first = true;
        for (var part : parts) {
            if (part.isEmpty()) {
                continue;
            }
            if (first) {
                result.append(Character.toLowerCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
                first = false;
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        if (result.isEmpty()) {
            result.append("pkg");
        }
        if (!Character.isJavaIdentifierStart(result.charAt(0))) {
            result.insert(0, 'p');
        }
        var identifier = result.toString();
        if (JAVA_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }

    private static String normalizeJavaTypeReference(String typeName) {
        return NORMALIZED_TYPE_REFERENCE_CACHE.computeIfAbsent(typeName, JavaExpressionEvaluator::computeNormalizedJavaTypeReference);
    }

    private static String computeNormalizedJavaTypeReference(String typeName) {
        var rawTypeName = stripGenericSuffix(typeName);
        var normalizedTypeName = normalizeQualifiedTypeName(rawTypeName);
        if ("Option".equals(rawTypeName)
            || normalizedTypeName.endsWith("/Option.Option")
            || normalizedTypeName.endsWith("/Option")) {
            return "java.util.Optional";
        }
        if ("Program".equals(rawTypeName)
            || normalizedTypeName.endsWith("/Program.Program")
            || normalizedTypeName.endsWith("/Program")) {
            return "capy.lang.Program";
        }
        if ("Effect".equals(rawTypeName)
            || normalizedTypeName.endsWith("/Effect.Effect")
            || normalizedTypeName.endsWith("/Effect")) {
            return "capy.lang.Effect";
        }
        if (rawTypeName.startsWith("/") && !rawTypeName.contains(".")) {
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < rawTypeName.length() - 1) {
                var packageName = normalizeJavaPackagePath(rawTypeName.substring(1, slashIndex));
                var className = normalizeJavaClassName(rawTypeName.substring(slashIndex + 1));
                return packageName + "." + className;
            }
        }
        if (rawTypeName.contains("/") && rawTypeName.contains(".")) {
            var dotIndex = rawTypeName.lastIndexOf('.');
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = rawTypeName.startsWith("/") ? 1 : 0;
                var packageName = normalizeJavaPackagePath(rawTypeName.substring(startIdx, slashIndex));
                var outer = normalizeJavaClassName(rawTypeName.substring(slashIndex + 1, dotIndex));
                var inner = normalizeJavaClassName(rawTypeName.substring(dotIndex + 1));
                if (outer.equals(inner)) {
                    return packageName + "." + outer;
                }
                return packageName + "." + outer + "." + inner;
            }
        }
        if (!rawTypeName.contains(".")) {
            return normalizeJavaClassName(rawTypeName);
        }
        var parts = rawTypeName.split("\\.");
        var normalized = new ArrayList<String>(parts.length);
        for (var part : parts) {
            normalized.add(normalizeJavaClassName(part));
        }
        return String.join(".", normalized);
    }

    private static String normalizeJavaPackagePath(String path) {
        var normalized = path.replace('\\', '/');
        return Arrays.stream(normalized.split("/"))
                .filter(part -> !part.isBlank())
                .map(JavaExpressionEvaluator::normalizeJavaPackageSegment)
                .collect(java.util.stream.Collectors.joining("."));
    }

    private static String normalizeJavaClassName(String rawName) {
        var leadingUnderscores = countLeadingUnderscores(rawName);
        var suffix = rawName.substring(leadingUnderscores);
        var parts = suffix.split("[^A-Za-z0-9]+");
        var result = new StringBuilder();
        for (var part : parts) {
            if (part.isEmpty()) {
                continue;
            }
            result.append(Character.toUpperCase(part.charAt(0)));
            if (part.length() > 1) {
                result.append(part.substring(1));
            }
        }
        if (result.isEmpty()) {
            return "Generated";
        }
        var identifier = result.toString();
        if (leadingUnderscores > 0) {
            identifier = "_".repeat(leadingUnderscores) + identifier;
        }
        if (!Character.isJavaIdentifierStart(identifier.charAt(0))) {
            identifier = "T" + identifier;
        }
        if (JAVA_KEYWORDS.contains(identifier)) {
            identifier = identifier + "_";
        }
        return identifier;
    }

    private static String escapeJavaString(String value) {
        return value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"");
    }

    private static String[] parseDictPipeArguments(String argumentName) {
        if (!argumentName.contains(DICT_PIPE_ARGS_SEPARATOR)) {
            return new String[0];
        }
        var parts = argumentName.split(java.util.regex.Pattern.quote(DICT_PIPE_ARGS_SEPARATOR), -1);
        if (parts.length != 2 || parts[0].isBlank() || parts[1].isBlank()) {
            return new String[0];
        }
        return parts;
    }

    private static String[] parseTuplePipeArguments(String argumentName) {
        if (!argumentName.contains(TUPLE_PIPE_ARGS_SEPARATOR)) {
            return new String[0];
        }
        return argumentName.split(java.util.regex.Pattern.quote(TUPLE_PIPE_ARGS_SEPARATOR), -1);
    }

    private static Optional<dev.capylang.compiler.CompiledType> optionElementType(dev.capylang.compiler.CompiledType sourceType) {
        return switch (sourceType) {
            case dev.capylang.compiler.CompiledDataParentType parentType when isOptionType(parentType) ->
                    parentType.fields().stream().findFirst().map(dev.capylang.compiler.CompiledDataType.CompiledField::type);
            default -> Optional.empty();
        };
    }

    private static PipeLambdaBinding bindPipeLambdaArgument(
            Scope scope,
            String argumentName,
            dev.capylang.compiler.CompiledType elementType
    ) {
        var tupleArgs = parseTuplePipeArguments(argumentName);
        if (tupleArgs.length == 0 || !(elementType instanceof dev.capylang.compiler.CompiledTupleType tupleType)) {
            return new PipeLambdaBinding(argumentName, scope.addLocalValue(argumentName));
        }
        var lambdaArgumentName = "__tupleItem";
        var lambdaScope = scope.addLocalValue(lambdaArgumentName);
        var tupleElementTypes = tupleType.elementTypes();
        var size = Math.min(tupleElementTypes.size(), tupleArgs.length);
        for (int i = 0; i < size; i++) {
            var tupleArg = tupleArgs[i];
            if ("_".equals(tupleArg) || tupleArg.isBlank()) {
                continue;
            }
            lambdaScope = lambdaScope.addValueOverride(
                    tupleArg,
                    tupleElementAccessExpression(lambdaArgumentName, tupleElementTypes.get(i), i)
            );
        }
        return new PipeLambdaBinding(lambdaArgumentName, lambdaScope);
    }

    private record PipeLambdaBinding(String lambdaArgumentName, Scope scope) {
    }

    private static String tupleElementAccessExpression(
            String tupleValueExpression,
            dev.capylang.compiler.CompiledType elementType,
            int index
    ) {
        return "((" + javaCastType(elementType) + ") ((java.util.List<?>) " + tupleValueExpression + ").get(" + index + "))";
    }

    private static String stripNumericSuffix(String value) {
        return value.replaceFirst("\\d+$", "");
    }

    private static String valueFieldOrFallback(dev.capylang.compiler.CompiledType valueType,
                                               String valueExpression,
                                               String fieldName,
                                               String fallbackExpression) {
        return switch (valueType) {
            case dev.capylang.compiler.CompiledDataType linkedDataType ->
                    linkedDataType.fields().stream().anyMatch(field -> field.name().equals(fieldName))
                            ? "(" + valueExpression + ")." + fieldName + "()"
                            : fallbackExpression;
            case dev.capylang.compiler.CompiledDataParentType linkedDataParentType ->
                    linkedDataParentType.fields().stream().anyMatch(field -> field.name().equals(fieldName))
                            ? "(" + valueExpression + ")." + fieldName + "()"
                            : fallbackExpression;
            default -> fallbackExpression;
        };
    }

    private static String ensureFloatSuffix(String literal) {
        if (literal.endsWith("f") || literal.endsWith("F")) {
            return literal;
        }
        return literal + "f";
    }

    private static boolean isOptionSomeTypeName(String typeName) {
        var normalized = normalizeQualifiedTypeName(typeName);
        return "Some".equals(typeName)
               || normalized.equals("/cap/lang/Option.Some")
               || normalized.equals("/capy/lang/Option.Some")
               || normalized.endsWith("/Option.Some")
               || normalized.endsWith(".Some");
    }

    private static boolean isOptionNoneTypeName(String typeName) {
        var normalized = normalizeQualifiedTypeName(typeName);
        return "None".equals(typeName)
               || normalized.equals("/cap/lang/Option.None")
               || normalized.equals("/capy/lang/Option.None")
               || normalized.endsWith("/Option.None")
               || normalized.endsWith(".None");
    }

    private static boolean isResultErrorDataType(dev.capylang.compiler.CompiledDataType dataType) {
        var normalized = normalizeQualifiedTypeName(dataType.name());
        if (normalized.equals("/cap/lang/Result.Error") || normalized.equals("/capy/lang/Result.Error")) {
            return true;
        }
        return "Error".equals(dataType.name())
               && dataType.fields().size() == 1
               && "message".equals(dataType.fields().getFirst().name())
               && dataType.fields().getFirst().type() == dev.capylang.compiler.PrimitiveLinkedType.STRING;
    }

    private static String resultErrorJavaTypeReference(String typeName) {
        var normalized = normalizeQualifiedTypeName(typeName);
        if (normalized.equals("/cap/lang/Result.Error") || normalized.endsWith("/cap/lang/Result.Error")) {
            return "cap.lang.Result.Error";
        }
        if (normalized.equals("/capy/lang/Result.Error") || normalized.endsWith("/capy/lang/Result.Error")) {
            return "capy.lang.Result.Error";
        }
        if ("Error".equals(typeName)) {
            return "capy.lang.Result.Error";
        }
        return normalizeJavaTypeReference(typeName);
    }

    private static String normalizeQualifiedTypeName(String typeName) {
        return NORMALIZED_QUALIFIED_TYPE_NAME_CACHE.computeIfAbsent(typeName, rawTypeName -> {
            var normalized = rawTypeName.replace('\\', '/');
            if (!normalized.startsWith("/")) {
                normalized = "/" + normalized;
            }
            return normalized;
        });
    }

    private static String stripGenericSuffix(String typeName) {
        var idx = typeName.indexOf('[');
        return idx >= 0 ? typeName.substring(0, idx) : typeName;
    }

    private static boolean isOptionType(dev.capylang.compiler.CompiledType type) {
        if (!(type instanceof dev.capylang.compiler.GenericDataType genericDataType)) {
            return false;
        }
        var normalized = normalizeQualifiedTypeName(genericDataType.name());
        return "Option".equals(genericDataType.name())
               || normalized.endsWith("/Option.Option")
               || normalized.endsWith("/Option");
    }

    private static boolean isOptionConstructor(CompiledExpression expression) {
        if (!(expression instanceof CompiledNewData linkedNewData)
            || !(linkedNewData.type() instanceof dev.capylang.compiler.GenericDataType dataType)) {
            return false;
        }
        return isOptionSomeTypeName(dataType.name()) || isOptionNoneTypeName(dataType.name());
    }

    private static String buildNumericStringParseResult(
            dev.capylang.compiler.CompiledType resultType,
            String parseExpression,
            String receiverExpression,
            String targetTypeName
    ) {
        var id = STRING_PARSE_VAR_COUNTER.incrementAndGet();
        var parsedVar = "__capybaraParsedValue" + id;
        var exVar = "__capybaraParseException" + id;
        var resultJavaType = resultParentJavaTypeReference(resultType);
        var successType = resultSuccessJavaTypeReference(resultType);
        var errorType = resultErrorJavaTypeReferenceForResultType(resultType);
        return "((java.util.function.Supplier<" + resultJavaType + ">) () -> { try { var " + parsedVar + " = " + parseExpression
               + "; return new " + successType + "(" + parsedVar + "); } catch (java.lang.Exception " + exVar + ") { return new "
               + errorType + "(new dev.capylang.CapybaraException(\"Cannot parse string to "
               + targetTypeName + ": \" + " + receiverExpression + ", " + exVar + ")); } }).get()";
    }

    private static String buildBoolStringParseResult(
            dev.capylang.compiler.CompiledType resultType,
            String receiverExpression
    ) {
        var id = STRING_PARSE_VAR_COUNTER.incrementAndGet();
        var normalizedVar = "__capybaraNormalizedBool" + id;
        var resultJavaType = resultParentJavaTypeReference(resultType);
        var successType = resultSuccessJavaTypeReference(resultType);
        var errorType = resultErrorJavaTypeReferenceForResultType(resultType);
        return "((java.util.function.Supplier<" + resultJavaType + ">) () -> { var " + normalizedVar + " = "
               + receiverExpression + ".toLowerCase(java.util.Locale.ROOT); if (\"true\".equals(" + normalizedVar
               + ") || \"false\".equals(" + normalizedVar + ")) { return new " + successType
               + "(java.lang.Boolean.parseBoolean(" + normalizedVar + ")); } return new " + errorType
               + "(new dev.capylang.CapybaraException(\"Cannot parse string to bool: \" + "
               + receiverExpression + ")); }).get()";
    }

    private static String resultSuccessJavaTypeReference(dev.capylang.compiler.CompiledType resultType) {
        return resultParentRawJavaTypeReference(resultType) + ".Success";
    }

    private static String resultErrorJavaTypeReferenceForResultType(dev.capylang.compiler.CompiledType resultType) {
        return resultParentRawJavaTypeReference(resultType) + ".Error";
    }

    private static String resultParentJavaTypeReference(dev.capylang.compiler.CompiledType resultType) {
        if (resultType instanceof dev.capylang.compiler.CompiledDataParentType parentType) {
            var raw = resultParentRawJavaTypeReference(resultType);
            if (parentType.typeParameters().isEmpty()) {
                return raw;
            }
            var mappedTypeParameters = parentType.typeParameters().stream()
                    .map(JavaExpressionEvaluator::javaCastTypeFromDescriptor)
                    .toList();
            return raw + "<" + String.join(", ", mappedTypeParameters) + ">";
        }
        return resultParentRawJavaTypeReference(resultType);
    }

    private static String resultParentRawJavaTypeReference(dev.capylang.compiler.CompiledType resultType) {
        if (resultType instanceof dev.capylang.compiler.CompiledDataParentType parentType) {
            var normalized = normalizeQualifiedTypeName(parentType.name());
            if (normalized.equals("/cap/lang/Result.Result") || normalized.endsWith("/cap/lang/Result.Result")) {
                return "cap.lang.Result";
            }
            if (normalized.equals("/capy/lang/Result.Result") || normalized.endsWith("/capy/lang/Result.Result")) {
                return "capy.lang.Result";
            }
            if ("Result".equals(parentType.name())) {
                return "capy.lang.Result";
            }
            var normalizedType = normalizeJavaTypeReference(parentType.name());
            return normalizedType.endsWith(".Result") ? normalizedType : normalizedType + ".Result";
        }
        return "capy.lang.Result";
    }

}
