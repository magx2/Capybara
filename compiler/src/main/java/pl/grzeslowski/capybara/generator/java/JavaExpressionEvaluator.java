package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.linker.expression.*;
import pl.grzeslowski.capybara.linker.LinkedDataParentType;
import pl.grzeslowski.capybara.linker.LinkedDataType;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;

import static java.lang.System.lineSeparator;

@SuppressWarnings("SwitchStatementWithTooFewBranches")
public class JavaExpressionEvaluator {
    private static final java.util.concurrent.atomic.AtomicLong OPTION_CASE_VAR_COUNTER =
            new java.util.concurrent.atomic.AtomicLong();
    private static final Logger log = Logger.getLogger(JavaExpressionEvaluator.class.getName());
    private static final String DICT_PIPE_ARGS_SEPARATOR = "::";
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final java.util.Set<String> JAVA_KEYWORDS = java.util.Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );

    public static String evaluateExpression(LinkedExpression expression) {
        log.fine(() -> "evaluateExpression: " + expression.getClass().getSimpleName() + " -> " + expression);
        var scope = evaluateExpression(expression, Scope.EMPTY);
        return expressionToJava(scope);
    }

    public static String evaluateExpression(LinkedExpression expression, List<JavaMethod.JavaFunctionParameter> parameters) {
        log.fine(() -> "evaluateExpression: " + expression.getClass().getSimpleName() + " -> " + expression);
        var scope = Scope.EMPTY;
        for (var parameter : parameters) {
            scope = scope.addLocalValue(parameter.sourceName())
                    .addValueOverride(parameter.sourceName(), parameter.generatedName());
        }
        var evaluatedScope = evaluateExpression(expression, scope);
        return expressionToJava(evaluatedScope);
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

    private static Scope evaluateExpression(LinkedExpression expression, Scope scope) {
        return switch (expression) {
            case LinkedBooleanValue booleanValue -> evaluateBooleanValue(booleanValue, scope);
            case LinkedByteValue byteValue -> evaluateByteValue(byteValue, scope);
            case LinkedDoubleValue doubleValue -> evaluateDoubleValue(doubleValue, scope);
            case LinkedFieldAccess fieldAccess -> evaluateFieldAccess(fieldAccess, scope);
            case LinkedFloatValue floatValue -> evaluateFloatValue(floatValue, scope);
            case LinkedFunctionCall functionCall -> evaluateFunctionCall(functionCall, scope);
            case LinkedFunctionInvoke functionInvoke -> evaluateFunctionInvoke(functionInvoke, scope);
            case LinkedIfExpression ifExpression -> evaluateIfExpression(ifExpression, scope);
            case LinkedIndexExpression indexExpression -> evaluateIndexExpression(indexExpression, scope);
            case LinkedInfixExpression infixExpression -> evaluateInfixExpression(infixExpression, scope);
            case LinkedIntValue intValue -> evaluateIntValue(intValue, scope);
            case LinkedLambdaExpression lambdaExpression -> evaluateLambdaExpression(lambdaExpression, scope);
            case LinkedLetExpression letExpression -> evaluateLetExpression(letExpression, scope);
            case LinkedLongValue longValue -> evaluateLongValue(longValue, scope);
            case LinkedMatchExpression matchExpression -> evaluateMatchExpression(matchExpression, scope);
            case LinkedNothingValue nothingValue -> evaluateNothingValue(nothingValue, scope);
            case LinkedPipeAllExpression pipeAllExpression -> evaluatePipeMatchExpression(pipeAllExpression.source(), pipeAllExpression.argumentName(), pipeAllExpression.predicate(), scope, "allMatch");
            case LinkedPipeAnyExpression pipeAnyExpression -> evaluatePipeMatchExpression(pipeAnyExpression.source(), pipeAnyExpression.argumentName(), pipeAnyExpression.predicate(), scope, "anyMatch");
            case LinkedPipeFlatMapExpression pipeFlatMapExpression -> evaluatePipeFlatMapExpression(pipeFlatMapExpression, scope);
            case LinkedPipeFilterOutExpression pipeFilterOutExpression -> evaluatePipeFilterOutExpression(pipeFilterOutExpression, scope);
            case LinkedPipeExpression pipeExpression -> evaluatePipeExpression(pipeExpression, scope);
            case LinkedPipeReduceExpression pipeReduceExpression -> evaluatePipeReduceExpression(pipeReduceExpression, scope);
            case LinkedSliceExpression sliceExpression -> evaluateSliceExpression(sliceExpression, scope);
            case LinkedTupleExpression tupleExpression -> evaluateTupleExpression(tupleExpression, scope);
            case LinkedNewDict newDict -> evaluateNewDict(newDict, scope);
            case LinkedNewList newList -> evaluateNewList(newList, scope);
            case LinkedNewSet newSet -> evaluateNewSet(newSet, scope);
            case LinkedNewData newData -> evaluateNewData(newData, scope);
            case LinkedStringValue stringValue -> evaluateStringValue(stringValue, scope);
            case LinkedVariable variable -> evaluateVariable(variable, scope);
        };
    }

    private static Scope evaluateBooleanValue(LinkedBooleanValue booleanValue, Scope scope) {
        return scope.addExpression(booleanValue.toString());
    }

    private static Scope evaluateByteValue(LinkedByteValue byteValue, Scope scope) {
        return scope.addExpression("((byte) " + byteValue.byteValue() + ")");
    }

    private static Scope evaluateDoubleValue(LinkedDoubleValue doubleValue, Scope scope) {
        return scope.addExpression(doubleValue.doubleValue());
    }

    private static Scope evaluateFloatValue(LinkedFloatValue floatValue, Scope scope) {
        return scope.addExpression(ensureFloatSuffix(floatValue.floatValue()));
    }

    private static Scope evaluateFieldAccess(LinkedFieldAccess fieldAccess, Scope scope) {
        var source = evaluateExpression(fieldAccess.source(), scope).popExpression();
        var expression = "(" + source.expression() + ")." + fieldAccess.field() + "()";
        if (requiresGenericFieldCast(fieldAccess)) {
            var castType = javaCastType(fieldAccess.type());
            if (!"java.lang.Object".equals(castType)) {
                expression = "((" + castType + ") (" + expression + "))";
            }
        }
        return source.scope().addExpression(expression);
    }

    private static boolean requiresGenericFieldCast(LinkedFieldAccess fieldAccess) {
        return switch (fieldAccess.source().type()) {
            case pl.grzeslowski.capybara.linker.LinkedDataType linkedDataType ->
                    !linkedDataType.typeParameters().isEmpty();
            case pl.grzeslowski.capybara.linker.LinkedDataParentType linkedDataParentType ->
                    !linkedDataParentType.typeParameters().isEmpty();
            default -> false;
        };
    }

    private static Scope evaluateFunctionCall(LinkedFunctionCall functionCall, Scope scope) {
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
            var receiver = args.get(0);
            var invokeArgs = args.size() > 1 ? String.join(", ", args.subList(1, args.size())) : "";
            return current.addExpression(receiver + "." + normalizeJavaMethodName(methodName) + "(" + invokeArgs + ")");
        }

        var expression = switch (functionCall.name()) {
            case "sqrt" -> {
                if (args.size() != 1) {
                    throw new IllegalStateException("sqrt expects exactly one argument");
                }
                yield "((float) java.lang.Math.sqrt(" + args.get(0) + "))";
            }
            default -> normalizeFunctionCallTarget(functionCall.name()) + "(" + String.join(", ", args) + ")";
        };
        return current.addExpression(expression);
    }

    private static Scope evaluateFunctionInvoke(LinkedFunctionInvoke functionInvoke, Scope scope) {
        var functionExSc = evaluateExpression(functionInvoke.function(), scope).popExpression();
        var current = functionExSc.scope();
        var call = new StringBuilder(functionExSc.expression());
        for (var argument : functionInvoke.arguments()) {
            var argumentScope = evaluateExpression(argument, current).popExpression();
            current = argumentScope.scope();
            call.append(".apply(").append(argumentScope.expression()).append(")");
        }
        return current.addExpression(call.toString());
    }

    private static Scope evaluateIfExpression(LinkedIfExpression expression, Scope scope) {
        var condition = evaluateExpression(expression.condition(), scope).popExpression();
        var then = evaluateExpression(expression.thenBranch(), condition.scope()).popExpression();
        var elseExSc = evaluateExpression(expression.elseBranch(), then.scope()).popExpression();

        var conditionBoolean = toBooleanExpression(condition.expression(), expression.condition().type());
        return elseExSc.scope()
                .addExpression("(%s) ? (%s) : (%s)".formatted(
                        conditionBoolean,
                        then.expression(),
                        elseExSc.expression()));
    }

    private static Scope evaluateInfixExpression(LinkedInfixExpression infixExpression, Scope scope) {
        var left = evaluateExpression(infixExpression.left(), scope).popExpression();
        var right = evaluateExpression(infixExpression.right(), left.scope()).popExpression();

        var operator = infixExpression.operator();
        var expression = switch (operator) {
            case POWER -> evaluatePowerExpression(infixExpression, left.expression(), right.expression());
            case PLUS -> {
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList) {
                    yield evaluateListAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet) {
                    yield evaluateSetAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
                    yield evaluateDictAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
                    yield stringConcatOperand(left.expression(), infixExpression.left().type())
                          + operator.symbol()
                          + stringConcatOperand(right.expression(), infixExpression.right().type());
                }
                yield left.expression() + operator.symbol() + right.expression();
            }
            case MINUS -> {
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList) {
                    yield evaluateListRemoveExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet) {
                    yield evaluateSetRemoveExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
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
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList) {
                    yield left.expression() + ".contains(" + right.expression() + ")";
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet) {
                    yield left.expression() + ".contains(" + right.expression() + ")";
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
                    yield left.expression() + ".containsKey(" + right.expression() + ")";
                }
                if (infixExpression.left().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING
                    && infixExpression.right().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
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
                    yield operator == pl.grzeslowski.capybara.parser.InfixOperator.EQUAL
                            ? equalsExpression
                            : "!(" + equalsExpression + ")";
                }
                yield left.expression() + operator.javaSymbol() + right.expression();
            }
            default -> left.expression() + operator.javaSymbol() + right.expression();
        };

        return right.scope().addExpression('(' + castIfNeeded(infixExpression.type(), expression) + ')');
    }

    private static String evaluatePowerExpression(LinkedInfixExpression infixExpression, String left, String right) {
        if (isStringLeftNumericRight(infixExpression)) {
            return left + "+" + right;
        }
        if (infixExpression.type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT
            && infixExpression.left().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT
            && infixExpression.right().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT) {
            return "pl.grzeslowski.capybara.CapybaraUtil.power(" + left + ", " + right + ")";
        }
        return switch (infixExpression.type()) {
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BYTE -> "((byte) java.lang.Math.pow(" + left + ", " + right + "))";
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT -> "((int) java.lang.Math.pow(" + left + ", " + right + "))";
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType.LONG -> "((long) java.lang.Math.pow(" + left + ", " + right + "))";
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType.FLOAT -> "((float) java.lang.Math.pow(" + left + ", " + right + "))";
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DOUBLE -> "java.lang.Math.pow(" + left + ", " + right + ")";
            default -> "java.lang.Math.pow(" + left + ", " + right + ")";
        };
    }

    private static String castIfNeeded(pl.grzeslowski.capybara.linker.LinkedType type, String expression) {
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BYTE) {
            return "((byte) (" + expression + "))";
        }
        return expression;
    }

    private static boolean isStringLeftNumericRight(LinkedInfixExpression infixExpression) {
        if (infixExpression.left().type() != pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
            return false;
        }
        return switch (infixExpression.right().type()) {
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BYTE,
                 pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT,
                 pl.grzeslowski.capybara.linker.PrimitiveLinkedType.LONG,
                 pl.grzeslowski.capybara.linker.PrimitiveLinkedType.FLOAT,
                 pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DOUBLE -> true;
            default -> false;
        };
    }

    private static boolean isBooleanCoercionComparison(LinkedInfixExpression infixExpression) {
        var leftType = infixExpression.left().type();
        var rightType = infixExpression.right().type();
        var leftIsBool = leftType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BOOL;
        var rightIsBool = rightType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BOOL;
        return (leftIsBool && isBooleanConvertibleType(rightType))
               || (rightIsBool && isBooleanConvertibleType(leftType));
    }

    private static boolean isStringComparison(LinkedInfixExpression infixExpression) {
        return infixExpression.left().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING
               || infixExpression.right().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING;
    }

    private static boolean isBooleanConvertibleType(pl.grzeslowski.capybara.linker.LinkedType type) {
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BOOL) {
            return true;
        }
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
            return true;
        }
        if (type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList
            || type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet
            || type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            return true;
        }
        return type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BYTE
               || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT
               || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.LONG
               || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.FLOAT
               || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DOUBLE;
    }

    private static String toBooleanExpression(String expression, pl.grzeslowski.capybara.linker.LinkedType type) {
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BOOL) {
            return expression;
        }
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
            return "(!(" + expression + ").isEmpty())";
        }
        if (type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList
            || type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet
            || type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            return "(!(" + expression + ").isEmpty())";
        }
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BYTE
            || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.INT) {
            return "((" + expression + ") != 0)";
        }
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.LONG) {
            return "((" + expression + ") != 0L)";
        }
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.FLOAT) {
            return "((" + expression + ") != 0f)";
        }
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DOUBLE) {
            return "((" + expression + ") != 0d)";
        }
        return "(" + expression + ")";
    }

    private static String evaluateListAppendExpression(LinkedInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList) {
            return "java.util.stream.Stream.concat(" + left + ".stream(), " + right + ".stream()).toList()";
        }
        return "java.util.stream.Stream.concat(" + left + ".stream(), java.util.stream.Stream.of(" + right + ")).toList()";
    }

    private static String evaluateSetAppendExpression(LinkedInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet) {
            return "java.util.stream.Stream.concat(" + left + ".stream(), " + right + ".stream())"
                   + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
        }
        return "java.util.stream.Stream.concat(" + left + ".stream(), java.util.stream.Stream.of(" + right + "))"
               + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
    }

    private static String evaluateDictAppendExpression(LinkedInfixExpression infixExpression, String left, String right) {
        var valueCastType = dictValueCastType(infixExpression.type());
        return "java.util.stream.Stream.concat(" + left + ".entrySet().stream(), " + right + ".entrySet().stream())"
               + ".collect(java.util.stream.Collectors.toMap("
               + "entry -> ((java.lang.String) ((java.util.Map.Entry<?, ?>) entry).getKey()), "
               + "entry -> ((" + valueCastType + ") ((java.util.Map.Entry<?, ?>) entry).getValue()), "
               + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))";
    }

    private static String stringConcatOperand(String expression, pl.grzeslowski.capybara.linker.LinkedType type) {
        if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
            return expression;
        }
        if (isOptionType(type)) {
            return expression + ".map(java.lang.String::valueOf).orElse(\"\")";
        }
        return "java.lang.String.valueOf(" + expression + ")";
    }

    private static String evaluateListRemoveExpression(LinkedInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList) {
            return left + ".stream().filter(v -> !" + right + ".contains(v)).toList()";
        }
        return left + ".stream().filter(v -> !java.util.Objects.equals(v, " + right + ")).toList()";
    }

    private static String evaluateSetRemoveExpression(LinkedInfixExpression infixExpression, String left, String right) {
        if (infixExpression.right().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet) {
            return left + ".stream().filter(v -> !" + right + ".contains(v))"
                   + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
        }
        return left + ".stream().filter(v -> !java.util.Objects.equals(v, " + right + "))"
               + ".collect(java.util.stream.Collectors.toUnmodifiableSet())";
    }

    private static String evaluateDictRemoveExpression(LinkedInfixExpression infixExpression, String left, String right) {
        var valueCastType = dictValueCastType(infixExpression.type());
        if (infixExpression.right().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
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

    private static String dictValueCastType(pl.grzeslowski.capybara.linker.LinkedType type) {
        if (type instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict linkedDict) {
            return javaCastType(linkedDict.valueType());
        }
        return "java.lang.Object";
    }

    private static Scope evaluateIntValue(LinkedIntValue intValue, Scope scope) {
        return scope.addExpression(intValue.intValue());
    }

    private static Scope evaluateLongValue(LinkedLongValue longValue, Scope scope) {
        return scope.addExpression(longValue.longValue());
    }

    private static Scope evaluateLambdaExpression(LinkedLambdaExpression lambdaExpression, Scope scope) {
        var bodyExSc = evaluateExpression(
                lambdaExpression.expression(),
                scope.addLocalValue(lambdaExpression.argumentName())
        ).popExpression();
        return bodyExSc.scope().addExpression(lambdaExpression.argumentName() + " -> (" + bodyExSc.expression() + ")");
    }

    private static Scope evaluatePipeExpression(LinkedPipeExpression pipeExpression, Scope scope) {
        if (isOptionType(pipeExpression.type())) {
            return evaluateOptionPipeExpression(pipeExpression, scope);
        }
        if (pipeExpression.type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict
            && pipeExpression.source().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            return evaluateDictPipeExpression(pipeExpression, scope);
        }
        var streamExSc = evaluatePipeExpressionAsStream(pipeExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeExpression.type()));
    }

    private static Scope evaluateDictPipeExpression(LinkedPipeExpression pipeExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
        var entryVar = "__entry";
        var dictArgs = parseDictPipeArguments(pipeExpression.argumentName());

        Scope mapperScope;
        if (dictArgs.length == 2) {
            mapperScope = sourceExSc.scope()
                    .addValueOverride(dictArgs[0], entryVar + ".getKey()")
                    .addValueOverride(dictArgs[1], entryVar + ".getValue()");
        } else {
            mapperScope = sourceExSc.scope()
                    .addValueOverride(pipeExpression.argumentName(), entryVar + ".getValue()");
        }

        var mapperExSc = evaluateExpression(pipeExpression.mapper(), mapperScope).popExpression();
        return mapperExSc.scope().addExpression(
                sourceExSc.expression()
                + ".entrySet().stream().collect(java.util.stream.Collectors.toMap("
                + entryVar + " -> " + entryVar + ".getKey(), "
                + entryVar + " -> (" + mapperExSc.expression() + "), "
                + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))"
        );
    }

    private static Scope evaluatePipeFlatMapExpression(LinkedPipeFlatMapExpression pipeFlatMapExpression, Scope scope) {
        var streamExSc = evaluatePipeFlatMapExpressionAsStream(pipeFlatMapExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeFlatMapExpression.type()));
    }

    private static Scope evaluatePipeFilterOutExpression(LinkedPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        if (isOptionType(pipeFilterOutExpression.type())) {
            return evaluateOptionPipeFilterOutExpression(pipeFilterOutExpression, scope);
        }
        if (pipeFilterOutExpression.type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict
            && pipeFilterOutExpression.source().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            return evaluateDictPipeFilterOutExpression(pipeFilterOutExpression, scope);
        }
        var streamExSc = evaluatePipeFilterOutExpressionAsStream(pipeFilterOutExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeFilterOutExpression.type()));
    }

    private static Scope evaluatePipeMatchExpression(
            LinkedExpression source,
            String argumentName,
            LinkedExpression predicate,
            Scope scope,
            String matchMethod
    ) {
        if (source.type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict
            && argumentName.contains(DICT_PIPE_ARGS_SEPARATOR)) {
            var sourceExSc = evaluateExpression(source, scope).popExpression();
            var entryVar = "__entry";
            var dictArgs = parseDictPipeArguments(argumentName);
            var predicateScope = sourceExSc.scope()
                    .addValueOverride(dictArgs[0], entryVar + ".getKey()")
                    .addValueOverride(dictArgs[1], entryVar + ".getValue()");
            var predicateExSc = evaluateExpression(predicate, predicateScope).popExpression();
            return predicateExSc.scope().addExpression(
                    sourceExSc.expression()
                    + ".entrySet().stream()." + matchMethod + "(" + entryVar + " -> (" + predicateExSc.expression() + "))"
            );
        }

        var sourceStreamExSc = evaluateSourceAsStream(source, scope);
        var predicateExSc = evaluateExpression(
                predicate,
                sourceStreamExSc.scope().addLocalValue(argumentName)
        ).popExpression();
        return predicateExSc.scope().addExpression(
                sourceStreamExSc.streamExpression() + "." + matchMethod + "(" + argumentName + " -> (" + predicateExSc.expression() + "))"
        );
    }

    private static Scope evaluateDictPipeFilterOutExpression(LinkedPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeFilterOutExpression.source(), scope).popExpression();
        var entryVar = "__entry";
        var dictArgs = parseDictPipeArguments(pipeFilterOutExpression.argumentName());

        Scope predicateScope;
        if (dictArgs.length == 2) {
            predicateScope = sourceExSc.scope()
                    .addValueOverride(dictArgs[0], entryVar + ".getKey()")
                    .addValueOverride(dictArgs[1], entryVar + ".getValue()");
        } else {
            predicateScope = sourceExSc.scope()
                    .addValueOverride(pipeFilterOutExpression.argumentName(), entryVar + ".getValue()");
        }
        var predicateExSc = evaluateExpression(pipeFilterOutExpression.predicate(), predicateScope).popExpression();
        return predicateExSc.scope().addExpression(
                sourceExSc.expression()
                + ".entrySet().stream()"
                + ".filter(" + entryVar + " -> !(" + predicateExSc.expression() + "))"
                + ".collect(java.util.stream.Collectors.toMap("
                + entryVar + " -> " + entryVar + ".getKey(), "
                + entryVar + " -> " + entryVar + ".getValue(), "
                + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))"
        );
    }

    private static Scope evaluateOptionPipeExpression(LinkedPipeExpression pipeExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
        var mapperExSc = evaluateExpression(
                pipeExpression.mapper(),
                sourceExSc.scope().addLocalValue(pipeExpression.argumentName())
        ).popExpression();
        return mapperExSc.scope().addExpression(
                sourceExSc.expression()
                + ".map(" + pipeExpression.argumentName() + " -> (" + mapperExSc.expression() + "))"
        );
    }

    private static Scope evaluateOptionPipeFilterOutExpression(LinkedPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeFilterOutExpression.source(), scope).popExpression();
        var predicateExSc = evaluateExpression(
                pipeFilterOutExpression.predicate(),
                sourceExSc.scope().addLocalValue(pipeFilterOutExpression.argumentName())
        ).popExpression();

        var sourceExpression = isOptionType(pipeFilterOutExpression.source().type())
                               || isOptionConstructor(pipeFilterOutExpression.source())
                ? sourceExSc.expression()
                : "java.util.Optional.of(" + sourceExSc.expression() + ")";

        return predicateExSc.scope().addExpression(
                sourceExpression
                + ".filter(" + pipeFilterOutExpression.argumentName() + " -> !(" + predicateExSc.expression() + "))"
        );
    }

    private static Scope evaluatePipeReduceExpression(LinkedPipeReduceExpression pipeReduceExpression, Scope scope) {
        if (pipeReduceExpression.source().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict
            && pipeReduceExpression.keyName().isPresent()) {
            var sourceExSc = evaluateExpression(pipeReduceExpression.source(), scope).popExpression();
            var initialExSc = evaluateExpression(pipeReduceExpression.initialValue(), sourceExSc.scope()).popExpression();
            var entryVar = "__entry";
            var keyName = pipeReduceExpression.keyName().orElseThrow();
            if (pipeReduceExpression.accumulatorName().contains("::")
                && pipeReduceExpression.initialValue().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
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
                var dictValueType = ((pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) pipeReduceExpression.source().type())
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
            if (pipeReduceExpression.initialValue().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
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
            return reducerExSc.scope().addExpression(
                    sourceExSc.expression()
                    + ".entrySet().stream().reduce("
                    + initialExSc.expression()
                    + ", (" + pipeReduceExpression.accumulatorName()
                    + ", " + entryVar
                    + ") -> (" + reducerExSc.expression() + ")"
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

        var maybeElementType = streamElementType(pipeReduceExpression.source().type());
        if (maybeElementType.isPresent() && maybeElementType.get().equals(pipeReduceExpression.initialValue().type())) {
            var reducedValueName = "__capybaraReducedValue";
            var maybeMapPrefix = pipeReduceExpression.initialValue().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING
                    && !"\"\"".equals(initialExSc.expression())
                    ? ".map(" + reducedValueName + " -> (" + initialExSc.expression() + "+" + reducedValueName + "))"
                    : "";
            return reducerExSc.scope().addExpression(
                    sourceStreamExSc.streamExpression()
                    + ".reduce("
                    + "(" + pipeReduceExpression.accumulatorName()
                    + ", " + pipeReduceExpression.valueName()
                    + ") -> (" + reducerExSc.expression() + "))"
                    + maybeMapPrefix
                    + ".orElse("
                    + initialExSc.expression()
                    + ")"
            );
        }

        return reducerExSc.scope().addExpression(
                sourceStreamExSc.streamExpression()
                + ".reduce("
                + initialExSc.expression()
                + ", (" + pipeReduceExpression.accumulatorName()
                + ", " + pipeReduceExpression.valueName()
                + ") -> (" + reducerExSc.expression() + ")"
                + ", (left, right) -> left)"
        );
    }

    private static java.util.Optional<pl.grzeslowski.capybara.linker.LinkedType> streamElementType(pl.grzeslowski.capybara.linker.LinkedType sourceType) {
        return switch (sourceType) {
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList linkedList ->
                    java.util.Optional.of(linkedList.elementType());
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet linkedSet ->
                    java.util.Optional.of(linkedSet.elementType());
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict linkedDict ->
                    java.util.Optional.of(linkedDict.valueType());
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType primitive when primitive == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING ->
                    java.util.Optional.of(pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING);
            default -> java.util.Optional.empty();
        };
    }

    private static StreamExpressionScope evaluatePipeExpressionAsStream(LinkedPipeExpression pipeExpression, Scope scope) {
        if (pipeExpression.source().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict
            && pipeExpression.argumentName().contains("::")) {
            var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
            var dictArgs = parseDictPipeArguments(pipeExpression.argumentName());
            var keyName = dictArgs[0];
            var valueName = dictArgs[1];
            var entryVar = "__entry";
            var mapperExSc = evaluateExpression(
                    pipeExpression.mapper(),
                    sourceExSc.scope()
                            .addValueOverride(keyName, entryVar + ".getKey()")
                            .addValueOverride(valueName, entryVar + ".getValue()")
            ).popExpression();
            return new StreamExpressionScope(
                    sourceExSc.expression() + ".entrySet().stream().map(" + entryVar + " -> (" + mapperExSc.expression() + "))",
                    mapperExSc.scope().withoutValueOverrides()
            );
        }

        var sourceStreamExSc = evaluateSourceAsStream(pipeExpression.source(), scope);
        var mapperExSc = evaluateExpression(
                pipeExpression.mapper(),
                sourceStreamExSc.scope().addLocalValue(pipeExpression.argumentName())
        ).popExpression();
        var mapperExpression = mapperExSc.expression();
        if (pipeExpression.type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList listType
            && listType.elementType() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY) {
            mapperExpression = "(java.lang.Object) (" + mapperExpression + ")";
        }
        if (pipeExpression.type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet setType
            && setType.elementType() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY) {
            mapperExpression = "(java.lang.Object) (" + mapperExpression + ")";
        }
        return new StreamExpressionScope(
                sourceStreamExSc.streamExpression()
                + ".map(" + pipeExpression.argumentName() + " -> (" + mapperExpression + "))",
                mapperExSc.scope().withoutValueOverrides()
        );
    }

    private static StreamExpressionScope evaluatePipeFilterOutExpressionAsStream(LinkedPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceStreamExSc = evaluateSourceAsStream(pipeFilterOutExpression.source(), scope);
        var predicateExSc = evaluateExpression(
                pipeFilterOutExpression.predicate(),
                sourceStreamExSc.scope().addLocalValue(pipeFilterOutExpression.argumentName())
        ).popExpression();
        return new StreamExpressionScope(
                sourceStreamExSc.streamExpression()
                + ".filter(" + pipeFilterOutExpression.argumentName() + " -> !(" + predicateExSc.expression() + "))",
                predicateExSc.scope().withoutValueOverrides()
        );
    }

    private static StreamExpressionScope evaluatePipeFlatMapExpressionAsStream(LinkedPipeFlatMapExpression pipeFlatMapExpression, Scope scope) {
        var sourceStreamExSc = evaluateSourceAsStream(pipeFlatMapExpression.source(), scope);
        var mapperExSc = evaluateExpression(
                pipeFlatMapExpression.mapper(),
                sourceStreamExSc.scope().addLocalValue(pipeFlatMapExpression.argumentName())
        ).popExpression();

        var streamExtractor = switch (pipeFlatMapExpression.mapper().type()) {
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict ignored -> ".values().stream()";
            default -> ".stream()";
        };
        return new StreamExpressionScope(
                sourceStreamExSc.streamExpression()
                + ".flatMap(" + pipeFlatMapExpression.argumentName() + " -> (" + mapperExSc.expression() + ")" + streamExtractor + ")",
                mapperExSc.scope().withoutValueOverrides()
        );
    }

    private static StreamExpressionScope evaluateSourceAsStream(LinkedExpression source, Scope scope) {
        if (source instanceof LinkedPipeExpression pipeExpression) {
            return evaluatePipeExpressionAsStream(pipeExpression, scope);
        }
        if (source instanceof LinkedPipeFilterOutExpression pipeFilterOutExpression) {
            return evaluatePipeFilterOutExpressionAsStream(pipeFilterOutExpression, scope);
        }
        if (source instanceof LinkedPipeFlatMapExpression pipeFlatMapExpression) {
            return evaluatePipeFlatMapExpressionAsStream(pipeFlatMapExpression, scope);
        }

        var sourceExSc = evaluateExpression(source, scope).popExpression();
        var sourceExpression = sourceExSc.expression();
        if (source.type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING) {
            return new StreamExpressionScope(
                    sourceExpression + ".chars().mapToObj(__capybaraChar -> java.lang.String.valueOf((char) __capybaraChar))",
                    sourceExSc.scope()
            );
        }
        if (source.type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            sourceExpression = sourceExpression + ".values()";
        }
        return new StreamExpressionScope(sourceExpression + ".stream()", sourceExSc.scope());
    }

    private record StreamExpressionScope(String streamExpression, Scope scope) {
    }

    private static String terminalCollect(pl.grzeslowski.capybara.linker.LinkedType type) {
        return switch (type) {
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet ignored ->
                    ".collect(java.util.stream.Collectors.toSet())";
            default -> ".toList()";
        };
    }

    private static Scope evaluateLetExpression(LinkedLetExpression let, Scope scope) {
        var valueScope = evaluateExpression(let.value(), scope);
        var valueExSc = valueScope.popExpression();
        var scopeExpression = valueExSc.scope().declareValue(let.name(), valueExSc.expression(), let.rest());
        return evaluateExpression(scopeExpression.expression(), scopeExpression.scope());
    }

    private static Scope evaluateSliceExpression(LinkedSliceExpression expression, Scope scope) {
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
        var isString = expression.type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING;
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
        if (expression.type() instanceof pl.grzeslowski.capybara.linker.LinkedTupleType) {
            slice = "new java.util.ArrayList<java.lang.Object>(" + slice + ")";
        }
        return current.addExpression(slice);
    }

    private static Scope evaluateIndexExpression(LinkedIndexExpression expression, Scope scope) {
        var sourceExSc = evaluateExpression(expression.source(), scope).popExpression();
        var indexExSc = evaluateExpression(expression.index(), sourceExSc.scope()).popExpression();
        var source = sourceExSc.expression();
        var index = indexExSc.expression();
        var isString = expression.source().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING;
        var sizeExpression = "(" + source + ")." + (isString ? "length()" : "size()");
        var normalizedIndex = normalizeSliceIndex(index, sizeExpression);
        if (expression.source().type() instanceof pl.grzeslowski.capybara.linker.LinkedTupleType) {
            var castType = javaCastType(expression.type());
            return indexExSc.scope().addExpression("((" + castType + ") (" + source + ").get(" + normalizedIndex + "))");
        }
        var inRange = "(" + normalizedIndex + " >= 0 && " + normalizedIndex + " < " + sizeExpression + ")";
        var value = isString
                ? "java.lang.String.valueOf((" + source + ").charAt(" + normalizedIndex + "))"
                : "(" + source + ").get(" + normalizedIndex + ")";
        return indexExSc.scope().addExpression("(" + inRange + " ? java.util.Optional.of(" + value + ") : java.util.Optional.empty())");
    }

    private static Scope evaluateTupleExpression(LinkedTupleExpression expression, Scope scope) {
        var current = scope;
        var values = new ArrayList<String>(expression.values().size());
        for (var value : expression.values()) {
            var valueExSc = evaluateExpression(value, current).popExpression();
            current = valueExSc.scope();
            values.add(valueExSc.expression());
        }
        return current.addExpression("java.util.List.of(" + String.join(", ", values) + ")");
    }

    private static String normalizeSliceIndex(String indexExpression, String sizeExpression) {
        return "((" + indexExpression + ") < 0 ? (" + sizeExpression + " + (" + indexExpression + ")) : (" + indexExpression + "))";
    }

    private static String javaCastType(pl.grzeslowski.capybara.linker.LinkedType type) {
        return switch (type) {
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType primitive -> switch (primitive) {
                case BYTE -> "java.lang.Byte";
                case INT -> "java.lang.Integer";
                case LONG -> "java.lang.Long";
                case DOUBLE -> "java.lang.Double";
                case STRING -> "java.lang.String";
                case BOOL -> "java.lang.Boolean";
                case FLOAT -> "java.lang.Float";
                case NOTHING, ANY, DATA -> "java.lang.Object";
            };
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList linkedList ->
                    "java.util.List<" + javaCastType(linkedList.elementType()) + ">";
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet linkedSet ->
                    "java.util.Set<" + javaCastType(linkedSet.elementType()) + ">";
            case pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict linkedDict ->
                    "java.util.Map<java.lang.String, " + javaCastType(linkedDict.valueType()) + ">";
            case pl.grzeslowski.capybara.linker.LinkedTupleType ignored -> "java.util.List<?>";
            case pl.grzeslowski.capybara.linker.LinkedDataType linkedDataType ->
                    normalizeJavaTypeReference(linkedDataType.name());
            case pl.grzeslowski.capybara.linker.LinkedDataParentType linkedDataParentType ->
                    normalizeJavaTypeReference(linkedDataParentType.name());
            case pl.grzeslowski.capybara.linker.LinkedGenericTypeParameter ignored -> "java.lang.Object";
            default -> "java.lang.Object";
        };
    }

    private static Scope evaluateMatchExpression(LinkedMatchExpression matchExpression, Scope scope) {
        var matchSelectorName = "__matchValue";
        var optionMatch = isOptionType(matchExpression.matchWith().type());
        var matchWithExSc = evaluateExpression(matchExpression.matchWith(), scope).popExpression();
        var selectorExpression = castMatchSelectorExpression(matchWithExSc.expression(), matchExpression.matchWith().type(), optionMatch);
        var declaredValue = matchWithExSc.scope().declareValue(
                matchSelectorName,
                selectorExpression,
                new LinkedVariable(matchSelectorName, matchExpression.matchWith().type())
        );
        var current = declaredValue.scope();
        var switchTarget = current.findValueOverride(matchSelectorName).orElse(matchSelectorName);
        var cases = new ArrayList<String>(matchExpression.cases().size());

        for (int caseIndex = 0; caseIndex < matchExpression.cases().size(); caseIndex++) {
            var matchCase = matchExpression.cases().get(caseIndex);
            var branchScope = current;
            var optionCaseVar = "__capybaraOptionCase" + OPTION_CASE_VAR_COUNTER.incrementAndGet();
            if (matchCase.pattern() instanceof LinkedMatchExpression.ConstructorPattern constructorPattern) {
                var bindingNames = constructorPatternBindingNames(constructorPattern);
                var bindingCastTypes = constructorBindingCastTypes(matchExpression.matchWith().type(), constructorPattern);
                for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
                    var fieldPattern = constructorPattern.fieldPatterns().get(i);
                    var bindingName = bindingNames.get(i);
                    if (fieldPattern instanceof LinkedMatchExpression.VariablePattern variablePattern) {
                        branchScope = branchScope.addLocalValue(variablePattern.name());
                        var castType = bindingCastTypes.get(i);
                        if (castType != null && !"java.lang.Object".equals(castType)) {
                            branchScope = branchScope.addValueOverride(variablePattern.name(), "((" + castType + ") " + bindingName + ")");
                        }
                    }
                    if (fieldPattern instanceof LinkedMatchExpression.TypedPattern typedPattern) {
                        branchScope = branchScope.addLocalValue(typedPattern.name());
                        branchScope = branchScope.addValueOverride(
                                typedPattern.name(),
                                "((" + javaPatternType(typedPattern.type()) + ") " + bindingName + ")"
                        );
                    }
                }
                if (optionMatch && isOptionSomePattern(constructorPattern.constructorName()) && constructorPattern.fieldPatterns().size() == 1) {
                    var firstPattern = constructorPattern.fieldPatterns().getFirst();
                    if (firstPattern instanceof LinkedMatchExpression.VariablePattern variablePattern) {
                        branchScope = branchScope.addValueOverride(
                                variablePattern.name(),
                                optionSomeBindingExpression(switchTarget + ".orElse(null)")
                        );
                    }
                    if (firstPattern instanceof LinkedMatchExpression.TypedPattern typedPattern) {
                        branchScope = branchScope.addValueOverride(
                                typedPattern.name(),
                                "((" + javaPatternType(typedPattern.type()) + ") " + optionSomeBindingExpression(switchTarget + ".orElse(null)") + ")"
                        );
                    }
                }
                if (isResultErrorConstructor(matchExpression.matchWith().type(), constructorPattern)
                    && constructorPattern.fieldPatterns().size() == 1
                    && constructorPattern.fieldPatterns().getFirst() instanceof LinkedMatchExpression.VariablePattern variablePattern) {
                    var valueName = variablePattern.name();
                    branchScope = branchScope.addValueOverride(valueName, "((" + valueName + ") == null ? null : " + valueName + ".getMessage())");
                }
            }
            if (matchCase.pattern() instanceof LinkedMatchExpression.TypedPattern typedPattern
                && matchExpression.matchWith() instanceof LinkedVariable matchedVariable) {
                branchScope = branchScope
                        .addLocalValue(typedPattern.name())
                        .addValueOverride(matchedVariable.name(), typedPattern.name());
            }
            var expressionScope = evaluateExpression(matchCase.expression(), branchScope).popExpression();
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
            var caseStatements = expressionScope.scope().getStatements()
                    .subList(branchScope.getStatements().size(), expressionScope.scope().getStatements().size());
            var caseStatementsCode = String.join(" ", caseStatements.stream()
                    .map(statement -> statement + ";")
                    .toList());
            var caseRule = caseStatements.isEmpty()
                    ? matchCasePattern(matchCase.pattern(), matchExpression.matchWith().type(), optionCaseVar) + " -> (" + caseExpression + ");"
                    : matchCasePattern(matchCase.pattern(), matchExpression.matchWith().type(), optionCaseVar)
                      + " -> { " + caseStatementsCode + " yield (" + caseExpression + "); }";
            cases.add(caseRule);
        }

        var hasWildcard = matchExpression.cases().stream()
                .map(LinkedMatchExpression.MatchCase::pattern)
                .anyMatch(LinkedMatchExpression.WildcardPattern.class::isInstance);
        if (!hasWildcard) {
            cases.add("default -> throw new java.lang.IllegalStateException(\"Unexpected value: \" + " + switchTarget + ");");
        }

        return current.addExpression("switch (" + switchTarget + ") { " + String.join(" ", cases) + " }");
    }

    private static List<String> constructorBindingCastTypes(
            pl.grzeslowski.capybara.linker.LinkedType matchType,
            LinkedMatchExpression.ConstructorPattern constructorPattern
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
                    if (fieldType instanceof pl.grzeslowski.capybara.linker.LinkedGenericTypeParameter genericTypeParameter) {
                        return genericCasts.get(genericTypeParameter.name());
                    }
                    return javaCastType(fieldType);
                })
                .toList();
    }

    private static LinkedDataType resolveConstructorType(
            pl.grzeslowski.capybara.linker.LinkedType matchType,
            String constructorName
    ) {
        if (matchType instanceof LinkedDataType linkedDataType) {
            return typeNameMatches(linkedDataType.name(), constructorName) ? linkedDataType : null;
        }
        if (matchType instanceof LinkedDataParentType parentType) {
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
            pl.grzeslowski.capybara.linker.LinkedType matchType,
            LinkedMatchExpression.ConstructorPattern constructorPattern
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

    private static java.util.Map<String, String> resolveGenericTypeCasts(
            pl.grzeslowski.capybara.linker.LinkedType matchType,
            LinkedDataType constructorType
    ) {
        if (!(matchType instanceof LinkedDataParentType parentType)
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
        if (normalized.startsWith("list[") || normalized.startsWith("Tuple[")) {
            return "java.util.List";
        }
        if (normalized.startsWith("set[")) {
            return "java.util.Set";
        }
        if (normalized.startsWith("dict[")) {
            return "java.util.Map";
        }
        var genericStart = normalized.indexOf('[');
        if (genericStart > 0 && normalized.endsWith("]")) {
            var rawType = normalized.substring(0, genericStart).trim();
            if (!rawType.isEmpty()) {
                return normalizeJavaTypeReference(rawType);
            }
        }
        return switch (normalized.toLowerCase(java.util.Locale.ROOT)) {
            case "byte" -> "java.lang.Byte";
            case "int" -> "java.lang.Integer";
            case "long" -> "java.lang.Long";
            case "float" -> "java.lang.Float";
            case "double" -> "java.lang.Double";
            case "string" -> "java.lang.String";
            case "bool" -> "java.lang.Boolean";
            default -> "java.lang.Object";
        };
    }

    private static String castMatchSelectorExpression(
            String expression,
            pl.grzeslowski.capybara.linker.LinkedType selectorType,
            boolean optionMatch
    ) {
        if (optionMatch
            || selectorType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY
            || selectorType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DATA
            || selectorType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.NOTHING
            || selectorType instanceof pl.grzeslowski.capybara.linker.LinkedGenericTypeParameter
            || selectorType instanceof pl.grzeslowski.capybara.linker.LinkedDataType
            || selectorType instanceof pl.grzeslowski.capybara.linker.LinkedDataParentType) {
            return expression;
        }
        return "((" + javaPatternType(selectorType) + ") (" + expression + "))";
    }

    private static String matchCasePattern(
            LinkedMatchExpression.Pattern pattern,
            pl.grzeslowski.capybara.linker.LinkedType matchType,
            String optionCaseVar
    ) {
        if (isOptionType(matchType)) {
            return switch (pattern) {
                case LinkedMatchExpression.VariablePattern variablePattern when isOptionNonePattern(variablePattern.name()) ->
                        "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isEmpty()";
                case LinkedMatchExpression.TypedPattern typedPattern when isOptionSomePattern(typedPattern.type().name()) ->
                        "case java.util.Optional " + typedPattern.name() + " when " + typedPattern.name() + ".isPresent()";
                case LinkedMatchExpression.TypedPattern typedPattern when isOptionNonePattern(typedPattern.type().name()) ->
                        "case java.util.Optional " + typedPattern.name() + " when " + typedPattern.name() + ".isEmpty()";
                case LinkedMatchExpression.ConstructorPattern constructorPattern when isOptionSomePattern(constructorPattern.constructorName()) ->
                        optionSomeCasePattern(constructorPattern, optionCaseVar);
                case LinkedMatchExpression.ConstructorPattern constructorPattern when isOptionNonePattern(constructorPattern.constructorName()) ->
                        "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isEmpty()";
                default -> "default";
            };
        }
        return switch (pattern) {
            case LinkedMatchExpression.IntPattern intPattern -> "case " + intPattern.value();
            case LinkedMatchExpression.StringPattern stringPattern -> "case " + stringPattern.value();
            case LinkedMatchExpression.BoolPattern boolPattern -> "case " + boolPattern.value();
            case LinkedMatchExpression.FloatPattern floatPattern -> "case " + floatPattern.value();
            case LinkedMatchExpression.TypedPattern typedPattern -> {
                if (typedPattern.type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DATA) {
                    yield "case java.lang.Object " + typedPattern.name() + " when " + dataGuard(typedPattern.name());
                }
                if (typedPattern.type() instanceof LinkedDataType typedDataType) {
                    var resolvedTypedType = resolveConstructorType(matchType, typedDataType.name());
                    var patternType = constructorPatternTypeName(matchType, resolvedTypedType, typedDataType.name());
                    yield "case " + patternType + " " + typedPattern.name();
                }
                yield "case " + javaPatternType(typedPattern.type()) + " " + typedPattern.name();
            }
            case LinkedMatchExpression.VariablePattern variablePattern -> "case " + variablePattern.name() + " __ignored";
            case LinkedMatchExpression.WildcardPattern wildcardPattern -> "default";
            case LinkedMatchExpression.ConstructorPattern constructorPattern -> {
                var constructorType = resolveConstructorType(matchType, constructorPattern.constructorName());
                var patternType = constructorPatternTypeName(matchType, constructorType, constructorPattern.constructorName());
                var bindingNames = constructorPatternBindingNames(constructorPattern);
                var constructorCasePattern = "case " + patternType + "("
                                             + bindingNames.stream().map(name -> "var " + name).reduce((a, b) -> a + ", " + b).orElse("")
                                             + ")";
                var guard = constructorPatternGuard(constructorPattern, bindingNames);
                yield guard.map(s -> constructorCasePattern + " when " + s).orElse(constructorCasePattern);
            }
        };
    }

    private static String constructorPatternTypeName(
            pl.grzeslowski.capybara.linker.LinkedType matchType,
            LinkedDataType constructorType,
            String constructorName
    ) {
        if (constructorType == null) {
            return unresolvedConstructorPatternType(constructorName);
        }
        if (constructorType.name().contains(".") || constructorType.name().contains("/")) {
            return normalizeJavaTypeReference(constructorType.name());
        }
        if (matchType instanceof LinkedDataParentType parentType) {
            var normalizedParent = normalizeQualifiedTypeName(parentType.name());
            if (normalizedParent.endsWith("/Result") || normalizedParent.endsWith(".Result") || "Result".equals(parentType.name())) {
                return "Result." + normalizeJavaClassName(constructorType.name());
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

    private static boolean isDirectOptionSomeVariableCase(LinkedMatchExpression.MatchCase matchCase) {
        if (!(matchCase.pattern() instanceof LinkedMatchExpression.ConstructorPattern constructorPattern)) {
            return false;
        }
        if (!isOptionSomePattern(constructorPattern.constructorName()) || constructorPattern.fieldPatterns().size() != 1) {
            return false;
        }
        if (!(constructorPattern.fieldPatterns().getFirst() instanceof LinkedMatchExpression.VariablePattern variablePattern)) {
            return false;
        }
        return matchCase.expression() instanceof LinkedVariable variable
               && variable.name().equals(variablePattern.name());
    }

    private static String optionSomeCasePattern(LinkedMatchExpression.ConstructorPattern constructorPattern, String optionCaseVar) {
        var base = "case java.util.Optional " + optionCaseVar + " when " + optionCaseVar + ".isPresent()";
        if (constructorPattern.fieldPatterns().size() != 1) {
            return base;
        }
        var fieldPattern = constructorPattern.fieldPatterns().getFirst();
        var guard = constructorFieldGuardExpression(fieldPattern, optionCaseVar + ".orElse(null)");
        return guard.map(s -> base + " && (" + s + ")").orElse(base);
    }

    private static List<String> constructorPatternBindingNames(LinkedMatchExpression.ConstructorPattern constructorPattern) {
        var names = new ArrayList<String>(constructorPattern.fieldPatterns().size());
        for (int i = 0; i < constructorPattern.fieldPatterns().size(); i++) {
            var fieldPattern = constructorPattern.fieldPatterns().get(i);
            if (fieldPattern instanceof LinkedMatchExpression.VariablePattern variablePattern) {
                names.add(variablePattern.name());
            } else {
                names.add("__capybaraCtorField" + i);
            }
        }
        return names;
    }

    private static java.util.Optional<String> constructorPatternGuard(
            LinkedMatchExpression.ConstructorPattern constructorPattern,
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
            LinkedMatchExpression.Pattern pattern,
            String valueExpression
    ) {
        return switch (pattern) {
            case LinkedMatchExpression.IntPattern intPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + intPattern.value() + ")");
            case LinkedMatchExpression.StringPattern stringPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + stringPattern.value() + ")");
            case LinkedMatchExpression.BoolPattern boolPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + boolPattern.value() + ")");
            case LinkedMatchExpression.FloatPattern floatPattern ->
                    java.util.Optional.of("java.util.Objects.equals(" + valueExpression + ", " + floatPattern.value() + ")");
            case LinkedMatchExpression.TypedPattern typedPattern ->
                    java.util.Optional.of("(" + valueExpression + " instanceof " + javaPatternType(typedPattern.type()) + ")");
            case LinkedMatchExpression.VariablePattern ignored -> java.util.Optional.empty();
            case LinkedMatchExpression.WildcardPattern ignored -> java.util.Optional.empty();
            case LinkedMatchExpression.ConstructorPattern ignored ->
                    throw new IllegalStateException("Nested constructor pattern in constructor pattern is not supported in Java generation");
        };
    }

    private static pl.grzeslowski.capybara.linker.LinkedType inferConcreteCaseType(
            LinkedMatchExpression matchExpression,
            int currentCaseIndex
    ) {
        for (int idx = 0; idx < matchExpression.cases().size(); idx++) {
            if (idx == currentCaseIndex) {
                continue;
            }
            var type = matchExpression.cases().get(idx).expression().type();
            if (type instanceof pl.grzeslowski.capybara.linker.LinkedGenericTypeParameter) {
                continue;
            }
            if (type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY
                || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DATA
                || type == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.NOTHING) {
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

    private static String castMatchCaseExpression(String expression, pl.grzeslowski.capybara.linker.LinkedType resultType) {
        if (resultType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY
            || resultType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.DATA
            || resultType == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.NOTHING
            || resultType instanceof pl.grzeslowski.capybara.linker.LinkedGenericTypeParameter) {
            return expression;
        }
        return "((" + javaPatternType(resultType) + ") (" + expression + "))";
    }

    private static String javaPatternType(pl.grzeslowski.capybara.linker.LinkedType type) {
        return switch (type) {
            case pl.grzeslowski.capybara.linker.PrimitiveLinkedType primitiveType -> switch (primitiveType) {
                case BYTE -> "java.lang.Byte";
                case INT -> "java.lang.Integer";
                case LONG -> "java.lang.Long";
                case DOUBLE -> "java.lang.Double";
                case STRING -> "java.lang.String";
                case BOOL -> "java.lang.Boolean";
                case FLOAT -> "java.lang.Float";
                case NOTHING, ANY, DATA -> "java.lang.Object";
            };
            case pl.grzeslowski.capybara.linker.LinkedDataType dataType -> normalizeJavaTypeReference(dataType.name());
            case pl.grzeslowski.capybara.linker.LinkedDataParentType dataParentType -> normalizeJavaTypeReference(dataParentType.name());
            default -> "java.lang.Object";
        };
    }

    private static Scope evaluateNewList(LinkedNewList newList, Scope scope) {
        var current = scope;
        var values = new ArrayList<String>(newList.values().size());
        for (var value : newList.values()) {
            var exSc = evaluateExpression(value, current).popExpression();
            current = exSc.scope();
            values.add(exSc.expression());
        }
        return current.addExpression("java.util.List.of(" + String.join(", ", values) + ")");
    }

    private static Scope evaluateNewSet(LinkedNewSet newSet, Scope scope) {
        var current = scope;
        var values = new ArrayList<String>(newSet.values().size());
        for (var value : newSet.values()) {
            var exSc = evaluateExpression(value, current).popExpression();
            current = exSc.scope();
            values.add(exSc.expression());
        }
        return current.addExpression("java.util.Set.of(" + String.join(", ", values) + ")");
    }

    private static Scope evaluateNewDict(LinkedNewDict newDict, Scope scope) {
        var current = scope;
        var entries = new ArrayList<String>(newDict.entries().size());
        for (var entry : newDict.entries()) {
            var keyExSc = evaluateExpression(entry.key(), current).popExpression();
            var valueExSc = evaluateExpression(entry.value(), keyExSc.scope()).popExpression();
            current = valueExSc.scope();
            entries.add("java.util.Map.entry(" + keyExSc.expression() + ", " + valueExSc.expression() + ")");
        }
        if (entries.isEmpty()) {
            return current.addExpression("java.util.Map.of()");
        }
        return current.addExpression(
                "java.util.stream.Stream.of(" + String.join(", ", entries) + ")"
                + ".collect(java.util.stream.Collectors.toMap("
                + "java.util.Map.Entry::getKey, java.util.Map.Entry::getValue, "
                + "(oldValue, newValue) -> newValue, java.util.LinkedHashMap::new))"
        );
    }

    private static Scope evaluateNewData(LinkedNewData newData, Scope scope) {
        if (!(newData.type() instanceof pl.grzeslowski.capybara.linker.LinkedDataType dataType)) {
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
                    return value;
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
                                         + "(new pl.grzeslowski.capybara.CapybaraException(" + args.get(0) + "))");
        }
        if (dataType.singleton()) {
            return current.addExpression(normalizeJavaTypeReference(dataType.name()) + ".INSTANCE");
        }

        return current.addExpression("new " + normalizeJavaTypeReference(dataType.name()) + "(" + String.join(", ", args) + ")");
    }

    private static Scope evaluateStringValue(LinkedStringValue stringValue, Scope scope) {
        return scope.addExpression(stringValue.toString());
    }

    private static Scope evaluateNothingValue(LinkedNothingValue nothingValue, Scope scope) {
        return scope.addExpression("__capybaraUnsupported(\"" + escapeJavaString(nothingValue.message()) + "\")");
    }

    private static Scope evaluateVariable(LinkedVariable variable, Scope scope) {
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
            default -> "op" + Integer.toHexString(symbol);
        };
    }

    private static String normalizeFunctionCallTarget(String target) {
        var lastDot = target.lastIndexOf('.');
        if (lastDot < 0) {
            return normalizeJavaMethodName(target);
        }
        var qualifier = target.substring(0, lastDot);
        var methodName = target.substring(lastDot + 1);
        return normalizeJavaQualifier(qualifier) + "." + normalizeJavaMethodName(methodName);
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
        if (typeName.contains("/") && typeName.contains(".")) {
            var dotIndex = typeName.lastIndexOf('.');
            var slashIndex = typeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = typeName.startsWith("/") ? 1 : 0;
                var packageName = typeName.substring(startIdx, slashIndex).replace('/', '.');
                var outer = normalizeJavaClassName(typeName.substring(slashIndex + 1, dotIndex));
                var inner = normalizeJavaClassName(typeName.substring(dotIndex + 1));
                if (outer.equals(inner)) {
                    return packageName + "." + outer;
                }
                return packageName + "." + outer + "." + inner;
            }
        }
        if (!typeName.contains(".")) {
            return normalizeJavaClassName(typeName);
        }
        var parts = typeName.split("\\.");
        var normalized = new ArrayList<String>(parts.length);
        for (var part : parts) {
            normalized.add(normalizeJavaClassName(part));
        }
        return String.join(".", normalized);
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

    private static String stripNumericSuffix(String value) {
        return value.replaceFirst("\\d+$", "");
    }

    private static String valueFieldOrFallback(pl.grzeslowski.capybara.linker.LinkedType valueType,
                                               String valueExpression,
                                               String fieldName,
                                               String fallbackExpression) {
        return switch (valueType) {
            case pl.grzeslowski.capybara.linker.LinkedDataType linkedDataType ->
                    linkedDataType.fields().stream().anyMatch(field -> field.name().equals(fieldName))
                            ? "(" + valueExpression + ")." + fieldName + "()"
                            : fallbackExpression;
            case pl.grzeslowski.capybara.linker.LinkedDataParentType linkedDataParentType ->
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

    private static boolean isResultErrorDataType(pl.grzeslowski.capybara.linker.LinkedDataType dataType) {
        var normalized = normalizeQualifiedTypeName(dataType.name());
        if (normalized.equals("/cap/lang/Result.Error") || normalized.equals("/capy/lang/Result.Error")) {
            return true;
        }
        return "Error".equals(dataType.name())
               && dataType.fields().size() == 1
               && "message".equals(dataType.fields().getFirst().name())
               && dataType.fields().getFirst().type() == pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING;
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
        var normalized = typeName.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private static boolean isOptionType(pl.grzeslowski.capybara.linker.LinkedType type) {
        if (!(type instanceof pl.grzeslowski.capybara.linker.GenericDataType genericDataType)) {
            return false;
        }
        var normalized = normalizeQualifiedTypeName(genericDataType.name());
        return "Option".equals(genericDataType.name())
               || normalized.endsWith("/Option.Option")
               || normalized.endsWith("/Option");
    }

    private static boolean isOptionConstructor(LinkedExpression expression) {
        if (!(expression instanceof LinkedNewData linkedNewData)
            || !(linkedNewData.type() instanceof pl.grzeslowski.capybara.linker.GenericDataType dataType)) {
            return false;
        }
        return isOptionSomeTypeName(dataType.name()) || isOptionNoneTypeName(dataType.name());
    }

}
