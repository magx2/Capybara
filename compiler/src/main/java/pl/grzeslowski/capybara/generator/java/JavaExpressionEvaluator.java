package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.linker.expression.*;

import java.util.ArrayList;
import java.util.logging.Logger;

import static java.lang.System.lineSeparator;

@SuppressWarnings("SwitchStatementWithTooFewBranches")
public class JavaExpressionEvaluator {
    private static final Logger log = Logger.getLogger(JavaExpressionEvaluator.class.getName());
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
            case LinkedFieldAccess fieldAccess -> evaluateFieldAccess(fieldAccess, scope);
            case LinkedFloatValue floatValue -> evaluateFloatValue(floatValue, scope);
            case LinkedFunctionCall functionCall -> evaluateFunctionCall(functionCall, scope);
            case LinkedFunctionInvoke functionInvoke -> evaluateFunctionInvoke(functionInvoke, scope);
            case LinkedIfExpression ifExpression -> evaluateIfExpression(ifExpression, scope);
            case LinkedInfixExpression infixExpression -> evaluateInfixExpression(infixExpression, scope);
            case LinkedIntValue intValue -> evaluateIntValue(intValue, scope);
            case LinkedLambdaExpression lambdaExpression -> evaluateLambdaExpression(lambdaExpression, scope);
            case LinkedLetExpression letExpression -> evaluateLetExpression(letExpression, scope);
            case LinkedMatchExpression matchExpression -> evaluateMatchExpression(matchExpression, scope);
            case LinkedPipeFlatMapExpression pipeFlatMapExpression -> evaluatePipeFlatMapExpression(pipeFlatMapExpression, scope);
            case LinkedPipeFilterOutExpression pipeFilterOutExpression -> evaluatePipeFilterOutExpression(pipeFilterOutExpression, scope);
            case LinkedPipeExpression pipeExpression -> evaluatePipeExpression(pipeExpression, scope);
            case LinkedPipeReduceExpression pipeReduceExpression -> evaluatePipeReduceExpression(pipeReduceExpression, scope);
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

    private static Scope evaluateFloatValue(LinkedFloatValue floatValue, Scope scope) {
        return scope.addExpression(floatValue.floatValue() + "f");
    }

    private static Scope evaluateFieldAccess(LinkedFieldAccess fieldAccess, Scope scope) {
        var source = evaluateExpression(fieldAccess.source(), scope).popExpression();
        return source.scope().addExpression("(" + source.expression() + ")." + fieldAccess.field() + "()");
    }

    private static Scope evaluateFunctionCall(LinkedFunctionCall functionCall, Scope scope) {
        var current = scope;
        var args = new ArrayList<String>(functionCall.arguments().size());
        for (var argument : functionCall.arguments()) {
            var argumentScope = evaluateExpression(argument, current).popExpression();
            current = argumentScope.scope();
            args.add(argumentScope.expression());
        }

        var expression = switch (functionCall.name()) {
            case "sqrt" -> {
                if (args.size() != 1) {
                    throw new IllegalStateException("sqrt expects exactly one argument");
                }
                yield "((float) java.lang.Math.sqrt(" + args.get(0) + "))";
            }
            default -> normalizeJavaMethodName(functionCall.name()) + "(" + String.join(", ", args) + ")";
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

        return elseExSc.scope()
                .addExpression("(%s) ? (%s) : (%s)".formatted(
                        condition.expression(),
                        then.expression(),
                        elseExSc.expression()));
    }

    private static Scope evaluateInfixExpression(LinkedInfixExpression infixExpression, Scope scope) {
        var left = evaluateExpression(infixExpression.left(), scope).popExpression();
        var right = evaluateExpression(infixExpression.right(), left.scope()).popExpression();

        var operator = infixExpression.operator();
        var expression = switch (operator) {
            case POWER -> "pl.grzeslowski.capybara.CapybaraUtil.power(" + left.expression() + ", " + right.expression() + ")";
            case PLUS -> {
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList) {
                    yield evaluateListAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet) {
                    yield evaluateSetAppendExpression(infixExpression, left.expression(), right.expression());
                }
                if (infixExpression.left().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
                    yield evaluateDictAppendExpression(left.expression(), right.expression());
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
                yield left.expression() + operator.symbol() + right.expression();
            }
            default -> left.expression() + operator.symbol() + right.expression();
        };

        return right.scope().addExpression('(' + expression + ')');
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

    private static String evaluateDictAppendExpression(String left, String right) {
        return "java.util.stream.Stream.concat(" + left + ".entrySet().stream(), " + right + ".entrySet().stream())"
               + ".collect(java.util.stream.Collectors.toUnmodifiableMap("
               + "java.util.Map.Entry::getKey, java.util.Map.Entry::getValue, (oldValue, newValue) -> newValue))";
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
        if (infixExpression.right().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            return left + ".entrySet().stream().filter(entry -> !" + right + ".containsKey(entry.getKey()))"
                   + ".collect(java.util.stream.Collectors.toUnmodifiableMap(java.util.Map.Entry::getKey, java.util.Map.Entry::getValue))";
        }
        return left + ".entrySet().stream().filter(entry -> !java.util.Objects.equals(entry.getKey(), " + right + "))"
               + ".collect(java.util.stream.Collectors.toUnmodifiableMap(java.util.Map.Entry::getKey, java.util.Map.Entry::getValue))";
    }

    private static Scope evaluateIntValue(LinkedIntValue intValue, Scope scope) {
        return scope.addExpression(intValue.intValue());
    }

    private static Scope evaluateLambdaExpression(LinkedLambdaExpression lambdaExpression, Scope scope) {
        var bodyExSc = evaluateExpression(
                lambdaExpression.expression(),
                scope.addLocalValue(lambdaExpression.argumentName())
        ).popExpression();
        return bodyExSc.scope().addExpression(lambdaExpression.argumentName() + " -> (" + bodyExSc.expression() + ")");
    }

    private static Scope evaluatePipeExpression(LinkedPipeExpression pipeExpression, Scope scope) {
        var streamExSc = evaluatePipeExpressionAsStream(pipeExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeExpression.type()));
    }

    private static Scope evaluatePipeFlatMapExpression(LinkedPipeFlatMapExpression pipeFlatMapExpression, Scope scope) {
        var streamExSc = evaluatePipeFlatMapExpressionAsStream(pipeFlatMapExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeFlatMapExpression.type()));
    }

    private static Scope evaluatePipeFilterOutExpression(LinkedPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var streamExSc = evaluatePipeFilterOutExpressionAsStream(pipeFilterOutExpression, scope);
        return streamExSc.scope().addExpression(streamExSc.streamExpression() + terminalCollect(pipeFilterOutExpression.type()));
    }

    private static Scope evaluatePipeReduceExpression(LinkedPipeReduceExpression pipeReduceExpression, Scope scope) {
        var sourceStreamExSc = evaluateSourceAsStream(pipeReduceExpression.source(), scope);
        var initialExSc = evaluateExpression(pipeReduceExpression.initialValue(), sourceStreamExSc.scope()).popExpression();
        var reducerExSc = evaluateExpression(
                pipeReduceExpression.reducerExpression(),
                initialExSc.scope()
                        .addLocalValue(pipeReduceExpression.accumulatorName())
                        .addLocalValue(pipeReduceExpression.valueName())
        ).popExpression();

        return reducerExSc.scope().addExpression(
                sourceStreamExSc.streamExpression()
                + ".reduce("
                + initialExSc.expression()
                + ", (" + pipeReduceExpression.accumulatorName()
                + ", " + pipeReduceExpression.valueName()
                + ") -> (" + reducerExSc.expression() + "))"
        );
    }

    private static StreamExpressionScope evaluatePipeExpressionAsStream(LinkedPipeExpression pipeExpression, Scope scope) {
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
                mapperExSc.scope()
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
                predicateExSc.scope()
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
                mapperExSc.scope()
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

    private static Scope evaluateMatchExpression(LinkedMatchExpression matchExpression, Scope scope) {
        var matchWithExSc = evaluateExpression(matchExpression.matchWith(), scope).popExpression();
        var declaredValue = matchWithExSc.scope().declareValue(
                "value",
                matchWithExSc.expression(),
                new LinkedVariable("value", matchExpression.matchWith().type())
        );
        var current = declaredValue.scope();
        var switchTarget = current.findValueOverride("value").orElse("value");
        var cases = new ArrayList<String>(matchExpression.cases().size());

        for (var matchCase : matchExpression.cases()) {
            var branchScope = current;
            if (matchCase.pattern() instanceof LinkedMatchExpression.ConstructorPattern constructorPattern) {
                for (var name : constructorPattern.names()) {
                    branchScope = branchScope.addLocalValue(name);
                }
            }
            var expressionScope = evaluateExpression(matchCase.expression(), branchScope).popExpression();
            current = expressionScope.scope();
            cases.add(matchCasePattern(matchCase.pattern()) + " -> (" + expressionScope.expression() + ")");
        }

        var hasWildcard = matchExpression.cases().stream()
                .map(LinkedMatchExpression.MatchCase::pattern)
                .anyMatch(LinkedMatchExpression.WildcardPattern.class::isInstance);
        if (!hasWildcard) {
            cases.add("default -> throw new java.lang.IllegalStateException(\"Unexpected value: \" + " + switchTarget + ")");
        }

        return current.addExpression("switch (" + switchTarget + ") { " + String.join("; ", cases) + "; }");
    }

    private static String matchCasePattern(LinkedMatchExpression.Pattern pattern) {
        return switch (pattern) {
            case LinkedMatchExpression.IntPattern intPattern -> "case " + intPattern.value();
            case LinkedMatchExpression.StringPattern stringPattern -> "case " + stringPattern.value();
            case LinkedMatchExpression.BoolPattern boolPattern -> "case " + boolPattern.value();
            case LinkedMatchExpression.FloatPattern floatPattern -> "case " + floatPattern.value();
            case LinkedMatchExpression.VariablePattern variablePattern -> "case " + variablePattern.name() + " _";
            case LinkedMatchExpression.WildcardPattern wildcardPattern -> "default";
            case LinkedMatchExpression.ConstructorPattern constructorPattern ->
                    "case " + constructorPattern.constructorName() + "("
                    + constructorPattern.names().stream().map(name -> "var " + name).reduce((a, b) -> a + ", " + b).orElse("")
                    + ")";
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
        var values = new ArrayList<String>(newDict.entries().size() * 2);
        for (var entry : newDict.entries()) {
            var keyExSc = evaluateExpression(entry.key(), current).popExpression();
            var valueExSc = evaluateExpression(entry.value(), keyExSc.scope()).popExpression();
            current = valueExSc.scope();
            values.add(keyExSc.expression());
            values.add(valueExSc.expression());
        }
        return current.addExpression("java.util.Map.of(" + String.join(", ", values) + ")");
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

        return current.addExpression("new " + dataType.name() + "(" + String.join(", ", args) + ")");
    }

    private static Scope evaluateStringValue(LinkedStringValue stringValue, Scope scope) {
        return scope.addExpression(stringValue.toString());
    }

    private static Scope evaluateVariable(LinkedVariable variable, Scope scope) {
        var name = scope.findValueOverride(variable.name()).orElse(variable.name());
        return scope.addExpression(name);
    }

    private static String normalizeJavaMethodName(String name) {
        var parts = name.split("[^A-Za-z0-9]+");
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
            return "generated";
        }
        var identifier = result.toString();
        if (JAVA_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }
}
