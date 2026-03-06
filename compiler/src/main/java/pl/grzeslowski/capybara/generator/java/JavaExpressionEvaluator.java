package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.linker.expression.*;

import java.util.ArrayList;
import java.util.logging.Logger;

import static java.lang.System.lineSeparator;

@SuppressWarnings("SwitchStatementWithTooFewBranches")
public class JavaExpressionEvaluator {
    private static final Logger log = Logger.getLogger(JavaExpressionEvaluator.class.getName());

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
            case LinkedFloatValue floatValue -> evaluateFloatValue(floatValue, scope);
            case LinkedFunctionCall functionCall -> evaluateFunctionCall(functionCall, scope);
            case LinkedIfExpression ifExpression -> evaluateIfExpression(ifExpression, scope);
            case LinkedInfixExpression infixExpression -> evaluateInfixExpression(infixExpression, scope);
            case LinkedIntValue intValue -> evaluateIntValue(intValue, scope);
            case LinkedLetExpression letExpression -> evaluateLetExpression(letExpression, scope);
            case LinkedMatchExpression matchExpression -> evaluateMatchExpression(matchExpression, scope);
            case LinkedPipeFilterOutExpression pipeFilterOutExpression -> evaluatePipeFilterOutExpression(pipeFilterOutExpression, scope);
            case LinkedPipeExpression pipeExpression -> evaluatePipeExpression(pipeExpression, scope);
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

    private static Scope evaluateFunctionCall(LinkedFunctionCall functionCall, Scope scope) {
        throw new UnsupportedOperationException("wip");
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

    private static Scope evaluatePipeExpression(LinkedPipeExpression pipeExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeExpression.source(), scope).popExpression();
        var source = sourceExSc.expression();
        if (pipeExpression.source().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            source = source + ".values()";
        }

        var mapperExSc = evaluateExpression(
                pipeExpression.mapper(),
                sourceExSc.scope().addLocalValue(pipeExpression.argumentName())
        ).popExpression();

        return mapperExSc.scope().addExpression(
                source + ".stream().map(" + pipeExpression.argumentName() + " -> (" + mapperExSc.expression() + ")).toList()"
        );
    }

    private static Scope evaluatePipeFilterOutExpression(LinkedPipeFilterOutExpression pipeFilterOutExpression, Scope scope) {
        var sourceExSc = evaluateExpression(pipeFilterOutExpression.source(), scope).popExpression();
        var source = sourceExSc.expression();
        if (pipeFilterOutExpression.source().type() instanceof pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict) {
            source = source + ".values()";
        }

        var predicateExSc = evaluateExpression(
                pipeFilterOutExpression.predicate(),
                sourceExSc.scope().addLocalValue(pipeFilterOutExpression.argumentName())
        ).popExpression();

        return predicateExSc.scope().addExpression(
                source + ".stream().filter(" + pipeFilterOutExpression.argumentName() + " -> !(" + predicateExSc.expression() + ")).toList()"
        );
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
}
