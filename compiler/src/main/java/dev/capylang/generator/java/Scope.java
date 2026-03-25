package dev.capylang.generator.java;

import dev.capylang.compiler.expression.CompiledExpression;

import java.util.*;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import static java.lang.Long.parseLong;
import static java.util.function.Predicate.not;
import static dev.capylang.generator.java.ValueNameRewriter.rewriteValueInExpression;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
class Scope {
    private static final Logger LOG = Logger.getLogger(Scope.class.getName());
    private static final String SEPARATOR = "_j";
    private static final Pattern LAST_NUMBER_PATTERN = Pattern.compile("(.+)" + SEPARATOR + "(\\d+)");
    static final Scope EMPTY = new Scope(0L, Set.of(), Map.of(), List.of(), Optional.empty());

    private final long valueIdx;
    private final Set<String> localValues;
    private final Map<String, String> valueNameToUniqueName;
    private final List<String> statements;
    private final Optional<String> expression;

    private Scope(long valueIdx, Set<String> localValues,
                  Map<String, String> valueNameToUniqueName,
                  List<String> statements,
                  Optional<String> expression) {
        this.valueIdx = valueIdx;
        this.localValues = Set.copyOf(localValues);
        this.valueNameToUniqueName = Map.copyOf(valueNameToUniqueName);
        this.statements = List.copyOf(statements);
        this.expression = expression;
    }

    UniqueNameScopeExpression addValue(String name, CompiledExpression expression) {
        return Optional.of(name)
                .filter(not(localValues::contains))
                .map(n -> {
                    var updatedValues = new HashSet<>(localValues);
                    updatedValues.add(name);
                    return new UniqueNameScopeExpression(
                            n,
                            new Scope(valueIdx, updatedValues, valueNameToUniqueName, statements, this.expression),
                            expression
                    );
                })
                .orElseGet(() -> generateUniqueName(name, expression));
/*
        var updatedVariableNameToUniqueName = new HashMap<>(valueNameToUniqueName);
        var scope = this;
        if (localValues.contains(name)) {
            var uniqueNameScope = findUniqueName(name);
            updatedVariableNameToUniqueName.put(name, uniqueNameScope.uniqueName);
            scope = uniqueNameScope.scope;
        }

        var updatedValues = new HashSet<>(scope.localValues);
        updatedValues.add(name);
        return new Scope(scope.valueIdx, updatedValues, updatedVariableNameToUniqueName, scope.statements, scope.expression);
 */
    }

    public Scope addStatement(String statement) {
        if (statement.startsWith("var")) {
            throw new IllegalArgumentException("For adding variable definition use `Scope.declareValue`");
        }
        return addStatementUnchecked(statement);
    }

    Scope addLocalValue(String name) {
        if (localValues.contains(name)) {
            return this;
        }
        var updated = new HashSet<>(localValues);
        updated.add(name);
        return new Scope(valueIdx, updated, valueNameToUniqueName, statements, expression);
    }

    private Scope addStatementUnchecked(String statement) {
        var updated = new ArrayList<>(statements);
        updated.add(statement);
        return new Scope(valueIdx, localValues, valueNameToUniqueName, updated, expression);
    }

    public Scope addExpression(String expression) {
        if (this.expression.isPresent()) {
            throw new IllegalStateException("Expression already exists and it's set to: " + this.expression.get());
        }
        return new Scope(valueIdx, localValues, valueNameToUniqueName, statements, Optional.of(expression));
    }

    private record UniqueNameScopeExpression(String uniqueName, Scope scope, CompiledExpression expression) {
    }

    private UniqueNameScopeExpression generateUniqueName(String name, CompiledExpression expression) {
        if (!localValues.contains(name)) {
            throw new IllegalStateException("Name `%s` should be in `localValues`: %s".formatted(name, localValues));
        }
        var idx = valueIdx;
        var matcher = LAST_NUMBER_PATTERN.matcher(name);
        if (matcher.matches()) {
            idx = parseLong(matcher.group(2));
            name = matcher.group(1);
        }
        var uniqueName = name + SEPARATOR + (idx + 1);
        LOG.fine("findUniqueName: " + name + " -> " + uniqueName);

        var set = new HashSet<>(localValues);
        set.add(uniqueName);
        var map = new HashMap<>(valueNameToUniqueName);
        map.put(name, uniqueName);
        return new UniqueNameScopeExpression(
                uniqueName,
                new Scope(
                        valueIdx + 1,
                        set,
                        map,
                        statements,
                        this.expression),
                rewriteValueInExpression(name, uniqueName, expression)
        );
    }

    Scope addValueOverride(String sourceName, String targetName) {
        var updatedMap = new HashMap<>(valueNameToUniqueName);
        updatedMap.put(sourceName, targetName);
        return new Scope(valueIdx, localValues, updatedMap, statements, expression);
    }

    public List<String> getStatements() {
        return statements;
    }

    public String getExpression() {
        return expression.orElseThrow();
    }

    public Scope add(Scope other) {
        return new Scope(valueIdx,
                union(localValues, other.localValues),
                union(valueNameToUniqueName, other.valueNameToUniqueName),
                union(statements, other.statements), union(expression, other.expression));
    }

    private static Set<String> union(Set<String> left, Set<String> right) {
        var union = new HashSet<>(left);
        union.addAll(right);
        return Set.copyOf(union);
    }

    private static Map<String, String> union(Map<String, String> left, Map<String, String> right) {
        var union = new HashMap<>(left);
        union.putAll(right);
        return Map.copyOf(union);
    }

    private static List<String> union(List<String> left, List<String> right) {
        var union = new ArrayList<String>(left.size() + right.size());
        union.addAll(left);
        union.addAll(right);
        return List.copyOf(union);
    }

    private static Optional<String> union(Optional<String> left, Optional<String> right) {
        if (left.isPresent() && right.isPresent()) {
            throw new IllegalArgumentException("Both left and right are not allowed");
        }
        return left.or(() -> right);
    }

    public Optional<String> findValueOverride(String name) {
        return Optional.ofNullable(valueNameToUniqueName.get(name));
    }

    Scope withoutValueOverrides() {
        return new Scope(valueIdx, localValues, Map.of(), statements, expression);
    }

    record ScopeExpression(Scope scope, CompiledExpression expression) {
    }

    ScopeExpression declareValue(String name, String lastExpression, CompiledExpression expression) {
        var unique = addValue(name, expression);
        return new ScopeExpression(
                unique.scope.addStatementUnchecked("var %s = %s".formatted(unique.uniqueName, lastExpression)),
                unique.expression);
    }

    ScopeExpression declareTypedValue(String name, String javaType, String lastExpression, CompiledExpression expression) {
        var unique = addValue(name, expression);
        return new ScopeExpression(
                unique.scope.addStatementUnchecked("%s %s = %s".formatted(javaType, unique.uniqueName, lastExpression)),
                unique.expression);
    }

    public record ExpressionScope(String expression, Scope scope) {
    }

    public ExpressionScope popExpression() {
        var expression = getExpression();
        return new ExpressionScope(expression, new Scope(valueIdx, localValues, valueNameToUniqueName, statements, Optional.empty()));
    }

    @Override
    public String toString() {
        return "Scope{" +
               "localValues=" + localValues +
               ", valueNameToUniqueName=" + valueNameToUniqueName +
               ", statements=" + statements +
               ", expression=" + expression +
               '}';
    }
}
