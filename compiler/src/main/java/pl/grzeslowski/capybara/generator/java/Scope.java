package pl.grzeslowski.capybara.generator.java;

import java.util.*;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import static java.lang.Long.parseLong;
import static java.util.function.Predicate.not;

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

    public UniqueNameScope addValue(String name) {
        return Optional.of(name)
                .filter(not(localValues::contains))
                .map(n -> {
                    var updatedValues = new HashSet<>(localValues);
                    updatedValues.add(name);
                    return new UniqueNameScope(
                            n,
                            new Scope(valueIdx, updatedValues, valueNameToUniqueName, statements, expression)
                    );
                })
                .orElseGet(() -> findUniqueName(name));
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

    private record UniqueNameScope(String uniqueName, Scope scope) {
    }

    private UniqueNameScope findUniqueName(String name) {
        if (!localValues.contains(name)) {
            return new UniqueNameScope(name, this);
        }
        var idx = valueIdx;
        var rawName = name;
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
        return new UniqueNameScope(
                uniqueName,
                new Scope(
                        valueIdx + 1,
                        set,
                        map,
                        statements,
                        expression));
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

    public Scope declareValue(String name, String lastExpression) {
        var uniqueNameScope = addValue(name);
        return uniqueNameScope.scope.addStatementUnchecked("var %s = %s".formatted(uniqueNameScope.uniqueName, lastExpression));
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
