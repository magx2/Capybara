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
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "false",
            "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof",
            "int", "interface", "long", "native", "new", "null", "package", "private", "protected",
            "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized",
            "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"
    );
    static final Scope EMPTY = new Scope(0L, Set.of(), Set.of(), Map.of(), List.of(), Optional.empty(), Optional.empty());

    private final long valueIdx;
    private final Set<String> localValues;
    private final Set<String> javaLocalValues;
    private final Map<String, String> valueNameToUniqueName;
    private final List<String> statements;
    private final Optional<String> expression;
    private final Optional<String> moduleHelperClass;

    private Scope(long valueIdx, Set<String> localValues,
                  Set<String> javaLocalValues,
                  Map<String, String> valueNameToUniqueName,
                  List<String> statements,
                  Optional<String> expression,
                  Optional<String> moduleHelperClass) {
        this.valueIdx = valueIdx;
        this.localValues = Set.copyOf(localValues);
        this.javaLocalValues = Set.copyOf(javaLocalValues);
        this.valueNameToUniqueName = Map.copyOf(valueNameToUniqueName);
        this.statements = List.copyOf(statements);
        this.expression = expression;
        this.moduleHelperClass = moduleHelperClass;
    }

    UniqueNameScopeExpression addValue(String name, CompiledExpression expression) {
        var javaName = normalizeJavaLocalIdentifier(name);
        if (!localValues.contains(name) && !javaLocalValues.contains(javaName)) {
            var updatedValues = new HashSet<>(localValues);
            updatedValues.add(name);
            var updatedJavaValues = new HashSet<>(javaLocalValues);
            updatedJavaValues.add(javaName);
            var updatedMappings = new HashMap<>(valueNameToUniqueName);
            if (!name.equals(javaName)) {
                updatedMappings.put(name, javaName);
            }
            return new UniqueNameScopeExpression(
                    javaName,
                    new Scope(valueIdx, updatedValues, updatedJavaValues, updatedMappings, statements, this.expression, moduleHelperClass),
                    expression
            );
        }
        if (localValues.contains(name)) {
            return generateUniqueName(name, expression);
        }
        return reserveUniqueJavaLocalName(name, expression);
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
        var javaName = normalizeJavaLocalIdentifier(name);
        if (!localValues.contains(name) && !javaLocalValues.contains(javaName)) {
            var updated = new HashSet<>(localValues);
            updated.add(name);
            var updatedJavaValues = new HashSet<>(javaLocalValues);
            updatedJavaValues.add(javaName);
            var updatedMappings = new HashMap<>(valueNameToUniqueName);
            if (!name.equals(javaName)) {
                updatedMappings.put(name, javaName);
            }
            return new Scope(valueIdx, updated, updatedJavaValues, updatedMappings, statements, expression, moduleHelperClass);
        }
        if (localValues.contains(name) && Objects.equals(valueNameToUniqueName.get(name), javaName)) {
            return this;
        }

        var reserved = reserveUniqueJavaLocalName(name);
        var updated = new HashSet<>(localValues);
        updated.add(name);
        var updatedJavaValues = new HashSet<>(javaLocalValues);
        updatedJavaValues.add(reserved.javaName());
        var updatedMappings = new HashMap<>(valueNameToUniqueName);
        updatedMappings.put(name, reserved.javaName());
        return new Scope(reserved.nextValueIdx(), updated, updatedJavaValues, updatedMappings, statements, expression, moduleHelperClass);
    }

    Scope withReservedJavaLocalName(String name) {
        var javaName = normalizeJavaLocalIdentifier(name);
        if (javaLocalValues.contains(javaName)) {
            return this;
        }
        var updatedJavaValues = new HashSet<>(javaLocalValues);
        updatedJavaValues.add(javaName);
        return new Scope(valueIdx, localValues, updatedJavaValues, valueNameToUniqueName, statements, expression, moduleHelperClass);
    }

    Scope withoutReservedJavaLocalName(String name) {
        var javaName = normalizeJavaLocalIdentifier(name);
        if (localValues.contains(name) || Objects.equals(valueNameToUniqueName.get(name), javaName)) {
            return this;
        }
        if (localValues.stream().map(Scope::normalizeJavaLocalIdentifier).anyMatch(javaName::equals)
            || valueNameToUniqueName.containsValue(javaName)) {
            return this;
        }
        if (!javaLocalValues.contains(javaName)) {
            return this;
        }
        var updatedJavaValues = new HashSet<>(javaLocalValues);
        updatedJavaValues.remove(javaName);
        return new Scope(valueIdx, localValues, updatedJavaValues, valueNameToUniqueName, statements, expression, moduleHelperClass);
    }

    private Scope addStatementUnchecked(String statement) {
        var updated = new ArrayList<>(statements);
        updated.add(statement);
        return new Scope(valueIdx, localValues, javaLocalValues, valueNameToUniqueName, updated, expression, moduleHelperClass);
    }

    public Scope addExpression(String expression) {
        if (this.expression.isPresent()) {
            throw new IllegalStateException("Expression already exists and it's set to: " + this.expression.get());
        }
        return new Scope(valueIdx, localValues, javaLocalValues, valueNameToUniqueName, statements, Optional.of(expression), moduleHelperClass);
    }

    private record UniqueNameScopeExpression(String uniqueName, Scope scope, CompiledExpression expression) {
    }

    private record ReservedJavaLocalName(String logicalName, String javaName, long nextValueIdx) {
    }

    private ReservedJavaLocalName reserveUniqueJavaLocalName(String baseName) {
        var idx = valueIdx;
        var matcher = LAST_NUMBER_PATTERN.matcher(baseName);
        if (matcher.matches()) {
            idx = parseLong(matcher.group(2));
            baseName = matcher.group(1);
        }
        String logicalName;
        String javaName;
        do {
            idx++;
            logicalName = baseName + SEPARATOR + idx;
            javaName = normalizeJavaLocalIdentifier(logicalName);
        } while (javaLocalValues.contains(javaName));
        return new ReservedJavaLocalName(logicalName, javaName, idx);
    }

    private UniqueNameScopeExpression generateUniqueName(String name, CompiledExpression expression) {
        if (!localValues.contains(name)) {
            throw new IllegalStateException("Name `%s` should be in `localValues`: %s".formatted(name, localValues));
        }
        var reserved = reserveUniqueJavaLocalName(name);
        var uniqueName = reserved.logicalName();
        var javaUniqueName = reserved.javaName();
        LOG.fine("findUniqueName: " + name + " -> " + uniqueName + " (" + javaUniqueName + ")");

        var set = new HashSet<>(localValues);
        set.add(uniqueName);
        var javaSet = new HashSet<>(javaLocalValues);
        javaSet.add(javaUniqueName);
        var map = new HashMap<>(valueNameToUniqueName);
        map.put(name, javaUniqueName);
        map.put(uniqueName, javaUniqueName);
        return new UniqueNameScopeExpression(
                javaUniqueName,
                new Scope(
                        reserved.nextValueIdx(),
                        set,
                        javaSet,
                        map,
                        statements,
                        this.expression,
                        moduleHelperClass),
                rewriteValueInExpression(name, uniqueName, expression)
        );
    }

    private UniqueNameScopeExpression reserveUniqueJavaLocalName(String name, CompiledExpression expression) {
        var reserved = reserveUniqueJavaLocalName(name);
        var uniqueName = reserved.logicalName();
        var javaUniqueName = reserved.javaName();

        var updatedValues = new HashSet<>(localValues);
        updatedValues.add(name);
        var updatedJavaValues = new HashSet<>(javaLocalValues);
        updatedJavaValues.add(javaUniqueName);
        var updatedMappings = new HashMap<>(valueNameToUniqueName);
        updatedMappings.put(name, javaUniqueName);
        return new UniqueNameScopeExpression(
                javaUniqueName,
                new Scope(
                        reserved.nextValueIdx(),
                        updatedValues,
                        updatedJavaValues,
                        updatedMappings,
                        statements,
                        this.expression,
                        moduleHelperClass
                ),
                expression
        );
    }

    Scope addValueOverride(String sourceName, String targetName) {
        var updatedMap = new HashMap<>(valueNameToUniqueName);
        updatedMap.put(sourceName, targetName);
        return new Scope(valueIdx, localValues, javaLocalValues, updatedMap, statements, expression, moduleHelperClass);
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
                union(javaLocalValues, other.javaLocalValues),
                union(valueNameToUniqueName, other.valueNameToUniqueName),
                union(statements, other.statements), union(expression, other.expression),
                moduleHelperClass.or(() -> other.moduleHelperClass));
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
        return new Scope(valueIdx, localValues, javaLocalValues, Map.of(), statements, expression, moduleHelperClass);
    }

    Scope withStatements(List<String> statements) {
        return new Scope(valueIdx, localValues, javaLocalValues, valueNameToUniqueName, statements, expression, moduleHelperClass);
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
        return new ExpressionScope(expression, new Scope(valueIdx, localValues, javaLocalValues, valueNameToUniqueName, statements, Optional.empty(), moduleHelperClass));
    }

    Scope withModuleHelperClass(String moduleHelperClass) {
        return new Scope(valueIdx, localValues, javaLocalValues, valueNameToUniqueName, statements, expression, Optional.of(moduleHelperClass));
    }

    Optional<String> moduleHelperClass() {
        return moduleHelperClass;
    }

    @Override
    public String toString() {
        return "Scope{" +
               "localValues=" + localValues +
               ", javaLocalValues=" + javaLocalValues +
               ", valueNameToUniqueName=" + valueNameToUniqueName +
               ", statements=" + statements +
               ", expression=" + expression +
               '}';
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
}
