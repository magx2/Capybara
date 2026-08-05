package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedJavaGenerator;

import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

/** Bootstrap-compatible entry point for the self-hosted Java generator. */
public final class JavaGenerator {
    private static final Set<String> PIPE_OPERATORS = Set.of("|", "|-", "|*", "|!");
    private static final Map<String, Set<String>> SUPPORTED_METHODS_BY_RECEIVER = Map.ofEntries(
            Map.entry("Effect", Set.of("map", "start")),
            Map.entry("Result", Set.of("map", "flat_map", "reduce", "reduce_left", "or_else", "or")),
            Map.entry("Option", Set.of("map", "filter", "flat_map")),
            Map.entry("Async", Set.of("join", "map", "flat_map", "`|`", "`|*`"))
    );
    private static final Pattern UNSUPPORTED_FUNCTION = Pattern.compile(
            "throw new UnsupportedOperationException\\(\\\"Unsupported CFUN expression at (\\d+):(\\d+)\\\"\\);"
    );

    private JavaGenerator() {
    }

    public static GeneratedProgram javaGenerator(CompiledProgram program) {
        var compiledProgram = dataMap(toGeneratedValue(program));
        var generated = dataMap(GeneratedJavaGenerator.java_generator__128_0(compiledProgram));
        var modules = list(generated.get("modules")).stream()
                .map(JavaGenerator::generatedModule)
                .toList();
        modules.forEach(module -> rejectUnsupportedFunctions(module, compiledProgram));
        return new GeneratedProgram(modules);
    }

    private static void rejectUnsupportedFunctions(GeneratedModule module, Map<String, Object> compiledProgram) {
        var matcher = UNSUPPORTED_FUNCTION.matcher(module.code());
        if (matcher.find()) {
            var functionLine = Integer.parseInt(matcher.group(1));
            var functionColumn = Integer.parseInt(matcher.group(2));
            var context = findFunction(compiledProgram, module.relativePath(), functionLine, functionColumn);
            var failure = context
                    .flatMap(value -> explainFailure(value, compiledProgram))
                    .orElseGet(() -> new GenerationFailure(
                            functionLine,
                            functionColumn,
                            "the function contains an expression unsupported by the Java backend"
                    ));
            var sourcePath = context
                    .map(FunctionContext::sourcePath)
                    .orElseGet(() -> sourcePath(module.relativePath()));
            var functionName = context
                    .map(value -> string(value.function().get("name")))
                    .filter(name -> !name.isBlank())
                    .orElse("<unknown>");
            throw new IllegalStateException(
                    "Java generation failed for `" + sourcePath + "` at "
                            + failure.line() + ":" + failure.column()
                            + " in function `" + functionName + "`: " + failure.reason()
                            + ". No Java source was written for this module."
            );
        }
    }

    private static Optional<FunctionContext> findFunction(
            Map<String, Object> compiledProgram,
            String generatedPath,
            int line,
            int column
    ) {
        for (Object moduleValue : list(compiledProgram.get("modules"))) {
            var module = dataMap(moduleValue);
            if (!generatedPath.equals(modulePath(module, ".java"))) {
                continue;
            }
            for (Object functionValue : list(module.get("functions"))) {
                var function = dataMap(functionValue);
                var location = dataMap(function.get("location"));
                if (integer(location.get("line")) == line && integer(location.get("column")) == column) {
                    return Optional.of(new FunctionContext(modulePath(module, ".cfun"), module, function));
                }
            }
        }
        return Optional.empty();
    }

    private static Optional<GenerationFailure> explainFailure(
            FunctionContext context,
            Map<String, Object> compiledProgram
    ) {
        var failures = new ArrayList<GenerationFailure>();
        collectActionableFailures(
                context.function().get("body"),
                knownFunctionNames(compiledProgram, context.module()),
                declaredVariableTypes(context.function()),
                failures
        );
        return failures.stream()
                .min(Comparator.comparingInt(GenerationFailure::line)
                        .thenComparingInt(GenerationFailure::column));
    }

    private static void collectActionableFailures(
            Object value,
            Set<String> knownFunctions,
            Map<String, TypeShape> declaredTypes,
            List<GenerationFailure> failures
    ) {
        if (value instanceof List<?> values) {
            values.forEach(item -> collectActionableFailures(item, knownFunctions, declaredTypes, failures));
            return;
        }
        if (!(value instanceof Map<?, ?> rawMap)) {
            return;
        }

        @SuppressWarnings("unchecked")
        var map = (Map<String, Object>) rawMap;
        var expressionType = string(map.get("__type"));
        if (expressionType.equals("CompiledReduceExpression")) {
            collectActionableFailures(map.get("receiver"), knownFunctions, declaredTypes, failures);
            collectActionableFailures(map.get("initial"), knownFunctions, declaredTypes, failures);

            var reduceTypes = new LinkedHashMap<>(declaredTypes);
            var accumulatorName = string(map.get("accumulatorName"));
            inferredExpressionType(map.get("initial"), declaredTypes)
                    .filter(type -> !type.name().isBlank())
                    .ifPresent(type -> reduceTypes.put(accumulatorName, type));
            collectActionableFailures(map.get("body"), knownFunctions, reduceTypes, failures);
            return;
        }
        if (expressionType.equals("CompiledBinaryExpression")) {
            var operator = string(map.get("operator"));
            var right = mapValue(map.get("right"));
            var rightType = right.map(item -> string(item.get("__type"))).orElse("");
            if (PIPE_OPERATORS.contains(operator) && !supportedPipeRight(right)) {
                var location = location(map);
                failures.add(new GenerationFailure(
                        location.line(),
                        location.column(),
                        "operator `" + operator
                                + "` requires a lambda or function reference on its right-hand side; found "
                                + expressionDescription(rightType)
                ));
            } else if (operator.equals("|*")) {
                collectionFlatMapWrapperFailure(map, declaredTypes).ifPresent(failures::add);
            }
        } else if (expressionType.equals("CompiledMethodCallExpression")) {
            var argument = mapValue(first(map.get("arguments")));
            if (string(map.get("name")).equals("flat_map")
                    && argument.filter(valueMap -> string(valueMap.get("__type"))
                            .equals("CompiledVariableExpression")).isPresent()) {
                var location = location(map);
                failures.add(new GenerationFailure(
                        location.line(),
                        location.column(),
                        "method `flat_map` requires a callable mapper; variable `"
                                + string(argument.orElseThrow().get("name")) + "` is not callable in this context"
                ));
            } else {
                unsupportedMethodFailure(map, knownFunctions, declaredTypes).ifPresent(failures::add);
            }
        } else if (expressionType.equals("CompiledFunctionCallExpression")) {
            var name = string(map.get("name"));
            if (unqualifiedName(name) && !knownFunctions.contains(name)) {
                var location = location(map);
                failures.add(new GenerationFailure(
                        location.line(),
                        location.column(),
                        "unresolved function call `" + name + "`"
                ));
            }
        }

        map.values().forEach(item -> collectActionableFailures(item, knownFunctions, declaredTypes, failures));
    }

    private static Optional<TypeShape> inferredExpressionType(
            Object value,
            Map<String, TypeShape> declaredTypes
    ) {
        return mapValue(value).flatMap(expression -> {
            var expressionType = string(expression.get("__type"));
            if (expressionType.equals("CompiledVariableExpression")) {
                return Optional.ofNullable(declaredTypes.get(string(expression.get("name"))));
            }
            if (expressionType.equals("CompiledFunctionCallExpression")) {
                var name = string(expression.get("name"));
                if (name.equals("pure") || name.equals("Effect.pure")
                        || name.equals("delay") || name.equals("Effect.delay")) {
                    return Optional.of(new TypeShape("Effect", List.of()));
                }
            }
            return Optional.empty();
        });
    }

    private static Optional<GenerationFailure> unsupportedMethodFailure(
            Map<String, Object> methodCall,
            Set<String> knownFunctions,
            Map<String, TypeShape> declaredTypes
    ) {
        var receiverType = inferredExpressionType(methodCall.get("receiver"), declaredTypes)
                .map(TypeShape::name)
                .map(JavaGenerator::unqualifiedTypeName);
        if (receiverType.isEmpty()) {
            return Optional.empty();
        }

        var supportedMethods = SUPPORTED_METHODS_BY_RECEIVER.get(receiverType.orElseThrow());
        var methodName = string(methodCall.get("name"));
        var unknownMethod = !knownFunctions.contains(methodName);
        var unsupportedTypeSpecificMethod = supportedMethods != null && !supportedMethods.contains(methodName);
        if (!unknownMethod && !unsupportedTypeSpecificMethod) {
            return Optional.empty();
        }

        var location = location(methodCall);
        return Optional.of(new GenerationFailure(
                location.line(),
                location.column(),
                "method `" + methodName + "` on `" + receiverType.orElseThrow()
                        + "` is not supported by the Java backend"
        ));
    }

    private static String unqualifiedTypeName(String name) {
        var slash = name.lastIndexOf('/');
        var dot = name.lastIndexOf('.');
        return name.substring(Math.max(slash, dot) + 1);
    }

    private static Optional<GenerationFailure> collectionFlatMapWrapperFailure(
            Map<String, Object> binary,
            Map<String, TypeShape> declaredTypes
    ) {
        var left = mapValue(binary.get("left"));
        var right = mapValue(binary.get("right"));
        if (left.isEmpty() || right.isEmpty()
                || !string(left.orElseThrow().get("__type")).equals("CompiledVariableExpression")
                || !string(right.orElseThrow().get("__type")).equals("CompiledLambdaExpression")) {
            return Optional.empty();
        }

        var receiverType = declaredTypes.get(string(left.orElseThrow().get("name")));
        if (receiverType == null || !Set.of("List", "Set", "Seq").contains(receiverType.name())
                || receiverType.arguments().isEmpty()) {
            return Optional.empty();
        }
        var valueType = receiverType.arguments().getFirst();
        if (!Set.of("Result", "Option", "Effect", "Async").contains(valueType.name())) {
            return Optional.empty();
        }

        var lambda = right.orElseThrow();
        var parameters = list(lambda.get("parameters"));
        var body = mapValue(lambda.get("body"));
        if (parameters.size() != 1 || body.isEmpty()
                || !string(body.orElseThrow().get("__type")).equals("CompiledMethodCallExpression")
                || !string(body.orElseThrow().get("name")).equals("map")) {
            return Optional.empty();
        }
        var bodyReceiver = mapValue(body.orElseThrow().get("receiver"));
        if (bodyReceiver.isEmpty()
                || !string(bodyReceiver.orElseThrow().get("__type")).equals("CompiledVariableExpression")
                || !string(bodyReceiver.orElseThrow().get("name")).equals(string(parameters.getFirst()))) {
            return Optional.empty();
        }

        var location = location(binary);
        return Optional.of(new GenerationFailure(
                location.line(),
                location.column(),
                "collection operator `|*` requires its mapper to return a collection; `"
                        + valueType.name() + ".map` returns `" + valueType.name() + "`"
        ));
    }

    private static boolean supportedPipeRight(Optional<Map<String, Object>> right) {
        if (right.isEmpty()) {
            return false;
        }
        var rightType = string(right.orElseThrow().get("__type"));
        if (rightType.equals("CompiledLambdaExpression")
                || rightType.equals("CompiledFunctionReferenceExpression")) {
            return true;
        }
        if (!rightType.equals("CompiledMethodCallExpression")) {
            return false;
        }
        return mapValue(right.orElseThrow().get("receiver"))
                .map(receiver -> string(receiver.get("__type")))
                .filter(type -> type.equals("CompiledLambdaExpression")
                        || type.equals("CompiledFunctionReferenceExpression"))
                .isPresent();
    }

    private static Set<String> knownFunctionNames(
            Map<String, Object> compiledProgram,
            Map<String, Object> currentModule
    ) {
        var names = new HashSet<String>();
        for (Object moduleValue : list(compiledProgram.get("modules"))) {
            var module = dataMap(moduleValue);
            for (Object functionValue : list(module.get("functions"))) {
                names.add(string(dataMap(functionValue).get("name")));
            }
        }
        for (Object importValue : list(currentModule.get("imports"))) {
            var declaration = dataMap(importValue);
            list(declaration.get("importedNames")).stream()
                    .map(JavaGenerator::string)
                    .forEach(names::add);
        }
        return names;
    }

    private static Map<String, TypeShape> declaredVariableTypes(Map<String, Object> function) {
        var types = new LinkedHashMap<String, TypeShape>();
        for (Object parameterValue : list(function.get("parameters"))) {
            var parameter = dataMap(parameterValue);
            types.put(string(parameter.get("name")), typeShape(parameter.get("typeReference")));
        }
        mapValue(function.get("body"))
                .filter(body -> string(body.get("__type")).equals("CompiledBlockExpression"))
                .ifPresent(body -> {
                    for (Object bindingValue : list(body.get("bindings"))) {
                        var binding = dataMap(bindingValue);
                        types.put(string(binding.get("name")), typeShape(binding.get("typeReference")));
                    }
                });
        return types;
    }

    private static TypeShape typeShape(Object value) {
        return mapValue(value)
                .map(type -> new TypeShape(
                        string(type.get("name")),
                        list(type.get("arguments")).stream().map(JavaGenerator::typeShape).toList()
                ))
                .orElseGet(() -> new TypeShape("", List.of()));
    }

    private static Object first(Object value) {
        var values = list(value);
        return values.isEmpty() ? null : values.getFirst();
    }

    private static boolean unqualifiedName(String name) {
        return !name.isBlank() && !name.contains(".") && !name.contains("/");
    }

    private static String expressionDescription(String expressionType) {
        return switch (expressionType) {
            case "CompiledMethodCallExpression" -> "a method call";
            case "CompiledBinaryExpression" -> "a binary expression";
            case "CompiledVariableExpression" -> "a variable";
            case "CompiledFunctionCallExpression" -> "a function call";
            case "" -> "an unknown expression";
            default -> "`" + expressionType.replaceFirst("^Compiled", "")
                    .replaceFirst("Expression$", "") + "`";
        };
    }

    private static SourceLocation location(Map<String, Object> value) {
        return mapValue(value.get("location"))
                .map(location -> new SourceLocation(
                        integer(location.get("line")),
                        integer(location.get("column"))
                ))
                .orElse(new SourceLocation(0, 0));
    }

    private static Optional<Map<String, Object>> mapValue(Object value) {
        if (value instanceof Map<?, ?> map) {
            @SuppressWarnings("unchecked")
            var typedMap = (Map<String, Object>) map;
            return Optional.of(typedMap);
        }
        return Optional.empty();
    }

    private static String modulePath(Map<String, Object> module, String extension) {
        var path = string(module.get("path")).replace('\\', '/');
        while (path.startsWith("/")) {
            path = path.substring(1);
        }
        while (path.endsWith("/")) {
            path = path.substring(0, path.length() - 1);
        }
        var name = string(module.get("name"));
        return path.isBlank() ? name + extension : path + "/" + name + extension;
    }

    private static String sourcePath(String generatedPath) {
        return generatedPath.endsWith(".java")
                ? generatedPath.substring(0, generatedPath.length() - ".java".length()) + ".cfun"
                : generatedPath;
    }

    private static GeneratedModule generatedModule(Object value) {
        var module = dataMap(value);
        return new GeneratedModule(
                string(module.get("relativePath")),
                string(module.get("code"))
        );
    }

    private static Object toGeneratedValue(Object value) {
        if (value == null
                || value instanceof String
                || value instanceof Number
                || value instanceof Boolean
                || value instanceof Character) {
            return value;
        }
        if (value instanceof Optional<?> optional) {
            return optional.map(JavaGenerator::toGeneratedValue);
        }
        if (value instanceof List<?> values) {
            return values.stream().map(JavaGenerator::toGeneratedValue).toList();
        }
        if (value instanceof Set<?> values) {
            var converted = new LinkedHashSet<>();
            values.forEach(item -> converted.add(toGeneratedValue(item)));
            return Collections.unmodifiableSet(converted);
        }
        if (value instanceof Map<?, ?> values) {
            var converted = new LinkedHashMap<Object, Object>();
            values.forEach((key, item) -> converted.put(toGeneratedValue(key), toGeneratedValue(item)));
            return Collections.unmodifiableMap(converted);
        }
        if (value.getClass().isRecord()) {
            return recordData(value);
        }
        throw new IllegalArgumentException("Unsupported compiler value: " + value.getClass().getName());
    }

    private static Map<String, Object> recordData(Object record) {
        var result = new LinkedHashMap<String, Object>();
        result.put("__type", record.getClass().getSimpleName());
        for (RecordComponent component : record.getClass().getRecordComponents()) {
            try {
                result.put(component.getName(), toGeneratedValue(component.getAccessor().invoke(record)));
            } catch (ReflectiveOperationException exception) {
                throw new IllegalStateException("Unable to adapt " + record.getClass().getName(), exception);
            }
        }
        return Collections.unmodifiableMap(result);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> dataMap(Object value) {
        if (value instanceof Map<?, ?> map) {
            return (Map<String, Object>) map;
        }
        throw new IllegalArgumentException("Expected generated data value, got: " + value);
    }

    @SuppressWarnings("unchecked")
    private static List<Object> list(Object value) {
        if (value instanceof List<?> list) {
            return (List<Object>) list;
        }
        return new ArrayList<>();
    }

    private static String string(Object value) {
        return value == null ? "" : value.toString();
    }

    private static int integer(Object value) {
        return value instanceof Number number ? number.intValue() : 0;
    }

    private record FunctionContext(
            String sourcePath,
            Map<String, Object> module,
            Map<String, Object> function
    ) {
    }

    private record GenerationFailure(int line, int column, String reason) {
    }

    private record SourceLocation(int line, int column) {
    }

    private record TypeShape(String name, List<TypeShape> arguments) {
    }
}
