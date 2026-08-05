package dev.capylang.generator;

import dev.capylang.compiler.CompiledProgram;
import dev.capylang.generator.internal.GeneratedJavaGenerator;

import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/** Bootstrap-compatible entry point for the self-hosted Java generator. */
public final class JavaGenerator {
    private JavaGenerator() {
    }

    public static GeneratedProgram javaGenerator(CompiledProgram program) {
        var generated = dataMap(GeneratedJavaGenerator.java_generator__128_0(toGeneratedValue(program)));
        var modules = list(generated.get("modules")).stream()
                .map(JavaGenerator::generatedModule)
                .toList();
        return new GeneratedProgram(modules);
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
}
