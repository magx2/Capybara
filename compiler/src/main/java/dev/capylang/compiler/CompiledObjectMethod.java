package dev.capylang.compiler;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public record CompiledObjectMethod(
        String name,
        List<CompiledObjectMethodParameter> parameters,
        String returnType,
        Map<String, String> backendMethodNames
) {
    public CompiledObjectMethod {
        parameters = parameters == null ? List.of() : List.copyOf(parameters);
        backendMethodNames = backendMethodNames == null ? Map.of() : Collections.unmodifiableSortedMap(new TreeMap<>(backendMethodNames));
    }
}
