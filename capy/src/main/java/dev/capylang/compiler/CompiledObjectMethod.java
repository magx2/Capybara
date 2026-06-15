package dev.capylang.compiler;

import java.util.List;
import java.util.Map;

public record CompiledObjectMethod(
        String name,
        List<CompiledObjectMethodParameter> parameters,
        String returnType,
        Map<String, String> backendMethodNames
) {
    public CompiledObjectMethod {
        parameters = parameters == null ? List.of() : List.copyOf(parameters);
        backendMethodNames = backendMethodNames == null ? Map.of() : Map.copyOf(backendMethodNames);
    }
}
