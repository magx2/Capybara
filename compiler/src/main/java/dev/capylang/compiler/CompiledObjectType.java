package dev.capylang.compiler;

import java.util.List;

public record CompiledObjectType(
        String name,
        String backendClassName,
        List<String> parents,
        Visibility visibility
) implements GenericDataType {
    public CompiledObjectType {
        parents = List.copyOf(parents);
    }

    @Override
    public List<CompiledDataType.CompiledField> fields() {
        return List.of();
    }
}
