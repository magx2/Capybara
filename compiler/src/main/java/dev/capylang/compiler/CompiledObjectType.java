package dev.capylang.compiler;

import java.util.List;

public record CompiledObjectType(
        String name,
        String backendClassName,
        List<String> parents,
        Visibility visibility,
        List<CompiledAnnotation> annotations
) implements GenericDataType {
    public CompiledObjectType {
        parents = List.copyOf(parents);
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

    public CompiledObjectType(
            String name,
            String backendClassName,
            List<String> parents,
            Visibility visibility
    ) {
        this(name, backendClassName, parents, visibility, List.of());
    }

    @Override
    public List<CompiledDataType.CompiledField> fields() {
        return List.of();
    }
}
