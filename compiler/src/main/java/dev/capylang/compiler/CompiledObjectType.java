package dev.capylang.compiler;

import java.util.List;

public record CompiledObjectType(
        String name,
        String backendClassName,
        List<String> parents,
        Visibility visibility,
        List<CompiledAnnotation> annotations,
        CompiledObjectKind kind,
        List<CompiledObjectMethod> methods
) implements GenericDataType {
    public CompiledObjectType(String name, String backendClassName, List<String> parents, Visibility visibility) {
        this(name, backendClassName, parents, visibility, List.of(), CompiledObjectKind.CLASS, List.of());
    }

    public CompiledObjectType(
            String name,
            String backendClassName,
            List<String> parents,
            Visibility visibility,
            List<CompiledAnnotation> annotations
    ) {
        this(name, backendClassName, parents, visibility, annotations, CompiledObjectKind.CLASS, List.of());
    }

    public CompiledObjectType(
            String name,
            String backendClassName,
            List<String> parents,
            Visibility visibility,
            CompiledObjectKind kind,
            List<CompiledObjectMethod> methods
    ) {
        this(name, backendClassName, parents, visibility, List.of(), kind, methods);
    }

    public CompiledObjectType {
        parents = List.copyOf(parents);
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
        kind = kind == null ? CompiledObjectKind.CLASS : kind;
        methods = methods == null ? List.of() : List.copyOf(methods);
    }

    @Override
    public List<CompiledDataType.CompiledField> fields() {
        return List.of();
    }
}
