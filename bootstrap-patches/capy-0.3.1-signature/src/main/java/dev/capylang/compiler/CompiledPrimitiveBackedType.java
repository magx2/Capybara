package dev.capylang.compiler;

import java.util.List;
import java.util.Objects;

public record CompiledPrimitiveBackedType(
        String name,
        PrimitiveLinkedType backingType,
        String cfunType,
        List<String> comments,
        Visibility visibility,
        List<CompiledAnnotation> annotations
) implements GenericDataType, Comparable<CompiledPrimitiveBackedType> {
    public CompiledPrimitiveBackedType {
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

    public CompiledPrimitiveBackedType(
            String name,
            PrimitiveLinkedType backingType,
            String cfunType,
            List<String> comments,
            Visibility visibility
    ) {
        this(name, backingType, cfunType, comments, visibility, List.of());
    }

    public CompiledPrimitiveBackedType(
            String name,
            PrimitiveLinkedType backingType,
            List<String> comments,
            Visibility visibility
    ) {
        this(name, backingType, name, comments, visibility);
    }

    @Override
    public List<CompiledDataType.CompiledField> fields() {
        return List.of();
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof CompiledPrimitiveBackedType type && Objects.equals(name, type.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(CompiledPrimitiveBackedType other) {
        return name.compareTo(other.name);
    }

    @Override
    public String toString() {
        return "CompiledPrimitiveBackedType[name=" + name
                + ", backingType=" + TypeStrings.describe(backingType)
                + ", cfunType=" + cfunType
                + "]";
    }
}
