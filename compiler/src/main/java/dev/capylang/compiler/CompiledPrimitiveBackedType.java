package dev.capylang.compiler;

import java.util.List;

public record CompiledPrimitiveBackedType(
        String name,
        PrimitiveLinkedType backingType,
        String cfunType,
        List<String> comments,
        Visibility visibility
) implements GenericDataType, Comparable<CompiledPrimitiveBackedType> {
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
    public final boolean equals(Object o) {
        if (!(o instanceof CompiledPrimitiveBackedType that)) return false;
        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(CompiledPrimitiveBackedType other) {
        return name.compareTo(other.name);
    }
}
