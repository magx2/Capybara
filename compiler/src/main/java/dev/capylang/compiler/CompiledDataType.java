package dev.capylang.compiler;

import java.util.List;

public record CompiledDataType(String name,
                             List<CompiledField> fields,
                             List<String> typeParameters,
                             List<String> extendedTypes,
                             List<String> comments,
                             Visibility visibility,
                             boolean singleton) implements GenericDataType, Comparable<CompiledDataType> {
    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            List<String> comments,
                            boolean singleton) {
        this(name, fields, typeParameters, extendedTypes, comments, null, singleton);
    }

    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            boolean singleton) {
        this(name, fields, typeParameters, extendedTypes, List.of(), null, singleton);
    }

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof CompiledDataType that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(CompiledDataType o) {
        return name.compareTo(o.name);
    }

    public record CompiledField(String name, CompiledType type) {
    }
}
