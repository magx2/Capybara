package dev.capylang.compiler;

import java.util.List;

public record CompiledDataType(
        String name,
        List<CompiledField> fields,
        List<String> typeParameters,
        List<String> extendedTypes,
        List<String> comments,
        Visibility visibility,
        boolean singleton,
        boolean nativeType,
        boolean enumValue,
        List<CompiledAnnotation> annotations
) implements GenericDataType, Comparable<CompiledDataType> {
    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            Visibility visibility,
            boolean singleton,
            boolean nativeType,
            boolean enumValue
    ) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, nativeType, enumValue, List.of());
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            Visibility visibility,
            boolean singleton
    ) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, false, false, List.of());
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            Visibility visibility,
            boolean singleton,
            boolean nativeType
    ) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, nativeType, false, List.of());
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            boolean singleton
    ) {
        this(name, fields, typeParameters, extendedTypes, comments, Visibility.LOCAL, singleton, false, false, List.of());
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            boolean singleton
    ) {
        this(name, fields, typeParameters, extendedTypes, List.of(), Visibility.LOCAL, singleton, false, false, List.of());
    }

    @Override
    public int compareTo(CompiledDataType other) {
        return name.compareTo(other.name);
    }

    @Override
    public String toString() {
        return "CompiledDataType[" + name + "]";
    }

    public record CompiledField(
            String name,
            CompiledType type,
            List<CompiledAnnotation> annotations
    ) {
        public CompiledField(String name, CompiledType type) {
            this(name, type, List.of());
        }

        @Override
        public String toString() {
            return "CompiledField[" + name + ":" + type.name() + "]";
        }
    }
}
