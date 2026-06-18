package dev.capylang.compiler;

import java.util.List;
import java.util.Objects;

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
    public CompiledDataType {
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

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
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, false, false);
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            Visibility visibility,
            boolean singleton,
            boolean enumValue
    ) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, false, enumValue);
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            boolean singleton
    ) {
        this(name, fields, typeParameters, extendedTypes, comments, null, singleton, false);
    }

    public CompiledDataType(
            String name,
            List<CompiledField> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            boolean singleton
    ) {
        this(name, fields, typeParameters, extendedTypes, List.of(), null, singleton, false);
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof CompiledDataType dataType && Objects.equals(name, dataType.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(CompiledDataType other) {
        return name.compareTo(other.name);
    }

    @Override
    public String toString() {
        return "CompiledDataType[name=" + name
                + ", fields=" + fields
                + ", typeParameters=" + typeParameters
                + ", extendedTypes=" + extendedTypes
                + ", singleton=" + singleton
                + ", nativeType=" + nativeType
                + ", enumValue=" + enumValue
                + "]";
    }

    public record CompiledField(String name, CompiledType type, List<CompiledAnnotation> annotations) {
        public CompiledField {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
        }

        public CompiledField(String name, CompiledType type) {
            this(name, type, List.of());
        }

        @Override
        public String toString() {
            return "CompiledField[name=" + name + ", type=" + TypeStrings.describe(type) + "]";
        }
    }
}
