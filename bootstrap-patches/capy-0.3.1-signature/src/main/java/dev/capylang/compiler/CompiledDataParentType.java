package dev.capylang.compiler;

import java.util.List;
import java.util.Objects;

public record CompiledDataParentType(
        String name,
        List<CompiledDataType.CompiledField> fields,
        List<CompiledDataType> subTypes,
        List<String> typeParameters,
        List<String> comments,
        Visibility visibility,
        boolean enumType,
        List<CompiledAnnotation> annotations
) implements GenericDataType, Comparable<CompiledDataParentType> {
    public CompiledDataParentType {
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

    public CompiledDataParentType(
            String name,
            List<CompiledDataType.CompiledField> fields,
            List<CompiledDataType> subTypes,
            List<String> typeParameters,
            List<String> comments,
            Visibility visibility,
            boolean enumType
    ) {
        this(name, fields, subTypes, typeParameters, comments, visibility, enumType, List.of());
    }

    public CompiledDataParentType(
            String name,
            List<CompiledDataType.CompiledField> fields,
            List<CompiledDataType> subTypes,
            List<String> typeParameters
    ) {
        this(name, fields, subTypes, typeParameters, List.of(), null, false);
    }

    public CompiledDataParentType(
            String name,
            List<CompiledDataType.CompiledField> fields,
            List<CompiledDataType> subTypes,
            List<String> typeParameters,
            boolean enumType
    ) {
        this(name, fields, subTypes, typeParameters, List.of(), null, enumType);
    }

    public CompiledDataParentType(
            String name,
            List<CompiledDataType.CompiledField> fields,
            List<CompiledDataType> subTypes,
            List<String> typeParameters,
            List<String> comments,
            boolean enumType
    ) {
        this(name, fields, subTypes, typeParameters, comments, null, enumType);
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof CompiledDataParentType dataType && Objects.equals(name, dataType.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public int compareTo(CompiledDataParentType other) {
        return name.compareTo(other.name);
    }

    @Override
    public String toString() {
        return "CompiledDataParentType[name=" + name
                + ", fields=" + fields
                + ", subTypes=" + subTypes.stream().map(CompiledDataType::name).toList()
                + ", typeParameters=" + typeParameters
                + ", enumType=" + enumType
                + "]";
    }
}
