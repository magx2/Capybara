package dev.capylang.compiler;

import java.util.List;

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
        this(name, fields, subTypes, typeParameters, List.of(), Visibility.LOCAL, false, List.of());
    }

    public CompiledDataParentType(
            String name,
            List<CompiledDataType.CompiledField> fields,
            List<CompiledDataType> subTypes,
            List<String> typeParameters,
            boolean enumType
    ) {
        this(name, fields, subTypes, typeParameters, List.of(), Visibility.LOCAL, enumType, List.of());
    }

    public CompiledDataParentType(
            String name,
            List<CompiledDataType.CompiledField> fields,
            List<CompiledDataType> subTypes,
            List<String> typeParameters,
            List<String> comments,
            boolean enumType
    ) {
        this(name, fields, subTypes, typeParameters, comments, Visibility.LOCAL, enumType, List.of());
    }

    @Override
    public int compareTo(CompiledDataParentType other) {
        return name.compareTo(other.name);
    }

    @Override
    public String toString() {
        return "CompiledDataParentType[" + name + "]";
    }
}
