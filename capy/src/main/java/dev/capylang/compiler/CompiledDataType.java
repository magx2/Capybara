package dev.capylang.compiler;

import java.util.List;

public record CompiledDataType(String name,
                             List<CompiledField> fields,
                             List<String> typeParameters,
                             List<String> extendedTypes,
                             List<String> comments,
                             Visibility visibility,
                             boolean singleton,
                             boolean nativeType,
                             boolean enumValue,
                             List<CompiledAnnotation> annotations) implements GenericDataType, Comparable<CompiledDataType> {
    public CompiledDataType {
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            List<String> comments,
                            Visibility visibility,
                            boolean singleton,
                            boolean nativeType,
                            boolean enumValue) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, nativeType, enumValue, List.of());
    }

    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            List<String> comments,
                            Visibility visibility,
                            boolean singleton) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, false, false);
    }

    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            List<String> comments,
                            Visibility visibility,
                            boolean singleton,
                            boolean enumValue) {
        this(name, fields, typeParameters, extendedTypes, comments, visibility, singleton, false, enumValue);
    }

    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            List<String> comments,
                            boolean singleton) {
        this(name, fields, typeParameters, extendedTypes, comments, null, singleton, false);
    }

    public CompiledDataType(String name,
                            List<CompiledField> fields,
                            List<String> typeParameters,
                            List<String> extendedTypes,
                            boolean singleton) {
        this(name, fields, typeParameters, extendedTypes, List.of(), null, singleton, false);
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

    public record CompiledField(String name, CompiledType type, List<CompiledAnnotation> annotations) {
        public CompiledField {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
        }

        public CompiledField(String name, CompiledType type) {
            this(name, type, List.of());
        }
    }
}
