package dev.capylang.generator.java;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.CompiledAnnotation;

import java.util.List;

public record JavaDataValueInfo(
        String name,
        String packageName,
        String packagePath,
        List<Field> fields,
        List<CompiledAnnotation> annotations
) {
    public JavaDataValueInfo {
        fields = List.copyOf(fields);
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

    public JavaDataValueInfo(String name, String packageName, String packagePath, List<Field> fields) {
        this(name, packageName, packagePath, fields, List.of());
    }

    public record Field(String name, CompiledType type, String valueExpression, List<CompiledAnnotation> annotations) {
        public Field {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
        }

        public Field(String name, CompiledType type, String valueExpression) {
            this(name, type, valueExpression, List.of());
        }
    }
}
