package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.CompiledAnnotation;

import java.util.List;

public record CompiledReflectionValue(
        CompiledExpression target,
        String name,
        String packageName,
        String packagePath,
        List<Field> fields,
        List<CompiledAnnotation> annotations,
        CompiledType type
) implements CompiledExpression {
    public CompiledReflectionValue {
        fields = fields == null ? List.of() : List.copyOf(fields);
        annotations = annotations == null ? List.of() : List.copyOf(annotations);
    }

    public record Field(String name, CompiledType type, List<CompiledAnnotation> annotations) {
        public Field {
            annotations = annotations == null ? List.of() : List.copyOf(annotations);
        }

        public Field(String name, CompiledType type) {
            this(name, type, List.of());
        }
    }
}
