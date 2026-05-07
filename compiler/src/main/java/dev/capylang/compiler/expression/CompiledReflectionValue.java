package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledReflectionValue(
        CompiledExpression target,
        String name,
        String packageName,
        String packagePath,
        List<Field> fields,
        CompiledType type
) implements CompiledExpression {
    public record Field(String name, CompiledType type) {
    }
}
