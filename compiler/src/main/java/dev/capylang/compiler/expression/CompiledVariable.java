package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledVariable(String name, CompiledType type, boolean emptyShapeCompatible) implements CompiledExpression {
    public CompiledVariable(String name, CompiledType type) {
        this(name, type, false);
    }
}
