package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledLongValue(String longValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.LONG;
    }

    @Override
    public String toString() {
        return longValue;
    }
}

