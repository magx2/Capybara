package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledIntValue(String intValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.INT;
    }

    @Override
    public String toString() {
        return intValue;
    }
}
