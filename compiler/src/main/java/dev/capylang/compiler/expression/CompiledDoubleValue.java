package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledDoubleValue(String doubleValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.DOUBLE;
    }

    @Override
    public String toString() {
        return doubleValue;
    }
}

