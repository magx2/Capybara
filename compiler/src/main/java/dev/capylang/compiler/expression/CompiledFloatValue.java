package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledFloatValue(String floatValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.FLOAT;
    }

    @Override
    public String toString() {
        return floatValue;
    }
}
