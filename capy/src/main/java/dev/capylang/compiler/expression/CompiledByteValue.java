package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledByteValue(String byteValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.BYTE;
    }

    @Override
    public String toString() {
        return byteValue;
    }
}

