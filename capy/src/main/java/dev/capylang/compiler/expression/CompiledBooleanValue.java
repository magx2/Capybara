package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public enum CompiledBooleanValue implements CompiledExpression {
    TRUE(true), FALSE(false);

    private final boolean value;

    CompiledBooleanValue(boolean value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }

    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.BOOL;
    }
}
