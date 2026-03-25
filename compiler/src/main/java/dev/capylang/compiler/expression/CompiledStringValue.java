package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledStringValue(String stringValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.STRING;
    }

    @Override
    public String toString() {
        return stringValue;
    }
}
