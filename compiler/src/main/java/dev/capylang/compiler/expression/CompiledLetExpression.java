package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledLetExpression(String name,
                                  CompiledExpression value,
                                  CompiledExpression rest) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return rest.type();
    }
}
