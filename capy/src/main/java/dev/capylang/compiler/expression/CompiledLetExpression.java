package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.Optional;

public record CompiledLetExpression(String name,
                                    CompiledExpression value,
                                    Optional<CompiledType> declaredType,
                                    CompiledExpression rest) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return rest.type();
    }
}
