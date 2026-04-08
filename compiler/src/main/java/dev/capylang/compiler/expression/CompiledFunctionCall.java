package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledFunctionCall(String name, List<CompiledExpression> arguments,
                                 CompiledType returnType) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return returnType;
    }
}
