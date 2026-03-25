package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledFunctionInvoke(
        CompiledExpression function,
        List<CompiledExpression> arguments,
        CompiledType returnType
) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return returnType;
    }
}
