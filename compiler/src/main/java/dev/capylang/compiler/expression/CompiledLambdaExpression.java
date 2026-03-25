package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledFunctionType;
import dev.capylang.compiler.CompiledType;

public record CompiledLambdaExpression(
        String argumentName,
        CompiledExpression expression,
        CompiledFunctionType functionType
) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return functionType;
    }
}
