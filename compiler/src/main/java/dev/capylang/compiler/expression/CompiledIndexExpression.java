package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledIndexExpression(
        CompiledExpression source,
        CompiledExpression index,
        CompiledType elementType,
        CompiledType type
) implements CompiledExpression {
}

