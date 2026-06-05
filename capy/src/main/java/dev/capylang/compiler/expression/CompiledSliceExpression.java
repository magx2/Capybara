package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.Optional;

public record CompiledSliceExpression(
        CompiledExpression source,
        Optional<CompiledExpression> start,
        Optional<CompiledExpression> end,
        CompiledType type
) implements CompiledExpression {
}

