package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledPipeAllExpression(
        CompiledExpression source,
        String argumentName,
        CompiledExpression predicate,
        CompiledType type
) implements CompiledExpression {
}
