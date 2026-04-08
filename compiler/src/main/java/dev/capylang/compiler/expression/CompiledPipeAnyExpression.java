package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledPipeAnyExpression(
        CompiledExpression source,
        String argumentName,
        CompiledExpression predicate,
        CompiledType type
) implements CompiledExpression {
}
