package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledIfExpression(CompiledExpression condition,
                                 CompiledExpression thenBranch,
                                 CompiledExpression elseBranch,
                                 CompiledType type) implements CompiledExpression {
}
