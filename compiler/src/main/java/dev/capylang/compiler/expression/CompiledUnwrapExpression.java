package dev.capylang.compiler.expression;

import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledUnwrapExpression(CompiledExpression expression, PrimitiveLinkedType type) implements CompiledExpression {
}
