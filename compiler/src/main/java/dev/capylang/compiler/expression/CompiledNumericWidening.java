package dev.capylang.compiler.expression;

import dev.capylang.compiler.PrimitiveLinkedType;

public record CompiledNumericWidening(CompiledExpression expression, PrimitiveLinkedType type) implements CompiledExpression {
}
