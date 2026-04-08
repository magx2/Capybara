package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledTupleType;

import java.util.List;

public record CompiledTupleExpression(
        List<CompiledExpression> values,
        CompiledTupleType type
) implements CompiledExpression {
}

