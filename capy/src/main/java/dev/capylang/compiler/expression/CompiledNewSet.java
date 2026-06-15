package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.List;

public record CompiledNewSet(List<CompiledExpression> values, CompiledType type) implements CompiledExpression {
}
