package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledFieldAccess(CompiledExpression source, String field, CompiledType type) implements CompiledExpression {
}
