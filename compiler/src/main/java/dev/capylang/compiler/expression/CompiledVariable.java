package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledVariable(String name, CompiledType type) implements CompiledExpression {
}
