package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledPipeExpression(CompiledExpression source,
                                   String argumentName,
                                   CompiledExpression mapper,
                                   CompiledType type) implements CompiledExpression {
}
