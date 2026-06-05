package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public record CompiledPipeFlatMapExpression(CompiledExpression source,
                                          String argumentName,
                                          CompiledExpression mapper,
                                          CompiledType type) implements CompiledExpression {
}
