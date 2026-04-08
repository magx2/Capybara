package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.Optional;

public record CompiledPipeReduceExpression(CompiledExpression source,
                                         CompiledExpression initialValue,
                                         String accumulatorName,
                                         Optional<String> keyName,
                                         String valueName,
                                         CompiledExpression reducerExpression,
                                         CompiledType type) implements CompiledExpression {
}
