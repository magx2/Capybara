package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.Optional;

public record CompiledPipeReduceExpression(CompiledExpression source,
                                         CompiledExpression initialValue,
                                         String accumulatorName,
                                         Optional<String> keyName,
                                         String valueName,
                                         CompiledExpression reducerExpression,
                                         CompiledType type) implements CompiledExpression {
}
