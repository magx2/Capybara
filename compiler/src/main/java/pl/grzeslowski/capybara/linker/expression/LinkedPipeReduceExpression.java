package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedPipeReduceExpression(LinkedExpression source,
                                         LinkedExpression initialValue,
                                         String accumulatorName,
                                         String valueName,
                                         LinkedExpression reducerExpression,
                                         LinkedType type) implements LinkedExpression {
}
