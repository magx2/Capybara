package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.Optional;

public record LinkedPipeReduceExpression(LinkedExpression source,
                                         LinkedExpression initialValue,
                                         String accumulatorName,
                                         Optional<String> keyName,
                                         String valueName,
                                         LinkedExpression reducerExpression,
                                         LinkedType type) implements LinkedExpression {
}
