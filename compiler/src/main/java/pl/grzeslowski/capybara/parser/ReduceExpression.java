package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record ReduceExpression(Expression initialValue,
                               String accumulatorName,
                               Optional<String> keyName,
                               String valueName,
                               Expression reducerExpression,
                               Optional<SourcePosition> position) implements Expression {
}
