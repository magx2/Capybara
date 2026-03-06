package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record LambdaExpression(String argumentName, Expression expression,
                               Optional<SourcePosition> position) implements Expression {
}
