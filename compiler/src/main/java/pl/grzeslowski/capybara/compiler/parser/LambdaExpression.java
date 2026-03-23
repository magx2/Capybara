package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;
import java.util.List;

public record LambdaExpression(List<String> argumentNames, Expression expression,
                               Optional<SourcePosition> position) implements Expression {
}
