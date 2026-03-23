package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record IndexExpression(
        Expression source,
        Expression index,
        Optional<SourcePosition> position
) implements Expression {
}

