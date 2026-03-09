package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record SliceExpression(
        Expression source,
        Optional<Expression> start,
        Optional<Expression> end,
        Optional<SourcePosition> position
) implements Expression {
}

