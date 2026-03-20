package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

import java.util.Optional;

public record LinkedSliceExpression(
        LinkedExpression source,
        Optional<LinkedExpression> start,
        Optional<LinkedExpression> end,
        LinkedType type
) implements LinkedExpression {
}

