package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.Optional;

public record LinkedSliceExpression(
        LinkedExpression source,
        Optional<LinkedExpression> start,
        Optional<LinkedExpression> end,
        LinkedType type
) implements LinkedExpression {
}

