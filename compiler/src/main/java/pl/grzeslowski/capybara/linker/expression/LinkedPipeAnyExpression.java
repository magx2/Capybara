package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedPipeAnyExpression(
        LinkedExpression source,
        String argumentName,
        LinkedExpression predicate,
        LinkedType type
) implements LinkedExpression {
}
