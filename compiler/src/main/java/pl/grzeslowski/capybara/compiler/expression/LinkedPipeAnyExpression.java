package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedPipeAnyExpression(
        LinkedExpression source,
        String argumentName,
        LinkedExpression predicate,
        LinkedType type
) implements LinkedExpression {
}
