package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedIndexExpression(
        LinkedExpression source,
        LinkedExpression index,
        LinkedType elementType,
        LinkedType type
) implements LinkedExpression {
}

