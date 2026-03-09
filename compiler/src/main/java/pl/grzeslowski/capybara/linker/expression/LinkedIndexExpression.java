package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedIndexExpression(
        LinkedExpression source,
        LinkedExpression index,
        LinkedType elementType,
        LinkedType type
) implements LinkedExpression {
}

