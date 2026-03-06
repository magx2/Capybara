package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedFieldAccess(LinkedExpression source, String field, LinkedType type) implements LinkedExpression {
}
