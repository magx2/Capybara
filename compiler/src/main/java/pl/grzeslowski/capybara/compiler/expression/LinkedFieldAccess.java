package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedFieldAccess(LinkedExpression source, String field, LinkedType type) implements LinkedExpression {
}
