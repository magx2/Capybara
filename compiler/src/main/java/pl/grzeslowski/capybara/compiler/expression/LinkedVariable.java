package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedVariable(String name, LinkedType type) implements LinkedExpression {
}
