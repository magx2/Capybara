package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public record LinkedVariable(String name, LinkedType type) implements LinkedExpression {
}
