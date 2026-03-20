package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedLetExpression(String name,
                                  LinkedExpression value,
                                  LinkedExpression rest) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return rest.type();
    }
}
