package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record LinkedStringValue(String stringValue) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.STRING;
    }

    @Override
    public String toString() {
        return stringValue;
    }
}
