package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

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
