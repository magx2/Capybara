package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

public record LinkedFloatValue(String floatValue) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.FLOAT;
    }

    @Override
    public String toString() {
        return floatValue;
    }
}
