package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

public record LinkedIntValue(String intValue) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.INT;
    }

    @Override
    public String toString() {
        return intValue;
    }
}
