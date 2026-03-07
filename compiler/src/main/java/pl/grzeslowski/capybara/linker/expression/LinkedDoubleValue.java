package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

public record LinkedDoubleValue(String doubleValue) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.DOUBLE;
    }

    @Override
    public String toString() {
        return doubleValue;
    }
}

