package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

public record LinkedLongValue(String longValue) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.LONG;
    }

    @Override
    public String toString() {
        return longValue;
    }
}

