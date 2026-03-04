package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

public enum LinkedBooleanValue implements LinkedExpression {
    TRUE(true), FALSE(false);

    private final boolean value;

    LinkedBooleanValue(boolean value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }

    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.BOOL;
    }
}
