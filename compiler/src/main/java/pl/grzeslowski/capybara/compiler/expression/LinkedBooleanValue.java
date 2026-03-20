package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

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
