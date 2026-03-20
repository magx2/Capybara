package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public enum CompiledBooleanValue implements CompiledExpression {
    TRUE(true), FALSE(false);

    private final boolean value;

    CompiledBooleanValue(boolean value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return Boolean.toString(value);
    }

    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.BOOL;
    }
}
