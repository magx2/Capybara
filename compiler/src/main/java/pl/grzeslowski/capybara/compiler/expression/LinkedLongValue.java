package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

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

