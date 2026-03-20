package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

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
