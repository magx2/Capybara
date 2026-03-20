package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

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
