package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

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

