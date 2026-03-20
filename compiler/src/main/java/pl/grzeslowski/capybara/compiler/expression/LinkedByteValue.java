package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record LinkedByteValue(String byteValue) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.BYTE;
    }

    @Override
    public String toString() {
        return byteValue;
    }
}

