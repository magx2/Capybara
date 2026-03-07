package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

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

