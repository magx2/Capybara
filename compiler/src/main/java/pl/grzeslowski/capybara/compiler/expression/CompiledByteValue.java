package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record CompiledByteValue(String byteValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.BYTE;
    }

    @Override
    public String toString() {
        return byteValue;
    }
}

