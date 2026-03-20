package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record CompiledFloatValue(String floatValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.FLOAT;
    }

    @Override
    public String toString() {
        return floatValue;
    }
}
