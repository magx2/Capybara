package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record CompiledIntValue(String intValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.INT;
    }

    @Override
    public String toString() {
        return intValue;
    }
}
