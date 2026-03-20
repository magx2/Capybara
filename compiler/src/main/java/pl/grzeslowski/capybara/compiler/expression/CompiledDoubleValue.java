package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record CompiledDoubleValue(String doubleValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.DOUBLE;
    }

    @Override
    public String toString() {
        return doubleValue;
    }
}

