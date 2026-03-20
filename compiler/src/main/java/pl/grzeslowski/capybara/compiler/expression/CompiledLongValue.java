package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record CompiledLongValue(String longValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.LONG;
    }

    @Override
    public String toString() {
        return longValue;
    }
}

