package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

public record CompiledStringValue(String stringValue) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.STRING;
    }

    @Override
    public String toString() {
        return stringValue;
    }
}
