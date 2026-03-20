package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledLetExpression(String name,
                                  CompiledExpression value,
                                  CompiledExpression rest) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return rest.type();
    }
}
