package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.List;

public record CompiledFunctionCall(String name, List<CompiledExpression> arguments,
                                 CompiledType returnType) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return returnType;
    }
}
