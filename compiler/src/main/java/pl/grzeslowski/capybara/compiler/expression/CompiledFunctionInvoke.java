package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.List;

public record CompiledFunctionInvoke(
        CompiledExpression function,
        List<CompiledExpression> arguments,
        CompiledType returnType
) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return returnType;
    }
}
