package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledFunctionType;
import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledLambdaExpression(
        String argumentName,
        CompiledExpression expression,
        CompiledFunctionType functionType
) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return functionType;
    }
}
