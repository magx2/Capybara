package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedFunctionType;
import pl.grzeslowski.capybara.compiler.LinkedType;

public record LinkedLambdaExpression(
        String argumentName,
        LinkedExpression expression,
        LinkedFunctionType functionType
) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return functionType;
    }
}
