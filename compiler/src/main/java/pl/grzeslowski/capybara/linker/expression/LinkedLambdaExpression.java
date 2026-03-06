package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedFunctionType;
import pl.grzeslowski.capybara.linker.LinkedType;

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
