package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.List;

public record LinkedFunctionInvoke(
        LinkedExpression function,
        List<LinkedExpression> arguments,
        LinkedType returnType
) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return returnType;
    }
}
