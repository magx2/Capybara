package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

import java.util.List;

public record LinkedFunctionCall(String name, List<LinkedExpression> arguments,
                                 LinkedType returnType) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return returnType;
    }
}
