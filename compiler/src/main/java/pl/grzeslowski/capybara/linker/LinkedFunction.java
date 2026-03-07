package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.linker.expression.LinkedExpression;

import java.util.List;

public record LinkedFunction(String name,
                             LinkedType returnType,
                             List<LinkedFunctionParameter> parameters,
                             LinkedExpression expression,
                             List<String> comments) {
    public record LinkedFunctionParameter(String name, LinkedType type) {
    }
}
