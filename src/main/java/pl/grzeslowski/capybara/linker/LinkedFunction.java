package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.parser.Expression;

import java.util.List;

public record LinkedFunction(String name,
                             LinkedType returnType,
                             List<LinkedFunctionParameter> parameters,
                             LinkedExpression expression) {
    public record LinkedFunctionParameter(String name, LinkedType type) {
    }
}
