package pl.grzeslowski.capybara.parser;

import java.util.List;

public record Function(String name, List<Parameter> parameters, Type returnType,
                       Expression expression) implements Definition {
    public Function {
        if (returnType == null) {
            returnType = expression.type();
        }
    }
}
