package pl.grzeslowski.capybara.parser;

import java.util.List;
import java.util.Optional;

public record Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                       Expression expression) implements Definition {
    public Type finalReturnType() {
        return returnType.orElseGet(expression::type);
    }
}
