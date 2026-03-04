package pl.grzeslowski.capybara.parser;

import java.util.List;

public record FunctionCall(String name, List<Expression> arguments) implements Expression {
}
