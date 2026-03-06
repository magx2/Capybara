package pl.grzeslowski.capybara.parser;

import java.util.List;

public record FunctionCall(String name, List<Expression> arguments, SourcePosition position) implements Expression {
}

