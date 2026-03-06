package pl.grzeslowski.capybara.parser;

public record IntValue(String intValue, SourcePosition position) implements Expression {
}

