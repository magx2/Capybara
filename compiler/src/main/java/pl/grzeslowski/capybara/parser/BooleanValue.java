package pl.grzeslowski.capybara.parser;

public record BooleanValue(boolean value, SourcePosition position) implements Expression {
}
