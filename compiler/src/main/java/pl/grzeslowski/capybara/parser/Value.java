package pl.grzeslowski.capybara.parser;

public record Value(String name, SourcePosition position) implements Expression {
}

