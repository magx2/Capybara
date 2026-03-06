package pl.grzeslowski.capybara.parser;

public record LetExpression(String name, Expression value, Expression rest, SourcePosition position) implements Expression {
}

