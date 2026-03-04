package pl.grzeslowski.capybara.parser;

public record LetExpression(String name, Expression value, Expression rest) implements Expression {
}
