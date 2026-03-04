package pl.grzeslowski.capybara.parser;

public record InfixExpression(Expression left,
                              InfixOperator operator,
                              Expression right) implements Expression {
}
