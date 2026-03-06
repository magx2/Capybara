package pl.grzeslowski.capybara.parser;

public record InfixExpression(Expression left,
                              InfixOperator operator,
                              Expression right,
                              SourcePosition position) implements Expression {
}

