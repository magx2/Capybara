package pl.grzeslowski.capybara.parser;

public record BoolInfixExpression(Expression left,
                                  BoolInfixOperator operator,
                                  Expression right) implements InfixExpression<BoolInfixOperator>, BoolExpression {
}
