package pl.grzeslowski.capybara.parser;

public sealed interface InfixExpression<OperatorT extends InfixOperator> extends Expression permits BoolInfixExpression, GenericInfixExpression {
    Expression left();

    OperatorT operator();

    Expression right();
}
