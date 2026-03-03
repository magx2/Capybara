package pl.grzeslowski.capybara.parser;

public record GenericInfixExpression(Expression left, GenericOperator operator,
                                     Expression right) implements InfixExpression<GenericOperator> {
    @Override
    public Type type() {
        return null;
    }
}
