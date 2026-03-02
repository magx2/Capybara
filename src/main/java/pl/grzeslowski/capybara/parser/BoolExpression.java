package pl.grzeslowski.capybara.parser;

public sealed interface BoolExpression extends Expression permits BoolInfixExpression, BooleanValue {
    @Override
    pl.grzeslowski.capybara.parser.BoolExpression value();

    @Override
    default Type type() {
        return Type.BOOL;
    }
}
