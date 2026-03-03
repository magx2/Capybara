package pl.grzeslowski.capybara.parser;

public sealed interface BoolExpression extends Expression permits BoolInfixExpression, BooleanValue {
    @Override
    default Type type() {
        return PrimitiveType.BOOL;
    }
}
