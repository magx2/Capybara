package pl.grzeslowski.capybara.parser;

public enum BooleanValue implements BoolExpression {
    TRUE, FALSE;

    @Override
    public BoolExpression value() {
        return this;
    }
}
