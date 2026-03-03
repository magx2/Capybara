package pl.grzeslowski.capybara.parser;

public record FloatValue(String intValue) implements Expression {
    @Override
    public Type type() {
        return Type.INT;
    }

    @Override
    public Expression value() {
        return this;
    }
}
