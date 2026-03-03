package pl.grzeslowski.capybara.parser;

public record IntValue(String intValue) implements Expression {
    @Override
    public Type type() {
        return PrimitiveType.INT;
    }
}
