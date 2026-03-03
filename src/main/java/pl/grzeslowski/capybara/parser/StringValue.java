package pl.grzeslowski.capybara.parser;

public record StringValue(String stringValue) implements Expression {
    @Override
    public Type type() {
        return Type.STRING;
    }
}
