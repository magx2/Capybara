package pl.grzeslowski.capybara.parser;

public record StringValue(String stringValue) implements Expression {
    @Override
    public String toString() {
        return stringValue;
    }
}
