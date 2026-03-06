package pl.grzeslowski.capybara.parser;

public record StringValue(String stringValue, SourcePosition position) implements Expression {
@Override
    public String toString() {
        return stringValue;
    }
}

