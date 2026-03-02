package pl.grzeslowski.capybara.parser;

public record Variable(String name) implements Expression {
    @Override
    public Type type() {
        // todo
        return null;
    }

    @Override
    public Expression value() {
        // todo implement it
        return null;
    }
}
