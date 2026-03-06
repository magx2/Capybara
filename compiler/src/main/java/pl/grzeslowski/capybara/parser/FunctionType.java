package pl.grzeslowski.capybara.parser;

public record FunctionType(Type argumentType, Type returnType) implements Type {
    @Override
    public String name() {
        return argumentType.name() + "->" + returnType.name();
    }
}
