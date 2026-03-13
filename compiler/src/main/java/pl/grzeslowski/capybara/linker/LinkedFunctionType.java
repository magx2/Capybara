package pl.grzeslowski.capybara.linker;

public record LinkedFunctionType(LinkedType argumentType, LinkedType returnType) implements LinkedType {
    @Override
    public String name() {
        return argumentType.name() + "=>" + returnType.name();
    }
}
