package pl.grzeslowski.capybara.generator.java;

public record JavaType(String name) {
    @Override
    public String toString() {
        return name;
    }
}
