package pl.grzeslowski.capybara.generator.java;

public record JavaPackage(String packageName) {
    @Override
    public String toString() {
        return packageName;
    }
}
