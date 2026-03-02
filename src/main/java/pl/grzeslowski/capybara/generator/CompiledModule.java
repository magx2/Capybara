package pl.grzeslowski.capybara.generator;

public record CompiledModule(String name, String path, String code) {
    public CompiledModule {
        if (name.isBlank()) {
            throw new IllegalArgumentException("Module name cannot be blank");
        }
        if (!name.endsWith(".java")
            && !name.endsWith(".js")
            && !name.endsWith(".py")) {
            throw new IllegalArgumentException("Module name must end with .java, .js or .py");
        }
        if (path.isBlank()) {
            throw new IllegalArgumentException("Module path cannot be blank");
        }
    }
}
