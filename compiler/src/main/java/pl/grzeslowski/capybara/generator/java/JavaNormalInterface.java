package pl.grzeslowski.capybara.generator.java;

import java.util.List;

public record JavaNormalInterface(JavaType name, List<JavaInterfaceMethod> methods) implements JavaInterface {
    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaNormalInterface that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
