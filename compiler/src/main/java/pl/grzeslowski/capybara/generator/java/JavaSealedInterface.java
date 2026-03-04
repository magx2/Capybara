package pl.grzeslowski.capybara.generator.java;

import java.util.List;

public record JavaSealedInterface(JavaType name, List<JavaInterfaceMethod> methods,
                                  List<String> permits) implements JavaInterface {
    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof JavaSealedInterface that)) return false;

        return name.equals(that.name);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
