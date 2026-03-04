package pl.grzeslowski.capybara.generator.java;

import java.util.List;

public sealed interface JavaInterface permits JavaNormalInterface, JavaSealedInterface {
    JavaType name();

    List<JavaInterfaceMethod> methods();

    public record JavaInterfaceMethod(String name, JavaType returnType) {
    }
}
