package pl.grzeslowski.capybara.generator.java;

import java.util.List;

public sealed interface JavaInterface permits JavaNormalInterface, JavaSealedInterface {
    JavaType name();

    List<JavaInterfaceMethod> methods();

    List<JavaMethod> defaultMethods();

    public record JavaInterfaceMethod(String name, JavaType returnType) {
    }
}
