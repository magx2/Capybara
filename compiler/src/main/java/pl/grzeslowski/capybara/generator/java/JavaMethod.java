package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.linker.LinkedExpression;

import java.util.List;

public record JavaMethod(
        String name,
        JavaType returnType,
        List<JavaFunctionParameter> parameters,
        LinkedExpression expression) {
    public record JavaFunctionParameter(JavaType type, String name) {
    }
}
