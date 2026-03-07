package pl.grzeslowski.capybara.generator.java;


import pl.grzeslowski.capybara.linker.expression.LinkedExpression;

import java.util.List;

public record JavaMethod(
        String name,
        boolean isPrivate,
        JavaType returnType,
        List<JavaFunctionParameter> parameters,
        LinkedExpression expression,
        List<String> comments) {
    public record JavaFunctionParameter(JavaType type, String sourceName, String generatedName) {
    }
}
