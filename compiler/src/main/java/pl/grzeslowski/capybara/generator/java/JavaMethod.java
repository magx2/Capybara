package pl.grzeslowski.capybara.generator.java;


import pl.grzeslowski.capybara.compiler.expression.LinkedExpression;

import java.util.List;

public record JavaMethod(
        String name,
        boolean isPrivate,
        boolean programMain,
        List<String> typeParameters,
        JavaType returnType,
        List<JavaFunctionParameter> parameters,
        LinkedExpression expression,
        List<String> comments) {
    public record JavaFunctionParameter(JavaType type, String sourceName, String generatedName) {
    }
}
