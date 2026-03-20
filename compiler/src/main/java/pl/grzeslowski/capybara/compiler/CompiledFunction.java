package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.compiler.expression.CompiledExpression;

import java.util.List;

public record CompiledFunction(String name,
                             CompiledType returnType,
                             List<CompiledFunctionParameter> parameters,
                             CompiledExpression expression,
                             List<String> comments,
                             boolean programMain) {
    public CompiledFunction(String name,
                          CompiledType returnType,
                          List<CompiledFunctionParameter> parameters,
                          CompiledExpression expression,
                          List<String> comments) {
        this(name, returnType, parameters, expression, comments, false);
    }

    public record CompiledFunctionParameter(String name, CompiledType type) {
    }
}
