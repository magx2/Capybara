package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public record CompiledIndexExpression(
        CompiledExpression source,
        CompiledExpression index,
        CompiledType elementType,
        CompiledType type
) implements CompiledExpression {
}

