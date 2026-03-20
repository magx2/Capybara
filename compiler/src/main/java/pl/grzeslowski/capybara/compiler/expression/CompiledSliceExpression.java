package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

import java.util.Optional;

public record CompiledSliceExpression(
        CompiledExpression source,
        Optional<CompiledExpression> start,
        Optional<CompiledExpression> end,
        CompiledType type
) implements CompiledExpression {
}

