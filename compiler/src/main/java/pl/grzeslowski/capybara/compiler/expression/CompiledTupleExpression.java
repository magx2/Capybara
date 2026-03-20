package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledTupleType;

import java.util.List;

public record CompiledTupleExpression(
        List<CompiledExpression> values,
        CompiledTupleType type
) implements CompiledExpression {
}

