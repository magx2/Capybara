package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedTupleType;

import java.util.List;

public record LinkedTupleExpression(
        List<LinkedExpression> values,
        LinkedTupleType type
) implements LinkedExpression {
}

