package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedTupleType;

import java.util.List;

public record LinkedTupleExpression(
        List<LinkedExpression> values,
        LinkedTupleType type
) implements LinkedExpression {
}

