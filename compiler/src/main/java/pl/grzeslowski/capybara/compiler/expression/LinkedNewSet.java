package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

import java.util.List;

public record LinkedNewSet(List<LinkedExpression> values, LinkedType type) implements LinkedExpression {
}
