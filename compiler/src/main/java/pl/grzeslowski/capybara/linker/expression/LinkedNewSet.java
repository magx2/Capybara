package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.List;

public record LinkedNewSet(List<LinkedExpression> values, LinkedType type) implements LinkedExpression {
}
