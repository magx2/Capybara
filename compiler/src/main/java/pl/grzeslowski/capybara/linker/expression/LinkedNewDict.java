package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.List;

public record LinkedNewDict(List<Entry> entries, LinkedType type) implements LinkedExpression {
    public record Entry(LinkedExpression key, LinkedExpression value) {
    }
}
