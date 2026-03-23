package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record LongValue(String longValue, Optional<SourcePosition> position) implements Expression {
}

