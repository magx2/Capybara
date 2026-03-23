package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record NothingValue(Optional<SourcePosition> position) implements Expression {
}

