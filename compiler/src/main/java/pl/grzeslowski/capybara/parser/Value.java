package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record Value(String name, Optional<SourcePosition> position) implements Expression {
}


