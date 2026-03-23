package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record Value(String name, Optional<SourcePosition> position) implements Expression {
}


