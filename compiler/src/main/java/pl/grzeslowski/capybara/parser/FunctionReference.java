package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record FunctionReference(String name, Optional<SourcePosition> position) implements Expression {
}
