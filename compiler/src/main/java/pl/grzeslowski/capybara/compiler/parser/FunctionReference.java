package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record FunctionReference(String name, Optional<SourcePosition> position) implements Expression {
}
