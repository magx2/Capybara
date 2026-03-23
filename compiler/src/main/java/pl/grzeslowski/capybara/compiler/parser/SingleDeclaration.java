package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public record SingleDeclaration(String name, Optional<SourcePosition> position) implements Definition {
}
