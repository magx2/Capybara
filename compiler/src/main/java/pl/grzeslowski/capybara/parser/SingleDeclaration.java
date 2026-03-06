package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public record SingleDeclaration(String name, Optional<SourcePosition> position) implements Definition {
}
