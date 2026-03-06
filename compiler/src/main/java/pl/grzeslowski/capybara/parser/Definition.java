package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public sealed interface Definition permits DataDeclaration, Function, SingleDeclaration, TypeDeclaration {
    Optional<SourcePosition> position();
}

