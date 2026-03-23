package pl.grzeslowski.capybara.compiler.parser;

import java.util.Optional;

public sealed interface Definition permits DataDeclaration, EnumDeclaration, Function, SingleDeclaration, TypeDeclaration {
    Optional<SourcePosition> position();
}

