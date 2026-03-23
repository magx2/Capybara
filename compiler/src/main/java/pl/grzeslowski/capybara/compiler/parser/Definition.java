package pl.grzeslowski.capybara.compiler.parser;

import pl.grzeslowski.capybara.compiler.Visibility;

import java.util.Optional;

public sealed interface Definition permits DataDeclaration, EnumDeclaration, Function, SingleDeclaration, TypeDeclaration {
    Optional<SourcePosition> position();

    default Visibility visibility() {
        return null;
    }
}
