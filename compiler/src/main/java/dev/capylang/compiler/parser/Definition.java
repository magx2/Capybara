package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.Optional;

public sealed interface Definition permits DataDeclaration, DeriverDeclaration, EnumDeclaration, Function, PrimitiveBackedTypeDeclaration, TypeDeclaration {
    Optional<SourcePosition> position();

    default Visibility visibility() {
        return null;
    }
}
