package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record PrimitiveBackedTypeDeclaration(
        String name,
        PrimitiveType backingType,
        Optional<Expression> constructor,
        List<String> comments,
        Visibility visibility,
        Optional<SourcePosition> position
) implements Definition {
}
