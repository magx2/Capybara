package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record DeriverDeclaration(
        String name,
        List<DeriverMethod> methods,
        List<String> comments,
        Visibility visibility,
        Optional<SourcePosition> position
) implements Definition {
    public record DeriverMethod(
            String name,
            List<Parameter> parameters,
            Type returnType,
            Expression expression,
            List<String> comments,
            Optional<SourcePosition> position
    ) {
    }
}
