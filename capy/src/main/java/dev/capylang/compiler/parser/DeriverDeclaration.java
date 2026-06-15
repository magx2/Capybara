package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record DeriverDeclaration(
        String name,
        List<DeriverMethod> methods,
        List<String> comments,
        Visibility visibility,
        Optional<SourcePosition> position,
        List<AnnotationUsage> annotations
) implements Definition {
    public DeriverDeclaration(
            String name,
            List<DeriverMethod> methods,
            List<String> comments,
            Visibility visibility,
            Optional<SourcePosition> position
    ) {
        this(name, methods, comments, visibility, position, List.of());
    }

    public record DeriverMethod(
            String name,
            List<Parameter> parameters,
            Type returnType,
            Expression expression,
            List<String> comments,
            Optional<SourcePosition> position,
            List<AnnotationUsage> annotations
    ) {
        public DeriverMethod(
                String name,
                List<Parameter> parameters,
                Type returnType,
                Expression expression,
                List<String> comments,
                Optional<SourcePosition> position
        ) {
            this(name, parameters, returnType, expression, comments, position, List.of());
        }
    }
}
