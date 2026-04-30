package dev.capylang.compiler.parser;

import dev.capylang.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                       Expression expression,
                       List<String> comments,
                       Visibility visibility,
                       Optional<SourcePosition> position,
                       boolean tailRecursive) implements Definition {
    public Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                    Expression expression,
                    List<String> comments,
                    Visibility visibility,
                    Optional<SourcePosition> position) {
        this(name, parameters, returnType, expression, comments, visibility, position, false);
    }

    public Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                    Expression expression,
                    List<String> comments,
                    Optional<SourcePosition> position) {
        this(name, parameters, returnType, expression, comments, null, position, false);
    }
}
