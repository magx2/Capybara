package dev.capylang.compiler.parser;

import java.util.Optional;

public record LetExpression(String name, Optional<Type> declaredType, Kind kind, Expression value, Expression rest,
                            Optional<SourcePosition> position) implements Expression {
    public enum Kind {
        ASSIGN,
        RESULT_BIND
    }
}

