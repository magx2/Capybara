package dev.capylang.compiler.parser;

import java.util.Optional;

public record NothingValue(Optional<SourcePosition> position, String literal) implements Expression {
    public NothingValue(Optional<SourcePosition> position) {
        this(position, "???");
    }
}

