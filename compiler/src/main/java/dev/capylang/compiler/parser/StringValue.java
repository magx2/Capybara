package dev.capylang.compiler.parser;

import java.util.Optional;

public record StringValue(String stringValue, Optional<SourcePosition> position) implements Expression {
    @Override
    public String toString() {
        return stringValue;
    }
}


