package dev.capylang.compiler.parser;

import java.util.List;
import java.util.Optional;

public record SingleDeclaration(String name, List<String> comments, Optional<SourcePosition> position) implements Definition {
    public SingleDeclaration(String name, Optional<SourcePosition> position) {
        this(name, List.of(), position);
    }
}
