package dev.capylang.compiler.parser;

import java.util.HashSet;
import java.util.Set;

public record Functional(Set<Definition> definitions) {
    public Functional reduce(Functional other) {
        var sum = new HashSet<>(definitions);
        sum.addAll(other.definitions);
        return new Functional(sum);
    }
}

