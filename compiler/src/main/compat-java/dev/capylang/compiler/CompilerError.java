package dev.capylang.compiler;

import java.util.Comparator;

public record CompilerError(int line, int column, String file, String message) implements Comparable<CompilerError> {
    public CompilerError(String message) {
        this(0, 0, "", message);
    }

    @Override
    public int compareTo(CompilerError other) {
        return Comparator
                .comparing(CompilerError::file)
                .thenComparing(CompilerError::line)
                .thenComparing(CompilerError::column)
                .thenComparing(CompilerError::message)
                .compare(this, other);
    }

    @Override
    public String toString() {
        return "%s %d:%s: %s".formatted(file, line, column, message);
    }
}
