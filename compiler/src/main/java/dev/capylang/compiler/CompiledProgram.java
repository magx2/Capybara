package dev.capylang.compiler;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

public record CompiledProgram(SortedSet<CompiledModule> modules) {
    public CompiledProgram(Collection<CompiledModule> modules) {
        this(new TreeSet<>(modules));
    }

    public CompiledProgram {
        modules = new TreeSet<>(modules);
    }
}
