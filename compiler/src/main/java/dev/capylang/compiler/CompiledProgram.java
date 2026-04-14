package dev.capylang.compiler;

import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public record CompiledProgram(SortedSet<CompiledModule> modules, List<ObjectOrientedModule> objectOrientedModules) {
    public CompiledProgram(Collection<CompiledModule> modules) {
        this(new TreeSet<>(modules), List.of());
    }

    public CompiledProgram(Collection<CompiledModule> modules, Collection<ObjectOrientedModule> objectOrientedModules) {
        this(new TreeSet<>(modules), List.copyOf(objectOrientedModules));
    }

    public CompiledProgram {
        modules = new TreeSet<>(modules);
        objectOrientedModules = List.copyOf(objectOrientedModules);
    }
}
