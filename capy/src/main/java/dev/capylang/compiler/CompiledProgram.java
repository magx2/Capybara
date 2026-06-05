package dev.capylang.compiler;

import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public record CompiledProgram(
        SortedSet<CompiledModule> modules,
        List<ObjectOrientedModule> objectOrientedModules,
        NativeProviderManifest nativeProviders,
        NativeProviderCatalog nativeProviderCatalog
) {
    public CompiledProgram(Collection<CompiledModule> modules) {
        this(new TreeSet<>(modules), List.of(), NativeProviderManifest.empty(), NativeProviderCatalog.empty());
    }

    public CompiledProgram(Collection<CompiledModule> modules, Collection<ObjectOrientedModule> objectOrientedModules) {
        this(new TreeSet<>(modules), List.copyOf(objectOrientedModules), NativeProviderManifest.empty(), NativeProviderCatalog.empty());
    }

    public CompiledProgram(
            Collection<CompiledModule> modules,
            Collection<ObjectOrientedModule> objectOrientedModules,
            NativeProviderManifest nativeProviders
    ) {
        this(new TreeSet<>(modules), List.copyOf(objectOrientedModules), nativeProviders, NativeProviderCatalog.empty());
    }

    public CompiledProgram(
            Collection<CompiledModule> modules,
            Collection<ObjectOrientedModule> objectOrientedModules,
            NativeProviderManifest nativeProviders,
            NativeProviderCatalog nativeProviderCatalog
    ) {
        this(new TreeSet<>(modules), List.copyOf(objectOrientedModules), nativeProviders, nativeProviderCatalog);
    }

    public CompiledProgram {
        modules = new TreeSet<>(modules);
        objectOrientedModules = List.copyOf(objectOrientedModules);
        nativeProviders = nativeProviders == null ? NativeProviderManifest.empty() : nativeProviders;
        nativeProviderCatalog = nativeProviderCatalog == null ? NativeProviderCatalog.empty() : nativeProviderCatalog;
    }
}
