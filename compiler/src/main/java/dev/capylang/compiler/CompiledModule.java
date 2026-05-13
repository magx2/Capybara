package dev.capylang.compiler;

import dev.capylang.compiler.parser.DeriverDeclaration;

import java.util.Collection;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import static java.util.Comparator.comparing;

public record CompiledModule(
        String name,
        String path,
        SortedMap<String, GenericDataType> types,
        SortedSet<CompiledFunction> functions,
        SortedMap<String, DeriverDeclaration> derivers,
        SortedSet<StaticImport> staticImports) implements Comparable<CompiledModule> {

    public static final String EXTENSION = ".json";

    public CompiledModule(
            String name,
            String path,
            Map<String, GenericDataType> types,
            Collection<CompiledFunction> functions,
            Collection<StaticImport> staticImports
    ) {
        this(name, path, types, functions, Map.of(), staticImports);
    }

    public CompiledModule(
            String name,
            String path,
            Map<String, GenericDataType> types,
            Collection<CompiledFunction> functions,
            Map<String, DeriverDeclaration> derivers,
            Collection<StaticImport> staticImports
    ) {
        this(
                name,
                path,
                new TreeMap<>(types),
                new TreeSet<>(functions),
                new TreeMap<>(derivers),
                new TreeSet<>(staticImports)
        );
    }

    public CompiledModule {
        types = new TreeMap<>(types);
        functions = new TreeSet<>(functions);
        derivers = derivers == null ? new TreeMap<>() : new TreeMap<>(derivers);
        staticImports = new TreeSet<>(staticImports);
    }

    public record StaticImport(String className, String memberName, boolean enumValue) implements Comparable<StaticImport> {
        public StaticImport(String className, String memberName) {
            this(className, memberName, false);
        }

        @Override
        public int compareTo(StaticImport o) {
            return comparing(StaticImport::className)
                    .thenComparing(StaticImport::memberName)
                    .thenComparing(StaticImport::enumValue)
                    .compare(this, o);
        }
    }

    @Override
    public int compareTo(CompiledModule o) {
        return comparing(CompiledModule::path)
                .thenComparing(CompiledModule::name)
                .compare(this, o);
    }

    @Override
    public final boolean equals(Object o) {
        if (!(o instanceof CompiledModule that)) return false;

        return name.equals(that.name) && path.equals(that.path);
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
