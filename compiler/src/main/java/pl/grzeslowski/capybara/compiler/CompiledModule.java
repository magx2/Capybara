package pl.grzeslowski.capybara.compiler;

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
        SortedSet<StaticImport> staticImports) implements Comparable<CompiledModule> {

    public static final String EXTENSION = ".json";

    public CompiledModule(
            String name,
            String path,
            Map<String, GenericDataType> types,
            Collection<CompiledFunction> functions,
            Collection<StaticImport> staticImports
    ) {
        this(
                name,
                path,
                new TreeMap<>(types),
                new TreeSet<>(functions),
                new TreeSet<>(staticImports)
        );
    }

    public CompiledModule {
        types = new TreeMap<>(types);
        functions = new TreeSet<>(functions);
        staticImports = new TreeSet<>(staticImports);
    }

    public record StaticImport(String className, String memberName) implements Comparable<StaticImport> {
        @Override
        public int compareTo(StaticImport o) {
            return comparing(StaticImport::className)
                    .thenComparing(StaticImport::memberName)
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

