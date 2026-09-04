package dev.capylang.generator;

import dev.capylang.AsyncTasks;
import dev.capylang.compiler.BackendCompilationContext;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.LinkedJsonCodec;
import dev.capylang.generator.internal.GeneratedJavaGenerator;

import java.io.IOException;
import java.lang.reflect.RecordComponent;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/** Bootstrap-compatible entry point for the self-hosted Java generator. */
public final class JavaGenerator {
    private static final ThreadLocal<ScopedProgram> MAIN_PROGRAM_CONTEXT = new ThreadLocal<>();
    private static final ThreadLocal<CachedGeneration> LAST_GENERATION = new ThreadLocal<>();
    private static final Map<String, Optional<Map<String, Object>>> BUNDLED_IMPORT_MODULES =
            new ConcurrentHashMap<>();
    private static final String PRIMITIVE_SCHEMA_PREFIX = "__capy_schema_primitive|";

    private JavaGenerator() {
    }

    public static GeneratedProgram javaGenerator(CompiledProgram program) {
        var standaloneTestProgram = standaloneTestProgram(program);
        var invocation = BackendCompilationContext.generationInvocation().orElse(null);
        var scopedContext = MAIN_PROGRAM_CONTEXT.get();
        var mainContext = scopedContext != null && scopedContext.invocation() == invocation
                ? scopedContext.program()
                : null;
        var generationProgram = standaloneTestProgram
                && mainContext != null
                && importsMainModule(program, mainContext)
                ? mergePrograms(mainContext, program)
                : program;
        if (standaloneTestProgram) {
            MAIN_PROGRAM_CONTEXT.remove();
        } else if (invocation != null && program.modules().stream().noneMatch(JavaGenerator::testModule)) {
            MAIN_PROGRAM_CONTEXT.set(new ScopedProgram(invocation, program));
        }
        return javaGeneratorWithLookup(program, generationProgram);
    }

    public static GeneratedProgram javaGeneratorWithLookup(
            CompiledProgram source,
            CompiledProgram lookup
    ) {
        var sameProgram = source == lookup;
        source = deduplicateProgram(source);
        lookup = sameProgram ? source : deduplicateProgram(lookup);
        var cached = LAST_GENERATION.get();
        if (cached != null && cached.source().equals(source) && cached.lookup().equals(lookup)) {
            return cached.generated();
        }
        var sourceProgram = dataMap(toGeneratedValue(source));
        var lookupProgram = source == lookup
                ? sourceProgram
                : dataMap(toGeneratedValue(lookup));
        var compiledProgram = withBundledImportModules(lookupProgram);
        var context = GeneratedJavaGenerator.java_generator_context__167_0(compiledProgram);
        var tasks = new ArrayList<Supplier<List<GeneratedModule>>>();
        for (var module : list(sourceProgram.get("modules"))) {
            tasks.add(() -> generatedModules(GeneratedJavaGenerator.java_generator_module_with_context__147_0(
                    module,
                    context
            )));
        }
        tasks.add(() -> generatedModules(GeneratedJavaGenerator.java_generator_support__154_0(
                sourceProgram
        )));
        var parts = AsyncTasks.run(tasks);
        var objectInterfaceModules = new ArrayList<GeneratedModule>();
        var javaModules = new ArrayList<GeneratedModule>();
        for (var index = 0; index < parts.size() - 1; index++) {
            var part = parts.get(index);
            if (!part.isEmpty()) {
                objectInterfaceModules.addAll(part.subList(0, part.size() - 1));
                javaModules.add(part.getLast());
            }
        }
        javaModules.addAll(parts.getLast());
        objectInterfaceModules.addAll(javaModules);
        var modules = List.copyOf(objectInterfaceModules);
        var result = new GeneratedProgram(modules);
        LAST_GENERATION.set(new CachedGeneration(source, lookup, result));
        return result;
    }

    static CompiledProgram deduplicateProgram(CompiledProgram program) {
        var modules = new LinkedHashMap<String, CompiledModule>();
        for (var module : program.modules()) {
            modules.putIfAbsent(moduleImportPath(module), module);
        }
        var objectOrientedModules = new LinkedHashMap<String, dev.capylang.compiler.CompiledObjectOrientedUnit>();
        for (var module : program.objectOrientedModules()) {
            var path = normalizeModuleImportPath(module.path());
            var modulePath = path.isBlank() ? module.name() : path + "/" + module.name();
            objectOrientedModules.putIfAbsent(modulePath, module);
        }
        if (modules.size() == program.modules().size()
                && objectOrientedModules.size() == program.objectOrientedModules().size()) {
            return program;
        }
        return new CompiledProgram(
                List.copyOf(modules.values()),
                List.copyOf(objectOrientedModules.values()),
                program.nativeProviders(),
                program.nativeProviderCatalog()
        );
    }

    private static boolean standaloneTestProgram(CompiledProgram program) {
        return !program.modules().isEmpty() && program.modules().stream().allMatch(JavaGenerator::testModule);
    }

    private static boolean testModule(CompiledModule module) {
        return module.name().endsWith(".test");
    }

    private static boolean importsMainModule(CompiledProgram tests, CompiledProgram main) {
        var mainModulePaths = main.modules().stream()
                .map(JavaGenerator::moduleImportPath)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        return tests.modules().stream()
                .flatMap(module -> module.imports().stream())
                .map(importDeclaration -> normalizeModuleImportPath(importDeclaration.modulePath()))
                .anyMatch(mainModulePaths::contains);
    }

    private static String moduleImportPath(CompiledModule module) {
        var path = normalizeModuleImportPath(module.path());
        return path.isBlank() ? module.name() : path + "/" + module.name();
    }

    static CompiledProgram mergePrograms(CompiledProgram main, CompiledProgram tests) {
        var modules = new ArrayList<CompiledModule>(main.modules());
        modules.addAll(tests.modules());
        var objectOrientedModules = new ArrayList<>(main.objectOrientedModules());
        objectOrientedModules.addAll(tests.objectOrientedModules());
        return new CompiledProgram(
                List.copyOf(modules),
                List.copyOf(objectOrientedModules),
                tests.nativeProviders(),
                tests.nativeProviderCatalog()
        );
    }

    static Optional<GenerationPrograms> separateTestGeneration(
            CompiledProgram program,
            CompiledProgram main
    ) {
        if (main == null) {
            return Optional.empty();
        }
        var mainModules = main.modules().stream()
                .map(JavaGenerator::moduleImportPath)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        var sourceModules = program.modules().stream()
                .filter(module -> !mainModules.contains(moduleImportPath(module)))
                .toList();
        if (sourceModules.isEmpty() || sourceModules.stream().noneMatch(JavaGenerator::testModule)) {
            return Optional.empty();
        }
        var mainObjectModules = main.objectOrientedModules().stream()
                .map(module -> normalizeModuleImportPath(module.path()) + "/" + module.name())
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
        var sourceObjectModules = program.objectOrientedModules().stream()
                .filter(module -> !mainObjectModules.contains(
                        normalizeModuleImportPath(module.path()) + "/" + module.name()
                ))
                .toList();
        var source = new CompiledProgram(
                sourceModules,
                sourceObjectModules,
                program.nativeProviders(),
                program.nativeProviderCatalog()
        );
        return Optional.of(new GenerationPrograms(source, deduplicateProgram(mergePrograms(main, program))));
    }

    static Map<String, Object> withBundledImportModules(Map<String, Object> program) {
        var modules = new ArrayList<>(list(program.get("modules")));
        var knownModules = new LinkedHashSet<String>();
        var pendingImports = new ArrayDeque<String>();
        modules.stream().map(JavaGenerator::dataMap).forEach(module -> {
            knownModules.add(moduleImportPath(module));
            enqueueImports(module, pendingImports);
        });

        while (!pendingImports.isEmpty()) {
            var modulePath = normalizeModuleImportPath(pendingImports.removeFirst());
            if (!modulePath.startsWith("capy/") || knownModules.contains(modulePath)) {
                continue;
            }
            var module = readBundledModule(modulePath);
            if (module.isEmpty()) {
                continue;
            }
            var generatedModule = module.get();
            modules.add(generatedModule);
            knownModules.add(modulePath);
            enqueueImports(generatedModule, pendingImports);
        }

        var linkedModules = withBundledPrimitiveSchemaFunctions(modules);
        if (modules.size() == list(program.get("modules")).size() && linkedModules.equals(modules)) {
            return program;
        }
        var enriched = new LinkedHashMap<>(program);
        enriched.put("modules", linkedModules);
        return Collections.unmodifiableMap(enriched);
    }

    private static List<Object> withBundledPrimitiveSchemaFunctions(List<Object> modules) {
        var modulesByPath = new LinkedHashMap<String, Map<String, Object>>();
        modules.stream().map(JavaGenerator::dataMap).forEach(module ->
                modulesByPath.put(moduleImportPath(module), module)
        );
        return modules.stream().map(value -> {
            var module = dataMap(value);
            var functions = new ArrayList<>(list(module.get("functions")));
            var functionNames = new LinkedHashSet<String>();
            functions.stream().map(JavaGenerator::dataMap)
                    .map(function -> string(function.get("name")))
                    .forEach(functionNames::add);
            for (var importValue : list(module.get("imports"))) {
                var declaration = dataMap(importValue);
                if (Boolean.TRUE.equals(declaration.get("qualified"))) {
                    continue;
                }
                var importedModule = modulesByPath.get(normalizeModuleImportPath(
                        string(declaration.get("modulePath"))
                ));
                if (importedModule == null) {
                    continue;
                }
                for (var functionValue : list(importedModule.get("functions"))) {
                    var function = dataMap(functionValue);
                    var functionName = string(function.get("name"));
                    if (!functionName.startsWith(PRIMITIVE_SCHEMA_PREFIX)) {
                        continue;
                    }
                    var typeName = functionName.substring(PRIMITIVE_SCHEMA_PREFIX.length());
                    if (importIncludesName(declaration, typeName) && functionNames.add(functionName)) {
                        functions.add(functionValue);
                    }
                }
            }
            if (functions.size() == list(module.get("functions")).size()) {
                return value;
            }
            var linked = new LinkedHashMap<>(module);
            linked.put("functions", List.copyOf(functions));
            return (Object) Collections.unmodifiableMap(linked);
        }).toList();
    }

    private static boolean importIncludesName(Map<String, Object> declaration, String name) {
        var excluded = list(declaration.get("excludedNames")).stream().map(JavaGenerator::string).toList();
        if (Boolean.TRUE.equals(declaration.get("wildcard"))) {
            return !excluded.contains(name);
        }
        return list(declaration.get("importedNames")).stream()
                .map(JavaGenerator::string)
                .anyMatch(name::equals);
    }

    private static void enqueueImports(Map<String, Object> module, ArrayDeque<String> pendingImports) {
        list(module.get("imports")).stream()
                .map(JavaGenerator::dataMap)
                .map(declaration -> string(declaration.get("modulePath")))
                .filter(path -> !path.isBlank())
                .forEach(pendingImports::addLast);
    }

    private static Optional<Map<String, Object>> readBundledModule(String modulePath) {
        return BUNDLED_IMPORT_MODULES.computeIfAbsent(modulePath, JavaGenerator::loadBundledModule);
    }

    private static Optional<Map<String, Object>> loadBundledModule(String modulePath) {
        var resource = "/" + modulePath + ".json";
        try (var input = JavaGenerator.class.getResourceAsStream(resource)) {
            if (input == null) {
                return Optional.empty();
            }
            var json = new String(input.readAllBytes(), StandardCharsets.UTF_8);
            var module = LinkedJsonCodec.read(json, CompiledModule.class);
            return Optional.of(dataMap(toGeneratedValue(module)));
        } catch (IOException exception) {
            throw new IllegalStateException("Unable to read bundled Capybara module `" + modulePath + "`", exception);
        }
    }

    private static String moduleImportPath(Map<String, Object> module) {
        var path = normalizeModuleImportPath(string(module.get("path")));
        var name = string(module.get("name"));
        return path.isBlank() ? name : path + "/" + name;
    }

    private static String generatedJavaModulePath(Map<String, Object> module) {
        var path = normalizeModuleImportPath(string(module.get("path")));
        var name = string(module.get("name")).replace('.', '_');
        return path.isBlank() ? name + ".java" : path + "/" + name + ".java";
    }

    private static String normalizeModuleImportPath(String path) {
        var normalized = path.replace('\\', '/');
        while (normalized.startsWith("/")) {
            normalized = normalized.substring(1);
        }
        while (normalized.endsWith("/")) {
            normalized = normalized.substring(0, normalized.length() - 1);
        }
        return normalized;
    }


    static GeneratedModule generatedModule(Object value) {
        var module = dataMap(value);
        return new GeneratedModule(
                string(module.get("relativePath")),
                string(module.get("code"))
        );
    }

    static List<GeneratedModule> generatedModules(Object value) {
        return list(dataMap(value).get("modules")).stream()
                .map(JavaGenerator::generatedModule)
                .toList();
    }

    static Object toGeneratedValue(Object value) {
        if (value == null
                || value instanceof String
                || value instanceof Number
                || value instanceof Boolean
                || value instanceof Character) {
            return value;
        }
        if (value instanceof Optional<?> optional) {
            return optional.map(JavaGenerator::toGeneratedValue);
        }
        if (value instanceof List<?> values) {
            return values.stream().map(JavaGenerator::toGeneratedValue).toList();
        }
        if (value instanceof Set<?> values) {
            var converted = new LinkedHashSet<>();
            values.forEach(item -> converted.add(toGeneratedValue(item)));
            return Collections.unmodifiableSet(converted);
        }
        if (value instanceof Map<?, ?> values) {
            var converted = new LinkedHashMap<Object, Object>();
            values.forEach((key, item) -> converted.put(toGeneratedValue(key), toGeneratedValue(item)));
            return Collections.unmodifiableMap(converted);
        }
        if (value.getClass().isRecord()) {
            return recordData(value);
        }
        throw new IllegalArgumentException("Unsupported compiler value: " + value.getClass().getName());
    }

    private static Map<String, Object> recordData(Object record) {
        var result = new LinkedHashMap<String, Object>();
        result.put("__type", record.getClass().getSimpleName());
        for (RecordComponent component : record.getClass().getRecordComponents()) {
            try {
                result.put(component.getName(), toGeneratedValue(component.getAccessor().invoke(record)));
            } catch (ReflectiveOperationException exception) {
                throw new IllegalStateException("Unable to adapt " + record.getClass().getName(), exception);
            }
        }
        addBootstrapCompatibilityFields(result);
        return Collections.unmodifiableMap(result);
    }

    private static void addBootstrapCompatibilityFields(Map<String, Object> data) {
        if ("CompiledModule".equals(data.get("__type")) && list(data.get("functionBindings")).isEmpty()) {
            var moduleName = string(data.get("name"));
            var modulePath = string(data.get("path"));
            var bindings = list(data.get("functions")).stream()
                    .map(function -> {
                        var binding = new LinkedHashMap<String, Object>();
                        binding.put("__type", "CompiledFunctionBinding");
                        binding.put("moduleName", moduleName);
                        binding.put("modulePath", modulePath);
                        binding.put("function", function);
                        return Collections.unmodifiableMap(binding);
                    })
                    .toList();
            data.put("functionBindings", bindings);
        }
        if ("CompiledPrimitiveBackedType".equals(data.get("__type"))
                && !data.containsKey("proxiedFunctions")) {
            data.put("proxiedFunctions", List.of());
        }
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> dataMap(Object value) {
        if (value instanceof Map<?, ?> map) {
            return (Map<String, Object>) map;
        }
        throw new IllegalArgumentException("Expected generated data value, got: " + value);
    }

    @SuppressWarnings("unchecked")
    static List<Object> list(Object value) {
        if (value instanceof List<?> list) {
            return (List<Object>) list;
        }
        return new ArrayList<>();
    }

    private static String string(Object value) {
        return value == null ? "" : value.toString();
    }

    private record CachedGeneration(
            CompiledProgram source,
            CompiledProgram lookup,
            GeneratedProgram generated
    ) {
    }

    record GenerationPrograms(CompiledProgram source, CompiledProgram lookup) {
    }

    private record ScopedProgram(Object invocation, CompiledProgram program) {
    }
}
