package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import dev.capylang.compiler.CompiledFunction.CompiledFunctionParameter;
import dev.capylang.compiler.expression.CapybaraExpressionCompiler;
import dev.capylang.compiler.expression.*;
import dev.capylang.compiler.parser.*;
import dev.capylang.compiler.parser.Module;

import java.io.IOException;
import java.net.URI;
import java.nio.file.*;
import java.time.Duration;
import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Stream;

import static java.util.Collections.unmodifiableSortedMap;
import static java.util.Collections.unmodifiableSortedSet;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class CapybaraCompiler {
    public static final CapybaraCompiler INSTANCE = new CapybaraCompiler();
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String DATA_CONSTRUCTOR_FUNCTION_PREFIX = "__constructor__data__";
    private static final String TYPE_CONSTRUCTOR_FUNCTION_PREFIX = "__constructor__type__";
    private static final String CONSTRUCTOR_STATE_TYPE_PREFIX = "__constructor_state__";
    private static final java.util.regex.Pattern IDENTIFIER_PATTERN = java.util.regex.Pattern.compile("[A-Za-z_][A-Za-z0-9_]*");
    private static final ObjectMapper OBJECT_MAPPER = objectMapper();
    private static final Logger log = Logger.getLogger(CapybaraCompiler.class.getName());
    private static final Object BUNDLED_LIBRARIES_LOCK = new Object();
    private static volatile SortedSet<CompiledModule> bundledLibrariesCache;

    public Result<CompiledProgram> compile(Collection<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        try {
            var objectOrientedModules = rawModules.stream()
                    .filter(rawModule -> rawModule.sourceKind() == SourceKind.OBJECT_ORIENTED)
                    .toList();
            var parsedObjectOrientedModules = List.<ObjectOrientedModule>of();
            if (!objectOrientedModules.isEmpty()) {
                var ooParseResult = ObjectOrientedParser.INSTANCE.parseModules(objectOrientedModules);
                if (ooParseResult instanceof Result.Error<List<ObjectOrientedModule>> error) {
                    return new Result.Error<>(error.errors());
                }
                parsedObjectOrientedModules = ((Result.Success<List<ObjectOrientedModule>>) ooParseResult).value();
                var ooValidationResult = ObjectOrientedValidator.INSTANCE.validate(parsedObjectOrientedModules);
                if (ooValidationResult instanceof Result.Error<List<ObjectOrientedModule>> error) {
                    return new Result.Error<>(error.errors());
                }
                parsedObjectOrientedModules = ((Result.Success<List<ObjectOrientedModule>>) ooValidationResult).value();
            }

            var functionalModules = rawModules.stream()
                    .filter(rawModule -> rawModule.sourceKind() == SourceKind.FUNCTIONAL)
                    .toList();
            if (functionalModules.isEmpty()) {
                return Result.success(new CompiledProgram(new TreeSet<>(), parsedObjectOrientedModules));
            }
            var program = CapybaraParser.INSTANCE.parseModule(functionalModules);
            if (program instanceof Result.Error<Program> error) {
                return new Result.Error<>(error.errors());
            }
            var mergedLibraries = mergeLibraries(functionalModules, libraries);
            var compiledProgram = compile(((Result.Success<Program>) program).value(), mergedLibraries);
            if (compiledProgram instanceof Result.Error<CompiledProgram> error) {
                return error;
            }
            var functionalProgram = ((Result.Success<CompiledProgram>) compiledProgram).value();
            return Result.success(new CompiledProgram(functionalProgram.modules(), parsedObjectOrientedModules));
        } catch (RuntimeException e) {
            // Public boundary: source/compiler errors should not escape as exceptions.
            return new Result.Error<>(new Result.Error.SingleError(boundaryErrorMessage(e)));
        }
    }

    private static String boundaryErrorMessage(RuntimeException exception) {
        return Objects.toString(exception.getMessage(), exception.getClass().getSimpleName());
    }

    private SortedSet<CompiledModule> mergeLibraries(Collection<RawModule> rawModules, SortedSet<CompiledModule> libraries) {
        var merged = new TreeSet<>(loadBundledLibraries(rawModules));
        libraries.forEach(library -> {
            // Replace bundled stdlib modules using canonical module identity so
            // `/capy/foo/Bar` and `capy/foo/Bar` are treated as the same module.
            merged.removeIf(existing -> canonicalModuleIdentity(existing.path(), existing.name())
                    .equals(canonicalModuleIdentity(library.path(), library.name())));
            merged.add(library);
        });
        return merged;
    }

    private String canonicalModuleIdentity(String path, String name) {
        var normalizedPath = normalizeModulePath(path);
        if (normalizedPath.isBlank()) {
            return name;
        }
        return normalizedPath + "/" + name;
    }

    private static SortedSet<CompiledModule> loadBundledLibraries(Collection<RawModule> rawModules) {
        var cached = bundledLibrariesCache;
        if (cached != null) {
            return new TreeSet<>(cached);
        }
        synchronized (BUNDLED_LIBRARIES_LOCK) {
            cached = bundledLibrariesCache;
            if (cached != null) {
                return new TreeSet<>(cached);
            }
            var loaded = loadBundledLibrariesUncached(rawModules);
            if (!loaded.isEmpty()) {
                bundledLibrariesCache = new TreeSet<>(loaded);
            }
            return loaded;
        }
    }

    private static SortedSet<CompiledModule> loadBundledLibrariesUncached(Collection<RawModule> rawModules) {
        try {
            var resource = CapybaraCompiler.class.getClassLoader().getResource("capy");
            if (resource != null) {
                try (var paths = Files.walk(resourcePath(resource.toURI()))) {
                    return readBundledLibraries(paths);
                }
            }
            var fallbackPath = bundledLibrariesFallbackPath();
            if (fallbackPath != null) {
                try (var paths = Files.walk(fallbackPath)) {
                    return readBundledLibraries(paths);
                }
            }
            if (isStdlibBootstrap(rawModules)) {
                return new TreeSet<>();
            }
            throw new IllegalStateException("Unable to find bundled Capybara libraries resource `capy`");
        } catch (IllegalStateException e) {
            throw e;
        } catch (Exception e) {
            throw new IllegalStateException("Unable to load bundled Capybara libraries", e);
        }
    }

    private static SortedSet<CompiledModule> readBundledLibraries(Stream<Path> paths) {
        return paths
                .filter(path -> path.getFileName().toString().endsWith(CompiledModule.EXTENSION))
                .map(CapybaraCompiler::readBundledLibrary)
                .collect(java.util.stream.Collectors.toCollection(TreeSet::new));
    }

    private static Path bundledLibrariesFallbackPath() {
        var candidate = Paths.get("lib", "capybara-lib", "build", "generated", "sources", "capybara", "linked", "main", "capy");
        if (Files.isDirectory(candidate)) {
            return candidate;
        }
        return null;
    }

    private static boolean isStdlibBootstrap(Collection<RawModule> rawModules) {
        return rawModules.stream().anyMatch(rawModule -> rawModule.path().replace('\\', '/').startsWith("capy"));
    }

    private static Path resourcePath(URI resource) throws IOException {
        if ("jar".equals(resource.getScheme())) {
            try {
                return Path.of(resource);
            } catch (FileSystemNotFoundException ignored) {
                try {
                    java.nio.file.FileSystems.newFileSystem(resource, Map.of());
                } catch (FileSystemAlreadyExistsException alreadyExists) {
                    java.nio.file.FileSystems.getFileSystem(resource);
                }
                return Path.of(resource);
            }
        }
        return Path.of(resource);
    }

    private static CompiledModule readBundledLibrary(Path path) {
        try (var input = Files.newInputStream(path)) {
            return OBJECT_MAPPER.readValue(input, CompiledModule.class);
        } catch (IOException e) {
            throw new IllegalStateException("Unable to read bundled Capybara library `" + path + "`", e);
        }
    }

    private static ObjectMapper objectMapper() {
        var mapper = new ObjectMapper();
        mapper.registerModule(new Jdk8Module());
        var validator = BasicPolymorphicTypeValidator.builder()
                .allowIfSubType("dev.capylang")
                .allowIfSubType("java.util")
                .build();
        mapper.activateDefaultTyping(
                validator,
                ObjectMapper.DefaultTyping.NON_FINAL,
                JsonTypeInfo.As.PROPERTY
        );
        return mapper;
    }
    private Result<CompiledProgram> compile(Program program, SortedSet<CompiledModule> libraries) {
        var totalStartedAt = System.nanoTime();
        var compileCache = new CompileCache();
        var programModuleRefs = program.modules().stream().map(module -> new ModuleRef(module.name(), module.path())).toList();
        var libraryModuleRefs = libraries.stream().map(module -> new ModuleRef(module.name(), module.path())).toList();
        var allModuleRefs = Stream.concat(programModuleRefs.stream(), libraryModuleRefs.stream()).toList();

        var linkedTypesByModule = new HashMap<String, SortedMap<String, GenericDataType>>();
        for (var library : libraries) {
            putImportedModuleEntry(linkedTypesByModule, new ModuleRef(library.name(), library.path()), library.types());
        }
        for (var module : program.modules()) {
            var sourceFile = moduleSourceFile(module);
            var linkedTypes = withFile(types(module), sourceFile);
            if (linkedTypes instanceof Result.Error<SortedMap<String, GenericDataType>> error) {
                return new Result.Error<>(error.errors());
            }
            putOwnedModuleEntry(
                    linkedTypesByModule,
                    new ModuleRef(module.name(), module.path()),
                    ((Result.Success<SortedMap<String, GenericDataType>>) linkedTypes).value()
            );
        }

        var moduleLinkIndex = buildModuleLinkIndex(allModuleRefs, linkedTypesByModule);
        var moduleClassNameByModuleName = moduleLinkIndex.moduleJavaClassNameByModuleName();
        var staticImportsByModule = new HashMap<String, SortedSet<CompiledModule.StaticImport>>();
        for (var library : libraries) {
            putImportedModuleEntry(
                    staticImportsByModule,
                    new ModuleRef(library.name(), library.path()),
                    library.staticImports()
            );
        }
        var deriversByModule = new HashMap<String, Map<String, DeriverDeclaration>>();
        for (var library : libraries) {
            putImportedModuleEntry(
                    deriversByModule,
                    new ModuleRef(library.name(), library.path()),
                    library.derivers()
            );
        }
        for (var module : program.modules()) {
            var sourceFile = moduleSourceFile(module);
            var derivers = withFile(derivers(module.functional().definitions(), sourceFile), sourceFile);
            if (derivers instanceof Result.Error<Map<String, DeriverDeclaration>> error) {
                return new Result.Error<>(error.errors());
            }
            putOwnedModuleEntry(
                    deriversByModule,
                    new ModuleRef(module.name(), module.path()),
                    ((Result.Success<Map<String, DeriverDeclaration>>) derivers).value()
            );
        }
        var constructorCatalog = constructorCatalog(program.modules(), libraries);
        var visibleConstructorsByModule = new HashMap<String, CapybaraExpressionCompiler.ConstructorRegistry>();
        for (var module : program.modules()) {
            putOwnedModuleEntry(
                    visibleConstructorsByModule,
                    new ModuleRef(module.name(), module.path()),
                    availableConstructors(module, moduleLinkIndex, constructorCatalog, allModuleRefs)
            );
        }

        var availableTypesStartedAt = System.nanoTime();
        var visibleTypesByModule = new HashMap<String, Map<String, GenericDataType>>();
        for (var module : program.modules()) {
            var sourceFile = moduleSourceFile(module);
            var visibleTypes = withFile(availableTypes(module, moduleLinkIndex, linkedTypesByModule, allModuleRefs, compileCache), sourceFile);
            if (visibleTypes instanceof Result.Error<Map<String, GenericDataType>> error) {
                return new Result.Error<>(error.errors());
            }
            putOwnedModuleEntry(
                    visibleTypesByModule,
                    new ModuleRef(module.name(), module.path()),
                    ((Result.Success<Map<String, GenericDataType>>) visibleTypes).value()
            );
        }
        log.info("Prepared available types for " + program.modules().size() + " modules in " + Duration.ofNanos(System.nanoTime() - availableTypesStartedAt));

        var availableSignaturesStartedAt = System.nanoTime();
        var signaturesByModule = new HashMap<String, List<CapybaraExpressionCompiler.FunctionSignature>>();
        for (var library : libraries) {
            putImportedModuleEntry(
                    signaturesByModule,
                    new ModuleRef(library.name(), library.path()),
                    signaturesFromLinkedFunctions(List.copyOf(library.functions()))
            );
        }
        for (var module : program.modules()) {
            var moduleRef = new ModuleRef(module.name(), module.path());
            var localTypes = getModuleEntry(linkedTypesByModule, moduleRef);
            var visibleTypes = getModuleEntry(visibleTypesByModule, moduleRef);
            var functions = functions(
                    module,
                    localTypes,
                    visibleTypes,
                    deriversByModule,
                    moduleLinkIndex
            );
            if (functions instanceof Result.Error<List<Function>> error) {
                return new Result.Error<>(error.errors());
            }
            var sourceFile = moduleSourceFile(module);
            var signatures = withFile(linkFunctionSignatures(((Result.Success<List<Function>>) functions).value(), visibleTypes, compileCache, sourceFile), sourceFile);
            if (signatures instanceof Result.Error<List<CapybaraExpressionCompiler.FunctionSignature>> error) {
                return new Result.Error<>(error.errors());
            }
            putOwnedModuleEntry(
                    signaturesByModule,
                    moduleRef,
                    ((Result.Success<List<CapybaraExpressionCompiler.FunctionSignature>>) signatures).value()
            );
        }

        for (var module : program.modules()) {
            var moduleRef = new ModuleRef(module.name(), module.path());
            var localTypes = getModuleEntry(linkedTypesByModule, moduleRef);
            var visibleTypes = getModuleEntry(visibleTypesByModule, moduleRef);
            var constructors = constructorFunctions(module.functional().definitions(), localTypes);
            if (constructors.isEmpty()) {
                continue;
            }
            var sourceFile = moduleSourceFile(module);
            var availableConstructorSignatures = availableSignatures(module, moduleLinkIndex, linkedTypesByModule, signaturesByModule, deriversByModule, staticImportsByModule, compileCache);
            if (availableConstructorSignatures instanceof Result.Error<List<CapybaraExpressionCompiler.FunctionSignature>> error) {
                return new Result.Error<>(error.errors());
            }
            var linkedConstructors = withFile(linkFunctions(
                    constructors,
                    visibleTypes,
                    localTypes.keySet(),
                    ((Result.Success<List<CapybaraExpressionCompiler.FunctionSignature>>) availableConstructorSignatures).value(),
                    signaturesByModule,
                    moduleClassNameByModuleName,
                    getModuleEntry(visibleConstructorsByModule, moduleRef),
                    sourceFile,
                    compileCache
            ), sourceFile);
            if (linkedConstructors instanceof Result.Error<List<CompiledFunction>> error) {
                return new Result.Error<>(error.errors());
            }
            var constructorValidation = validateResultReturningTypeConstructors(
                    module,
                    ((Result.Success<List<CompiledFunction>>) linkedConstructors).value(),
                    visibleTypes,
                    sourceFile
            );
            if (constructorValidation instanceof Result.Error<Void> error) {
                return new Result.Error<>(error.errors());
            }
            putOwnedModuleEntry(
                    signaturesByModule,
                    moduleRef,
                    mergeSignatures(
                            getModuleEntry(signaturesByModule, moduleRef),
                            signaturesFromLinkedFunctions(((Result.Success<List<CompiledFunction>>) linkedConstructors).value())
                    )
            );
        }
        log.info("Prepared available signatures for " + program.modules().size() + " modules in " + Duration.ofNanos(System.nanoTime() - availableSignaturesStartedAt));

        var refinedSignaturesByModule = new HashMap<>(signaturesByModule);
        var firstPassStartedAt = System.nanoTime();
        for (var module : program.modules()) {
            var moduleRef = new ModuleRef(module.name(), module.path());
            var firstPassFunctions = firstPassLinkedFunctions(
                    module,
                    moduleLinkIndex,
                    linkedTypesByModule,
                    visibleTypesByModule,
                    signaturesByModule,
                    deriversByModule,
                    staticImportsByModule,
                    moduleClassNameByModuleName,
                    getModuleEntry(visibleConstructorsByModule, moduleRef),
                    compileCache
            );
            firstPassFunctions = withFile(firstPassFunctions, moduleSourceFile(module));
            if (firstPassFunctions instanceof Result.Error<List<CompiledFunction>> error) {
                return new Result.Error<>(error.errors());
            }
            var refined = mergeSignatures(
                    getModuleEntry(signaturesByModule, moduleRef),
                    signaturesFromLinkedFunctions(((Result.Success<List<CompiledFunction>>) firstPassFunctions).value())
            );
            putOwnedModuleEntry(refinedSignaturesByModule, moduleRef, refined);
        }
        log.info("Completed first-pass linking for " + program.modules().size() + " modules in " + Duration.ofNanos(System.nanoTime() - firstPassStartedAt));

        var finalLinkStartedAt = System.nanoTime();
        var result = program.modules().stream()
                .map(module -> {
                    var moduleRef = new ModuleRef(module.name(), module.path());
                    return linkModule(
                            module,
                            moduleLinkIndex,
                            linkedTypesByModule,
                            visibleTypesByModule,
                            refinedSignaturesByModule,
                            deriversByModule,
                            staticImportsByModule,
                            moduleClassNameByModuleName,
                            getModuleEntry(visibleConstructorsByModule, moduleRef),
                            compileCache
                    );
                })
                .collect(new ResultCollectionCollector<>())
                .map(CompiledProgram::new);
        if (result instanceof Result.Success<CompiledProgram> success) {
            var postValidation = validateResultReturningTypeConstructors(program, success.value());
            if (postValidation instanceof Result.Error<Void> error) {
                return new Result.Error<>(error.errors());
            }
            var getterCompatibilityValidation = validateMethodGetterCompatibility(program, success.value());
            if (getterCompatibilityValidation instanceof Result.Error<Void> error) {
                return new Result.Error<>(error.errors());
            }
        }
        log.info("Completed final linking for " + program.modules().size() + " modules in " + Duration.ofNanos(System.nanoTime() - finalLinkStartedAt));
        log.info("Generated static imports in " + Duration.ofNanos(compileCache.staticImportGenerationNanos));
        log.info("Compiled program with " + allModuleRefs.size() + " total modules in " + Duration.ofNanos(System.nanoTime() - totalStartedAt));
        return result;
    }

    private record ModuleRef(String name, String path) {
    }

    private record FieldOrigin(String ownerName, CompiledType type) {
    }

    private record LinkedDataFields(
            List<List<CompiledDataType.CompiledField>> inherited,
            List<CompiledDataType.CompiledField> own
    ) {
    }

    private enum ConstructorKind {
        DATA,
        TYPE
    }

    private record ConstructorDescriptor(ConstructorKind kind, String targetTypeName) {
    }

    private record TypeConstructorPlan(
            TypeDeclaration declaration,
            CompiledDataParentType linkedType,
            CompiledDataType stateType
    ) {
    }

    private record ConstructorCatalog(
            Map<String, Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef>> constructorsByModule,
            Map<String, Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>>> parentConstructorsByModule,
            Map<String, Map<String, String>> dataOwnersByModule
    ) {
    }

    private ModuleLinkIndex buildModuleLinkIndex(
            List<ModuleRef> allModuleRefs,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModule
    ) {
        var modulesByExactName = new LinkedHashMap<String, ModuleRef>();
        var modulesByQualifiedName = new LinkedHashMap<String, ModuleRef>();
        var modulesByTailName = new LinkedHashMap<String, ModuleRef>();
        var ambiguousTailNames = new LinkedHashSet<String>();
        var linkedTypesByModuleName = new LinkedHashMap<String, SortedMap<String, GenericDataType>>();
        var moduleJavaClassNameByModuleName = new LinkedHashMap<String, String>();

        for (var module : allModuleRefs) {
            modulesByExactName.putIfAbsent(module.name(), module);
            modulesByQualifiedName.putIfAbsent(qualifiedModuleName(module), module);

            var linkedTypes = getModuleEntry(linkedTypesByModule, module);
            putImportedModuleEntry(linkedTypesByModuleName, module, linkedTypes);
            putImportedModuleEntry(moduleJavaClassNameByModuleName, module, moduleJavaClassName(module, linkedTypes));

            var tailName = moduleTailName(module);
            if (tailName == null || ambiguousTailNames.contains(tailName)) {
                continue;
            }
            var existing = modulesByTailName.putIfAbsent(tailName, module);
            if (existing != null && !existing.equals(module)) {
                modulesByTailName.remove(tailName);
                ambiguousTailNames.add(tailName);
            }
        }

        return new ModuleLinkIndex(
                Map.copyOf(modulesByExactName),
                Map.copyOf(modulesByQualifiedName),
                Map.copyOf(modulesByTailName),
                Set.copyOf(ambiguousTailNames),
                Map.copyOf(linkedTypesByModuleName),
                Map.copyOf(moduleJavaClassNameByModuleName)
        );
    }

    private String qualifiedModuleName(ModuleRef module) {
        var normalizedPath = normalizeModulePath(module.path());
        if (".".equals(normalizedPath) || normalizedPath.isBlank()) {
            return module.name();
        }
        return normalizedPath + "/" + module.name();
    }

    private String qualifiedTypeName(ModuleRef module, String typeName) {
        return qualifiedModuleName(module) + "." + typeName;
    }

    private String moduleTailName(ModuleRef module) {
        var qualifiedName = qualifiedModuleName(module);
        var lastSlash = qualifiedName.lastIndexOf('/');
        if (lastSlash < 0 || lastSlash == qualifiedName.length() - 1) {
            return null;
        }
        return qualifiedName.substring(lastSlash + 1);
    }

    private <T> void putImportedModuleEntry(Map<String, T> target, ModuleRef moduleRef, T value) {
        target.putIfAbsent(moduleRef.name(), value);
        target.put(qualifiedModuleName(moduleRef), value);
    }

    private <T> void putOwnedModuleEntry(Map<String, T> target, ModuleRef moduleRef, T value) {
        target.put(moduleRef.name(), value);
        target.put(qualifiedModuleName(moduleRef), value);
    }

    private <T> T getModuleEntry(Map<String, T> source, ModuleRef moduleRef) {
        var qualified = source.get(qualifiedModuleName(moduleRef));
        return qualified != null ? qualified : source.get(moduleRef.name());
    }

    private void addQualifiedConstructorAliases(
            Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> all,
            ModuleRef module,
            Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> constructors
    ) {
        constructors.forEach((typeName, constructor) -> {
            all.put(module.name() + "." + typeName, constructor);
            var modulePath = module.path().replace('\\', '/') + "/" + module.name();
            all.put(modulePath + "." + typeName, constructor);
            all.put("/" + modulePath + "." + typeName, constructor);
            if (module.name().equals(typeName)) {
                all.put(modulePath, constructor);
                all.put("/" + modulePath, constructor);
            }
        });
    }

    private void addQualifiedParentConstructorAliases(
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> all,
            ModuleRef module,
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> parentConstructors
    ) {
        parentConstructors.forEach((typeName, constructors) -> {
            all.put(module.name() + "." + typeName, constructors);
            var modulePath = module.path().replace('\\', '/') + "/" + module.name();
            all.put(modulePath + "." + typeName, constructors);
            all.put("/" + modulePath + "." + typeName, constructors);
            if (module.name().equals(typeName)) {
                all.put(modulePath, constructors);
                all.put("/" + modulePath, constructors);
            }
        });
    }

    private void addQualifiedDataOwnerAliases(
            Map<String, String> all,
            ModuleRef module,
            Map<String, String> dataOwners
    ) {
        dataOwners.forEach((typeName, ownerModule) -> {
            all.put(module.name() + "." + typeName, ownerModule);
            var modulePath = module.path().replace('\\', '/') + "/" + module.name();
            all.put(modulePath + "." + typeName, ownerModule);
            all.put("/" + modulePath + "." + typeName, ownerModule);
            if (module.name().equals(typeName)) {
                all.put(modulePath, ownerModule);
                all.put("/" + modulePath, ownerModule);
            }
        });
    }

    private record ModuleLinkIndex(
            Map<String, ModuleRef> modulesByExactName,
            Map<String, ModuleRef> modulesByQualifiedName,
            Map<String, ModuleRef> modulesByTailName,
            Set<String> ambiguousTailNames,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModuleName,
            Map<String, String> moduleJavaClassNameByModuleName
    ) {
    }

    private Result<List<CompiledFunction>> firstPassLinkedFunctions(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModule,
            Map<String, Map<String, GenericDataType>> visibleTypesByModule,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            Map<String, SortedSet<CompiledModule.StaticImport>> staticImportsByModule,
            Map<String, String> moduleClassNameByModuleName,
            CapybaraExpressionCompiler.ConstructorRegistry protectedConstructorsByType,
            CompileCache compileCache
    ) {
        var moduleRef = new ModuleRef(module.name(), module.path());
        var dataTypes = getModuleEntry(visibleTypesByModule, moduleRef);
        var localTypes = getModuleEntry(linkedTypesByModule, moduleRef);
        var localTypeNames = localTypes.keySet();
        var functions = functions(module, localTypes, dataTypes, deriversByModule, moduleLinkIndex);
        if (functions instanceof Result.Error<List<Function>> error) {
            return new Result.Error<>(error.errors());
        }
        var moduleSourceFile = moduleSourceFile(module);
        var availableSignatures = availableSignatures(module, moduleLinkIndex, linkedTypesByModule, signaturesByModule, deriversByModule, staticImportsByModule, compileCache);
        if (availableSignatures instanceof Result.Error<List<CapybaraExpressionCompiler.FunctionSignature>> error) {
            return withFile(new Result.Error<>(error.errors()), moduleSourceFile);
        }
        var initialSignatures = ((Result.Success<List<CapybaraExpressionCompiler.FunctionSignature>>) availableSignatures).value();
        return withFile(linkFunctions(((Result.Success<List<Function>>) functions).value(), dataTypes, localTypeNames, initialSignatures, signaturesByModule, moduleClassNameByModuleName, protectedConstructorsByType, moduleSourceFile, compileCache), moduleSourceFile);
    }

    private Result<CompiledModule> linkModule(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModule,
            Map<String, Map<String, GenericDataType>> visibleTypesByModule,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            Map<String, SortedSet<CompiledModule.StaticImport>> staticImportsByModule,
            Map<String, String> moduleClassNameByModuleName,
            CapybaraExpressionCompiler.ConstructorRegistry protectedConstructorsByType,
            CompileCache compileCache
    ) {
        var moduleRef = new ModuleRef(module.name(), module.path());
        var localTypes = getModuleEntry(linkedTypesByModule, moduleRef);
        var visibleTypes = getModuleEntry(visibleTypesByModule, moduleRef);
        var functions = functions(module, localTypes, visibleTypes, deriversByModule, moduleLinkIndex);
        if (functions instanceof Result.Error<List<Function>> error) {
            return new Result.Error<>(error.errors());
        }
        var moduleSourceFile = moduleSourceFile(module);
        var availableSignatures = availableSignatures(module, moduleLinkIndex, linkedTypesByModule, signaturesByModule, deriversByModule, staticImportsByModule, compileCache);
        if (availableSignatures instanceof Result.Error<List<CapybaraExpressionCompiler.FunctionSignature>> error) {
            return withFile(new Result.Error<>(error.errors()), moduleSourceFile);
        }
        var initialSignatures = ((Result.Success<List<CapybaraExpressionCompiler.FunctionSignature>>) availableSignatures).value();
        return withFile(linkFunctions(((Result.Success<List<Function>>) functions).value(), visibleTypes, localTypes.keySet(), initialSignatures, signaturesByModule, moduleClassNameByModuleName, protectedConstructorsByType, moduleSourceFile, compileCache)
                .flatMap(firstPassFunctions -> {
                    var moduleSignatures = getModuleEntry(signaturesByModule, moduleRef);
                    var refinedSignatures = mergeSignatures(
                            moduleSignatures,
                            signaturesFromLinkedFunctions(firstPassFunctions)
                    );
                    if (refinedSignatures.equals(moduleSignatures)) {
                        return Result.success(new CompiledModule(
                                module.name(),
                                module.path(),
                                localTypes,
                                deduplicateFunctions(firstPassFunctions),
                                getModuleEntry(deriversByModule, new ModuleRef(module.name(), module.path())),
                                staticImports(module, moduleLinkIndex, linkedTypesByModule, signaturesByModule, deriversByModule, staticImportsByModule, compileCache)
                        ));
                    }
                    var refinedAvailableSignatures = mergeSignatures(initialSignatures, refinedSignatures);
                    return linkFunctions(((Result.Success<List<Function>>) functions).value(), visibleTypes, localTypes.keySet(), refinedAvailableSignatures, signaturesByModule, moduleClassNameByModuleName, protectedConstructorsByType, moduleSourceFile, compileCache)
                            .map(linkedFunctions -> new CompiledModule(
                                    module.name(),
                                    module.path(),
                                    localTypes,
                                    deduplicateFunctions(linkedFunctions),
                                    getModuleEntry(deriversByModule, new ModuleRef(module.name(), module.path())),
                                    staticImports(module, moduleLinkIndex, linkedTypesByModule, signaturesByModule, deriversByModule, staticImportsByModule, compileCache)
                            ));
                }), moduleSourceFile);
    }
    private Result<List<CapybaraExpressionCompiler.FunctionSignature>> availableSignatures(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModule,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            Map<String, SortedSet<CompiledModule.StaticImport>> staticImportsByModule,
            CompileCache compileCache
    ) {
        var cacheKey = moduleCacheKey(module);
        var cachedByModule = compileCache.availableSignaturesByModulePhase
                .computeIfAbsent(signaturesByModule, ignored -> new HashMap<>());
        var cached = cachedByModule.get(cacheKey);
        if (cached != null) {
            return cached;
        }
        var all = new ArrayList<CapybaraExpressionCompiler.FunctionSignature>(
                getModuleEntry(signaturesByModule, new ModuleRef(module.name(), module.path()))
        );
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), moduleLinkIndex);
            if (importedModule == null) {
                return Result.error("Module `" + module.name() + "` imports unknown module `" + importDeclaration.moduleName() + "`");
            }
            var importedSignaturesByName = visibleSignaturesByName(
                    module.path(),
                    importedModule,
                    getModuleEntry(signaturesByModule, importedModule),
                    compileCache
            );
            var availableFunctionMembers = importedSignaturesByName.keySet();
            var availableTypeMembers = visibleTypes(
                    module.path(),
                    importedModule,
                    getModuleEntry(linkedTypesByModule, importedModule),
                    compileCache
            ).keySet();
            var availableDeriverMembers = visibleDerivers(
                    module.path(),
                    importedModule,
                    getModuleEntry(deriversByModule, importedModule)
            ).keySet();
            var availableMembers = new HashSet<String>(availableFunctionMembers);
            availableMembers.addAll(availableTypeMembers);
            availableMembers.addAll(availableDeriverMembers);
            for (var excludedSymbol : importDeclaration.excludedSymbols()) {
                if (!availableMembers.contains(excludedSymbol)) {
                    return Result.error(
                            "Module `" + module.name() + "` excludes unknown symbol `" + excludedSymbol
                            + "` from module `" + importDeclaration.moduleName() + "`"
                    );
                }
            }
            if (!importDeclaration.isStarImport()) {
                for (var symbol : importDeclaration.symbols()) {
                    if (!availableMembers.contains(symbol)) {
                        return Result.error(
                                "Module `" + module.name() + "` imports unknown symbol `" + symbol
                                + "` from module `" + importDeclaration.moduleName() + "`"
                        );
                    }
                }
            }
            for (var symbol : importDeclaration.selectedSymbols(availableMembers)) {
                all.addAll(importedSignaturesByName.getOrDefault(symbol, List.of()));
            }
        }
        all.addAll(deriverLexicalSignatures(
                module,
                moduleLinkIndex,
                signaturesByModule,
                deriversByModule,
                staticImportsByModule,
                compileCache
        ));
        var result = Result.success(List.copyOf(all));
        cachedByModule.put(cacheKey, result);
        return result;
    }

    private List<CapybaraExpressionCompiler.FunctionSignature> deriverLexicalSignatures(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            Map<String, SortedSet<CompiledModule.StaticImport>> staticImportsByModule,
            CompileCache compileCache
    ) {
        var signatures = new LinkedHashSet<CapybaraExpressionCompiler.FunctionSignature>();
        for (var ownerModule : usedImportedDeriverOwnerModules(module, moduleLinkIndex, deriversByModule)) {
            signatures.addAll(visibleSignatures(
                    ownerModule.path(),
                    ownerModule,
                    getModuleEntry(signaturesByModule, ownerModule),
                    compileCache
            ));
            var ownerStaticImports = getModuleEntry(staticImportsByModule, ownerModule);
            if (ownerStaticImports == null) {
                continue;
            }
            for (var staticImport : ownerStaticImports) {
                var importedModule = resolveModuleByJavaClassName(staticImport.className(), moduleLinkIndex);
                if (importedModule == null) {
                    continue;
                }
                var importedSignaturesByName = visibleSignaturesByName(
                        ownerModule.path(),
                        importedModule,
                        getModuleEntry(signaturesByModule, importedModule),
                        compileCache
                );
                if ("*".equals(staticImport.memberName())) {
                    importedSignaturesByName.values().forEach(signatures::addAll);
                } else {
                    signatures.addAll(importedSignaturesByName.getOrDefault(staticImport.memberName(), List.of()));
                }
            }
        }
        return List.copyOf(signatures);
    }

    private Result<Map<String, GenericDataType>> availableTypes(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModule,
            List<ModuleRef> allModules,
            CompileCache compileCache
    ) {
        var currentModule = new ModuleRef(module.name(), module.path());
        var localTypes = getModuleEntry(linkedTypesByModule, currentModule);
        var all = new LinkedHashMap<String, GenericDataType>(localTypes);
        addQualifiedTypeAliases(all, currentModule, localTypes);
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), moduleLinkIndex);
            if (importedModule == null) {
                return Result.error("Module `" + module.name() + "` imports unknown module `" + importDeclaration.moduleName() + "`");
            }
            var importedTypes = visibleTypes(
                    module.path(),
                    importedModule,
                    getModuleEntry(linkedTypesByModule, importedModule),
                    compileCache
            );
            addQualifiedTypeAliases(all, importedModule, importedTypes);
            if (importDeclaration.isStarImport() && importDeclaration.excludedSymbols().isEmpty()) {
                importedTypes.forEach(all::put);
                continue;
            }

            var selected = importDeclaration.selectedSymbols(importedTypes.keySet());
            for (var symbol : selected) {
                var imported = importedTypes.get(symbol);
                if (imported != null) {
                    all.put(symbol, imported);
                }
            }
        }
        allModules.forEach(knownModule -> addQualifiedTypeAliases(
                all,
                knownModule,
                visibleTypes(module.path(), knownModule, getModuleEntry(linkedTypesByModule, knownModule), compileCache)
        ));
        resolveQualifiedExternalFieldTypes(all);
        return Result.success(Map.copyOf(all));
    }

    private void addQualifiedTypeAliases(Map<String, GenericDataType> all, ModuleRef module, Map<String, GenericDataType> importedTypes) {
        importedTypes.forEach((typeName, type) -> all.put(module.name() + "." + typeName, type));
        addPathQualifiedTypeAliases(all, module, importedTypes);
    }

    private void addPathQualifiedTypeAliases(Map<String, GenericDataType> all, ModuleRef module, Map<String, GenericDataType> importedTypes) {
        var modulePath = module.path().replace('\\', '/') + "/" + module.name();
        importedTypes.forEach((typeName, type) -> {
            all.put(modulePath + "." + typeName, type);
            all.put("/" + modulePath + "." + typeName, type);
            // Allow fully-qualified references without duplicate suffix when type name equals module name:
            // /capy/lang/Program instead of /capy/lang/Program.Program
            if (module.name().equals(typeName)) {
                all.put(modulePath, type);
                all.put("/" + modulePath, type);
            }
        });
    }

    private void resolveQualifiedExternalFieldTypes(Map<String, GenericDataType> all) {
        var resolved = new HashMap<String, GenericDataType>();
        for (var entry : all.entrySet()) {
            resolved.put(entry.getKey(), resolveGenericDataType(entry.getValue(), all));
        }
        all.putAll(resolved);
    }

    private GenericDataType resolveGenericDataType(GenericDataType type, Map<String, GenericDataType> all) {
        return resolveGenericDataType(type, all, Set.of());
    }

    private GenericDataType resolveGenericDataType(
            GenericDataType type,
            Map<String, GenericDataType> all,
            Set<String> resolvingTypeNames
    ) {
        var nextResolvingTypeNames = appendResolvingTypeName(resolvingTypeNames, type.name());
        return switch (type) {
            case CompiledDataType linkedDataType -> new CompiledDataType(
                    linkedDataType.name(),
                    linkedDataType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(field.name(), resolveLinkedType(field.type(), all, nextResolvingTypeNames)))
                            .toList(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.comments(),
                    linkedDataType.visibility(),
                    linkedDataType.singleton(),
                    linkedDataType.enumValue()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    linkedDataParentType.name(),
                    linkedDataParentType.fields().stream()
                            .map(field -> new CompiledDataType.CompiledField(field.name(), resolveLinkedType(field.type(), all, nextResolvingTypeNames)))
                            .toList(),
                    linkedDataParentType.subTypes().stream()
                            .map(subType -> (CompiledDataType) resolveGenericDataType(subType, all, nextResolvingTypeNames))
                            .toList(),
                    linkedDataParentType.typeParameters(),
                    linkedDataParentType.comments(),
                    linkedDataParentType.visibility(),
                    linkedDataParentType.enumType()
            );
        };
    }

    private CompiledType resolveLinkedType(CompiledType type, Map<String, GenericDataType> all) {
        return resolveLinkedType(type, all, Set.of());
    }

    private CompiledType resolveLinkedType(
            CompiledType type,
            Map<String, GenericDataType> all,
            Set<String> resolvingTypeNames
    ) {
        return switch (type) {
            case CompiledDataType linkedDataType -> {
                if (isUnresolvedDataPlaceholder(linkedDataType)) {
                    var resolved = resolveQualifiedExternalType(all, linkedDataType.name());
                    if (resolved != null) {
                        if (resolvingTypeNames.contains(linkedDataType.name())) {
                            yield withRequestedName(resolved, linkedDataType.name());
                        }
                        yield withRequestedName(
                                resolveGenericDataType(resolved, all, appendResolvingTypeName(resolvingTypeNames, linkedDataType.name())),
                                linkedDataType.name()
                        );
                    }
                }
                yield resolveGenericDataType(linkedDataType, all, appendResolvingTypeName(resolvingTypeNames, linkedDataType.name()));
            }
            case CompiledDataParentType linkedDataParentType -> {
                if (isUnresolvedTypePlaceholder(linkedDataParentType) || isQualifiedExternalPlaceholder(linkedDataParentType)) {
                    var resolved = resolveQualifiedExternalType(all, linkedDataParentType.name());
                    if (resolved != null) {
                        if (resolvingTypeNames.contains(linkedDataParentType.name())) {
                            yield withRequestedName(resolved, linkedDataParentType.name());
                        }
                        yield withRequestedName(
                                resolveGenericDataType(resolved, all, appendResolvingTypeName(resolvingTypeNames, linkedDataParentType.name())),
                                linkedDataParentType.name()
                        );
                    }
                }
                yield resolveGenericDataType(linkedDataParentType, all, appendResolvingTypeName(resolvingTypeNames, linkedDataParentType.name()));
            }
            case CollectionLinkedType.CompiledList linkedList ->
                    new CollectionLinkedType.CompiledList(resolveLinkedType(linkedList.elementType(), all, resolvingTypeNames));
            case CollectionLinkedType.CompiledSet linkedSet ->
                    new CollectionLinkedType.CompiledSet(resolveLinkedType(linkedSet.elementType(), all, resolvingTypeNames));
            case CollectionLinkedType.CompiledDict linkedDict ->
                    new CollectionLinkedType.CompiledDict(resolveLinkedType(linkedDict.valueType(), all, resolvingTypeNames));
            case CompiledTupleType linkedTupleType -> new CompiledTupleType(
                    linkedTupleType.elementTypes().stream().map(element -> resolveLinkedType(element, all, resolvingTypeNames)).toList()
            );
            case CompiledFunctionType linkedFunctionType -> new CompiledFunctionType(
                    resolveLinkedType(linkedFunctionType.argumentType(), all, resolvingTypeNames),
                    resolveLinkedType(linkedFunctionType.returnType(), all, resolvingTypeNames)
            );
            default -> type;
        };
    }

    private Set<String> appendResolvingTypeName(Set<String> resolvingTypeNames, String typeName) {
        var next = new LinkedHashSet<>(resolvingTypeNames);
        next.add(typeName);
        return Set.copyOf(next);
    }

    private boolean isUnresolvedTypePlaceholder(CompiledDataParentType type) {
        return type.fields().isEmpty()
               && type.subTypes().isEmpty()
               && type.typeParameters().isEmpty();
    }

    private boolean isUnresolvedDataPlaceholder(CompiledDataType type) {
        return type.fields().isEmpty()
               && type.typeParameters().isEmpty()
               && type.extendedTypes().isEmpty()
               && !type.singleton();
    }


    private Result<Void> validateMethodGetterCompatibility(Program program, CompiledProgram compiledProgram) {
        var modulesByName = compiledProgram.modules().stream()
                .collect(toMap(CompiledModule::name, identity(), (first, second) -> first));
        for (var module : program.modules()) {
            var compiledModule = modulesByName.get(module.name());
            if (compiledModule == null) {
                continue;
            }
            var validation = validateMethodGetterCompatibility(
                    module,
                    List.copyOf(compiledModule.functions()),
                    moduleSourceFile(module)
            );
            if (validation instanceof Result.Error<Void> error) {
                return error;
            }
        }
        return new Result.Success<>(null);
    }

    private Result<Void> validateMethodGetterCompatibility(
            Module module,
            List<CompiledFunction> compiledFunctions,
            String moduleSourceFile
    ) {
        var parserFunctionsByKey = module.functional().definitions().stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .collect(toMap(
                        function -> functionKey(function.name(), function.parameters().size()),
                        identity(),
                        (first, second) -> first
                ));
        var declarationsByName = module.functional().definitions().stream()
                .filter(definition -> definition instanceof DataDeclaration || definition instanceof TypeDeclaration)
                .collect(toMap(
                        definition -> switch (definition) {
                            case DataDeclaration dataDeclaration -> dataDeclaration.name();
                            case TypeDeclaration typeDeclaration -> typeDeclaration.name();
                            default -> throw new IllegalStateException("Unexpected definition: " + definition);
                        },
                        identity(),
                        (first, second) -> first
                ));
        for (var function : compiledFunctions) {
            var validation = validateMethodGetterCompatibility(function, parserFunctionsByKey, declarationsByName, moduleSourceFile);
            if (validation instanceof Result.Error<Void> error) {
                return error;
            }
        }
        return new Result.Success<>(null);
    }

    private Result<Void> validateMethodGetterCompatibility(
            CompiledFunction function,
            Map<String, Function> parserFunctionsByKey,
            Map<String, Definition> declarationsByName,
            String moduleSourceFile
    ) {
        var ownerTypeName = methodOwnerType(function.name());
        var methodName = methodSimpleName(function.name());
        if (ownerTypeName.isEmpty() || methodName.isEmpty() || function.parameters().size() != 1) {
            return new Result.Success<>(null);
        }

        var parserFunction = parserFunctionsByKey.get(functionKey(function.name(), function.parameters().size()));
        if (parserFunction == null) {
            return new Result.Success<>(null);
        }

        var ownerType = function.parameters().getFirst().type();
        if (ownerType instanceof CompiledDataType dataType) {
            return conflictingField(dataType.fields(), methodName.get())
                    .filter(field -> !field.type().equals(function.returnType()))
                    .<Result<Void>>map(field -> methodGetterConflictError(
                            parserFunction,
                            ownerTypeName.get(),
                            methodName.get(),
                            function.returnType(),
                            dataType.name(),
                            field,
                            declarationsByName.get(dataType.name()),
                            moduleSourceFile
                    ))
                    .orElseGet(() -> new Result.Success<>(null));
        }
        if (ownerType instanceof CompiledDataParentType parentType) {
            for (var subType : parentType.subTypes()) {
                var conflict = conflictingField(subType.fields(), methodName.get())
                        .filter(field -> !field.type().equals(function.returnType()));
                if (conflict.isPresent()) {
                    return methodGetterConflictError(
                            parserFunction,
                            ownerTypeName.get(),
                            methodName.get(),
                            function.returnType(),
                            subType.name(),
                            conflict.get(),
                            declarationsByName.get(subType.name()),
                            moduleSourceFile
                    );
                }
            }
        }
        return new Result.Success<>(null);
    }

    private Result<Void> methodGetterConflictError(
            Function methodFunction,
            String ownerTypeName,
            String methodName,
            CompiledType methodReturnType,
            String conflictingSubtypeName,
            CompiledDataType.CompiledField field,
            Definition conflictingDeclaration,
            String moduleSourceFile
    ) {
        var normalizedFile = normalizeFile(moduleSourceFile);
        var methodLine = methodDeclarationErrorLine(methodFunction);
        var methodColumn = methodDeclarationErrorColumn(methodFunction);
        var functionPreview = formatFunctionHeader(methodFunction) + " =";
        var pointerIndent = methodDeclarationPointerIndent(methodFunction, functionPreview);
        var conflictLocation = declarationLocation(conflictingDeclaration, normalizedFile);
        var pointer = " ".repeat(Math.max(pointerIndent, 0))
                      + "^ Field getter `" + conflictingSubtypeName + "." + field.name() + "` returns `" + field.type() + "`,"
                      + " but this method returns `" + methodReturnType + "`. Conflicting declaration: " + conflictLocation;
        var message = "error: mismatched types\n"
                      + " --> " + normalizedFile + ":" + methodLine + ":" + methodColumn + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new Result.Error<>(List.of(new Result.Error.SingleError(methodLine, methodColumn, normalizedFile, message)));
    }

    private String declarationLocation(Definition definition, String fallbackFile) {
        if (definition instanceof DataDeclaration dataDeclaration) {
            return sourceLocation(fallbackFile, dataDeclaration.position(), "data `" + dataDeclaration.name() + "`");
        }
        if (definition instanceof TypeDeclaration typeDeclaration) {
            return sourceLocation(fallbackFile, typeDeclaration.position(), "type `" + typeDeclaration.name() + "`");
        }
        return sourceLocation(fallbackFile, Optional.empty(), "declaration");
    }

    private String sourceLocation(String file, Optional<SourcePosition> position, String label) {
        var sourcePosition = position.orElse(SourcePosition.EMPTY);
        if (sourcePosition == SourcePosition.EMPTY) {
            return label + " in " + file;
        }
        return label + " at " + file + ":" + sourcePosition.line() + ":" + sourcePosition.column();
    }

    private String functionKey(String name, int parameterCount) {
        return name + "#" + parameterCount;
    }

    private Optional<CompiledDataType.CompiledField> conflictingField(
            List<CompiledDataType.CompiledField> fields,
            String fieldName
    ) {
        return fields.stream()
                .filter(field -> field.name().equals(fieldName))
                .findFirst();
    }

    private boolean isQualifiedExternalPlaceholder(CompiledDataParentType type) {
        return type.name().startsWith("/") && isUnresolvedTypePlaceholder(type);
    }

    private GenericDataType resolveQualifiedExternalType(Map<String, GenericDataType> all, String qualifiedTypeName) {
        var direct = all.get(qualifiedTypeName);
        if (direct != null) {
            return direct;
        }
        var genericStart = qualifiedTypeName.indexOf('[');
        if (genericStart > 0) {
            return all.get(qualifiedTypeName.substring(0, genericStart));
        }
        return null;
    }

    private GenericDataType withRequestedName(GenericDataType type, String requestedName) {
        return switch (type) {
            case CompiledDataType linkedDataType -> new CompiledDataType(
                    requestedName,
                    linkedDataType.fields(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.comments(),
                    linkedDataType.visibility(),
                    linkedDataType.singleton(),
                    linkedDataType.enumValue()
            );
            case CompiledDataParentType linkedDataParentType -> new CompiledDataParentType(
                    requestedName,
                    linkedDataParentType.fields(),
                    linkedDataParentType.subTypes(),
                    linkedDataParentType.typeParameters(),
                    linkedDataParentType.comments(),
                    linkedDataParentType.visibility(),
                    linkedDataParentType.enumType()
            );
        };
    }

    private SortedSet<CompiledModule.StaticImport> deriverLexicalStaticImports(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            Map<String, SortedSet<CompiledModule.StaticImport>> staticImportsByModule
    ) {
        var imports = new TreeSet<CompiledModule.StaticImport>();
        for (var ownerModule : usedImportedDeriverOwnerModules(module, moduleLinkIndex, deriversByModule)) {
            var ownerClassName = getModuleEntry(moduleLinkIndex.moduleJavaClassNameByModuleName(), ownerModule);
            if (ownerClassName != null) {
                imports.add(new CompiledModule.StaticImport(ownerClassName, "*"));
            }
            var ownerStaticImports = getModuleEntry(staticImportsByModule, ownerModule);
            if (ownerStaticImports != null) {
                imports.addAll(ownerStaticImports);
            }
        }
        return unmodifiableSortedSet(imports);
    }

    private SortedSet<CompiledModule.StaticImport> staticImports(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, SortedMap<String, GenericDataType>> linkedTypesByModule,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            Map<String, SortedSet<CompiledModule.StaticImport>> staticImportsByModule,
            CompileCache compileCache
    ) {
        var cacheKey = moduleCacheKey(module);
        var cachedByModule = compileCache.staticImportsByModulePhase
                .computeIfAbsent(signaturesByModule, ignored -> new HashMap<>());
        var cached = cachedByModule.get(cacheKey);
        if (cached != null) {
            return cached;
        }
        var startedAt = System.nanoTime();
        var imports = new HashSet<CompiledModule.StaticImport>();
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), moduleLinkIndex);
            if (importedModule == null) {
                continue;
            }
            var className = getModuleEntry(moduleLinkIndex.moduleJavaClassNameByModuleName(), importedModule);
            var importedSignaturesByName = visibleSignaturesByName(
                    module.path(),
                    importedModule,
                    getModuleEntry(signaturesByModule, importedModule),
                    compileCache
            );
            var importedTypes = visibleTypes(
                    module.path(),
                    importedModule,
                    getModuleEntry(linkedTypesByModule, importedModule),
                    compileCache
            );
            if (importDeclaration.isStarImport() && importDeclaration.excludedSymbols().isEmpty()) {
                imports.add(new CompiledModule.StaticImport(className, "*"));
                continue;
            }
            var availableFunctionMembers = importedSignaturesByName.keySet();
            var availableTypeMembers = new HashSet<>(importedTypes.keySet());
            var availableMembers = new HashSet<String>(availableFunctionMembers);
            availableMembers.addAll(availableTypeMembers);
            for (var symbol : importDeclaration.selectedSymbols(availableMembers)) {
                if (availableMembers.contains(symbol)) {
                    imports.add(new CompiledModule.StaticImport(className, symbol));
                }
            }
        }
        imports.addAll(deriverLexicalStaticImports(module, moduleLinkIndex, deriversByModule, staticImportsByModule));
        compileCache.staticImportGenerationNanos += System.nanoTime() - startedAt;
        var result = unmodifiableSortedSet(new TreeSet<>(imports));
        cachedByModule.put(cacheKey, result);
        return result;
    }
    private SortedSet<CompiledFunction> deduplicateFunctions(List<CompiledFunction> linkedFunctions) {
        var byKey = new LinkedHashMap<String, CompiledFunction>();
        for (var function : linkedFunctions) {
            var parameters = function.parameters().stream().map(parameter -> String.valueOf(parameter.type())).toList();
            byKey.put(function.name() + "#" + parameters, function);
        }
        return unmodifiableSortedSet(new TreeSet<>(byKey.values()));
    }

    private ModuleRef resolveImportedModule(String rawImportedModuleName, ModuleLinkIndex moduleLinkIndex) {
        var direct = moduleLinkIndex.modulesByExactName().get(rawImportedModuleName);
        if (direct != null) {
            return direct;
        }

        var normalized = rawImportedModuleName.replace('\\', '/');
        var normalizedQualified = normalizeModulePath(normalized);
        var qualified = moduleLinkIndex.modulesByQualifiedName().get(normalizedQualified);
        if (qualified != null) {
            return qualified;
        }
        var slashIdx = normalized.lastIndexOf('/');
        if (slashIdx >= 0 && slashIdx < normalized.length() - 1) {
            var tail = normalized.substring(slashIdx + 1);
            var byTail = moduleLinkIndex.modulesByTailName().get(tail);
            if (byTail != null) {
                return byTail;
            }
        }

        var dotIdx = normalized.lastIndexOf('.');
        if (dotIdx >= 0 && dotIdx < normalized.length() - 1) {
            return moduleLinkIndex.modulesByTailName().get(normalized.substring(dotIdx + 1));
        }

        return null;
    }

    private String moduleJavaClassName(ModuleRef module, SortedMap<String, GenericDataType> linkedTypes) {
        var className = module.path().replace('/', '.').replace('\\', '.') + "." + module.name();
        if (linkedTypes == null) {
            return className;
        }
        var ownerType = linkedTypes.get(module.name());
        if (ownerType instanceof CompiledDataParentType) {
            return className;
        }
        if (ownerType != null) {
            return className + "Module";
        }
        return className;
    }

    private List<CapybaraExpressionCompiler.FunctionSignature> visibleSignatures(
            String currentModulePath,
            ModuleRef ownerModule,
            List<CapybaraExpressionCompiler.FunctionSignature> signatures,
            CompileCache compileCache
    ) {
        var key = signatureVisibilityCacheKey(currentModulePath, ownerModule, signatures);
        return compileCache.visibleSignaturesByScope.computeIfAbsent(key, ignored -> signatures.stream()
                .filter(signature -> isVisibleFromModule(currentModulePath, ownerModule.path(), signature.visibility()))
                .toList());
    }

    private SortedMap<String, GenericDataType> visibleTypes(
            String currentModulePath,
            ModuleRef ownerModule,
            SortedMap<String, GenericDataType> types,
            CompileCache compileCache
    ) {
        var key = visibilityCacheKey(currentModulePath, ownerModule);
        return compileCache.visibleTypesByScope.computeIfAbsent(key, ignored -> {
            var filteredTypes = types.entrySet().stream()
                    .filter(entry -> isVisibleFromModule(currentModulePath, ownerModule.path(), entry.getValue().visibility()))
                    .collect(java.util.stream.Collectors.toMap(
                            Map.Entry::getKey,
                            Map.Entry::getValue,
                            (first, second) -> first,
                            TreeMap::new
                    ));
            return unmodifiableSortedMap(filteredTypes);
        });
    }

    private Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> visibleSignaturesByName(
            String currentModulePath,
            ModuleRef ownerModule,
            List<CapybaraExpressionCompiler.FunctionSignature> signatures,
            CompileCache compileCache
    ) {
        var key = signatureVisibilityCacheKey(currentModulePath, ownerModule, signatures);
        return compileCache.visibleSignaturesByNameByScope.computeIfAbsent(key, ignored -> {
            var visibleSignatures = visibleSignatures(currentModulePath, ownerModule, signatures, compileCache);
            var signaturesByName = new LinkedHashMap<String, List<CapybaraExpressionCompiler.FunctionSignature>>();
            for (var signature : visibleSignatures) {
                signaturesByName.computeIfAbsent(signature.name(), ignoredName -> new ArrayList<>()).add(signature);
            }
            var immutableSignaturesByName = new LinkedHashMap<String, List<CapybaraExpressionCompiler.FunctionSignature>>();
            signaturesByName.forEach((name, items) -> immutableSignaturesByName.put(name, List.copyOf(items)));
            return Map.copyOf(immutableSignaturesByName);
        });
    }

    private VisibilityCacheKey visibilityCacheKey(String currentModulePath, ModuleRef ownerModule) {
        return new VisibilityCacheKey(normalizeModulePath(currentModulePath), ownerModule.name(), normalizeModulePath(ownerModule.path()));
    }

    private SignatureVisibilityCacheKey signatureVisibilityCacheKey(
            String currentModulePath,
            ModuleRef ownerModule,
            List<CapybaraExpressionCompiler.FunctionSignature> signatures
    ) {
        return new SignatureVisibilityCacheKey(
                normalizeModulePath(currentModulePath),
                ownerModule.name(),
                normalizeModulePath(ownerModule.path()),
                System.identityHashCode(signatures)
        );
    }

    private ModuleCacheKey moduleCacheKey(Module module) {
        return new ModuleCacheKey(
                normalizeModulePath(module.path()),
                module.name()
        );
    }

    private boolean isVisibleFromModule(String currentModulePath, String ownerModulePath) {
        var current = normalizeModulePath(currentModulePath);
        var owner = normalizeModulePath(ownerModulePath);
        return current.equals(owner) || current.startsWith(owner + "/");
    }

    private boolean isVisibleFromModule(String currentModulePath, String ownerModulePath, Visibility visibility) {
        if (visibility == null) {
            return true;
        }
        var current = normalizeModulePath(currentModulePath);
        var owner = normalizeModulePath(ownerModulePath);
        return switch (visibility) {
            case LOCAL -> isVisibleFromModule(current, owner);
            case PRIVATE -> current.equals(owner);
        };
    }

    private String normalizeModulePath(String modulePath) {
        var normalized = modulePath.replace('\\', '/');
        var parts = new java.util.ArrayList<String>();
        for (var part : normalized.split("/+")) {
            if (part.isBlank() || part.equals(".")) {
                continue;
            }
            parts.add(part);
        }
        return String.join("/", parts);
    }

    private List<CapybaraExpressionCompiler.FunctionSignature> mergeSignatures(
            List<CapybaraExpressionCompiler.FunctionSignature> first,
            List<CapybaraExpressionCompiler.FunctionSignature> second
    ) {
        var merged = new LinkedHashMap<String, CapybaraExpressionCompiler.FunctionSignature>();
        first.forEach(signature -> merged.put(signatureKey(signature), signature));
        second.forEach(signature -> merged.put(signatureKey(signature), signature));
        return List.copyOf(merged.values());
    }

    private String signatureKey(CapybaraExpressionCompiler.FunctionSignature signature) {
        var parameters = signature.parameterTypes().stream().map(String::valueOf).toList();
        return signature.name() + "#" + parameters;
    }

    private List<Function> findFunctions(Set<Definition> definitions) {
        return definitions.stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .sorted(Comparator
                        .comparingInt((Function function) -> function.position().map(SourcePosition::line).orElse(Integer.MAX_VALUE))
                        .thenComparingInt(function -> function.position().map(SourcePosition::column).orElse(Integer.MAX_VALUE)))
                .toList();
    }

    private Result<List<Function>> functions(
            Module module,
            SortedMap<String, GenericDataType> linkedTypes,
            Map<String, GenericDataType> visibleTypes,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            ModuleLinkIndex moduleLinkIndex
    ) {
        var combined = new ArrayList<Function>();
        combined.addAll(findFunctions(module.functional().definitions()));
        combined.addAll(constructorFunctions(module.functional().definitions(), linkedTypes));
        var derivedFunctions = derivedFunctions(module, linkedTypes, visibleTypes, deriversByModule, moduleLinkIndex);
        if (derivedFunctions instanceof Result.Error<List<Function>> error) {
            return new Result.Error<>(error.errors());
        }
        combined.addAll(((Result.Success<List<Function>>) derivedFunctions).value());
        combined.sort(Comparator
                .comparingInt((Function function) -> function.position().map(SourcePosition::line).orElse(Integer.MAX_VALUE))
                .thenComparingInt(function -> function.position().map(SourcePosition::column).orElse(Integer.MAX_VALUE))
                .thenComparing(Function::name));
        return Result.success(List.copyOf(combined));
    }

    private Result<List<Function>> derivedFunctions(
            Module module,
            SortedMap<String, GenericDataType> linkedTypes,
            Map<String, GenericDataType> visibleTypes,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule,
            ModuleLinkIndex moduleLinkIndex
    ) {
        var deriversByName = availableDerivers(module, moduleLinkIndex, deriversByModule);
        var generated = new ArrayList<Function>();
        for (var target : deriveTargets(module.functional().definitions())) {
            var linkedTarget = linkedTypes.get(target.name());
            if (linkedTarget == null) {
                continue;
            }
            for (var directive : target.derives()) {
                var availableDeriver = deriversByName.get(directive.name());
                if (availableDeriver == null) {
                    return withPosition(
                            Result.error("Deriver `" + directive.name() + "` not found for `" + target.name() + "`"),
                            directive.position().or(target::position),
                            normalizeFile(moduleSourceFile(module))
                    );
                }
                var deriver = availableDeriver.deriver();
                for (var method : deriver.methods()) {
                    var expression = expandDeriverExpression(method.expression(), linkedTarget, moduleSourceFile(module));
                    if (expression instanceof Result.Error<Expression> error) {
                        return new Result.Error<>(error.errors());
                    }
                    var parameters = new ArrayList<Parameter>(method.parameters().size() + 1);
                    parameters.add(new Parameter(
                            compiledTypeToParserType(linkedTarget),
                            "this",
                            directive.position().or(target::position)
                    ));
                    parameters.addAll(method.parameters());
                    generated.add(new Function(
                            METHOD_DECL_PREFIX + baseTypeName(linkedTarget.name()) + "__" + method.name(),
                            List.copyOf(parameters),
                            Optional.of(method.returnType()),
                            ((Result.Success<Expression>) expression).value(),
                            method.comments(),
                            linkedTarget.visibility(),
                            directive.position().or(target::position)
                    ));
                }
            }
        }
        return Result.success(List.copyOf(generated));
    }

    private Map<String, AvailableDeriver> availableDerivers(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule
    ) {
        var all = new LinkedHashMap<String, AvailableDeriver>();
        var currentModule = new ModuleRef(module.name(), module.path());
        Optional.ofNullable(getModuleEntry(deriversByModule, currentModule)).orElse(Map.of())
                .forEach((name, deriver) -> all.put(name, new AvailableDeriver(currentModule, deriver)));
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), moduleLinkIndex);
            if (importedModule == null) {
                continue;
            }
            var importedDerivers = visibleDerivers(
                    module.path(),
                    importedModule,
                    getModuleEntry(deriversByModule, importedModule)
            );
            for (var symbol : importDeclaration.selectedSymbols(importedDerivers.keySet())) {
                var deriver = importedDerivers.get(symbol);
                if (deriver != null) {
                    all.putIfAbsent(symbol, new AvailableDeriver(importedModule, deriver));
                }
            }
        }
        return Map.copyOf(all);
    }

    private record AvailableDeriver(ModuleRef ownerModule, DeriverDeclaration deriver) {
    }

    private Map<String, DeriverDeclaration> visibleDerivers(
            String currentModulePath,
            ModuleRef ownerModule,
            Map<String, DeriverDeclaration> derivers
    ) {
        if (derivers == null || derivers.isEmpty()) {
            return Map.of();
        }
        var visible = new LinkedHashMap<String, DeriverDeclaration>();
        for (var entry : derivers.entrySet()) {
            if (isVisibleFromModule(currentModulePath, ownerModule.path(), entry.getValue().visibility())) {
                visible.put(entry.getKey(), entry.getValue());
            }
        }
        return Map.copyOf(visible);
    }

    private List<ModuleRef> usedImportedDeriverOwnerModules(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            Map<String, Map<String, DeriverDeclaration>> deriversByModule
    ) {
        var currentModule = new ModuleRef(module.name(), module.path());
        var availableDerivers = availableDerivers(module, moduleLinkIndex, deriversByModule);
        var owners = new LinkedHashMap<String, ModuleRef>();
        for (var target : deriveTargets(module.functional().definitions())) {
            for (var directive : target.derives()) {
                var deriver = availableDerivers.get(directive.name());
                if (deriver == null || sameModule(currentModule, deriver.ownerModule())) {
                    continue;
                }
                owners.putIfAbsent(qualifiedModuleName(deriver.ownerModule()), deriver.ownerModule());
            }
        }
        return List.copyOf(owners.values());
    }

    private boolean sameModule(ModuleRef left, ModuleRef right) {
        return left.name().equals(right.name())
               && normalizeModulePath(left.path()).equals(normalizeModulePath(right.path()));
    }

    private ModuleRef resolveModuleByJavaClassName(String javaClassName, ModuleLinkIndex moduleLinkIndex) {
        for (var entry : moduleLinkIndex.moduleJavaClassNameByModuleName().entrySet()) {
            if (!entry.getValue().equals(javaClassName) || !entry.getKey().contains("/")) {
                continue;
            }
            var module = moduleLinkIndex.modulesByQualifiedName().get(entry.getKey());
            if (module != null) {
                return module;
            }
        }
        for (var entry : moduleLinkIndex.moduleJavaClassNameByModuleName().entrySet()) {
            if (!entry.getValue().equals(javaClassName)) {
                continue;
            }
            var module = moduleLinkIndex.modulesByQualifiedName().get(entry.getKey());
            if (module != null) {
                return module;
            }
            module = moduleLinkIndex.modulesByExactName().get(entry.getKey());
            if (module != null) {
                return module;
            }
        }
        return null;
    }

    private Result<Map<String, DeriverDeclaration>> derivers(Set<Definition> definitions, String moduleSourceFile) {
        var derivers = new LinkedHashMap<String, DeriverDeclaration>();
        for (var definition : definitions) {
            if (!(definition instanceof DeriverDeclaration deriver)) {
                continue;
            }
            var existing = derivers.putIfAbsent(deriver.name(), deriver);
            if (existing != null) {
                return withPosition(
                        Result.error("Duplicate deriver `" + deriver.name() + "`"),
                        deriver.position(),
                        normalizeFile(moduleSourceFile)
                );
            }
            var methodSignatures = new HashSet<String>();
            for (var method : deriver.methods()) {
                var methodKey = method.name() + "#"
                                + method.parameters().stream()
                                        .map(parameter -> parameter.type().name())
                                        .toList();
                if (!methodSignatures.add(methodKey)) {
                    return withPosition(
                            Result.error("Duplicate deriver method signature `" + method.name() + "`"),
                            method.position(),
                            normalizeFile(moduleSourceFile)
                    );
                }
                var reservedReceiver = method.parameters().stream()
                        .filter(parameter -> "receiver".equals(parameter.name()))
                        .findFirst();
                if (reservedReceiver.isPresent()) {
                    return withPosition(
                            Result.error("Deriver method parameter cannot be named `receiver` because `receiver` is the generated derive receiver"),
                            reservedReceiver.orElseThrow().position(),
                            normalizeFile(moduleSourceFile)
                    );
                }
            }
        }
        return Result.success(Map.copyOf(derivers));
    }

    private List<DeriveTarget> deriveTargets(Set<Definition> definitions) {
        return definitions.stream()
                .map(definition -> switch (definition) {
                    case DataDeclaration dataDeclaration -> new DeriveTarget(
                            dataDeclaration.name(),
                            dataDeclaration.derives(),
                            dataDeclaration.visibility(),
                            dataDeclaration.position()
                    );
                    case TypeDeclaration typeDeclaration -> new DeriveTarget(
                            typeDeclaration.name(),
                            typeDeclaration.derives(),
                            typeDeclaration.visibility(),
                            typeDeclaration.position()
                    );
                    default -> null;
                })
                .filter(Objects::nonNull)
                .filter(target -> !target.derives().isEmpty())
                .toList();
    }

    private record DeriveTarget(
            String name,
            List<DeriveDirective> derives,
            Visibility visibility,
            Optional<SourcePosition> position
    ) {
    }

    private Result<Expression> expandDeriverExpression(
            Expression expression,
            GenericDataType targetType,
            String moduleSourceFile
    ) {
        return expandDeriverExpression(expression, targetType, moduleSourceFile, Set.of());
    }

    private Result<Expression> expandDeriverExpression(
            Expression expression,
            GenericDataType targetType,
            String moduleSourceFile,
            Set<String> boundNames
    ) {
        if (expression instanceof Value value && "receiver".equals(value.name()) && !boundNames.contains("receiver")) {
            return Result.success(new Value("this", value.position()));
        }
        return switch (expression) {
            case FunctionCall functionCall -> expandDeriverFunctionCall(functionCall, targetType, moduleSourceFile, boundNames);
            case FunctionReference functionReference -> expandDeriverFunctionReference(functionReference, moduleSourceFile);
            case FunctionInvoke functionInvoke -> expandDeriverExpressions(functionInvoke.arguments(), targetType, moduleSourceFile, boundNames)
                    .flatMap(arguments -> expandDeriverExpression(functionInvoke.function(), targetType, moduleSourceFile, boundNames)
                            .map(function -> new FunctionInvoke(function, arguments, functionInvoke.position())));
            case LetExpression letExpression -> expandDeriverExpression(letExpression.value(), targetType, moduleSourceFile, boundNames)
                    .flatMap(value -> expandDeriverExpression(letExpression.rest(), targetType, moduleSourceFile, withBoundName(boundNames, letExpression.name()))
                            .map(rest -> new LetExpression(
                                    letExpression.name(),
                                    letExpression.declaredType(),
                                    letExpression.kind(),
                                    value,
                                    rest,
                                    letExpression.position()
                            )));
            case IfExpression ifExpression -> expandDeriverExpression(ifExpression.condition(), targetType, moduleSourceFile, boundNames)
                    .flatMap(condition -> expandDeriverExpression(ifExpression.thenBranch(), targetType, moduleSourceFile, boundNames)
                            .flatMap(thenBranch -> expandDeriverExpression(ifExpression.elseBranch(), targetType, moduleSourceFile, boundNames)
                                    .map(elseBranch -> new IfExpression(condition, thenBranch, elseBranch, ifExpression.position()))));
            case InfixExpression infixExpression -> expandDeriverExpression(infixExpression.left(), targetType, moduleSourceFile, boundNames)
                    .flatMap(left -> expandDeriverExpression(infixExpression.right(), targetType, moduleSourceFile, boundNames)
                            .map(right -> new InfixExpression(left, infixExpression.operator(), right, infixExpression.position())));
            case FieldAccess fieldAccess -> expandDeriverExpression(fieldAccess.source(), targetType, moduleSourceFile, boundNames)
                    .map(source -> new FieldAccess(source, fieldAccess.field(), fieldAccess.position()));
            case LambdaExpression lambdaExpression -> expandDeriverExpression(lambdaExpression.expression(), targetType, moduleSourceFile, withBoundNames(boundNames, lambdaExpression.argumentNames()))
                    .map(body -> new LambdaExpression(lambdaExpression.argumentNames(), body, lambdaExpression.position()));
            case ReduceExpression reduceExpression -> expandDeriverExpression(reduceExpression.initialValue(), targetType, moduleSourceFile, boundNames)
                    .flatMap(initialValue -> expandDeriverExpression(reduceExpression.reducerExpression(), targetType, moduleSourceFile, withReduceBoundNames(boundNames, reduceExpression))
                            .map(reducerExpression -> new ReduceExpression(
                                    initialValue,
                                    reduceExpression.accumulatorName(),
                                    reduceExpression.keyName(),
                                    reduceExpression.valueName(),
                                    reducerExpression,
                                    reduceExpression.position()
                            )));
            case IndexExpression indexExpression -> expandDeriverExpression(indexExpression.source(), targetType, moduleSourceFile, boundNames)
                    .flatMap(source -> expandDeriverExpressions(indexExpression.arguments(), targetType, moduleSourceFile, boundNames)
                            .map(arguments -> new IndexExpression(source, arguments, indexExpression.position())));
            case SliceExpression sliceExpression -> expandDeriverExpression(sliceExpression.source(), targetType, moduleSourceFile, boundNames)
                    .flatMap(source -> expandOptionalDeriverExpression(sliceExpression.start(), targetType, moduleSourceFile, boundNames)
                            .flatMap(start -> expandOptionalDeriverExpression(sliceExpression.end(), targetType, moduleSourceFile, boundNames)
                                    .map(end -> new SliceExpression(source, start, end, sliceExpression.position()))));
            case MatchExpression matchExpression -> expandDeriverExpression(matchExpression.matchWith(), targetType, moduleSourceFile, boundNames)
                    .flatMap(matchWith -> matchExpression.cases().stream()
                            .map(matchCase -> {
                                var branchBoundNames = withBoundNames(boundNames, patternBoundNames(matchCase.pattern()));
                                return expandOptionalDeriverExpression(matchCase.guard(), targetType, moduleSourceFile, branchBoundNames)
                                        .flatMap(guard -> expandDeriverExpression(matchCase.expression(), targetType, moduleSourceFile, branchBoundNames)
                                                .map(body -> new MatchExpression.MatchCase(matchCase.pattern(), guard, body)));
                            })
                            .collect(new ResultCollectionCollector<>())
                            .map(cases -> new MatchExpression(matchWith, cases, matchExpression.position())));
            case NewData newData -> expandFieldAssignments(newData.assignments(), targetType, moduleSourceFile, boundNames)
                    .flatMap(assignments -> expandDeriverExpressions(newData.positionalArguments(), targetType, moduleSourceFile, boundNames)
                            .flatMap(positionalArguments -> expandDeriverExpressions(newData.spreads(), targetType, moduleSourceFile, boundNames)
                                    .map(spreads -> new NewData(
                                            newData.type(),
                                            newData.bypassConstructor(),
                                            assignments,
                                            positionalArguments,
                                            spreads,
                                            newData.position()
                                    ))));
            case ConstructorData constructorData -> expandFieldAssignments(constructorData.assignments(), targetType, moduleSourceFile, boundNames)
                    .flatMap(assignments -> expandDeriverExpressions(constructorData.positionalArguments(), targetType, moduleSourceFile, boundNames)
                            .flatMap(positionalArguments -> expandDeriverExpressions(constructorData.spreads(), targetType, moduleSourceFile, boundNames)
                                    .map(spreads -> new ConstructorData(
                                            assignments,
                                            positionalArguments,
                                            spreads,
                                            constructorData.position()
                                    ))));
            case WithExpression withExpression -> expandDeriverExpression(withExpression.source(), targetType, moduleSourceFile, boundNames)
                    .flatMap(source -> expandFieldAssignments(withExpression.assignments(), targetType, moduleSourceFile, boundNames)
                            .map(assignments -> new WithExpression(source, assignments, withExpression.position())));
            case NewListExpression newListExpression -> expandDeriverExpressions(newListExpression.values(), targetType, moduleSourceFile, boundNames)
                    .map(values -> new NewListExpression(values, newListExpression.position()));
            case NewSetExpression newSetExpression -> expandDeriverExpressions(newSetExpression.values(), targetType, moduleSourceFile, boundNames)
                    .map(values -> new NewSetExpression(values, newSetExpression.position()));
            case NewDictExpression newDictExpression -> newDictExpression.entries().stream()
                    .map(entry -> expandDeriverExpression(entry.key(), targetType, moduleSourceFile, boundNames)
                            .flatMap(key -> expandDeriverExpression(entry.value(), targetType, moduleSourceFile, boundNames)
                                    .map(value -> new NewDictExpression.Entry(key, value))))
                    .collect(new ResultCollectionCollector<>())
                    .map(entries -> new NewDictExpression(entries, newDictExpression.position()));
            case TupleExpression tupleExpression -> expandDeriverExpressions(tupleExpression.values(), targetType, moduleSourceFile, boundNames)
                    .map(values -> new TupleExpression(values, tupleExpression.position()));
            default -> Result.success(expression);
        };
    }

    private Result<Expression> expandDeriverFunctionReference(
            FunctionReference functionReference,
            String moduleSourceFile
    ) {
        if ("derive_type_name".equals(functionReference.name())) {
            return legacyDeriveHelperError(
                    "`derive_type_name()` has been replaced by `reflection(receiver)`. Import `/capy/meta_prog/Reflection` and use `reflection(receiver).name`.",
                    functionReference.position(),
                    moduleSourceFile
            );
        }
        if ("derive_fields_join".equals(functionReference.name())) {
            return legacyDeriveHelperError(
                    "`derive_fields_join(...)` has been replaced by `reflection(receiver)`. Import `/capy/meta_prog/Reflection` and fold over `reflection(receiver).fields`.",
                    functionReference.position(),
                    moduleSourceFile
            );
        }
        return Result.success(functionReference);
    }

    private Result<Expression> expandDeriverFunctionCall(
            FunctionCall functionCall,
            GenericDataType targetType,
            String moduleSourceFile
    ) {
        return expandDeriverFunctionCall(functionCall, targetType, moduleSourceFile, Set.of());
    }

    private Result<Expression> expandDeriverFunctionCall(
            FunctionCall functionCall,
            GenericDataType targetType,
            String moduleSourceFile,
            Set<String> boundNames
    ) {
        if (functionCall.moduleName().isEmpty() && "derive_type_name".equals(functionCall.name())) {
            return legacyDeriveHelperError(
                    "`derive_type_name()` has been replaced by `reflection(receiver)`. Import `/capy/meta_prog/Reflection` and use `reflection(receiver).name`.",
                    functionCall.position(),
                    moduleSourceFile
            );
        }
        if (functionCall.moduleName().isEmpty() && "derive_fields_join".equals(functionCall.name())) {
            return legacyDeriveHelperError(
                    "`derive_fields_join(...)` has been replaced by `reflection(receiver)`. Import `/capy/meta_prog/Reflection` and fold over `reflection(receiver).fields`.",
                    functionCall.position(),
                    moduleSourceFile
            );
        }
        return expandDeriverExpressions(functionCall.arguments(), targetType, moduleSourceFile, boundNames)
                .map(arguments -> new FunctionCall(functionCall.moduleName(), functionCall.name(), arguments, functionCall.position()));
    }

    private Result<Expression> legacyDeriveHelperError(
            String message,
            Optional<SourcePosition> position,
            String moduleSourceFile
    ) {
        return withPosition(
                Result.error(message),
                position,
                normalizeFile(moduleSourceFile)
        );
    }

    private Result<List<Expression>> expandDeriverExpressions(
            List<Expression> expressions,
            GenericDataType targetType,
            String moduleSourceFile
    ) {
        return expandDeriverExpressions(expressions, targetType, moduleSourceFile, Set.of());
    }

    private Result<List<Expression>> expandDeriverExpressions(
            List<Expression> expressions,
            GenericDataType targetType,
            String moduleSourceFile,
            Set<String> boundNames
    ) {
        return expressions.stream()
                .map(expression -> expandDeriverExpression(expression, targetType, moduleSourceFile, boundNames))
                .collect(new ResultCollectionCollector<>());
    }

    private Result<Optional<Expression>> expandOptionalDeriverExpression(
            Optional<Expression> expression,
            GenericDataType targetType,
            String moduleSourceFile
    ) {
        return expandOptionalDeriverExpression(expression, targetType, moduleSourceFile, Set.of());
    }

    private Result<Optional<Expression>> expandOptionalDeriverExpression(
            Optional<Expression> expression,
            GenericDataType targetType,
            String moduleSourceFile,
            Set<String> boundNames
    ) {
        return expression
                .map(value -> expandDeriverExpression(value, targetType, moduleSourceFile, boundNames).map(Optional::of))
                .orElseGet(() -> Result.success(Optional.empty()));
    }

    private Result<List<NewData.FieldAssignment>> expandFieldAssignments(
            List<NewData.FieldAssignment> assignments,
            GenericDataType targetType,
            String moduleSourceFile
    ) {
        return expandFieldAssignments(assignments, targetType, moduleSourceFile, Set.of());
    }

    private Result<List<NewData.FieldAssignment>> expandFieldAssignments(
            List<NewData.FieldAssignment> assignments,
            GenericDataType targetType,
            String moduleSourceFile,
            Set<String> boundNames
    ) {
        return assignments.stream()
                .map(assignment -> expandDeriverExpression(assignment.value(), targetType, moduleSourceFile, boundNames)
                        .map(value -> new NewData.FieldAssignment(assignment.name(), value)))
                .collect(new ResultCollectionCollector<>());
    }

    private Set<String> withBoundName(Set<String> names, String name) {
        var updated = new HashSet<>(names);
        updated.add(name);
        return Set.copyOf(updated);
    }

    private Set<String> withBoundNames(Set<String> names, Collection<String> additionalNames) {
        var updated = new HashSet<>(names);
        updated.addAll(additionalNames);
        return Set.copyOf(updated);
    }

    private Set<String> withReduceBoundNames(Set<String> names, ReduceExpression reduceExpression) {
        var updated = new HashSet<>(names);
        updated.add(reduceExpression.accumulatorName());
        reduceExpression.keyName().ifPresent(updated::add);
        updated.add(reduceExpression.valueName());
        return Set.copyOf(updated);
    }

    private Set<String> patternBoundNames(MatchExpression.Pattern pattern) {
        return patternBoundNames(pattern, false);
    }

    private Set<String> patternBoundNames(MatchExpression.Pattern pattern, boolean nested) {
        var names = new HashSet<String>();
        switch (pattern) {
            case MatchExpression.TypedPattern typedPattern -> {
                if (!"_".equals(typedPattern.name())) {
                    names.add(typedPattern.name());
                }
            }
            case MatchExpression.WildcardBindingPattern wildcardBindingPattern -> names.add(wildcardBindingPattern.name());
            case MatchExpression.VariablePattern variablePattern -> {
                if (nested) {
                    names.add(variablePattern.name());
                }
            }
            case MatchExpression.ConstructorPattern constructorPattern ->
                    constructorPattern.fieldPatterns().forEach(fieldPattern -> names.addAll(patternBoundNames(fieldPattern, true)));
            default -> {
            }
        }
        return Set.copyOf(names);
    }

    private String simpleTypeName(String typeName) {
        var raw = baseTypeName(typeName);
        var slash = raw.lastIndexOf('/');
        if (slash >= 0 && slash < raw.length() - 1) {
            raw = raw.substring(slash + 1);
        }
        var dot = raw.lastIndexOf('.');
        if (dot >= 0 && dot < raw.length() - 1) {
            raw = raw.substring(dot + 1);
        }
        return raw;
    }

    private List<Function> constructorFunctions(Set<Definition> definitions, SortedMap<String, GenericDataType> linkedTypes) {
        var functions = new ArrayList<Function>();
        var typeDeclarations = definitions.stream()
                .filter(TypeDeclaration.class::isInstance)
                .map(TypeDeclaration.class::cast)
                .toList();
        for (var typeDeclaration : typeDeclarations) {
            constructorFunction(typeDeclaration, linkedTypes).ifPresent(functions::add);
        }
        definitions.stream()
                .filter(DataDeclaration.class::isInstance)
                .map(DataDeclaration.class::cast)
                .map(dataDeclaration -> constructorFunction(dataDeclaration, linkedTypes))
                .flatMap(Optional::stream)
                .forEach(functions::add);
        return List.copyOf(functions);
    }

    private Optional<Function> constructorFunction(DataDeclaration dataDeclaration, SortedMap<String, GenericDataType> linkedTypes) {
        var constructor = dataDeclaration.constructor();
        if (constructor.isEmpty()) {
            return Optional.empty();
        }
        var linkedType = linkedTypes.get(dataDeclaration.name());
        if (!(linkedType instanceof GenericDataType genericDataType)) {
            return Optional.empty();
        }
        var parameters = genericDataType.fields().stream()
                .map(field -> new Parameter(compiledTypeToParserType(field.type()), field.name(), dataDeclaration.position()))
                .toList();
        return Optional.of(new Function(
                dataConstructorFunctionName(dataDeclaration.name()),
                parameters,
                Optional.empty(),
                constructor.orElseThrow(),
                List.of(),
                dataDeclaration.visibility(),
                dataDeclaration.position()
        ));
    }

    private Optional<Function> constructorFunction(TypeDeclaration typeDeclaration, SortedMap<String, GenericDataType> linkedTypes) {
        var constructor = typeDeclaration.constructor();
        if (constructor.isEmpty()) {
            return Optional.empty();
        }
        var linkedType = linkedTypes.get(typeDeclaration.name());
        var stateType = linkedTypes.get(constructorStateTypeName(typeDeclaration.name()));
        if (!(linkedType instanceof GenericDataType genericDataType) || !(stateType instanceof GenericDataType)) {
            return Optional.empty();
        }
        var parameters = genericDataType.fields().stream()
                .map(field -> new Parameter(compiledTypeToParserType(field.type()), field.name(), typeDeclaration.position()))
                .toList();
        return Optional.of(new Function(
                typeConstructorFunctionName(typeDeclaration.name()),
                parameters,
                Optional.empty(),
                constructor.orElseThrow(),
                List.of(),
                typeDeclaration.visibility(),
                typeDeclaration.position()
        ));
    }

    private ConstructorCatalog constructorCatalog(
            List<Module> modules,
            SortedSet<CompiledModule> libraries
    ) {
        var constructorsByModule = new LinkedHashMap<String, Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef>>();
        var parentConstructorsByModule = new LinkedHashMap<String, Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>>>();
        var dataOwnersByModule = new LinkedHashMap<String, Map<String, String>>();
        for (var module : modules) {
            var moduleRef = new ModuleRef(module.name(), module.path());
            putOwnedModuleEntry(constructorsByModule, moduleRef, moduleConstructors(module, moduleRef));
            putOwnedModuleEntry(parentConstructorsByModule, moduleRef, moduleParentConstructors(module, moduleRef));
            putOwnedModuleEntry(dataOwnersByModule, moduleRef, moduleDataOwners(module, moduleRef));
        }
        for (var library : libraries) {
            var moduleRef = new ModuleRef(library.name(), library.path());
            putImportedModuleEntry(constructorsByModule, moduleRef, libraryConstructors(library, moduleRef));
            putImportedModuleEntry(parentConstructorsByModule, moduleRef, libraryParentConstructors(library, moduleRef));
            putImportedModuleEntry(dataOwnersByModule, moduleRef, libraryDataOwners(library, moduleRef));
        }
        return new ConstructorCatalog(Map.copyOf(constructorsByModule), Map.copyOf(parentConstructorsByModule), Map.copyOf(dataOwnersByModule));
    }

    private Map<String, String> moduleDataOwners(
            Module module,
            ModuleRef moduleRef
    ) {
        return module.functional().definitions().stream()
                .filter(DataDeclaration.class::isInstance)
                .map(DataDeclaration.class::cast)
                .collect(java.util.stream.Collectors.toUnmodifiableMap(
                        DataDeclaration::name,
                        ignored -> qualifiedModuleName(moduleRef),
                        (first, second) -> first
                ));
    }

    private Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> moduleConstructors(
            Module module,
            ModuleRef moduleRef
    ) {
        var constructors = new LinkedHashMap<String, CapybaraExpressionCompiler.ProtectedConstructorRef>();
        module.functional().definitions().stream()
                .filter(DataDeclaration.class::isInstance)
                .map(DataDeclaration.class::cast)
                .filter(dataDeclaration -> dataDeclaration.constructor().isPresent())
                .forEach(dataDeclaration -> constructors.put(
                        dataDeclaration.name(),
                        new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                qualifiedModuleName(moduleRef),
                                dataConstructorFunctionName(dataDeclaration.name()),
                                dataDeclaration.name(),
                                qualifiedTypeName(moduleRef, dataDeclaration.name()),
                                false,
                                expressionMayProduceResult(dataDeclaration.constructor().orElseThrow())
                        )
                ));
        module.functional().definitions().stream()
                .filter(TypeDeclaration.class::isInstance)
                .map(TypeDeclaration.class::cast)
                .filter(typeDeclaration -> typeDeclaration.constructor().isPresent())
                .forEach(typeDeclaration -> constructors.put(
                        typeDeclaration.name(),
                        new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                qualifiedModuleName(moduleRef),
                                typeConstructorFunctionName(typeDeclaration.name()),
                                typeDeclaration.name(),
                                qualifiedTypeName(moduleRef, constructorStateTypeName(typeDeclaration.name())),
                                true,
                                expressionMayProduceResult(typeDeclaration.constructor().orElseThrow())
                        )
                ));
        return Map.copyOf(constructors);
    }

    private Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> moduleParentConstructors(
            Module module,
            ModuleRef moduleRef
    ) {
        var typeDeclarations = module.functional().definitions().stream()
                .filter(TypeDeclaration.class::isInstance)
                .map(TypeDeclaration.class::cast)
                .toList();
        var typeDeclarationsByName = typeDeclarations.stream()
                .collect(toMap(TypeDeclaration::name, identity(), (first, second) -> first));
        var typeConstructors = moduleConstructors(module, moduleRef);
        var parentConstructorsBySubtype = new LinkedHashMap<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>>();
        var referencedNestedTypes = typeDeclarations.stream()
                .flatMap(typeDeclaration -> typeDeclaration.subTypes().stream())
                .filter(typeDeclarationsByName::containsKey)
                .collect(java.util.stream.Collectors.toSet());
        var visitedTypes = new HashSet<String>();
        typeDeclarations.stream()
                .filter(typeDeclaration -> !referencedNestedTypes.contains(typeDeclaration.name()))
                .forEach(typeDeclaration -> collectParentConstructorChains(
                        typeDeclaration,
                        typeDeclarationsByName,
                        typeConstructors,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        typeDeclarations.stream()
                .filter(typeDeclaration -> !visitedTypes.contains(typeDeclaration.name()))
                .forEach(typeDeclaration -> collectParentConstructorChains(
                        typeDeclaration,
                        typeDeclarationsByName,
                        typeConstructors,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        return parentConstructorsBySubtype.entrySet().stream()
                .collect(java.util.stream.Collectors.toUnmodifiableMap(
                        Map.Entry::getKey,
                        entry -> List.copyOf(entry.getValue())
                ));
    }

    private Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> libraryConstructors(
            CompiledModule library,
            ModuleRef moduleRef
    ) {
        var constructors = new LinkedHashMap<String, CapybaraExpressionCompiler.ProtectedConstructorRef>();
        library.functions().forEach(function -> constructorTargetTypeName(function.name())
                .ifPresent(descriptor -> constructors.put(
                        descriptor.targetTypeName(),
                        new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                qualifiedModuleName(moduleRef),
                                descriptor.kind() == ConstructorKind.DATA
                                        ? dataConstructorFunctionName(descriptor.targetTypeName())
                                        : typeConstructorFunctionName(descriptor.targetTypeName()),
                                descriptor.targetTypeName(),
                                descriptor.kind() == ConstructorKind.DATA
                                        ? qualifiedTypeName(moduleRef, descriptor.targetTypeName())
                                        : qualifiedTypeName(moduleRef, constructorStateTypeName(descriptor.targetTypeName())),
                                descriptor.kind() == ConstructorKind.TYPE,
                                isResultLikeType(function.returnType(), library.types())
                        )
                )));
        return Map.copyOf(constructors);
    }

    private Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> libraryParentConstructors(
            CompiledModule library,
            ModuleRef moduleRef
    ) {
        var constructors = libraryConstructors(library, moduleRef);
        var parentTypesByName = library.types().values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .collect(toMap(CompiledDataParentType::name, identity(), (first, second) -> first));
        var referencedNestedTypes = parentTypesByName.values().stream()
                .flatMap(parentType -> parentType.subTypes().stream())
                .map(CompiledDataType::name)
                .filter(parentTypesByName::containsKey)
                .collect(java.util.stream.Collectors.toSet());
        var parentConstructorsBySubtype = new LinkedHashMap<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>>();
        var visitedTypes = new HashSet<String>();
        parentTypesByName.values().stream()
                .filter(parentType -> !referencedNestedTypes.contains(parentType.name()))
                .forEach(parentType -> collectLibraryParentConstructorChains(
                        parentType,
                        parentTypesByName,
                        constructors,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        parentTypesByName.values().stream()
                .filter(parentType -> !visitedTypes.contains(parentType.name()))
                .forEach(parentType -> collectLibraryParentConstructorChains(
                        parentType,
                        parentTypesByName,
                        constructors,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        return parentConstructorsBySubtype.entrySet().stream()
                .collect(java.util.stream.Collectors.toUnmodifiableMap(
                        Map.Entry::getKey,
                        entry -> List.copyOf(entry.getValue())
                ));
    }

    private Map<String, String> libraryDataOwners(
            CompiledModule library,
            ModuleRef moduleRef
    ) {
        return library.types().values().stream()
                .filter(CompiledDataType.class::isInstance)
                .map(CompiledDataType.class::cast)
                .collect(java.util.stream.Collectors.toUnmodifiableMap(
                        CompiledDataType::name,
                        ignored -> qualifiedModuleName(moduleRef),
                        (first, second) -> first
                ));
    }

    private CapybaraExpressionCompiler.ConstructorRegistry availableConstructors(
            Module module,
            ModuleLinkIndex moduleLinkIndex,
            ConstructorCatalog constructorCatalog,
            List<ModuleRef> allModules
    ) {
        var moduleRef = new ModuleRef(module.name(), module.path());
        var localConstructors = Optional.ofNullable(getModuleEntry(constructorCatalog.constructorsByModule(), moduleRef)).orElse(Map.of());
        var localParentConstructors = Optional.ofNullable(getModuleEntry(constructorCatalog.parentConstructorsByModule(), moduleRef)).orElse(Map.of());
        var localDataOwners = Optional.ofNullable(getModuleEntry(constructorCatalog.dataOwnersByModule(), moduleRef)).orElse(Map.of());
        var constructors = new LinkedHashMap<String, CapybaraExpressionCompiler.ProtectedConstructorRef>(localConstructors);
        var parentConstructors = new LinkedHashMap<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>>(localParentConstructors);
        var dataOwners = new LinkedHashMap<String, String>(localDataOwners);
        addQualifiedConstructorAliases(constructors, moduleRef, localConstructors);
        addQualifiedParentConstructorAliases(parentConstructors, moduleRef, localParentConstructors);
        addQualifiedDataOwnerAliases(dataOwners, moduleRef, localDataOwners);

        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), moduleLinkIndex);
            if (importedModule == null) {
                continue;
            }
            var importedConstructors = Optional.ofNullable(getModuleEntry(constructorCatalog.constructorsByModule(), importedModule)).orElse(Map.of());
            var importedParentConstructors = Optional.ofNullable(getModuleEntry(constructorCatalog.parentConstructorsByModule(), importedModule)).orElse(Map.of());
            var importedDataOwners = Optional.ofNullable(getModuleEntry(constructorCatalog.dataOwnersByModule(), importedModule)).orElse(Map.of());
            addQualifiedConstructorAliases(constructors, importedModule, importedConstructors);
            addQualifiedParentConstructorAliases(parentConstructors, importedModule, importedParentConstructors);
            addQualifiedDataOwnerAliases(dataOwners, importedModule, importedDataOwners);
            if (importDeclaration.isStarImport() && importDeclaration.excludedSymbols().isEmpty()) {
                importedConstructors.forEach(constructors::put);
                importedParentConstructors.forEach(parentConstructors::put);
                importedDataOwners.forEach(dataOwners::put);
                continue;
            }
            var selected = importDeclaration.selectedSymbols(importedConstructors.keySet());
            for (var symbol : selected) {
                var importedConstructor = importedConstructors.get(symbol);
                if (importedConstructor != null) {
                    constructors.put(symbol, importedConstructor);
                }
                var importedParentConstructor = importedParentConstructors.get(symbol);
                if (importedParentConstructor != null) {
                    parentConstructors.put(symbol, importedParentConstructor);
                }
                var importedDataOwner = importedDataOwners.get(symbol);
                if (importedDataOwner != null) {
                    dataOwners.put(symbol, importedDataOwner);
                }
            }
        }

        allModules.forEach(knownModule -> {
            var knownConstructors = Optional.ofNullable(getModuleEntry(constructorCatalog.constructorsByModule(), knownModule)).orElse(Map.of());
            var knownParentConstructors = Optional.ofNullable(getModuleEntry(constructorCatalog.parentConstructorsByModule(), knownModule)).orElse(Map.of());
            var knownDataOwners = Optional.ofNullable(getModuleEntry(constructorCatalog.dataOwnersByModule(), knownModule)).orElse(Map.of());
            addQualifiedConstructorAliases(constructors, knownModule, knownConstructors);
            addQualifiedParentConstructorAliases(parentConstructors, knownModule, knownParentConstructors);
            addQualifiedDataOwnerAliases(dataOwners, knownModule, knownDataOwners);
        });
        return new CapybaraExpressionCompiler.ConstructorRegistry(Map.copyOf(constructors), Map.copyOf(parentConstructors), Map.copyOf(dataOwners));
    }

    private void registerSourceProtectedConstructors(
            Module module,
            Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> protectedConstructors,
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> parentConstructorsBySubtype
    ) {
        var typeDeclarations = module.functional().definitions().stream()
                .filter(TypeDeclaration.class::isInstance)
                .map(TypeDeclaration.class::cast)
                .toList();
        var typeDeclarationsByName = typeDeclarations.stream()
                .collect(toMap(TypeDeclaration::name, identity(), (first, second) -> first));
        module.functional().definitions().stream()
                .filter(DataDeclaration.class::isInstance)
                .map(DataDeclaration.class::cast)
                .filter(dataDeclaration -> dataDeclaration.constructor().isPresent())
                .forEach(dataDeclaration -> protectedConstructors.put(
                        dataDeclaration.name(),
                        new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                module.name(),
                                dataConstructorFunctionName(dataDeclaration.name()),
                                dataDeclaration.name(),
                                dataDeclaration.name(),
                                false,
                                expressionMayProduceResult(dataDeclaration.constructor().orElseThrow())
                        )
                ));
        typeDeclarations.stream()
                .filter(typeDeclaration -> typeDeclaration.constructor().isPresent())
                .forEach(typeDeclaration -> protectedConstructors.put(
                        typeDeclaration.name(),
                        new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                module.name(),
                                typeConstructorFunctionName(typeDeclaration.name()),
                                typeDeclaration.name(),
                                constructorStateTypeName(typeDeclaration.name()),
                                true,
                                expressionMayProduceResult(typeDeclaration.constructor().orElseThrow())
                        )
                ));
        var referencedNestedTypes = typeDeclarations.stream()
                .flatMap(typeDeclaration -> typeDeclaration.subTypes().stream())
                .filter(typeDeclarationsByName::containsKey)
                .collect(java.util.stream.Collectors.toSet());
        var visitedTypes = new HashSet<String>();
        typeDeclarations.stream()
                .filter(typeDeclaration -> !referencedNestedTypes.contains(typeDeclaration.name()))
                .forEach(typeDeclaration -> collectParentConstructorChains(
                        typeDeclaration,
                        typeDeclarationsByName,
                        protectedConstructors,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        typeDeclarations.stream()
                .filter(typeDeclaration -> !visitedTypes.contains(typeDeclaration.name()))
                .forEach(typeDeclaration -> collectParentConstructorChains(
                        typeDeclaration,
                        typeDeclarationsByName,
                        protectedConstructors,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
    }

    private void registerLibraryProtectedConstructors(
            CompiledModule library,
            SortedMap<String, GenericDataType> linkedTypes,
            Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> protectedConstructors,
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> parentConstructorsBySubtype
    ) {
        var typeConstructorsByName = new HashMap<String, CapybaraExpressionCompiler.ProtectedConstructorRef>();
        for (var function : library.functions()) {
            var constructorTarget = constructorTargetTypeName(function.name());
            if (constructorTarget.isEmpty()) {
                continue;
            }
            var target = constructorTarget.orElseThrow();
            var linkedType = linkedTypes.get(target.targetTypeName());
            if (target.kind() == ConstructorKind.DATA && linkedType instanceof CompiledDataType) {
                protectedConstructors.put(
                        target.targetTypeName(),
                        new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                library.name(),
                                function.name(),
                                target.targetTypeName(),
                                target.targetTypeName(),
                                false,
                                isResultLikeType(function.returnType(), linkedTypes)
                        )
                );
                continue;
            }
            if (target.kind() == ConstructorKind.TYPE && linkedType instanceof CompiledDataParentType) {
                var constructorRef = new CapybaraExpressionCompiler.ProtectedConstructorRef(
                        library.name(),
                        function.name(),
                        target.targetTypeName(),
                        constructorStateTypeName(target.targetTypeName()),
                        true,
                        isResultLikeType(function.returnType(), linkedTypes)
                );
                protectedConstructors.put(target.targetTypeName(), constructorRef);
                typeConstructorsByName.put(target.targetTypeName(), constructorRef);
            }
        }

        var parentTypes = linkedTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .collect(java.util.stream.Collectors.toMap(CompiledDataParentType::name, java.util.function.Function.identity()));
        var referencedNestedTypes = parentTypes.values().stream()
                .flatMap(parentType -> parentType.subTypes().stream())
                .map(CompiledDataType::name)
                .filter(parentTypes::containsKey)
                .collect(java.util.stream.Collectors.toSet());
        var visitedTypes = new HashSet<String>();

        parentTypes.values().stream()
                .filter(parentType -> !referencedNestedTypes.contains(parentType.name()))
                .forEach(parentType -> collectLibraryParentConstructorChains(
                        parentType,
                        parentTypes,
                        typeConstructorsByName,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        parentTypes.values().stream()
                .filter(parentType -> !visitedTypes.contains(parentType.name()))
                .forEach(parentType -> collectLibraryParentConstructorChains(
                        parentType,
                        parentTypes,
                        typeConstructorsByName,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
    }

    private void collectLibraryParentConstructorChains(
            CompiledDataParentType parentType,
            Map<String, CompiledDataParentType> parentTypesByName,
            Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> constructorsByType,
            List<CapybaraExpressionCompiler.ProtectedConstructorRef> activeConstructors,
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> parentConstructorsBySubtype,
            Set<String> visitedTypes
    ) {
        if (!visitedTypes.add(parentType.name())) {
            return;
        }
        var nextActive = new ArrayList<>(activeConstructors);
        var currentConstructor = constructorsByType.get(parentType.name());
        if (currentConstructor != null) {
            nextActive.add(currentConstructor);
        }

        for (var subtype : parentType.subTypes()) {
            if (subtype.name().startsWith(CONSTRUCTOR_STATE_TYPE_PREFIX)) {
                continue;
            }
            var nestedType = parentTypesByName.get(subtype.name());
            if (nestedType != null) {
                collectLibraryParentConstructorChains(
                        nestedType,
                        parentTypesByName,
                        constructorsByType,
                        nextActive,
                        parentConstructorsBySubtype,
                        visitedTypes
                );
                continue;
            }
            if (!nextActive.isEmpty()) {
                mergeParentConstructorChain(parentConstructorsBySubtype, subtype.name(), nextActive);
            }
        }
    }

    private void collectParentConstructorChains(
            TypeDeclaration typeDeclaration,
            Map<String, TypeDeclaration> typeDeclarationsByName,
            Map<String, CapybaraExpressionCompiler.ProtectedConstructorRef> constructorsByType,
            List<CapybaraExpressionCompiler.ProtectedConstructorRef> activeConstructors,
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> parentConstructorsBySubtype,
            Set<String> visitedTypes
    ) {
        if (!visitedTypes.add(typeDeclaration.name())) {
            return;
        }
        var nextActive = new ArrayList<>(activeConstructors);
        var currentConstructor = constructorsByType.get(typeDeclaration.name());
        if (currentConstructor != null && currentConstructor.typeConstructor()) {
            nextActive.add(currentConstructor);
        }
        for (var subtypeName : typeDeclaration.subTypes()) {
            var nestedType = typeDeclarationsByName.get(subtypeName);
            if (nestedType != null) {
                collectParentConstructorChains(
                        nestedType,
                        typeDeclarationsByName,
                        constructorsByType,
                        nextActive,
                        parentConstructorsBySubtype,
                        visitedTypes
                );
                continue;
            }
            if (!nextActive.isEmpty()) {
                mergeParentConstructorChain(parentConstructorsBySubtype, subtypeName, nextActive);
            }
        }
    }

    private void mergeParentConstructorChain(
            Map<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>> parentConstructorsBySubtype,
            String subtypeName,
            List<CapybaraExpressionCompiler.ProtectedConstructorRef> nextActive
    ) {
        parentConstructorsBySubtype.merge(
                subtypeName,
                List.copyOf(nextActive),
                (existing, incoming) -> {
                    var merged = new ArrayList<CapybaraExpressionCompiler.ProtectedConstructorRef>(existing.size() + incoming.size());
                    merged.addAll(existing);
                    incoming.stream()
                            .filter(ref -> !merged.contains(ref))
                            .forEach(merged::add);
                    return List.copyOf(merged);
                }
        );
    }

    private Result<Void> validateResultReturningTypeConstructors(
            Module module,
            List<CompiledFunction> linkedConstructors,
            Map<String, GenericDataType> dataTypes,
            String moduleSourceFile
    ) {
        var typeDeclarations = module.functional().definitions().stream()
                .filter(TypeDeclaration.class::isInstance)
                .map(TypeDeclaration.class::cast)
                .toList();
        var typeDeclarationsByName = typeDeclarations.stream()
                .collect(toMap(TypeDeclaration::name, identity(), (first, second) -> first));
        var typeConstructorRefs = typeDeclarations.stream()
                .filter(typeDeclaration -> typeDeclaration.constructor().isPresent())
                .collect(toMap(
                        TypeDeclaration::name,
                        typeDeclaration -> new CapybaraExpressionCompiler.ProtectedConstructorRef(
                                module.name(),
                                typeConstructorFunctionName(typeDeclaration.name()),
                                typeDeclaration.name(),
                                constructorStateTypeName(typeDeclaration.name()),
                                true,
                                expressionMayProduceResult(typeDeclaration.constructor().orElseThrow())
                        ),
                        (first, second) -> first
                ));
        var resultReturningTypeNames = typeDeclarations.stream()
                .filter(typeDeclaration -> typeDeclaration.constructor().isPresent())
                .filter(typeDeclaration -> expressionMayProduceResult(typeDeclaration.constructor().orElseThrow()))
                .map(TypeDeclaration::name)
                .collect(java.util.stream.Collectors.toSet());
        var parentConstructorsBySubtype = new HashMap<String, List<CapybaraExpressionCompiler.ProtectedConstructorRef>>();
        var referencedNestedTypes = typeDeclarations.stream()
                .flatMap(typeDeclaration -> typeDeclaration.subTypes().stream())
                .filter(typeDeclarationsByName::containsKey)
                .collect(java.util.stream.Collectors.toSet());
        var visitedTypes = new HashSet<String>();
        typeDeclarations.stream()
                .filter(typeDeclaration -> !referencedNestedTypes.contains(typeDeclaration.name()))
                .forEach(typeDeclaration -> collectParentConstructorChains(
                        typeDeclaration,
                        typeDeclarationsByName,
                        typeConstructorRefs,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        typeDeclarations.stream()
                .filter(typeDeclaration -> !visitedTypes.contains(typeDeclaration.name()))
                .forEach(typeDeclaration -> collectParentConstructorChains(
                        typeDeclaration,
                        typeDeclarationsByName,
                        typeConstructorRefs,
                        List.of(),
                        parentConstructorsBySubtype,
                        visitedTypes
                ));
        var linkedConstructorsByName = linkedConstructors.stream()
                .collect(toMap(CompiledFunction::name, identity(), (first, second) -> first));
        for (var dataDeclaration : module.functional().definitions().stream()
                .filter(DataDeclaration.class::isInstance)
                .map(DataDeclaration.class::cast)
                .filter(dataDeclaration -> dataDeclaration.constructor().isPresent())
                .toList()) {
            var parentType = parentConstructorsBySubtype.getOrDefault(dataDeclaration.name(), List.of()).stream()
                    .filter(ref -> resultReturningTypeNames.contains(ref.targetTypeName()))
                    .findFirst();
            if (parentType.isEmpty()) {
                continue;
            }
            var linkedDataConstructor = linkedConstructorsByName.get(dataConstructorFunctionName(dataDeclaration.name()));
            if (linkedDataConstructor != null && !isResultLikeType(linkedDataConstructor.returnType(), dataTypes)) {
                var parentTarget = parentType.orElseThrow().targetTypeName();
                return withPosition(
                        Result.error("Constructor for `" + dataDeclaration.name() + "` must return `Result[" + dataDeclaration.name() + "]` because parent type constructor for `"
                                     + parentTarget + "` returns `Result[" + parentTarget + "]`"),
                        dataDeclaration.position(),
                        normalizeFile(moduleSourceFile)
                );
            }
        }
        return Result.success(null);
    }

    private Result<Void> validateResultReturningTypeConstructors(
            Program program,
            CompiledProgram compiledProgram
    ) {
        var modulesByName = compiledProgram.modules().stream()
                .collect(toMap(CompiledModule::name, identity(), (first, second) -> first));
        for (var module : program.modules()) {
            var compiledModule = modulesByName.get(module.name());
            if (compiledModule == null) {
                continue;
            }
            var validation = validateResultReturningTypeConstructors(
                    module,
                    List.copyOf(compiledModule.functions()),
                    compiledModule.types(),
                    moduleSourceFile(module)
            );
            if (validation instanceof Result.Error<Void> error) {
                return new Result.Error<>(error.errors());
            }
        }
        return Result.success(null);
    }

    private boolean isResultLikeType(CompiledType type, Map<String, GenericDataType> dataTypes) {
        if (type instanceof CompiledDataType dataType) {
            return resultTypeForData(dataType, dataTypes) != null;
        }
        if (!(type instanceof CompiledDataParentType parentType)) {
            return false;
        }
        if ("Result".equals(parentType.name())
            || parentType.subTypes().stream().anyMatch(subType -> "Success".equals(subType.name()) || "Error".equals(subType.name()))) {
            return true;
        }
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(candidate -> candidate.subTypes().stream().anyMatch(subType -> "Success".equals(subType.name()) || "Error".equals(subType.name())))
                .anyMatch(candidate -> sameTypeName(candidate.name(), parentType.name()));
    }

    private boolean expressionMayProduceResult(Expression expression) {
        return switch (expression) {
            case NewData newData -> newData.type() instanceof DataType dataType
                                    && ("Success".equals(dataType.name()) || "Error".equals(dataType.name()));
            case ConstructorData ignored -> false;
            case IfExpression ifExpression ->
                    expressionMayProduceResult(ifExpression.thenBranch()) || expressionMayProduceResult(ifExpression.elseBranch());
            case LetExpression letExpression ->
                    letExpression.kind() == LetExpression.Kind.RESULT_BIND
                    || expressionMayProduceResult(letExpression.value()) || expressionMayProduceResult(letExpression.rest());
            case MatchExpression matchExpression ->
                    matchExpression.cases().stream().anyMatch(matchCase -> expressionMayProduceResult(matchCase.expression()));
            case FieldAccess ignored -> false;
            case FunctionCall ignored -> false;
            case FunctionInvoke ignored -> false;
            case FunctionReference ignored -> false;
            case InfixExpression ignored -> false;
            case IndexExpression ignored -> false;
            case LambdaExpression ignored -> false;
            case ReduceExpression ignored -> false;
            case SliceExpression ignored -> false;
            case NewDictExpression ignored -> false;
            case NewListExpression ignored -> false;
            case NewSetExpression ignored -> false;
            case TupleExpression ignored -> false;
            case Value ignored -> false;
            case BooleanValue ignored -> false;
            case ByteValue ignored -> false;
            case DoubleValue ignored -> false;
            case FloatValue ignored -> false;
            case IntValue ignored -> false;
            case LongValue ignored -> false;
            case NothingValue ignored -> false;
            case PlaceholderExpression ignored -> false;
            case StringValue ignored -> false;
            case WithExpression withExpression -> expressionMayProduceResult(withExpression.source());
        };
    }

    private static String dataConstructorFunctionName(String dataTypeName) {
        return DATA_CONSTRUCTOR_FUNCTION_PREFIX + dataTypeName;
    }

    private static String typeConstructorFunctionName(String typeName) {
        return TYPE_CONSTRUCTOR_FUNCTION_PREFIX + typeName;
    }

    private static String constructorStateTypeName(String typeName) {
        return CONSTRUCTOR_STATE_TYPE_PREFIX + typeName;
    }

    private static CompiledDataType constructorStateType(CompiledDataParentType parentType) {
        return new CompiledDataType(
                constructorStateTypeName(parentType.name()),
                parentType.fields(),
                parentType.typeParameters(),
                List.of(),
                List.of(),
                parentType.visibility(),
                false
        );
    }

    private static Optional<ConstructorDescriptor> constructorTargetTypeName(String functionName) {
        if (functionName.startsWith(DATA_CONSTRUCTOR_FUNCTION_PREFIX)) {
            return Optional.of(new ConstructorDescriptor(
                    ConstructorKind.DATA,
                    functionName.substring(DATA_CONSTRUCTOR_FUNCTION_PREFIX.length())
            ));
        }
        if (functionName.startsWith(TYPE_CONSTRUCTOR_FUNCTION_PREFIX)) {
            return Optional.of(new ConstructorDescriptor(
                    ConstructorKind.TYPE,
                    functionName.substring(TYPE_CONSTRUCTOR_FUNCTION_PREFIX.length())
            ));
        }
        return Optional.empty();
    }

    private Result<List<CompiledFunction>> linkFunctions(
            List<Function> functions,
            Map<String, GenericDataType> dataTypes,
            Set<String> localTypeNames,
            List<CapybaraExpressionCompiler.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            CapybaraExpressionCompiler.ConstructorRegistry protectedConstructorsByType,
            String moduleSourceFile,
            CompileCache compileCache
    ) {
        var linkCache = compileCache.expressionLinkCaches.computeIfAbsent(
                dataTypes,
                CapybaraExpressionCompiler.LinkCache::new
        );
        return functions.stream()
                .map(f -> linkFunction(f, dataTypes, localTypeNames, signatures, signaturesByModule, moduleClassNameByModuleName, protectedConstructorsByType, moduleSourceFile, linkCache, compileCache))
                .collect(new ResultCollectionCollector<>());
    }

    private Result<CompiledFunction> linkFunction(
            Function function,
            Map<String, GenericDataType> dataTypes,
            Set<String> localTypeNames,
            List<CapybaraExpressionCompiler.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            CapybaraExpressionCompiler.ConstructorRegistry protectedConstructorsByType,
            String moduleSourceFile,
            CapybaraExpressionCompiler.LinkCache linkCache,
            CompileCache compileCache
    ) {
        var privateTypeSignatureError = privateTypeEscapingFunctionSignatureError(function, moduleSourceFile);
        if (privateTypeSignatureError.isPresent()) {
            return privateTypeSignatureError.get();
        }
        var localMethodValidationError = validateTypeMethodDeclaredInLocalType(function, localTypeNames, moduleSourceFile);
        if (localMethodValidationError.isPresent()) {
            return localMethodValidationError.get();
        }
        var functionGenericTypeNames = functionGenericTypeNames(function, dataTypes, compileCache);
        var linked = linkParameters(function.parameters(), dataTypes, functionGenericTypeNames, compileCache)
                .flatMap(parameters -> linkExpressionWithRecursiveInference(
                        function,
                        parameters,
                        moduleSourceFile,
                        dataTypes,
                        signatures,
                        signaturesByModule,
                        moduleClassNameByModuleName,
                        protectedConstructorsByType,
                        linkCache
                ).flatMap(ex -> validateConstructorReturnType(function, ex, dataTypes, functionGenericTypeNames, compileCache)
                        .flatMap(validatedExpression -> function.returnType()
                                .map(type -> linkType(type, dataTypes, functionGenericTypeNames, compileCache))
                                .orElseGet(() -> Result.success(validatedExpression.type()))
                                .flatMap(rtype -> validateFunctionReturnType(function, validatedExpression, rtype, dataTypes, moduleSourceFile)
                                        .flatMap(finalExpression -> classifyRecursion(
                                                function,
                                                parameters,
                                                finalExpression,
                                                tailRecursiveSelfCallNames(function.name(), moduleSourceFile, moduleClassNameByModuleName),
                                                moduleSourceFile
                                        ).map(recursion -> new CompiledFunction(
                                                function.name(),
                                                rtype,
                                                parameters,
                                                enrichNothing(finalExpression, function.name(), moduleSourceFile),
                                                function.comments(),
                                                function.visibility(),
                                                isProgramMain(function.name(), rtype, parameters),
                                                recursion.recursive(),
                                                recursion.tailRecursive()
                                        )))))));
        linked = normalizeInfixOperatorErrors(linked, function, moduleSourceFile);
        linked = normalizeMatchExhaustivenessErrors(linked, function, moduleSourceFile);
        linked = normalizeIntLiteralErrors(linked, function, moduleSourceFile);
        linked = normalizeExpectedFoundErrors(linked, function, moduleSourceFile);
        var normalizedFile = normalizeFile(moduleSourceFile);
        var fallbackPosition = returnExpressionPosition(function.expression()).or(() -> function.position());
        linked = withPosition(linked, fallbackPosition, normalizedFile);
        return normalizeReadableFunctionErrors(linked, function, moduleSourceFile);
    }

    private Result<RecursionClassification> classifyRecursion(
            Function function,
            List<CompiledFunctionParameter> parameters,
            CompiledExpression expression,
            Set<String> selfCallNames,
            String moduleSourceFile
    ) {
        var analysis = analyzeTailRecursion(selfCallNames, parameters, expression, true);
        if (methodOwnerType(function.name()).isPresent()) {
            if (function.tailRecursive()) {
                return tailRecursiveFunctionError(
                        function,
                        function.position(),
                        moduleSourceFile,
                        "`fun rec` is supported for functions, not type methods"
                );
            }
            return Result.success(new RecursionClassification(analysis.hasSelfCall(), false));
        }

        if (analysis.firstNonTailCall().isPresent()) {
            if (function.tailRecursive()) {
                return tailRecursiveFunctionError(
                        function,
                        function.position(),
                        moduleSourceFile,
                        "Recursive call to `" + displayFunctionName(function.name()) + "` is not in tail position"
                );
            }
            return Result.success(new RecursionClassification(true, false));
        }
        if (!analysis.hasSelfCall()) {
            if (function.tailRecursive()) {
                return tailRecursiveFunctionError(
                        function,
                        function.position(),
                        moduleSourceFile,
                        "`fun rec` function `" + displayFunctionName(function.name()) + "` must call itself in tail position"
                );
            }
            return Result.success(new RecursionClassification(false, false));
        }
        return Result.success(new RecursionClassification(true, true));
    }

    private record RecursionClassification(boolean recursive, boolean tailRecursive) {
    }

    private <T> Result<T> tailRecursiveFunctionError(
            Function function,
            Optional<SourcePosition> position,
            String moduleSourceFile,
            String details
    ) {
        var sourcePosition = position.or(() -> function.position()).orElse(SourcePosition.EMPTY);
        var line = Math.max(sourcePosition.line(), 1);
        var column = Math.max(sourcePosition.column(), 0);
        var file = normalizeFile(moduleSourceFile);
        var functionLine = function.position().map(SourcePosition::line).orElse(line);
        var functionPreview = functionLine == line
                ? formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()))
                : formatFunctionPreviewUpToLine(function, line);
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + functionPreview + "\n"
                      + " ".repeat(Math.max(column, 0)) + "^ " + details + "\n";
        var error = new Result.Error.SingleError(line, column, file, message);
        return new Result.Error<>(List.of(error));
    }

    private TailRecursionAnalysis analyzeTailRecursion(
            Set<String> selfCallNames,
            List<CompiledFunctionParameter> parameters,
            CompiledExpression expression,
            boolean tailPosition
    ) {
        return switch (expression) {
            case CompiledFunctionCall functionCall -> {
                var result = TailRecursionAnalysis.empty();
                var selfCall = isDirectSelfCall(selfCallNames, parameters, functionCall);
                if (selfCall) {
                    result = tailPosition
                            ? TailRecursionAnalysis.tailCall()
                            : TailRecursionAnalysis.nonTailCall(functionCall);
                }
                yield merge(result, analyzeAll(selfCallNames, parameters, functionCall.arguments(), false));
            }
            case CompiledIfExpression ifExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, ifExpression.condition(), false),
                    analyzeTailRecursion(selfCallNames, parameters, ifExpression.thenBranch(), tailPosition),
                    analyzeTailRecursion(selfCallNames, parameters, ifExpression.elseBranch(), tailPosition)
            );
            case CompiledLetExpression letExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, letExpression.value(), false),
                    analyzeTailRecursion(selfCallNames, parameters, letExpression.rest(), tailPosition)
            );
            case CompiledMatchExpression matchExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, matchExpression.matchWith(), false),
                    merge(matchExpression.cases().stream()
                            .map(matchCase -> merge(
                                    matchCase.guard()
                                            .map(guard -> analyzeTailRecursion(selfCallNames, parameters, guard, false))
                                            .orElseGet(TailRecursionAnalysis::empty),
                                    analyzeTailRecursion(selfCallNames, parameters, matchCase.expression(), tailPosition)
                            ))
                            .toList())
            );
            case CompiledFunctionInvoke functionInvoke -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, functionInvoke.function(), false),
                    analyzeAll(selfCallNames, parameters, functionInvoke.arguments(), false)
            );
            case CompiledInfixExpression infixExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, infixExpression.left(), false),
                    analyzeTailRecursion(selfCallNames, parameters, infixExpression.right(), false)
            );
            case CompiledFieldAccess fieldAccess ->
                    analyzeTailRecursion(selfCallNames, parameters, fieldAccess.source(), false);
            case CompiledIndexExpression indexExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, indexExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, indexExpression.index(), false)
            );
            case CompiledSliceExpression sliceExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, sliceExpression.source(), false),
                    sliceExpression.start()
                            .map(start -> analyzeTailRecursion(selfCallNames, parameters, start, false))
                            .orElseGet(TailRecursionAnalysis::empty),
                    sliceExpression.end()
                            .map(end -> analyzeTailRecursion(selfCallNames, parameters, end, false))
                            .orElseGet(TailRecursionAnalysis::empty)
            );
            case CompiledLambdaExpression lambdaExpression ->
                    analyzeTailRecursion(selfCallNames, parameters, lambdaExpression.expression(), false);
            case CompiledEffectExpression effectExpression ->
                    analyzeTailRecursion(selfCallNames, parameters, effectExpression.body(), false);
            case CompiledEffectBindExpression effectBindExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, effectBindExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, effectBindExpression.rest(), false)
            );
            case CompiledPipeExpression pipeExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, pipeExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeExpression.mapper(), false)
            );
            case CompiledPipeFlatMapExpression pipeFlatMapExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, pipeFlatMapExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeFlatMapExpression.mapper(), false)
            );
            case CompiledPipeFilterOutExpression pipeFilterOutExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, pipeFilterOutExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeFilterOutExpression.predicate(), false)
            );
            case CompiledPipeAllExpression pipeAllExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, pipeAllExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeAllExpression.predicate(), false)
            );
            case CompiledPipeAnyExpression pipeAnyExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, pipeAnyExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeAnyExpression.predicate(), false)
            );
            case CompiledPipeReduceExpression pipeReduceExpression -> merge(
                    analyzeTailRecursion(selfCallNames, parameters, pipeReduceExpression.source(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeReduceExpression.initialValue(), false),
                    analyzeTailRecursion(selfCallNames, parameters, pipeReduceExpression.reducerExpression(), false)
            );
            case CompiledReflectionValue reflectionValue ->
                    analyzeTailRecursion(selfCallNames, parameters, reflectionValue.target(), false);
            case CompiledNumericWidening numericWidening ->
                    analyzeTailRecursion(selfCallNames, parameters, numericWidening.expression(), false);
            case CompiledNewData newData -> analyzeAll(
                    selfCallNames,
                    parameters,
                    newData.assignments().stream().map(CompiledNewData.FieldAssignment::value).toList(),
                    false
            );
            case CompiledNewList newList -> analyzeAll(selfCallNames, parameters, newList.values(), false);
            case CompiledNewSet newSet -> analyzeAll(selfCallNames, parameters, newSet.values(), false);
            case CompiledNewDict newDict -> merge(newDict.entries().stream()
                    .map(entry -> merge(
                            analyzeTailRecursion(selfCallNames, parameters, entry.key(), false),
                            analyzeTailRecursion(selfCallNames, parameters, entry.value(), false)
                    ))
                    .toList());
            case CompiledTupleExpression tupleExpression ->
                    analyzeAll(selfCallNames, parameters, tupleExpression.values(), false);
            case CompiledBooleanValue ignored -> TailRecursionAnalysis.empty();
            case CompiledByteValue ignored -> TailRecursionAnalysis.empty();
            case CompiledDoubleValue ignored -> TailRecursionAnalysis.empty();
            case CompiledFloatValue ignored -> TailRecursionAnalysis.empty();
            case CompiledIntValue ignored -> TailRecursionAnalysis.empty();
            case CompiledLongValue ignored -> TailRecursionAnalysis.empty();
            case CompiledNothingValue ignored -> TailRecursionAnalysis.empty();
            case CompiledStringValue ignored -> TailRecursionAnalysis.empty();
            case CompiledVariable ignored -> TailRecursionAnalysis.empty();
        };
    }

    private TailRecursionAnalysis analyzeAll(
            Set<String> selfCallNames,
            List<CompiledFunctionParameter> parameters,
            List<CompiledExpression> expressions,
            boolean tailPosition
    ) {
        return merge(expressions.stream()
                .map(expression -> analyzeTailRecursion(selfCallNames, parameters, expression, tailPosition))
                .toList());
    }

    private boolean isDirectSelfCall(
            Set<String> selfCallNames,
            List<CompiledFunctionParameter> parameters,
            CompiledFunctionCall functionCall
    ) {
        if (!selfCallNames.contains(functionCall.name()) || functionCall.arguments().size() != parameters.size()) {
            return false;
        }
        for (int i = 0; i < parameters.size(); i++) {
            if (!functionCall.arguments().get(i).type().equals(parameters.get(i).type())) {
                return false;
            }
        }
        return true;
    }

    private Set<String> tailRecursiveSelfCallNames(
            String functionName,
            String moduleSourceFile,
            Map<String, String> moduleClassNameByModuleName
    ) {
        var names = new LinkedHashSet<String>();
        names.add(functionName);
        var qualifiedModuleName = qualifiedModuleNameFromSourceFile(moduleSourceFile);
        if (qualifiedModuleName.startsWith("/")) {
            qualifiedModuleName = qualifiedModuleName.substring(1);
        }
        names.add(qualifiedModuleName + "." + functionName);
        names.add(qualifiedModuleName.replace("/", ".") + "." + functionName);
        names.add("." + qualifiedModuleName.replace("/", ".") + "." + functionName);
        var lastSlash = qualifiedModuleName.lastIndexOf('/');
        if (lastSlash >= 0 && lastSlash + 1 < qualifiedModuleName.length()) {
            names.add(qualifiedModuleName.substring(lastSlash + 1) + "." + functionName);
        }
        var className = moduleClassNameByModuleName.get(qualifiedModuleName);
        if (className != null) {
            names.add(className + "." + functionName);
            if (className.startsWith(".")) {
                names.add(className.substring(1) + "." + functionName);
            }
        }
        return Set.copyOf(names);
    }

    private TailRecursionAnalysis merge(TailRecursionAnalysis... analyses) {
        return merge(List.of(analyses));
    }

    private TailRecursionAnalysis merge(List<TailRecursionAnalysis> analyses) {
        var hasSelfCall = false;
        Optional<CompiledFunctionCall> firstNonTailCall = Optional.empty();
        for (var analysis : analyses) {
            hasSelfCall = hasSelfCall || analysis.hasSelfCall();
            if (firstNonTailCall.isEmpty()) {
                firstNonTailCall = analysis.firstNonTailCall();
            }
        }
        return new TailRecursionAnalysis(hasSelfCall, firstNonTailCall);
    }

    private record TailRecursionAnalysis(boolean hasSelfCall, Optional<CompiledFunctionCall> firstNonTailCall) {
        private static TailRecursionAnalysis empty() {
            return new TailRecursionAnalysis(false, Optional.empty());
        }

        private static TailRecursionAnalysis tailCall() {
            return new TailRecursionAnalysis(true, Optional.empty());
        }

        private static TailRecursionAnalysis nonTailCall(CompiledFunctionCall call) {
            return new TailRecursionAnalysis(true, Optional.of(call));
        }
    }

    private Result<dev.capylang.compiler.expression.CompiledExpression> validateConstructorReturnType(
            Function function,
            dev.capylang.compiler.expression.CompiledExpression expression,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames,
            CompileCache compileCache
    ) {
        var constructorTarget = constructorTargetTypeName(function.name());
        if (constructorTarget.isEmpty()) {
            return Result.success(expression);
        }
        var target = constructorTarget.orElseThrow();
        var internalTargetName = target.kind() == ConstructorKind.TYPE
                ? constructorStateTypeName(target.targetTypeName())
                : target.targetTypeName();
        var linkedTarget = linkType(new DataType(internalTargetName), dataTypes, functionGenericTypeNames, compileCache);
        if (linkedTarget instanceof Result.Error<CompiledType> error) {
            return new Result.Error<>(error.errors());
        }
        var targetType = ((Result.Success<CompiledType>) linkedTarget).value();
        if (!(targetType instanceof CompiledDataType dataType)) {
            return Result.success(expression);
        }
        if (expression.type().equals(dataType)) {
            return Result.success(expression);
        }
        var resultType = resultTypeForData(dataType, dataTypes);
        if (resultType != null && expression.type().equals(resultType)) {
            return Result.success(expression);
        }
        var userVisibleName = target.targetTypeName();
        return withPosition(
                Result.error("Constructor for `" + userVisibleName + "` must return `" + userVisibleName + "` or `Result[" + userVisibleName + "]`, but got `" + expression.type() + "`"),
                returnExpressionPosition(function.expression()).or(() -> function.position()),
                ""
        );
    }

    private Result<dev.capylang.compiler.expression.CompiledExpression> linkExpressionWithRecursiveInference(
            Function function,
            List<CompiledFunctionParameter> parameters,
            String moduleSourceFile,
            Map<String, GenericDataType> dataTypes,
            List<CapybaraExpressionCompiler.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionCompiler.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            CapybaraExpressionCompiler.ConstructorRegistry protectedConstructorsByType,
            CapybaraExpressionCompiler.LinkCache linkCache
    ) {
        var linker = new CapybaraExpressionCompiler(
                parameters,
                dataTypes,
                signatures,
                signaturesByModule,
                moduleClassNameByModuleName,
                protectedConstructorsByType,
                linkCache,
                Optional.of(qualifiedModuleNameFromSourceFile(moduleSourceFile)),
                constructorTargetTypeName(function.name())
                        .map(target -> target.kind() == ConstructorKind.TYPE
                                ? constructorStateTypeName(target.targetTypeName())
                                : target.targetTypeName())
        );
        var linkedDeclaredReturnType = function.returnType().isPresent()
                ? signatures.stream()
                        .filter(signature -> signature.name().equals(function.name()))
                        .filter(signature -> signature.parameterTypes().size() == parameters.size())
                        .filter(signature -> {
                            for (var i = 0; i < parameters.size(); i++) {
                                if (!signature.parameterTypes().get(i).equals(parameters.get(i).type())) {
                                    return false;
                                }
                            }
                            return true;
                        })
                        .map(CapybaraExpressionCompiler.FunctionSignature::returnType)
                        .findFirst()
                : Optional.<CompiledType>empty();
        var shouldLinkForExpectedType = linkedDeclaredReturnType.isPresent();
        var linkedExpression = shouldLinkForExpectedType
                ? linker.linkExpressionForExpectedType(function.expression(), linkedDeclaredReturnType.orElseThrow())
                : linker.linkExpression(function.expression());
        return linkedExpression.flatMap(expression -> {
            if (function.returnType().isPresent()
                || expression.type() != PrimitiveLinkedType.ANY
                || !function.name().contains("__local_fun_")) {
                return Result.success(expression);
            }
            var selfSignatureAsNothing = signatures.stream()
                    .map(signature -> signature.name().equals(function.name())
                            ? new CapybaraExpressionCompiler.FunctionSignature(
                            signature.name(),
                            signature.parameterTypes(),
                            signature.parameterNames(),
                            PrimitiveLinkedType.NOTHING,
                            signature.visibility()
                    )
                            : signature)
                    .toList();
            var retryLinker = new CapybaraExpressionCompiler(
                    parameters,
                    dataTypes,
                    selfSignatureAsNothing,
                    signaturesByModule,
                    moduleClassNameByModuleName,
                    protectedConstructorsByType,
                    linkCache,
                    Optional.of(qualifiedModuleNameFromSourceFile(moduleSourceFile)),
                    constructorTargetTypeName(function.name())
                            .map(target -> target.kind() == ConstructorKind.TYPE
                                    ? constructorStateTypeName(target.targetTypeName())
                                    : target.targetTypeName())
            );
            return retryLinker.linkExpression(function.expression()).map(retry ->
                    retry.type() != PrimitiveLinkedType.ANY ? retry : expression
            );
        });
    }
    private Optional<Result<CompiledFunction>> validateTypeMethodDeclaredInLocalType(
            Function function,
            Set<String> localTypeNames,
            String moduleSourceFile
    ) {
        var ownerType = methodOwnerType(function.name());
        if (ownerType.isEmpty() || localTypeNames.contains(ownerType.get())) {
            return Optional.empty();
        }
        var normalizedFile = normalizeFile(moduleSourceFile);
        var methodLine = methodDeclarationErrorLine(function);
        var methodColumn = methodDeclarationErrorColumn(function);
        var position = new SourcePosition(methodLine, methodColumn, Optional.empty());
        var functionPreview = formatFunctionHeader(function) + " =";
        var pointerIndent = methodDeclarationPointerIndent(function, functionPreview);
        var pointer = " ".repeat(Math.max(pointerIndent, 0))
                      + "^ Cannot declare method on external type `" + ownerType.get()
                      + "`. Type methods/infix operators must be declared in the module where the type is defined.";
        var message = "error: mismatched types\n"
                      + " --> " + normalizedFile + ":" + position.line() + ":" + position.column() + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        var error = new Result.Error.SingleError(
                position.line(),
                position.column(),
                normalizedFile,
                message
        );
        return Optional.of(new Result.Error<>(List.of(error)));
    }

    private Optional<String> methodOwnerType(String functionName) {
        if (!functionName.startsWith(METHOD_DECL_PREFIX)) {
            return Optional.empty();
        }
        var separatorIndex = functionName.indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0) {
            return Optional.empty();
        }
        return Optional.of(functionName.substring(METHOD_DECL_PREFIX.length(), separatorIndex));
    }


    private Optional<String> methodSimpleName(String functionName) {
        if (!functionName.startsWith(METHOD_DECL_PREFIX)) {
            return Optional.empty();
        }
        var separatorIndex = functionName.indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0 || separatorIndex + 2 > functionName.length()) {
            return Optional.empty();
        }
        return Optional.of(functionName.substring(separatorIndex + 2));
    }

    private Result<CompiledFunction> normalizeInfixOperatorErrors(
            Result<CompiledFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof Result.Error<CompiledFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeInfixOperatorError(singleError, function, moduleSourceFile))
                .toList();
        return new Result.Error<>(transformed);
    }

    private Result.Error.SingleError normalizeInfixOperatorError(
            Result.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        var pattern = java.util.regex.Pattern.compile("Cannot apply `([^`]+)` to `([^`]+)` and `([^`]+)`");
        var matcher = pattern.matcher(error.message());
        if (!matcher.matches()) {
            return error;
        }
        var operator = matcher.group(1);
        var leftType = normalizeReportedTypeName(matcher.group(2));
        var rightType = normalizeReportedTypeName(matcher.group(3));
        var line = Math.max(error.line(), 1);
        var column = Math.max(error.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()));
        var pointer = " ".repeat(Math.max(column, 0))
                      + "^ `" + operator + "` operator is not defined for `" + leftType + " " + operator + " " + rightType + "`";
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new Result.Error.SingleError(line, column, file, message);
    }
    private Result<CompiledFunction> normalizeMatchExhaustivenessErrors(
            Result<CompiledFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof Result.Error<CompiledFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeMatchExhaustivenessError(singleError, function, moduleSourceFile))
                .toList();
        return new Result.Error<>(transformed);
    }

    private Result<CompiledFunction> normalizeIntLiteralErrors(
            Result<CompiledFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof Result.Error<CompiledFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeIntLiteralError(singleError, function, moduleSourceFile))
                .toList();
        return new Result.Error<>(transformed);
    }

    private Result<CompiledFunction> normalizeExpectedFoundErrors(
            Result<CompiledFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof Result.Error<CompiledFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeExpectedFoundError(singleError, function, moduleSourceFile))
                .toList();
        return new Result.Error<>(transformed);
    }

    private Result<CompiledFunction> normalizeReadableFunctionErrors(
            Result<CompiledFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof Result.Error<CompiledFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeReadableFunctionError(singleError, function, moduleSourceFile))
                .toList();
        return new Result.Error<>(transformed);
    }

    private Result.Error.SingleError normalizeReadableFunctionError(
            Result.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        if (error.message().startsWith("error: ")) {
            return error;
        }
        var fallbackLine = function.position().map(SourcePosition::line).orElse(1);
        var fallbackColumn = function.position().map(SourcePosition::column).orElse(0);
        var line = Math.max(error.line(), fallbackLine);
        var column = error.column() > 0 ? error.column() : fallbackColumn;
        var file = error.file().isBlank() ? normalizeFile(moduleSourceFile) : error.file();
        var functionLine = function.position().map(SourcePosition::line).orElse(line);
        var functionPreview = functionLine == line
                ? formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()))
                : formatFunctionPreviewUpToLine(function, line);
        var pointer = " ".repeat(Math.max(column, 0)) + "^ " + normalizeUserVisibleNames(error.message());
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new Result.Error.SingleError(line, column, file, message);
    }

    private Result.Error.SingleError normalizeExpectedFoundError(
            Result.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        if (!error.message().matches("^Expected `[^`]+`, (but )?got `[^`]+`$")) {
            return error;
        }
        var line = Math.max(error.line(), 1);
        var column = Math.max(error.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var functionLine = function.position().map(SourcePosition::line).orElse(line);
        var functionPreview = functionLine == line
                ? formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()))
                : formatFunctionPreviewUpToLine(function, line);
        var pointer = " ".repeat(Math.max(column, 0)) + "^ " + normalizeUserVisibleNames(error.message());
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new Result.Error.SingleError(line, column, file, message);
    }

    private String formatFunctionPreviewUpToLine(Function function, int line) {
        var full = formatFunctionHeader(function) + " =\n" + formatMultilineExpression(function.expression(), 4);
        var startLine = function.position().map(SourcePosition::line).orElse(line);
        var lines = full.split("\n", -1);
        var maxLines = Math.max(1, line - startLine + 1);
        return java.util.Arrays.stream(lines)
                .limit(Math.min(maxLines, lines.length))
                .collect(java.util.stream.Collectors.joining("\n"));
    }

    private Result.Error.SingleError normalizeIntLiteralError(
            Result.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        if (!error.message().startsWith("Int literal out of range:")
            && !error.message().startsWith("Invalid int literal:")) {
            return error;
        }
        var line = Math.max(error.line(), 1);
        var messageColumn = Math.max(error.column(), 1);
        var reportedColumn = messageColumn + 4;
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()));
        var pointer = " ".repeat(Math.max(messageColumn, 0)) + "^ " + error.message();
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + messageColumn + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new Result.Error.SingleError(line, reportedColumn, file, message);
    }

    private Result.Error.SingleError normalizeMatchExhaustivenessError(
            Result.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        if (!error.message().startsWith("`match` is not exhaustive.")) {
            return error;
        }
        var line = Math.max(error.line(), 1);
        var column = Math.max(error.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var header = formatFunctionHeader(function) + " =";
        var matchLine = "    " + formatMatchLine(function.expression());
        var pointer = " ".repeat(Math.max(column, 0)) + "^ " + normalizeUserVisibleNames(error.message());
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + header + "\n"
                      + matchLine + "\n"
                      + pointer + "\n";
        return new Result.Error.SingleError(line, column, file, message);
    }

    private Result<dev.capylang.compiler.expression.CompiledExpression> validateFunctionReturnType(
            Function function,
            dev.capylang.compiler.expression.CompiledExpression expression,
            CompiledType declaredReturnType,
            Map<String, GenericDataType> dataTypes,
            String moduleSourceFile
    ) {
        var branchMismatch = firstConcreteReturnMismatch(expression, declaredReturnType, dataTypes);
        if (branchMismatch.isPresent()) {
            return returnTypeMismatchError(function, function.expression(), declaredReturnType, branchMismatch.orElseThrow(), moduleSourceFile);
        }
        var coercedExpression = coerceReturnExpression(expression, declaredReturnType, dataTypes);
        if (isAssignableReturnType(declaredReturnType, coercedExpression.type(), dataTypes)) {
            return Result.success(coercedExpression);
        }
        return returnTypeMismatchError(function, terminalReturnExpression(function.expression()), declaredReturnType, coercedExpression.type(), moduleSourceFile);
    }

    private Result<dev.capylang.compiler.expression.CompiledExpression> returnTypeMismatchError(
            Function function,
            Expression returnExpression,
            CompiledType declaredReturnType,
            CompiledType actualReturnType,
            String moduleSourceFile
    ) {
        var position = returnExpressionPosition(returnExpression)
                .or(() -> returnExpression.position())
                .or(() -> function.position())
                .orElse(SourcePosition.EMPTY);
        var line = Math.max(position.line(), 1);
        var column = Math.max(position.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = formatReturnTypeMismatchSourceLine(function, returnExpression, position, declaredReturnType);
        var pointer = " ".repeat(Math.max(column, 0))
                      + "^ expected `" + formatLinkedType(declaredReturnType)
                      + "`, found `" + formatLinkedType(actualReturnType) + "`";
        return Result.error(
                "error: mismatched types\n"
                + " --> " + file + ":" + line + ":" + column + "\n"
                + functionPreview + "\n"
                + pointer + "\n"
        );
    }
    private String formatReturnTypeMismatchSourceLine(
            Function function,
            Expression returnExpression,
            SourcePosition position,
            CompiledType declaredReturnType
    ) {
        var expressionPosition = function.expression().position().orElse(SourcePosition.EMPTY);
        if (function.expression() instanceof LetExpression && expressionPosition.line() != position.line()) {
            return formatMultilineFunctionPreview(function, declaredReturnType);
        }
        return functionKeyword(function) + " " + displayFunctionName(function.name())
               + "(" + function.parameters().stream()
                       .map(parameter -> parameter.name() + ": " + formatParserTypeInHeader(parameter.type()))
                       .collect(java.util.stream.Collectors.joining(", "))
               + "): " + formatLinkedType(declaredReturnType)
               + " = " + formatExpressionPreview(function.expression());
    }

    private String formatFunctionHeaderAndExpression(Function function, String expressionPreview) {
        return formatFunctionHeader(function) + " = " + expressionPreview;
    }

    private String formatFunctionHeader(Function function) {
        var methodDeclaration = methodDeclarationInfo(function);
        var methodParameters = methodDeclaration
                .map(ignored -> function.parameters().stream().skip(1).toList())
                .orElse(function.parameters());
        var methodHeaderName = methodDeclaration
                .map(this::formatMethodDeclarationName)
                .orElse(displayFunctionName(function.name()));
        var header = new StringBuilder(functionKeyword(function))
                .append(" ")
                .append(methodHeaderName)
                .append("(")
                .append(methodParameters.stream()
                        .map(parameter -> parameter.name() + ": " + formatParserTypeInHeader(parameter.type()))
                        .collect(java.util.stream.Collectors.joining(", ")))
                .append(")");
        function.returnType().ifPresent(type -> header.append(": ").append(formatParserTypeInHeader(type)));
        return header.toString();
    }

    private String functionKeyword(Function function) {
        return function.tailRecursive() ? "fun rec" : "fun";
    }

    private int methodDeclarationErrorLine(Function function) {
        if (function.expression() instanceof MatchExpression matchExpression && !matchExpression.cases().isEmpty()) {
            return matchExpression.cases().getFirst().expression().position()
                    .map(SourcePosition::line)
                    .orElseGet(() -> function.position().map(SourcePosition::line).orElse(1));
        }
        return function.position().map(SourcePosition::line).orElse(1);
    }

    private int methodDeclarationErrorColumn(Function function) {
        var methodDeclaration = methodDeclarationInfo(function);
        if (methodDeclaration.isEmpty()) {
            return function.position().map(SourcePosition::column).orElse(1);
        }
        var header = formatFunctionHeader(function);
        var declaration = methodDeclaration.get();
        if (IDENTIFIER_PATTERN.matcher(declaration.methodName()).matches()) {
            var idx = header.indexOf("." + declaration.methodName());
            if (idx >= 0) {
                return idx + 3;
            }
        } else {
            var wrappedLiteral = "`" + declaration.methodName() + "`";
            var idx = header.indexOf(wrappedLiteral);
            if (idx >= 0) {
                return idx + 1;
            }
        }
        return function.position().map(SourcePosition::column).orElse(1);
    }

    private int methodDeclarationPointerIndent(Function function, String functionPreview) {
        var methodDeclaration = methodDeclarationInfo(function);
        if (methodDeclaration.isEmpty()) {
            return Math.max(function.position().map(SourcePosition::column).orElse(1) - 1, 0);
        }
        var methodName = methodDeclaration.get().methodName();
        var idx = functionPreview.indexOf(methodName);
        if (idx >= 0) {
            return idx;
        }
        return Math.max(function.position().map(SourcePosition::column).orElse(1) - 1, 0);
    }

    private String formatMethodDeclarationName(MethodDeclarationInfo methodDeclarationInfo) {
        var owner = methodDeclarationInfo.ownerName();
        var methodName = methodDeclarationInfo.methodName();
        var methodDisplay = IDENTIFIER_PATTERN.matcher(methodName).matches()
                ? methodName
                : "`" + methodName + "`";
        return owner + "." + methodDisplay;
    }

    private Optional<MethodDeclarationInfo> methodDeclarationInfo(Function function) {
        if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
            return Optional.empty();
        }
        var separatorIndex = function.name().indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0 || separatorIndex + 2 > function.name().length()) {
            return Optional.empty();
        }
        var ownerFromName = function.name().substring(METHOD_DECL_PREFIX.length(), separatorIndex);
        var methodName = function.name().substring(separatorIndex + 2);
        var ownerWithTypeParameters = function.parameters().stream()
                .findFirst()
                .map(Parameter::type)
                .map(this::formatParserTypeInHeader)
                .orElse(ownerFromName);
        return Optional.of(new MethodDeclarationInfo(ownerWithTypeParameters, methodName));
    }

    private String formatParserTypeInHeader(Type type) {
        return switch (type) {
            case FunctionType functionType -> formatParserTypeInHeader(functionType.argumentType())
                                              + " => "
                                              + formatParserTypeInHeader(functionType.returnType());
            default -> formatParserType(restorePrivateTypeNameForDisplay(type));
        };
    }

    private record MethodDeclarationInfo(String ownerName, String methodName) {
    }

    private String formatMatchLine(Expression expression) {
        if (expression instanceof MatchExpression matchExpression) {
            return "match " + formatExpressionPreview(matchExpression.matchWith()) + " with";
        }
        return formatExpressionPreview(expression);
    }

    private String formatMultilineFunctionPreview(Function function, CompiledType declaredReturnType) {
        var builder = new StringBuilder();
        builder.append("  ")
                .append(functionKeyword(function))
                .append(" ")
                .append(displayFunctionName(function.name()))
                .append("(")
                .append(function.parameters().stream()
                        .map(parameter -> parameter.name() + ": " + formatParserTypeInHeader(parameter.type()))
                        .collect(java.util.stream.Collectors.joining(", ")))
                .append("): ")
                .append(formatLinkedType(declaredReturnType))
                .append(" =\n");
        builder.append(formatMultilineExpression(function.expression(), 4));
        return builder.toString();
    }

    private String formatMultilineExpression(Expression expression, int indent) {
        return switch (expression) {
            case LetExpression letExpression -> {
                var builder = new StringBuilder();
                builder.append(" ".repeat(indent))
                        .append("let ")
                        .append(letExpression.name());
                letExpression.declaredType().ifPresent(type -> builder.append(": ").append(formatParserType(type)));
                builder.append(letExpression.kind() == LetExpression.Kind.RESULT_BIND ? " <- " : " = ");
                if (isMultilinePreviewExpression(letExpression.value())) {
                    builder.append(formatExpressionMultilineForLetValue(letExpression.value(), indent));
                } else {
                    builder.append(formatExpressionPreview(letExpression.value()));
                }
                builder.append('\n').append(formatMultilineExpression(letExpression.rest(), indent));
                yield builder.toString();
            }
            case NewData newData -> formatNewDataMultiline(newData, indent);
            default -> " ".repeat(indent) + formatExpressionPreview(expression);
        };
    }

    private boolean isMultilinePreviewExpression(Expression expression) {
        return expression instanceof NewData;
    }

    private String formatExpressionMultilineForLetValue(Expression expression, int indent) {
        return switch (expression) {
            case NewData newData -> formatNewDataInlineInLet(newData, indent);
            default -> formatExpressionPreview(expression);
        };
    }

    private String formatExpressionMultilineBlock(Expression expression, int indent) {
        return switch (expression) {
            case NewData newData -> formatNewDataMultiline(newData, indent);
            default -> " ".repeat(indent) + formatExpressionPreview(expression);
        };
    }

    private String formatNewDataInlineInLet(NewData newData, int indent) {
        var typeName = formatParserType(newData.type());
        var builder = new StringBuilder();
        builder.append(typeName).append(" {");
        if (!newData.positionalArguments().isEmpty() && newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.positionalArguments().size(); i++) {
                builder.append(" ".repeat(indent + 4))
                        .append(formatExpressionPreview(newData.positionalArguments().get(i)));
                if (i < newData.positionalArguments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        if (!newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.assignments().size(); i++) {
                var assignment = newData.assignments().get(i);
                var assignmentValue = formatAssignmentValue(assignment.value(), indent + 4);
                builder.append(" ".repeat(indent + 4))
                        .append(assignment.name())
                        .append(": ")
                        .append(assignmentValue);
                if (i < newData.assignments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        builder.append("}");
        return builder.toString();
    }

    private String formatAssignmentValue(Expression expression, int indent) {
        return switch (expression) {
            case NewDictExpression newDictExpression -> formatNewDictMultiline(newDictExpression, indent);
            case NewData newData -> formatNewDataSingleLine(newData);
            default -> formatExpressionPreview(expression);
        };
    }

    private String formatNewDictMultiline(NewDictExpression expression, int indent) {
        var builder = new StringBuilder("{");
        if (!expression.entries().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < expression.entries().size(); i++) {
                var entry = expression.entries().get(i);
                builder.append(" ".repeat(indent + 4))
                        .append(formatExpressionPreview(entry.key()))
                        .append(": ")
                        .append(formatAssignmentValue(entry.value(), indent + 4));
                if (i < expression.entries().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent));
        }
        builder.append("}");
        return builder.toString();
    }

    private String formatNewDataSingleLine(NewData newData) {
        var typeName = formatParserType(newData.type());
        if (!newData.positionalArguments().isEmpty() && newData.assignments().isEmpty()) {
            return typeName + " { "
                   + newData.positionalArguments().stream()
                           .map(this::formatExpressionPreview)
                           .collect(java.util.stream.Collectors.joining(", "))
                   + " }";
        }
        if (!newData.assignments().isEmpty()) {
            return typeName + " { "
                   + newData.assignments().stream()
                           .map(assignment -> assignment.name() + ": " + formatExpressionPreview(assignment.value()))
                           .collect(java.util.stream.Collectors.joining(", "))
                   + " }";
        }
        return typeName + " {}";
    }

    private String formatNewDataMultiline(NewData newData, int indent) {
        var typeName = formatParserType(newData.type());
        var builder = new StringBuilder();
        builder.append(" ".repeat(indent)).append(typeName).append(" {");
        if (!newData.positionalArguments().isEmpty() && newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.positionalArguments().size(); i++) {
                builder.append(" ".repeat(indent + 4))
                        .append(formatExpressionPreview(newData.positionalArguments().get(i)));
                if (i < newData.positionalArguments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        if (!newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.assignments().size(); i++) {
                var assignment = newData.assignments().get(i);
                builder.append(" ".repeat(indent + 4))
                        .append(assignment.name())
                        .append(": ")
                        .append(formatAssignmentValue(assignment.value(), indent + 4));
                if (i < newData.assignments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        builder.append("}");
        return builder.toString();
    }

    private Expression terminalReturnExpression(Expression expression) {
        return switch (expression) {
            case LetExpression letExpression -> terminalReturnExpression(letExpression.rest());
            default -> expression;
        };
    }

    private Optional<SourcePosition> returnExpressionPosition(Expression expression) {
        return switch (expression) {
            case LetExpression letExpression ->
                    returnExpressionPosition(letExpression.rest()).or(() -> letExpression.position());
            default -> expression.position();
        };
    }

    private String formatParserType(Type type) {
        return switch (type) {
            case PrimitiveType primitiveType -> primitiveType.name().toLowerCase(java.util.Locale.ROOT);
            case CollectionType.ListType listType -> "list[" + formatParserType(listType.elementType()) + "]";
            case CollectionType.SetType setType -> "set[" + formatParserType(setType.elementType()) + "]";
            case CollectionType.DictType dictType -> "dict[" + formatParserType(dictType.valueType()) + "]";
            case TupleType tupleType -> "tuple[" + tupleType.elementTypes().stream()
                    .map(this::formatParserType)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case FunctionType functionType ->
                    formatParserType(functionType.argumentType()) + "=>" + formatParserType(functionType.returnType());
            case DataType dataType -> restorePrivateTypeNameForDisplay(dataType.name());
        };
    }

    private String formatExpressionPreview(Expression expression) {
        return switch (expression) {
            case StringValue stringValue -> stringValue.stringValue();
            case IntValue intValue -> intValue.intValue();
            case LongValue longValue -> longValue.longValue();
            case FloatValue floatValue -> floatValue.floatValue();
            case DoubleValue doubleValue -> doubleValue.doubleValue();
            case ByteValue byteValue -> byteValue.byteValue();
            case BooleanValue booleanValue -> String.valueOf(booleanValue.value());
            case Value value -> restorePrivateFunctionNameForDisplay(value.name());
            case PlaceholderExpression ignored -> "_";
            case FieldAccess fieldAccess -> formatExpressionPreview(fieldAccess.source()) + "." + fieldAccess.field();
            case IndexExpression indexExpression -> formatExpressionPreview(indexExpression.source())
                                                    + "[" + indexExpression.arguments().stream()
                                                            .map(this::formatExpressionPreview)
                                                            .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case FunctionCall functionCall -> formatFunctionCallPreview(functionCall);
            case FunctionInvoke functionInvoke -> formatExpressionPreview(functionInvoke.function())
                                                  + "(" + functionInvoke.arguments().stream()
                                                          .map(this::formatExpressionPreview)
                                                          .collect(java.util.stream.Collectors.joining(", ")) + ")";
            case FunctionReference functionReference -> ":" + restorePrivateFunctionNameForDisplay(functionReference.name());
            case NewData newData -> formatNewDataPreview(newData);
            case WithExpression withExpression -> formatWithPreview(withExpression);
            case NewListExpression newListExpression -> "["
                                                        + newListExpression.values().stream()
                                                                .map(this::formatExpressionPreview)
                                                                .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case NewSetExpression newSetExpression -> "{"
                                                      + newSetExpression.values().stream()
                                                              .map(this::formatExpressionPreview)
                                                              .collect(java.util.stream.Collectors.joining(", ")) + "}";
            case NewDictExpression newDictExpression -> "{"
                                                        + newDictExpression.entries().stream()
                                                                .map(entry -> formatExpressionPreview(entry.key()) + ": " + formatExpressionPreview(entry.value()))
                                                                .collect(java.util.stream.Collectors.joining(", ")) + "}";
            case TupleExpression tupleExpression -> "("
                                                    + tupleExpression.values().stream()
                                                            .map(this::formatExpressionPreview)
                                                            .collect(java.util.stream.Collectors.joining(", ")) + ")";
            case LambdaExpression lambdaExpression -> {
                var args = String.join(", ", lambdaExpression.argumentNames());
                var renderedArgs = lambdaExpression.argumentNames().isEmpty() ? "()" : args;
                var lambda = renderedArgs + " => " + formatExpressionPreview(lambdaExpression.expression());
                if (lambdaExpression.argumentNames().isEmpty()) {
                    yield "(" + lambda + ")";
                }
                yield lambda;
            }
            case SliceExpression sliceExpression -> formatSliceExpressionPreview(sliceExpression);
            case InfixExpression infixExpression -> formatExpressionPreview(infixExpression.left())
                                                    + previewOperator(infixExpression.operator().symbol())
                                                    + formatExpressionPreview(infixExpression.right());
            case NothingValue value -> value.literal();
            case IfExpression ifExpression -> "if " + formatExpressionPreview(ifExpression.condition())
                                              + " then " + formatExpressionPreview(ifExpression.thenBranch())
                                              + " else " + formatExpressionPreview(ifExpression.elseBranch());
            case LetExpression letExpression -> "let " + letExpression.name()
                                                + (letExpression.kind() == LetExpression.Kind.RESULT_BIND ? " <- " : " = ")
                                                + formatExpressionPreview(letExpression.value())
                                                + " " + formatExpressionPreview(letExpression.rest());
            case MatchExpression matchExpression ->
                    "match " + formatExpressionPreview(matchExpression.matchWith()) + " with ...";
            default -> expression.toString();
        };
    }

    private String formatExpressionPreviewWithSpaces(Expression expression) {
        return switch (expression) {
            case InfixExpression infixExpression -> formatExpressionPreviewWithSpaces(infixExpression.left())
                                                    + " " + previewOperator(infixExpression.operator().symbol()) + " "
                                                    + formatExpressionPreviewWithSpaces(infixExpression.right());
            default -> formatExpressionPreview(expression);
        };
    }

    private String previewOperator(String operator) {
        if (operator != null && operator.length() >= 2 && operator.startsWith("\"") && operator.endsWith("\"")) {
            return operator.substring(1, operator.length() - 1);
        }
        return operator;
    }

    private String formatFunctionCallPreview(FunctionCall functionCall) {
        var modulePrefix = functionCall.moduleName().map(moduleName -> moduleName + ".").orElse("");
        return modulePrefix + restorePrivateFunctionNameForDisplay(functionCall.name())
               + "(" + functionCall.arguments().stream()
                       .map(this::formatExpressionPreview)
                       .collect(java.util.stream.Collectors.joining(", ")) + ")";
    }

    private String formatNewDataPreview(NewData newData) {
        var assignments = newData.assignments().stream()
                .map(assignment -> assignment.name() + ": " + formatExpressionPreview(assignment.value()))
                .toList();
        var positional = newData.positionalArguments().stream()
                .map(this::formatExpressionPreview)
                .toList();
        var spreads = newData.spreads().stream()
                .map(expression -> "..." + formatExpressionPreview(expression))
                .toList();
        var all = new ArrayList<String>(assignments.size() + positional.size() + spreads.size());
        all.addAll(assignments);
        all.addAll(positional);
        all.addAll(spreads);
        var body = all.stream().collect(java.util.stream.Collectors.joining(", "));
        return formatParserType(newData.type()) + " { " + body + " }";
    }

    private String formatSliceExpressionPreview(SliceExpression sliceExpression) {
        var start = sliceExpression.start().map(this::formatExpressionPreview).orElse("");
        var end = sliceExpression.end().map(this::formatExpressionPreview).orElse("");
        return formatExpressionPreview(sliceExpression.source()) + "[" + start + ":" + end + "]";
    }

    private String formatWithPreview(WithExpression withExpression) {
        var assignments = withExpression.assignments().stream()
                .map(assignment -> assignment.name() + ": " + formatExpressionPreview(assignment.value()))
                .collect(java.util.stream.Collectors.joining(", "));
        return formatExpressionPreview(withExpression.source()) + ".with(" + assignments + ")";
    }

    private String normalizeReportedTypeName(String typeName) {
        typeName = typeName.trim();
        if (typeName.startsWith("CompiledList[elementType=") && typeName.endsWith("]")) {
            var inner = typeName.substring("CompiledList[elementType=".length(), typeName.length() - 1);
            return "list[" + normalizeReportedTypeName(inner) + "]";
        }
        if (typeName.startsWith("CompiledSet[elementType=") && typeName.endsWith("]")) {
            var inner = typeName.substring("CompiledSet[elementType=".length(), typeName.length() - 1);
            return "set[" + normalizeReportedTypeName(inner) + "]";
        }
        if (typeName.startsWith("CompiledDict[valueType=") && typeName.endsWith("]")) {
            var inner = typeName.substring("CompiledDict[valueType=".length(), typeName.length() - 1);
            return "dict[" + normalizeReportedTypeName(inner) + "]";
        }
        if (typeName.startsWith("CompiledGenericTypeParameter[name=") && typeName.endsWith("]")) {
            return normalizeUserVisibleNames(typeName.substring("CompiledGenericTypeParameter[name=".length(), typeName.length() - 1));
        }
        if (typeName.startsWith("CompiledTupleType[elementTypes=[") && typeName.endsWith("]]")) {
            var inner = typeName.substring("CompiledTupleType[elementTypes=[".length(), typeName.length() - 2);
            return "tuple[" + splitTopLevelTypeArguments(inner).stream()
                    .map(this::normalizeReportedTypeName)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
        }
        if (typeName.startsWith("CompiledFunctionType[")) {
            var argumentType = extractStructuredFieldValue(typeName, "argumentType=");
            var returnType = extractStructuredFieldValue(typeName, "returnType=");
            if (argumentType != null && returnType != null) {
                return normalizeReportedTypeName(argumentType) + "=>" + normalizeReportedTypeName(returnType);
            }
        }
        if (typeName.startsWith("CompiledDataType[name=")) {
            var name = extractStructuredFieldValue(typeName, "name=");
            var typeParameters = extractStructuredFieldValue(typeName, "typeParameters=");
            if (name != null) {
                return renderReportedNamedType(name, typeParameters);
            }
        }
        if (typeName.startsWith("CompiledDataParentType[name=")) {
            var name = extractStructuredFieldValue(typeName, "name=");
            var typeParameters = extractStructuredFieldValue(typeName, "typeParameters=");
            if (name != null) {
                return renderReportedNamedType(name, typeParameters);
            }
        }
        return restorePrivateTypeNameForDisplay(switch (typeName) {
            case "BOOL" -> "bool";
            case "BYTE" -> "byte";
            case "INT" -> "int";
            case "LONG" -> "long";
            case "FLOAT" -> "float";
            case "DOUBLE" -> "double";
            case "STRING" -> "string";
            case "ANY" -> "any";
            case "NOTHING" -> "nothing";
            default -> typeName;
        });
    }

    private String renderReportedNamedType(String rawName, String rawTypeParameters) {
        var displayName = restorePrivateTypeNameForDisplay(stripQualifiedTypeName(rawName));
        if (rawTypeParameters == null || rawTypeParameters.isBlank() || "[]".equals(rawTypeParameters)) {
            return displayName;
        }
        var inner = rawTypeParameters;
        if (inner.startsWith("[") && inner.endsWith("]")) {
            inner = inner.substring(1, inner.length() - 1);
        }
        if (inner.isBlank()) {
            return displayName;
        }
        return displayName + "[" + splitTopLevelTypeArguments(inner).stream()
                .map(this::normalizeReportedTypeName)
                .collect(java.util.stream.Collectors.joining(", ")) + "]";
    }

    private String extractStructuredFieldValue(String source, String fieldName) {
        var start = source.indexOf(fieldName);
        if (start < 0) {
            return null;
        }
        start += fieldName.length();
        var depth = 0;
        for (var i = start; i < source.length(); i++) {
            var current = source.charAt(i);
            if (current == '[') {
                depth++;
            } else if (current == ']') {
                if (depth == 0) {
                    return source.substring(start, i).trim();
                }
                depth--;
            } else if (current == ',' && depth == 0) {
                return source.substring(start, i).trim();
            }
        }
        return source.substring(start).trim();
    }

    private String stripQualifiedTypeName(String typeName) {
        var normalized = typeName.trim();
        var slash = normalized.lastIndexOf('/');
        if (slash >= 0 && slash + 1 < normalized.length()) {
            normalized = normalized.substring(slash + 1);
        }
        var dot = normalized.lastIndexOf('.');
        if (dot >= 0 && dot + 1 < normalized.length()) {
            normalized = normalized.substring(dot + 1);
        }
        return normalized;
    }

    private String formatLinkedType(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitiveType -> primitiveType.name().toLowerCase(java.util.Locale.ROOT);
            case CollectionLinkedType.CompiledList linkedList ->
                    "list[" + formatLinkedType(linkedList.elementType()) + "]";
            case CollectionLinkedType.CompiledSet linkedSet -> "set[" + formatLinkedType(linkedSet.elementType()) + "]";
            case CollectionLinkedType.CompiledDict linkedDict ->
                    "dict[" + formatLinkedType(linkedDict.valueType()) + "]";
            case CompiledTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(this::formatLinkedType)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledFunctionType linkedFunctionType ->
                    formatLinkedType(linkedFunctionType.argumentType()) + "=>" + formatLinkedType(linkedFunctionType.returnType());
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? restorePrivateTypeNameForDisplay(linkedDataType.name())
                    : restorePrivateTypeNameForDisplay(linkedDataType.name()) + "[" + linkedDataType.typeParameters().stream()
                    .map(this::restorePrivateTypeNameForDisplay)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? restorePrivateTypeNameForDisplay(linkedDataParentType.name())
                    : restorePrivateTypeNameForDisplay(linkedDataParentType.name()) + "[" + linkedDataParentType.typeParameters().stream()
                    .map(this::restorePrivateTypeNameForDisplay)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledGenericTypeParameter linkedGenericTypeParameter -> restorePrivateTypeNameForDisplay(linkedGenericTypeParameter.name());
        };
    }

    private boolean isAssignableReturnType(CompiledType expected, CompiledType actual, Map<String, GenericDataType> dataTypes) {
        if (expected == actual
            || (!(expected instanceof GenericDataType) && !(actual instanceof GenericDataType) && expected.equals(actual))) {
            return true;
        }
        if (actual == PrimitiveLinkedType.NOTHING
            || actual == PrimitiveLinkedType.ANY
            || expected == PrimitiveLinkedType.ANY) {
            return true;
        }
        if (expected == PrimitiveLinkedType.DATA) {
            return actual instanceof GenericDataType || actual == PrimitiveLinkedType.DATA || actual == PrimitiveLinkedType.ENUM;
        }
        if (expected == PrimitiveLinkedType.ENUM) {
            return isEnumLikeType(actual, dataTypes);
        }
        if (expected instanceof PrimitiveLinkedType expectedPrimitive
            && actual instanceof PrimitiveLinkedType actualPrimitive) {
            return isAssignablePrimitiveReturnType(expectedPrimitive, actualPrimitive);
        }
        if (expected instanceof CollectionLinkedType.CompiledList expectedList
            && actual instanceof CollectionLinkedType.CompiledList actualList) {
            return isAssignableReturnType(expectedList.elementType(), actualList.elementType(), dataTypes);
        }
        if (expected instanceof CollectionLinkedType.CompiledSet expectedSet
            && actual instanceof CollectionLinkedType.CompiledSet actualSet) {
            return isAssignableReturnType(expectedSet.elementType(), actualSet.elementType(), dataTypes);
        }
        if (expected instanceof CollectionLinkedType.CompiledDict expectedDict
            && actual instanceof CollectionLinkedType.CompiledDict actualDict) {
            return isAssignableReturnType(expectedDict.valueType(), actualDict.valueType(), dataTypes);
        }
        if (expected instanceof CompiledTupleType expectedTuple
            && actual instanceof CompiledTupleType actualTuple) {
            if (expectedTuple.elementTypes().size() != actualTuple.elementTypes().size()) {
                return false;
            }
            for (int i = 0; i < expectedTuple.elementTypes().size(); i++) {
                if (!isAssignableReturnType(expectedTuple.elementTypes().get(i), actualTuple.elementTypes().get(i), dataTypes)) {
                    return false;
                }
            }
            return true;
        }
        if (expected instanceof CompiledFunctionType expectedFunction
            && actual instanceof CompiledFunctionType actualFunction) {
            return isAssignableReturnType(expectedFunction.argumentType(), actualFunction.argumentType(), dataTypes)
                   && isAssignableReturnType(expectedFunction.returnType(), actualFunction.returnType(), dataTypes);
        }
        if (expected instanceof CompiledDataParentType expectedParent) {
            if (actual instanceof CompiledDataParentType actualParent) {
                return sameTypeName(expectedParent.name(), actualParent.name())
                       && areAssignableDataTypeParameters(expectedParent.typeParameters(), actualParent.typeParameters(), dataTypes);
            }
            if (actual instanceof CompiledDataType actualData) {
                var matchingExtendedParent = actualData.extendedTypes().stream()
                        .map(this::normalizeDescriptor)
                        .map(this::parseGenericTypeName)
                        .filter(parent -> sameTypeName(expectedParent.name(), parent.baseName()))
                        .findFirst();
                if (matchingExtendedParent.isPresent()) {
                    return areAssignableDataTypeParameters(
                            expectedParent.typeParameters(),
                            resolvedExtendedParentTypeArguments(actualData, matchingExtendedParent.orElseThrow(), dataTypes),
                            dataTypes
                    );
                }
                var matchingSubtype = expectedParent.subTypes().stream()
                        .filter(subType -> sameTypeName(subType.name(), actualData.name()))
                        .findFirst();
                if (matchingSubtype.isPresent()) {
                    if (!actualData.typeParameters().isEmpty()) {
                        return areAssignableDataTypeParameters(expectedParent.typeParameters(), actualData.typeParameters(), dataTypes);
                    }
                    var expectedSubtype = matchingSubtype.orElseThrow();
                    if (expectedSubtype.fields().size() != actualData.fields().size()) {
                        return false;
                    }
                    for (int i = 0; i < expectedSubtype.fields().size(); i++) {
                        if (!isAssignableReturnType(expectedSubtype.fields().get(i).type(), actualData.fields().get(i).type(), dataTypes)) {
                            return false;
                        }
                    }
                    return true;
                }
                if (sameTypeName(expectedParent.name(), actualData.name())) {
                    return actualData.typeParameters().isEmpty()
                           || areAssignableDataTypeParameters(expectedParent.typeParameters(), actualData.typeParameters(), dataTypes);
                }
                return false;
            }
        }
        if (expected instanceof CompiledDataType expectedData) {
            if (actual instanceof CompiledDataType actualData) {
                return sameTypeName(expectedData.name(), actualData.name())
                       && areAssignableDataTypeParameters(expectedData.typeParameters(), actualData.typeParameters(), dataTypes);
            }
            if (actual instanceof CompiledDataParentType actualParent) {
                return sameTypeName(expectedData.name(), actualParent.name())
                       && areAssignableDataTypeParameters(expectedData.typeParameters(), actualParent.typeParameters(), dataTypes);
            }
        }
        if (expected instanceof CompiledGenericTypeParameter) {
            return true;
        }
        return false;
    }

    private List<String> resolvedExtendedParentTypeArguments(
            CompiledDataType actualData,
            ParsedGenericTypeName extendedParent,
            Map<String, GenericDataType> dataTypes
    ) {
        if (extendedParent.typeArguments().isEmpty() || actualData.typeParameters().isEmpty()) {
            return extendedParent.typeArguments();
        }
        var rawType = dataTypes.values().stream()
                .filter(CompiledDataType.class::isInstance)
                .map(CompiledDataType.class::cast)
                .filter(candidate -> sameTypeName(candidate.name(), actualData.name()))
                .findFirst();
        if (rawType.isEmpty() || rawType.orElseThrow().typeParameters().isEmpty()) {
            return extendedParent.typeArguments();
        }
        var substitutions = new LinkedHashMap<String, String>();
        var max = Math.min(rawType.orElseThrow().typeParameters().size(), actualData.typeParameters().size());
        for (var i = 0; i < max; i++) {
            substitutions.put(rawType.orElseThrow().typeParameters().get(i), actualData.typeParameters().get(i));
        }
        if (substitutions.isEmpty()) {
            return extendedParent.typeArguments();
        }
        return extendedParent.typeArguments().stream()
                .map(typeArgument -> substituteTypeArgumentDescriptor(typeArgument, substitutions))
                .toList();
    }

    private String substituteTypeArgumentDescriptor(String descriptor, Map<String, String> substitutions) {
        var normalized = normalizeDescriptor(descriptor);
        var direct = substitutions.get(normalized);
        if (direct != null) {
            return direct;
        }
        var parsed = parseGenericTypeName(normalized);
        if (parsed.typeArguments().isEmpty()) {
            return normalized;
        }
        return parsed.baseName() + "[" + parsed.typeArguments().stream()
                .map(typeArgument -> substituteTypeArgumentDescriptor(typeArgument, substitutions))
                .collect(java.util.stream.Collectors.joining(", ")) + "]";
    }

    private boolean isEnumLikeType(CompiledType type, Map<String, GenericDataType> dataTypes) {
        if (type == PrimitiveLinkedType.ENUM) {
            return true;
        }
        if (type instanceof CompiledDataParentType parentType) {
            return parentType.enumType();
        }
        if (!(type instanceof CompiledDataType dataType)) {
            return false;
        }
        return dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(CompiledDataParentType::enumType)
                .flatMap(parentType -> parentType.subTypes().stream())
                .anyMatch(subType -> sameTypeName(subType.name(), dataType.name()));
    }

    private boolean isAssignablePrimitiveReturnType(PrimitiveLinkedType expected, PrimitiveLinkedType actual) {
        if (expected == actual) {
            return true;
        }
        if (actual == PrimitiveLinkedType.NOTHING) {
            return true;
        }
        if (expected == PrimitiveLinkedType.ANY) {
            return true;
        }
        if (expected == PrimitiveLinkedType.DATA || actual == PrimitiveLinkedType.DATA
            || expected == PrimitiveLinkedType.ENUM || actual == PrimitiveLinkedType.ENUM) {
            return false;
        }
        if (expected == PrimitiveLinkedType.BOOL || actual == PrimitiveLinkedType.BOOL) {
            return false;
        }
        if (expected == PrimitiveLinkedType.STRING || actual == PrimitiveLinkedType.STRING) {
            return false;
        }
        return (actual == PrimitiveLinkedType.INT
                && (expected == PrimitiveLinkedType.LONG
                    || expected == PrimitiveLinkedType.FLOAT
                    || expected == PrimitiveLinkedType.DOUBLE))
               || (actual == PrimitiveLinkedType.LONG
                   && (expected == PrimitiveLinkedType.FLOAT
                       || expected == PrimitiveLinkedType.DOUBLE))
               || (actual == PrimitiveLinkedType.FLOAT && expected == PrimitiveLinkedType.DOUBLE);
    }

    private boolean sameTypeName(String left, String right) {
        return normalizeTypeName(left).equals(normalizeTypeName(right));
    }

    private String normalizeTypeName(String name) {
        var raw = name;
        var genericStart = raw.indexOf('[');
        if (genericStart > 0) {
            raw = raw.substring(0, genericStart);
        }
        var slashIdx = raw.lastIndexOf('/');
        if (slashIdx >= 0 && slashIdx < raw.length() - 1) {
            raw = raw.substring(slashIdx + 1);
        }
        var dotIdx = raw.lastIndexOf('.');
        if (dotIdx >= 0 && dotIdx < raw.length() - 1) {
            raw = raw.substring(dotIdx + 1);
        }
        return raw;
    }

    private dev.capylang.compiler.expression.CompiledExpression coerceReturnExpression(
            dev.capylang.compiler.expression.CompiledExpression expression,
            CompiledType returnType,
            Map<String, GenericDataType> dataTypes
    ) {
        if (returnType instanceof CollectionLinkedType.CompiledDict dictType
            && expression instanceof dev.capylang.compiler.expression.CompiledNewSet linkedNewSet
            && linkedNewSet.values().isEmpty()) {
            return new dev.capylang.compiler.expression.CompiledNewDict(
                    List.of(),
                    new CollectionLinkedType.CompiledDict(dictType.valueType())
            );
        }
        if (returnType instanceof CompiledDataParentType parentType
            && expression instanceof dev.capylang.compiler.expression.CompiledNewData newData
            && newData.type() instanceof CompiledDataType actualDataType) {
            if (parentType.enumType()
                && actualDataType.singleton()
                && parentType.subTypes().stream().anyMatch(subType -> sameTypeName(subType.name(), actualDataType.name()))) {
                return expression;
            }
            var matchingSubtype = parentType.subTypes().stream()
                    .filter(subType -> sameTypeName(subType.name(), actualDataType.name()))
                    .findFirst();
            if (matchingSubtype.isPresent()) {
                if (actualDataType.name().startsWith("/capy/meta_prog/Reflection.")) {
                    return expression;
                }
                var expectedSubtype = matchingSubtype.orElseThrow();
                if (expectedSubtype.fields().size() != newData.assignments().size()) {
                    return expression;
                }
                var coercedAssignments = new ArrayList<dev.capylang.compiler.expression.CompiledNewData.FieldAssignment>();
                for (int i = 0; i < newData.assignments().size(); i++) {
                    var expectedField = expectedSubtype.fields().get(i);
                    var assignment = newData.assignments().get(i);
                    var coercedValue = coerceReturnExpression(assignment.value(), expectedField.type(), dataTypes);
                    if (!isAssignableReturnType(expectedField.type(), coercedValue.type(), dataTypes)) {
                        return expression;
                    }
                    coercedAssignments.add(new dev.capylang.compiler.expression.CompiledNewData.FieldAssignment(
                            assignment.name(),
                            coercedValue
                    ));
                }
                return new dev.capylang.compiler.expression.CompiledNewData(expectedSubtype, coercedAssignments);
            }
        }
        return expression;
    }

    private Optional<CompiledType> firstConcreteReturnMismatch(
            dev.capylang.compiler.expression.CompiledExpression expression,
            CompiledType expectedReturnType,
            Map<String, GenericDataType> dataTypes
    ) {
        return switch (expression) {
            case dev.capylang.compiler.expression.CompiledLetExpression letExpression ->
                    firstConcreteReturnMismatch(letExpression.rest(), expectedReturnType, dataTypes);
            case dev.capylang.compiler.expression.CompiledIfExpression ifExpression -> {
                var thenMismatch = firstConcreteReturnMismatch(ifExpression.thenBranch(), expectedReturnType, dataTypes);
                if (thenMismatch.isPresent()) {
                    yield thenMismatch;
                }
                yield firstConcreteReturnMismatch(ifExpression.elseBranch(), expectedReturnType, dataTypes);
            }
            case dev.capylang.compiler.expression.CompiledMatchExpression matchExpression ->
                    matchExpression.cases().stream()
                            .map(matchCase -> firstConcreteReturnMismatch(matchCase.expression(), expectedReturnType, dataTypes))
                            .flatMap(Optional::stream)
                            .findFirst();
            default -> {
                var coercedExpression = coerceReturnExpression(expression, expectedReturnType, dataTypes);
                if (coercedExpression.type() != PrimitiveLinkedType.ANY
                    && coercedExpression.type() != PrimitiveLinkedType.NOTHING
                    && shouldReportConcreteBranchMismatch(expectedReturnType, coercedExpression.type(), dataTypes)) {
                    yield Optional.of(coercedExpression.type());
                }
                yield Optional.empty();
            }
        };
    }

    private boolean shouldReportConcreteBranchMismatch(
            CompiledType expectedReturnType,
            CompiledType actualReturnType,
            Map<String, GenericDataType> dataTypes
    ) {
        if (isAssignableReturnType(expectedReturnType, actualReturnType, dataTypes)) {
            return false;
        }
        return !(expectedReturnType instanceof GenericDataType && actualReturnType instanceof GenericDataType);
    }
    private boolean isProgramMain(String name, CompiledType returnType, List<CompiledFunctionParameter> parameters) {
        if (!"main".equals(name)) {
            return false;
        }
        if (parameters.size() != 1) {
            return false;
        }
        if (!(parameters.getFirst().type() instanceof CollectionLinkedType.CompiledList listType)
            || listType.elementType() != PrimitiveLinkedType.STRING) {
            return false;
        }
        if (!(returnType instanceof GenericDataType genericDataType)) {
            return false;
        }
        if (isProgramType(genericDataType)) {
            return true;
        }
        var typeParameters = typeParameters(genericDataType);
        return isEffectType(genericDataType)
               && typeParameters.size() == 1
               && isProgramTypeDescriptor(typeParameters.getFirst());
    }

    private boolean isProgramType(GenericDataType genericDataType) {
        var normalized = normalizeQualifiedTypeName(genericDataType.name());
        return "Program".equals(genericDataType.name())
               || normalized.equals("/capy/lang/Program")
               || normalized.equals("/capy/lang/Program.Program")
               || normalized.equals("/cap/lang/Program")
               || normalized.equals("/cap/lang/Program.Program");
    }

    private boolean isEffectType(GenericDataType genericDataType) {
        var normalized = normalizeQualifiedTypeName(genericDataType.name());
        return "Effect".equals(genericDataType.name())
               || normalized.equals("/capy/lang/Effect")
               || normalized.equals("/capy/lang/Effect.Effect")
               || normalized.equals("/cap/lang/Effect")
               || normalized.equals("/cap/lang/Effect.Effect");
    }

    private List<String> typeParameters(GenericDataType genericDataType) {
        return switch (genericDataType) {
            case CompiledDataType dataType -> dataType.typeParameters();
            case CompiledDataParentType parentType -> parentType.typeParameters();
        };
    }

    private boolean isProgramTypeDescriptor(String descriptor) {
        var rawType = descriptor;
        var genericStart = rawType.indexOf('[');
        if (genericStart >= 0) {
            rawType = rawType.substring(0, genericStart);
        }
        var normalized = normalizeQualifiedTypeName(rawType);
        return "Program".equals(rawType)
               || normalized.equals("/capy/lang/Program")
               || normalized.equals("/capy/lang/Program.Program")
               || normalized.equals("/cap/lang/Program")
               || normalized.equals("/cap/lang/Program.Program");
    }

    private String normalizeQualifiedTypeName(String typeName) {
        var normalized = typeName.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private String moduleSourceFile(Module module) {
        return module.sourceKind().moduleFile(module.path(), module.name());
    }

    private dev.capylang.compiler.expression.CompiledExpression enrichNothing(
            dev.capylang.compiler.expression.CompiledExpression expression,
            String functionName,
            String moduleSourceFile
    ) {
        return switch (expression) {
            case dev.capylang.compiler.expression.CompiledBooleanValue value -> value;
            case dev.capylang.compiler.expression.CompiledByteValue value -> value;
            case dev.capylang.compiler.expression.CompiledDoubleValue value -> value;
            case dev.capylang.compiler.expression.CompiledEffectBindExpression value ->
                    new dev.capylang.compiler.expression.CompiledEffectBindExpression(
                            value.name(),
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.payloadType(),
                            value.letType(),
                            value.declaredType(),
                            enrichNothing(value.rest(), functionName, moduleSourceFile),
                            value.effectType()
                    );
            case dev.capylang.compiler.expression.CompiledEffectExpression value ->
                    new dev.capylang.compiler.expression.CompiledEffectExpression(
                            enrichNothing(value.body(), functionName, moduleSourceFile),
                            value.effectType()
                    );
            case dev.capylang.compiler.expression.CompiledFieldAccess value ->
                    new dev.capylang.compiler.expression.CompiledFieldAccess(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.field(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledFloatValue value -> value;
            case dev.capylang.compiler.expression.CompiledFunctionCall value ->
                    new dev.capylang.compiler.expression.CompiledFunctionCall(
                            value.name(),
                            value.arguments().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                            value.returnType()
                    );
            case dev.capylang.compiler.expression.CompiledFunctionInvoke value ->
                    new dev.capylang.compiler.expression.CompiledFunctionInvoke(
                            enrichNothing(value.function(), functionName, moduleSourceFile),
                            value.arguments().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                            value.returnType()
                    );
            case dev.capylang.compiler.expression.CompiledIfExpression value ->
                    new dev.capylang.compiler.expression.CompiledIfExpression(
                            enrichNothing(value.condition(), functionName, moduleSourceFile),
                            enrichNothing(value.thenBranch(), functionName, moduleSourceFile),
                            enrichNothing(value.elseBranch(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledIndexExpression value ->
                    new dev.capylang.compiler.expression.CompiledIndexExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            enrichNothing(value.index(), functionName, moduleSourceFile),
                            value.elementType(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledInfixExpression value ->
                    new dev.capylang.compiler.expression.CompiledInfixExpression(
                            enrichNothing(value.left(), functionName, moduleSourceFile),
                            value.operator(),
                            enrichNothing(value.right(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledIntValue value -> value;
            case dev.capylang.compiler.expression.CompiledLambdaExpression value ->
                    new dev.capylang.compiler.expression.CompiledLambdaExpression(
                            value.argumentName(),
                            enrichNothing(value.expression(), functionName, moduleSourceFile),
                            value.functionType()
                    );
            case dev.capylang.compiler.expression.CompiledLetExpression value ->
                    new dev.capylang.compiler.expression.CompiledLetExpression(
                            value.name(),
                            enrichNothing(value.value(), functionName, moduleSourceFile),
                            value.declaredType(),
                            enrichNothing(value.rest(), functionName, moduleSourceFile)
                    );
            case dev.capylang.compiler.expression.CompiledLongValue value -> value;
            case dev.capylang.compiler.expression.CompiledMatchExpression value ->
                    new dev.capylang.compiler.expression.CompiledMatchExpression(
                            enrichNothing(value.matchWith(), functionName, moduleSourceFile),
                            value.cases().stream().map(matchCase -> new dev.capylang.compiler.expression.CompiledMatchExpression.MatchCase(
                                    matchCase.pattern(),
                                    matchCase.guard().map(guard -> enrichNothing(guard, functionName, moduleSourceFile)),
                                    enrichNothing(matchCase.expression(), functionName, moduleSourceFile)
                            )).toList(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledNothingValue value -> {
                var line = value.position().map(SourcePosition::line).orElse(-1);
                var column = value.position().map(SourcePosition::column).orElse(-1);
                var normalizedFile = moduleSourceFile.startsWith("/") ? moduleSourceFile : "/" + moduleSourceFile;
                var message = value.message().contains("`<native>`")
                        ? "line " + line + ", column " + column + ", file " + normalizedFile
                          + ": native expression in function `" + functionName + "` must be replaced by code generation"
                        : "line " + line + ", column " + column + ", file " + normalizedFile
                          + ": the function `" + functionName + "` is not yet implemented";
                yield new dev.capylang.compiler.expression.CompiledNothingValue(value.position(), message);
            }
            case dev.capylang.compiler.expression.CompiledNumericWidening value ->
                    new dev.capylang.compiler.expression.CompiledNumericWidening(
                            enrichNothing(value.expression(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledPipeAllExpression value ->
                    new dev.capylang.compiler.expression.CompiledPipeAllExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.argumentName(),
                            enrichNothing(value.predicate(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledPipeAnyExpression value ->
                    new dev.capylang.compiler.expression.CompiledPipeAnyExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.argumentName(),
                            enrichNothing(value.predicate(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledPipeFlatMapExpression value ->
                    new dev.capylang.compiler.expression.CompiledPipeFlatMapExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.argumentName(),
                            enrichNothing(value.mapper(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledPipeFilterOutExpression value ->
                    new dev.capylang.compiler.expression.CompiledPipeFilterOutExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.argumentName(),
                            enrichNothing(value.predicate(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledPipeExpression value ->
                    new dev.capylang.compiler.expression.CompiledPipeExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.argumentName(),
                            enrichNothing(value.mapper(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledPipeReduceExpression value ->
                    new dev.capylang.compiler.expression.CompiledPipeReduceExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            enrichNothing(value.initialValue(), functionName, moduleSourceFile),
                            value.accumulatorName(),
                            value.keyName(),
                            value.valueName(),
                            enrichNothing(value.reducerExpression(), functionName, moduleSourceFile),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledNewDict value ->
                    new dev.capylang.compiler.expression.CompiledNewDict(
                            value.entries().stream().map(entry -> new dev.capylang.compiler.expression.CompiledNewDict.Entry(
                                    enrichNothing(entry.key(), functionName, moduleSourceFile),
                                    enrichNothing(entry.value(), functionName, moduleSourceFile)
                            )).toList(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledNewList value ->
                    new dev.capylang.compiler.expression.CompiledNewList(
                            value.values().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledNewSet value ->
                    new dev.capylang.compiler.expression.CompiledNewSet(
                            value.values().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledNewData value ->
                    new dev.capylang.compiler.expression.CompiledNewData(
                            value.type(),
                            value.assignments().stream().map(assignment -> new dev.capylang.compiler.expression.CompiledNewData.FieldAssignment(
                                    assignment.name(),
                                    enrichNothing(assignment.value(), functionName, moduleSourceFile)
                            )).toList()
                    );
            case dev.capylang.compiler.expression.CompiledReflectionValue value ->
                    new dev.capylang.compiler.expression.CompiledReflectionValue(
                            enrichNothing(value.target(), functionName, moduleSourceFile),
                            value.name(),
                            value.packageName(),
                            value.packagePath(),
                            value.fields(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledSliceExpression value ->
                    new dev.capylang.compiler.expression.CompiledSliceExpression(
                            enrichNothing(value.source(), functionName, moduleSourceFile),
                            value.start().map(v -> enrichNothing(v, functionName, moduleSourceFile)),
                            value.end().map(v -> enrichNothing(v, functionName, moduleSourceFile)),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledTupleExpression value ->
                    new dev.capylang.compiler.expression.CompiledTupleExpression(
                            value.values().stream().map(v -> enrichNothing(v, functionName, moduleSourceFile)).toList(),
                            value.type()
                    );
            case dev.capylang.compiler.expression.CompiledStringValue value -> value;
            case dev.capylang.compiler.expression.CompiledVariable value -> value;
        };
    }

    private Result<List<CapybaraExpressionCompiler.FunctionSignature>> linkFunctionSignatures(
            List<Function> functions,
            Map<String, GenericDataType> dataTypes,
            CompileCache compileCache,
            String moduleSourceFile
    ) {
        var linked = functions.stream()
                .map(function -> {
                    var functionGenericTypeNames = functionGenericTypeNames(function, dataTypes, compileCache);
                    return linkParameters(function.parameters(), dataTypes, functionGenericTypeNames, compileCache)
                            .flatMap(parameters -> linkSignatureReturnType(function, dataTypes, functionGenericTypeNames, compileCache, moduleSourceFile)
                                    .map(returnType -> new CapybaraExpressionCompiler.FunctionSignature(
                                            function.name(),
                                            parameters.stream().map(CompiledFunctionParameter::type).toList(),
                                            parameters.stream().map(CompiledFunctionParameter::name).toList(),
                                            returnType,
                                            function.visibility()
                                    )));
                })
                .collect(new ResultCollectionCollector<>());
        if (linked instanceof Result.Error<List<CapybaraExpressionCompiler.FunctionSignature>> error) {
            return error;
        }
        var signatures = ((Result.Success<List<CapybaraExpressionCompiler.FunctionSignature>>) linked).value();
        var signaturesByKey = new LinkedHashMap<String, Function>();
        for (var i = 0; i < signatures.size(); i++) {
            var signature = signatures.get(i);
            var previous = signaturesByKey.putIfAbsent(signatureKey(signature), functions.get(i));
            if (previous != null) {
                return withPosition(
                        Result.error("Duplicate function signature `" + displaySignatureName(signature.name()) + "` for parameter types "
                                     + signature.parameterTypes().stream().map(CompiledType::name).toList()),
                        functions.get(i).position(),
                        normalizeFile(moduleSourceFile)
                );
            }
        }
        return Result.success(signatures);
    }

    private boolean areAssignableDataTypeParameters(List<String> expectedParameters, List<String> actualParameters, Map<String, GenericDataType> dataTypes) {
        if (expectedParameters.isEmpty()) {
            return true;
        }
        if (expectedParameters.size() != actualParameters.size()) {
            return false;
        }
        for (int i = 0; i < expectedParameters.size(); i++) {
            if (!isAssignableTypeDescriptor(expectedParameters.get(i), actualParameters.get(i), dataTypes)) {
                return false;
            }
        }
        return true;
    }

    private boolean isAssignableTypeDescriptor(String expectedDescriptor, String actualDescriptor, Map<String, GenericDataType> dataTypes) {
        var expected = normalizeDescriptor(expectedDescriptor);
        var actual = normalizeDescriptor(actualDescriptor);
        if (expected.equals(actual)) {
            return true;
        }
        if (expected.matches("[A-Z]") || actual.matches("[A-Z]")) {
            return true;
        }

        if ("any".equals(expected) || "nothing".equals(actual)) {
            return true;
        }
        var expectedPrimitive = PrimitiveType.find(expected).map(this::toPrimitiveLinkedType);
        var actualPrimitive = PrimitiveType.find(actual).map(this::toPrimitiveLinkedType);
        if (expectedPrimitive.isPresent() && actualPrimitive.isPresent()) {
            return isAssignablePrimitiveReturnType(expectedPrimitive.get(), actualPrimitive.get());
        }
        if (expected.startsWith("list[") && expected.endsWith("]")
            && actual.startsWith("list[") && actual.endsWith("]")) {
            return isAssignableTypeDescriptor(
                    expected.substring(5, expected.length() - 1),
                    actual.substring(5, actual.length() - 1),
                    dataTypes
            );
        }
        if (expected.startsWith("set[") && expected.endsWith("]")
            && actual.startsWith("set[") && actual.endsWith("]")) {
            return isAssignableTypeDescriptor(
                    expected.substring(4, expected.length() - 1),
                    actual.substring(4, actual.length() - 1),
                    dataTypes
            );
        }
        if (expected.startsWith("dict[") && expected.endsWith("]")
            && actual.startsWith("dict[") && actual.endsWith("]")) {
            return isAssignableTypeDescriptor(
                    expected.substring(5, expected.length() - 1),
                    actual.substring(5, actual.length() - 1),
                    dataTypes
            );
        }
        var expectedArrow = indexOfTopLevelArrow(expected, "=>");
        var actualArrow = indexOfTopLevelArrow(actual, "=>");
        if (expectedArrow > 0 && actualArrow > 0) {
            return isAssignableTypeDescriptor(
                    stripOptionalParentheses(expected.substring(0, expectedArrow)),
                    stripOptionalParentheses(actual.substring(0, actualArrow)),
                    dataTypes
            ) && isAssignableTypeDescriptor(
                    expected.substring(expectedArrow + 2).trim(),
                    actual.substring(actualArrow + 2).trim(),
                    dataTypes
            );
        }
        var expectedParsed = parseGenericTypeName(expected);
        var actualParsed = parseGenericTypeName(actual);
        if (!sameTypeName(expectedParsed.baseName(), actualParsed.baseName())) {
            if (!isSubtypeDescriptorAssignable(actualParsed, expectedParsed, dataTypes)) {
                return false;
            }
            return true;
        }
        if (expectedParsed.typeArguments().isEmpty()) {
            return true;
        }
        if (expectedParsed.typeArguments().size() != actualParsed.typeArguments().size()) {
            return false;
        }
        for (int i = 0; i < expectedParsed.typeArguments().size(); i++) {
            if (!isAssignableTypeDescriptor(expectedParsed.typeArguments().get(i), actualParsed.typeArguments().get(i), dataTypes)) {
                return false;
            }
        }
        return true;
    }

    private boolean isSubtypeDescriptorAssignable(ParsedGenericTypeName actual, ParsedGenericTypeName expected, Map<String, GenericDataType> dataTypes) {
        if (!isSubtypeNameOfParent(actual.baseName(), expected.baseName(), dataTypes, new HashSet<>())) {
            return false;
        }
        if (actual.typeArguments().isEmpty() || expected.typeArguments().isEmpty()) {
            return true;
        }
        if (actual.typeArguments().size() != expected.typeArguments().size()) {
            return false;
        }
        for (int i = 0; i < expected.typeArguments().size(); i++) {
            if (!isAssignableTypeDescriptor(expected.typeArguments().get(i), actual.typeArguments().get(i), dataTypes)) {
                return false;
            }
        }
        return true;
    }

    private boolean isSubtypeNameOfParent(String actualTypeName, String expectedParentName, Map<String, GenericDataType> dataTypes, Set<String> visited) {
        var normalizedActual = normalizeDescriptor(actualTypeName);
        if (!visited.add(normalizedActual)) {
            return false;
        }
        var maybeActual = dataTypes.values().stream()
                .filter(CompiledDataType.class::isInstance)
                .map(CompiledDataType.class::cast)
                .filter(dataType -> sameTypeName(dataType.name(), normalizedActual))
                .findFirst();
        if (maybeActual.isEmpty()) {
            var maybeParent = dataTypes.values().stream()
                    .filter(CompiledDataParentType.class::isInstance)
                    .map(CompiledDataParentType.class::cast)
                    .filter(parentType -> sameTypeName(parentType.name(), normalizedActual))
                    .findFirst();
            if (maybeParent.isEmpty()) {
                return false;
            }
            var actualParent = maybeParent.orElseThrow();
            if (sameTypeName(actualParent.name(), expectedParentName)) {
                return true;
            }
            return dataTypes.values().stream()
                    .filter(CompiledDataParentType.class::isInstance)
                    .map(CompiledDataParentType.class::cast)
                    .filter(parent -> parent.subTypes().stream().anyMatch(subType -> sameTypeName(subType.name(), actualParent.name())))
                    .anyMatch(parent -> sameTypeName(parent.name(), expectedParentName)
                                        || isSubtypeNameOfParent(parent.name(), expectedParentName, dataTypes, visited));
        }
        var actualData = maybeActual.orElseThrow();
        for (var extendedType : actualData.extendedTypes()) {
            var parsedExtended = parseGenericTypeName(normalizeDescriptor(extendedType));
            if (sameTypeName(parsedExtended.baseName(), expectedParentName)) {
                return true;
            }
            if (isSubtypeNameOfParent(parsedExtended.baseName(), expectedParentName, dataTypes, visited)) {
                return true;
            }
        }
        for (var parentType : dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parent -> parent.subTypes().stream().anyMatch(subType -> sameTypeName(subType.name(), actualData.name())))
                .toList()) {
            if (sameTypeName(parentType.name(), expectedParentName)) {
                return true;
            }
            if (isSubtypeNameOfParent(parentType.name(), expectedParentName, dataTypes, visited)) {
                return true;
            }
        }
        return false;
    }
    private String normalizeDescriptor(String descriptor) {
        return descriptor.replaceAll("\\s+", "").replace("->", "=>");
    }

    private PrimitiveLinkedType toPrimitiveLinkedType(PrimitiveType primitiveType) {
        return switch (primitiveType) {
            case BYTE -> PrimitiveLinkedType.BYTE;
            case INT -> PrimitiveLinkedType.INT;
            case LONG -> PrimitiveLinkedType.LONG;
            case DOUBLE -> PrimitiveLinkedType.DOUBLE;
            case BOOL -> PrimitiveLinkedType.BOOL;
            case STRING -> PrimitiveLinkedType.STRING;
            case FLOAT -> PrimitiveLinkedType.FLOAT;
            case NOTHING -> PrimitiveLinkedType.NOTHING;
            case ANY -> PrimitiveLinkedType.ANY;
            case DATA -> PrimitiveLinkedType.DATA;
            case ENUM -> PrimitiveLinkedType.ENUM;
        };
    }

    private Result<CompiledType> linkSignatureReturnType(
            Function function,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames,
            CompileCache compileCache,
            String moduleSourceFile
    ) {
        var linked = function.returnType()
                .map(type -> linkType(type, dataTypes, functionGenericTypeNames, compileCache))
                .orElseGet(() -> Result.success(PrimitiveLinkedType.ANY));
        if (!(linked instanceof Result.Error<CompiledType> error)) {
            return linked;
        }

        var line = function.position().map(SourcePosition::line).orElse(0);
        var column = signatureReturnTypeColumn(function);
        var file = normalizeFile(moduleSourceFile);
        var header = formatFunctionHeader(function) + " =";
        var formattedErrors = error.errors().stream()
                .map(singleError -> normalizeSignatureTypeError(singleError.message()))
                .map(message -> {
                    var pointer = " ".repeat(Math.max(column, 0)) + "^ " + message;
                    var formatted = "error: mismatched types\n"
                                    + " --> " + file + ":" + line + ":" + column + "\n"
                                    + header + "\n"
                                    + pointer + "\n";
                    return new Result.Error.SingleError(line, column, file, formatted);
                })
                .toList();
        return new Result.Error<>(formattedErrors);
    }

    private String normalizeSignatureTypeError(String message) {
        if (message.startsWith("Data type \"") && message.endsWith("\" not found")) {
            var typeName = message.substring("Data type \"".length(), message.length() - "\" not found".length());
            return "Data type `" + restorePrivateTypeNameForDisplay(typeName) + "` not found";
        }
        return normalizeUserVisibleNames(message);
    }

    private Optional<Result.Error<CompiledFunction>> privateTypeEscapingFunctionSignatureError(
            Function function,
            String moduleSourceFile
    ) {
        if (function.name().contains("__local_fun_")) {
            return Optional.empty();
        }

        for (int i = 0; i < function.parameters().size(); i++) {
            var parameter = function.parameters().get(i);
            var escaped = firstEscapedPrivateLocalType(parameter.type());
            if (escaped.isPresent()) {
                var line = function.position().map(SourcePosition::line).orElse(0);
                var column = signatureParameterTypeColumn(function, i);
                return Optional.of(new Result.Error<>(new Result.Error.SingleError(
                        line,
                        column,
                        "",
                        privateTypeEscapingFunctionSignatureMessage(
                                function,
                                moduleSourceFile,
                                line,
                                column,
                                escaped.get()
                        )
                )));
            }
        }

        if (function.returnType().isPresent()) {
            var escaped = firstEscapedPrivateLocalType(function.returnType().get());
            if (escaped.isPresent()) {
                var line = function.position().map(SourcePosition::line).orElse(0);
                var column = signatureReturnTypeColumn(function);
                return Optional.of(new Result.Error<>(new Result.Error.SingleError(
                        line,
                        column,
                        "",
                        privateTypeEscapingFunctionSignatureMessage(
                                function,
                                moduleSourceFile,
                                line,
                                column,
                                escaped.get()
                        )
                )));
            }
        }

        return Optional.empty();
    }

    private Optional<String> firstEscapedPrivateLocalType(Type type) {
        return switch (type) {
            case DataType dataType -> dataType.name().contains("__local_type_") || dataType.name().contains("__local_single_")
                    ? Optional.of(dataType.name())
                    : Optional.empty();
            case CollectionType.ListType listType -> firstEscapedPrivateLocalType(listType.elementType());
            case CollectionType.SetType setType -> firstEscapedPrivateLocalType(setType.elementType());
            case CollectionType.DictType dictType -> firstEscapedPrivateLocalType(dictType.valueType());
            case TupleType tupleType -> tupleType.elementTypes().stream()
                    .map(this::firstEscapedPrivateLocalType)
                    .flatMap(Optional::stream)
                    .findFirst();
            case FunctionType functionType -> firstEscapedPrivateLocalType(functionType.argumentType())
                    .or(() -> firstEscapedPrivateLocalType(functionType.returnType()));
            default -> Optional.empty();
        };
    }

    private int signatureParameterTypeColumn(Function function, int parameterIndex) {
        var column = 4 + function.name().length() + 1;
        for (int i = 0; i < parameterIndex; i++) {
            if (i > 0) {
                column += 2;
            }
            var parameter = function.parameters().get(i);
            column += parameter.name().length() + 2 + formatParserTypeForPosition(parameter.type()).length();
        }
        if (parameterIndex > 0) {
            column += 2;
        }
        var targetParameter = function.parameters().get(parameterIndex);
        return column + targetParameter.name().length() + 2;
    }

    private int signatureReturnTypeColumn(Function function) {
        var column = 4 + function.name().length() + 1;
        for (int i = 0; i < function.parameters().size(); i++) {
            if (i > 0) {
                column += 2;
            }
            var parameter = function.parameters().get(i);
            column += parameter.name().length() + 2 + formatParserTypeForPosition(parameter.type()).length();
        }
        return column + 1 + 2;
    }

    private String formatParserTypeForPosition(Type type) {
        return formatParserType(restorePrivateTypeNameForDisplay(type));
    }

    private Type restorePrivateTypeNameForDisplay(Type type) {
        return switch (type) {
            case DataType dataType -> new DataType(restorePrivateTypeNameForDisplay(dataType.name()));
            case CollectionType.ListType listType -> new CollectionType.ListType(
                    restorePrivateTypeNameForDisplay(listType.elementType())
            );
            case CollectionType.SetType setType -> new CollectionType.SetType(
                    restorePrivateTypeNameForDisplay(setType.elementType())
            );
            case CollectionType.DictType dictType -> new CollectionType.DictType(
                    restorePrivateTypeNameForDisplay(dictType.valueType())
            );
            case TupleType tupleType -> new TupleType(tupleType.elementTypes().stream()
                    .map(this::restorePrivateTypeNameForDisplay)
                    .toList());
            case FunctionType functionType -> new FunctionType(
                    restorePrivateTypeNameForDisplay(functionType.argumentType()),
                    restorePrivateTypeNameForDisplay(functionType.returnType())
            );
            default -> type;
        };
    }

    private String restorePrivateTypeNameForDisplay(String typeName) {
        var restoredLocalSingles = replacePrivateLocalNamesInText(typeName, "__local_single_", false);
        return replacePrivateLocalNamesInText(restoredLocalSingles, "__local_type_", true);
    }

    private String toUserPrivateTypeName(String typeName) {
        var localSingle = toUserPrivateLocalName(typeName, "__local_single_", false);
        return toUserPrivateLocalName(localSingle, "__local_type_", true);
    }

    private String restorePrivateFunctionNameForDisplay(String functionName) {
        var restoredLocalFunction = toUserPrivateLocalName(functionName, "__local_fun_", false);
        return toUserPrivateLocalName(restoredLocalFunction, "__local_const_", true);
    }

    private String displayFunctionName(String functionName) {
        var constructorTarget = constructorTargetTypeName(functionName);
        if (constructorTarget.isPresent()) {
            return restorePrivateTypeNameForDisplay(constructorTarget.orElseThrow().targetTypeName());
        }
        return restorePrivateFunctionNameForDisplay(functionName);
    }

    private String displaySignatureName(String functionName) {
        var owner = methodOwnerType(functionName);
        var method = methodSimpleName(functionName);
        if (owner.isPresent() && method.isPresent()) {
            return owner.orElseThrow() + "." + method.orElseThrow();
        }
        return displayFunctionName(functionName);
    }

    private String normalizeUserVisibleNames(String message) {
        var expectedFoundMatcher = java.util.regex.Pattern.compile("^Expected `([^`]+)`, (but )?got `([^`]+)`$").matcher(message);
        if (expectedFoundMatcher.matches()) {
            var expected = normalizeReportedTypeName(expectedFoundMatcher.group(1));
            var got = normalizeReportedTypeName(expectedFoundMatcher.group(3));
            var separator = expectedFoundMatcher.group(2) == null ? ", got `" : ", but got `";
            message = "Expected `" + expected + "`" + separator + got + "`";
        }
        var restoredLocalFunctions = replacePrivateLocalNamesInText(message, "__local_fun_", false);
        var restoredLocalConsts = replacePrivateLocalNamesInText(restoredLocalFunctions, "__local_const_", true);
        var restoredLocalSingles = replacePrivateLocalNamesInText(restoredLocalConsts, "__local_single_", false);
        return replacePrivateLocalNamesInText(restoredLocalSingles, "__local_type_", true);
    }

    private String replacePrivateLocalNamesInText(String text, String marker, boolean withPrivatePrefix) {
        var normalized = new StringBuilder(text.length());
        var cursor = 0;
        while (cursor < text.length()) {
            var markerIndex = text.indexOf(marker, cursor);
            if (markerIndex < 0) {
                normalized.append(text, cursor, text.length());
                break;
            }
            var qualifiedNameStart = findQualifiedLocalNameStart(text, markerIndex);
            if (qualifiedNameStart < cursor) {
                normalized.append(text, cursor, markerIndex + marker.length());
                cursor = markerIndex + marker.length();
                continue;
            }
            var localSuffixStart = markerIndex + marker.length();
            var localSuffixEnd = localSuffixStart;
            while (localSuffixEnd < text.length() && isLocalNameChar(text.charAt(localSuffixEnd))) {
                localSuffixEnd++;
            }
            var localSuffix = text.substring(localSuffixStart, localSuffixEnd);
            var underscoreIndex = localSuffix.indexOf('_');
            if (underscoreIndex < 0 || underscoreIndex + 1 >= localSuffix.length()) {
                normalized.append(text, cursor, localSuffixEnd);
                cursor = localSuffixEnd;
                continue;
            }
            normalized.append(text, cursor, qualifiedNameStart);
            if (withPrivatePrefix) {
                normalized.append("__");
            }
            normalized.append(localSuffix.substring(underscoreIndex + 1));
            cursor = localSuffixEnd;
        }
        return normalized.toString();
    }

    private int findQualifiedLocalNameStart(String text, int markerIndex) {
        var start = markerIndex;
        while (start > 0 && isLocalNameChar(text.charAt(start - 1))) {
            start--;
        }
        if (start + 1 >= text.length() || text.charAt(start) != '_' || text.charAt(start + 1) != '_') {
            return -1;
        }
        return start;
    }

    private boolean isLocalNameChar(char c) {
        return Character.isLetterOrDigit(c) || c == '_';
    }

    private String toUserPrivateLocalName(String name, String marker, boolean withPrivatePrefix) {
        var idx = name.indexOf(marker);
        if (idx < 0) {
            return name;
        }
        if (findQualifiedLocalNameStart(name, idx) != 0) {
            return name;
        }
        var suffix = name.substring(idx + marker.length());
        var underscoreIdx = suffix.indexOf('_');
        if (underscoreIdx < 0 || underscoreIdx + 1 >= suffix.length()) {
            return name;
        }
        return (withPrivatePrefix ? "__" : "") + suffix.substring(underscoreIdx + 1);
    }

    private String privateTypeEscapingFunctionSignatureMessage(
            Function function,
            String moduleSourceFile,
            int line,
            int column,
            String escapedType
    ) {
        var functionPreview = formatFunctionHeaderWithRestoredPrivateTypes(function) + " = ...";
        var pointer = " ".repeat(Math.max(column, 0))
                      + "^ Private type `" + toUserPrivateTypeName(escapedType) + "` cannot be used in function signature";
        return "error: mismatched types\n"
               + " --> " + normalizeFile(moduleSourceFile) + ":" + line + ":" + column + "\n"
               + functionPreview + "\n"
               + pointer + "\n";
    }

    private String formatFunctionHeaderWithRestoredPrivateTypes(Function function) {
        var header = new StringBuilder(functionKeyword(function))
                .append(" ")
                .append(displayFunctionName(function.name()))
                .append("(")
                .append(function.parameters().stream()
                        .map(parameter -> parameter.name() + ": " + formatParserTypeForPosition(parameter.type()))
                        .collect(java.util.stream.Collectors.joining(", ")))
                .append(")");
        function.returnType().ifPresent(type -> header.append(": ").append(formatParserTypeForPosition(type)));
        return header.toString();
    }

    private List<CapybaraExpressionCompiler.FunctionSignature> signaturesFromLinkedFunctions(List<CompiledFunction> functions) {
        return functions.stream()
                .map(function -> new CapybaraExpressionCompiler.FunctionSignature(
                        function.name(),
                        function.parameters().stream().map(CompiledFunctionParameter::type).toList(),
                        function.parameters().stream().map(CompiledFunctionParameter::name).toList(),
                        function.returnType()
                ))
                .toList();
    }

    private Result<List<CompiledFunctionParameter>> linkParameters(List<Parameter> parameters, Map<String, GenericDataType> dataTypes) {
        return linkParameters(parameters, dataTypes, Set.of(), new CompileCache());
    }

    private Result<List<CompiledFunctionParameter>> linkParameters(
            List<Parameter> parameters,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames,
            CompileCache compileCache
    ) {
        return parameters.stream()
                .map(p -> linkParameter(p, dataTypes, functionGenericTypeNames, compileCache))
                .collect(new ResultCollectionCollector<>());
    }

    private Result<CompiledFunctionParameter> linkParameter(Parameter parameter, Map<String, GenericDataType> dataTypes) {
        return linkParameter(parameter, dataTypes, Set.of(), new CompileCache());
    }

    private Result<CompiledFunctionParameter> linkParameter(
            Parameter parameter,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames,
            CompileCache compileCache
    ) {
        return withPosition(
                linkType(parameter.type(), dataTypes, functionGenericTypeNames, compileCache)
                        .map(type -> new CompiledFunctionParameter(parameter.name(), type)),
                parameter.position(),
                "");
    }

    private Result<CompiledType> linkType(
            Type type,
            Map<String, GenericDataType> dataTypes
    ) {
        return linkType(type, dataTypes, new CompileCache());
    }

    private Result<CompiledType> linkType(
            Type type,
            Map<String, GenericDataType> dataTypes,
            CompileCache compileCache
    ) {
        var linkCache = compileCache.typeLinkCaches.computeIfAbsent(
                dataTypes,
                ignored -> new CapybaraTypeCompiler.LinkCache()
        );
        return CapybaraTypeCompiler.linkType(type, dataTypes, linkCache);
    }

    private Type compiledTypeToParserType(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> switch (primitive) {
                case BYTE -> PrimitiveType.BYTE;
                case INT -> PrimitiveType.INT;
                case LONG -> PrimitiveType.LONG;
                case DOUBLE -> PrimitiveType.DOUBLE;
                case BOOL -> PrimitiveType.BOOL;
                case STRING -> PrimitiveType.STRING;
                case FLOAT -> PrimitiveType.FLOAT;
                case ANY -> PrimitiveType.ANY;
                case NOTHING -> PrimitiveType.NOTHING;
                case DATA -> PrimitiveType.DATA;
                case ENUM -> PrimitiveType.ENUM;
            };
            case CollectionLinkedType.CompiledList compiledList ->
                    new CollectionType.ListType(compiledTypeToParserType(compiledList.elementType()));
            case CollectionLinkedType.CompiledSet compiledSet ->
                    new CollectionType.SetType(compiledTypeToParserType(compiledSet.elementType()));
            case CollectionLinkedType.CompiledDict compiledDict ->
                    new CollectionType.DictType(compiledTypeToParserType(compiledDict.valueType()));
            case CompiledTupleType tupleType -> new TupleType(
                    tupleType.elementTypes().stream().map(this::compiledTypeToParserType).toList()
            );
            case CompiledFunctionType functionType -> new FunctionType(
                    compiledTypeToParserType(functionType.argumentType()),
                    compiledTypeToParserType(functionType.returnType())
            );
            case CompiledGenericTypeParameter genericTypeParameter -> new DataType(genericTypeParameter.name());
            case CompiledDataType compiledDataType -> compiledDataType.typeParameters().isEmpty()
                    ? new DataType(compiledDataType.name())
                    : new DataType(compiledDataType.name() + "["
                                   + String.join(", ", compiledDataType.typeParameters())
                                   + "]");
            case CompiledDataParentType compiledDataParentType -> compiledDataParentType.typeParameters().isEmpty()
                    ? new DataType(compiledDataParentType.name())
                    : new DataType(compiledDataParentType.name() + "["
                                   + String.join(", ", compiledDataParentType.typeParameters())
                                   + "]");
        };
    }

    private Result<CompiledType> linkType(
            Type type,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames,
            CompileCache compileCache
    ) {
        if (functionGenericTypeNames.isEmpty()) {
            return linkType(type, dataTypes, compileCache);
        }
        var linkedTypesByGenerics = compileCache.genericTypeLinkCaches
                .computeIfAbsent(dataTypes, ignored -> new HashMap<>())
                .computeIfAbsent(functionGenericTypeNames, ignored -> new HashMap<>());
        var cacheKey = typeCacheKey(type);
        var cached = linkedTypesByGenerics.get(cacheKey);
        if (cached != null) {
            return cached;
        }
        Result<CompiledType> linked = switch (type) {
            case PrimitiveType primitiveType -> Result.success(switch (primitiveType) {
                case BYTE -> PrimitiveLinkedType.BYTE;
                case INT -> PrimitiveLinkedType.INT;
                case LONG -> PrimitiveLinkedType.LONG;
                case DOUBLE -> PrimitiveLinkedType.DOUBLE;
                case STRING -> PrimitiveLinkedType.STRING;
                case BOOL -> PrimitiveLinkedType.BOOL;
                case FLOAT -> PrimitiveLinkedType.FLOAT;
                case ANY -> PrimitiveLinkedType.ANY;
                case DATA -> PrimitiveLinkedType.DATA;
                case ENUM -> PrimitiveLinkedType.ENUM;
                case NOTHING -> PrimitiveLinkedType.NOTHING;
            });
            case CollectionType.ListType listType ->
                    linkType(listType.elementType(), dataTypes, functionGenericTypeNames, compileCache)
                            .map(CollectionLinkedType.CompiledList::new)
                            .map(CompiledType.class::cast);
            case CollectionType.SetType setType -> linkType(setType.elementType(), dataTypes, functionGenericTypeNames, compileCache)
                    .map(CollectionLinkedType.CompiledSet::new)
                    .map(CompiledType.class::cast);
            case CollectionType.DictType dictType -> linkType(dictType.valueType(), dataTypes, functionGenericTypeNames, compileCache)
                    .map(CollectionLinkedType.CompiledDict::new)
                    .map(CompiledType.class::cast);
            case FunctionType functionType -> linkType(functionType.argumentType(), dataTypes, functionGenericTypeNames, compileCache)
                    .flatMap(argumentType -> linkType(functionType.returnType(), dataTypes, functionGenericTypeNames, compileCache)
                            .map(returnType -> (CompiledType) new CompiledFunctionType(argumentType, returnType)));
            case TupleType tupleType -> tupleType.elementTypes().stream()
                    .map(elementType -> linkType(elementType, dataTypes, functionGenericTypeNames, compileCache))
                    .collect(new ResultCollectionCollector<>())
                    .map(linkedTypes -> (CompiledType) new CompiledTupleType(linkedTypes));
            case DataType dataType ->
                    linkDataTypeWithFunctionGenerics(dataType.name(), dataTypes, functionGenericTypeNames, compileCache);
        };
        linkedTypesByGenerics.put(cacheKey, linked);
        return linked;
    }

    private Result<CompiledType> linkDataTypeWithFunctionGenerics(
            String rawTypeName,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames,
            CompileCache compileCache
    ) {
        var parsed = parseGenericTypeName(rawTypeName, compileCache);
        if (parsed.typeArguments().isEmpty() && functionGenericTypeNames.contains(parsed.baseName())) {
            return Result.success(new CompiledGenericTypeParameter(parsed.baseName()));
        }

        var linkedBase = linkType(new DataType(parsed.baseName()), dataTypes, compileCache);
        if (linkedBase instanceof Result.Error<CompiledType> error) {
            return new Result.Error<>(error.errors());
        }
        var baseType = ((Result.Success<CompiledType>) linkedBase).value();
        if (parsed.typeArguments().isEmpty()) {
            return Result.success(baseType);
        }

        return parsed.typeArguments().stream()
                .map(argument -> linkType(parseTypeArgument(argument, compileCache), dataTypes, functionGenericTypeNames, compileCache))
                .collect(new ResultCollectionCollector<>())
                .map(arguments -> instantiateTypeArguments(baseType, arguments));
    }

    private String typeCacheKey(Type type) {
        return switch (type) {
            case PrimitiveType primitiveType -> primitiveType.name();
            case CollectionType.ListType listType -> "list[" + typeCacheKey(listType.elementType()) + "]";
            case CollectionType.SetType setType -> "set[" + typeCacheKey(setType.elementType()) + "]";
            case CollectionType.DictType dictType -> "dict[" + typeCacheKey(dictType.valueType()) + "]";
            case DataType dataType -> dataType.name();
            case FunctionType functionType ->
                    "(" + typeCacheKey(functionType.argumentType()) + " => " + typeCacheKey(functionType.returnType()) + ")";
            case TupleType tupleType -> "tuple[" + tupleType.elementTypes().stream()
                    .map(this::typeCacheKey)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
        };
    }

    private CompiledType instantiateTypeArguments(CompiledType linkedType, List<CompiledType> typeArguments) {
        var mappedTypeArguments = typeArguments.stream().map(this::typeDescriptor).toList();
        return switch (linkedType) {
            case CompiledDataParentType parentType -> {
                var substitutions = new LinkedHashMap<String, CompiledType>();
                var max = Math.min(parentType.typeParameters().size(), typeArguments.size());
                for (int i = 0; i < max; i++) {
                    substitutions.put(parentType.typeParameters().get(i), typeArguments.get(i));
                }
                yield new CompiledDataParentType(
                        parentType.name(),
                        parentType.fields().stream()
                                .map(field -> new CompiledDataType.CompiledField(
                                        field.name(),
                                        substituteTypeParameters(field.type(), substitutions)
                                ))
                                .toList(),
                        parentType.subTypes().stream()
                                .map(subType -> (CompiledDataType) substituteTypeParameters(subType, substitutions))
                                .toList(),
                        mappedTypeArguments,
                        parentType.enumType()
                );
            }
            case CompiledDataType dataType -> {
                if (dataType.typeParameters().isEmpty()) {
                    yield new CompiledDataType(
                            dataType.name(),
                            dataType.fields(),
                            mappedTypeArguments,
                            dataType.extendedTypes(),
                            dataType.comments(),
                            dataType.visibility(),
                            dataType.singleton(),
                            dataType.enumValue()
                    );
                }
                var substitutions = new LinkedHashMap<String, CompiledType>();
                var max = Math.min(dataType.typeParameters().size(), typeArguments.size());
                for (int i = 0; i < max; i++) {
                    substitutions.put(dataType.typeParameters().get(i), typeArguments.get(i));
                }
                var substitutedFields = dataType.fields().stream()
                        .map(field -> new CompiledDataType.CompiledField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                        .toList();
                yield new CompiledDataType(
                        dataType.name(),
                        substitutedFields,
                        mappedTypeArguments,
                        dataType.extendedTypes(),
                        dataType.comments(),
                        dataType.visibility(),
                        dataType.singleton(),
                        dataType.enumValue()
                );
            }
            default -> linkedType;
        };
    }

    private CompiledType substituteTypeParameters(CompiledType type, Map<String, CompiledType> substitutions) {
        if (type instanceof CompiledGenericTypeParameter genericTypeParameter) {
            return substitutions.getOrDefault(genericTypeParameter.name(), type);
        }
        return switch (type) {
            case CollectionLinkedType.CompiledList linkedList -> new CollectionLinkedType.CompiledList(
                    substituteTypeParameters(linkedList.elementType(), substitutions));
            case CollectionLinkedType.CompiledSet linkedSet -> new CollectionLinkedType.CompiledSet(
                    substituteTypeParameters(linkedSet.elementType(), substitutions));
            case CollectionLinkedType.CompiledDict linkedDict -> new CollectionLinkedType.CompiledDict(
                    substituteTypeParameters(linkedDict.valueType(), substitutions));
            case CompiledFunctionType functionType -> new CompiledFunctionType(
                    substituteTypeParameters(functionType.argumentType(), substitutions),
                    substituteTypeParameters(functionType.returnType(), substitutions)
            );
            case CompiledTupleType linkedTupleType -> new CompiledTupleType(
                    linkedTupleType.elementTypes().stream()
                            .map(elementType -> substituteTypeParameters(elementType, substitutions))
                            .toList()
            );
            default -> type;
        };
    }

    private String typeDescriptor(CompiledType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase();
            case CollectionLinkedType.CompiledList linkedList ->
                    "list[" + typeDescriptor(linkedList.elementType()) + "]";
            case CollectionLinkedType.CompiledSet linkedSet -> "set[" + typeDescriptor(linkedSet.elementType()) + "]";
            case CollectionLinkedType.CompiledDict linkedDict -> "dict[" + typeDescriptor(linkedDict.valueType()) + "]";
            case CompiledTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(this::typeDescriptor)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case CompiledFunctionType linkedFunctionType ->
                    "(" + typeDescriptor(linkedFunctionType.argumentType()) + " => " + typeDescriptor(linkedFunctionType.returnType()) + ")";
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? linkedDataType.name()
                    : linkedDataType.name() + "[" + String.join(", ", linkedDataType.typeParameters()) + "]";
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? linkedDataParentType.name()
                    : linkedDataParentType.name() + "[" + String.join(", ", linkedDataParentType.typeParameters()) + "]";
            case CompiledGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private ParsedGenericTypeName parseGenericTypeName(String rawName, CompileCache compileCache) {
        var cached = compileCache.parsedGenericTypeNames.get(rawName);
        if (cached != null) {
            return cached;
        }
        var idx = rawName.indexOf('[');
        if (idx <= 0 || !rawName.endsWith("]")) {
            var parsed = new ParsedGenericTypeName(rawName, List.of());
            compileCache.parsedGenericTypeNames.put(rawName, parsed);
            return parsed;
        }
        var baseName = rawName.substring(0, idx);
        var argsContent = rawName.substring(idx + 1, rawName.length() - 1);
        var parsed = new ParsedGenericTypeName(baseName, splitTopLevelTypeArguments(argsContent, compileCache));
        compileCache.parsedGenericTypeNames.put(rawName, parsed);
        return parsed;
    }

    private ParsedGenericTypeName parseGenericTypeName(String rawName) {
        return parseGenericTypeName(rawName, new CompileCache());
    }

    private List<String> splitTopLevelTypeArguments(String content, CompileCache compileCache) {
        var cached = compileCache.splitTopLevelGenericTypeArguments.get(content);
        if (cached != null) {
            return cached;
        }
        var result = new ArrayList<String>();
        var depthSquare = 0;
        var depthParen = 0;
        var current = new StringBuilder();
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch == '[') {
                depthSquare++;
                current.append(ch);
                continue;
            }
            if (ch == ']') {
                depthSquare = Math.max(0, depthSquare - 1);
                current.append(ch);
                continue;
            }
            if (ch == '(') {
                depthParen++;
                current.append(ch);
                continue;
            }
            if (ch == ')') {
                depthParen = Math.max(0, depthParen - 1);
                current.append(ch);
                continue;
            }
            if (ch == ',' && depthSquare == 0 && depthParen == 0) {
                var token = current.toString().trim();
                if (!token.isEmpty()) {
                    result.add(token);
                }
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        var token = current.toString().trim();
        if (!token.isEmpty()) {
            result.add(token);
        }
        var parsed = List.copyOf(result);
        compileCache.splitTopLevelGenericTypeArguments.put(content, parsed);
        return parsed;
    }

    private List<String> splitTopLevelTypeArguments(String content) {
        return splitTopLevelTypeArguments(content, new CompileCache());
    }

    private Type parseTypeArgument(String raw, CompileCache compileCache) {
        var cached = compileCache.parsedGenericTypeArguments.get(raw);
        if (cached != null) {
            return cached;
        }
        var trimmed = raw.trim();
        var parsed = PrimitiveType.find(trimmed)
                .map(Type.class::cast)
                .orElseGet(() -> {
                    if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
                        return new CollectionType.ListType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1), compileCache));
                    }
                    if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
                        return new CollectionType.SetType(parseTypeArgument(trimmed.substring(4, trimmed.length() - 1), compileCache));
                    }
                    if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
                        return new CollectionType.DictType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1), compileCache));
                    }
                    if (trimmed.startsWith("tuple[") && trimmed.endsWith("]")) {
                        var inner = trimmed.substring(6, trimmed.length() - 1);
                        var elements = splitTopLevelTypeArguments(inner, compileCache).stream()
                                .map(argument -> parseTypeArgument(argument, compileCache))
                                .toList();
                        return new TupleType(elements);
                    }
                    var arrowIndex = indexOfTopLevelArrow(trimmed, "=>");
                    if (arrowIndex > 0) {
                        var left = trimmed.substring(0, arrowIndex).trim();
                        var right = trimmed.substring(arrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left), compileCache), parseTypeArgument(right, compileCache));
                    }
                    arrowIndex = indexOfTopLevelArrow(trimmed, "->");
                    if (arrowIndex > 0) {
                        var left = trimmed.substring(0, arrowIndex).trim();
                        var right = trimmed.substring(arrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left), compileCache), parseTypeArgument(right, compileCache));
                    }
                    return new DataType(trimmed);
                });
        compileCache.parsedGenericTypeArguments.put(raw, parsed);
        return parsed;
    }

    private Type parseTypeArgument(String raw) {
        return parseTypeArgument(raw, new CompileCache());
    }

    private int indexOfTopLevelArrow(String value, String arrow) {
        var square = 0;
        var paren = 0;
        for (int i = 0; i < value.length() - 1; i++) {
            var ch = value.charAt(i);
            if (ch == '[') {
                square++;
                continue;
            }
            if (ch == ']') {
                square = Math.max(0, square - 1);
                continue;
            }
            if (ch == '(') {
                paren++;
                continue;
            }
            if (ch == ')') {
                paren = Math.max(0, paren - 1);
                continue;
            }
            if (square == 0 && paren == 0 && value.startsWith(arrow, i)) {
                return i;
            }
        }
        return -1;
    }

    private String stripOptionalParentheses(String value) {
        var trimmed = value.trim();
        if (!trimmed.startsWith("(") || !trimmed.endsWith(")")) {
            return trimmed;
        }
        var depth = 0;
        for (int i = 0; i < trimmed.length(); i++) {
            var ch = trimmed.charAt(i);
            if (ch == '(') {
                depth++;
            } else if (ch == ')') {
                depth--;
            }
            if (depth == 0 && i < trimmed.length() - 1) {
                return trimmed;
            }
        }
        return trimmed.substring(1, trimmed.length() - 1).trim();
    }

    private Set<String> functionGenericTypeNames(Function function, Map<String, GenericDataType> dataTypes, CompileCache compileCache) {
        var cached = compileCache.functionGenericTypeNamesByFunction.get(function);
        if (cached != null) {
            return cached;
        }
        var names = new LinkedHashSet<String>();
        function.parameters().forEach(parameter -> collectFunctionGenericTypeNames(parameter.type(), dataTypes, names));
        function.returnType().ifPresent(type -> collectFunctionGenericTypeNames(type, dataTypes, names));
        var resolved = Set.copyOf(names);
        compileCache.functionGenericTypeNamesByFunction.put(function, resolved);
        return resolved;
    }

    private void collectFunctionGenericTypeNames(
            Type type,
            Map<String, GenericDataType> dataTypes,
            Set<String> names
    ) {
        switch (type) {
            case CollectionType.ListType listType ->
                    collectFunctionGenericTypeNames(listType.elementType(), dataTypes, names);
            case CollectionType.SetType setType ->
                    collectFunctionGenericTypeNames(setType.elementType(), dataTypes, names);
            case CollectionType.DictType dictType ->
                    collectFunctionGenericTypeNames(dictType.valueType(), dataTypes, names);
            case FunctionType functionType -> {
                collectFunctionGenericTypeNames(functionType.argumentType(), dataTypes, names);
                collectFunctionGenericTypeNames(functionType.returnType(), dataTypes, names);
            }
            case TupleType tupleType -> tupleType.elementTypes()
                    .forEach(elementType -> collectFunctionGenericTypeNames(elementType, dataTypes, names));
            case DataType dataType -> {
                var parsed = parseGenericTypeName(dataType.name());
                if (isFunctionGenericName(parsed.baseName()) && !isKnownTypeName(parsed.baseName(), dataTypes)) {
                    names.add(parsed.baseName());
                }
                parsed.typeArguments().stream()
                        .map(this::parseTypeArgument)
                        .forEach(argType -> collectFunctionGenericTypeNames(argType, dataTypes, names));
            }
            default -> {
            }
        }
    }

    private boolean isFunctionGenericName(String name) {
        return name.length() == 1 && Character.isUpperCase(name.charAt(0));
    }

    private boolean isKnownTypeName(String typeName, Map<String, GenericDataType> dataTypes) {
        return CapybaraTypeCompiler.linkType(new DataType(typeName), dataTypes) instanceof Result.Success<CompiledType>;
    }

    private CompiledDataParentType resultTypeForData(CompiledDataType dataType, Map<String, GenericDataType> dataTypes) {
        var resultParent = dataTypes.values().stream()
                .filter(CompiledDataParentType.class::isInstance)
                .map(CompiledDataParentType.class::cast)
                .filter(parentType -> "Result".equals(parentType.name()) || normalizeQualifiedTypeName(parentType.name()).endsWith("/Result"))
                .findFirst()
                .orElse(null);
        if (resultParent == null) {
            return null;
        }
        return new CompiledDataParentType(
                resultParent.name(),
                resultParent.fields(),
                resultParent.subTypes(),
                List.of(dataType.name(), PrimitiveLinkedType.STRING.name()),
                resultParent.comments(),
                resultParent.visibility(),
                resultParent.enumType()
        );
    }

    private record ParsedGenericTypeName(String baseName, List<String> typeArguments) {
    }

    private static final class CompileCache {
        private final Map<SignatureVisibilityCacheKey, List<CapybaraExpressionCompiler.FunctionSignature>> visibleSignaturesByScope = new HashMap<>();
        private final Map<VisibilityCacheKey, SortedMap<String, GenericDataType>> visibleTypesByScope = new HashMap<>();
        private final Map<SignatureVisibilityCacheKey, Map<String, List<CapybaraExpressionCompiler.FunctionSignature>>> visibleSignaturesByNameByScope = new HashMap<>();
        // CompileCache is compile-local; modulesByName and linkedTypesByModule are invariant for its lifetime.
        private final IdentityHashMap<Map<String, List<CapybaraExpressionCompiler.FunctionSignature>>, Map<ModuleCacheKey, Result<List<CapybaraExpressionCompiler.FunctionSignature>>>> availableSignaturesByModulePhase = new IdentityHashMap<>();
        private final IdentityHashMap<Map<String, List<CapybaraExpressionCompiler.FunctionSignature>>, Map<ModuleCacheKey, SortedSet<CompiledModule.StaticImport>>> staticImportsByModulePhase = new IdentityHashMap<>();
        private final IdentityHashMap<Map<String, GenericDataType>, CapybaraExpressionCompiler.LinkCache> expressionLinkCaches = new IdentityHashMap<>();
        private final IdentityHashMap<Map<String, GenericDataType>, CapybaraTypeCompiler.LinkCache> typeLinkCaches = new IdentityHashMap<>();
        private final IdentityHashMap<Map<String, GenericDataType>, Map<Set<String>, Map<String, Result<CompiledType>>>> genericTypeLinkCaches = new IdentityHashMap<>();
        private final IdentityHashMap<Function, Set<String>> functionGenericTypeNamesByFunction = new IdentityHashMap<>();
        private final Map<String, ParsedGenericTypeName> parsedGenericTypeNames = new HashMap<>();
        private final Map<String, Type> parsedGenericTypeArguments = new HashMap<>();
        private final Map<String, List<String>> splitTopLevelGenericTypeArguments = new HashMap<>();
        private long staticImportGenerationNanos;
    }

    private record VisibilityCacheKey(String currentModulePath, String ownerModuleName, String ownerModulePath) {
    }

    private record SignatureVisibilityCacheKey(
            String currentModulePath,
            String ownerModuleName,
            String ownerModulePath,
            int signaturesIdentity
    ) {
    }

    private record ModuleCacheKey(
            String modulePath,
            String moduleName
    ) {
    }

    private Result<SortedMap<String, GenericDataType>> types(Module module) {
        var normalizedFile = normalizeFile(moduleSourceFile(module));
        var rawTypeDeclarations = castList(module, TypeDeclaration.class);
        var rawEnumDeclarations = castList(module, EnumDeclaration.class);
        var enumDeclarations = rawEnumDeclarations.stream()
                .map(this::linkEnumDeclaration)
                .toList();
        var enumDeclarationsByName = enumDeclarations.stream()
                .collect(toMap(CompiledDataParentType::name, identity(), (first, second) -> first));
        var rawTypeDeclarationsByName = rawTypeDeclarations.stream()
                .collect(toMap(TypeDeclaration::name, identity(), (first, second) -> first));
        var dataDeclarationsOrError = linkDataDeclarations(
                castList(module, DataDeclaration.class),
                rawTypeDeclarationsByName,
                enumDeclarationsByName,
                module.imports(),
                normalizedFile
        );
        var singlesDeclarationsOrError = castList(module, SingleDeclaration.class)
                .stream()
                .map(this::linkSingleDeclaration)
                .collect(new ResultCollectionCollector<>());

        if (dataDeclarationsOrError instanceof Result.Error<?> error) {
            return new Result.Error<>(error.errors());
        }
        var dataDeclarations = ((Result.Success<List<CompiledDataType>>) dataDeclarationsOrError).value();
        if (singlesDeclarationsOrError instanceof Result.Error<?> error) {
            return new Result.Error<>(error.errors());
        }
        var singleDeclarations = ((Result.Success<List<CompiledDataType>>) singlesDeclarationsOrError).value();
        Map<String, GenericDataType> knownDataTypes = new HashMap<>();
        dataDeclarations.forEach(dataType -> knownDataTypes.put(dataType.name(), dataType));
        singleDeclarations.forEach(dataType -> knownDataTypes.put(dataType.name(), dataType));
        enumDeclarations.forEach(enumType -> knownDataTypes.put(enumType.name(), enumType));

        var typeDeclarationsOrError = rawTypeDeclarations
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(
                        typeDeclaration,
                        Stream.concat(
                                Stream.concat(dataDeclarations.stream(), singleDeclarations.stream()),
                                enumDeclarations.stream().flatMap(enumType -> enumType.subTypes().stream())
                        ).toList(),
                        rawTypeDeclarationsByName,
                        knownDataTypes,
                        normalizedFile))
                .collect(new ResultCollectionCollector<>());

        if (typeDeclarationsOrError instanceof Result.Error<?> error) {
            return new Result.Error<>(error.errors());
        }
        var typeDeclarations = ((Result.Success<List<CompiledDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(singleDeclarations);
        set.addAll(enumDeclarations);
        set.addAll(typeDeclarations);
        var map = new TreeMap<>(set.stream().collect(toMap(GenericDataType::name, identity())));
        typeDeclarations.forEach(parentType -> parentType.subTypes().forEach(subType -> map.put(subType.name(), subType)));
        enumDeclarations.forEach(enumType -> enumType.subTypes().forEach(subType -> map.put(subType.name(), subType)));
        rawTypeDeclarations.stream()
                .filter(typeDeclaration -> typeDeclaration.constructor().isPresent())
                .forEach(typeDeclaration -> {
                    var linkedType = typeDeclarations.stream()
                            .filter(parentType -> parentType.name().equals(typeDeclaration.name()))
                            .findFirst();
                    if (linkedType.isPresent()) {
                        map.put(
                                constructorStateTypeName(typeDeclaration.name()),
                                constructorStateType(linkedType.orElseThrow())
                        );
                    }
                });
        return Result.success(map);
    }

    private Result<List<CompiledDataType>> linkDataDeclarations(List<DataDeclaration> dataDeclarations) {
        return linkDataDeclarations(dataDeclarations, Map.of(), Map.of(), List.of(), "");
    }

    private Result<List<CompiledDataType>> linkDataDeclarations(
            List<DataDeclaration> dataDeclarations,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, ? extends GenericDataType> additionalKnownTypes,
            List<ImportDeclaration> importDeclarations,
            String normalizedFile
    ) {
        var declarationsByName = dataDeclarations.stream()
                .collect(toMap(DataDeclaration::name, identity(), (first, second) -> first));
        var cache = new HashMap<String, Result<CompiledDataType>>();
        return dataDeclarations.stream()
                .map(dataDeclaration -> linkDataDeclaration(
                        dataDeclaration,
                        declarationsByName,
                        rawTypeDeclarationsByName,
                        additionalKnownTypes,
                        importDeclarations,
                        cache,
                        new HashSet<>(),
                        normalizedFile))
                .collect(new ResultCollectionCollector<>());
    }

    private Result<CompiledDataType> linkDataDeclaration(
            DataDeclaration dataDeclaration,
            Map<String, DataDeclaration> declarationsByName,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, ? extends GenericDataType> additionalKnownTypes,
            List<ImportDeclaration> importDeclarations,
            Map<String, Result<CompiledDataType>> cache,
            Set<String> visiting,
            String normalizedFile
    ) {
        var cached = cache.get(dataDeclaration.name());
        if (cached != null) {
            return cached;
        }
        if (!visiting.add(dataDeclaration.name())) {
            return withPosition(
                    Result.error("Circular data extension detected for `" + dataDeclaration.name() + "`"),
                    dataDeclaration.position(),
                    normalizedFile);
        }

        var genericTypes = Set.copyOf(dataDeclaration.typeParameters());
        var inheritedFields = dataDeclaration.extendsTypes().stream()
                .map(parentName -> {
                    var parent = declarationsByName.get(parentName);
                    if (parent == null) {
                        return Result.<List<CompiledDataType.CompiledField>>error(
                                "Extended data type `" + parentName + "` not found"
                        );
                    }
                    return linkDataDeclaration(
                            parent,
                            declarationsByName,
                            rawTypeDeclarationsByName,
                            additionalKnownTypes,
                            importDeclarations,
                            cache,
                            visiting,
                            normalizedFile)
                            .map(CompiledDataType::fields);
                })
                .collect(new ResultCollectionCollector<>());
        var ownFields = dataDeclaration.fields().stream()
                .map(field -> linkField(
                        field,
                        genericTypes,
                        declarationsByName,
                        rawTypeDeclarationsByName,
                        additionalKnownTypes,
                        importDeclarations,
                        cache,
                        visiting,
                        normalizedFile))
                .collect(new ResultCollectionCollector<>());
        var linked = Result.join(
                LinkedDataFields::new,
                inheritedFields,
                ownFields
        ).flatMap(linkedFields -> {
            var fields = new ArrayList<CompiledDataType.CompiledField>();
            var fieldOrigins = new LinkedHashMap<String, FieldOrigin>();
            for (var i = 0; i < dataDeclaration.extendsTypes().size(); i++) {
                var parentName = dataDeclaration.extendsTypes().get(i);
                var parentFields = linkedFields.inherited().get(i);
                var duplicateError = mergeDataFields(fields, fieldOrigins, parentFields, parentName, dataDeclaration.name());
                if (duplicateError != null) {
                    return Result.error(duplicateError);
                }
            }
            var ownDuplicateError = mergeDataFields(fields, fieldOrigins, linkedFields.own(), dataDeclaration.name(), dataDeclaration.name());
            if (ownDuplicateError != null) {
                return Result.error(ownDuplicateError);
            }
            return Result.success(new CompiledDataType(
                    dataDeclaration.name(),
                    List.copyOf(fields),
                    dataDeclaration.typeParameters(),
                    dataDeclaration.extendsTypes(),
                    dataDeclaration.comments(),
                    dataDeclaration.visibility(),
                    false
            ));
        });
        visiting.remove(dataDeclaration.name());
        var withPosition = withPosition(linked, dataDeclaration.position(), normalizedFile);
        cache.put(dataDeclaration.name(), withPosition);
        return withPosition;
    }

    private String mergeDataFields(
            List<CompiledDataType.CompiledField> mergedFields,
            Map<String, FieldOrigin> fieldOrigins,
            List<CompiledDataType.CompiledField> candidateFields,
            String ownerName,
            String dataName
    ) {
        for (var field : candidateFields) {
            var existing = fieldOrigins.get(field.name());
            if (existing == null) {
                mergedFields.add(field);
                fieldOrigins.put(field.name(), new FieldOrigin(ownerName, field.type()));
                continue;
            }
            if (!existing.type().equals(field.type())) {
                return "Conflicting inherited field `%s` for data `%s`: `%s.%s` has type `%s` and `%s.%s` has type `%s`"
                        .formatted(
                                field.name(),
                                dataName,
                                existing.ownerName(),
                                field.name(),
                                existing.type(),
                                ownerName,
                                field.name(),
                                field.type()
                        );
            }
        }
        return null;
    }

    private Result<CompiledDataType> linkSingleDeclaration(SingleDeclaration singleDeclaration) {
        return Result.success(new CompiledDataType(singleDeclaration.name(), List.of(), List.of(), List.of(), true));
    }

    private CompiledDataParentType linkEnumDeclaration(EnumDeclaration enumDeclaration) {
        var values = enumDeclaration.values().stream()
                .map(value -> new CompiledDataType(value, List.of(), List.of(), List.of(), List.of(), null, true, true))
                .toList();
        return new CompiledDataParentType(
                enumDeclaration.name(),
                List.of(),
                values,
                List.of(),
                true
        );
    }

    private Result<CompiledDataType.CompiledField> linkField(
            DataDeclaration.DataField type,
            Set<String> genericTypes,
            Map<String, DataDeclaration> declarationsByName,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, ? extends GenericDataType> additionalKnownTypes,
            List<ImportDeclaration> importDeclarations,
            Map<String, Result<CompiledDataType>> cache,
            Set<String> visiting,
            String normalizedFile
    ) {
        if (type.type() instanceof DataType dataType && genericTypes.contains(dataType.name())) {
            return Result.success(new CompiledDataType.CompiledField(type.name(), new CompiledGenericTypeParameter(dataType.name())));
        }
        if (type.type() instanceof DataType dataType && declarationsByName.containsKey(dataType.name())) {
            return linkDataDeclaration(
                    declarationsByName.get(dataType.name()),
                    declarationsByName,
                    rawTypeDeclarationsByName,
                    additionalKnownTypes,
                    importDeclarations,
                    cache,
                    visiting,
                    normalizedFile)
                    .map(linkedDataType -> new CompiledDataType.CompiledField(type.name(), linkedDataType));
        }
        var knownDataTypes = new HashMap<String, GenericDataType>();
        declarationsByName.forEach((name, declaration) -> {
            var cached = cache.get(name);
            if (cached instanceof Result.Success<CompiledDataType> value) {
                knownDataTypes.put(name, value.value());
            } else {
                knownDataTypes.put(name, new CompiledDataType(
                        declaration.name(),
                        List.of(),
                        declaration.typeParameters(),
                        declaration.extendsTypes(),
                        declaration.comments(),
                        declaration.visibility(),
                        false
                ));
            }
        });
        rawTypeDeclarationsByName.forEach((name, declaration) -> knownDataTypes.putIfAbsent(
                name,
                new CompiledDataParentType(name, List.of(), List.of(), declaration.typeParameters(), declaration.comments(), declaration.visibility(), false)
        ));
        additionalKnownTypes.forEach(knownDataTypes::putIfAbsent);
        genericTypes.forEach(genericTypeName -> knownDataTypes.putIfAbsent(
                genericTypeName,
                new CompiledDataParentType(genericTypeName, List.of(), List.of(), List.of())
        ));
        importedExternalTypePlaceholders(importDeclarations).forEach(knownDataTypes::putIfAbsent);
        var linkedType = linkType(type.type(), knownDataTypes);
        if (linkedType instanceof Result.Error<CompiledType>
            && type.type() instanceof DataType dataType) {
            var importedQualifiedName = resolveImportedQualifiedTypeName(dataType.name(), importDeclarations);
            if (importedQualifiedName.isPresent() && isQualifiedExternalTypeName(importedQualifiedName.get())) {
                return Result.success(new CompiledDataType.CompiledField(
                        type.name(),
                        externalTypePlaceholder(importedQualifiedName.get())
                ));
            }
        }
        if (linkedType instanceof Result.Error<CompiledType>
            && type.type() instanceof DataType dataType
            && isQualifiedExternalTypeName(dataType.name())) {
            return Result.success(new CompiledDataType.CompiledField(
                    type.name(),
                    externalTypePlaceholder(dataType.name())
            ));
        }
        return linkedType.map(t -> new CompiledDataType.CompiledField(type.name(), t));
    }

    private Map<String, GenericDataType> importedExternalTypePlaceholders(List<ImportDeclaration> importDeclarations) {
        var placeholders = new LinkedHashMap<String, GenericDataType>();
        for (var importDeclaration : importDeclarations) {
            var importedTypeName = importedTypeName(importDeclaration.moduleName());
            if (!importDeclaration.isStarImport()
                && !importDeclaration.symbols().contains(importedTypeName)) {
                continue;
            }
            var moduleName = importDeclaration.moduleName();
            var modulePlaceholder = externalTypePlaceholder(moduleName);
            placeholders.put(importedTypeName, aliasedTypePlaceholder(importedTypeName, modulePlaceholder));
            placeholders.put(moduleName, modulePlaceholder);
        }
        return Map.copyOf(placeholders);
    }

    private GenericDataType aliasedTypePlaceholder(String alias, GenericDataType placeholder) {
        return switch (placeholder) {
            case CompiledDataParentType parentType -> new CompiledDataParentType(
                    alias,
                    parentType.fields(),
                    parentType.subTypes(),
                    parentType.typeParameters(),
                    parentType.comments(),
                    parentType.visibility(),
                    parentType.enumType()
            );
            case CompiledDataType dataType -> new CompiledDataType(
                    alias,
                    dataType.fields(),
                    dataType.typeParameters(),
                    dataType.extendedTypes(),
                    dataType.comments(),
                    dataType.visibility(),
                    dataType.singleton(),
                    dataType.enumValue()
            );
            default -> placeholder;
        };
    }
    private Optional<String> resolveImportedQualifiedTypeName(String rawTypeName, List<ImportDeclaration> importDeclarations) {
        var parsed = parseGenericTypeName(rawTypeName);
        var baseName = parsed.baseName();
        for (var importDeclaration : importDeclarations) {
            var importedTypeName = importedTypeName(importDeclaration.moduleName());
            var imported = importDeclaration.isStarImport() || importDeclaration.symbols().contains(baseName);
            if (!imported || !importedTypeName.equals(baseName)) {
                continue;
            }
            if (parsed.typeArguments().isEmpty()) {
                return Optional.of(importDeclaration.moduleName());
            }
            return Optional.of(importDeclaration.moduleName() + "[" + String.join(", ", parsed.typeArguments()) + "]");
        }
        return Optional.empty();
    }

    private boolean isQualifiedExternalTypeName(String typeName) {
        return typeName.startsWith("/");
    }

    private boolean isOptionExternalTypeName(String typeName) {
        var baseName = baseTypeName(typeName);
        return "/capy/lang/Option.Option".equals(baseName) || "/capy/lang/Option".equals(baseName);
    }

    private boolean isResultExternalTypeName(String typeName) {
        var baseName = baseTypeName(typeName);
        return "/capy/lang/Result.Result".equals(baseName) || "/capy/lang/Result".equals(baseName);
    }

    private String baseTypeName(String typeName) {
        var idx = typeName.indexOf('[');
        if (idx > 0 && typeName.endsWith("]")) {
            return typeName.substring(0, idx);
        }
        return typeName;
    }

    private String importedTypeName(String moduleName) {
        var normalized = moduleName.replace('\\', '/');
        var slash = normalized.lastIndexOf('/');
        return slash >= 0 ? normalized.substring(slash + 1) : normalized;
    }

    private CompiledDataParentType externalTypePlaceholder(String typeName) {
        if (isOptionExternalTypeName(typeName)) {
            return optionExternalPlaceholder(typeName);
        }
        if (isResultExternalTypeName(typeName) || "Result".equals(baseTypeName(typeName))) {
            return resultExternalPlaceholder(typeName);
        }
        return new CompiledDataParentType(
                baseTypeName(typeName),
                List.of(),
                List.of(),
                externalTypeParameters(typeName)
        );
    }

    private List<String> externalTypeParameters(String typeName) {
        var parsed = parseGenericTypeName(typeName);
        return parsed.typeArguments().isEmpty() ? List.of() : List.copyOf(parsed.typeArguments());
    }

    private CompiledDataParentType optionExternalPlaceholder(String typeName) {
        var optionTypeParameter = optionExternalTypeParameter(typeName);
        var optionParentDescriptor = baseTypeName(typeName) + "[T]";
        var some = new CompiledDataType(
                "Some",
                List.of(new CompiledDataType.CompiledField("value", new CompiledGenericTypeParameter("T"))),
                List.of("T"),
                List.of(optionParentDescriptor),
                false
        );
        var none = new CompiledDataType(
                "None",
                List.of(),
                List.of(),
                List.of(optionParentDescriptor),
                true
        );
        return new CompiledDataParentType(
                baseTypeName(typeName),
                List.of(),
                List.of(some, none),
                List.of(optionTypeParameter)
        );
    }

    private CompiledDataParentType resultExternalPlaceholder(String typeName) {
        var resultTypeParameter = resultExternalTypeParameter(typeName);
        var resultParentDescriptor = baseTypeName(typeName) + "[T]";
        var success = new CompiledDataType(
                "Success",
                List.of(new CompiledDataType.CompiledField("value", new CompiledGenericTypeParameter("T"))),
                List.of("T"),
                List.of(resultParentDescriptor),
                false
        );
        var error = new CompiledDataType(
                "Error",
                List.of(new CompiledDataType.CompiledField("message", PrimitiveLinkedType.STRING)),
                List.of(),
                List.of(resultParentDescriptor),
                false
        );
        return new CompiledDataParentType(
                baseTypeName(typeName),
                List.of(),
                List.of(success, error),
                List.of(resultTypeParameter)
        );
    }

    private String resultExternalTypeParameter(String typeName) {
        var start = typeName.indexOf('[');
        if (start > 0 && typeName.endsWith("]")) {
            var value = typeName.substring(start + 1, typeName.length() - 1).trim();
            if (!value.isEmpty()) {
                return value;
            }
        }
        return "T";
    }

    private String optionExternalTypeParameter(String typeName) {
        var start = typeName.indexOf('[');
        if (start > 0 && typeName.endsWith("]")) {
            var value = typeName.substring(start + 1, typeName.length() - 1).trim();
            if (!value.isEmpty()) {
                return value.toUpperCase(java.util.Locale.ROOT);
            }
        }
        return "ANY";
    }

    private Result<CompiledDataParentType> linkTypeDeclaration(
            TypeDeclaration typeDeclaration,
            List<CompiledDataType> dataDeclarations,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, GenericDataType> knownDataTypes,
            String normalizedFile
    ) {
        var linked = findSubtypes(typeDeclaration.subTypes(), dataDeclarations, rawTypeDeclarationsByName, new HashSet<>())
                .flatMap(subTypes -> linkedDataParentType(typeDeclaration, subTypes, knownDataTypes));
        return withPosition(linked, typeDeclaration.position(), normalizedFile);
    }

    private Result<CompiledDataParentType> linkedDataParentType(
            TypeDeclaration typeDeclaration,
            List<CompiledDataType> subTypes,
            Map<String, GenericDataType> knownDataTypes
    ) {
        var genericTypes = Set.copyOf(typeDeclaration.typeParameters());
        return typeDeclaration.fields()
                .stream()
                .map(field -> linkField(field, genericTypes, knownDataTypes))
                .collect(new ResultCollectionCollector<>())
                .flatMap(fields -> subTypes.stream()
                        .map(subType -> mergeParentFields(typeDeclaration.name(), fields, subType))
                        .collect(new ResultCollectionCollector<>())
                        .map(inheritedSubtypes -> new CompiledDataParentType(
                                typeDeclaration.name(),
                                fields,
                                inheritedSubtypes,
                                typeDeclaration.typeParameters(),
                                typeDeclaration.comments(),
                                typeDeclaration.visibility(),
                                false
                        )));
    }

    private Result<CompiledDataType> mergeParentFields(
            String parentTypeName,
            List<CompiledDataType.CompiledField> parentFields,
            CompiledDataType childType
    ) {
        var merged = new ArrayList<CompiledDataType.CompiledField>(parentFields);
        var childFieldsByName = childType.fields().stream()
                .collect(java.util.stream.Collectors.toMap(
                        CompiledDataType.CompiledField::name,
                        java.util.function.Function.identity(),
                        (first, second) -> second,
                        java.util.LinkedHashMap::new
                ));
        for (var parentField : parentFields) {
            var childField = childFieldsByName.get(parentField.name());
            if (childField == null) {
                continue;
            }
            if (!parentField.type().equals(childField.type())) {
                return Result.error("Field `%s` in subtype `%s` must match parent type `%s` field type `%s`, but was `%s`"
                        .formatted(
                                parentField.name(),
                                childType.name(),
                                parentTypeName,
                                parentField.type(),
                                childField.type()
                        ));
            }
            merged.removeIf(field -> field.name().equals(parentField.name()));
        }
        merged.addAll(childType.fields());
        return Result.success(new CompiledDataType(
                childType.name(),
                List.copyOf(merged),
                childType.typeParameters(),
                childType.extendedTypes(),
                childType.comments(),
                childType.visibility(),
                childType.singleton(),
                childType.enumValue()
        ));
    }

    private Result<CompiledDataType.CompiledField> linkField(
            DataDeclaration.DataField type,
            Set<String> genericTypes,
            Map<String, GenericDataType> knownDataTypes
    ) {
        if (type.type() instanceof DataType dataType && genericTypes.contains(dataType.name())) {
            return Result.success(new CompiledDataType.CompiledField(type.name(), new CompiledGenericTypeParameter(dataType.name())));
        }
        return linkType(type.type(), knownDataTypes)
                .map(t -> new CompiledDataType.CompiledField(type.name(), t));
    }

    private Result<List<CompiledDataType>> findSubtypes(
            List<String> rawSubTypes,
            List<CompiledDataType> dataDeclarations,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Set<String> visitingTypes
    ) {
        var dataTypesMap = dataDeclarations.stream().collect(toMap(CompiledDataType::name, identity(), (first, second) -> first));
        return rawSubTypes.stream()
                .map(key -> {
                    var dataType = dataTypesMap.get(key);
                    if (dataType != null) {
                        return Result.success(List.of(dataType));
                    }
                    var typeDeclaration = rawTypeDeclarationsByName.get(key);
                    if (typeDeclaration == null) {
                        return Result.<List<CompiledDataType>>error("Type " + key + " not found");
                    }
                    if (!visitingTypes.add(key)) {
                        return Result.<List<CompiledDataType>>error("Circular type hierarchy detected for `" + key + "`");
                    }
                    var nested = findSubtypes(typeDeclaration.subTypes(), dataDeclarations, rawTypeDeclarationsByName, visitingTypes);
                    visitingTypes.remove(key);
                    return nested;
                })
                .collect(new ResultCollectionCollector<List<CompiledDataType>>())
                .map(list -> list.stream().flatMap(Collection::stream).toList());
    }

    private static <T> Result<T> withPosition(Result<T> valueOrError, Optional<SourcePosition> position, String file) {
        if (valueOrError instanceof Result.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new Result.Error<>(error.errors()
                    .stream()
                    .map(singleError -> {
                        var hasKnownPosition = singleError.line() > 0;
                        var line = hasKnownPosition ? singleError.line() : pos.line();
                        var column = hasKnownPosition ? singleError.column() : pos.column();
                        var sourceFile = singleError.file().isBlank() ? file : singleError.file();
                        return new Result.Error.SingleError(line, column, sourceFile, singleError.message());
                    })
                    .toList());
        }
        return valueOrError;
    }

    private static <T> Result<T> withFile(Result<T> valueOrError, String moduleSourceFile) {
        if (!(valueOrError instanceof Result.Error<T> error)) {
            return valueOrError;
        }
        var normalizedFile = normalizeFile(moduleSourceFile);
        return new Result.Error<>(error.errors().stream()
                .map(singleError -> new Result.Error.SingleError(
                        singleError.line(),
                        singleError.column(),
                        singleError.file().isBlank() ? normalizedFile : singleError.file(),
                        singleError.message()))
                .toList());
    }

    private static String normalizeFile(String moduleSourceFile) {
        var normalized = moduleSourceFile.replace('\\', '/');
        return normalized.startsWith("/") ? normalized : "/" + normalized;
    }

    private static String qualifiedModuleNameFromSourceFile(String moduleSourceFile) {
        var normalized = normalizeFile(moduleSourceFile);
        if (normalized.endsWith(".cfun")) {
            normalized = normalized.substring(0, normalized.length() - ".cfun".length());
        }
        return normalized;
    }

    private static <T> List<T> castList(Module module, Class<T> clazz) {
        return module.functional().definitions()
                .stream()
                .filter(clazz::isInstance)
                .map(clazz::cast)
                .toList();
    }
}
