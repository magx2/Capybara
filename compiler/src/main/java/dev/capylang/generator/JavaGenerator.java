package dev.capylang.generator;

import dev.capylang.generator.java.*;
import dev.capylang.compiler.CompiledNativeProviderBinding;
import dev.capylang.compiler.CompiledNativeProviderDeclaration;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledObjectKind;
import dev.capylang.compiler.CompiledObjectType;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderLifetime;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledVariable;
import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Supplier;
import java.util.logging.Logger;

import static java.lang.String.join;
import static java.util.stream.Collectors.joining;
import static dev.capylang.generator.java.JavaExpressionEvaluator.evaluateExpression;
import static dev.capylang.generator.java.JavaExpressionEvaluator.evaluateTailRecursiveExpression;

public final class JavaGenerator implements Generator {
    private static final Logger log = Logger.getLogger(JavaGenerator.class.getName());
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX = "__constructor__primitive__";
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );

    @Override
    public GeneratedProgram generate(CompiledProgram program) {
        var timings = new GenerationTimings();
        var functionNameOverrides = buildFunctionNameOverrides(program);
        var staticExtensionMethodOwners = buildStaticExtensionMethodOwners(program);
        var enumValueOwnerOverrides = buildEnumValueOwnerOverrides(program);
        var primitiveBackedTypeInfo = primitiveBackedTypeInfo(program);
        var singletonTypeInfo = singletonTypeInfo(program);
        var nativeProviderInfos = nativeProviderInfos(program);
        var astBuilder = new JavaAstBuilder(functionNameOverrides, enumValueOwnerOverrides, program);
        JavaExpressionEvaluator.setFunctionNameOverrides(functionNameOverrides);
        JavaExpressionEvaluator.setStaticExtensionMethodOwners(staticExtensionMethodOwners);
        JavaExpressionEvaluator.setPrimitiveBackedTypes(primitiveBackedEvaluatorTypes(primitiveBackedTypeInfo));
        var modules = new ArrayList<GeneratedModule>();
        for (var module : program.modules()) {
            modules.addAll(modules(module, timings, astBuilder, enumValueOwnerOverrides));
        }
        var objectOrientedJavaGenerator = new ObjectOrientedJavaGenerator(
                primitiveBackedTypeInfo,
                singletonTypeInfo,
                nativeProviderInfos.stream()
                        .map(info -> new ObjectOrientedJavaGenerator.NativeProviderInfo(
                                info.providerSymbolName(),
                                info.bootstrapMethodName(),
                                info.targetBackendType(),
                                info.interfaceId(),
                                info.qualifier(),
                                info.sourceModulePath(),
                                info.sourceModuleName(),
                                info.sourceFile()
                        ))
                        .toList()
        );
        modules.addAll(time(timings::addSourceRenderNanos, () -> objectOrientedJavaGenerator.generate(program.objectOrientedModules())));
        modules.addAll(nativeProviderBootstrapModules(nativeProviderInfos));
        log.info(() -> "Java generation timings: AST build="
                       + Duration.ofNanos(timings.astBuildNanos())
                       + ", Java source rendering="
                       + Duration.ofNanos(timings.sourceRenderNanos()));
        return new GeneratedProgram(modules);
    }

    private java.util.Map<String, PrimitiveLinkedType> primitiveBackedEvaluatorTypes(
            java.util.Map<String, ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo> primitiveBackedTypes
    ) {
        var result = new java.util.LinkedHashMap<String, PrimitiveLinkedType>();
        primitiveBackedTypes.forEach((alias, info) -> result.put(alias, info.backingType()));
        return java.util.Map.copyOf(result);
    }

    private List<NativeProviderInfo> nativeProviderInfos(CompiledProgram program) {
        if (program.nativeProviderCatalog().declarations().isEmpty()) {
            return List.of();
        }
        var bindings = nativeProviderBindings(program.nativeProviderCatalog().bindings());
        var interfaces = nativeProviderInterfaceTypes(program);
        var baseNames = new java.util.LinkedHashMap<String, Integer>();
        for (var declaration : program.nativeProviderCatalog().declarations()) {
            baseNames.merge(sanitizeJavaIdentifier(declaration.providerName()), 1, Integer::sum);
        }

        var usedNames = new java.util.LinkedHashSet<String>();
        var infos = new ArrayList<NativeProviderInfo>();
        for (var declaration : program.nativeProviderCatalog().declarations()) {
            var binding = bindings.get(nativeProviderKey(declaration.interfaceId(), declaration.qualifier()));
            var javaBinding = binding == null ? null : binding.javaBinding();
            if (javaBinding == null) {
                throw new IllegalArgumentException("UnsupportedBackend: Native provider `" + declaration.providerName()
                                                   + "` for interface `" + declaration.interfaceId()
                                                   + "` with qualifier `" + declaration.qualifier()
                                                   + "` for backend `java` has no java binding in source `"
                                                   + declaration.sourceFile() + "`");
            }
            var interfaceType = interfaces.get(declaration.interfaceId());
            if (interfaceType == null) {
                throw new IllegalArgumentException("TypeMismatch: Native provider `" + declaration.providerName()
                                                   + "` targets unknown interface `" + declaration.interfaceId()
                                                   + "` with qualifier `" + declaration.qualifier()
                                                   + "` in source `" + declaration.sourceFile() + "`");
            }
            var baseName = sanitizeJavaIdentifier(declaration.providerName());
            var bootstrapName = baseNames.getOrDefault(baseName, 0) == 1
                    ? baseName
                    : uniqueNativeProviderBootstrapName(baseName, declaration, usedNames);
            usedNames.add(bootstrapName);
            infos.add(new NativeProviderInfo(
                    declaration.providerName(),
                    bootstrapName,
                    declaration.interfaceId(),
                    declaration.qualifier(),
                    interfaceType.backendClassName(),
                    declaration.sourceModulePath(),
                    declaration.sourceModuleName(),
                    declaration.sourceFile(),
                    binding.lifetime(),
                    javaBinding
            ));
        }
        return List.copyOf(infos);
    }

    private Map<String, CompiledNativeProviderBinding> nativeProviderBindings(List<CompiledNativeProviderBinding> bindings) {
        var result = new java.util.LinkedHashMap<String, CompiledNativeProviderBinding>();
        for (var binding : bindings) {
            result.putIfAbsent(nativeProviderKey(binding.interfaceId(), binding.qualifier()), binding);
        }
        return Map.copyOf(result);
    }

    private Map<String, ProviderInterfaceInfo> nativeProviderInterfaceTypes(CompiledProgram program) {
        var result = new java.util.LinkedHashMap<String, ProviderInterfaceInfo>();
        for (var module : program.modules()) {
            for (var type : module.types().values()) {
                if (type instanceof CompiledObjectType objectType && objectType.kind() == CompiledObjectKind.INTERFACE) {
                    var info = new ProviderInterfaceInfo(objectType.backendClassName());
                    result.put(cfunTypeName(module, objectType.name()), info);
                    if (module.name().equals(objectType.name())) {
                        result.put(cfunModuleName(module), info);
                    }
                }
            }
        }
        for (var module : program.objectOrientedModules()) {
            for (var definition : module.objectOriented().definitions()) {
                if (definition instanceof ObjectOriented.InterfaceDeclaration) {
                    result.put(objectInterfaceId(module, definition.name()), new ProviderInterfaceInfo(objectBackendClassName(module, definition.name())));
                }
            }
        }
        return Map.copyOf(result);
    }

    private String objectInterfaceId(ObjectOrientedModule module, String typeName) {
        var moduleName = objectModuleName(module);
        var qualified = module.name().equals(typeName)
                ? moduleName
                : moduleName + "." + typeName;
        return qualified.startsWith("/") ? qualified : "/" + qualified;
    }

    private String objectModuleName(ObjectOrientedModule module) {
        var path = module.path().replace('\\', '/').replaceFirst("^/+", "").replaceFirst("/+$", "");
        return path.isBlank() ? module.name() : path + "/" + module.name();
    }

    private String objectBackendClassName(ObjectOrientedModule module, String typeName) {
        var packageName = module.path().replace('\\', '/')
                .replaceFirst("^/+", "")
                .replaceFirst("/+$", "")
                .replace('/', '.');
        return packageName.isBlank() || ".".equals(packageName)
                ? typeName
                : packageName + "." + typeName;
    }

    private String uniqueNativeProviderBootstrapName(
            String baseName,
            CompiledNativeProviderDeclaration declaration,
            Set<String> usedNames
    ) {
        var suffix = sanitizeJavaIdentifier((declaration.sourceModulePath() + "_" + declaration.sourceModuleName())
                .replaceAll("[^A-Za-z0-9_]+", "_"));
        var candidate = baseName + "__" + suffix;
        var index = 2;
        while (usedNames.contains(candidate)) {
            candidate = baseName + "__" + suffix + "_" + index;
            index++;
        }
        return candidate;
    }

    private String nativeProviderKey(String interfaceId, String qualifier) {
        return interfaceId + "\u0000" + qualifier;
    }

    private List<GeneratedModule> nativeProviderBootstrapModules(List<NativeProviderInfo> providers) {
        if (providers.isEmpty()) {
            return List.of();
        }
        return List.of(new GeneratedModule(Path.of("dev", "capylang", "NativeProviderBootstrap.java"), renderNativeProviderBootstrap(providers)));
    }

    private String renderNativeProviderBootstrap(List<NativeProviderInfo> providers) {
        var code = new StringBuilder();
        code.append("package dev.capylang;\n\n");
        code.append("@javax.annotation.processing.Generated(\"Capybara Compiler\")\n");
        code.append("public final class NativeProviderBootstrap {\n");
        code.append("    private static final NativeProviders PROVIDERS = NativeProviders.of(\n");
        for (int i = 0; i < providers.size(); i++) {
            code.append(renderNativeProviderRegistration(providers.get(i)));
            if (i < providers.size() - 1) {
                code.append(",");
            }
            code.append("\n");
        }
        code.append("    );\n\n");
        code.append("    private NativeProviderBootstrap() {\n");
        code.append("    }\n\n");
        for (var provider : providers) {
            code.append(renderNativeProviderMethod(provider)).append("\n");
        }
        code.append("}\n");
        return code.toString();
    }

    private String renderNativeProviderRegistration(NativeProviderInfo provider) {
        var binding = provider.binding();
        if (binding.className() == null || binding.className().isBlank()) {
            throw new IllegalArgumentException("UnsupportedBackend: Native provider `" + provider.providerSymbolName()
                                               + "` for interface `" + provider.interfaceId()
                                               + "` with qualifier `" + provider.qualifier()
                                               + "` for backend `java` requires manifest field `java.className` in source `"
                                               + provider.sourceFile() + "`");
        }
        if (!"constructor".equals(binding.factory())) {
            throw new IllegalArgumentException("UnsupportedBackend: Native provider `" + provider.providerSymbolName()
                                               + "` for interface `" + provider.interfaceId()
                                               + "` with qualifier `" + provider.qualifier()
                                               + "` for backend `java`"
                                               + " has unsupported java factory `" + binding.factory()
                                               + "`. Supported values: constructor. Source `" + provider.sourceFile() + "`");
        }
        var lifetimeMethod = provider.lifetime() == NativeProviderLifetime.FACTORY ? "factory" : "singleton";
        return "            NativeProviders." + lifetimeMethod + "(\n"
               + "                    " + javaString(provider.interfaceId()) + ",\n"
               + "                    " + javaString(provider.qualifier()) + ",\n"
               + "                    " + javaString(provider.providerSymbolName()) + ",\n"
               + "                    \"java\",\n"
               + "                    " + javaString(provider.sourceFile()) + ",\n"
               + "                    " + provider.targetBackendType() + ".class,\n"
               + "                    " + binding.className() + "::new\n"
               + "            )";
    }

    private String renderNativeProviderMethod(NativeProviderInfo provider) {
        return "    public static " + provider.targetBackendType() + " " + provider.bootstrapMethodName() + "() {\n"
               + "        return PROVIDERS.resolve(" + javaString(provider.interfaceId())
               + ", " + javaString(provider.qualifier())
               + ", " + javaString(provider.providerSymbolName())
               + ", \"java\""
               + ", " + javaString(provider.sourceFile())
               + ", " + provider.targetBackendType() + ".class);\n"
               + "    }\n";
    }

    private static String javaString(String value) {
        return "\"" + value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t")
                + "\"";
    }

    private List<GeneratedModule> modules(
            CompiledModule module,
            GenerationTimings timings,
            JavaAstBuilder astBuilder,
            java.util.Map<String, String> globalEnumValueOwnerOverrides
    ) {
        var javaClass = time(timings::addAstBuildNanos, () -> astBuilder.build(module));
        var enumValueOwnerOverrides = new java.util.LinkedHashMap<>(globalEnumValueOwnerOverrides);
        enumValueOwnerOverrides.putAll(importedEnumValueOwnerOverrides(module));
        enumValueOwnerOverrides.putAll(javaClass.enumValueOwnerOverrides());
        JavaExpressionEvaluator.setEnumValueOwnerOverrides(enumValueOwnerOverrides);
        try {
            if (!hasTypeOrDataNameConflictWithFile(javaClass)) {
                return List.of(new GeneratedModule(
                        relativePath(javaClass, javaClass.name().toString()),
                        time(timings::addSourceRenderNanos, () -> code(javaClass, javaClass.name().toString(), true))
                ));
            }

            var ownerInterface = javaClass.interfaces().stream()
                    .filter(javaInterface -> javaInterface.name().toString().equals(javaClass.name().toString()))
                    .findFirst();
            if (ownerInterface.isPresent()) {
                return List.of(new GeneratedModule(
                        relativePath(javaClass, javaClass.name().toString()),
                        time(timings::addSourceRenderNanos, () -> codeNestedInOwnerInterface(javaClass, ownerInterface.get(), ownerInterface.get().name().toString()))
                ));
            }

            var helperCallOwnerName = javaClass.name() + "Module";
            var compiled = new ArrayList<GeneratedModule>();
            for (var declaration : topLevelDeclarations(javaClass, helperCallOwnerName)) {
                compiled.add(new GeneratedModule(
                        relativePath(javaClass, declaration.name()),
                        time(timings::addSourceRenderNanos, () -> codeTopLevelDeclaration(javaClass, declaration.code()))
                ));
            }
            var utilityStaticImports = new TreeSet<>(javaClass.staticImports());
            javaClass.enums().forEach(javaEnum -> utilityStaticImports.add(javaClass.javaPackage() + "." + javaEnum.name() + ".*"));
            if (!javaClass.staticMethods().isEmpty() || !javaClass.staticConsts().isEmpty()) {
                var utilityClass = new JavaClass(
                        javaClass.annotations(),
                        new JavaType(javaClass.name() + "Module"),
                        javaClass.javaPackage(),
                        utilityStaticImports,
                        javaClass.staticConsts(),
                        javaClass.staticMethods(),
                        new TreeSet<>(),
                        new TreeSet<>(),
                        new TreeSet<>(),
                        javaClass.enumValueOwnerOverrides()
                );
                compiled.add(new GeneratedModule(
                        relativePath(javaClass, utilityClass.name().toString()),
                        time(timings::addSourceRenderNanos, () -> code(utilityClass, utilityClass.name().toString(), false))
                ));
            }
            return List.copyOf(compiled);
        } finally {
            JavaExpressionEvaluator.setEnumValueOwnerOverrides(java.util.Map.of());
        }
    }

    private java.util.Map<String, String> importedEnumValueOwnerOverrides(CompiledModule module) {
        var overrides = new java.util.LinkedHashMap<String, String>();
        for (var staticImport : module.staticImports()) {
            if (!staticImport.enumValue() || "*".equals(staticImport.memberName())) {
                continue;
            }
            overrides.put(staticImport.memberName(), enumValueOverride(staticImport.className(), staticImport.memberName()));
            var alias = staticImport.memberName().replace("_", "");
            if (!alias.equals(staticImport.memberName())) {
                overrides.put(alias, enumValueOverride(staticImport.className(), staticImport.memberName()));
            }
        }
        return java.util.Map.copyOf(overrides);
    }

    private java.util.Map<String, ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo> primitiveBackedTypeInfo(CompiledProgram program) {
        var result = new java.util.LinkedHashMap<String, ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo>();
        program.modules().forEach(module -> module.types().values().stream()
                .filter(dev.capylang.compiler.CompiledPrimitiveBackedType.class::isInstance)
                .map(dev.capylang.compiler.CompiledPrimitiveBackedType.class::cast)
                .forEach(type -> {
                    var info = new ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo(
                            type.name(),
                            type.cfunType(),
                            type.backingType(),
                            !constructorTypes(module).contains(type.name())
                    );
                    putPrimitiveBackedTypeAliases(result, info, module);
                }));
        program.modules().forEach(module -> module.visiblePrimitiveBackedTypes().forEach((alias, type) -> {
            var info = result.values().stream()
                    .filter(existing -> existing.cfunType().equals(type.cfunType()))
                    .findFirst()
                    .orElseGet(() -> primitiveBackedTypeInfo(type));
            if (alias.contains("/") || alias.contains(".")) {
                result.putIfAbsent(alias, info);
            }
            putPrimitiveBackedTypeAliases(result, info);
        }));
        program.modules().forEach(module -> module.functions().forEach(function -> {
            function.parameters().stream()
                    .flatMap(parameter -> primitiveBackedTypes(parameter.type()))
                    .forEach(type -> putPrimitiveBackedTypeAliases(result, primitiveBackedTypeInfo(type)));
            primitiveBackedTypes(function.returnType())
                    .forEach(type -> putPrimitiveBackedTypeAliases(result, primitiveBackedTypeInfo(type)));
        }));
        putUniqueSimplePrimitiveBackedTypeAliases(result);
        return java.util.Map.copyOf(result);
    }

    private java.util.Map<String, ObjectOrientedJavaGenerator.SingletonTypeInfo> singletonTypeInfo(CompiledProgram program) {
        var result = new java.util.LinkedHashMap<String, ObjectOrientedJavaGenerator.SingletonTypeInfo>();
        program.modules().forEach(module -> module.types().values().stream()
                .filter(dev.capylang.compiler.CompiledDataType.class::isInstance)
                .map(dev.capylang.compiler.CompiledDataType.class::cast)
                .filter(dev.capylang.compiler.CompiledDataType::singleton)
                .filter(type -> !type.enumValue())
                .forEach(type -> putSingletonTypeAliases(
                        result,
                        new ObjectOrientedJavaGenerator.SingletonTypeInfo(
                                type.name(),
                                cfunTypeName(module, type.name())
                        ),
                        module
                )));
        putRuntimeSingletonTypeAliases(result);
        putUniqueSimpleSingletonTypeAliases(result);
        return java.util.Map.copyOf(result);
    }

    private static void putRuntimeSingletonTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.SingletonTypeInfo> result
    ) {
        putSingletonTypeAliases(result, new ObjectOrientedJavaGenerator.SingletonTypeInfo("None", "/capy/lang/Option.None"));
        putSingletonTypeAliases(result, new ObjectOrientedJavaGenerator.SingletonTypeInfo("Success", "/capy/lang/Program.Success"));
    }

    private static void putSingletonTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.SingletonTypeInfo> result,
            ObjectOrientedJavaGenerator.SingletonTypeInfo info,
            CompiledModule module
    ) {
        putSingletonTypeAliases(result, info);
        result.putIfAbsent(module.name() + "." + info.name(), info);
    }

    private static void putSingletonTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.SingletonTypeInfo> result,
            ObjectOrientedJavaGenerator.SingletonTypeInfo info
    ) {
        if (info.name().contains("/") || info.name().contains(".")) {
            result.putIfAbsent(info.name(), info);
        }
        result.putIfAbsent(info.cfunType(), info);
        result.putIfAbsent(withoutLeadingSlash(info.cfunType()), info);
    }

    private static void putUniqueSimpleSingletonTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.SingletonTypeInfo> result
    ) {
        var bySimpleName = result.values().stream()
                .distinct()
                .collect(java.util.stream.Collectors.groupingBy(
                        info -> simpleTypeName(info.name()),
                        java.util.LinkedHashMap::new,
                        java.util.stream.Collectors.toList()
                ));
        bySimpleName.forEach((simpleName, infos) -> {
            if (infos.size() == 1) {
                result.putIfAbsent(simpleName, infos.getFirst());
            }
        });
    }

    private static java.util.stream.Stream<dev.capylang.compiler.CompiledPrimitiveBackedType> primitiveBackedTypes(
            dev.capylang.compiler.CompiledType type
    ) {
        return switch (type) {
            case dev.capylang.compiler.CompiledPrimitiveBackedType primitiveBackedType ->
                    java.util.stream.Stream.of(primitiveBackedType);
            case dev.capylang.compiler.CollectionLinkedType.CompiledList listType ->
                    primitiveBackedTypes(listType.elementType());
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet setType ->
                    primitiveBackedTypes(setType.elementType());
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict dictType ->
                    primitiveBackedTypes(dictType.valueType());
            case dev.capylang.compiler.CompiledTupleType tupleType ->
                    tupleType.elementTypes().stream().flatMap(JavaGenerator::primitiveBackedTypes);
            case dev.capylang.compiler.CompiledFunctionType functionType ->
                    java.util.stream.Stream.concat(
                            primitiveBackedTypes(functionType.argumentType()),
                            primitiveBackedTypes(functionType.returnType())
                    );
            case dev.capylang.compiler.CompiledDataType dataType ->
                    dataType.fields().stream().flatMap(field -> primitiveBackedTypes(field.type()));
            case dev.capylang.compiler.CompiledDataParentType parentType ->
                    parentType.subTypes().stream()
                            .flatMap(dataType -> dataType.fields().stream())
                            .flatMap(field -> primitiveBackedTypes(field.type()));
            default -> java.util.stream.Stream.empty();
        };
    }

    private static ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo primitiveBackedTypeInfo(
            dev.capylang.compiler.CompiledPrimitiveBackedType type
    ) {
        return new ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo(
                type.name(),
                type.cfunType(),
                type.backingType(),
                false
        );
    }

    private static Set<String> constructorTypes(CompiledModule module) {
        return module.functions().stream()
                .map(dev.capylang.compiler.CompiledFunction::name)
                .filter(name -> name.startsWith(PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX))
                .map(name -> name.substring(PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX.length()))
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
    }

    private static void putPrimitiveBackedTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo> result,
            ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo info,
            CompiledModule module
    ) {
        if (info.name().contains("/") || info.name().contains(".")) {
            result.putIfAbsent(info.name(), info);
        }
        result.putIfAbsent(info.cfunType(), info);
        result.putIfAbsent(withoutLeadingSlash(info.cfunType()), info);
        result.putIfAbsent(module.name() + "." + info.name(), info);
    }

    private static void putPrimitiveBackedTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo> result,
            ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo info
    ) {
        if (info.name().contains("/") || info.name().contains(".")) {
            result.putIfAbsent(info.name(), info);
        }
        result.putIfAbsent(info.cfunType(), info);
        result.putIfAbsent(withoutLeadingSlash(info.cfunType()), info);
    }

    private static void putUniqueSimplePrimitiveBackedTypeAliases(
            java.util.Map<String, ObjectOrientedJavaGenerator.PrimitiveBackedTypeInfo> result
    ) {
        var bySimpleName = result.values().stream()
                .distinct()
                .collect(java.util.stream.Collectors.groupingBy(
                        info -> simpleTypeName(info.name()),
                        java.util.LinkedHashMap::new,
                        java.util.stream.Collectors.toList()
                ));
        bySimpleName.forEach((simpleName, infos) -> {
            if (infos.size() == 1) {
                result.putIfAbsent(simpleName, infos.getFirst());
            }
        });
    }

    private static String withoutLeadingSlash(String value) {
        return value.startsWith("/") ? value.substring(1) : value;
    }

    private static String cfunTypeName(CompiledModule module, String typeName) {
        var path = module.path().replace('\\', '/').replaceFirst("^/+", "").replaceFirst("/+$", "");
        var owner = path.isBlank() ? "/" + module.name() : "/" + path + "/" + module.name();
        return owner + "." + typeName;
    }

    private static String cfunModuleName(CompiledModule module) {
        var path = module.path().replace('\\', '/').replaceFirst("^/+", "").replaceFirst("/+$", "");
        return path.isBlank() ? "/" + module.name() : "/" + path + "/" + module.name();
    }

    private static String simpleTypeName(String name) {
        var normalized = name.replace('\\', '/');
        var slash = normalized.lastIndexOf('/');
        if (slash >= 0) {
            return normalized.substring(slash + 1);
        }
        var dot = normalized.lastIndexOf('.');
        return dot >= 0 ? normalized.substring(dot + 1) : normalized;
    }

    private static String sanitizeJavaIdentifier(String value) {
        return JAVA_KEYWORDS.contains(value) ? value + "_" : value;
    }


    private java.util.Map<String, String> buildStaticExtensionMethodOwners(CompiledProgram program) {
        var nativeTypeNames = program.modules().stream()
                .flatMap(module -> module.types().values().stream())
                .flatMap(type -> switch (type) {
                    case dev.capylang.compiler.CompiledDataType dataType when dataType.nativeType() ->
                            java.util.stream.Stream.of(dataType.name());
                    case dev.capylang.compiler.CompiledPrimitiveBackedType primitiveBackedType ->
                            java.util.stream.Stream.of(primitiveBackedType.name());
                    default -> java.util.stream.Stream.empty();
                })
                .collect(java.util.stream.Collectors.toSet());
        var owners = new java.util.LinkedHashMap<String, String>();
        for (var module : program.modules()) {
            var ownerClassName = staticMethodOwnerClassName(module);
            for (var function : module.functions()) {
                if (!isStaticExtensionMethod(function, nativeTypeNames)) {
                    continue;
                }
                owners.put(function.name(), ownerClassName);
            }
        }
        return java.util.Map.copyOf(owners);
    }

    private java.util.Map<String, String> buildEnumValueOwnerOverrides(CompiledProgram program) {
        var ownersByValue = new java.util.LinkedHashMap<String, java.util.LinkedHashSet<String>>();
        for (var module : program.modules()) {
            for (var type : module.types().values()) {
                if (!(type instanceof dev.capylang.compiler.CompiledDataParentType parentType) || !parentType.enumType()) {
                    continue;
                }
                var ownerClassName = enumOwnerClassName(module, parentType.name());
                for (var subType : parentType.subTypes()) {
                    ownersByValue
                            .computeIfAbsent(subType.name(), ignored -> new java.util.LinkedHashSet<>())
                            .add(ownerClassName);
                }
            }
        }

        var overrides = new java.util.LinkedHashMap<String, String>();
        ownersByValue.forEach((valueName, ownerNames) -> {
            if (ownerNames.size() == 1) {
                var ownerName = ownerNames.iterator().next();
                overrides.put(valueName, enumValueOverride(ownerName, valueName));
                var alias = valueName.replace("_", "");
                if (!alias.equals(valueName)) {
                    overrides.put(alias, enumValueOverride(ownerName, valueName));
                }
            }
        });
        return java.util.Map.copyOf(overrides);
    }

    private String enumValueOverride(String ownerName, String valueName) {
        return ownerName + "#" + valueName;
    }

    private String enumOwnerClassName(CompiledModule module, String enumTypeName) {
        var modulePath = module.path().replace('\\', '/');
        var normalizedPath = modulePath.startsWith("/") ? modulePath : "/" + modulePath;
        var pathPrefix = normalizedPath.equals("/") ? "" : normalizedPath;
        return staticMethodsAreNestedInOwner(module)
                ? qualifiedTypeName(pathPrefix, module.name() + "." + enumTypeName)
                : qualifiedTypeName(pathPrefix, enumTypeName);
    }

    private String qualifiedTypeName(String pathPrefix, String typeName) {
        return pathPrefix.isBlank() ? typeName : pathPrefix + "/" + typeName;
    }

    private String staticMethodOwnerClassName(CompiledModule module) {
        var packageName = buildJavaPackageName(module.path());
        var className = normalizeJavaIdentifier(module.name(), true);
        var simpleName = staticMethodsAreNestedInOwner(module)
                ? className
                : className + "Module";
        return packageName.isBlank() ? simpleName : packageName + "." + simpleName;
    }

    private boolean staticMethodsAreNestedInOwner(CompiledModule module) {
        var fileClassName = normalizeJavaIdentifier(module.name(), true);
        var hasNameConflict = false;
        var hasOwnerInterface = false;
        for (var type : module.types().values()) {
            if (type instanceof dev.capylang.compiler.CompiledDataParentType parentType) {
                var typeClassName = normalizeJavaIdentifier(parentType.name(), true);
                if (typeClassName.equals(fileClassName)) {
                    hasNameConflict = true;
                    if (!parentType.enumType()) {
                        hasOwnerInterface = true;
                    }
                }
            } else if (type instanceof dev.capylang.compiler.CompiledDataType dataType
                       && !dataType.singleton()
                       && !dataType.nativeType()
                       && normalizeJavaIdentifier(dataType.name(), true).equals(fileClassName)) {
                hasNameConflict = true;
            }
        }
        return !hasNameConflict || hasOwnerInterface;
    }

    private String buildJavaPackageName(String rawPath) {
        var normalized = rawPath.replace('\\', '/');
        return java.util.stream.Stream.of(normalized.split("/"))
                .filter(part -> !part.isBlank())
                .map(part -> normalizeJavaIdentifier(part, false))
                .collect(joining("."));
    }

    private boolean isStaticExtensionMethod(
            dev.capylang.compiler.CompiledFunction function,
            Set<String> nativeTypeNames
    ) {
        return methodOwnerType(function.name())
                .filter(nativeTypeNames::contains)
                .filter(ignored -> !isNativeExpression(function))
                .isPresent();
    }

    private java.util.Optional<String> methodOwnerType(String functionName) {
        if (!functionName.startsWith(METHOD_DECL_PREFIX)) {
            return java.util.Optional.empty();
        }
        var separatorIndex = functionName.indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0) {
            return java.util.Optional.empty();
        }
        return java.util.Optional.of(functionName.substring(METHOD_DECL_PREFIX.length(), separatorIndex));
    }

    private boolean isNativeExpression(dev.capylang.compiler.CompiledFunction function) {
        return function.expression() instanceof dev.capylang.compiler.expression.CompiledNothingValue nothingValue
               && (nothingValue.message().contains("`<native>`")
                   || nothingValue.message().contains("native expression in function"));
    }

    private java.util.Map<String, String> buildFunctionNameOverrides(CompiledProgram program) {
        var overrides = new java.util.LinkedHashMap<String, String>();
        var collisions = new java.util.LinkedHashMap<String, java.util.List<dev.capylang.compiler.CompiledFunction>>();
        var ownerModuleNames = new java.util.IdentityHashMap<dev.capylang.compiler.CompiledFunction, String>();
        var primitiveBackedTypeNames = primitiveBackedTypeNames(program);
        var primitiveBackedMethods = new java.util.ArrayList<dev.capylang.compiler.CompiledFunction>();
        for (var module : program.modules()) {
            for (var function : module.functions()) {
                ownerModuleNames.put(function, module.name());
                var primitiveBackedMethod = methodOwnerType(function.name())
                        .filter(primitiveBackedTypeNames::contains)
                        .isPresent();
                if (primitiveBackedMethod) {
                    primitiveBackedMethods.add(function);
                }
                var ownerKey = function.name().startsWith(METHOD_DECL_PREFIX) && !primitiveBackedMethod
                        ? function.name().substring(0, Math.max(function.name().lastIndexOf("__"), METHOD_DECL_PREFIX.length()))
                        : module.name();
                var normalizedBaseName = normalizeJavaMethodIdentifier(baseMethodName(function.name()));
                var erasedSignature = function.parameters().stream()
                        .map(parameter -> erasedJavaType(parameter.type()))
                        .collect(joining(","));
                collisions.computeIfAbsent(ownerKey + "|" + normalizedBaseName + "|" + erasedSignature, ignored -> new java.util.ArrayList<>()).add(function);
            }
        }
        for (var entry : collisions.entrySet()) {
            var functions = entry.getValue();
            if (functions.size() < 2) {
                continue;
            }
            var canonicalNamedFunction = canonicalNamedFunction(functions);
            var mixedRawNames = functions.stream()
                    .map(function -> baseMethodName(function.name()))
                    .distinct()
                    .count() > 1;
            for (var function : functions) {
                var rawBaseName = baseMethodName(function.name());
                var normalizedBaseName = normalizeJavaMethodIdentifier(rawBaseName);
                var overloadSuffix = overloadSuffix(function);
                var legacyEmittedName = normalizedBaseName + overloadSuffix;
                var namedCanonical = canonicalNamedFunction.filter(named -> named == function).isPresent();
                var emittedName = mixedRawNames
                        ? (namedCanonical
                                ? normalizedBaseName
                                : normalizedBaseName + "__" + methodVariantSuffix(rawBaseName) + overloadSuffix)
                        : legacyEmittedName;
                var parameterTypes = function.parameters().stream().map(dev.capylang.compiler.CompiledFunction.CompiledFunctionParameter::type).toList();
                overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
                if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(moduleQualifiedName(ownerModuleNames, function), parameterTypes), emittedName);
                }
                if (mixedRawNames || !function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(baseMethodName(function.name()), parameterTypes), emittedName);
                }
                if (!emittedName.equals(legacyEmittedName)) {
                    overrides.put(signatureKey(legacyEmittedName, parameterTypes), emittedName);
                }
            }
        }
        for (var function : primitiveBackedMethods) {
            var emittedName = primitiveBackedMethodName(function);
            var parameterTypes = function.parameters().stream().map(dev.capylang.compiler.CompiledFunction.CompiledFunctionParameter::type).toList();
            overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
            overrides.put(signatureKey(moduleQualifiedName(ownerModuleNames, function), parameterTypes), emittedName);
        }
        return java.util.Map.copyOf(overrides);
    }

    private static String primitiveBackedMethodName(dev.capylang.compiler.CompiledFunction function) {
        var rawBaseName = baseMethodName(function.name());
        return normalizeJavaMethodIdentifier(rawBaseName) + "__" + methodVariantSuffix(rawBaseName) + overloadSuffix(function);
    }

    private static java.util.Set<String> primitiveBackedTypeNames(CompiledProgram program) {
        return program.modules().stream()
                .flatMap(module -> module.types().values().stream())
                .filter(dev.capylang.compiler.CompiledPrimitiveBackedType.class::isInstance)
                .map(dev.capylang.compiler.CompiledPrimitiveBackedType.class::cast)
                .map(dev.capylang.compiler.CompiledPrimitiveBackedType::name)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
    }

    private String moduleQualifiedName(
            java.util.IdentityHashMap<dev.capylang.compiler.CompiledFunction, String> ownerModuleNames,
            dev.capylang.compiler.CompiledFunction function
    ) {
        var ownerModuleName = ownerModuleNames.get(function);
        if (ownerModuleName == null) {
            throw new IllegalStateException("Missing owner module for function: " + function.name());
        }
        return ownerModuleName + "." + function.name();
    }

    private static String signatureKey(String name, java.util.List<dev.capylang.compiler.CompiledType> parameterTypes) {
        return name + "|" + parameterTypes.stream().map(type -> String.valueOf(type)).collect(joining(","));
    }

    private static String baseMethodName(String name) {
        if (!name.startsWith(METHOD_DECL_PREFIX)) {
            return name;
        }
        var idx = name.lastIndexOf("__");
        return idx >= 0 ? name.substring(idx + 2) : name;
    }

    private static java.util.Optional<dev.capylang.compiler.CompiledFunction> canonicalNamedFunction(
            java.util.List<dev.capylang.compiler.CompiledFunction> functions
    ) {
        return functions.stream()
                .filter(function -> isNamedIdentifier(baseMethodName(function.name())))
                .findFirst();
    }

    private static boolean isNamedIdentifier(String value) {
        return value.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_');
    }

    private static String overloadSuffix(dev.capylang.compiler.CompiledFunction function) {
        var suffix = function.parameters().stream()
                .map(parameter -> sanitizeOverloadSuffix(overloadTypeName(parameter.type())))
                .collect(joining("__"));
        return suffix.isBlank() ? "" : "__" + suffix;
    }

    private static String methodVariantSuffix(String rawBaseName) {
        var prefix = rawBaseName.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_') ? "name" : "op";
        return prefix + "_" + sanitizeMethodNameVariant(rawBaseName);
    }

    private static String sanitizeMethodNameVariant(String rawName) {
        var builder = new StringBuilder();
        for (var i = 0; i < rawName.length(); i++) {
            var ch = rawName.charAt(i);
            if (Character.isLetterOrDigit(ch)) {
                builder.append(Character.toLowerCase(ch));
            } else if (ch == '_') {
                builder.append('_');
            } else {
                if (!builder.isEmpty() && builder.charAt(builder.length() - 1) != '_') {
                    builder.append('_');
                }
                builder.append(symbolName(ch));
                builder.append('_');
            }
        }
        var sanitized = builder.toString().replaceAll("_+", "_");
        if (sanitized.startsWith("_")) {
            sanitized = sanitized.substring(1);
        }
        if (sanitized.endsWith("_")) {
            sanitized = sanitized.substring(0, sanitized.length() - 1);
        }
        return sanitized.isBlank() ? "generated" : sanitized;
    }

    private static String normalizeJavaMethodIdentifier(String rawName) {
        var leadingUnderscores = countLeadingUnderscores(rawName);
        var suffix = rawName.substring(leadingUnderscores);
        var normalized = normalizeJavaIdentifier(suffix, false);
        if (leadingUnderscores == 0) {
            return normalized;
        }
        return "_".repeat(leadingUnderscores) + normalized;
    }

    private static int countLeadingUnderscores(String value) {
        var count = 0;
        while (count < value.length() && value.charAt(count) == '_') {
            count++;
        }
        return count;
    }

    private static String normalizeJavaIdentifier(String name, boolean upperCamel) {
        var parts = java.util.stream.Stream.of(name.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();

        var base = new StringBuilder();
        if (parts.isEmpty()) {
            base.append(encodeSymbolicIdentifier(name, upperCamel));
        } else {
            for (var i = 0; i < parts.size(); i++) {
                var part = parts.get(i);
                if (i == 0 && !upperCamel) {
                    base.append(Character.toLowerCase(part.charAt(0)));
                } else {
                    base.append(Character.toUpperCase(part.charAt(0)));
                }
                if (part.length() > 1) {
                    base.append(part.substring(1));
                }
            }
        }

        var identifier = base.toString();
        if (!Character.isJavaIdentifierStart(identifier.charAt(0))) {
            identifier = (upperCamel ? "T" : "v") + identifier;
        }
        return switch (identifier) {
            case "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
                    "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
                    "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
                    "interface", "long", "native", "new", "package", "private", "protected", "public",
                    "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
                    "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
                    "null", "record", "sealed", "permits", "var", "yield" -> identifier + "_";
            default -> identifier;
        };
    }

    private static String encodeSymbolicIdentifier(String raw, boolean upperCamel) {
        var parts = new java.util.ArrayList<String>(raw.length());
        for (var i = 0; i < raw.length(); i++) {
            parts.add(symbolName(raw.charAt(i)));
        }
        if (parts.isEmpty()) {
            return upperCamel ? "Generated" : "generated";
        }
        var result = new StringBuilder();
        for (var i = 0; i < parts.size(); i++) {
            var part = parts.get(i);
            if (i == 0 && !upperCamel) {
                result.append(part);
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        return result.toString();
    }

    private static String symbolName(char symbol) {
        return switch (symbol) {
            case '+' -> "plus";
            case '-' -> "minus";
            case '*' -> "star";
            case '/' -> "slash";
            case '\\' -> "backslash";
            case '^' -> "power";
            case '%' -> "mod";
            case '$' -> "dollar";
            case '#' -> "hash";
            case '@' -> "at";
            case '~' -> "tilde";
            case '!' -> "bang";
            case ':' -> "colon";
            case '<' -> "less";
            case '>' -> "greater";
            case '|' -> "pipe";
            default -> "op" + Integer.toHexString(symbol);
        };
    }

    private static String sanitizeOverloadSuffix(String typeName) {
        var sanitized = typeName.replaceAll("[^A-Za-z0-9]+", "_").replaceAll("_+", "_");
        if (sanitized.startsWith("_")) {
            sanitized = sanitized.substring(1);
        }
        if (sanitized.endsWith("_")) {
            sanitized = sanitized.substring(0, sanitized.length() - 1);
        }
        return sanitized.toLowerCase();
    }

    private static String erasedJavaType(dev.capylang.compiler.CompiledType type) {
        return switch (type) {
            case dev.capylang.compiler.CollectionLinkedType.CompiledList ignored -> "java.util.List";
            case dev.capylang.compiler.CollectionLinkedType.CompiledSet ignored -> "java.util.Set";
            case dev.capylang.compiler.CollectionLinkedType.CompiledDict ignored -> "java.util.Map";
            case dev.capylang.compiler.CompiledTupleType ignored -> "java.util.List";
            case dev.capylang.compiler.CompiledFunctionType functionType -> functionType.argumentType() == dev.capylang.compiler.PrimitiveLinkedType.NOTHING
                    ? "java.util.function.Supplier"
                    : "java.util.function.Function";
            case dev.capylang.compiler.PrimitiveLinkedType primitive -> primitive.name();
            case dev.capylang.compiler.CompiledGenericTypeParameter ignored -> "java.lang.Object";
            case dev.capylang.compiler.CompiledPrimitiveBackedType primitiveBackedType -> primitiveBackedType.backingType().name();
            case dev.capylang.compiler.GenericDataType genericDataType -> {
                var name = genericDataType.name();
                var idx = name.indexOf('[');
                yield idx > 0 ? name.substring(0, idx) : name;
            }
        };
    }

    private static String overloadTypeName(dev.capylang.compiler.CompiledType type) {
        if (type instanceof dev.capylang.compiler.CompiledPrimitiveBackedType primitiveBackedType) {
            return primitiveBackedType.name();
        }
        return String.valueOf(type);
    }

    private <T> T time(java.util.function.LongConsumer recorder, Supplier<T> action) {
        var startedAt = System.nanoTime();
        var result = action.get();
        recorder.accept(System.nanoTime() - startedAt);
        return result;
    }

    private static final class GenerationTimings {
        private long astBuildNanos;
        private long sourceRenderNanos;

        private void addAstBuildNanos(long nanos) {
            astBuildNanos += nanos;
        }

        private void addSourceRenderNanos(long nanos) {
            sourceRenderNanos += nanos;
        }

        private long astBuildNanos() {
            return astBuildNanos;
        }

        private long sourceRenderNanos() {
            return sourceRenderNanos;
        }
    }

    private Path relativePath(JavaClass javaClass, String simpleName) {
        var packageName = javaClass.javaPackage().toString();
        if (packageName.isBlank()) {
            return Path.of(simpleName + ".java");
        }
        return Path.of(packageName.replace('.', '/'), simpleName + ".java");
    }

    private String code(JavaClass javaClass, String helperCallOwnerName, boolean allowPrivateStaticMethods) {
        var code = new StringBuilder();

        // package
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");

        // imports
        appendImports(code, javaClass.staticImports());
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }

        // generated annotation
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        // class declaration
        code.append("public class ")
                .append(javaClass.name())
                .append("{");

        // interfaces
        code.append('\n');
        javaClass.interfaces()
                .stream()
                .map(javaInterface -> mapJavaInterface(javaInterface, helperCallOwnerName))
                .forEach(code::append);

        // records
        code.append('\n');
        javaClass.records()
                .stream()
                .map(record -> mapJavaRecord(record, helperCallOwnerName, false))
                .forEach(code::append);

        // enums
        code.append('\n');
        javaClass.enums()
                .stream()
                .map(this::mapJavaEnum)
                .forEach(code::append);

        // static members
        code.append('\n');
        javaClass.staticConsts().stream()
                .map(javaConst -> mapJavaConst(javaConst, allowPrivateStaticMethods, false, javaClass.name().toString()))
                .forEach(code::append);
        javaClass.staticMethods()
                .stream()
                .map(method -> mapJavaMethod(method, allowPrivateStaticMethods, javaClass.javaPackage().toString(), javaClass.name().toString()))
                .forEach(code::append);
        if (requiresUnsupportedHelper(code)) {
            code.append('\n').append(unsupportedHelperMethod());
        }

        // close object declaration
        code.append("}");

        return code.toString();
    }

    private String codeNestedInOwnerInterface(JavaClass javaClass, JavaInterface ownerInterface, String helperCallOwnerName) {
        var code = new StringBuilder();
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");
        appendImports(code, javaClass.staticImports());
        if (!javaClass.staticImports().isEmpty()) {
            code.append('\n');
        }
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));

        code.append(mapJavaOwnerInterfaceHeader(ownerInterface)).append("{\n");
        if (ownerInterface instanceof JavaNormalInterface normalInterface) {
            normalInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
            normalInterface.defaultMethods().stream()
                    .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                    .forEach(code::append);
        }
        if (ownerInterface instanceof JavaSealedInterface sealedInterface) {
            sealedInterface.methods().stream()
                    .map(this::mapJavaInterfaceMethod)
                    .forEach(method -> code.append(method).append('\n'));
            sealedInterface.defaultMethods().stream()
                    .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                    .forEach(code::append);
        }

        javaClass.interfaces().stream()
                .filter(javaInterface -> javaInterface != ownerInterface)
                .map(javaInterface -> mapJavaInterface(javaInterface, helperCallOwnerName))
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.records().stream()
                .map(record -> mapJavaRecord(record, helperCallOwnerName, false))
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.enums().stream()
                .map(this::mapJavaEnum)
                .map(this::removeVisibilityModifier)
                .forEach(code::append);
        javaClass.staticConsts().stream()
                .map(javaConst -> mapJavaConst(javaConst, false, true, javaClass.name().toString()))
                .forEach(code::append);
        javaClass.staticMethods().stream()
                .map(method -> mapJavaMethod(method, true, javaClass.javaPackage().toString(), javaClass.name().toString()))
                .forEach(code::append);
        if (requiresUnsupportedHelper(code)) {
            code.append('\n').append(unsupportedHelperMethod());
        }

        code.append("}\n");
        return code.toString();
    }

    private String removeVisibilityModifier(String declaration) {
        if (declaration.startsWith("public ")) {
            return declaration.substring("public ".length());
        }
        if (declaration.startsWith("private ")) {
            return declaration.substring("private ".length());
        }
        return declaration;
    }

    private String mapJavaInterfaceHeader(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> "public interface " + javaNormalInterface.name() + " ";
            case JavaSealedInterface javaSealedInterface -> {
                var permits = join(", ", javaSealedInterface.permits());
                var typeParameters = javaSealedInterface.typeParameters().isEmpty()
                        ? ""
                        : javaSealedInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
                yield "public sealed interface " + javaSealedInterface.name() + typeParameters + " permits " + permits + " ";
            }
        };
    }

    private String mapJavaOwnerInterfaceHeader(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> "public interface " + javaNormalInterface.name() + " ";
            case JavaSealedInterface javaSealedInterface -> {
                var ownerName = javaSealedInterface.name().toString();
                var permits = javaSealedInterface.permits().stream()
                        .map(permit -> ownerName + "." + permit)
                        .collect(joining(", "));
                var typeParameters = javaSealedInterface.typeParameters().isEmpty()
                        ? ""
                        : javaSealedInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
                yield "public sealed interface " + ownerName + typeParameters + " permits " + permits + " ";
            }
        };
    }

    private String codeTopLevelDeclaration(JavaClass javaClass, String declarationCode) {
        var code = new StringBuilder();
        code.append("package ").append(javaClass.javaPackage()).append(";\n\n");
        javaClass.annotations().forEach(annotation -> code.append(annotation).append("\n"));
        code.append(declarationCode);
        return code.toString();
    }

    private void appendImports(StringBuilder code, Set<String> staticImports) {
        var classImports = new TreeSet<String>();
        staticImports.stream()
                .map(this::classImportForStaticImport)
                .filter(className -> !className.isBlank())
                .forEach(classImport -> {
                    classImports.add(classImport);
                    var companionImport = extractCompanionOwnerImport(classImport);
                    if (!companionImport.isBlank()) {
                        classImports.add(companionImport);
                    }
                });
        classImports.forEach(classImport -> code.append("import ").append(classImport).append(";\n"));
        staticImports.stream()
                .sorted()
                .filter(staticImport -> !isTypeImport(staticImport))
                .forEach(staticImport -> code.append("import static ").append(staticImport).append(";\n"));
    }

    private String classImportForStaticImport(String staticImport) {
        if (!isTypeImport(staticImport)) {
            return extractClassNameFromStaticImport(staticImport);
        }
        var ownerImport = extractClassNameFromStaticImport(staticImport);
        if (ownerImport.isBlank()) {
            return "";
        }
        var member = staticImport.substring(staticImport.lastIndexOf('.') + 1);
        var ownerSimpleName = ownerImport.substring(ownerImport.lastIndexOf('.') + 1);
        return ownerSimpleName.equals(member) ? ownerImport : staticImport;
    }

    private boolean isTypeImport(String staticImport) {
        if (staticImport.endsWith(".*")) {
            return false;
        }
        var lastDot = staticImport.lastIndexOf('.');
        if (lastDot <= 0 || lastDot == staticImport.length() - 1) {
            return false;
        }
        var member = staticImport.substring(lastDot + 1);
        var ownerImport = extractClassNameFromStaticImport(staticImport);
        var ownerSimpleName = ownerImport.substring(ownerImport.lastIndexOf('.') + 1);
        if (ownerSimpleName.equals(member)) {
            return true;
        }
        return Character.isUpperCase(member.charAt(0)) && !member.equals(member.toUpperCase(java.util.Locale.ROOT));
    }

    private String extractCompanionOwnerImport(String classImport) {
        if (!classImport.endsWith("Module")) {
            return "";
        }
        return classImport.substring(0, classImport.length() - "Module".length());
    }

    private String extractClassNameFromStaticImport(String staticImport) {
        if (staticImport.endsWith(".*")) {
            return staticImport.substring(0, staticImport.length() - 2);
        }
        var lastDot = staticImport.lastIndexOf('.');
        if (lastDot <= 0) {
            return "";
        }
        return staticImport.substring(0, lastDot);
    }

    private boolean hasTypeOrDataNameConflictWithFile(JavaClass javaClass) {
        var fileName = javaClass.name().toString();
        return javaClass.interfaces().stream().anyMatch(javaInterface -> javaInterface.name().toString().equals(fileName))
               || javaClass.records().stream().anyMatch(record -> record.name().toString().equals(fileName))
               || javaClass.enums().stream().anyMatch(javaEnum -> javaEnum.name().toString().equals(fileName));
    }

    private List<TopLevelDeclaration> topLevelDeclarations(JavaClass javaClass, String helperCallOwnerName) {
        var declarations = new ArrayList<TopLevelDeclaration>();
        javaClass.interfaces().forEach(javaInterface -> declarations.add(
                new TopLevelDeclaration(javaInterface.name().toString(), mapJavaInterface(javaInterface, helperCallOwnerName))
        ));
        javaClass.records().forEach(javaRecord -> declarations.add(
                new TopLevelDeclaration(javaRecord.name().toString(), mapJavaRecord(javaRecord, helperCallOwnerName, true))
        ));
        javaClass.enums().forEach(javaEnum -> declarations.add(
                new TopLevelDeclaration(javaEnum.name().toString(), mapJavaEnum(javaEnum))
        ));
        return List.copyOf(declarations);
    }

    private record TopLevelDeclaration(String name, String code) {
    }

    private record NativeProviderInfo(
            String providerSymbolName,
            String bootstrapMethodName,
            String interfaceId,
            String qualifier,
            String targetBackendType,
            String sourceModulePath,
            String sourceModuleName,
            String sourceFile,
            NativeProviderLifetime lifetime,
            NativeProviderBackendBinding binding
    ) {
    }

    private record ProviderInterfaceInfo(String backendClassName) {
    }

    private String mapJavaRecord(JavaRecord record, String helperCallOwnerName, boolean topLevel) {
        var fields = record.fields().stream().map(this::mapJavaRecordField).collect(joining(", "));
        var typeParameters = record.typeParameters().isEmpty()
                ? ""
                : record.typeParameters().stream().collect(joining(", ", "<", ">"));
        var implementInterfaces = record.implementInterfaces().size() > 0
                ? record.implementInterfaces().stream()
                .map(Objects::toString)
                .collect(joining(", ", " implements ", " "))
                : "";
        var staticMethods = record.staticMethods().stream()
                .map(method -> mapJavaMethod(method, true, "", record.name().toString()))
                .collect(joining("\n"));
        var methods = record.methods().stream()
                .map(method -> mapJavaRecordMethod(method, helperCallOwnerName))
                .collect(joining("\n"));
        var dataValueInfoMethod = mapJavaRecordDataValueInfoMethod(record);
        var withMethods = mapJavaRecordWithMethods(record);
        var toStringMethod = mapJavaRecordToString(record);
        var visibility = record.isPrivate() ? (topLevel ? "" : "private ") : "public ";
        return mapJavaDoc(record.comments())
               + visibility + " record " + record.name() + typeParameters + "(" + fields + ")" + implementInterfaces + "{"
               + staticMethods + methods + dataValueInfoMethod + withMethods + toStringMethod + "}\n";
    }

    private String mapJavaRecordField(JavaRecord.JavaRecordField field) {
        return field.type() + " " + field.name();
    }


    private String mapJavaRecordWithMethods(JavaRecord record) {
        if (record.methods().stream().anyMatch(method -> "with".equals(method.name()))) {
            return "";
        }
        var parameters = record.fields().stream().map(this::mapJavaRecordField).collect(joining(", "));
        var arguments = record.fields().stream().map(JavaRecord.JavaRecordField::name).collect(joining(", "));
        if (record.fields().isEmpty()) {
            return "public " + record.name() + " with() { return this; }\n";
        }
        return "public " + record.name() + " with(" + parameters + ") {\n"
               + "return new " + record.name() + "(" + arguments + ");\n"
               + "}\n";
    }

    private String mapJavaRecordToString(JavaRecord record) {
        if (record.methods().stream().anyMatch(method -> "toString".equals(method.name()))) {
            return "";
        }
        if (record.fields().isEmpty()) {
            return "@Override public java.lang.String toString() { return \"" + record.name() + " { }\"; }\n";
        }
        var body = new StringBuilder();
        body.append("@Override public java.lang.String toString() { return \"")
                .append(record.name())
                .append(" { \"");
        for (int i = 0; i < record.fields().size(); i++) {
            var field = record.fields().get(i);
            if (i > 0) {
                body.append(" + \", \"");
            }
            var displayFieldName = mapRecordFieldDisplayName(record, field);
            body.append(" + \"\\\"")
                    .append(displayFieldName)
                    .append("\\\": \" + ")
                    .append(mapRecordFieldToStringValue(field));
        }
        body.append(" + \" }\"; }\n");
        return body.toString();
    }

    private String mapRecordFieldDisplayName(JavaRecord record, JavaRecord.JavaRecordField field) {
        if ("Error".equals(record.name().toString())
            && "ex".equals(field.name())
            && "dev.capylang.CapybaraException".equals(field.type().toString())) {
            return "message";
        }
        return field.name();
    }

    private String mapRecordFieldToStringValue(JavaRecord.JavaRecordField field) {
        return "dev.capylang.CapybaraToStringUtil.toStringValue(" + field.name() + ")";
    }

    private String mapJavaRecordDataValueInfoMethod(JavaRecord record) {
        return "@Override public java.lang.Object capybaraDataValueInfo() {\n"
               + "return " + ReflectionValueInfoJava.dataValueInfoExpression(record.dataValueInfo()) + ";\n"
               + "}\n";
    }

    private String mapJavaEnum(JavaEnum javaEnum) {
        var implementInterfaces = javaEnum.implementInterfaces().isEmpty()
                ? ""
                : javaEnum.implementInterfaces().stream().map(Objects::toString).collect(joining(", ", " implements ", " "));
        var values = javaEnum.values().isEmpty() ? List.of("INSTANCE") : javaEnum.values();
        var body = String.join(", ", values);
        var dataValueInfoMethod = mapJavaEnumDataValueInfoMethod(javaEnum, values);
        if (values.size() == 1 && "INSTANCE".equals(values.getFirst())) {
            return mapJavaDoc(javaEnum.comments())
                   + "public enum " + javaEnum.name() + implementInterfaces + "{"
                   + body + ";\n"
                   + dataValueInfoMethod
                   + "}\n";
        }
        return mapJavaDoc(javaEnum.comments())
               + "public enum " + javaEnum.name() + implementInterfaces + "{"
               + body + ";\n"
               + dataValueInfoMethod
               + "public static java.util.Set<" + javaEnum.name() + "> valuesSet() {\n"
               + "return java.util.EnumSet.allOf(" + javaEnum.name() + ".class);\n"
               + "}\n"
               + "public static capy.lang.Result<" + javaEnum.name() + "> parse(java.lang.String name) {\n"
               + "try {\n"
               + "return new capy.lang.Result.Success<>(" + javaEnum.name() + ".valueOf(name));\n"
               + "} catch (java.lang.IllegalArgumentException ex) {\n"
               + "return new capy.lang.Result.Error(new dev.capylang.CapybaraException(\"Cannot parse enum `"
               + javaEnum.name()
               + "` from string: \" + name));\n"
               + "}\n"
               + "}\n"
               + "public static capy.lang.Result<" + javaEnum.name() + "> parse(int order) {\n"
               + "var all = " + javaEnum.name() + ".values();\n"
               + "if (order < 0 || order >= all.length) {\n"
               + "return new capy.lang.Result.Error(new dev.capylang.CapybaraException(\"Cannot parse enum `"
               + javaEnum.name()
               + "` from order: \" + order));\n"
               + "}\n"
               + "return new capy.lang.Result.Success<>(all[order]);\n"
               + "}\n"
               + "}\n";
    }

    private String mapJavaEnumDataValueInfoMethod(JavaEnum javaEnum, List<String> values) {
        if (values.size() == 1 && "INSTANCE".equals(values.getFirst())) {
            return "@Override public java.lang.Object capybaraDataValueInfo() {\n"
                   + "return " + ReflectionValueInfoJava.dataValueInfoExpression(javaEnum.dataValueInfos().getFirst()) + ";\n"
                   + "}\n";
        }
        var cases = new StringBuilder();
        for (var i = 0; i < values.size(); i++) {
            cases.append("case ")
                    .append(values.get(i))
                    .append(" -> ")
                    .append(ReflectionValueInfoJava.dataValueInfoExpression(javaEnum.dataValueInfos().get(i)))
                    .append(";\n");
        }
        return "@Override public java.lang.Object capybaraDataValueInfo() {\n"
               + "return switch (this) {\n"
               + cases
               + "};\n"
               + "}\n";
    }


    private String mapJavaInterface(JavaInterface javaInterface, String helperCallOwnerName) {
        return switch (javaInterface) {
            case JavaNormalInterface javaNormalInterface -> mapJavaNormalInterface(javaNormalInterface, helperCallOwnerName);
            case JavaSealedInterface javaSealedInterface -> mapJavaSealedInterface(javaSealedInterface, helperCallOwnerName);
        };
    }

    private String mapJavaNormalInterface(JavaNormalInterface javaInterface, String helperCallOwnerName) {
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        var defaultMethods = javaInterface.defaultMethods().stream()
                .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                .collect(joining());
        return mapJavaDoc(javaInterface.comments())
               + "public interface " + javaInterface.name() + " {" + methods + defaultMethods + "}\n";
    }

    private String mapJavaSealedInterface(JavaSealedInterface javaInterface, String helperCallOwnerName) {
        var permits = join(", ", javaInterface.permits());
        var typeParameters = javaInterface.typeParameters().isEmpty()
                ? ""
                : javaInterface.typeParameters().stream().collect(joining(", ", "<", ">"));
        var methods = javaInterface.methods().size() > 0
                ? javaInterface.methods().stream()
                .map(this::mapJavaInterfaceMethod)
                .collect(joining("\n", "\n", ""))
                : "";
        var defaultMethods = javaInterface.defaultMethods().stream()
                .map(method -> mapJavaInterfaceDefaultMethod(method, helperCallOwnerName))
                .collect(joining());
        return mapJavaDoc(javaInterface.comments())
               + "public sealed interface " + javaInterface.name() + typeParameters + " permits " + permits + " {" + methods + defaultMethods + "}\n";
    }

    private String mapJavaInterfaceMethod(JavaInterface.JavaInterfaceMethod method) {
        return method.returnType() + " " + method.name() + "();";
    }

    private String mapJavaInterfaceDefaultMethod(JavaMethod method, String helperCallOwnerName) {
        var prefix = method.isPrivate() ? "private" : "default";
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + prefix + " " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateMethodBody(method, helperCallOwnerName)
               + "\n}\n";
    }

    private String mapJavaMethod(JavaMethod method, boolean allowPrivateStaticMethods, String ownerPackage, String ownerName) {
        if (method.programMain()) {
            return mapJavaProgramMainMethod(method);
        }
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        var visibility = method.isPrivate()
                ? (allowPrivateStaticMethods ? "private " : "")
                : "public ";
        if (isCapyDateTimeClockNowMethod(ownerPackage, ownerName, method)) {
            return mapCapyDateTimeClockNowMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyLangSystemCurrentMillisMethod(ownerPackage, ownerName, method)) {
            return mapCapyLangSystemCurrentMillisMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyLangSystemNanoTimeMethod(ownerPackage, ownerName, method)) {
            return mapCapyLangSystemNanoTimeMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyLangRandomSeedMethod(ownerPackage, ownerName, method)) {
            return mapCapyLangRandomSeedMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyLangPrimitivesStringParseMethod(ownerPackage, ownerName, method)) {
            return mapCapyLangPrimitivesStringParseMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyIoConsoleMethod(ownerPackage, ownerName, method)) {
            return mapCapyIoConsoleMethod(method, visibility, methodTypeParameters);
        }
        if (isCapyIoIOMethod(ownerPackage, ownerName, method)) {
            return mapCapyIoIOMethod(method, visibility, methodTypeParameters);
        }
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateMethodBody(method, null)
               + "\n}\n";
    }

    private boolean isCapyDateTimeClockNowMethod(String ownerPackage, String ownerName, JavaMethod method) {
        return "capy.dateTime".equals(ownerPackage)
               && "Clock".equals(ownerName)
               && "now".equals(mapMethodName(method.name()))
               && method.parameters().isEmpty()
               && isEffectTypeReference(method.returnType().toString());
    }

    private String mapCapyDateTimeClockNowMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "() {\n"
               + "return capy.lang.Effect.delay(() -> dev.capylang.DateTimeUtil.fromJavaOffsetDateTime(java.time.OffsetDateTime.now(java.time.ZoneOffset.UTC)));\n"
               + "}\n";
    }

    private boolean isCapyLangSystemCurrentMillisMethod(String ownerPackage, String ownerName, JavaMethod method) {
        return "capy.lang".equals(ownerPackage)
               && "System".equals(ownerName)
               && "currentMillis".equals(mapMethodName(method.name()))
               && method.parameters().isEmpty()
               && isEffectTypeReference(method.returnType().toString());
    }

    private String mapCapyLangSystemCurrentMillisMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "() {\n"
               + "return capy.lang.Effect.delay(java.lang.System::currentTimeMillis);\n"
               + "}\n";
    }

    private boolean isCapyLangSystemNanoTimeMethod(String ownerPackage, String ownerName, JavaMethod method) {
        return "capy.lang".equals(ownerPackage)
               && "System".equals(ownerName)
               && "nanoTime".equals(mapMethodName(method.name()))
               && method.parameters().isEmpty()
               && isEffectTypeReference(method.returnType().toString());
    }

    private String mapCapyLangSystemNanoTimeMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "() {\n"
               + "return capy.lang.Effect.delay(java.lang.System::nanoTime);\n"
               + "}\n";
    }

    private boolean isCapyLangRandomSeedMethod(String ownerPackage, String ownerName, JavaMethod method) {
        return "capy.lang".equals(ownerPackage)
               && "Random".equals(ownerName)
               && "seed".equals(mapMethodName(method.name()))
               && method.parameters().isEmpty()
               && method.sourceReturnType() instanceof dev.capylang.compiler.CompiledPrimitiveBackedType primitiveBackedType
               && "seed".equals(primitiveBackedType.name())
               && isNativeExpression(method);
    }

    private String mapCapyLangRandomSeedMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "() {\n"
               + "return java.util.concurrent.ThreadLocalRandom.current().nextLong();\n"
               + "}\n";
    }

    private boolean isCapyLangPrimitivesStringParseMethod(String ownerPackage, String ownerName, JavaMethod method) {
        return "capy.lang".equals(ownerPackage)
               && "Primitives".equals(ownerName)
               && method.parameters().size() == 1
               && method.sourceParameterTypes().size() == 1
               && method.sourceParameterTypes().getFirst() == PrimitiveLinkedType.STRING
               && isNativeExpression(method)
               && switch (method.sourceName()) {
                   case "to_int", "to_long", "to_double", "to_float", "to_bool" -> true;
                   default -> false;
               };
    }

    private String mapCapyLangPrimitivesStringParseMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        var parameter = method.parameters().getFirst();
        var syntheticMethodCall = new CompiledFunctionCall(
                METHOD_DECL_PREFIX + "String__" + method.sourceName(),
                List.of(new CompiledVariable(parameter.sourceName(), PrimitiveLinkedType.STRING)),
                method.sourceReturnType()
        );
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateExpression(syntheticMethodCall, method.parameters(), null)
               + "\n}\n";
    }

    private boolean isNativeExpression(JavaMethod method) {
        return method.expression() instanceof dev.capylang.compiler.expression.CompiledNothingValue nothingValue
               && (nothingValue.message().contains("`<native>`")
                   || nothingValue.message().contains("native expression in function"));
    }

    private boolean isCapyIoConsoleMethod(String ownerPackage, String ownerName, JavaMethod method) {
        if (!"capy.io".equals(ownerPackage) || !"Console".equals(ownerName) || !isEffectTypeReference(method.returnType().toString())) {
            return false;
        }
        var methodName = mapMethodName(method.name());
        return switch (methodName) {
            case "print", "println", "printError", "printlnError" ->
                    method.parameters().size() == 1;
            case "readLine" -> method.parameters().isEmpty();
            default -> false;
        };
    }

    private String mapCapyIoConsoleMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        var methodName = mapMethodName(method.name());
        var runtimeCall = switch (methodName) {
            case "print", "println", "printError", "printlnError" ->
                    "dev.capylang.ConsoleUtil." + methodName + "(" + method.parameters().getFirst().generatedName() + ")";
            case "readLine" -> "dev.capylang.ConsoleUtil.readLine()";
            default -> throw new IllegalStateException("Unsupported Console method: " + methodName);
        };
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + methodName + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + "return capy.lang.Effect.delay(() -> " + runtimeCall + ");\n"
               + "}\n";
    }

    private boolean isCapyIoIOMethod(String ownerPackage, String ownerName, JavaMethod method) {
        if (!"capy.io".equals(ownerPackage) || !"IO".equals(ownerName) || !isEffectTypeReference(method.returnType().toString())) {
            return false;
        }
        var methodName = mapMethodName(method.name());
        return switch (methodName) {
            case "readText", "readLines", "readBytes", "exists", "isFile", "isDirectory", "size",
                 "createFile", "createDirectory", "createDirectories", "listEntries", "delete" ->
                    method.parameters().size() == 1;
            case "writeText", "writeLines", "writeBytes", "appendText", "appendLines", "appendBytes",
                 "copy", "copyReplace", "move", "moveReplace" ->
                    method.parameters().size() == 2;
            default -> false;
        };
    }

    private String mapCapyIoIOMethod(JavaMethod method, String visibility, String methodTypeParameters) {
        var methodName = mapMethodName(method.name());
        var arguments = method.parameters().stream()
                .map(JavaMethod.JavaFunctionParameter::generatedName)
                .collect(joining(", "));
        return mapJavaDoc(method.comments())
               + visibility + "static " + methodTypeParameters + method.returnType() + " " + methodName + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + "return capy.lang.Effect.delay(() -> dev.capylang.IOUtil." + methodName + "(" + arguments + "));\n"
               + "}\n";
    }

    private String mapJavaRecordMethod(JavaMethod method, String helperCallOwnerName) {
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        return mapJavaDoc(method.comments())
               + (method.isPrivate() ? "private" : "public") + " " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateMethodBody(method, helperCallOwnerName)
               + "\n}\n";
    }

    private String mapJavaProgramMainMethod(JavaMethod method) {
        var methodTypeParameters = method.typeParameters().isEmpty()
                ? ""
                : method.typeParameters().stream().collect(joining(", ", "<", ">")) + " ";
        var capybaraMainMethod = mapJavaDoc(method.comments())
               + "public static " + methodTypeParameters + method.returnType() + " " + mapMethodName(method.name()) + "(" + mapFunctionParameters(method.parameters()) + ") {\n"
               + evaluateMethodBody(method, null)
               + "\n}\n";
        var programType = normalizeProgramTypeReference(effectPayloadTypeReference(method.returnType().toString()));
        var failedType = programType + ".Failed";
        return capybaraMainMethod
               + "\n"
               + "public static final void main(String... args) {\n"
               + "var __capybaraArgsList = java.util.List.of(args);\n"
               + programType + " __capybaraProgram = " + mapMethodName(method.name()) + "(__capybaraArgsList).unsafeRun();\n"
               + "if (__capybaraProgram instanceof " + failedType + " __capybaraFailed) {\n"
               + "System.exit(__capybaraFailed.exit_code());\n"
               + "}\n"
               + "}\n";
    }

    private String evaluateMethodBody(JavaMethod method, String moduleHelperClass) {
        if (!method.tailRecursive()) {
            return evaluateExpression(method.expression(), method.parameters(), moduleHelperClass);
        }
        return evaluateTailRecursiveExpression(
                method.expression(),
                method.parameters(),
                method.selfCallNames(),
                method.sourceReturnType(),
                method.sourceParameterTypes(),
                moduleHelperClass
        );
    }

    private String normalizeProgramTypeReference(String typeName) {
        var parts = typeName.split("\\.");
        if (parts.length >= 2 && parts[parts.length - 1].equals(parts[parts.length - 2])) {
            return String.join(".", java.util.Arrays.copyOf(parts, parts.length - 1));
        }
        return typeName;
    }

    private boolean isEffectTypeReference(String typeName) {
        var normalized = typeName.replace(" ", "");
        var genericStart = normalized.indexOf('<');
        var rawType = genericStart >= 0 ? normalized.substring(0, genericStart) : normalized;
        return "Effect".equals(rawType) || rawType.endsWith(".Effect");
    }

    private String effectPayloadTypeReference(String typeName) {
        var normalized = typeName.replace(" ", "");
        var genericStart = normalized.indexOf('<');
        if (genericStart < 0 || !normalized.endsWith(">")) {
            return "capy.lang.Program";
        }
        return normalized.substring(genericStart + 1, normalized.length() - 1);
    }

    private String mapJavaDoc(List<String> comments) {
        if (comments == null || comments.isEmpty()) {
            return "";
        }
        return comments.stream()
                .map(line -> line.isEmpty() ? "///" : "/// " + line)
                .collect(joining("\n", "", "\n"));
    }

    private String mapJavaConst(
            JavaConst javaConst,
            boolean allowPrivateStaticMembers,
            boolean ownerInterfaceMember,
        String ownerTypeName
    ) {
        var visibility = constVisibility(javaConst, allowPrivateStaticMembers, ownerInterfaceMember);
        return mapJavaDoc(javaConst.comments())
               + visibility + "static final " + javaConst.type() + " " + javaConst.name() + " = "
               + extractInitializerExpression(evaluateExpression(javaConst.expression(), List.of(), ownerTypeName))
               + ";\n";
    }

    private String constVisibility(JavaConst javaConst, boolean allowPrivateStaticMembers, boolean ownerInterfaceMember) {
        if (ownerInterfaceMember) {
            return "public ";
        }
        if (javaConst.isPrivate()) {
            return allowPrivateStaticMembers ? "private " : "";
        }
        return "public ";
    }

    private String extractInitializerExpression(String methodBody) {
        var trimmed = methodBody.trim();
        if (trimmed.startsWith("return ") && trimmed.endsWith(";")) {
            return trimmed.substring("return ".length(), trimmed.length() - 1).trim();
        }
        throw new IllegalStateException("Const initializer should be a simple expression: " + methodBody);
    }

    private String mapMethodName(String name) {
        if (name.startsWith(METHOD_DECL_PREFIX)) {
            var idx = name.lastIndexOf("__");
            if (idx >= 0 && idx + 2 < name.length()) {
                return name.substring(idx + 2);
            }
        }
        return name;
    }

    private String mapFunctionParameters(List<JavaMethod.JavaFunctionParameter> parameters) {
        return parameters.stream()
                .map(this::mapFunctionParameter)
                .collect(joining(", "));
    }

    private String mapFunctionParameter(JavaMethod.JavaFunctionParameter parameter) {
        return parameter.type() + " " + parameter.generatedName();
    }

    private boolean requiresUnsupportedHelper(StringBuilder code) {
        var marker = "__capybaraUnsupported(\"";
        var generatedCode = code.toString();
        var index = generatedCode.indexOf(marker);
        while (index >= 0) {
            if (index == 0) {
                return true;
            }
            var previousChar = generatedCode.charAt(index - 1);
            if (previousChar != '\\' && previousChar != '"') {
                return true;
            }
            index = generatedCode.indexOf(marker, index + 1);
        }
        return false;
    }

    private String unsupportedHelperMethod() {
        return "private static <T> T __capybaraUnsupported(String message) {\n"
               + "throw new java.lang.UnsupportedOperationException(message);\n"
               + "}\n";
    }

}
