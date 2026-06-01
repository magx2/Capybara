package dev.capylang.generator.java;

import dev.capylang.compiler.*;
import dev.capylang.compiler.CompiledDict;
import dev.capylang.compiler.CompiledList;
import dev.capylang.compiler.CompiledSet;

import java.util.*;
import java.util.stream.Stream;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.*;
import static dev.capylang.generator.java.JavaHelperAst.generatedAnnotation;

public class JavaAstBuilder {
    private static final String PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX = "__constructor__primitive__";
    private static final Comparator<CompiledDataParentType> COMPILED_DATA_PARENT_TYPE_COMPARATOR =
            Comparator.comparing(CompiledDataParentType::name);
    private static final Comparator<CompiledDataType> COMPILED_DATA_TYPE_COMPARATOR =
            Comparator.comparing(CompiledDataType::name);
    private static final Comparator<CompiledPrimitiveBackedType> COMPILED_PRIMITIVE_BACKED_TYPE_COMPARATOR =
            Comparator.comparing(CompiledPrimitiveBackedType::name);
    private static final Comparator<JavaType> JAVA_TYPE_COMPARATOR =
            Comparator.comparing(JavaHelperAst::javaTypeSortKey);
    private static final Comparator<JavaConst> JAVA_CONST_COMPARATOR =
            Comparator.comparing(JavaHelperAst::javaConstSortKey);
    private static final Comparator<JavaMethod> JAVA_METHOD_COMPARATOR =
            Comparator.comparing(JavaHelperAst::javaMethodSortKey);
    private static final Comparator<JavaInterface> JAVA_INTERFACE_COMPARATOR =
            Comparator.comparing(JavaHelperAst::javaInterfaceSortKey);
    private static final Comparator<JavaRecord> JAVA_RECORD_COMPARATOR =
            Comparator.comparing(JavaHelperAst::javaRecordSortKey);
    private static final Comparator<JavaEnum> JAVA_ENUM_COMPARATOR =
            Comparator.comparing(JavaHelperAst::javaEnumSortKey);
    private final Map<String, String> functionNameOverrides;
    private final Map<String, String> enumValueOwnerOverrides;
    private final Map<String, PrimitiveBackedTypeInfo> globalPrimitiveBackedTypes;
    private final Map<String, SortedSet<JavaType>> globalInterfaceExtendsByName;
    private final Map<String, SortedSet<JavaType>> globalInterfacesBySubtypeName;

    private static boolean sameType(CompiledType actual, CompiledType expected) {
        return expected.equals(actual);
    }

    public JavaAstBuilder() {
        this(Map.of(), Map.of());
    }

    public JavaAstBuilder(Map<String, String> functionNameOverrides) {
        this(functionNameOverrides, Map.of());
    }

    public JavaAstBuilder(Map<String, String> functionNameOverrides, Map<String, String> enumValueOwnerOverrides) {
        this(functionNameOverrides, enumValueOwnerOverrides, Map.of());
    }

    public JavaAstBuilder(
            Map<String, String> functionNameOverrides,
            Map<String, String> enumValueOwnerOverrides,
            CompiledProgram program
    ) {
        this(
                functionNameOverrides,
                enumValueOwnerOverrides,
                primitiveBackedTypes(program),
                globalInterfaceExtendsByName(program),
                globalInterfacesBySubtypeName(program)
        );
    }

    private JavaAstBuilder(
            Map<String, String> functionNameOverrides,
            Map<String, String> enumValueOwnerOverrides,
            Map<String, PrimitiveBackedTypeInfo> globalPrimitiveBackedTypes
    ) {
        this(functionNameOverrides, enumValueOwnerOverrides, globalPrimitiveBackedTypes, Map.of(), Map.of());
    }

    private JavaAstBuilder(
            Map<String, String> functionNameOverrides,
            Map<String, String> enumValueOwnerOverrides,
            Map<String, PrimitiveBackedTypeInfo> globalPrimitiveBackedTypes,
            Map<String, SortedSet<JavaType>> globalInterfaceExtendsByName,
            Map<String, SortedSet<JavaType>> globalInterfacesBySubtypeName
    ) {
        this.functionNameOverrides = Map.copyOf(functionNameOverrides);
        this.enumValueOwnerOverrides = Map.copyOf(enumValueOwnerOverrides);
        this.globalPrimitiveBackedTypes = Map.copyOf(globalPrimitiveBackedTypes);
        this.globalInterfaceExtendsByName = copyInterfaceMap(globalInterfaceExtendsByName);
        this.globalInterfacesBySubtypeName = copyInterfaceMap(globalInterfacesBySubtypeName);
    }
    private static final java.util.regex.Pattern CONST_NAME_PATTERN = java.util.regex.Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");
    private final Map<String, String> normalizedJavaClassReferenceCache = new HashMap<>();
    private final Map<String, String> normalizedRawTypeReferenceCache = new HashMap<>();
    private final Map<String, String> mappedTypeParameterDescriptorCache = new HashMap<>();
    private final Map<String, List<String>> topLevelDescriptorSplitCache = new HashMap<>();
    private Set<String> localTypeNames = Set.of();
    private Map<String, PrimitiveBackedTypeInfo> primitiveBackedTypes = Map.of();

    private static SortedSet<JavaType> javaTypeSet() {
        return new TreeSet<>(JAVA_TYPE_COMPARATOR);
    }

    private static SortedSet<JavaType> javaTypeSet(Collection<JavaType> values) {
        var result = javaTypeSet();
        result.addAll(values);
        return result;
    }

    private static SortedSet<JavaInterface> javaInterfaceSet() {
        return new TreeSet<>(JAVA_INTERFACE_COMPARATOR);
    }

    private static <T> List<T> sortedUniqueList(Stream<T> values, Comparator<? super T> comparator) {
        var sorted = new TreeSet<T>(comparator);
        values.forEach(sorted::add);
        return List.copyOf(sorted);
    }

    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final JavaType CAPYBARA_DATA_VALUE = new JavaType("dev.capylang.CapybaraDataValue");
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );

    public JavaClass build(CompiledModule module) {
        var previousLocalTypeNames = localTypeNames;
        var previousPrimitiveBackedTypes = primitiveBackedTypes;
        localTypeNames = module.types().keySet().stream()
                .map(JavaAstBuilder::simpleTypeName)
                .collect(toUnmodifiableSet());
        var mergedPrimitiveBackedTypes = new LinkedHashMap<>(globalPrimitiveBackedTypes);
        mergedPrimitiveBackedTypes.putAll(primitiveBackedTypes(module));
        primitiveBackedTypes = Map.copyOf(mergedPrimitiveBackedTypes);
        try {
            normalizedRawTypeReferenceCache.clear();
            mappedTypeParameterDescriptorCache.clear();
            return buildModule(module);
        } finally {
            localTypeNames = previousLocalTypeNames;
            primitiveBackedTypes = previousPrimitiveBackedTypes;
        }
    }

    private JavaClass buildModule(CompiledModule module) {
        var typeIndex = indexTypes(new TreeMap<>(module.types()));
        var functions = new LinkedHashSet<>(module.functions());
        var functionsByOwnerPrefix = indexFunctionsByOwnerPrefix(functions);
        var javaPackageName = buildJavaPackageName(module.path());
        var javaClassName = buildClassName(module.name());
        var reflectionFallbackPackagePath = reflectionFallbackPackagePath(module);
        var staticMethodsClassName = staticMethodsClassName(module, javaClassName.toString());
        var staticMethodsSelfCallClassNames = staticMethodsSelfCallClassNames(module, javaPackageName, staticMethodsClassName);
        var nativeDataTypeNames = typeIndex.dataTypes().stream()
                .filter(CompiledDataType::nativeType)
                .map(CompiledDataType::name);
        var primitiveBackedTypeNames = typeIndex.primitiveBackedTypes().stream()
                .map(CompiledPrimitiveBackedType::name);
        var nativeTypeNames = Stream.concat(nativeDataTypeNames, primitiveBackedTypeNames)
                .collect(toSet());
        var interfaces = buildInterfaces(module, typeIndex.dataParentTypes().stream()
                .filter(parentType -> !parentType.enumType())
                .collect(toCollection(() -> new TreeSet<>(COMPILED_DATA_PARENT_TYPE_COMPARATOR))), functionsByOwnerPrefix);
        var subClassToInterface = findSubClassToInterface(typeIndex.dataParentTypes(), interfaces);
        var enumValueOwnerOverrides = enumValueOwnerOverrides(typeIndex);
        return new JavaClass(
                List.of(generatedAnnotation()),
                javaClassName,
                new JavaPackage(javaPackageName),
                module.staticImports().stream()
                        .map(this::buildJavaStaticImport)
                        .collect(collectingAndThen(toCollection(TreeSet::new), List::copyOf)),
                buildStaticConsts(functions),
                buildStaticMethods(functions, staticMethodsSelfCallClassNames, nativeTypeNames),
                interfaces,
                buildRecords(module, typeIndex.dataTypes(), subClassToInterface, functionsByOwnerPrefix, reflectionFallbackPackagePath),
                buildEnums(module, typeIndex, subClassToInterface, reflectionFallbackPackagePath),
                enumValueOwnerOverrides);
    }

    private String reflectionFallbackPackagePath(CompiledModule module) {
        var modulePath = module.path().replace('\\', '/').replaceFirst("^/", "");
        if (modulePath.isBlank()) {
            return module.name();
        }
        return modulePath + "/" + module.name();
    }

    private String staticMethodsClassName(CompiledModule module, String javaClassName) {
        var ownerType = module.types().get(module.name());
        if (ownerType == null || ownerType instanceof CompiledDataParentType) {
            return javaClassName;
        }
        return javaClassName + "Module";
    }

    private List<String> staticMethodsSelfCallClassNames(CompiledModule module, String javaPackageName, String staticMethodsClassName) {
        var generatedClassName = javaPackageName.isBlank()
                ? staticMethodsClassName
                : javaPackageName + "." + staticMethodsClassName;
        return Stream.of(generatedClassName, linkedStaticMethodsClassName(module))
                .distinct()
                .toList();
    }

    private String linkedStaticMethodsClassName(CompiledModule module) {
        var javaPackageName = buildJavaPackageName(module.path());
        var javaClassName = buildClassName(module.name()).toString();
        var className = javaPackageName.isBlank()
                ? javaClassName
                : javaPackageName + "." + javaClassName;
        var ownerType = module.types().get(module.name());
        if (ownerType == null || ownerType instanceof CompiledDataParentType) {
            return className;
        }
        return className + "Module";
    }

    private ModuleTypeIndex indexTypes(SortedMap<String, GenericDataType> types) {
        var dataParentTypes = new TreeSet<>(COMPILED_DATA_PARENT_TYPE_COMPARATOR);
        var dataTypes = new TreeSet<>(COMPILED_DATA_TYPE_COMPARATOR);
        var primitiveBackedTypes = new TreeSet<>(COMPILED_PRIMITIVE_BACKED_TYPE_COMPARATOR);
        var enumValueTypeNames = new HashSet<String>();
        for (var type : types.values()) {
            if (type instanceof CompiledDataParentType parentType) {
                dataParentTypes.add(parentType);
                if (parentType.enumType()) {
                    parentType.subTypes().stream()
                            .map(CompiledDataType::name)
                            .forEach(enumValueTypeNames::add);
                }
            } else if (type instanceof CompiledDataType dataType) {
                dataTypes.add(dataType);
            } else if (type instanceof CompiledPrimitiveBackedType primitiveBackedType) {
                primitiveBackedTypes.add(primitiveBackedType);
            }
        }
        return new ModuleTypeIndex(dataParentTypes, dataTypes, primitiveBackedTypes, Set.copyOf(enumValueTypeNames));
    }

    private Map<String, PrimitiveBackedTypeInfo> primitiveBackedTypes(CompiledModule module) {
        var result = new LinkedHashMap<String, PrimitiveBackedTypeInfo>();
        module.visiblePrimitiveBackedTypes()
                .forEach((alias, type) -> putPrimitiveBackedTypeAlias(result, alias, type, true));
        for (var type : module.types().values()) {
            if (type instanceof CompiledPrimitiveBackedType primitiveBackedType) {
                var info = new PrimitiveBackedTypeInfo(
                        primitiveBackedType.cfunType(),
                        primitiveBackedType.backingType()
                );
                result.put(primitiveBackedType.name(), info);
                result.put(simpleTypeName(primitiveBackedType.name()), info);
            }
        }
        return Map.copyOf(result);
    }

    private static Map<String, PrimitiveBackedTypeInfo> primitiveBackedTypes(CompiledProgram program) {
        var result = new LinkedHashMap<String, PrimitiveBackedTypeInfo>();
        for (var module : program.modules()) {
            module.visiblePrimitiveBackedTypes()
                    .forEach((alias, type) -> putPrimitiveBackedTypeAlias(result, alias, type, false));
            for (var type : module.types().values()) {
                if (type instanceof CompiledPrimitiveBackedType primitiveBackedType) {
                    var info = new PrimitiveBackedTypeInfo(
                            primitiveBackedType.cfunType(),
                            primitiveBackedType.backingType()
                    );
                    putPrimitiveBackedTypeAliases(result, info, primitiveBackedType, module);
                }
            }
            for (var function : module.functions()) {
                function.parameters().stream()
                        .flatMap(parameter -> primitiveBackedTypes(parameter.type()))
                        .forEach(type -> putPrimitiveBackedTypeAliases(result, type));
                primitiveBackedTypes(function.returnType())
                        .forEach(type -> putPrimitiveBackedTypeAliases(result, type));
            }
        }
        putUniqueSimplePrimitiveBackedTypeAliases(result);
        return Map.copyOf(result);
    }

    private static Map<String, SortedSet<JavaType>> copyInterfaceMap(Map<String, SortedSet<JavaType>> source) {
        var copy = new LinkedHashMap<String, SortedSet<JavaType>>();
        source.forEach((key, value) -> copy.put(key, Collections.unmodifiableSortedSet(javaTypeSet(value))));
        return Map.copyOf(copy);
    }

    private static Map<String, SortedSet<JavaType>> globalInterfaceExtendsByName(CompiledProgram program) {
        var interfaces = globalInterfacesBySubtypeName(program);
        var result = new LinkedHashMap<String, SortedSet<JavaType>>();
        var interfaceNames = program.modules().stream()
                .flatMap(module -> module.types().values().stream()
                        .filter(CompiledDataParentType.class::isInstance)
                        .map(CompiledDataParentType.class::cast)
                        .filter(parentType -> !parentType.enumType())
                        .map(parentType -> moduleTypeKey(module, parentType.name())))
                .collect(toSet());
        interfaces.forEach((subtypeName, parents) -> {
            if (interfaceNames.contains(subtypeName)) {
                result.computeIfAbsent(subtypeName, ignored -> javaTypeSet()).addAll(parents);
            }
        });
        return result;
    }

    private static Map<String, SortedSet<JavaType>> globalInterfacesBySubtypeName(CompiledProgram program) {
        var result = new LinkedHashMap<String, SortedSet<JavaType>>();
        for (var module : program.modules()) {
            for (var type : module.types().values()) {
                if (!(type instanceof CompiledDataParentType parentType) || parentType.enumType()) {
                    continue;
                }
                var interfaceName = buildClassName(parentType.name());
                for (var subtype : parentType.subTypes()) {
                    result.computeIfAbsent(moduleTypeKey(module, subtype.name()), ignored -> javaTypeSet())
                            .add(interfaceName);
                }
            }
        }
        return result;
    }

    private static String moduleTypeKey(CompiledModule module, String typeName) {
        var modulePath = module.path().replace('\\', '/').replaceFirst("^/", "");
        return modulePath + "/" + module.name() + "#" + simpleTypeName(typeName);
    }

    private static String rawJavaTypeName(JavaType type) {
        var name = type.name();
        var genericStart = name.indexOf('<');
        if (genericStart < 0) {
            return name;
        }
        return name.substring(0, genericStart);
    }

    private SortedSet<JavaType> globalImplementedInterfaces(
            CompiledModule module,
            String typeName,
            SortedSet<JavaType> currentInterfaces
    ) {
        var representedRawNames = currentInterfaces.stream()
                .map(JavaAstBuilder::rawJavaTypeName)
                .collect(toSet());
        return globalInterfacesBySubtypeName.getOrDefault(moduleTypeKey(module, typeName), javaTypeSet()).stream()
                .filter(javaType -> !representedRawNames.contains(rawJavaTypeName(javaType)))
                .collect(toCollection(JavaAstBuilder::javaTypeSet));
    }

    private static void putPrimitiveBackedTypeAlias(
            Map<String, PrimitiveBackedTypeInfo> result,
            String alias,
            CompiledPrimitiveBackedType type,
            boolean includeUnqualifiedAlias
    ) {
        var info = new PrimitiveBackedTypeInfo(type.cfunType(), type.backingType());
        if (includeUnqualifiedAlias || alias.contains("/") || alias.contains(".")) {
            result.putIfAbsent(alias, info);
        }
        putPrimitiveBackedTypeAliases(result, type);
    }

    private static Stream<CompiledPrimitiveBackedType> primitiveBackedTypes(CompiledType type) {
        return switch (type) {
            case CompiledPrimitiveBackedType primitiveBackedType -> Stream.of(primitiveBackedType);
            case CompiledList listType -> primitiveBackedTypes(listType.elementType());
            case CompiledSet setType -> primitiveBackedTypes(setType.elementType());
            case CompiledDict dictType -> primitiveBackedTypes(dictType.valueType());
            case CompiledTupleType tupleType -> tupleType.elementTypes().stream().flatMap(JavaAstBuilder::primitiveBackedTypes);
            case CompiledFunctionType functionType -> Stream.concat(
                    primitiveBackedTypes(functionType.argumentType()),
                    primitiveBackedTypes(functionType.returnType())
            );
            case CompiledDataType dataType -> dataType.fields().stream().flatMap(field -> primitiveBackedTypes(field.type()));
            case CompiledDataParentType parentType -> parentType.subTypes().stream()
                    .flatMap(dataType -> dataType.fields().stream())
                    .flatMap(field -> primitiveBackedTypes(field.type()));
            default -> Stream.empty();
        };
    }

    private static void putPrimitiveBackedTypeAliases(
            Map<String, PrimitiveBackedTypeInfo> result,
            PrimitiveBackedTypeInfo info,
            CompiledPrimitiveBackedType type,
            CompiledModule module
    ) {
        if (type.name().contains("/") || type.name().contains(".")) {
            result.putIfAbsent(type.name(), info);
        }
        result.putIfAbsent(type.cfunType(), info);
        result.putIfAbsent(withoutLeadingSlash(type.cfunType()), info);
        result.putIfAbsent(module.name() + "." + type.name(), info);
    }

    private static void putPrimitiveBackedTypeAliases(
            Map<String, PrimitiveBackedTypeInfo> result,
            CompiledPrimitiveBackedType type
    ) {
        var info = new PrimitiveBackedTypeInfo(type.cfunType(), type.backingType());
        if (type.name().contains("/") || type.name().contains(".")) {
            result.putIfAbsent(type.name(), info);
        }
        result.putIfAbsent(type.cfunType(), info);
        result.putIfAbsent(withoutLeadingSlash(type.cfunType()), info);
    }

    private static void putUniqueSimplePrimitiveBackedTypeAliases(Map<String, PrimitiveBackedTypeInfo> result) {
        var bySimpleName = result.entrySet().stream()
                .collect(groupingBy(
                        entry -> simpleTypeName(entry.getKey()),
                        LinkedHashMap::new,
                        mapping(Map.Entry::getValue, toList())
                ));
        bySimpleName.forEach((simpleName, infos) -> {
            var distinctInfos = infos.stream().distinct().toList();
            if (distinctInfos.size() == 1) {
                result.putIfAbsent(simpleName, distinctInfos.getFirst());
            }
        });
    }

    private static String withoutLeadingSlash(String value) {
        return value.startsWith("/") ? value.substring(1) : value;
    }

    private Map<String, String> enumValueOwnerOverrides(ModuleTypeIndex typeIndex) {
        var ownersByValue = new LinkedHashMap<String, LinkedHashSet<String>>();
        for (var parentType : typeIndex.dataParentTypes()) {
            if (!parentType.enumType()) {
                continue;
            }
            for (var subType : parentType.subTypes()) {
                ownersByValue.computeIfAbsent(subType.name(), ignored -> new LinkedHashSet<>()).add(parentType.name());
            }
        }
        var overrides = new LinkedHashMap<String, String>();
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
        return Map.copyOf(overrides);
    }

    private String enumValueOverride(String ownerName, String valueName) {
        return ownerName + "#" + valueName;
    }

    private SortedMap<String, List<CompiledFunction>> indexFunctionsByOwnerPrefix(Set<CompiledFunction> functions) {
        var indexed = new TreeMap<String, List<CompiledFunction>>();
        for (var function : functions) {
            if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
                continue;
            }
            var ownerSeparator = function.name().lastIndexOf("__");
            if (ownerSeparator < METHOD_DECL_PREFIX.length()) {
                continue;
            }
            var ownerPrefix = function.name().substring(0, ownerSeparator + 2);
            indexed.computeIfAbsent(ownerPrefix, ignored -> new ArrayList<>()).add(function);
        }
        indexed.replaceAll((ignored, ownerFunctions) -> List.copyOf(ownerFunctions));
        return Collections.unmodifiableSortedMap(indexed);
    }

    private SortedMap<CompiledDataType, SortedSet<JavaInterface>> findSubClassToInterface(SortedSet<CompiledDataParentType> dataParentTypes, List<JavaInterface> interfaces) {
        var javaInterfaceByJavaName = interfaces.stream()
                .collect(toMap(
                        jInterface -> jInterface.name().name(),
                        identity()));

        record ClassToInterface(CompiledDataType data, CompiledDataParentType parent) {
        }
        record ClassToJavaInterface(CompiledDataType data, JavaInterface parent) {
        }

        return dataParentTypes.stream()
                .flatMap(parent -> parent.subTypes().stream().map(data -> new ClassToInterface(data, parent)))
                .map(pair -> new ClassToJavaInterface(
                        pair.data,
                        javaInterfaceByJavaName.get(buildClassName(pair.parent.name()).name())))
                .filter(pair -> pair.parent != null)
                .collect(groupingBy(
                        ClassToJavaInterface::data,
                        () -> new TreeMap<>(COMPILED_DATA_TYPE_COMPARATOR),
                        mapping(ClassToJavaInterface::parent, toCollection(JavaAstBuilder::javaInterfaceSet))
                ));
    }

    private static JavaType buildClassName(String name) {
        return new JavaType(normalizeJavaTypeIdentifier(name));
    }

    private List<JavaMethod> buildStaticMethods(
            Set<CompiledFunction> functions,
            List<String> qualifiedJavaClassNames,
            Set<String> nativeTypeNames
    ) {
        return sortedUniqueList(functions.stream()
                .filter(function -> !isConstFunction(function))
                .filter(function -> !function.name().startsWith(METHOD_DECL_PREFIX)
                                    || isStaticNativeTypeMethod(function, nativeTypeNames))
                .map(function -> buildStaticMethod(function, qualifiedJavaClassNames)), JAVA_METHOD_COMPARATOR);
    }

    private boolean isStaticNativeTypeMethod(CompiledFunction function, Set<String> nativeTypeNames) {
        return methodOwnerType(function.name())
                .filter(nativeTypeNames::contains)
                .filter(ignored -> !isNativeExpression(function))
                .isPresent();
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

    private boolean isNativeExpression(CompiledFunction function) {
        return function.expression() instanceof dev.capylang.compiler.expression.CompiledNothingValue nothingValue
               && (nothingValue.message().contains("`<native>`")
                   || nothingValue.message().contains("native expression in function"));
    }

    private List<JavaConst> buildStaticConsts(Set<CompiledFunction> functions) {
        return sortedUniqueList(functions.stream()
                .filter(function -> !function.name().startsWith(METHOD_DECL_PREFIX))
                .filter(this::isConstFunction)
                .map(this::buildStaticConst), JAVA_CONST_COMPARATOR);
    }

    private boolean isConstFunction(CompiledFunction function) {
        if (!function.parameters().isEmpty()) {
            return false;
        }
        return isTopLevelConstName(function.name()) || function.name().contains("__local_const_");
    }

    private boolean isTopLevelConstName(String name) {
        return CONST_NAME_PATTERN.matcher(name).matches();
    }

    private JavaConst buildStaticConst(CompiledFunction function) {
        return new JavaConst(
                emittedConstName(function),
                function.name().startsWith("_") || function.visibility() == Visibility.PRIVATE,
                buildJavaType(function.returnType()),
                function.expression(),
                function.comments()
        );
    }

    private String emittedConstName(CompiledFunction function) {
        if (isTopLevelConstName(function.name())) {
            return function.name();
        }
        return emittedFunctionName(function);
    }

    private JavaMethod buildStaticMethod(CompiledFunction function, List<String> qualifiedJavaClassNames) {
        var methodTypeParameters = methodTypeParameters(function, Set.of());
        var expression = specializeReturnNewData(function.expression(), function.returnType());
        return new JavaMethod(
                emittedFunctionName(function),
                function.name(),
                isPrivateFunction(function),
                isExecutableProgramMain(function),
                function.tailRecursive(),
                selfCallNames(function, qualifiedJavaClassNames),
                methodTypeParameters,
                buildJavaReturnType(function),
                buildJavaFunctionParameters(function.parameters()),
                function.returnType(),
                function.parameters().stream()
                        .map(CompiledFunctionParameter::type)
                        .map(type -> (Object) type)
                        .toList(),
                expression,
                function.comments()
        );
    }

    private boolean isExecutableProgramMain(CompiledFunction function) {
        if (!function.programMain()) {
            return false;
        }
        if (function.parameters().size() != 1
            || !(function.parameters().getFirst().type() instanceof CompiledList listType)
            || !sameType(listType.elementType(), CompiledIrModule.STRING)) {
            return false;
        }
        if (!(function.returnType() instanceof GenericDataType returnType)) {
            return false;
        }
        return isCanonicalEffectType(returnType)
               && typeParameters(returnType).size() == 1
               && isCanonicalProgramDescriptor(typeParameters(returnType).getFirst());
    }

    private boolean isCanonicalEffectType(GenericDataType type) {
        return "Effect".equals(type.name())
               || normalizeQualifiedTypeName(type.name()).equals("/capy/lang/Effect");
    }

    private boolean isCanonicalProgramDescriptor(String descriptor) {
        var rawType = descriptor;
        var genericStart = rawType.indexOf('[');
        if (genericStart >= 0) {
            rawType = rawType.substring(0, genericStart);
        }
        return "Program".equals(rawType)
               || normalizeQualifiedTypeName(rawType).equals("/capy/lang/Program");
    }

    private List<String> typeParameters(GenericDataType type) {
        return switch (type) {
            case CompiledDataType dataType -> dataType.typeParameters();
            case CompiledDataParentType parentType -> parentType.typeParameters();
            case CompiledObjectType ignored -> List.of();
            case CompiledPrimitiveBackedType ignored -> List.of();
        };
    }

    private boolean isPrivateFunction(CompiledFunction function) {
        if (function.name().startsWith(PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX)) {
            return function.visibility() == Visibility.PRIVATE;
        }
        var userVisibleName = baseMethodName(function.name());
        return userVisibleName.startsWith("_") || function.visibility() == Visibility.PRIVATE;
    }

    private dev.capylang.compiler.expression.CompiledExpression specializeReturnNewData(
            dev.capylang.compiler.expression.CompiledExpression expression,
            CompiledType returnType
    ) {
        if (!(returnType instanceof CompiledDataType expectedData)
            || !(expression instanceof dev.capylang.compiler.expression.CompiledNewData newData)
            || !(newData.type() instanceof CompiledDataType actualData)
            || !expectedData.name().equals(actualData.name())
            || expectedData.typeParameters().equals(actualData.typeParameters())) {
            return expression;
        }
        var byName = new java.util.LinkedHashMap<String, dev.capylang.compiler.expression.CompiledExpression>();
        for (var assignment : newData.assignments()) {
            byName.put(assignment.name(), assignment.value());
        }
        return new dev.capylang.compiler.expression.CompiledNewData(
                expectedData,
                expectedData.fields().stream()
                        .map(field -> new dev.capylang.compiler.expression.CompiledNewDataFieldAssignment(
                                field.name(),
                                byName.get(field.name())
                        ))
                        .toList()
        );
    }

    private String buildMethodName(String name) {
        return normalizeJavaMethodIdentifier(name);
    }

    private String emittedFunctionName(CompiledFunction function) {
        return functionNameOverrides.getOrDefault(signatureKey(function.name(), function.parameters().stream().map(CompiledFunctionParameter::type).toList()), buildMethodName(baseMethodName(function.name())));
    }

    private List<String> selfCallNames(CompiledFunction function, List<String> qualifiedJavaClassNames) {
        return Stream.concat(
                        Stream.of(function.name()),
                        qualifiedJavaClassNames.stream().flatMap(className -> Stream.of(
                                className + "." + function.name(),
                                className + "." + emittedFunctionName(function)
                        ))
                )
                .distinct()
                .toList();
    }

    private String baseMethodName(String name) {
        if (!name.startsWith(METHOD_DECL_PREFIX)) {
            return name;
        }
        var ownerSeparator = name.lastIndexOf("__");
        return ownerSeparator >= 0 ? name.substring(ownerSeparator + 2) : name;
    }

    private static String signatureKey(String name, List<CompiledType> parameterTypes) {
        return name + "|" + parameterTypes.stream().map(type -> String.valueOf(type)).collect(joining(","));
    }

    private String normalizeJavaMethodIdentifier(String rawName) {
        var leadingUnderscores = countLeadingUnderscores(rawName);
        var suffix = rawName.substring(leadingUnderscores);
        var normalized = normalizeJavaIdentifier(suffix, false);
        if (leadingUnderscores == 0) {
            return normalized;
        }
        return "_".repeat(leadingUnderscores) + normalized;
    }

    private int countLeadingUnderscores(String value) {
        var count = 0;
        while (count < value.length() && value.charAt(count) == '_') {
            count++;
        }
        return count;
    }

    private String buildJavaStaticImportMember(String memberName) {
        if ("*".equals(memberName)) {
            return memberName;
        }
        if (isUpperSnakeConstName(memberName)) {
            return memberName;
        }
        if (isTypeLikeIdentifier(memberName)) {
            return buildClassName(memberName).toString();
        }
        return buildMethodName(memberName);
    }

    private String buildJavaStaticImport(StaticImport staticImport) {
        if (staticImport.enumValue()) {
            if ("*".equals(staticImport.memberName())) {
                return normalizeRawTypeReference(staticImport.className()) + ".*";
            }
            return normalizeRawTypeReference(staticImport.className()) + "." + enumValuePatternName(staticImport.memberName());
        }
        if ("*".equals(staticImport.memberName())) {
            return normalizeJavaClassReference(staticImport.className()) + ".*";
        }
        var enumValueOwner = enumValueOwnerOverrides.get(staticImport.memberName());
        if (enumValueOwner != null) {
            var override = parseEnumValueOverride(enumValueOwner, staticImport.memberName());
            return normalizeRawTypeReference(override.ownerName()) + "." + enumValuePatternName(override.valueName());
        }
        return normalizeJavaClassReference(staticImport.className()) + "." + buildJavaStaticImportMember(staticImport.memberName());
    }

    private EnumValueOverride parseEnumValueOverride(String rawOverride, String fallbackValueName) {
        var separator = rawOverride.lastIndexOf('#');
        if (separator < 0) {
            return new EnumValueOverride(rawOverride, fallbackValueName);
        }
        return new EnumValueOverride(rawOverride.substring(0, separator), rawOverride.substring(separator + 1));
    }

    private record EnumValueOverride(String ownerName, String valueName) {
    }

    private String enumValuePatternName(String constructorName) {
        var normalized = constructorName.replace('\\', '/');
        var separator = Math.max(normalized.lastIndexOf('/'), normalized.lastIndexOf('.'));
        return separator >= 0 ? normalized.substring(separator + 1) : normalized;
    }

    private boolean isUpperSnakeConstName(String name) {
        if (!CONST_NAME_PATTERN.matcher(name).matches()) {
            return false;
        }
        var index = countLeadingUnderscores(name);
        return name.indexOf('_', index) >= 0;
    }

    private boolean isTypeLikeIdentifier(String name) {
        if (name == null || name.isEmpty()) {
            return false;
        }
        var index = countLeadingUnderscores(name);
        if (index >= name.length()) {
            return false;
        }
        return Character.isUpperCase(name.charAt(index));
    }

    private String buildJavaPackageName(String rawPath) {
        var normalized = rawPath.replace('\\', '/');
        return Stream.of(normalized.split("/"))
                .filter(part -> !part.isBlank())
                .map(this::normalizeJavaPackageSegment)
                .collect(joining("."));
    }

    private String normalizeJavaClassReference(String classReference) {
        return normalizedJavaClassReferenceCache.computeIfAbsent(classReference, this::computeNormalizedJavaClassReference);
    }

    private String computeNormalizedJavaClassReference(String classReference) {
        var parts = Stream.of(classReference.split("\\."))
                .filter(part -> !part.isBlank())
                .toList();
        if (parts.isEmpty()) {
            return classReference;
        }
        var normalized = new StringBuilder();
        for (int i = 0; i < parts.size(); i++) {
            if (i > 0) {
                normalized.append('.');
            }
            if (i == parts.size() - 1) {
                normalized.append(buildClassName(parts.get(i)));
            } else {
                normalized.append(normalizeJavaPackageSegment(parts.get(i)));
            }
        }
        return normalized.toString();
    }

    private String normalizeJavaPackageSegment(String rawSegment) {
        var normalized = normalizeJavaIdentifier(rawSegment, false);
        if (!Character.isJavaIdentifierStart(normalized.charAt(0))) {
            normalized = "p" + normalized;
        }
        if (JAVA_KEYWORDS.contains(normalized)) {
            return normalized + "_";
        }
        return normalized;
    }

    private JavaType buildJavaType(CompiledType type) {
        return switch (type) {
            case CompiledPrimitiveBackedType primitiveBackedType -> buildPrimitiveBackedType(primitiveBackedType, false);
            case GenericDataType genericDataType -> buildGenericDataType(genericDataType);
            case PrimitiveLinkedType primitiveLinkedType -> buildPrimitiveLinkedType(primitiveLinkedType);
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType);
            case CompiledTupleType linkedTupleType -> new JavaType("java.util.List<?>");
            case CompiledFunctionType functionType -> sameType(functionType.argumentType(), CompiledIrModule.NOTHING)
                    ? new JavaType("java.util.function.Supplier<" + buildJavaBoxedType(functionType.returnType()) + ">")
                    : new JavaType(
                            "java.util.function.Function<"
                            + buildJavaBoxedType(functionType.argumentType())
                            + ", "
                            + buildJavaBoxedType(functionType.returnType())
                            + ">"
                    );
            case CompiledGenericTypeParameter linkedGenericTypeParameter -> new JavaType(linkedGenericTypeParameter.name());
        };
    }

    private JavaType buildJavaReturnType(CompiledFunction function) {
        if (function.returnType() instanceof GenericDataType genericDataType
            && ("Option".equals(genericDataType.name()) || isOptionTypeName(genericDataType.name()))) {
            var elementDescriptor = optionElementTypeDescriptor(genericDataType);
            if (elementDescriptor.isPresent()) {
                return new JavaType("java.util.Optional<" + mapTypeParameterDescriptor(elementDescriptor.orElseThrow()) + ">");
            }
            var elementType = optionElementType(function.returnType())
                    .orElseGet(() -> inferOptionElementType(function.expression()));
            return new JavaType("java.util.Optional<" + buildJavaBoxedType(elementType) + ">");
        }
        return buildJavaType(function.returnType());
    }

    private Optional<String> optionElementTypeDescriptor(GenericDataType type) {
        var typeParameters = switch (type) {
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            case CompiledPrimitiveBackedType ignored -> List.<String>of();
            case CompiledObjectType ignored -> List.<String>of();
        };
        return typeParameters.stream().findFirst();
    }

    private Optional<CompiledType> optionElementType(CompiledType type) {
        if (!isOptionType(type) || !(type instanceof GenericDataType genericDataType)) {
            return Optional.empty();
        }
        return genericDataType.fields().stream()
                .findFirst()
                .map(CompiledField::type);
    }

    private CompiledType inferOptionElementType(dev.capylang.compiler.expression.CompiledExpression expression) {
        return switch (expression) {
            case dev.capylang.compiler.expression.CompiledPipeExpression pipeExpression ->
                    isOptionType(pipeExpression.type()) ? pipeExpression.mapper().type() : CompiledIrModule.ANY;
            case dev.capylang.compiler.expression.CompiledPipeFilterOutExpression filterOutExpression -> {
                if (!isOptionType(filterOutExpression.type())) {
                    yield CompiledIrModule.ANY;
                }
                var inferredSource = inferOptionElementType(filterOutExpression.source());
                if (!(inferredSource instanceof PrimitiveLinkedType primitiveLinkedType)
                    || !sameType(primitiveLinkedType, CompiledIrModule.ANY)) {
                    yield inferredSource;
                }
                yield filterOutExpression.source().type();
            }
            case dev.capylang.compiler.expression.CompiledNewData newDataExpression -> {
                if (!(newDataExpression.type() instanceof GenericDataType genericDataType)
                    || !isOptionSomeTypeName(genericDataType.name())) {
                    yield CompiledIrModule.ANY;
                }
                yield newDataExpression.assignments().stream()
                        .filter(assignment -> "value".equals(assignment.name()))
                        .map(dev.capylang.compiler.expression.CompiledNewDataFieldAssignment::value)
                        .map(dev.capylang.compiler.expression.CompiledExpression::type)
                        .findFirst()
                        .orElse(CompiledIrModule.ANY);
            }
            case dev.capylang.compiler.expression.CompiledLetExpression letExpression ->
                    inferOptionElementType(letExpression.rest());
            case dev.capylang.compiler.expression.CompiledIfExpression ifExpression ->
                    dev.capylang.compiler.expression.CapybaraTypeFinder.findHigherType(
                            inferOptionElementType(ifExpression.thenBranch()),
                            inferOptionElementType(ifExpression.elseBranch()));
            case dev.capylang.compiler.expression.CompiledIndexExpression indexExpression ->
                    indexExpression.elementType();
            case dev.capylang.compiler.expression.CompiledMatchExpression matchExpression ->
                    matchExpression.cases().stream()
                            .map(dev.capylang.compiler.expression.CompiledMatchCase::expression)
                            .map(this::inferOptionElementType)
                            .reduce(dev.capylang.compiler.expression.CapybaraTypeFinder::findHigherType)
                            .orElse(CompiledIrModule.ANY);
            default -> CompiledIrModule.ANY;
        };
    }

    private boolean isOptionType(CompiledType type) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return false;
        }
        if ("Option".equals(genericDataType.name())) {
            return true;
        }
        var normalized = normalizeQualifiedTypeName(genericDataType.name());
        return normalized.endsWith("/Option")
               || normalized.endsWith("/Option.Option")
               || isOptionTypeName(genericDataType.name());
    }

    private JavaType buildGenericDataType(GenericDataType type) {
        var rawTypeName = type.name();
        var genericStart = rawTypeName.indexOf('[');
        if (genericStart > 0) {
            rawTypeName = rawTypeName.substring(0, genericStart);
        }
        if (type instanceof CompiledDataType dataType && dataType.singleton()) {
            var enumOwner = qualifiedEnumValueOwner(rawTypeName);
            if (enumOwner.isPresent()) {
                return buildGenericDataType(new CompiledDataParentType(enumOwner.orElseThrow(), List.of(), List.of(), List.of(), true));
            }
        }
        if ("Option".equals(rawTypeName) || isOptionTypeName(rawTypeName)) {
            return new JavaType("java.util.Optional");
        }
        if ("Program".equals(rawTypeName) || isProgramTypeName(rawTypeName)) {
            return withTypeParametersIfGeneric(type, "capy.lang.Program");
        }
        if (shouldUseStandardSeqTypeName(rawTypeName)) {
            return withTypeParametersIfGeneric(type, "capy.lang.Seq");
        }
        if ("Result".equals(rawTypeName) || isResultTypeName(rawTypeName)) {
            return withTypeParametersIfGeneric(type, "capy.lang.Result");
        }
        if ("Effect".equals(rawTypeName) || isEffectTypeName(rawTypeName)) {
            return withTypeParametersIfGeneric(type, "capy.lang.Effect");
        }
        if (rawTypeName.startsWith("/") && !rawTypeName.contains(".")) {
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < rawTypeName.length() - 1) {
                var packageName = buildJavaPackageName(rawTypeName.substring(1, slashIndex));
                var className = buildClassName(rawTypeName.substring(slashIndex + 1));
                return withTypeParametersIfGeneric(type, packageName + "." + className);
            }
        }
        if (rawTypeName.contains("/") && rawTypeName.contains(".")) {
            var dotIndex = rawTypeName.lastIndexOf('.');
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = rawTypeName.startsWith("/") ? 1 : 0;
                var modulePath = buildJavaPackageName(rawTypeName.substring(startIdx, slashIndex));
                var moduleName = buildClassName(rawTypeName.substring(slashIndex + 1, dotIndex));
                var nestedType = buildClassName(rawTypeName.substring(dotIndex + 1));
                if (moduleName.equals(nestedType)) {
                    return withTypeParametersIfGeneric(type, modulePath + "." + moduleName);
                }
                return withTypeParametersIfGeneric(type, modulePath + "." + moduleName + "." + nestedType);
            }
        }
        if (rawTypeName.contains(".")) {
            var qualifiedName = Stream.of(rawTypeName.split("\\."))
                    .map(JavaAstBuilder::normalizeJavaTypeIdentifier)
                    .collect(joining("."));
            return withTypeParametersIfGeneric(type, qualifiedName);
        }
        return withTypeParametersIfGeneric(type, buildClassName(rawTypeName).toString());
    }

    private Optional<String> qualifiedEnumValueOwner(String typeName) {
        var normalized = typeName.replace('\\', '/');
        var valueSeparator = normalized.lastIndexOf('.');
        if (valueSeparator <= 0 || valueSeparator == normalized.length() - 1) {
            return Optional.empty();
        }
        var valueName = normalized.substring(valueSeparator + 1);
        if (!valueName.equals(valueName.toUpperCase(Locale.ROOT))) {
            return Optional.empty();
        }
        return Optional.of(normalized.substring(0, valueSeparator));
    }

    private JavaType withTypeParametersIfGeneric(GenericDataType type, String rawJavaTypeName) {
        var typeParameters = switch (type) {
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            case CompiledPrimitiveBackedType ignored -> List.<String>of();
            case CompiledObjectType ignored -> List.<String>of();
        };
        if (typeParameters.isEmpty()) {
            return new JavaType(rawJavaTypeName);
        }
        var mappedTypeParameters = typeParameters.stream()
                .map(this::mapTypeParameterDescriptor)
                .collect(joining(", "));
        return new JavaType(rawJavaTypeName + "<" + mappedTypeParameters + ">");
    }

    private String mapTypeParameterDescriptor(String descriptor) {
        var cacheKey = descriptor.contains("Seq")
                ? descriptor + "#localSeq=" + localTypeNames.contains("Seq")
                : descriptor;
        var cached = mappedTypeParameterDescriptorCache.get(cacheKey);
        if (cached != null) {
            return cached;
        }
        var mapped = computeTypeParameterDescriptor(descriptor);
        mappedTypeParameterDescriptorCache.put(cacheKey, mapped);
        return mapped;
    }

    private String computeTypeParameterDescriptor(String descriptor) {
        var normalized = descriptor.trim();
        var primitiveBackedType = primitiveBackedTypes.get(normalized);
        if (primitiveBackedType != null) {
            return buildPrimitiveBackedType(primitiveBackedType.cfunType(), primitiveBackedType.backingType(), true).toString();
        }
        return switch (normalized) {
            case "byte" -> "java.lang.Byte";
            case "int" -> "java.lang.Integer";
            case "long" -> "java.lang.Long";
            case "double" -> "java.lang.Double";
            case "float" -> "java.lang.Float";
            case "bool" -> "java.lang.Boolean";
            case "String" -> "java.lang.String";
            case "any", "nothing", "data" -> "java.lang.Object";
            default -> {
                if (normalized.matches("[A-Z][A-Za-z0-9_]*")) {
                    yield normalized;
                }
                if (normalized.startsWith("List[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("List[".length(), normalized.length() - 1);
                    yield "java.util.List<" + mapTypeParameterDescriptor(inner) + ">";
                }
                if (normalized.startsWith("Set[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("Set[".length(), normalized.length() - 1);
                    yield "java.util.Set<" + mapTypeParameterDescriptor(inner) + ">";
                }
                if (normalized.startsWith("Dict[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("Dict[".length(), normalized.length() - 1);
                    yield "java.util.Map<java.lang.String, " + mapTypeParameterDescriptor(inner) + ">";
                }
                if (normalized.startsWith("Tuple[") && normalized.endsWith("]")) {
                    yield "java.util.List<?>";
                }
                var genericStart = normalized.indexOf('[');
                if (genericStart > 0 && normalized.endsWith("]")) {
                    var rawType = normalized.substring(0, genericStart).trim();
                    var argsDescriptor = normalized.substring(genericStart + 1, normalized.length() - 1);
                    var javaArgs = splitTopLevelDescriptorParts(argsDescriptor).stream()
                            .map(this::mapTypeParameterDescriptor)
                            .collect(joining(", "));
                    yield normalizeRawTypeReference(rawType) + "<" + javaArgs + ">";
                }
                yield normalizeRawTypeReference(normalized);
            }
        };
    }

    private List<String> splitTopLevelDescriptorParts(String descriptors) {
        return topLevelDescriptorSplitCache.computeIfAbsent(descriptors, this::computeSplitTopLevelDescriptorParts);
    }

    private List<String> computeSplitTopLevelDescriptorParts(String descriptors) {
        var result = new ArrayList<String>();
        var depth = 0;
        var start = 0;
        for (int i = 0; i < descriptors.length(); i++) {
            var ch = descriptors.charAt(i);
            if (ch == '[') {
                depth++;
            } else if (ch == ']') {
                depth--;
            } else if (ch == ',' && depth == 0) {
                result.add(descriptors.substring(start, i).trim());
                start = i + 1;
            }
        }
        var last = descriptors.substring(start).trim();
        if (!last.isEmpty()) {
            result.add(last);
        }
        return List.copyOf(result);
    }

    private String normalizeRawTypeReference(String rawTypeName) {
        var cacheKey = "Seq".equals(rawTypeName)
                ? rawTypeName + "#local=" + localTypeNames.contains("Seq")
                : rawTypeName;
        return normalizedRawTypeReferenceCache.computeIfAbsent(cacheKey, ignored -> computeNormalizedRawTypeReference(rawTypeName));
    }

    private String computeNormalizedRawTypeReference(String rawTypeName) {
        var primitiveBackedType = primitiveBackedTypes.get(rawTypeName);
        if (primitiveBackedType != null) {
            return buildPrimitiveBackedType(primitiveBackedType.cfunType(), primitiveBackedType.backingType(), true).toString();
        }
        if ("Option".equals(rawTypeName) || isOptionTypeName(rawTypeName) || isOptionSomeTypeName(rawTypeName)) {
            return "java.util.Optional";
        }
        if ("None".equals(rawTypeName) || normalizeQualifiedTypeName(rawTypeName).endsWith("/Option.None")) {
            return "java.util.Optional";
        }
        if ("Program".equals(rawTypeName) || isProgramTypeName(rawTypeName)) {
            return "capy.lang.Program";
        }
        if (shouldUseStandardSeqTypeName(rawTypeName)) {
            return "capy.lang.Seq";
        }
        if ("Result".equals(rawTypeName) || isResultTypeName(rawTypeName)) {
            return "capy.lang.Result";
        }
        if ("Effect".equals(rawTypeName) || isEffectTypeName(rawTypeName)) {
            return "capy.lang.Effect";
        }
        if (rawTypeName.startsWith("/") && !rawTypeName.contains(".")) {
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < rawTypeName.length() - 1) {
                var packageName = buildJavaPackageName(rawTypeName.substring(1, slashIndex));
                var className = buildClassName(rawTypeName.substring(slashIndex + 1));
                return packageName + "." + className;
            }
        }
        if (rawTypeName.contains("/") && rawTypeName.contains(".")) {
            var dotIndex = rawTypeName.lastIndexOf('.');
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = rawTypeName.startsWith("/") ? 1 : 0;
                var modulePath = buildJavaPackageName(rawTypeName.substring(startIdx, slashIndex));
                var moduleName = buildClassName(rawTypeName.substring(slashIndex + 1, dotIndex));
                var nestedType = buildClassName(rawTypeName.substring(dotIndex + 1));
                if (moduleName.equals(nestedType)) {
                    return modulePath + "." + moduleName;
                }
                return modulePath + "." + moduleName + "." + nestedType;
            }
        }
        if (rawTypeName.contains(".")) {
            return Stream.of(rawTypeName.split("\\."))
                    .map(JavaAstBuilder::normalizeJavaTypeIdentifier)
                    .collect(joining("."));
        }
        return buildClassName(rawTypeName).toString();
    }

    private JavaType buildPrimitiveLinkedType(PrimitiveLinkedType type) {
        return switch (type.name()) {
            case "BYTE" -> new JavaType("byte");
            case "INT" -> new JavaType("int");
            case "LONG" -> new JavaType("long");
            case "DOUBLE" -> new JavaType("double");
            case "STRING" -> new JavaType("java.lang.String");
            case "BOOL" -> new JavaType("boolean");
            case "FLOAT" -> new JavaType("float");
            case "NOTHING" -> new JavaType("java.lang.Object");
            case "ANY" -> new JavaType("java.lang.Object");
            case "DATA" -> new JavaType("java.lang.Object");
            case "ENUM" -> new JavaType("java.lang.Enum<?>");
            default -> new JavaType("java.lang.Object");
        };
    }

    private JavaType buildPrimitiveBackedType(String cfunType, PrimitiveLinkedType backingType, boolean boxed) {
        var erasedType = boxed ? buildJavaBoxedType(backingType) : buildPrimitiveLinkedType(backingType).toString();
        var annotation = "@dev.capylang.PrimitiveType(cfunType = " + javaString(cfunType) + ")";
        if (boxed) {
            var dotIndex = erasedType.lastIndexOf('.');
            if (dotIndex > 0 && dotIndex < erasedType.length() - 1) {
                return new JavaType(erasedType.substring(0, dotIndex + 1) + annotation + " " + erasedType.substring(dotIndex + 1));
            }
        }
        return new JavaType(annotation + " " + erasedType);
    }

    private JavaType buildPrimitiveBackedType(CompiledPrimitiveBackedType primitiveBackedType, boolean boxed) {
        var info = primitiveBackedTypes.get(primitiveBackedType.name());
        if (info == null) {
            info = new PrimitiveBackedTypeInfo(primitiveBackedType.cfunType(), primitiveBackedType.backingType());
        }
        return buildPrimitiveBackedType(info.cfunType(), info.backingType(), boxed);
    }

    private JavaType buildCollectionLinkedType(CollectionLinkedType type) {
        return switch (type) {
            case CompiledList linkedList -> new JavaType("java.util.List<" + buildJavaBoxedType(linkedList.elementType()) + ">");
            case CompiledDict linkedDict -> new JavaType("java.util.Map<java.lang.String, " + buildJavaBoxedType(linkedDict.valueType()) + ">");
            case CompiledSet linkedSet -> new JavaType("java.util.Set<" + buildJavaBoxedType(linkedSet.elementType()) + ">");
        };
    }

    private String buildJavaBoxedType(CompiledType type) {
        return switch (type) {
            case CompiledPrimitiveBackedType primitiveBackedType -> buildPrimitiveBackedType(primitiveBackedType, true).toString();
            case PrimitiveLinkedType primitiveLinkedType -> switch (primitiveLinkedType.name()) {
                case "BYTE" -> "java.lang.Byte";
                case "INT" -> "java.lang.Integer";
                case "LONG" -> "java.lang.Long";
                case "DOUBLE" -> "java.lang.Double";
                case "STRING" -> "java.lang.String";
                case "BOOL" -> "java.lang.Boolean";
                case "FLOAT" -> "java.lang.Float";
                case "NOTHING" -> "java.lang.Object";
                case "ANY" -> "java.lang.Object";
                case "DATA" -> "java.lang.Object";
                case "ENUM" -> "java.lang.Enum<?>";
                default -> "java.lang.Object";
            };
            case GenericDataType genericDataType -> buildGenericDataType(genericDataType).toString();
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType).toString();
            case CompiledTupleType linkedTupleType -> "java.util.List<?>";
            case CompiledFunctionType functionType -> sameType(functionType.argumentType(), CompiledIrModule.NOTHING)
                    ? "java.util.function.Supplier<" + buildJavaBoxedType(functionType.returnType()) + ">"
                    : "java.util.function.Function<"
                      + buildJavaBoxedType(functionType.argumentType())
                      + ", "
                      + buildJavaBoxedType(functionType.returnType())
                      + ">";
            case CompiledGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private boolean isOptionTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return normalized.equals("/cap/lang/Option")
               || normalized.equals("/cap/lang/Option.Option")
               || normalized.equals("/capy/lang/Option")
               || normalized.equals("/capy/lang/Option.Option");
    }

    private boolean isResultTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return normalized.equals("/cap/lang/Result")
               || normalized.equals("/cap/lang/Result.Result")
               || normalized.equals("/capy/lang/Result")
               || normalized.equals("/capy/lang/Result.Result")
               || normalized.endsWith("/Result")
               || normalized.endsWith("/Result.Result");
    }

    private boolean isSeqTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return normalized.equals("/cap/lang/Seq")
               || normalized.equals("/capy/lang/Seq")
               || normalized.equals("/cap/lang/Seq.Seq")
               || normalized.equals("/capy/lang/Seq.Seq")
               || normalized.endsWith("/Seq")
               || normalized.endsWith("/Seq.Seq");
    }

    private boolean shouldUseStandardSeqTypeName(String name) {
        return "Seq".equals(name) ? !localTypeNames.contains("Seq") : isSeqTypeName(name);
    }

    private boolean isEffectTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return normalized.equals("/cap/lang/Effect")
               || normalized.equals("/cap/lang/Effect.Effect")
               || normalized.equals("/capy/lang/Effect")
               || normalized.equals("/capy/lang/Effect.Effect")
               || normalized.endsWith("/Effect")
               || normalized.endsWith("/Effect.Effect");
    }

    private boolean isResultErrorTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return "Error".equals(name)
               || normalized.equals("/cap/lang/Result.Error")
               || normalized.equals("/capy/lang/Result.Error")
               || normalized.endsWith("/Result.Error");
    }

    private boolean isResultErrorDataType(CompiledDataType type) {
        if (!isResultErrorTypeName(type.name())) {
            return false;
        }
        return type.fields().size() == 1
               && "message".equals(type.fields().getFirst().name())
               && sameType(type.fields().getFirst().type(), CompiledIrModule.STRING);
    }

    private boolean isOptionSomeTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return "Some".equals(name)
               || normalized.equals("/cap/lang/Option.Some")
               || normalized.equals("/capy/lang/Option.Some")
               || normalized.endsWith("/Option.Some")
               || normalized.endsWith(".Some");
    }

    private boolean isProgramTypeName(String name) {
        var normalized = normalizeQualifiedTypeName(name);
        return normalized.equals("/cap/lang/Program")
               || normalized.equals("/capy/lang/Program")
               || normalized.equals("/cap/lang/Program.Program")
               || normalized.equals("/capy/lang/Program.Program")
               || normalized.endsWith("/Program")
               || normalized.endsWith("/Program.Program");
    }

    private String normalizeQualifiedTypeName(String name) {
        var normalized = name.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private static String simpleTypeName(String typeName) {
        var normalized = baseTypeName(typeName).replace('\\', '/');
        var slash = normalized.lastIndexOf('/');
        var dot = normalized.lastIndexOf('.');
        var index = Math.max(slash, dot);
        return index >= 0 ? normalized.substring(index + 1) : normalized;
    }

    private static String baseTypeName(String typeName) {
        var genericStart = typeName.indexOf('[');
        return genericStart > 0 ? typeName.substring(0, genericStart) : typeName;
    }

    private List<JavaFunctionParameter> buildJavaFunctionParameters(List<CompiledFunctionParameter> parameters) {
        return parameters.stream().map(this::buildJavaFunctionParameter).toList();
    }

    private JavaFunctionParameter buildJavaFunctionParameter(CompiledFunctionParameter parameter) {
        return new JavaFunctionParameter(
                buildJavaType(parameter.type()),
                parameter.name(),
                buildJavaFunctionParameterName(parameter.name())
        );
    }

    private String buildJavaFunctionParameterName(String name) {
        return normalizeJavaVariableName(name);
    }

    private String normalizeJavaVariableName(String name) {
        if ("_".equals(name)) {
            return "__unused";
        }
        if (name.isEmpty()) {
            return "value";
        }
        var normalized = new StringBuilder(name.length());
        for (int i = 0; i < name.length(); i++) {
            var ch = name.charAt(i);
            normalized.append(Character.isJavaIdentifierPart(ch) ? ch : '_');
        }
        var identifier = normalized.toString();
        if (!Character.isJavaIdentifierStart(identifier.charAt(0))) {
            identifier = "v" + identifier;
        }
        if (JAVA_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
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

    private static String normalizeJavaIdentifier(String name, boolean upperCamel) {
        var parts = Stream.of(name.split("[^A-Za-z0-9]+"))
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
        if (JAVA_KEYWORDS.contains(identifier)) {
            identifier = identifier + "_";
        }
        return identifier;
    }

    private static String normalizeJavaTypeIdentifier(String rawName) {
        var leadingUnderscores = countLeadingUnderscoresStatic(rawName);
        var suffix = rawName.substring(leadingUnderscores);
        var normalized = normalizeJavaIdentifier(suffix, true);
        if (leadingUnderscores == 0) {
            return normalized;
        }
        return "_".repeat(leadingUnderscores) + normalized;
    }

    private static int countLeadingUnderscoresStatic(String value) {
        var count = 0;
        while (count < value.length() && value.charAt(count) == '_') {
            count++;
        }
        return count;
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

    private List<JavaInterface> buildInterfaces(
            CompiledModule module,
            SortedSet<CompiledDataParentType> dataParentTypes,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix
    ) {
        return sortedUniqueList(dataParentTypes.stream()
                .filter(parentType -> !parentType.enumType())
                .map(parentType -> buildInterface(module, parentType, functionsByOwnerPrefix)), JAVA_INTERFACE_COMPARATOR);
    }

    private JavaInterface buildInterface(
            CompiledModule module,
            CompiledDataParentType type,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix
    ) {
        return new JavaInterface.JavaSealedInterface(
                buildClassName(type.name()),
                List.copyOf(globalInterfaceExtendsByName.getOrDefault(moduleTypeKey(module, type.name()), javaTypeSet())),
                type.comments(),
                buildJavaMethods(type.fields()),
                type.subTypes().stream().map(CompiledDataType::name).map(name -> buildClassName(name).toString()).toList(),
                type.typeParameters(),
                buildInterfaceMethods(type, functionsByOwnerPrefix)
        );
    }

    private List<JavaMethod> buildInterfaceMethods(CompiledDataParentType type, SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix) {
        var ownerPrefix = METHOD_DECL_PREFIX + type.name() + "__";
        return functionsByOwnerPrefix.getOrDefault(ownerPrefix, List.of()).stream()
                .map(function -> buildInterfaceMethod(function, ownerPrefix))
                .toList();
    }

    private JavaMethod buildInterfaceMethod(CompiledFunction function, String ownerPrefix) {
        var methodName = function.name().substring(ownerPrefix.length());
        var parameters = function.parameters().stream().skip(1).toList();
        var methodTypeParameters = methodTypeParameters(function, Set.copyOf(extractOwnerTypeParameters(function)));
        return new JavaMethod(
                emittedFunctionName(function),
                function.name(),
                methodName.startsWith("_") || function.visibility() == Visibility.PRIVATE,
                false,
                function.tailRecursive(),
                List.of(function.name()),
                methodTypeParameters,
                buildJavaReturnType(function),
                buildJavaFunctionParameters(parameters),
                function.returnType(),
                parameters.stream()
                        .map(CompiledFunctionParameter::type)
                        .map(type -> (Object) type)
                        .toList(),
                function.expression(),
                function.comments()
        );
    }

    private List<JavaInterfaceMethod> buildJavaMethods(List<CompiledField> fields) {
        return fields.stream().map(this::buildJavaMethod).toList();
    }

    private JavaInterfaceMethod buildJavaMethod(CompiledField field) {
        return new JavaInterfaceMethod(
                buildMethodName(field.name()),
                buildJavaType(field.type()));
    }

    private List<JavaRecord> buildRecords(
            CompiledModule module,
            SortedSet<CompiledDataType> dataTypes,
            SortedMap<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix,
            String reflectionFallbackPackagePath
    ) {
        return sortedUniqueList(dataTypes.stream()
                .filter(dt -> !dt.singleton())
                .filter(dt -> !dt.nativeType())
                .map(dt -> buildRecord(module, dt, subClassToInterface, functionsByOwnerPrefix, reflectionFallbackPackagePath)), JAVA_RECORD_COMPARATOR);
    }

    private JavaRecord buildRecord(
            CompiledModule module,
            CompiledDataType type,
            Map<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix,
            String reflectionFallbackPackagePath
    ) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? javaTypeSet()
                : javaInterface.stream().map(javaType -> implementedInterfaceType(type, javaType)).collect(toCollection(JavaAstBuilder::javaTypeSet));
        implementInterfaces.addAll(globalImplementedInterfaces(module, type.name(), implementInterfaces));
        implementInterfaces.add(CAPYBARA_DATA_VALUE);

        var interfaceFields = javaInterface == null
                ? Stream.<JavaRecordField>empty()
                : javaInterface.stream()
                        .map(JavaInterface::methods)
                        .flatMap(List::stream)
                        .map(method -> new JavaRecordField(
                                method.name(),
                                method.returnType()));
        var recordFields = isResultErrorDataType(type)
                ? Stream.of(new JavaRecordField(
                        "ex",
                        new JavaType("dev.capylang.CapybaraException")))
                : type.fields().stream()
                        .map(field -> new JavaRecordField(
                                field.name(),
                                buildJavaType(field.type())));
        var fieldsByName = new java.util.LinkedHashMap<String, JavaRecordField>();
        Stream.concat(interfaceFields, recordFields).forEach(field -> fieldsByName.put(field.name(), field));
        var fields = List.copyOf(fieldsByName.values());
        var recordTypeParameters = type.typeParameters().isEmpty() && javaInterface != null
                ? javaInterface.stream()
                        .flatMap(parent -> interfaceTypeParameters(parent).stream())
                        .distinct()
                        .toList()
                : type.typeParameters();
        var dataValueInfoPackagePath = ReflectionValueInfoJava.reflectionPackagePath(type.name(), reflectionFallbackPackagePath);
        return new JavaRecord(
                buildClassName(type.name()),
                type.name().startsWith("_") || type.visibility() == Visibility.PRIVATE,
                type.comments(),
                List.copyOf(implementInterfaces),
                fields,
                recordTypeParameters,
                ReflectionValueInfoJava.dataValueInfo(
                        type.name(),
                        reflectionFallbackPackagePath,
                        buildDataValueFields(type, dataValueInfoPackagePath),
                        type.annotations()
                ),
                List.of(),
                buildRecordMethods(type, functionsByOwnerPrefix));
    }

    private List<JavaDataValueInfoField> buildDataValueFields(CompiledDataType type, String dataValueInfoPackagePath) {
        return type.fields().stream()
                .map(field -> new JavaDataValueInfoField(
                        field.name(),
                        field.type(),
                        ReflectionValueInfoJava.reflectionTypeInfo(field.type(), dataValueInfoPackagePath),
                        dataValueFieldExpression(type, field),
                        field.annotations(),
                        ReflectionValueInfoJava.reflectionAnnotations(field.annotations())))
                .toList();
    }

    private String dataValueFieldExpression(CompiledDataType type, CompiledField field) {
        if (isResultErrorDataType(type) && "message".equals(field.name())) {
            return "(this.ex() == null ? null : this.ex().getMessage())";
        }
        return "this." + field.name() + "()";
    }

    private List<JavaMethod> buildRecordMethods(CompiledDataType type, SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix) {
        var ownerPrefix = METHOD_DECL_PREFIX + type.name() + "__";
        return sortedUniqueList(
                functionsByOwnerPrefix.getOrDefault(ownerPrefix, List.of()).stream()
                        .map(function -> buildRecordMethod(function, ownerPrefix)),
                JAVA_METHOD_COMPARATOR);
    }

    private JavaMethod buildRecordMethod(CompiledFunction function, String ownerPrefix) {
        var methodName = function.name().substring(ownerPrefix.length());
        var parameters = function.parameters().stream().skip(1).toList();
        var methodTypeParameters = methodTypeParameters(function, Set.copyOf(extractOwnerTypeParameters(function)));
        return new JavaMethod(
                emittedFunctionName(function),
                function.name(),
                methodName.startsWith("_") || function.visibility() == Visibility.PRIVATE,
                false,
                function.tailRecursive(),
                List.of(function.name()),
                methodTypeParameters,
                buildJavaReturnType(function),
                buildJavaFunctionParameters(parameters),
                function.returnType(),
                parameters.stream()
                        .map(CompiledFunctionParameter::type)
                        .map(type -> (Object) type)
                        .toList(),
                function.expression(),
                function.comments()
        );
    }

    private List<String> extractOwnerTypeParameters(CompiledFunction function) {
        if (function.parameters().isEmpty()) {
            return List.of();
        }
        var thisType = function.parameters().getFirst().type();
        return switch (thisType) {
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            default -> List.of();
        };
    }

    private List<String> methodTypeParameters(CompiledFunction function, Set<String> ownerTypeParameters) {
        var genericNames = new java.util.LinkedHashSet<String>();
        function.parameters().forEach(parameter -> collectGenericTypeParameters(parameter.type(), genericNames));
        collectGenericTypeParameters(function.returnType(), genericNames);
        genericNames.removeAll(ownerTypeParameters);
        return List.copyOf(genericNames);
    }

    private void collectGenericTypeParameters(CompiledType type, Set<String> genericNames) {
        switch (type) {
            case CompiledGenericTypeParameter genericTypeParameter -> genericNames.add(genericTypeParameter.name());
            case CompiledDataType linkedDataType ->
                    linkedDataType.typeParameters().forEach(typeName -> addGenericTypeName(typeName, genericNames));
            case CompiledDataParentType linkedDataParentType ->
                    linkedDataParentType.typeParameters().forEach(typeName -> addGenericTypeName(typeName, genericNames));
            case CompiledList linkedList ->
                    collectGenericTypeParameters(linkedList.elementType(), genericNames);
            case CompiledSet linkedSet ->
                    collectGenericTypeParameters(linkedSet.elementType(), genericNames);
            case CompiledDict linkedDict ->
                    collectGenericTypeParameters(linkedDict.valueType(), genericNames);
            case CompiledTupleType linkedTupleType ->
                    linkedTupleType.elementTypes().forEach(elementType -> collectGenericTypeParameters(elementType, genericNames));
            case CompiledFunctionType linkedFunctionType -> {
                collectGenericTypeParameters(linkedFunctionType.argumentType(), genericNames);
                collectGenericTypeParameters(linkedFunctionType.returnType(), genericNames);
            }
            default -> {
            }
        }
    }

    private void addGenericTypeName(String typeName, Set<String> genericNames) {
        if (typeName != null && typeName.matches("[A-Z]")) {
            genericNames.add(typeName);
        }
    }

    private List<String> interfaceTypeParameters(JavaInterface javaInterface) {
        return switch (javaInterface) {
            case JavaInterface.JavaSealedInterface javaSealedInterface -> javaSealedInterface.typeParameters();
            case JavaInterface.JavaNormalInterface javaNormalInterface -> List.<String>of();
        };
    }

    private JavaType implementedInterfaceType(CompiledDataType type, JavaInterface javaInterface) {
        var interfaceTypeParameters = interfaceTypeParameters(javaInterface);
        if (interfaceTypeParameters.isEmpty()) {
            return javaInterface.name();
        }
        var implementedTypeParameters = type.typeParameters().isEmpty()
                ? interfaceTypeParameters
                : type.typeParameters();
        if (implementedTypeParameters.size() != interfaceTypeParameters.size()) {
            return javaInterface.name();
        }
        return new JavaType(javaInterface.name() + "<" + String.join(", ", implementedTypeParameters) + ">");
    }

    private List<JavaEnum> buildEnums(
            CompiledModule module,
            ModuleTypeIndex typeIndex,
            SortedMap<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface,
            String reflectionFallbackPackagePath
    ) {
        var singletonEnums = typeIndex.dataTypes().stream()
                .filter(CompiledDataType::singleton)
                .filter(dt -> !typeIndex.enumValueTypeNames().contains(dt.name()))
                .map(dt -> buildSingletonEnum(module, dt, subClassToInterface, reflectionFallbackPackagePath));
        var declaredEnums = typeIndex.dataParentTypes().stream()
                .filter(CompiledDataParentType::enumType)
                .map(enumType -> buildDeclaredEnum(module, enumType, reflectionFallbackPackagePath));
        return sortedUniqueList(Stream.concat(singletonEnums, declaredEnums), JAVA_ENUM_COMPARATOR);
    }

    private JavaEnum buildSingletonEnum(
            CompiledModule module,
            CompiledDataType type,
            SortedMap<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface,
            String reflectionFallbackPackagePath
    ) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? javaTypeSet()
                : javaInterface.stream().map(JavaInterface::name).collect(toCollection(JavaAstBuilder::javaTypeSet));
        implementInterfaces.addAll(globalImplementedInterfaces(module, type.name(), implementInterfaces));
        implementInterfaces.add(CAPYBARA_DATA_VALUE);
        return new JavaEnum(
                buildClassName(type.name()),
                type.comments(),
                List.copyOf(implementInterfaces),
                List.of("INSTANCE"),
                List.of(ReflectionValueInfoJava.dataValueInfo(
                        type.name(),
                        reflectionFallbackPackagePath,
                        List.of(),
                        type.annotations()
                ))
        );
    }

    private JavaEnum buildDeclaredEnum(CompiledModule module, CompiledDataParentType enumType, String reflectionFallbackPackagePath) {
        var implementInterfaces = javaTypeSet();
        implementInterfaces.addAll(globalImplementedInterfaces(module, enumType.name(), implementInterfaces));
        implementInterfaces.add(CAPYBARA_DATA_VALUE);
        return new JavaEnum(
                buildClassName(enumType.name()),
                enumType.comments(),
                List.copyOf(implementInterfaces),
                enumType.subTypes().stream().map(CompiledDataType::name).toList(),
                enumType.subTypes().stream()
                        .map(subType -> ReflectionValueInfoJava.dataValueInfo(
                                subType.name(),
                                ReflectionValueInfoJava.reflectionPackagePath(enumType.name(), reflectionFallbackPackagePath),
                                List.of(),
                                enumType.annotations()))
                        .toList()
        );
    }

    private record ModuleTypeIndex(
            SortedSet<CompiledDataParentType> dataParentTypes,
            SortedSet<CompiledDataType> dataTypes,
            SortedSet<CompiledPrimitiveBackedType> primitiveBackedTypes,
            Set<String> enumValueTypeNames
    ) {
    }

    private record PrimitiveBackedTypeInfo(String cfunType, PrimitiveLinkedType backingType) {
    }
}
