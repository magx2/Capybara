package dev.capylang.generator.java;

import dev.capylang.generator.java.JavaInterface.JavaInterfaceMethod;
import dev.capylang.compiler.*;
import dev.capylang.compiler.CollectionLinkedType.CompiledDict;
import dev.capylang.compiler.CollectionLinkedType.CompiledList;
import dev.capylang.compiler.CollectionLinkedType.CompiledSet;

import java.util.*;
import java.util.stream.Stream;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.*;
import static dev.capylang.generator.java.JavaAnnotation.generatedAnnotation;

public class JavaAstBuilder {
    private final Map<String, String> normalizedJavaClassReferenceCache = new HashMap<>();
    private final Map<String, String> normalizedRawTypeReferenceCache = new HashMap<>();
    private final Map<String, String> mappedTypeParameterDescriptorCache = new HashMap<>();
    private final Map<String, List<String>> topLevelDescriptorSplitCache = new HashMap<>();

    @SafeVarargs
    private static <T extends Comparable<? super T>> SortedSet<T> sortedSetOf(T... values) {
        var sorted = new TreeSet<T>();
        Collections.addAll(sorted, values);
        return sorted;
    }
    private static final String METHOD_DECL_PREFIX = "__method__";
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
        var typeIndex = indexTypes(module.types());
        var functionsByOwnerPrefix = indexFunctionsByOwnerPrefix(module.functions());
        var interfaces = buildInterfaces(typeIndex.dataParentTypes().stream()
                .filter(parentType -> !parentType.enumType())
                .collect(toCollection(TreeSet::new)), functionsByOwnerPrefix);
        var subClassToInterface = findSubClassToInterface(typeIndex.dataParentTypes(), interfaces);
        return new JavaClass(
                sortedSetOf(generatedAnnotation()),
                buildClassName(module.name()),
                new JavaPackage(buildJavaPackageName(module.path())),
                module.staticImports().stream()
                        .map(staticImport -> normalizeJavaClassReference(staticImport.className()) + "." + buildJavaStaticImportMember(staticImport.memberName()))
                        .collect(toCollection(TreeSet::new)),
                buildStaticMethods(module.functions()),
                interfaces,
                buildRecords(typeIndex.dataTypes(), subClassToInterface, functionsByOwnerPrefix),
                buildEnums(typeIndex, subClassToInterface));
    }

    private ModuleTypeIndex indexTypes(SortedMap<String, GenericDataType> types) {
        var dataParentTypes = new TreeSet<CompiledDataParentType>();
        var dataTypes = new TreeSet<CompiledDataType>();
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
            }
        }
        return new ModuleTypeIndex(dataParentTypes, dataTypes, Set.copyOf(enumValueTypeNames));
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

    private SortedMap<CompiledDataType, SortedSet<JavaInterface>> findSubClassToInterface(SortedSet<CompiledDataParentType> dataParentTypes, SortedSet<JavaInterface> interfaces) {
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
                        TreeMap::new,
                        mapping(ClassToJavaInterface::parent, toCollection(TreeSet::new))
                ));
    }

    private static JavaType buildClassName(String name) {
        return new JavaType(normalizeJavaTypeIdentifier(name));
    }

    private SortedSet<JavaMethod> buildStaticMethods(Set<CompiledFunction> functions) {
        return functions.stream()
                .filter(function -> !function.name().startsWith(METHOD_DECL_PREFIX))
                .map(this::buildStaticMethod)
                .collect(toCollection(TreeSet::new));
    }

    private JavaMethod buildStaticMethod(CompiledFunction function) {
        var methodTypeParameters = methodTypeParameters(function, Set.of());
        return new JavaMethod(
                buildMethodName(function.name()),
                function.name().startsWith("_"),
                function.programMain(),
                methodTypeParameters,
                buildJavaReturnType(function),
                buildJavaFunctionParameters(function.parameters()),
                function.expression(),
                function.comments()
        );
    }

    private String buildMethodName(String name) {
        return normalizeJavaMethodIdentifier(name);
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
        if (isTypeLikeIdentifier(memberName)) {
            return buildClassName(memberName).toString();
        }
        return buildMethodName(memberName);
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
            case GenericDataType genericDataType -> buildGenericDataType(genericDataType);
            case PrimitiveLinkedType primitiveLinkedType -> buildPrimitiveLinkedType(primitiveLinkedType);
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType);
            case CompiledTupleType linkedTupleType -> new JavaType("java.util.List<?>");
            case CompiledFunctionType functionType -> functionType.argumentType() == PrimitiveLinkedType.NOTHING
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
            var elementType = inferOptionElementType(function.expression());
            return new JavaType("java.util.Optional<" + buildJavaBoxedType(elementType) + ">");
        }
        return buildJavaType(function.returnType());
    }

    private CompiledType inferOptionElementType(dev.capylang.compiler.expression.CompiledExpression expression) {
        return switch (expression) {
            case dev.capylang.compiler.expression.CompiledPipeExpression pipeExpression ->
                    isOptionType(pipeExpression.type()) ? pipeExpression.mapper().type() : PrimitiveLinkedType.ANY;
            case dev.capylang.compiler.expression.CompiledPipeFilterOutExpression filterOutExpression -> {
                if (!isOptionType(filterOutExpression.type())) {
                    yield PrimitiveLinkedType.ANY;
                }
                var inferredSource = inferOptionElementType(filterOutExpression.source());
                if (!(inferredSource instanceof PrimitiveLinkedType primitiveLinkedType)
                    || primitiveLinkedType != PrimitiveLinkedType.ANY) {
                    yield inferredSource;
                }
                yield filterOutExpression.source().type();
            }
            case dev.capylang.compiler.expression.CompiledNewData newDataExpression -> {
                if (!(newDataExpression.type() instanceof GenericDataType genericDataType)
                    || !isOptionSomeTypeName(genericDataType.name())) {
                    yield PrimitiveLinkedType.ANY;
                }
                yield newDataExpression.assignments().stream()
                        .filter(assignment -> "value".equals(assignment.name()))
                        .map(dev.capylang.compiler.expression.CompiledNewData.FieldAssignment::value)
                        .map(dev.capylang.compiler.expression.CompiledExpression::type)
                        .findFirst()
                        .orElse(PrimitiveLinkedType.ANY);
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
                            .map(dev.capylang.compiler.expression.CompiledMatchExpression.MatchCase::expression)
                            .map(this::inferOptionElementType)
                            .reduce(dev.capylang.compiler.expression.CapybaraTypeFinder::findHigherType)
                            .orElse(PrimitiveLinkedType.ANY);
            default -> PrimitiveLinkedType.ANY;
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
        if ("Option".equals(rawTypeName) || isOptionTypeName(rawTypeName)) {
            return new JavaType("java.util.Optional");
        }
        if ("Program".equals(rawTypeName) || isProgramTypeName(rawTypeName)) {
            return new JavaType("capy.lang.Program");
        }
        if (rawTypeName.startsWith("/") && !rawTypeName.contains(".")) {
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < rawTypeName.length() - 1) {
                var packageName = rawTypeName.substring(1, slashIndex).replace('/', '.');
                var className = buildClassName(rawTypeName.substring(slashIndex + 1));
                return new JavaType(packageName + "." + className);
            }
        }
        if (rawTypeName.contains("/") && rawTypeName.contains(".")) {
            var dotIndex = rawTypeName.lastIndexOf('.');
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = rawTypeName.startsWith("/") ? 1 : 0;
                var modulePath = rawTypeName.substring(startIdx, slashIndex).replace('/', '.');
                var moduleName = buildClassName(rawTypeName.substring(slashIndex + 1, dotIndex));
                var nestedType = buildClassName(rawTypeName.substring(dotIndex + 1));
                if (moduleName.equals(nestedType)) {
                    return new JavaType(modulePath + "." + moduleName);
                }
                return new JavaType(modulePath + "." + moduleName + "." + nestedType);
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

    private JavaType withTypeParametersIfGeneric(GenericDataType type, String rawJavaTypeName) {
        var typeParameters = switch (type) {
            case CompiledDataType linkedDataType -> linkedDataType.typeParameters();
            case CompiledDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
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
        var cached = mappedTypeParameterDescriptorCache.get(descriptor);
        if (cached != null) {
            return cached;
        }
        var mapped = computeTypeParameterDescriptor(descriptor);
        mappedTypeParameterDescriptorCache.put(descriptor, mapped);
        return mapped;
    }

    private String computeTypeParameterDescriptor(String descriptor) {
        var normalized = descriptor.trim();
        return switch (normalized) {
            case "byte" -> "java.lang.Byte";
            case "int" -> "java.lang.Integer";
            case "long" -> "java.lang.Long";
            case "double" -> "java.lang.Double";
            case "float" -> "java.lang.Float";
            case "bool" -> "java.lang.Boolean";
            case "string" -> "java.lang.String";
            case "any", "nothing", "data" -> "java.lang.Object";
            default -> {
                if (normalized.matches("[A-Z][A-Za-z0-9_]*")) {
                    yield normalized;
                }
                if (normalized.startsWith("list[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("list[".length(), normalized.length() - 1);
                    yield "java.util.List<" + mapTypeParameterDescriptor(inner) + ">";
                }
                if (normalized.startsWith("set[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("set[".length(), normalized.length() - 1);
                    yield "java.util.Set<" + mapTypeParameterDescriptor(inner) + ">";
                }
                if (normalized.startsWith("dict[") && normalized.endsWith("]")) {
                    var inner = normalized.substring("dict[".length(), normalized.length() - 1);
                    yield "java.util.Map<java.lang.String, " + mapTypeParameterDescriptor(inner) + ">";
                }
                if (normalized.startsWith("tuple[") && normalized.endsWith("]")) {
                    yield "java.util.List<java.lang.Object>";
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
        return normalizedRawTypeReferenceCache.computeIfAbsent(rawTypeName, this::computeNormalizedRawTypeReference);
    }

    private String computeNormalizedRawTypeReference(String rawTypeName) {
        if ("Option".equals(rawTypeName) || isOptionTypeName(rawTypeName) || isOptionSomeTypeName(rawTypeName)) {
            return "java.util.Optional";
        }
        if ("None".equals(rawTypeName) || normalizeQualifiedTypeName(rawTypeName).endsWith("/Option.None")) {
            return "java.util.Optional";
        }
        if ("Program".equals(rawTypeName) || isProgramTypeName(rawTypeName)) {
            return "capy.lang.Program";
        }
        if (rawTypeName.startsWith("/") && !rawTypeName.contains(".")) {
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < rawTypeName.length() - 1) {
                var packageName = rawTypeName.substring(1, slashIndex).replace('/', '.');
                var className = buildClassName(rawTypeName.substring(slashIndex + 1));
                return packageName + "." + className;
            }
        }
        if (rawTypeName.contains("/") && rawTypeName.contains(".")) {
            var dotIndex = rawTypeName.lastIndexOf('.');
            var slashIndex = rawTypeName.lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = rawTypeName.startsWith("/") ? 1 : 0;
                var modulePath = rawTypeName.substring(startIdx, slashIndex).replace('/', '.');
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
        return switch (type) {
            case BYTE -> new JavaType("byte");
            case INT -> new JavaType("int");
            case LONG -> new JavaType("long");
            case DOUBLE -> new JavaType("double");
            case STRING -> new JavaType("java.lang.String");
            case BOOL -> new JavaType("boolean");
            case FLOAT -> new JavaType("float");
            case NOTHING -> new JavaType("java.lang.Object");
            case ANY -> new JavaType("java.lang.Object");
            case DATA -> new JavaType("java.lang.Object");
        };
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
            case PrimitiveLinkedType primitiveLinkedType -> switch (primitiveLinkedType) {
                case BYTE -> "java.lang.Byte";
                case INT -> "java.lang.Integer";
                case LONG -> "java.lang.Long";
                case DOUBLE -> "java.lang.Double";
                case STRING -> "java.lang.String";
                case BOOL -> "java.lang.Boolean";
                case FLOAT -> "java.lang.Float";
                case NOTHING -> "java.lang.Object";
                case ANY -> "java.lang.Object";
                case DATA -> "java.lang.Object";
            };
            case GenericDataType genericDataType -> buildGenericDataType(genericDataType).toString();
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType).toString();
            case CompiledTupleType linkedTupleType -> "java.util.List<?>";
            case CompiledFunctionType functionType -> functionType.argumentType() == PrimitiveLinkedType.NOTHING
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
               && type.fields().getFirst().type() == PrimitiveLinkedType.STRING;
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

    private List<JavaMethod.JavaFunctionParameter> buildJavaFunctionParameters(List<CompiledFunction.CompiledFunctionParameter> parameters) {
        return parameters.stream().map(this::buildJavaFunctionParameter).toList();
    }

    private JavaMethod.JavaFunctionParameter buildJavaFunctionParameter(CompiledFunction.CompiledFunctionParameter parameter) {
        return new JavaMethod.JavaFunctionParameter(
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

    private SortedSet<JavaInterface> buildInterfaces(
            SortedSet<CompiledDataParentType> dataParentTypes,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix
    ) {
        return dataParentTypes.stream()
                .filter(parentType -> !parentType.enumType())
                .map(parentType -> buildInterface(parentType, functionsByOwnerPrefix))
                .collect(toCollection(TreeSet::new));
    }

    private JavaInterface buildInterface(CompiledDataParentType type, SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix) {
        return new JavaSealedInterface(
                buildClassName(type.name()),
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
                buildMethodName(methodName),
                methodName.startsWith("_"),
                false,
                methodTypeParameters,
                buildJavaReturnType(function),
                buildJavaFunctionParameters(parameters),
                function.expression(),
                function.comments()
        );
    }

    private List<JavaInterfaceMethod> buildJavaMethods(List<CompiledDataType.CompiledField> fields) {
        return fields.stream().map(this::buildJavaMethod).toList();
    }

    private JavaInterfaceMethod buildJavaMethod(CompiledDataType.CompiledField field) {
        return new JavaInterfaceMethod(
                buildMethodName(field.name()),
                buildJavaType(field.type()));
    }

    private SortedSet<JavaRecord> buildRecords(
            SortedSet<CompiledDataType> dataTypes,
            SortedMap<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix
    ) {
        return dataTypes.stream()
                .filter(dt -> !dt.singleton())
                .map(dt -> buildRecord(dt, subClassToInterface, functionsByOwnerPrefix))
                .collect(toCollection(TreeSet::new));
    }

    private JavaRecord buildRecord(
            CompiledDataType type,
            Map<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface,
            SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix
    ) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? new TreeSet<JavaType>()
                : javaInterface.stream().map(javaType -> implementedInterfaceType(type, javaType)).collect(toCollection(TreeSet::new));

        var interfaceFields = javaInterface == null
                ? Stream.<JavaRecord.JavaRecordField>empty()
                : javaInterface.stream()
                        .map(JavaInterface::methods)
                        .flatMap(List::stream)
                        .map(method -> new JavaRecord.JavaRecordField(
                                method.name(),
                                method.returnType()));
        var recordFields = isResultErrorDataType(type)
                ? Stream.of(new JavaRecord.JavaRecordField(
                        "ex",
                        new JavaType("dev.capylang.CapybaraException")))
                : type.fields().stream()
                        .map(field -> new JavaRecord.JavaRecordField(
                                field.name(),
                                buildJavaType(field.type())));
        var fieldsByName = new java.util.LinkedHashMap<String, JavaRecord.JavaRecordField>();
        Stream.concat(interfaceFields, recordFields).forEach(field -> fieldsByName.put(field.name(), field));
        var fields = List.copyOf(fieldsByName.values());
        var recordTypeParameters = type.typeParameters().isEmpty() && javaInterface != null
                ? javaInterface.stream()
                        .flatMap(parent -> interfaceTypeParameters(parent).stream())
                        .distinct()
                        .toList()
                : type.typeParameters();
        return new JavaRecord(
                buildClassName(type.name()),
                type.name().startsWith("_"),
                type.comments(),
                implementInterfaces,
                fields,
                recordTypeParameters,
                new TreeSet<JavaMethod>(),
                buildRecordMethods(type, functionsByOwnerPrefix));
    }

    private SortedSet<JavaMethod> buildRecordMethods(CompiledDataType type, SortedMap<String, List<CompiledFunction>> functionsByOwnerPrefix) {
        var ownerPrefix = METHOD_DECL_PREFIX + type.name() + "__";
        return functionsByOwnerPrefix.getOrDefault(ownerPrefix, List.of()).stream()
                .map(function -> buildRecordMethod(function, ownerPrefix))
                .collect(toCollection(TreeSet::new));
    }

    private JavaMethod buildRecordMethod(CompiledFunction function, String ownerPrefix) {
        var methodName = function.name().substring(ownerPrefix.length());
        var parameters = function.parameters().stream().skip(1).toList();
        var methodTypeParameters = methodTypeParameters(function, Set.copyOf(extractOwnerTypeParameters(function)));
        return new JavaMethod(
                buildMethodName(methodName),
                methodName.startsWith("_"),
                false,
                methodTypeParameters,
                buildJavaReturnType(function),
                buildJavaFunctionParameters(parameters),
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
            case CollectionLinkedType.CompiledList linkedList ->
                    collectGenericTypeParameters(linkedList.elementType(), genericNames);
            case CollectionLinkedType.CompiledSet linkedSet ->
                    collectGenericTypeParameters(linkedSet.elementType(), genericNames);
            case CollectionLinkedType.CompiledDict linkedDict ->
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
            case JavaSealedInterface javaSealedInterface -> javaSealedInterface.typeParameters();
            case JavaNormalInterface javaNormalInterface -> List.<String>of();
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

    private SortedSet<JavaEnum> buildEnums(
            ModuleTypeIndex typeIndex,
            SortedMap<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface
    ) {
        var singletonEnums = typeIndex.dataTypes().stream()
                .filter(CompiledDataType::singleton)
                .filter(dt -> !typeIndex.enumValueTypeNames().contains(dt.name()))
                .map(dt -> buildSingletonEnum(dt, subClassToInterface));
        var declaredEnums = typeIndex.dataParentTypes().stream()
                .filter(CompiledDataParentType::enumType)
                .map(this::buildDeclaredEnum);
        return Stream.concat(singletonEnums, declaredEnums).collect(toCollection(TreeSet::new));
    }

    private JavaEnum buildSingletonEnum(CompiledDataType type, SortedMap<CompiledDataType, SortedSet<JavaInterface>> subClassToInterface) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? new TreeSet<JavaType>()
                : javaInterface.stream().map(JavaInterface::name).collect(toCollection(TreeSet::new));
        return new JavaEnum(buildClassName(type.name()), implementInterfaces, List.of("INSTANCE"));
    }

    private JavaEnum buildDeclaredEnum(CompiledDataParentType enumType) {
        return new JavaEnum(
                buildClassName(enumType.name()),
                new TreeSet<JavaType>(),
                enumType.subTypes().stream().map(CompiledDataType::name).toList()
        );
    }

    private record ModuleTypeIndex(
            SortedSet<CompiledDataParentType> dataParentTypes,
            SortedSet<CompiledDataType> dataTypes,
            Set<String> enumValueTypeNames
    ) {
    }
}
