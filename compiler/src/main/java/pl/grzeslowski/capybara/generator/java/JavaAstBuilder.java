package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.generator.java.JavaInterface.JavaInterfaceMethod;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.*;
import static pl.grzeslowski.capybara.generator.java.JavaAnnotation.generatedAnnotation;

public class JavaAstBuilder {
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

    public JavaClass build(LinkedModule module) {
        var interfaces = buildInterfaces(module.types()
                .values()
                .stream()
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .toList(), module.functions());
        var subClassToInterface = findSubClassToInterface(module.types(), interfaces);
        var dataTypes = module.types()
                .values()
                .stream()
                .filter(LinkedDataType.class::isInstance)
                .map(LinkedDataType.class::cast)
                .toList();
        return new JavaClass(
                Set.of(generatedAnnotation()),
                buildClassName(module.name()),
                new JavaPackage(buildJavaPackageName(module.path())),
                module.staticImports().stream()
                        .map(staticImport -> normalizeJavaClassReference(staticImport.className()) + "." + buildJavaStaticImportMember(staticImport.memberName()))
                        .collect(toSet()),
                buildStaticMethods(module.functions()),
                interfaces,
                buildRecords(dataTypes, subClassToInterface, module.functions()),
                buildEnums(dataTypes, subClassToInterface));
    }

    private Map<LinkedDataType, Set<JavaInterface>> findSubClassToInterface(Map<String, GenericDataType> types, Set<JavaInterface> interfaces) {
        var javaInterfaceByJavaName = interfaces.stream()
                .collect(toMap(
                        jInterface -> jInterface.name().name(),
                        identity()));

        record ClassToInterface(LinkedDataType data, LinkedDataParentType parent) {
        }
        record ClassToJavaInterface(LinkedDataType data, JavaInterface parent) {
        }

        return types.values()
                .stream()
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .flatMap(parent -> parent.subTypes().stream().map(data -> new ClassToInterface(data, parent)))
                .map(pair -> new ClassToJavaInterface(
                        pair.data,
                        javaInterfaceByJavaName.get(buildClassName(pair.parent.name()).name())))
                .filter(pair -> pair.parent != null)
                .collect(groupingBy(
                        ClassToJavaInterface::data,
                        mapping(ClassToJavaInterface::parent, toSet())
                ));
    }

    private static JavaType buildClassName(String name) {
        return new JavaType(normalizeJavaTypeIdentifier(name));
    }

    private Set<JavaMethod> buildStaticMethods(Set<LinkedFunction> functions) {
        return functions.stream()
                .filter(function -> !function.name().startsWith(METHOD_DECL_PREFIX))
                .map(this::buildStaticMethod)
                .collect(toSet());
    }

    private JavaMethod buildStaticMethod(LinkedFunction function) {
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

    private JavaType buildJavaType(LinkedType type) {
        return switch (type) {
            case GenericDataType genericDataType -> buildGenericDataType(genericDataType);
            case PrimitiveLinkedType primitiveLinkedType -> buildPrimitiveLinkedType(primitiveLinkedType);
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType);
            case LinkedTupleType linkedTupleType -> new JavaType("java.util.List<?>");
            case LinkedFunctionType functionType -> new JavaType(
                    "java.util.function.Function<"
                    + buildJavaBoxedType(functionType.argumentType())
                    + ", "
                    + buildJavaBoxedType(functionType.returnType())
                    + ">"
            );
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> new JavaType(linkedGenericTypeParameter.name());
        };
    }

    private JavaType buildJavaReturnType(LinkedFunction function) {
        if (function.returnType() instanceof GenericDataType genericDataType
            && ("Option".equals(genericDataType.name()) || isOptionTypeName(genericDataType.name()))) {
            var elementType = inferOptionElementType(function.expression());
            return new JavaType("java.util.Optional<" + buildJavaBoxedType(elementType) + ">");
        }
        return buildJavaType(function.returnType());
    }

    private LinkedType inferOptionElementType(pl.grzeslowski.capybara.linker.expression.LinkedExpression expression) {
        return switch (expression) {
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeExpression pipeExpression ->
                    isOptionType(pipeExpression.type()) ? pipeExpression.mapper().type() : PrimitiveLinkedType.ANY;
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeFilterOutExpression filterOutExpression -> {
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
            case pl.grzeslowski.capybara.linker.expression.LinkedNewData newDataExpression -> {
                if (!(newDataExpression.type() instanceof GenericDataType genericDataType)
                    || !isOptionSomeTypeName(genericDataType.name())) {
                    yield PrimitiveLinkedType.ANY;
                }
                yield newDataExpression.assignments().stream()
                        .filter(assignment -> "value".equals(assignment.name()))
                        .map(pl.grzeslowski.capybara.linker.expression.LinkedNewData.FieldAssignment::value)
                        .map(pl.grzeslowski.capybara.linker.expression.LinkedExpression::type)
                        .findFirst()
                        .orElse(PrimitiveLinkedType.ANY);
            }
            case pl.grzeslowski.capybara.linker.expression.LinkedLetExpression letExpression ->
                    inferOptionElementType(letExpression.rest());
            case pl.grzeslowski.capybara.linker.expression.LinkedIfExpression ifExpression ->
                    pl.grzeslowski.capybara.linker.expression.CapybaraTypeFinder.findHigherType(
                            inferOptionElementType(ifExpression.thenBranch()),
                            inferOptionElementType(ifExpression.elseBranch()));
            case pl.grzeslowski.capybara.linker.expression.LinkedIndexExpression indexExpression ->
                    indexExpression.elementType();
            case pl.grzeslowski.capybara.linker.expression.LinkedMatchExpression matchExpression ->
                    matchExpression.cases().stream()
                            .map(pl.grzeslowski.capybara.linker.expression.LinkedMatchExpression.MatchCase::expression)
                            .map(this::inferOptionElementType)
                            .reduce(pl.grzeslowski.capybara.linker.expression.CapybaraTypeFinder::findHigherType)
                            .orElse(PrimitiveLinkedType.ANY);
            default -> PrimitiveLinkedType.ANY;
        };
    }

    private boolean isOptionType(LinkedType type) {
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
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters();
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
        };
        if (typeParameters.isEmpty() || !typeParameters.stream().allMatch(typeName -> typeName.matches("[A-Z]"))) {
            return new JavaType(rawJavaTypeName);
        }
        return new JavaType(rawJavaTypeName + "<" + String.join(", ", typeParameters) + ">");
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
            case LinkedList linkedList -> new JavaType("java.util.List<" + buildJavaBoxedType(linkedList.elementType()) + ">");
            case LinkedDict linkedDict -> new JavaType("java.util.Map<java.lang.String, " + buildJavaBoxedType(linkedDict.valueType()) + ">");
            case LinkedSet linkedSet -> new JavaType("java.util.Set<" + buildJavaBoxedType(linkedSet.elementType()) + ">");
        };
    }

    private String buildJavaBoxedType(LinkedType type) {
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
            case LinkedTupleType linkedTupleType -> "java.util.List<?>";
            case LinkedFunctionType functionType -> "java.util.function.Function<"
                    + buildJavaBoxedType(functionType.argumentType())
                    + ", "
                    + buildJavaBoxedType(functionType.returnType())
                    + ">";
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
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

    private boolean isResultErrorDataType(LinkedDataType type) {
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

    private String normalizeQualifiedTypeName(String name) {
        var normalized = name.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private List<JavaMethod.JavaFunctionParameter> buildJavaFunctionParameters(List<LinkedFunction.LinkedFunctionParameter> parameters) {
        return parameters.stream().map(this::buildJavaFunctionParameter).toList();
    }

    private JavaMethod.JavaFunctionParameter buildJavaFunctionParameter(LinkedFunction.LinkedFunctionParameter parameter) {
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
        var parts = Stream.of(name.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();

        var result = new StringBuilder();
        var first = true;
        for (var part : parts) {
            if (first) {
                result.append(Character.toLowerCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
                first = false;
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        if (result.isEmpty()) {
            result.append("value");
        }
        if (!Character.isJavaIdentifierStart(result.charAt(0))) {
            result.insert(0, 'v');
        }
        var identifier = result.toString();
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

    private Set<JavaInterface> buildInterfaces(List<LinkedDataParentType> dataParentTypes, Set<LinkedFunction> functions) {
        return dataParentTypes.stream()
                .map(parentType -> buildInterface(parentType, functions))
                .collect(toSet());
    }

    private JavaInterface buildInterface(LinkedDataParentType type, Set<LinkedFunction> functions) {
        return new JavaSealedInterface(
                buildClassName(type.name()),
                buildJavaMethods(type.fields()),
                type.subTypes().stream().map(LinkedDataType::name).map(name -> buildClassName(name).toString()).toList(),
                type.typeParameters(),
                buildInterfaceMethods(type, functions)
        );
    }

    private List<JavaMethod> buildInterfaceMethods(LinkedDataParentType type, Set<LinkedFunction> functions) {
        var ownerPrefix = METHOD_DECL_PREFIX + type.name() + "__";
        return functions.stream()
                .filter(function -> function.name().startsWith(ownerPrefix))
                .map(function -> buildInterfaceMethod(function, ownerPrefix))
                .toList();
    }

    private JavaMethod buildInterfaceMethod(LinkedFunction function, String ownerPrefix) {
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

    private List<JavaInterfaceMethod> buildJavaMethods(List<LinkedDataType.LinkedField> fields) {
        return fields.stream().map(this::buildJavaMethod).toList();
    }

    private JavaInterfaceMethod buildJavaMethod(LinkedDataType.LinkedField field) {
        return new JavaInterfaceMethod(
                buildMethodName(field.name()),
                buildJavaType(field.type()));
    }

    private Set<JavaRecord> buildRecords(
            List<LinkedDataType> dataTypes,
            Map<LinkedDataType, Set<JavaInterface>> subClassToInterface,
            Set<LinkedFunction> functions
    ) {
        return dataTypes.stream()
                .filter(dt -> !dt.singleton())
                .map(dt -> buildRecord(dt, subClassToInterface, functions))
                .collect(toSet());
    }

    private JavaRecord buildRecord(
            LinkedDataType type,
            Map<LinkedDataType, Set<JavaInterface>> subClassToInterface,
            Set<LinkedFunction> functions
    ) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? Set.<JavaType>of()
                : javaInterface.stream().map(javaType -> implementedInterfaceType(type, javaType)).collect(toSet());

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
                        new JavaType("pl.grzeslowski.capybara.CapybaraException")))
                : type.fields().stream()
                        .map(field -> new JavaRecord.JavaRecordField(
                                field.name(),
                                buildJavaType(field.type())));
        var fieldsByName = new java.util.LinkedHashMap<String, JavaRecord.JavaRecordField>();
        Stream.concat(interfaceFields, recordFields).forEach(field -> fieldsByName.put(field.name(), field));
        var fields = List.copyOf(fieldsByName.values());
        return new JavaRecord(
                buildClassName(type.name()),
                type.name().startsWith("_"),
                implementInterfaces,
                fields,
                type.typeParameters(),
                Set.of(),
                buildRecordMethods(type, functions));
    }

    private Set<JavaMethod> buildRecordMethods(LinkedDataType type, Set<LinkedFunction> functions) {
        var ownerPrefix = METHOD_DECL_PREFIX + type.name() + "__";
        return functions.stream()
                .filter(function -> function.name().startsWith(ownerPrefix))
                .map(function -> buildRecordMethod(function, ownerPrefix))
                .collect(toSet());
    }

    private JavaMethod buildRecordMethod(LinkedFunction function, String ownerPrefix) {
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

    private List<String> extractOwnerTypeParameters(LinkedFunction function) {
        if (function.parameters().isEmpty()) {
            return List.of();
        }
        var thisType = function.parameters().getFirst().type();
        return switch (thisType) {
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters();
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters();
            default -> List.of();
        };
    }

    private List<String> methodTypeParameters(LinkedFunction function, Set<String> ownerTypeParameters) {
        var genericNames = new java.util.LinkedHashSet<String>();
        function.parameters().forEach(parameter -> collectGenericTypeParameters(parameter.type(), genericNames));
        collectGenericTypeParameters(function.returnType(), genericNames);
        genericNames.removeAll(ownerTypeParameters);
        return List.copyOf(genericNames);
    }

    private void collectGenericTypeParameters(LinkedType type, Set<String> genericNames) {
        switch (type) {
            case LinkedGenericTypeParameter genericTypeParameter -> genericNames.add(genericTypeParameter.name());
            case LinkedDataType linkedDataType ->
                    linkedDataType.typeParameters().forEach(typeName -> addGenericTypeName(typeName, genericNames));
            case LinkedDataParentType linkedDataParentType ->
                    linkedDataParentType.typeParameters().forEach(typeName -> addGenericTypeName(typeName, genericNames));
            case CollectionLinkedType.LinkedList linkedList ->
                    collectGenericTypeParameters(linkedList.elementType(), genericNames);
            case CollectionLinkedType.LinkedSet linkedSet ->
                    collectGenericTypeParameters(linkedSet.elementType(), genericNames);
            case CollectionLinkedType.LinkedDict linkedDict ->
                    collectGenericTypeParameters(linkedDict.valueType(), genericNames);
            case LinkedTupleType linkedTupleType ->
                    linkedTupleType.elementTypes().forEach(elementType -> collectGenericTypeParameters(elementType, genericNames));
            case LinkedFunctionType linkedFunctionType -> {
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

    private JavaType implementedInterfaceType(LinkedDataType type, JavaInterface javaInterface) {
        var interfaceTypeParameters = switch (javaInterface) {
            case JavaSealedInterface javaSealedInterface -> javaSealedInterface.typeParameters();
            case JavaNormalInterface javaNormalInterface -> List.<String>of();
        };
        if (interfaceTypeParameters.isEmpty() || type.typeParameters().size() != interfaceTypeParameters.size()) {
            return javaInterface.name();
        }
        return new JavaType(javaInterface.name() + "<" + String.join(", ", type.typeParameters()) + ">");
    }

    private Set<JavaEnum> buildEnums(List<LinkedDataType> dataTypes, Map<LinkedDataType, Set<JavaInterface>> subClassToInterface) {
        return dataTypes.stream()
                .filter(LinkedDataType::singleton)
                .map(dt -> buildEnum(dt, subClassToInterface))
                .collect(toSet());
    }

    private JavaEnum buildEnum(LinkedDataType type, Map<LinkedDataType, Set<JavaInterface>> subClassToInterface) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? Set.<JavaType>of()
                : javaInterface.stream().map(JavaInterface::name).collect(toSet());
        return new JavaEnum(buildClassName(type.name()), implementInterfaces);
    }
}
