package dev.capylang.compiler;

import capy.serialization.Json;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.RecordComponent;
import java.lang.reflect.Type;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

public final class LinkedJsonCodec {
    private static final String CLASS_KEY = "@class";
    private static final String PROGRAM_FILE = "program.json";
    private static final String BUILD_INFO_FILE = "build-info.json";
    private static final String TREE_MAP_CLASS = "java.util.TreeMap";
    private static final String LIST_CLASS = "java.util.ImmutableCollections$ListN";
    private static final String LEGACY_COMPILER_PACKAGE = "dev.capylang.compiler.";
    private static final String LEGACY_EXPRESSION_PACKAGE = "dev.capylang.compiler.expression.";

    private LinkedJsonCodec() {
    }

    public static CompiledProgram readProgram(Path inputDir, boolean requireModules) {
        var programFile = inputDir.resolve(PROGRAM_FILE);
        if (Files.isRegularFile(programFile)) {
            var program = readFile(programFile, CompiledProgram.class);
            if (requireModules && program.modules().isEmpty()) {
                throw new IllegalArgumentException("Missing linked module files in directory: " + inputDir);
            }
            return program;
        }
        try (var files = Files.walk(inputDir)) {
            var modules = files
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".json"))
                    .filter(path -> !path.getFileName().toString().equals(BUILD_INFO_FILE))
                    .map(path -> readFile(path, CompiledModule.class))
                    .toList();
            if (requireModules && modules.isEmpty()) {
                throw new IllegalArgumentException("Missing linked module files in directory: " + inputDir);
            }
            return new CompiledProgram(
                    modules,
                    List.of(),
                    CompiledProgramModule.emptyNativeProviderManifest(),
                    CompiledProgramModule.emptyNativeProviderCatalog()
            );
        } catch (IOException exception) {
            throw new UncheckedIOException("Unable to read linked module JSONs from: " + inputDir, exception);
        }
    }

    public static void writeProgram(Path outputDir, CompiledProgram program) throws IOException {
        Files.createDirectories(outputDir);
        Files.writeString(outputDir.resolve(PROGRAM_FILE), write(program));
        for (var module : program.modules()) {
            writeModule(outputDir, module);
        }
    }

    public static <T> T readFile(Path path, Class<T> targetType) {
        try {
            return read(Files.readString(path), targetType);
        } catch (IOException exception) {
            throw new UncheckedIOException("Unable to read linked JSON: " + path, exception);
        }
    }

    public static <T> T read(String json, Class<T> targetType) {
        return targetType.cast(fromJson(parseObject(json), targetType, targetType));
    }

    public static Object readPlain(String json) {
        return plainValue(parseObject(json));
    }

    public static String write(Object value) {
        return Json.stringify(toJson(value, false));
    }

    private static void writeModule(Path outputDir, CompiledModule module) throws IOException {
        var directory = module.path().isBlank() ? outputDir : outputDir.resolve(module.path());
        Files.createDirectories(directory);
        Files.writeString(directory.resolve(module.name() + ".json"), write(module));
    }

    private static Json.JsonObject parseObject(String json) {
        return new Parser(json).parseObject();
    }

    private static Object fromJson(Json json, Type declaredType, Class<?> rawDeclaredType) {
        if (json == null || json instanceof Json.JsonNull) {
            return optionalOrNull(rawDeclaredType);
        }
        if (rawDeclaredType == CompiledExpression.class && !isCurrentExpressionJson(json)) {
            return fromJson(
                    unsupportedExpressionJson("legacy linked expression", null),
                    CompiledExpression.CompiledUnsupportedExpression.class,
                    CompiledExpression.CompiledUnsupportedExpression.class
            );
        }
        if (rawDeclaredType == CompiledTypeReference.class || isLegacyLinkedTypeJson(json)) {
            return typeReference(json);
        }
        if (rawDeclaredType == Optional.class) {
            return Optional.ofNullable(fromJson(json, Object.class, Object.class));
        }
        if (json instanceof Json.JsonString string) {
            return fromJsonString(string.value(), rawDeclaredType);
        }
        if (json instanceof Json.JsonBool bool) {
            return bool.value();
        }
        if (json instanceof Json.JsonNumberLong number) {
            return fromJsonLong(number.value(), rawDeclaredType);
        }
        if (json instanceof Json.JsonNumberDouble number) {
            return fromJsonDouble(number.value(), rawDeclaredType);
        }
        if (json instanceof Json.JsonArray array) {
            if (rawDeclaredType == String.class) {
                return stringValue(array);
            }
            return fromJsonArray(array.value(), declaredType, rawDeclaredType);
        }
        if (json instanceof Json.JsonValueArray array) {
            return fromJson(array.value(), declaredType, rawDeclaredType);
        }
        if (json instanceof Json.JsonValueObject object) {
            return fromJson(object.value(), declaredType, rawDeclaredType);
        }
        if (json instanceof Json.JsonObject object) {
            if (rawDeclaredType == String.class) {
                return stringValue(object);
            }
            return fromJsonObject(object.value(), declaredType, rawDeclaredType);
        }
        throw new IllegalArgumentException("Unsupported JSON value: " + json);
    }

    private static Object optionalOrNull(Class<?> rawDeclaredType) {
        if (rawDeclaredType == Optional.class) {
            return Optional.empty();
        }
        return null;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static Object fromJsonString(String value, Class<?> rawDeclaredType) {
        if (rawDeclaredType == Object.class || rawDeclaredType == String.class) {
            return value;
        }
        if (rawDeclaredType.isEnum()) {
            return Enum.valueOf((Class<? extends Enum>) rawDeclaredType.asSubclass(Enum.class), value);
        }
        return value;
    }

    private static Object fromJsonLong(long value, Class<?> rawDeclaredType) {
        if (rawDeclaredType == int.class || rawDeclaredType == Integer.class) {
            return (int) value;
        }
        if (rawDeclaredType == byte.class || rawDeclaredType == Byte.class) {
            return (byte) value;
        }
        if (rawDeclaredType == short.class || rawDeclaredType == Short.class) {
            return (short) value;
        }
        if (rawDeclaredType == double.class || rawDeclaredType == Double.class) {
            return (double) value;
        }
        if (rawDeclaredType == float.class || rawDeclaredType == Float.class) {
            return (float) value;
        }
        return value;
    }

    private static Object fromJsonDouble(double value, Class<?> rawDeclaredType) {
        if (rawDeclaredType == float.class || rawDeclaredType == Float.class) {
            return (float) value;
        }
        return value;
    }

    private static Object fromJsonArray(List<Json> values, Type declaredType, Class<?> rawDeclaredType) {
        var items = isTypedCollection(values) ? ((Json.JsonArray) values.get(1)).value() : values;
        var elementType = listElementType(declaredType);
        var rawElementType = rawType(elementType);
        if (java.util.Set.class.isAssignableFrom(rawDeclaredType)) {
            return new java.util.LinkedHashSet<>(items.stream()
                    .map(value -> fromJson(value, elementType, rawElementType))
                    .toList());
        }
        return items.stream()
                .map(value -> fromJson(value, elementType, rawElementType))
                .toList();
    }

    private static boolean isTypedCollection(List<Json> values) {
        return values.size() == 2
               && values.get(0) instanceof Json.JsonString
               && values.get(1) instanceof Json.JsonArray;
    }

    private static Object fromJsonObject(Map<String, Json> value, Type declaredType, Class<?> rawDeclaredType) {
        if (value.containsKey(CLASS_KEY)) {
            var className = stringValue(value.get(CLASS_KEY));
            if (isLegacyLinkedTypeClass(className)) {
                return typeReference(new Json.JsonObject(value));
            }
            if (rawDeclaredType.isEnum()) {
                return enumSingleton(rawDeclaredType);
            }
            if (className.startsWith(LEGACY_EXPRESSION_PACKAGE)) {
                return fromJson(
                        unsupportedExpressionJson("legacy linked expression: " + className, value.get("location")),
                        CompiledExpression.CompiledUnsupportedExpression.class,
                        CompiledExpression.CompiledUnsupportedExpression.class
                );
            }
            if (TREE_MAP_CLASS.equals(className) || Map.class.isAssignableFrom(rawDeclaredType)) {
                return typedMap(value, declaredType);
            }
            rawDeclaredType = loadClass(migratedClassName(className));
            declaredType = rawDeclaredType;
        }
        if (Map.class.isAssignableFrom(rawDeclaredType)) {
            return typedMap(value, declaredType);
        }
        if (rawDeclaredType == Object.class) {
            return plainObject(value);
        }
        if (rawDeclaredType.isRecord()) {
            return recordValue(value, rawDeclaredType);
        }
        if (rawDeclaredType.isEnum()) {
            return enumSingleton(rawDeclaredType);
        }
        throw new IllegalArgumentException("Cannot decode JSON object as " + rawDeclaredType.getName());
    }

    private static Map<String, Object> typedMap(Map<String, Json> value, Type declaredType) {
        var result = new TreeMap<String, Object>();
        var valueType = mapValueType(declaredType);
        var rawValueType = rawType(valueType);
        value.forEach((key, item) -> {
            if (!CLASS_KEY.equals(key)) {
                result.put(key, fromJson(item, valueType, rawValueType));
            }
        });
        return result;
    }

    private static Map<String, Object> plainObject(Map<String, Json> value) {
        var result = new LinkedHashMap<String, Object>();
        value.forEach((key, item) -> {
            if (!CLASS_KEY.equals(key)) {
                result.put(key, plainValue(item));
            }
        });
        return result;
    }

    private static Object plainValue(Json json) {
        if (json instanceof Json.JsonNull) {
            return null;
        }
        if (json instanceof Json.JsonString string) {
            return string.value();
        }
        if (json instanceof Json.JsonBool bool) {
            return bool.value();
        }
        if (json instanceof Json.JsonNumberLong number) {
            return number.value();
        }
        if (json instanceof Json.JsonNumberDouble number) {
            return number.value();
        }
        if (json instanceof Json.JsonArray array) {
            return array.value().stream().map(LinkedJsonCodec::plainValue).toList();
        }
        if (json instanceof Json.JsonValueArray array) {
            return plainValue(array.value());
        }
        if (json instanceof Json.JsonValueObject object) {
            return plainValue(object.value());
        }
        if (json instanceof Json.JsonObject object) {
            return plainObject(object.value());
        }
        throw new IllegalArgumentException("Unsupported JSON value: " + json);
    }

    private static Object recordValue(Map<String, Json> value, Class<?> targetType) {
        try {
            var components = targetType.getRecordComponents();
            var parameterTypes = new Class<?>[components.length];
            var arguments = new Object[components.length];
            for (var i = 0; i < components.length; i++) {
                var component = components[i];
                parameterTypes[i] = component.getType();
                arguments[i] = fromJson(componentJson(value, component), component.getGenericType(), component.getType());
            }
            var constructor = targetType.getDeclaredConstructor(parameterTypes);
            constructor.setAccessible(true);
            try {
                return constructor.newInstance(arguments);
            } catch (IllegalArgumentException exception) {
                throw new IllegalArgumentException("Cannot instantiate " + targetType.getName()
                                                   + " with " + argumentTypes(arguments), exception);
            }
        } catch (ReflectiveOperationException exception) {
            throw new IllegalArgumentException("Cannot instantiate " + targetType.getName(), exception);
        }
    }

    private static String argumentTypes(Object[] arguments) {
        return java.util.Arrays.stream(arguments)
                .map(argument -> argument == null ? "null" : argument.getClass().getName())
                .toList()
                .toString();
    }

    private static Json componentJson(Map<String, Json> value, RecordComponent component) {
        var name = component.getName();
        var exact = value.get(name);
        if (exact != null && !(exact instanceof Json.JsonNull)) {
            return exact;
        }
        var alias = switch (name) {
            case "body" -> value.get("expression");
            case "visibility" -> new Json.JsonString("public");
            case "typeReference" -> value.get("type");
            case "thenBranch" -> firstPresent(value, "ifTrue", "trueBranch", "thenExpression");
            case "elseBranch" -> firstPresent(value, "ifFalse", "falseBranch", "elseExpression");
            case "values" -> firstPresent(value, "items", "elements");
            case "arguments" -> firstPresent(value, "types", "parameters");
            case "typeName" -> firstPresent(value, "dataType", "constructor", "name");
            case "receiver" -> firstPresent(value, "target", "source");
            case "index" -> value.get("start");
            case "endIndex" -> value.get("end");
            default -> null;
        };
        return alias == null ? defaultJson(component) : alias;
    }

    private static Json firstPresent(Map<String, Json> value, String... names) {
        for (var name : names) {
            var item = value.get(name);
            if (item != null) {
                return item;
            }
        }
        return null;
    }

    private static Json defaultJson(RecordComponent component) {
        var type = component.getType();
        if (type == boolean.class || type == Boolean.class) {
            return new Json.JsonBool(false);
        }
        if (type == byte.class || type == short.class || type == int.class || type == long.class
            || type == Byte.class || type == Short.class || type == Integer.class || type == Long.class) {
            return new Json.JsonNumberLong(0);
        }
        if (type == float.class || type == double.class || type == Float.class || type == Double.class) {
            return new Json.JsonNumberDouble(0.0);
        }
        if (type == String.class) {
            return new Json.JsonString("");
        }
        if (type == CompiledSourceLocation.class) {
            return new Json.JsonObject(Map.of(
                    "line", new Json.JsonNumberLong(0),
                    "column", new Json.JsonNumberLong(0)
            ));
        }
        if (type == CompiledTypeReference.class) {
            return new Json.JsonObject(Map.of(
                    "name", new Json.JsonString("any"),
                    "arguments", new Json.JsonArray(List.of(new Json.JsonString(LIST_CLASS), new Json.JsonArray(List.of())))
            ));
        }
        if (type == CompiledExpression.class) {
            return unsupportedExpressionJson();
        }
        if (java.util.List.class.isAssignableFrom(type) || java.util.Set.class.isAssignableFrom(type)) {
            return new Json.JsonArray(List.of(new Json.JsonString(LIST_CLASS), new Json.JsonArray(List.of())));
        }
        if (Map.class.isAssignableFrom(type)) {
            return new Json.JsonObject(Map.of(CLASS_KEY, new Json.JsonString(TREE_MAP_CLASS)));
        }
        if (Optional.class.isAssignableFrom(type)) {
            return Json.JsonNull.INSTANCE;
        }
        return Json.JsonNull.INSTANCE;
    }

    private static Json.JsonObject unsupportedExpressionJson() {
        return unsupportedExpressionJson("legacy linked expression", null);
    }

    private static Json.JsonObject unsupportedExpressionJson(String source, Json location) {
        return new Json.JsonObject(Map.of(
                CLASS_KEY, new Json.JsonString(CompiledExpression.CompiledUnsupportedExpression.class.getName()),
                "source", new Json.JsonString(source),
                "location", location == null ? new Json.JsonObject(Map.of(
                        "line", new Json.JsonNumberLong(0),
                        "column", new Json.JsonNumberLong(0)
                )) : location
        ));
    }

    private static Object enumSingleton(Class<?> targetType) {
        var constants = targetType.getEnumConstants();
        if (constants.length == 1) {
            return constants[0];
        }
        throw new IllegalArgumentException("Cannot decode enum object as " + targetType.getName());
    }

    private static boolean isCurrentExpressionJson(Json json) {
        return json instanceof Json.JsonObject object
               && object.value().containsKey(CLASS_KEY)
               && stringValue(object.value().get(CLASS_KEY)).startsWith(CompiledExpression.class.getName() + "$");
    }

    private static boolean isLegacyLinkedTypeJson(Json json) {
        if (json instanceof Json.JsonArray array && !array.value().isEmpty()) {
            return array.value().getFirst() instanceof Json.JsonString string
                   && isLegacyLinkedTypeClass(string.value());
        }
        if (json instanceof Json.JsonObject object && object.value().containsKey(CLASS_KEY)) {
            return isLegacyLinkedTypeClass(stringValue(object.value().get(CLASS_KEY)));
        }
        return false;
    }

    private static boolean isLegacyLinkedTypeClass(String className) {
        return className.equals(LEGACY_COMPILER_PACKAGE + "PrimitiveLinkedType")
               || className.startsWith(LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$")
               || className.equals(LEGACY_COMPILER_PACKAGE + "CompiledDataType")
               || className.equals(LEGACY_COMPILER_PACKAGE + "CompiledDataParentType")
               || className.equals(LEGACY_COMPILER_PACKAGE + "CompiledFunctionType")
               || className.equals(LEGACY_COMPILER_PACKAGE + "CompiledGenericTypeParameter")
               || className.equals(LEGACY_COMPILER_PACKAGE + "CompiledTupleType");
    }

    private static CompiledTypeReference typeReference(Json json) {
        if (json instanceof Json.JsonArray array) {
            return typeReferenceFromArray(array.value());
        }
        if (json instanceof Json.JsonObject object) {
            return typeReferenceFromObject(object.value());
        }
        if (json instanceof Json.JsonString string) {
            return new CompiledTypeReference(typeName(string.value()), List.of());
        }
        return new CompiledTypeReference("any", List.of());
    }

    private static CompiledTypeReference typeReferenceFromArray(List<Json> values) {
        if (values.size() >= 2 && values.get(0) instanceof Json.JsonString className) {
            if (className.value().equals(LEGACY_COMPILER_PACKAGE + "PrimitiveLinkedType")
                && values.get(1) instanceof Json.JsonString primitiveName) {
                return new CompiledTypeReference(typeName(primitiveName.value()), List.of());
            }
            if (isLegacyLinkedTypeClass(className.value())) {
                return new CompiledTypeReference(typeName(className.value()), List.of());
            }
        }
        return new CompiledTypeReference("any", List.of());
    }

    private static CompiledTypeReference typeReferenceFromObject(Map<String, Json> value) {
        var className = value.containsKey(CLASS_KEY) ? stringValue(value.get(CLASS_KEY)) : "";
        if (className.equals(LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$CompiledList")) {
            return new CompiledTypeReference("List", List.of(typeReference(value.get("elementType"))));
        }
        if (className.equals(LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$CompiledSet")) {
            return new CompiledTypeReference("Set", List.of(typeReference(value.get("elementType"))));
        }
        if (className.equals(LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$CompiledDict")) {
            return new CompiledTypeReference("Dict", List.of(
                    typeReference(firstPresent(value, "keyType", "key")),
                    typeReference(firstPresent(value, "valueType", "value"))
            ));
        }
        if (className.equals(LEGACY_COMPILER_PACKAGE + "CompiledTupleType")) {
            return new CompiledTypeReference("Tuple", typeReferenceList(firstPresent(value, "elements", "types")));
        }
        if (className.equals(LEGACY_COMPILER_PACKAGE + "CompiledFunctionType")) {
            return new CompiledTypeReference("Function", List.of(
                    typeReference(value.get("argumentType")),
                    typeReference(value.get("returnType"))
            ));
        }
        if (value.containsKey("name")) {
            return new CompiledTypeReference(typeName(stringValue(value.get("name"))), typeReferenceList(value.get("arguments")));
        }
        return new CompiledTypeReference(typeName(className), List.of());
    }

    private static List<CompiledTypeReference> typeReferenceList(Json json) {
        if (json instanceof Json.JsonArray array) {
            var items = isTypedCollection(array.value()) ? ((Json.JsonArray) array.value().get(1)).value() : array.value();
            return items.stream().map(LinkedJsonCodec::typeReference).toList();
        }
        return List.of();
    }

    private static String typeName(String value) {
        return switch (value) {
            case "ANY" -> "any";
            case "NOTHING" -> "nothing";
            case "BOOL" -> "bool";
            case "BYTE" -> "byte";
            case "SHORT" -> "short";
            case "INT" -> "int";
            case "LONG" -> "long";
            case "FLOAT" -> "float";
            case "DOUBLE" -> "double";
            case "STRING" -> "String";
            case LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$CompiledList" -> "List";
            case LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$CompiledSet" -> "Set";
            case LEGACY_COMPILER_PACKAGE + "CollectionLinkedType$CompiledDict" -> "Dict";
            case LEGACY_COMPILER_PACKAGE + "CompiledTupleType" -> "Tuple";
            case LEGACY_COMPILER_PACKAGE + "CompiledFunctionType" -> "Function";
            default -> value == null || value.isBlank() ? "any" : value;
        };
    }

    private static String migratedClassName(String className) {
        return switch (className) {
            case LEGACY_EXPRESSION_PACKAGE + "CompiledBooleanValue" -> CompiledExpression.CompiledBoolLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledByteValue",
                 LEGACY_EXPRESSION_PACKAGE + "CompiledIntValue" -> CompiledExpression.CompiledIntLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledLongValue" -> CompiledExpression.CompiledLongLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledFloatValue" -> CompiledExpression.CompiledFloatLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledDoubleValue" -> CompiledExpression.CompiledDoubleLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledStringValue" -> CompiledExpression.CompiledStringLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledVariable" -> CompiledExpression.CompiledVariableExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledIfExpression" -> CompiledExpression.CompiledIfExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledInfixExpression",
                 LEGACY_EXPRESSION_PACKAGE + "CompiledPipeExpression" -> CompiledExpression.CompiledBinaryExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledFunctionCall",
                 LEGACY_EXPRESSION_PACKAGE + "CompiledFunctionInvoke" -> CompiledExpression.CompiledFunctionCallExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledFieldAccess" -> CompiledExpression.CompiledFieldAccessExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledIndexExpression",
                 LEGACY_EXPRESSION_PACKAGE + "CompiledSliceExpression" -> CompiledExpression.CompiledIndexExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledLambdaExpression" -> CompiledExpression.CompiledLambdaExpression.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledNewList" -> CompiledExpression.CompiledListLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledNewSet" -> CompiledExpression.CompiledSetLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledNewDict" -> CompiledExpression.CompiledDictLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledTupleExpression" -> CompiledExpression.CompiledTupleLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledNewData" -> CompiledExpression.CompiledDataLiteral.class.getName();
            case LEGACY_EXPRESSION_PACKAGE + "CompiledMatchExpression" -> CompiledExpression.CompiledMatchExpression.class.getName();
            default -> className.startsWith(LEGACY_EXPRESSION_PACKAGE)
                    ? CompiledExpression.CompiledUnsupportedExpression.class.getName()
                    : className;
        };
    }

    private static Json toJson(Object value, boolean includeType) {
        if (value == null) {
            return Json.JsonNull.INSTANCE;
        }
        if (value instanceof String string) {
            return new Json.JsonString(string);
        }
        if (value instanceof Boolean bool) {
            return new Json.JsonBool(bool);
        }
        if (value instanceof Byte || value instanceof Short || value instanceof Integer || value instanceof Long) {
            return new Json.JsonNumberLong(((Number) value).longValue());
        }
        if (value instanceof Float || value instanceof Double) {
            return new Json.JsonNumberDouble(((Number) value).doubleValue());
        }
        if (value instanceof Enum<?> enumValue) {
            return enumValue.getDeclaringClass().getEnumConstants().length == 1 && includeType
                    ? typeTaggedObject(enumValue.getClass(), Map.of())
                    : new Json.JsonString(enumValue.name());
        }
        if (value instanceof Optional<?> optional) {
            return optional.map(item -> toJson(item, true)).orElse(Json.JsonNull.INSTANCE);
        }
        if (value instanceof Map<?, ?> map) {
            return mapJson(map);
        }
        if (value instanceof java.util.Set<?> set) {
            return typedListJson(set);
        }
        if (value instanceof Iterable<?> iterable) {
            return typedListJson(iterable);
        }
        if (value instanceof Json json) {
            return json;
        }
        if (value.getClass().isRecord()) {
            return recordJson(value, includeType);
        }
        return new Json.JsonString(String.valueOf(value));
    }

    private static Json.JsonObject recordJson(Object value, boolean includeType) {
        var fields = new LinkedHashMap<String, Json>();
        if (includeType) {
            fields.put(CLASS_KEY, new Json.JsonString(value.getClass().getName()));
        }
        for (var component : value.getClass().getRecordComponents()) {
            fields.put(component.getName(), toJson(recordField(value, component), shouldTypeField(component)));
        }
        return new Json.JsonObject(fields);
    }

    private static boolean shouldTypeField(RecordComponent component) {
        var type = component.getType();
        return type == Object.class || type.isInterface() || Modifier.isAbstract(type.getModifiers());
    }

    private static Object recordField(Object value, RecordComponent component) {
        try {
            var accessor = component.getAccessor();
            accessor.setAccessible(true);
            return accessor.invoke(value);
        } catch (ReflectiveOperationException exception) {
            throw new IllegalArgumentException("Cannot read field " + component.getName(), exception);
        }
    }

    private static Json.JsonObject mapJson(Map<?, ?> value) {
        var fields = new LinkedHashMap<String, Json>();
        fields.put(CLASS_KEY, new Json.JsonString(TREE_MAP_CLASS));
        value.forEach((key, item) -> fields.put(String.valueOf(key), toJson(item, true)));
        return new Json.JsonObject(fields);
    }

    private static Json.JsonArray typedListJson(Iterable<?> value) {
        var items = new ArrayList<Json>();
        for (var item : value) {
            items.add(toJson(item, true));
        }
        return new Json.JsonArray(List.of(new Json.JsonString(LIST_CLASS), new Json.JsonArray(items)));
    }

    private static Json.JsonObject typeTaggedObject(Class<?> type, Map<String, Json> fields) {
        var result = new LinkedHashMap<String, Json>();
        result.put(CLASS_KEY, new Json.JsonString(type.getName()));
        result.putAll(fields);
        return new Json.JsonObject(result);
    }

    private static String stringValue(Json json) {
        if (json instanceof Json.JsonString string) {
            return string.value();
        }
        if (json instanceof Json.JsonNumberLong number) {
            return Long.toString(number.value());
        }
        if (json instanceof Json.JsonNumberDouble number) {
            return Double.toString(number.value());
        }
        if (json instanceof Json.JsonBool bool) {
            return Boolean.toString(bool.value());
        }
        if (json instanceof Json.JsonArray array) {
            if (!array.value().isEmpty() && array.value().getFirst() instanceof Json.JsonString string) {
                return typeName(string.value());
            }
            return plainValue(array).toString();
        }
        if (json instanceof Json.JsonObject object) {
            var value = object.value();
            var named = firstPresent(value, "name", "value", "text", CLASS_KEY);
            return named == null ? plainObject(value).toString() : stringValue(named);
        }
        throw new IllegalArgumentException("Expected JSON string, got " + json);
    }

    private static Class<?> loadClass(String className) {
        try {
            return Class.forName(className);
        } catch (ClassNotFoundException exception) {
            throw new IllegalArgumentException("Unknown linked JSON type `" + className + "`", exception);
        }
    }

    private static Type listElementType(Type declaredType) {
        if (declaredType instanceof ParameterizedType parameterizedType
            && parameterizedType.getActualTypeArguments().length == 1) {
            return parameterizedType.getActualTypeArguments()[0];
        }
        return Object.class;
    }

    private static Type mapValueType(Type declaredType) {
        if (declaredType instanceof ParameterizedType parameterizedType
            && parameterizedType.getActualTypeArguments().length == 2) {
            return parameterizedType.getActualTypeArguments()[1];
        }
        return Object.class;
    }

    private static Class<?> rawType(Type type) {
        if (type instanceof Class<?> clazz) {
            return clazz;
        }
        if (type instanceof ParameterizedType parameterizedType && parameterizedType.getRawType() instanceof Class<?> clazz) {
            return clazz;
        }
        return Object.class;
    }

    private static final class Parser {
        private final String source;
        private int position;

        private Parser(String source) {
            this.source = source;
        }

        private Json.JsonObject parseObject() {
            var value = parseValue();
            skipWhitespace();
            if (position != source.length()) {
                throw error("Expected end of JSON");
            }
            if (value instanceof Json.JsonObject object) {
                return object;
            }
            throw error("Expected JSON object");
        }

        private Json parseValue() {
            skipWhitespace();
            if (position >= source.length()) {
                throw error("Expected JSON value");
            }
            return switch (source.charAt(position)) {
                case '{' -> parseObjectValue();
                case '[' -> parseArrayValue();
                case '"' -> new Json.JsonString(parseString());
                case 't' -> parseTrue();
                case 'f' -> parseFalse();
                case 'n' -> parseNull();
                case '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> parseNumber();
                default -> throw error("Expected JSON value");
            };
        }

        private Json.JsonObject parseObjectValue() {
            expect('{');
            skipWhitespace();
            var result = new LinkedHashMap<String, Json>();
            if (consume('}')) {
                return new Json.JsonObject(result);
            }
            while (true) {
                skipWhitespace();
                if (position >= source.length() || source.charAt(position) != '"') {
                    throw error("Expected JSON object key");
                }
                var key = parseString();
                skipWhitespace();
                expect(':');
                result.put(key, parseValue());
                skipWhitespace();
                if (consume('}')) {
                    return new Json.JsonObject(result);
                }
                expect(',');
            }
        }

        private Json.JsonArray parseArrayValue() {
            expect('[');
            skipWhitespace();
            var result = new ArrayList<Json>();
            if (consume(']')) {
                return new Json.JsonArray(result);
            }
            while (true) {
                result.add(parseValue());
                skipWhitespace();
                if (consume(']')) {
                    return new Json.JsonArray(result);
                }
                expect(',');
            }
        }

        private String parseString() {
            expect('"');
            var result = new StringBuilder();
            while (position < source.length()) {
                var character = source.charAt(position++);
                if (character == '"') {
                    return result.toString();
                }
                if (character != '\\') {
                    result.append(character);
                    continue;
                }
                if (position >= source.length()) {
                    throw error("Expected JSON escape sequence");
                }
                var escaped = source.charAt(position++);
                switch (escaped) {
                    case '"' -> result.append('"');
                    case '\\' -> result.append('\\');
                    case '/' -> result.append('/');
                    case 'b' -> result.append('\b');
                    case 'f' -> result.append('\f');
                    case 'n' -> result.append('\n');
                    case 'r' -> result.append('\r');
                    case 't' -> result.append('\t');
                    case 'u' -> result.append(parseUnicodeEscape());
                    default -> throw error("Unsupported JSON escape sequence");
                }
            }
            throw error("Unterminated JSON string");
        }

        private char parseUnicodeEscape() {
            if (position + 4 > source.length()) {
                throw error("Incomplete JSON unicode escape");
            }
            var value = 0;
            for (var i = 0; i < 4; i++) {
                var digit = Character.digit(source.charAt(position++), 16);
                if (digit < 0) {
                    throw error("Invalid JSON unicode escape");
                }
                value = value * 16 + digit;
            }
            return (char) value;
        }

        private Json.JsonBool parseTrue() {
            expectLiteral("true");
            return new Json.JsonBool(true);
        }

        private Json.JsonBool parseFalse() {
            expectLiteral("false");
            return new Json.JsonBool(false);
        }

        private Json.JsonNull parseNull() {
            expectLiteral("null");
            return Json.JsonNull.INSTANCE;
        }

        private Json parseNumber() {
            var start = position;
            if (consume('-') && position >= source.length()) {
                throw error("Expected JSON number");
            }
            if (consume('0')) {
                if (position < source.length() && Character.isDigit(source.charAt(position))) {
                    throw error("Invalid JSON number");
                }
            } else {
                readDigits();
            }
            var floatingPoint = false;
            if (consume('.')) {
                floatingPoint = true;
                readDigits();
            }
            if (position < source.length() && (source.charAt(position) == 'e' || source.charAt(position) == 'E')) {
                floatingPoint = true;
                position++;
                if (position < source.length() && (source.charAt(position) == '+' || source.charAt(position) == '-')) {
                    position++;
                }
                readDigits();
            }
            var number = source.substring(start, position);
            try {
                if (floatingPoint) {
                    return new Json.JsonNumberDouble(Double.parseDouble(number));
                }
                return new Json.JsonNumberLong(Long.parseLong(number));
            } catch (NumberFormatException exception) {
                throw error("Invalid JSON number");
            }
        }

        private void readDigits() {
            var start = position;
            while (position < source.length() && Character.isDigit(source.charAt(position))) {
                position++;
            }
            if (start == position) {
                throw error("Expected JSON number digit");
            }
        }

        private void expect(char expected) {
            if (position >= source.length() || source.charAt(position) != expected) {
                throw error("Expected `" + expected + "`");
            }
            position++;
        }

        private boolean consume(char expected) {
            if (position < source.length() && source.charAt(position) == expected) {
                position++;
                return true;
            }
            return false;
        }

        private void expectLiteral(String literal) {
            if (!source.startsWith(literal, position)) {
                throw error("Expected `" + literal + "`");
            }
            position += literal.length();
        }

        private void skipWhitespace() {
            while (position < source.length()) {
                var character = source.charAt(position);
                if (character == ' ' || character == '\n' || character == '\r' || character == '\t') {
                    position++;
                } else {
                    return;
                }
            }
        }

        private IllegalArgumentException error(String message) {
            return new IllegalArgumentException(message + " at character " + position);
        }
    }
}
