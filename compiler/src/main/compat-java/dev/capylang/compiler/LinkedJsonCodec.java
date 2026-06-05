package dev.capylang.compiler;

import capy.serialization.Json;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.RecordComponent;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

/**
 * Linked artifact JSON codec backed by capy/serialization/Json.
 */
public final class LinkedJsonCodec {
    private static final String CLASS_KEY = "@class";
    private static final String TREE_MAP_CLASS = "java.util.TreeMap";
    private static final String LIST_CLASS = "java.util.ImmutableCollections$ListN";

    private LinkedJsonCodec() {
    }

    public static <T> T read(String json, Class<T> targetType) {
        return targetType.cast(fromJson(parseObject(json), targetType, targetType));
    }

    public static Object readPlain(String json) {
        return plainValue(parseObject(json));
    }

    public static String write(Object value) {
        return spaceObjectColons(Json.stringify(toJson(value, false)));
    }

    private static Json.JsonObject parseObject(String json) {
        return new Parser(json).parseObject();
    }

    private static Object fromJson(Json json, Type declaredType, Class<?> rawDeclaredType) {
        if (json == null) {
            return optionalOrNull(rawDeclaredType);
        }
        if (json instanceof Json.JsonNull) {
            return optionalOrNull(rawDeclaredType);
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
            return fromJsonArray(array.value(), declaredType, rawDeclaredType);
        }
        if (json instanceof Json.JsonValueArray array) {
            return fromJson(array.value(), declaredType, rawDeclaredType);
        }
        if (json instanceof Json.JsonValueObject object) {
            return fromJson(object.value(), declaredType, rawDeclaredType);
        }
        if (json instanceof Json.JsonObject object) {
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
        if (isTypedCollection(values)) {
            var payload = (Json.JsonArray) values.get(1);
            return payload.value().stream()
                    .map(value -> fromJson(value, listElementType(declaredType), rawType(listElementType(declaredType))))
                    .toList();
        }
        return values.stream()
                .map(value -> fromJson(value, listElementType(declaredType), rawType(listElementType(declaredType))))
                .toList();
    }

    private static boolean isTypedCollection(List<Json> values) {
        return values.size() == 2
               && values.get(0) instanceof Json.JsonString
               && values.get(1) instanceof Json.JsonArray;
    }

    @SuppressWarnings("unchecked")
    private static Object fromJsonObject(Map<String, Json> value, Type declaredType, Class<?> rawDeclaredType) {
        if (value.containsKey(CLASS_KEY)) {
            var className = stringValue(value.get(CLASS_KEY));
            if (TREE_MAP_CLASS.equals(className) || Map.class.isAssignableFrom(rawDeclaredType)) {
                return typedMap(value, declaredType);
            }
            rawDeclaredType = loadClass(className);
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
                arguments[i] = fromJson(
                        value.get(component.getName()),
                        component.getGenericType(),
                        component.getType()
                );
            }
            var constructor = targetType.getDeclaredConstructor(parameterTypes);
            constructor.setAccessible(true);
            return constructor.newInstance(arguments);
        } catch (ReflectiveOperationException e) {
            throw new IllegalArgumentException("Cannot instantiate " + targetType.getName(), e);
        }
    }

    private static Object enumSingleton(Class<?> targetType) {
        var constants = targetType.getEnumConstants();
        if (constants.length == 1) {
            return constants[0];
        }
        throw new IllegalArgumentException("Cannot decode enum object as " + targetType.getName());
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
        if (value instanceof Iterable<?> iterable) {
            return typedListJson(iterable);
        }
        if (value instanceof Json json) {
            return json;
        }
        if (value.getClass().isRecord()) {
            return recordJson(value, includeType);
        }
        return primitiveFallback(value);
    }

    private static Json primitiveFallback(Object value) {
        return new Json.JsonString(String.valueOf(value));
    }

    private static String spaceObjectColons(String compactJson) {
        var result = new StringBuilder(compactJson.length());
        var inString = false;
        var escaped = false;
        for (var i = 0; i < compactJson.length(); i++) {
            var ch = compactJson.charAt(i);
            if (inString) {
                result.append(ch);
                if (escaped) {
                    escaped = false;
                } else if (ch == '\\') {
                    escaped = true;
                } else if (ch == '"') {
                    inString = false;
                }
                continue;
            }
            if (ch == '"') {
                inString = true;
                result.append(ch);
            } else if (ch == ':') {
                result.append(" : ");
            } else {
                result.append(ch);
            }
        }
        return result.toString();
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
        return type == Object.class || type.isInterface() || java.lang.reflect.Modifier.isAbstract(type.getModifiers());
    }

    private static Object recordField(Object value, RecordComponent component) {
        try {
            var accessor = component.getAccessor();
            accessor.setAccessible(true);
            return accessor.invoke(value);
        } catch (ReflectiveOperationException e) {
            throw new IllegalArgumentException("Cannot read field " + component.getName(), e);
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
        throw new IllegalArgumentException("Expected JSON string, got " + json);
    }

    private static Class<?> loadClass(String className) {
        try {
            return Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Unknown linked JSON type `" + className + "`", e);
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
                var ch = source.charAt(position++);
                if (ch == '"') {
                    return result.toString();
                }
                if (ch != '\\') {
                    result.append(ch);
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
            if (consume('-')) {
                if (position >= source.length()) {
                    throw error("Expected JSON number");
                }
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
            } catch (NumberFormatException e) {
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
                var ch = source.charAt(position);
                if (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') {
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
