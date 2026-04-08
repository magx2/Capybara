package dev.capylang;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

public final class CapybaraToStringUtil {
    private CapybaraToStringUtil() {
    }

    public static String toStringValue(Object value) {
        if (value == null) {
            return "null";
        }
        if (value instanceof String stringValue) {
            return "\"" + stringValue.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
        }
        if (value instanceof Enum<?> enumValue) {
            return "INSTANCE".equals(enumValue.name())
                    ? enumValue.getDeclaringClass().getSimpleName()
                    : enumValue.name();
        }
        if (value instanceof Map<?, ?> mapValue) {
            return mapValue.entrySet().stream()
                    .map(entry -> String.valueOf(entry.getKey()) + "=" + toStringValue(entry.getValue()))
                    .collect(Collectors.joining(",", "{", "}"));
        }
        if (value instanceof Collection<?> collectionValue) {
            return collectionValue.stream()
                    .map(CapybaraToStringUtil::toStringValue)
                    .collect(Collectors.joining(",", "[", "]"));
        }
        if (value instanceof Map.Entry<?, ?> entryValue) {
            return String.valueOf(entryValue.getKey()) + "=" + toStringValue(entryValue.getValue());
        }
        return String.valueOf(value);
    }
}
