package dev.capylang;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

public final class CapybaraToStringUtil {
    private CapybaraToStringUtil() {
    }

    public static String __capybaraToStringValue(Object value) {
        if (value == null) {
            return "null";
        }
        if (value instanceof String capybaraStringValue) {
            return "\"" + capybaraStringValue.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
        }
        if (value instanceof Enum<?> capybaraEnumValue) {
            return "INSTANCE".equals(capybaraEnumValue.name())
                    ? capybaraEnumValue.getDeclaringClass().getSimpleName()
                    : capybaraEnumValue.name();
        }
        if (value instanceof Map<?, ?> capybaraMapValue) {
            return capybaraMapValue.entrySet().stream()
                    .map(capybaraEntry -> String.valueOf(capybaraEntry.getKey()) + "=" + __capybaraToStringValue(capybaraEntry.getValue()))
                    .collect(Collectors.joining(",", "{", "}"));
        }
        if (value instanceof Collection<?> capybaraCollectionValue) {
            return capybaraCollectionValue.stream()
                    .map(CapybaraToStringUtil::__capybaraToStringValue)
                    .collect(Collectors.joining(",", "[", "]"));
        }
        if (value instanceof Map.Entry<?, ?> capybaraEntryValue) {
            return String.valueOf(capybaraEntryValue.getKey()) + "=" + __capybaraToStringValue(capybaraEntryValue.getValue());
        }
        return String.valueOf(value);
    }
}
