package dev.capylang;

import java.util.Map;
import java.util.Optional;

public final class OptionUtil {
    private OptionUtil() {
    }

    public static <T> Optional<T> toCapyOption(Optional<T> optional) {
        return optional;
    }

    @SuppressWarnings("unchecked")
    public static <T> Optional<T> toJavaOptional(Object option) {
        if (option instanceof Optional<?> optional) {
            return (Optional<T>) optional;
        }
        if (option instanceof Map<?, ?> map && "Some".equals(map.get("__type"))) {
            return Optional.ofNullable((T) map.get("value"));
        }
        return Optional.empty();
    }
}
