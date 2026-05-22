package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import java.util.Arrays;
import java.util.Locale;

public enum NativeProviderLifetime {
    SINGLETON("singleton"),
    FACTORY("factory");

    private final String jsonValue;

    NativeProviderLifetime(String jsonValue) {
        this.jsonValue = jsonValue;
    }

    @JsonCreator
    public static NativeProviderLifetime fromJson(String value) {
        if (value != null) {
            var normalized = value.toLowerCase(Locale.ROOT);
            for (var lifetime : values()) {
                if (lifetime.jsonValue.equals(normalized)) {
                    return lifetime;
                }
            }
        }
        throw new IllegalArgumentException(
                "Unknown native provider lifetime `" + value + "`. Supported values: " + supportedValues() + "."
        );
    }

    @JsonValue
    public String jsonValue() {
        return jsonValue;
    }

    private static String supportedValues() {
        return String.join(", ", Arrays.stream(values()).map(NativeProviderLifetime::jsonValue).toList());
    }
}
