package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import java.util.Arrays;
import java.util.Locale;

public enum NativeProviderBackend {
    JAVA("java"),
    JAVASCRIPT("javascript"),
    PYTHON("python");

    private final String jsonValue;

    NativeProviderBackend(String jsonValue) {
        this.jsonValue = jsonValue;
    }

    @JsonCreator
    public static NativeProviderBackend fromJson(String value) {
        if (value != null) {
            var normalized = value.toLowerCase(Locale.ROOT);
            for (var backend : values()) {
                if (backend.jsonValue.equals(normalized)) {
                    return backend;
                }
            }
        }
        throw new IllegalArgumentException(
                "UnsupportedBackend: Unknown native provider backend `" + value + "`. Supported values: " + supportedValues() + "."
        );
    }

    @JsonValue
    public String jsonValue() {
        return jsonValue;
    }

    private static String supportedValues() {
        return String.join(", ", Arrays.stream(values()).map(NativeProviderBackend::jsonValue).toList());
    }
}
