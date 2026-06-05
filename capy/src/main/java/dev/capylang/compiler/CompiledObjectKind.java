package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

import java.util.Arrays;
import java.util.Locale;

public enum CompiledObjectKind {
    CLASS("class"),
    ABSTRACT_CLASS("abstract class"),
    TRAIT("trait"),
    INTERFACE("interface");

    private final String jsonValue;

    CompiledObjectKind(String jsonValue) {
        this.jsonValue = jsonValue;
    }

    @JsonCreator
    public static CompiledObjectKind fromJson(String value) {
        if (value != null) {
            var normalized = value.toLowerCase(Locale.ROOT);
            for (var kind : values()) {
                if (kind.jsonValue.equals(normalized) || kind.name().equalsIgnoreCase(value)) {
                    return kind;
                }
            }
        }
        throw new IllegalArgumentException(
                "Unknown compiled object kind `" + value + "`. Supported values: " + supportedValues() + "."
        );
    }

    @JsonValue
    public String jsonValue() {
        return jsonValue;
    }

    public String displayName() {
        return jsonValue;
    }

    private static String supportedValues() {
        return String.join(", ", Arrays.stream(values()).map(CompiledObjectKind::jsonValue).toList());
    }
}
