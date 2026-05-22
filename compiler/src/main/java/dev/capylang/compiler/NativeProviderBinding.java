package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public record NativeProviderBinding(
        @JsonProperty("interface") String interfaceId,
        @JsonProperty("qualifier") String qualifier,
        @JsonProperty("lifetime") String lifetime,
        @JsonProperty("java") NativeProviderBackendBinding javaBinding,
        @JsonProperty("javascript") NativeProviderBackendBinding javascriptBinding,
        @JsonProperty("python") NativeProviderBackendBinding pythonBinding
) {
    public NativeProviderBinding(
            String interfaceId,
            String qualifier,
            NativeProviderLifetime lifetime,
            NativeProviderBackendBinding javaBinding,
            NativeProviderBackendBinding javascriptBinding,
            NativeProviderBackendBinding pythonBinding
    ) {
        this(
                interfaceId,
                qualifier,
                lifetime == null ? null : lifetime.jsonValue(),
                javaBinding,
                javascriptBinding,
                pythonBinding
        );
    }

    @JsonCreator
    public NativeProviderBinding {
        interfaceId = requireText(interfaceId, "Native provider `interface` is required.");
        qualifier = requireText(qualifier, "Native provider `qualifier` is required.");
        lifetime = requireText(lifetime, "Native provider `lifetime` is required.");
    }

    private static String requireText(String value, String message) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(message);
        }
        return value;
    }
}
