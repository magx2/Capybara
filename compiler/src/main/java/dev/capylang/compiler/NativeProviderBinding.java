package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public record NativeProviderBinding(
        @JsonProperty("interface") String interfaceId,
        @JsonProperty("qualifier") String qualifier,
        @JsonProperty("lifetime") NativeProviderLifetime lifetime,
        @JsonProperty("java") NativeProviderBackendBinding javaBinding,
        @JsonProperty("javascript") NativeProviderBackendBinding javascriptBinding,
        @JsonProperty("python") NativeProviderBackendBinding pythonBinding
) {
    @JsonCreator
    public NativeProviderBinding {
        interfaceId = requireText(interfaceId, "Native provider `interface` is required.");
        qualifier = requireText(qualifier, "Native provider `qualifier` is required.");
        if (lifetime == null) {
            throw new IllegalArgumentException("Native provider `lifetime` is required.");
        }
        validateBackend(interfaceId, NativeProviderBackend.JAVA, javaBinding);
        validateBackend(interfaceId, NativeProviderBackend.JAVASCRIPT, javascriptBinding);
        validateBackend(interfaceId, NativeProviderBackend.PYTHON, pythonBinding);
    }

    private static void validateBackend(
            String interfaceId,
            NativeProviderBackend backend,
            NativeProviderBackendBinding binding
    ) {
        if (binding == null) {
            return;
        }
        switch (backend) {
            case JAVA -> {
                requireText(binding.className(), "`java.className` is required for native provider `" + interfaceId + "`.");
                requireFactory(interfaceId, backend, binding.factory(), "constructor");
            }
            case JAVASCRIPT -> {
                requireText(binding.moduleName(), "`javascript.module` is required for native provider `" + interfaceId + "`.");
                requireText(binding.exportName(), "`javascript.export` is required for native provider `" + interfaceId + "`.");
                requireFactory(interfaceId, backend, binding.factory(), "new", "call");
            }
            case PYTHON -> {
                requireText(binding.moduleName(), "`python.module` is required for native provider `" + interfaceId + "`.");
                requireText(binding.className(), "`python.className` is required for native provider `" + interfaceId + "`.");
                requireFactory(interfaceId, backend, binding.factory(), "call");
            }
        }
    }

    private static void requireFactory(
            String interfaceId,
            NativeProviderBackend backend,
            String actual,
            String... allowed
    ) {
        for (var value : allowed) {
            if (value.equals(actual)) {
                return;
            }
        }
        throw new IllegalArgumentException(
                "Unsupported " + backend.jsonValue() + " native provider factory `" + actual
                + "` for `" + interfaceId + "`. Supported values: " + String.join(", ", allowed) + "."
        );
    }

    private static String requireText(String value, String message) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(message);
        }
        return value;
    }
}
