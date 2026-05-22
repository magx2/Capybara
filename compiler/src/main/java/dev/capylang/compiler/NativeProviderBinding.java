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
    @JsonCreator
    public static NativeProviderBinding fromJson(
            @JsonProperty("interface") String interfaceId,
            @JsonProperty("qualifier") String qualifier,
            @JsonProperty("lifetime") String lifetime,
            @JsonProperty("java") NativeProviderBackendBinding javaBinding,
            @JsonProperty("javascript") NativeProviderBackendBinding javascriptBinding,
            @JsonProperty("python") NativeProviderBackendBinding pythonBinding
    ) {
        var binding = new NativeProviderBinding(
                interfaceId,
                qualifier,
                lifetime,
                javaBinding,
                javascriptBinding,
                pythonBinding
        );
        NativeProviderLifetime.fromJson(binding.lifetime());
        validateBackendBinding(NativeProviderBackend.JAVA, binding.javaBinding());
        validateBackendBinding(NativeProviderBackend.JAVASCRIPT, binding.javascriptBinding());
        validateBackendBinding(NativeProviderBackend.PYTHON, binding.pythonBinding());
        return binding;
    }

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

    public NativeProviderBinding {
        interfaceId = requireText(interfaceId, "TypeMismatch: Native provider `interface` is required.");
        qualifier = requireText(qualifier, "TypeMismatch: Native provider `qualifier` is required.");
        lifetime = requireText(lifetime, "UnsupportedBackend: Native provider `lifetime` is required.");
    }

    private static String requireText(String value, String message) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(message);
        }
        return value;
    }

    private static void validateBackendBinding(NativeProviderBackend backend, NativeProviderBackendBinding binding) {
        if (binding == null) {
            return;
        }
        switch (backend) {
            case JAVA -> {
                requireText(binding.className(), "UnsupportedBackend: Native provider backend `java` requires manifest field `java.className`.");
                requireFactory(backend, binding.factory(), "constructor");
            }
            case JAVASCRIPT -> {
                requireText(binding.moduleName(), "UnsupportedBackend: Native provider backend `javascript` requires manifest field `javascript.module`.");
                requireText(binding.exportName(), "UnsupportedBackend: Native provider backend `javascript` requires manifest field `javascript.export`.");
                requireFactory(backend, binding.factory(), "new", "call");
            }
            case PYTHON -> {
                requireText(binding.moduleName(), "UnsupportedBackend: Native provider backend `python` requires manifest field `python.module`.");
                requireText(binding.className(), "UnsupportedBackend: Native provider backend `python` requires manifest field `python.className`.");
                requireFactory(backend, binding.factory(), "call");
            }
        }
    }

    private static void requireFactory(NativeProviderBackend backend, String actual, String... supported) {
        for (var candidate : supported) {
            if (candidate.equals(actual)) {
                return;
            }
        }
        throw new IllegalArgumentException(
                "UnsupportedBackend: Native provider backend `" + backend.jsonValue() + "` manifest field `"
                + backend.jsonValue() + ".factory` has unsupported value `" + actual
                + "`. Supported values: " + String.join(", ", supported) + "."
        );
    }
}
