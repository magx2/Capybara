package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public record NativeProviderBinding(
        @JsonProperty("interface") String interfaceId,
        @JsonProperty("qualifier") String qualifier,
        @JsonProperty("java") NativeProviderBackendBinding javaBinding,
        @JsonProperty("javascript") NativeProviderBackendBinding javascriptBinding,
        @JsonProperty("python") NativeProviderBackendBinding pythonBinding
) {
    @JsonCreator
    public static NativeProviderBinding fromJson(
            @JsonProperty("interface") String interfaceId,
            @JsonProperty("qualifier") String qualifier,
            @JsonProperty("java") NativeProviderBackendBinding javaBinding,
            @JsonProperty("javascript") NativeProviderBackendBinding javascriptBinding,
            @JsonProperty("python") NativeProviderBackendBinding pythonBinding
    ) {
        var binding = new NativeProviderBinding(
                interfaceId,
                qualifier,
                javaBinding,
                javascriptBinding,
                pythonBinding
        );
        validateBackendBinding(NativeProviderBackend.JAVA, binding.javaBinding());
        validateBackendBinding(NativeProviderBackend.JAVASCRIPT, binding.javascriptBinding());
        validateBackendBinding(NativeProviderBackend.PYTHON, binding.pythonBinding());
        return binding;
    }

    public NativeProviderBinding {
        interfaceId = requireText(interfaceId, "TypeMismatch: Native provider `interface` is required.");
        qualifier = qualifier == null ? "" : qualifier;
        javaBinding = withDefaultFactory(NativeProviderBackend.JAVA, javaBinding);
        javascriptBinding = withDefaultFactory(NativeProviderBackend.JAVASCRIPT, javascriptBinding);
        pythonBinding = withDefaultFactory(NativeProviderBackend.PYTHON, pythonBinding);
    }

    private static String requireText(String value, String message) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(message);
        }
        return value;
    }

    private static NativeProviderBackendBinding withDefaultFactory(NativeProviderBackend backend, NativeProviderBackendBinding binding) {
        if (binding == null || binding.factory() != null) {
            return binding;
        }
        return new NativeProviderBackendBinding(
                binding.className(),
                binding.moduleName(),
                binding.exportName(),
                defaultFactory(backend)
        );
    }

    private static String defaultFactory(NativeProviderBackend backend) {
        return switch (backend) {
            case JAVA -> "constructor";
            case JAVASCRIPT -> "new";
            case PYTHON -> "call";
        };
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
