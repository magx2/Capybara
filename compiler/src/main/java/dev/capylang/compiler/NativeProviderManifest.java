package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public record NativeProviderManifest(
        @JsonProperty("providers") List<NativeProviderBinding> providers,
        @JsonIgnore String sourceFile
) {
    @JsonCreator
    public NativeProviderManifest(@JsonProperty("providers") List<NativeProviderBinding> providers) {
        this(providers, null);
    }

    public NativeProviderManifest {
        if (providers == null) {
            providers = List.of();
        }
        if (providers.stream().anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("TypeMismatch: Native provider manifest cannot contain null providers.");
        }
        providers = providers.stream()
                .sorted(Comparator.comparing(NativeProviderBinding::interfaceId)
                        .thenComparing(NativeProviderBinding::qualifier)
                        .thenComparing(binding -> backendSortKey(binding.javaBinding()))
                        .thenComparing(binding -> backendSortKey(binding.javascriptBinding()))
                        .thenComparing(binding -> backendSortKey(binding.pythonBinding())))
                .toList();
        sourceFile = sourceFile == null || sourceFile.isBlank() ? null : sourceFile;
    }

    public static NativeProviderManifest empty() {
        return new NativeProviderManifest(List.of());
    }

    public NativeProviderManifest withSourceFile(String sourceFile) {
        return new NativeProviderManifest(providers, sourceFile);
    }

    private static String backendSortKey(NativeProviderBackendBinding binding) {
        if (binding == null) {
            return "";
        }
        return String.join(
                "\u0000",
                Objects.toString(binding.className(), ""),
                Objects.toString(binding.moduleName(), ""),
                Objects.toString(binding.exportName(), ""),
                Objects.toString(binding.factory(), "")
        );
    }

    @JsonIgnore
    public boolean isEmpty() {
        return providers.isEmpty();
    }
}
