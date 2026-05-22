package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

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
        providers = providers == null ? List.of() : List.copyOf(providers);
        if (providers.stream().anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("TypeMismatch: Native provider manifest cannot contain null providers.");
        }
        sourceFile = sourceFile == null || sourceFile.isBlank() ? null : sourceFile;
    }

    public static NativeProviderManifest empty() {
        return new NativeProviderManifest(List.of());
    }

    public NativeProviderManifest withSourceFile(String sourceFile) {
        return new NativeProviderManifest(providers, sourceFile);
    }

    @JsonIgnore
    public boolean isEmpty() {
        return providers.isEmpty();
    }
}
