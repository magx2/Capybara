package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Objects;

public record NativeProviderManifest(
        @JsonProperty("providers") List<NativeProviderBinding> providers
) {
    @JsonCreator
    public NativeProviderManifest {
        providers = providers == null ? List.of() : List.copyOf(providers);
        if (providers.stream().anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("Native provider manifest cannot contain null providers.");
        }
    }

    public static NativeProviderManifest empty() {
        return new NativeProviderManifest(List.of());
    }

    @JsonIgnore
    public boolean isEmpty() {
        return providers.isEmpty();
    }
}
