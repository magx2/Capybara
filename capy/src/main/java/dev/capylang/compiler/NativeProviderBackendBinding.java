package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public record NativeProviderBackendBinding(
        @JsonProperty("className") String className,
        @JsonProperty("module") String moduleName,
        @JsonProperty("export") String exportName,
        @JsonProperty("factory") String factory
) {
    @JsonCreator
    public NativeProviderBackendBinding {
        className = blankToNull(className);
        moduleName = blankToNull(moduleName);
        exportName = blankToNull(exportName);
        factory = blankToNull(factory);
    }

    private static String blankToNull(String value) {
        return value == null || value.isBlank() ? null : value;
    }
}
