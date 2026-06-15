package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.List;

public record NativeProviderCatalog(
        List<CompiledNativeProviderDeclaration> declarations,
        List<CompiledNativeProviderBinding> bindings
) {
    public NativeProviderCatalog {
        declarations = declarations == null ? List.of() : List.copyOf(declarations);
        bindings = bindings == null ? List.of() : List.copyOf(bindings);
    }

    public static NativeProviderCatalog empty() {
        return new NativeProviderCatalog(List.of(), List.of());
    }

    @JsonIgnore
    public boolean isEmpty() {
        return declarations.isEmpty() && bindings.isEmpty();
    }
}
