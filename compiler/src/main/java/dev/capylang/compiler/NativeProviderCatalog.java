package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public record NativeProviderCatalog(
        List<CompiledNativeProviderDeclaration> declarations,
        List<CompiledNativeProviderBinding> bindings
) {
    public NativeProviderCatalog {
        declarations = declarations == null ? List.of() : declarations.stream()
                .sorted(Comparator.comparing(CompiledNativeProviderDeclaration::interfaceId)
                        .thenComparing(CompiledNativeProviderDeclaration::qualifier)
                        .thenComparing(CompiledNativeProviderDeclaration::sourceModulePath)
                        .thenComparing(CompiledNativeProviderDeclaration::sourceModuleName)
                        .thenComparing(CompiledNativeProviderDeclaration::providerName)
                        .thenComparing(CompiledNativeProviderDeclaration::targetTypeName)
                        .thenComparing(declaration -> Objects.toString(declaration.sourceFile(), "")))
                .toList();
        bindings = bindings == null ? List.of() : bindings.stream()
                .sorted(Comparator.comparing(CompiledNativeProviderBinding::interfaceId)
                        .thenComparing(CompiledNativeProviderBinding::qualifier)
                        .thenComparing(binding -> backendSortKey(binding.javaBinding()))
                        .thenComparing(binding -> backendSortKey(binding.javascriptBinding()))
                        .thenComparing(binding -> backendSortKey(binding.pythonBinding())))
                .toList();
    }

    public static NativeProviderCatalog empty() {
        return new NativeProviderCatalog(List.of(), List.of());
    }

    @JsonIgnore
    public boolean isEmpty() {
        return declarations.isEmpty() && bindings.isEmpty();
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
}
