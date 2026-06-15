package dev.capylang.compiler;

public record CompiledNativeProviderBinding(
        String interfaceId,
        String qualifier,
        NativeProviderBackendBinding javaBinding,
        NativeProviderBackendBinding javascriptBinding,
        NativeProviderBackendBinding pythonBinding
) {
}
