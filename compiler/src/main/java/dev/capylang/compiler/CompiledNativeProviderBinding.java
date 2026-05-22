package dev.capylang.compiler;

public record CompiledNativeProviderBinding(
        String interfaceId,
        String qualifier,
        NativeProviderLifetime lifetime,
        NativeProviderBackendBinding javaBinding,
        NativeProviderBackendBinding javascriptBinding,
        NativeProviderBackendBinding pythonBinding
) {
}
