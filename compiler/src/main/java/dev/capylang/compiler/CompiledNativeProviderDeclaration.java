package dev.capylang.compiler;

public record CompiledNativeProviderDeclaration(
        String providerName,
        String sourceModulePath,
        String sourceModuleName,
        String targetTypeName,
        String interfaceId,
        String qualifier,
        String sourceFile
) {
}
