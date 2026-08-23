package dev.capylang;

@FunctionalInterface
public interface NativeProviderFactory<T> {
    T create();
}
