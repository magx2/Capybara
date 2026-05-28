package dev.capylang;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public final class NativeProviders {
    private static final String LIFETIME_FACTORY = "factory";
    private static final String LIFETIME_SINGLETON = "singleton";

    private final Map<Key, Provider> providers;

    private NativeProviders(Map<Key, Provider> providers) {
        this.providers = Map.copyOf(providers);
    }

    public static NativeProviders of(Provider... providers) {
        Objects.requireNonNull(providers, "providers");
        var table = new LinkedHashMap<Key, Provider>();
        for (var provider : providers) {
            if (provider == null) {
                throw new NativeProviderException("TypeMismatch: Native provider table cannot contain null entries.");
            }
            var previous = table.putIfAbsent(provider.key(), provider);
            if (previous != null) {
                throw new NativeProviderException("DuplicateProvider: Duplicate native provider for "
                                                  + provider.context() + ".");
            }
        }
        return new NativeProviders(table);
    }

    public static <T> Provider factory(
            String interfaceId,
            String qualifier,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return factory(interfaceId, qualifier, null, null, type, factory);
    }

    public static <T> Provider factory(
            String interfaceId,
            String qualifier,
            String providerSymbol,
            String backend,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return factory(interfaceId, qualifier, providerSymbol, backend, null, type, factory);
    }

    public static <T> Provider factory(
            String interfaceId,
            String qualifier,
            String providerSymbol,
            String backend,
            String sourceFile,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return factory(interfaceId, qualifier, providerSymbol, backend, sourceFile, LIFETIME_FACTORY, type, factory);
    }

    public static <T> Provider factory(
            String interfaceId,
            String qualifier,
            String providerSymbol,
            String backend,
            String sourceFile,
            String lifetime,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return new Provider(interfaceId, qualifier, providerSymbol, backend, sourceFile, lifetime, type, factory);
    }

    public <T> T resolve(String interfaceId, String qualifier, Class<T> type) {
        return resolve(interfaceId, qualifier, null, null, type);
    }

    public <T> T resolve(String interfaceId, String qualifier, String providerSymbol, String backend, Class<T> type) {
        return resolve(interfaceId, qualifier, providerSymbol, backend, null, type);
    }

    public <T> T resolve(String interfaceId, String qualifier, String providerSymbol, String backend, String sourceFile, Class<T> type) {
        var key = new Key(requireText(interfaceId, "interfaceId"), requireString(qualifier, "qualifier"));
        Objects.requireNonNull(type, "type");
        var context = context(interfaceId, qualifier, providerSymbol, backend, sourceFile);
        var provider = providers.get(key);
        if (provider == null) {
            throw new NativeProviderException("NotWired: No native provider registered for " + context + ".");
        }
        if (!type.isAssignableFrom(provider.type())) {
            throw new NativeProviderException("TypeMismatch: Native provider for " + context
                                              + " was registered as `" + provider.type().getName()
                                              + "` but was resolved as `" + type.getName() + "`.");
        }
        var value = provider.resolve();
        if (!type.isInstance(value)) {
            throw new NativeProviderException("TypeMismatch: Native provider for " + context
                                              + " returned `" + value.getClass().getName()
                                              + "`, which is not assignable to `" + type.getName() + "`.");
        }
        return type.cast(value);
    }

    private static String requireText(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("TypeMismatch: Native provider " + name + " is required.");
        }
        return value;
    }

    private static String requireString(String value, String name) {
        if (value == null) {
            throw new IllegalArgumentException("TypeMismatch: Native provider " + name + " is required.");
        }
        return value;
    }

    private static String requireLifetime(
            String value,
            String interfaceId,
            String qualifier,
            String providerSymbol,
            String backend,
            String sourceFile
    ) {
        var lifetime = value == null || value.isBlank() ? LIFETIME_FACTORY : value;
        if (LIFETIME_FACTORY.equals(lifetime) || LIFETIME_SINGLETON.equals(lifetime)) {
            return lifetime;
        }
        throw new NativeProviderException("UnsupportedBackend: Native provider for "
                                          + context(interfaceId, qualifier, providerSymbol, backend, sourceFile)
                                          + " has unsupported lifetime `" + lifetime
                                          + "`. Supported values: factory, singleton.");
    }

    private static String providerSymbolSegment(String value) {
        return value == null || value.isBlank() ? "" : "provider symbol `" + value + "`, ";
    }

    private static String context(String interfaceId, String qualifier, String providerSymbol, String backend, String sourceFile) {
        return providerSymbolSegment(providerSymbol)
               + "interface `" + interfaceId + "` with qualifier `" + qualifier + "`"
               + (backend == null || backend.isBlank() ? "" : " for backend `" + backend + "`")
               + (sourceFile == null || sourceFile.isBlank() ? "" : " in source `" + sourceFile + "`");
    }

    private record Key(String interfaceId, String qualifier) {
    }

    public static final class Provider {
        private final Key key;
        private final String providerSymbol;
        private final String backend;
        private final String sourceFile;
        private final String lifetime;
        private final Class<?> type;
        private final NativeProviderFactory<?> factory;
        private volatile Object singletonValue;

        private <T> Provider(
                String interfaceId,
                String qualifier,
                String providerSymbol,
                String backend,
                String sourceFile,
                String lifetime,
                Class<T> type,
                NativeProviderFactory<? extends T> factory
        ) {
            this.key = new Key(requireText(interfaceId, "interfaceId"), requireString(qualifier, "qualifier"));
            this.providerSymbol = providerSymbol;
            this.backend = backend;
            this.sourceFile = sourceFile;
            this.lifetime = requireLifetime(lifetime, interfaceId, qualifier, providerSymbol, backend, sourceFile);
            this.type = Objects.requireNonNull(type, "type");
            this.factory = Objects.requireNonNull(factory, "factory");
        }

        private String interfaceId() {
            return key.interfaceId();
        }

        private String qualifier() {
            return key.qualifier();
        }

        private Key key() {
            return key;
        }

        private Class<?> type() {
            return type;
        }

        private String context() {
            return NativeProviders.context(interfaceId(), qualifier(), providerSymbol, backend, sourceFile);
        }

        private Object resolve() {
            if (LIFETIME_SINGLETON.equals(lifetime)) {
                var existing = singletonValue;
                if (existing != null) {
                    return existing;
                }
                synchronized (this) {
                    if (singletonValue == null) {
                        singletonValue = create();
                    }
                    return singletonValue;
                }
            }
            return create();
        }

        private Object create() {
            final Object value;
            try {
                value = factory.create();
            } catch (NativeProviderException exception) {
                throw exception;
            } catch (RuntimeException exception) {
                throw new NativeProviderException("InvocationFailure: Native provider for " + context()
                                                  + " failed during construction.", exception);
            }
            if (value == null) {
                throw new NativeProviderException("TypeMismatch: Native provider for " + context()
                                                  + " returned null.");
            }
            if (!type.isInstance(value)) {
                throw new NativeProviderException("TypeMismatch: Native provider for " + context()
                                                  + " returned `" + value.getClass().getName()
                                                  + "`, which is not assignable to `" + type.getName() + "`.");
            }
            return value;
        }
    }
}
