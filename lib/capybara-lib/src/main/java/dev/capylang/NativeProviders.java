package dev.capylang;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public final class NativeProviders {
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

    public static <T> Provider singleton(
            String interfaceId,
            String qualifier,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return singleton(interfaceId, qualifier, null, null, type, factory);
    }

    public static <T> Provider singleton(
            String interfaceId,
            String qualifier,
            String providerSymbol,
            String backend,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return singleton(interfaceId, qualifier, providerSymbol, backend, null, type, factory);
    }

    public static <T> Provider singleton(
            String interfaceId,
            String qualifier,
            String providerSymbol,
            String backend,
            String sourceFile,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return new Provider(interfaceId, qualifier, providerSymbol, backend, sourceFile, type, factory, Lifetime.SINGLETON);
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
        return new Provider(interfaceId, qualifier, providerSymbol, backend, sourceFile, type, factory, Lifetime.FACTORY);
    }

    public <T> T resolve(String interfaceId, String qualifier, Class<T> type) {
        return resolve(interfaceId, qualifier, null, null, type);
    }

    public <T> T resolve(String interfaceId, String qualifier, String providerSymbol, String backend, Class<T> type) {
        return resolve(interfaceId, qualifier, providerSymbol, backend, null, type);
    }

    public <T> T resolve(String interfaceId, String qualifier, String providerSymbol, String backend, String sourceFile, Class<T> type) {
        var key = new Key(requireText(interfaceId, "interfaceId"), requireText(qualifier, "qualifier"));
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

    private static String providerSymbolSegment(String value) {
        return value == null || value.isBlank() ? "" : "provider symbol `" + value + "`, ";
    }

    private static String context(String interfaceId, String qualifier, String providerSymbol, String backend, String sourceFile) {
        return providerSymbolSegment(providerSymbol)
               + "interface `" + interfaceId + "` with qualifier `" + qualifier + "`"
               + (backend == null || backend.isBlank() ? "" : " for backend `" + backend + "`")
               + (sourceFile == null || sourceFile.isBlank() ? "" : " in source `" + sourceFile + "`");
    }

    private enum Lifetime {
        SINGLETON,
        FACTORY
    }

    private record Key(String interfaceId, String qualifier) {
    }

    public static final class Provider {
        private static final Object UNINITIALIZED = new Object();

        private final Key key;
        private final String providerSymbol;
        private final String backend;
        private final String sourceFile;
        private final Class<?> type;
        private final NativeProviderFactory<?> factory;
        private final Lifetime lifetime;
        private volatile Object singleton = UNINITIALIZED;

        private <T> Provider(
                String interfaceId,
                String qualifier,
                String providerSymbol,
                String backend,
                String sourceFile,
                Class<T> type,
                NativeProviderFactory<? extends T> factory,
                Lifetime lifetime
        ) {
            this.key = new Key(requireText(interfaceId, "interfaceId"), requireText(qualifier, "qualifier"));
            this.providerSymbol = providerSymbol;
            this.backend = backend;
            this.sourceFile = sourceFile;
            this.type = Objects.requireNonNull(type, "type");
            this.factory = Objects.requireNonNull(factory, "factory");
            this.lifetime = Objects.requireNonNull(lifetime, "lifetime");
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
            if (lifetime == Lifetime.FACTORY) {
                return create();
            }
            var value = singleton;
            if (value == UNINITIALIZED) {
                synchronized (this) {
                    value = singleton;
                    if (value == UNINITIALIZED) {
                        value = create();
                        singleton = value;
                    }
                }
            }
            return value;
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
