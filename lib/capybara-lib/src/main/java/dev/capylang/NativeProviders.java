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
                throw new NativeProviderException("Native provider table cannot contain null entries.");
            }
            var previous = table.putIfAbsent(provider.key(), provider);
            if (previous != null) {
                throw new NativeProviderException("Duplicate native provider for interface `"
                                                  + provider.interfaceId()
                                                  + "` with qualifier `" + provider.qualifier() + "`.");
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
        return new Provider(interfaceId, qualifier, type, factory, Lifetime.SINGLETON);
    }

    public static <T> Provider factory(
            String interfaceId,
            String qualifier,
            Class<T> type,
            NativeProviderFactory<? extends T> factory
    ) {
        return new Provider(interfaceId, qualifier, type, factory, Lifetime.FACTORY);
    }

    public <T> T resolve(String interfaceId, String qualifier, Class<T> type) {
        var key = new Key(requireText(interfaceId, "interfaceId"), requireText(qualifier, "qualifier"));
        Objects.requireNonNull(type, "type");
        var provider = providers.get(key);
        if (provider == null) {
            throw new NativeProviderException("No native provider registered for interface `"
                                              + interfaceId + "` with qualifier `" + qualifier + "`.");
        }
        if (!type.isAssignableFrom(provider.type())) {
            throw new NativeProviderException("Native provider for interface `" + interfaceId
                                              + "` with qualifier `" + qualifier
                                              + "` was registered as `" + provider.type().getName()
                                              + "` but was resolved as `" + type.getName() + "`.");
        }
        var value = provider.resolve();
        if (!type.isInstance(value)) {
            throw new NativeProviderException("Native provider for interface `" + interfaceId
                                              + "` with qualifier `" + qualifier
                                              + "` returned `" + value.getClass().getName()
                                              + "`, which is not assignable to `" + type.getName() + "`.");
        }
        return type.cast(value);
    }

    private static String requireText(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Native provider " + name + " is required.");
        }
        return value;
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
        private final Class<?> type;
        private final NativeProviderFactory<?> factory;
        private final Lifetime lifetime;
        private volatile Object singleton = UNINITIALIZED;

        private <T> Provider(
                String interfaceId,
                String qualifier,
                Class<T> type,
                NativeProviderFactory<? extends T> factory,
                Lifetime lifetime
        ) {
            this.key = new Key(requireText(interfaceId, "interfaceId"), requireText(qualifier, "qualifier"));
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
                throw new NativeProviderException("Native provider for interface `" + interfaceId()
                                                  + "` with qualifier `" + qualifier()
                                                  + "` failed during construction.", exception);
            }
            if (value == null) {
                throw new NativeProviderException("Native provider for interface `" + interfaceId()
                                                  + "` with qualifier `" + qualifier()
                                                  + "` returned null.");
            }
            if (!type.isInstance(value)) {
                throw new NativeProviderException("Native provider for interface `" + interfaceId()
                                                  + "` with qualifier `" + qualifier()
                                                  + "` returned `" + value.getClass().getName()
                                                  + "`, which is not assignable to `" + type.getName() + "`.");
            }
            return value;
        }
    }
}
