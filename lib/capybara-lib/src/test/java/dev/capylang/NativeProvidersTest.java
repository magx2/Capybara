package dev.capylang;

import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class NativeProvidersTest {
    @Test
    void providersCreateNewInstances() {
        var creations = new AtomicInteger();
        var providers = NativeProviders.of(NativeProviders.factory(
                "/test/Clock",
                "system",
                Clock.class,
                () -> new TestClock(creations.incrementAndGet())
        ));

        assertThat(providers.resolve("/test/Clock", "system", Clock.class).now()).isEqualTo(1);
        assertThat(providers.resolve("/test/Clock", "system", Clock.class).now()).isEqualTo(2);
        assertThat(creations).hasValue(2);
    }

    @Test
    void singletonProvidersReuseInstance() {
        var creations = new AtomicInteger();
        var providers = NativeProviders.of(NativeProviders.factory(
                "/test/Clock",
                "system",
                "system_clock",
                "java",
                "/Providers.cfun",
                "singleton",
                Clock.class,
                () -> new TestClock(creations.incrementAndGet())
        ));

        assertThat(providers.resolve("/test/Clock", "system", Clock.class).now()).isEqualTo(1);
        assertThat(providers.resolve("/test/Clock", "system", Clock.class).now()).isEqualTo(1);
        assertThat(creations).hasValue(1);
    }

    @Test
    void unsupportedLifetimesFailWithProviderContext() {
        assertThatThrownBy(() -> NativeProviders.factory(
                "/test/Clock",
                "system",
                "system_clock",
                "java",
                "/Providers.cfun",
                "request",
                Clock.class,
                () -> new TestClock(1)
        ))
                .isInstanceOf(NativeProviderException.class)
                .hasMessageContaining("UnsupportedBackend")
                .hasMessageContaining("system_clock")
                .hasMessageContaining("/test/Clock")
                .hasMessageContaining("system")
                .hasMessageContaining("java")
                .hasMessageContaining("/Providers.cfun")
                .hasMessageContaining("request");
    }

    @Test
    void missingProvidersFailWithProviderKey() {
        var providers = NativeProviders.of();

        assertThatThrownBy(() -> providers.resolve("/test/Clock", "system", Clock.class))
                .isInstanceOf(NativeProviderException.class)
                .hasMessageContaining("/test/Clock")
                .hasMessageContaining("system");
    }

    @Test
    @SuppressWarnings({"rawtypes", "unchecked"})
    void typeMismatchesFailWithExpectedAndActualTypes() {
        var providers = NativeProviders.of(NativeProviders.factory(
                "/test/Clock",
                "system",
                (Class) Clock.class,
                (NativeProviderFactory) Object::new
        ));

        assertThatThrownBy(() -> providers.resolve("/test/Clock", "system", Clock.class))
                .isInstanceOf(NativeProviderException.class)
                .hasMessageContaining(Object.class.getName())
                .hasMessageContaining(Clock.class.getName());
    }

    private interface Clock {
        int now();
    }

    private record TestClock(int now) implements Clock {
    }
}
