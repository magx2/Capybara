package dev.capylang;

import org.junit.jupiter.api.Test;

import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class NativeProvidersTest {
    @Test
    void singletonProvidersAreResolvedOnceAcrossThreads() throws Exception {
        var creations = new AtomicInteger();
        var providers = NativeProviders.of(NativeProviders.singleton(
                "/test/Clock",
                "system",
                Clock.class,
                () -> new TestClock(creations.incrementAndGet())
        ));

        var executor = Executors.newFixedThreadPool(8);
        try {
            var tasks = IntStream.range(0, 32)
                    .<java.util.concurrent.Callable<Clock>>mapToObj(ignored -> () -> providers.resolve("/test/Clock", "system", Clock.class))
                    .toList();
            var resolved = executor.invokeAll(tasks).stream()
                    .map(future -> {
                        try {
                            return future.get();
                        } catch (Exception exception) {
                            throw new RuntimeException(exception);
                        }
                    })
                    .toList();

            assertThat(resolved).allSatisfy(clock -> assertThat(clock.now()).isEqualTo(1));
            assertThat(creations).hasValue(1);
        } finally {
            executor.shutdownNow();
            assertThat(executor.awaitTermination(5, TimeUnit.SECONDS)).isTrue();
        }
    }

    @Test
    void factoryProvidersCreateNewInstances() {
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
