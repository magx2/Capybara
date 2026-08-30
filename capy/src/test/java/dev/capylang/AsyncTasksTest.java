package dev.capylang;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class AsyncTasksTest {
    @Test
    void runsTasksConcurrentlyAndKeepsInputOrder() {
        var started = new CountDownLatch(2);
        var tasks = List.<Supplier<String>>of(
                () -> awaitOtherTask(started, "first"),
                () -> awaitOtherTask(started, "second")
        );

        assertThat(AsyncTasks.run(tasks)).containsExactly("first", "second");
    }

    @Test
    void rethrowsTaskRuntimeFailureWithoutCompletionWrapper() {
        var expected = new IllegalArgumentException("broken module");

        assertThatThrownBy(() -> AsyncTasks.run(List.of(
                () -> "valid",
                () -> {
                    throw expected;
                }
        ))).isSameAs(expected);
    }

    private static String awaitOtherTask(CountDownLatch started, String value) {
        started.countDown();
        try {
            assertThat(started.await(5, TimeUnit.SECONDS)).isTrue();
            return value;
        } catch (InterruptedException failure) {
            Thread.currentThread().interrupt();
            throw new AssertionError(failure);
        }
    }
}
