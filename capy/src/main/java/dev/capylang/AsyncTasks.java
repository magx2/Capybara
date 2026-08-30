package dev.capylang;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.function.Supplier;

/** Runs independent compiler work concurrently while preserving input order. */
public final class AsyncTasks {
    private static final int PARALLELISM = Math.max(2, Runtime.getRuntime().availableProcessors());
    private static final ExecutorService EXECUTOR = Executors.newFixedThreadPool(PARALLELISM, runnable -> {
        var thread = new Thread(runnable, "capy-compiler-async");
        thread.setDaemon(true);
        return thread;
    });

    private AsyncTasks() {
    }

    public static <T, R> List<R> map(List<T> values, Function<? super T, ? extends R> mapper) {
        var tasks = values.stream()
                .map(value -> (Supplier<R>) () -> mapper.apply(value))
                .toList();
        return run(tasks);
    }

    public static <T> List<T> run(List<Supplier<T>> tasks) {
        if (tasks.size() < 2) {
            return tasks.stream().map(Supplier::get).toList();
        }
        var futures = tasks.stream()
                .map(task -> CompletableFuture.supplyAsync(task, EXECUTOR))
                .toList();
        return futures.stream().map(AsyncTasks::join).toList();
    }

    private static <T> T join(CompletableFuture<? extends T> future) {
        try {
            return future.join();
        } catch (CompletionException failure) {
            if (failure.getCause() instanceof RuntimeException runtimeFailure) {
                throw runtimeFailure;
            }
            if (failure.getCause() instanceof Error error) {
                throw error;
            }
            throw failure;
        }
    }
}
