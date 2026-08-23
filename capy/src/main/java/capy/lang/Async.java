package capy.lang;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Function;

/** Java runtime support for the Capybara Async standard-library type. */
public final class Async<T> {
    private static final ExecutorService EXECUTOR = Executors.newCachedThreadPool(runnable -> {
        var thread = new Thread(runnable, "capy-async");
        thread.setDaemon(true);
        return thread;
    });

    private static final class ResultFailure extends RuntimeException {
        private final Object error;

        private ResultFailure(Object error) {
            super("Async.flat_map returned Error");
            this.error = error;
        }
    }

    private final CompletableFuture<T> future;

    private Async(CompletableFuture<T> future) {
        this.future = future;
    }

    public static <T> Async<T> start(Effect<T> effect) {
        return new Async<>(CompletableFuture.supplyAsync(effect::unsafeRun, EXECUTOR));
    }

    public static <T> Effect<List<Object>> all(List<Async<T>> tasks) {
        return Effect.delay(() -> tasks.stream().map(Async::joinedResult).toList());
    }

    public static <T> Effect<Object> any(List<Async<T>> tasks) {
        return Effect.delay(() -> {
            if (tasks.isEmpty()) {
                return error("capy.lang.async.empty", "Async.any requires at least one task");
            }
            var futures = tasks.stream().map(task -> task.future).toArray(CompletableFuture[]::new);
            try {
                CompletableFuture.anyOf(futures).handle((value, failure) -> null).get();
            } catch (InterruptedException failure) {
                Thread.currentThread().interrupt();
                return error("capy.lang.async.failed", safeMessage(failure), failure);
            } catch (ExecutionException failure) {
                var cause = unwrap(failure);
                return error("capy.lang.async.failed", safeMessage(cause), cause);
            }
            return tasks.stream()
                    .filter(task -> task.future.isDone())
                    .findFirst()
                    .orElse(tasks.getFirst())
                    .joinedResult();
        });
    }

    public <Y> Async<Y> map(Function<? super T, ? extends Y> mapper) {
        return new Async<>(future.thenApplyAsync(mapper, EXECUTOR));
    }

    @SuppressWarnings("unchecked")
    public <Y> Async<Y> flatMap(Function<? super T, ?> mapper) {
        return new Async<>(future.thenApplyAsync(value -> {
            var result = mapper.apply(value);
            if (isError(result)) {
                throw new ResultFailure(result);
            }
            if (isSuccess(result)) {
                return (Y) successValue(result);
            }
            throw new ResultFailure(error(
                    "capy.lang.argument.invalid",
                    "Async.flat_map expected Result value"
            ));
        }, EXECUTOR));
    }

    public Effect<Object> join() {
        return Effect.delay(this::joinedResult);
    }

    private Object joinedResult() {
        try {
            return success(future.get());
        } catch (InterruptedException failure) {
            Thread.currentThread().interrupt();
            return error("capy.lang.async.failed", safeMessage(failure), failure);
        } catch (ExecutionException failure) {
            var cause = unwrap(failure);
            if (cause instanceof ResultFailure resultFailure) {
                return resultFailure.error;
            }
            return error("capy.lang.async.failed", safeMessage(cause), cause);
        }
    }

    private static Object success(Object value) {
        var result = new LinkedHashMap<java.lang.String, Object>();
        result.put("__type", "Success");
        result.put("value", value);
        return result;
    }

    private static boolean isSuccess(Object value) {
        return resultType(value, "Success");
    }

    private static boolean isError(Object value) {
        return resultType(value, "Error");
    }

    private static Object successValue(Object value) {
        return value instanceof Map<?, ?> map ? map.get("value") : null;
    }

    private static boolean resultType(Object value, java.lang.String expected) {
        if (!(value instanceof Map<?, ?> map) || map.get("__type") == null) {
            return false;
        }
        var actual = java.lang.String.valueOf(map.get("__type"));
        return actual.equals(expected) || actual.endsWith("." + expected);
    }

    private static Object error(java.lang.String kind, java.lang.String message) {
        return errorFull(kind, message, List.of(), Optional.empty());
    }

    private static Object error(java.lang.String kind, java.lang.String message, Throwable failure) {
        return errorFull(kind, message, stackTrace(failure), Optional.of(rawStack(failure)));
    }

    private static Object errorFull(
            java.lang.String kind,
            java.lang.String message,
            List<Object> stackTrace,
            Optional<java.lang.String> rawStack
    ) {
        return Map.ofEntries(
                Map.entry("__type", "Error"),
                Map.entry("kind", kind),
                Map.entry("message", message),
                Map.entry("details", Optional.empty()),
                Map.entry("location", Optional.empty()),
                Map.entry("stack_trace", stackTrace),
                Map.entry("raw_stack", rawStack),
                Map.entry("cause", Optional.empty()),
                Map.entry("suppressed", List.of())
        );
    }

    private static List<Object> stackTrace(Throwable failure) {
        return java.util.Arrays.stream(failure.getStackTrace())
                .map(frame -> (Object) Map.ofEntries(
                        Map.entry("__type", "StackFrame"),
                        Map.entry("backend", "java"),
                        Map.entry("module", Optional.ofNullable(frame.getModuleName())),
                        Map.entry("type_name", Optional.ofNullable(frame.getClassName())),
                        Map.entry("function", Optional.ofNullable(frame.getMethodName())),
                        Map.entry("file", Optional.ofNullable(frame.getFileName())),
                        Map.entry("line", frame.getLineNumber() >= 0
                                ? Optional.of(frame.getLineNumber())
                                : Optional.empty()),
                        Map.entry("column", Optional.empty()),
                        Map.entry("end_line", Optional.empty()),
                        Map.entry("end_column", Optional.empty()),
                        Map.entry("source_line", Optional.empty()),
                        Map.entry("native", frame.isNativeMethod()),
                        Map.entry("raw", frame.toString())
                ))
                .toList();
    }

    private static java.lang.String rawStack(Throwable failure) {
        var writer = new StringWriter();
        failure.printStackTrace(new PrintWriter(writer));
        return writer.toString();
    }

    private static java.lang.String safeMessage(Throwable failure) {
        var message = failure.getMessage();
        return message == null || message.isBlank() ? failure.getClass().getSimpleName() : message;
    }

    private static Throwable unwrap(Throwable failure) {
        var current = failure;
        while ((current instanceof ExecutionException || current instanceof CompletionException)
                && current.getCause() != null) {
            current = current.getCause();
        }
        return current;
    }
}
