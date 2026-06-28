package dev.capylang;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public final class ResultUtil {
    private ResultUtil() {
    }

    public static Object success(Object value) {
        var result = new LinkedHashMap<String, Object>();
        result.put("__type", "Success");
        result.put("value", value);
        return result;
    }

    public static Object error(String kind, String message) {
        return errorFull(
                kind,
                message,
                Optional.empty(),
                Optional.empty(),
                List.of(),
                Optional.empty(),
                Optional.empty(),
                List.of()
        );
    }

    public static Object error(String kind, String message, Throwable throwable) {
        var stackTrace = stackTrace(throwable);
        return errorFull(
                kind,
                message,
                Optional.empty(),
                sourceLocation(stackTrace),
                stackTrace,
                Optional.of(rawStack(throwable)),
                cause(throwable),
                suppressed(throwable)
        );
    }

    public static Object thrownError(Object error, Throwable throwable) {
        if (!(error instanceof Map<?, ?> map)) {
            return error(
                    "capy.runtime.invalid_thrown_error",
                    "OO throw expected capy/lang/Result.Error",
                    throwable
            );
        }

        var stackTrace = stackTrace(throwable);
        return errorFull(
                stringField(map, "kind", "capy.error"),
                stringField(map, "message", ""),
                optionalObjectField(map, "details"),
                sourceLocation(stackTrace),
                stackTrace,
                Optional.of(rawStack(throwable)),
                optionalObjectField(map, "cause"),
                listField(map, "suppressed")
        );
    }

    public static Object errorFull(
            String kind,
            String message,
            Optional<Object> details,
            Optional<Object> location,
            List<Object> stackTrace,
            Optional<String> rawStack,
            Optional<Object> cause,
            List<Object> suppressed
    ) {
        return Map.ofEntries(
                Map.entry("__type", "Error"),
                Map.entry("kind", kind),
                Map.entry("message", message),
                Map.entry("details", details),
                Map.entry("location", location),
                Map.entry("stack_trace", stackTrace),
                Map.entry("raw_stack", rawStack),
                Map.entry("cause", cause),
                Map.entry("suppressed", suppressed)
        );
    }

    private static String stringField(Map<?, ?> map, String name, String fallback) {
        var value = map.get(name);
        return value == null ? fallback : String.valueOf(value);
    }

    private static Optional<Object> optionalObjectField(Map<?, ?> map, String name) {
        var value = map.get(name);
        if (value instanceof Optional<?> optional) {
            return optional.map(item -> item);
        }
        return Optional.empty();
    }

    private static List<Object> listField(Map<?, ?> map, String name) {
        var value = map.get(name);
        if (value instanceof List<?> list) {
            return List.copyOf(list);
        }
        return List.of();
    }

    private static List<Object> stackTrace(Throwable throwable) {
        var frames = new ArrayList<Object>();
        for (var element : throwable.getStackTrace()) {
            frames.add(stackFrame(element));
        }
        return List.copyOf(frames);
    }

    private static Object stackFrame(StackTraceElement element) {
        return Map.ofEntries(
                Map.entry("__type", "StackFrame"),
                Map.entry("backend", "java"),
                Map.entry("module", Optional.ofNullable(element.getModuleName())),
                Map.entry("type_name", Optional.ofNullable(element.getClassName())),
                Map.entry("function", Optional.ofNullable(element.getMethodName())),
                Map.entry("file", Optional.ofNullable(element.getFileName())),
                Map.entry("line", element.getLineNumber() >= 0 ? Optional.of(element.getLineNumber()) : Optional.empty()),
                Map.entry("column", Optional.empty()),
                Map.entry("end_line", Optional.empty()),
                Map.entry("end_column", Optional.empty()),
                Map.entry("source_line", Optional.empty()),
                Map.entry("native", element.isNativeMethod()),
                Map.entry("raw", element.toString())
        );
    }

    private static Optional<Object> sourceLocation(List<Object> stackTrace) {
        for (var frame : stackTrace) {
            if (frame instanceof Map<?, ?> map && map.get("file") instanceof Optional<?> file && file.isPresent()) {
                var line = map.get("line") instanceof Optional<?> lineOption && lineOption.isPresent()
                        ? (Integer) lineOption.get()
                        : 0;
                return Optional.of(Map.ofEntries(
                        Map.entry("__type", "SourceLocation"),
                        Map.entry("file", String.valueOf(file.get())),
                        Map.entry("line", line),
                        Map.entry("column", Optional.empty()),
                        Map.entry("end_line", Optional.empty()),
                        Map.entry("end_column", Optional.empty())
                ));
            }
        }
        return Optional.empty();
    }

    private static Optional<Object> cause(Throwable throwable) {
        var cause = throwable.getCause();
        if (cause == null || cause == throwable) {
            return Optional.empty();
        }
        return Optional.of(error(cause.getClass().getName(), safeMessage(cause), cause));
    }

    private static List<Object> suppressed(Throwable throwable) {
        var suppressed = new ArrayList<Object>();
        for (var item : throwable.getSuppressed()) {
            suppressed.add(error(item.getClass().getName(), safeMessage(item), item));
        }
        return List.copyOf(suppressed);
    }

    private static String rawStack(Throwable throwable) {
        var writer = new StringWriter();
        throwable.printStackTrace(new PrintWriter(writer));
        return writer.toString();
    }

    public static String safeMessage(Throwable throwable) {
        var message = throwable.getMessage();
        return message == null || message.isBlank()
                ? throwable.getClass().getSimpleName()
                : message;
    }
}
