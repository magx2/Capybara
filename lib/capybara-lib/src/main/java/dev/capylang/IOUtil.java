package dev.capylang;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.AccessDeniedException;
import java.nio.charset.StandardCharsets;
import java.nio.file.CopyOption;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.NoSuchFileException;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.NotDirectoryException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public final class IOUtil {
    private IOUtil() {
    }

    public static Object readText(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(Files.readString(javaPath, StandardCharsets.UTF_8));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("read_text", pathText(javaPath, path), e);
        }
    }

    public static Object readLines(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(Files.readAllLines(javaPath, StandardCharsets.UTF_8));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("read_lines", pathText(javaPath, path), e);
        }
    }

    public static Object readBytes(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(toByteList(Files.readAllBytes(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("read_bytes", pathText(javaPath, path), e);
        }
    }

    public static Object writeText(Object path, String text) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            Files.writeString(
                    javaPath,
                    text,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return success(text);
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("write_text", pathText(javaPath, path), e);
        }
    }

    public static Object writeLines(Object path, List<String> lines) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            Files.write(
                    javaPath,
                    lines,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return success(lines);
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("write_lines", pathText(javaPath, path), e);
        }
    }

    public static Object writeBytes(Object path, List<Byte> bytes) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            Files.write(
                    javaPath,
                    toByteArray(bytes),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return success(bytes);
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("write_bytes", pathText(javaPath, path), e);
        }
    }

    public static Object appendText(Object path, String text) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            Files.writeString(
                    javaPath,
                    text,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
            return success(text);
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("append_text", pathText(javaPath, path), e);
        }
    }

    public static Object appendLines(Object path, List<String> lines) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            Files.write(
                    javaPath,
                    lines,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
            return success(lines);
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("append_lines", pathText(javaPath, path), e);
        }
    }

    public static Object appendBytes(Object path, List<Byte> bytes) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            Files.write(
                    javaPath,
                    toByteArray(bytes),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
            return success(bytes);
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("append_bytes", pathText(javaPath, path), e);
        }
    }

    public static boolean exists(Object path) {
        return Files.exists(PathUtil.toJavaPath(path));
    }

    public static boolean isFile(Object path) {
        return Files.isRegularFile(PathUtil.toJavaPath(path));
    }

    public static boolean isDirectory(Object path) {
        return Files.isDirectory(PathUtil.toJavaPath(path));
    }

    public static Object size(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(Files.size(javaPath));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("size", pathText(javaPath, path), e);
        }
    }

    public static Object createFile(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(PathUtil.fromJavaPath(Files.createFile(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("create_file", pathText(javaPath, path), e);
        }
    }

    public static Object createDirectory(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(PathUtil.fromJavaPath(Files.createDirectory(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("create_directory", pathText(javaPath, path), e);
        }
    }

    public static Object createDirectories(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(PathUtil.fromJavaPath(Files.createDirectories(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("create_directories", pathText(javaPath, path), e);
        }
    }

    public static Object listEntries(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            try (var entries = Files.list(javaPath)) {
                return success(entries
                        .map(PathUtil::fromJavaPath)
                        .toList());
            }
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("list_entries", pathText(javaPath, path), e);
        }
    }

    public static Object delete(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return success(Files.deleteIfExists(javaPath));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("delete", pathText(javaPath, path), e);
        }
    }

    public static Object copy(Object source, Object target) {
        return copyWithOptions(source, target, "copy");
    }

    public static Object copyReplace(Object source, Object target) {
        return copyWithOptions(source, target, "copy_replace", StandardCopyOption.REPLACE_EXISTING);
    }

    public static Object move(Object source, Object target) {
        return moveWithOptions(source, target, "move");
    }

    public static Object moveReplace(Object source, Object target) {
        return moveWithOptions(source, target, "move_replace", StandardCopyOption.REPLACE_EXISTING);
    }

    private static Object copyWithOptions(Object source, Object target, String operation, CopyOption... options) {
        java.nio.file.Path javaSource = null;
        java.nio.file.Path javaTarget = null;
        try {
            javaSource = PathUtil.toJavaPath(source);
            javaTarget = PathUtil.toJavaPath(target);
            return success(PathUtil.fromJavaPath(Files.copy(javaSource, javaTarget, options)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error(operation, pathText(javaSource, source) + " -> " + pathText(javaTarget, target), e);
        }
    }

    private static Object moveWithOptions(Object source, Object target, String operation, CopyOption... options) {
        java.nio.file.Path javaSource = null;
        java.nio.file.Path javaTarget = null;
        try {
            javaSource = PathUtil.toJavaPath(source);
            javaTarget = PathUtil.toJavaPath(target);
            return success(PathUtil.fromJavaPath(Files.move(javaSource, javaTarget, options)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error(operation, pathText(javaSource, source) + " -> " + pathText(javaTarget, target), e);
        }
    }

    private static String pathText(java.nio.file.Path path, Object fallback) {
        return path == null ? String.valueOf(fallback) : path.toString();
    }

    private static Object error(String operation, String path, Exception e) {
        var message = operation + " failed for `" + path + "`: " + e.getMessage();
        return resultError(kind(operation, e), message, e);
    }

    private static Object success(Object value) {
        var result = new java.util.LinkedHashMap<String, Object>();
        result.put("__type", "Success");
        result.put("value", value);
        return result;
    }

    private static Object resultError(String kind, String message, Throwable throwable) {
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

    private static Object errorFull(
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
        return Optional.of(resultError(cause.getClass().getName(), safeMessage(cause), cause));
    }

    private static List<Object> suppressed(Throwable throwable) {
        var suppressed = new ArrayList<Object>();
        for (var item : throwable.getSuppressed()) {
            suppressed.add(resultError(item.getClass().getName(), safeMessage(item), item));
        }
        return List.copyOf(suppressed);
    }

    private static String rawStack(Throwable throwable) {
        var writer = new StringWriter();
        throwable.printStackTrace(new PrintWriter(writer));
        return writer.toString();
    }

    private static String safeMessage(Throwable throwable) {
        var message = throwable.getMessage();
        return message == null || message.isBlank()
                ? throwable.getClass().getSimpleName()
                : message;
    }

    private static String kind(String operation, Exception e) {
        if (e instanceof NoSuchFileException) {
            return "capy.io.path.not_found";
        }
        if (e instanceof AccessDeniedException || e instanceof SecurityException) {
            return "capy.io.path.permission_denied";
        }
        if (e instanceof FileAlreadyExistsException) {
            return "capy.io.path.already_exists";
        }
        if (e instanceof NotDirectoryException) {
            return "capy.io.path.not_directory";
        }
        if (e instanceof DirectoryNotEmptyException) {
            return "capy.io.path.is_directory";
        }
        if (e instanceof InvalidPathException) {
            return "capy.io.path.invalid";
        }
        return switch (operation) {
            case "read_text", "read_lines", "read_bytes" -> "capy.io.read.failed";
            case "write_text", "write_lines", "write_bytes", "append_text", "append_lines", "append_bytes",
                 "create_file", "create_directory", "create_directories" -> "capy.io.write.failed";
            case "copy", "copy_replace" -> "capy.io.copy.failed";
            case "move", "move_replace" -> "capy.io.move.failed";
            case "delete" -> "capy.io.delete.failed";
            case "list_entries" -> "capy.io.list.failed";
            case "size" -> "capy.io.size.failed";
            default -> "capy.io.path.invalid";
        };
    }

    private static byte[] toByteArray(List<Byte> values) {
        var bytes = new byte[values.size()];
        for (var i = 0; i < values.size(); i++) {
            bytes[i] = values.get(i);
        }
        return bytes;
    }

    private static List<Byte> toByteList(byte[] values) {
        var bytes = new Byte[values.length];
        for (var i = 0; i < values.length; i++) {
            bytes[i] = values[i];
        }
        return List.of(bytes);
    }
}
