package dev.capylang;

import capy.io.Path;
import capy.lang.Result;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.CopyOption;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.List;

public final class IOUtil {
    private IOUtil() {
    }

    public static Result<String> readText(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(Files.readString(javaPath, StandardCharsets.UTF_8));
        } catch (IOException | SecurityException e) {
            return error("read_text", javaPath, e);
        }
    }

    public static Result<List<String>> readLines(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(Files.readAllLines(javaPath, StandardCharsets.UTF_8));
        } catch (IOException | SecurityException e) {
            return error("read_lines", javaPath, e);
        }
    }

    public static Result<List<Byte>> readBytes(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(toByteList(Files.readAllBytes(javaPath)));
        } catch (IOException | SecurityException e) {
            return error("read_bytes", javaPath, e);
        }
    }

    public static Result<String> writeText(Path path, String text) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            Files.writeString(
                    javaPath,
                    text,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return success(text);
        } catch (IOException | SecurityException e) {
            return error("write_text", javaPath, e);
        }
    }

    public static Result<List<String>> writeLines(Path path, List<String> lines) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            Files.write(
                    javaPath,
                    lines,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return success(lines);
        } catch (IOException | SecurityException e) {
            return error("write_lines", javaPath, e);
        }
    }

    public static Result<List<Byte>> writeBytes(Path path, List<Byte> bytes) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            Files.write(
                    javaPath,
                    toByteArray(bytes),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
            return success(bytes);
        } catch (IOException | SecurityException e) {
            return error("write_bytes", javaPath, e);
        }
    }

    public static Result<String> appendText(Path path, String text) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            Files.writeString(
                    javaPath,
                    text,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
            return success(text);
        } catch (IOException | SecurityException e) {
            return error("append_text", javaPath, e);
        }
    }

    public static Result<List<String>> appendLines(Path path, List<String> lines) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            Files.write(
                    javaPath,
                    lines,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
            return success(lines);
        } catch (IOException | SecurityException e) {
            return error("append_lines", javaPath, e);
        }
    }

    public static Result<List<Byte>> appendBytes(Path path, List<Byte> bytes) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            Files.write(
                    javaPath,
                    toByteArray(bytes),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
            return success(bytes);
        } catch (IOException | SecurityException e) {
            return error("append_bytes", javaPath, e);
        }
    }

    public static boolean exists(Path path) {
        return Files.exists(PathUtil.toJavaPath(path));
    }

    public static boolean isFile(Path path) {
        return Files.isRegularFile(PathUtil.toJavaPath(path));
    }

    public static boolean isDirectory(Path path) {
        return Files.isDirectory(PathUtil.toJavaPath(path));
    }

    public static Result<Long> size(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(Files.size(javaPath));
        } catch (IOException | SecurityException e) {
            return error("size", javaPath, e);
        }
    }

    public static Result<Path> createFile(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(PathUtil.fromJavaPath(Files.createFile(javaPath)));
        } catch (IOException | SecurityException e) {
            return error("create_file", javaPath, e);
        }
    }

    public static Result<Path> createDirectory(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(PathUtil.fromJavaPath(Files.createDirectory(javaPath)));
        } catch (IOException | SecurityException e) {
            return error("create_directory", javaPath, e);
        }
    }

    public static Result<Path> createDirectories(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(PathUtil.fromJavaPath(Files.createDirectories(javaPath)));
        } catch (IOException | SecurityException e) {
            return error("create_directories", javaPath, e);
        }
    }

    public static Result<List<Path>> listEntries(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try (var entries = Files.list(javaPath)) {
            return success(entries
                    .map(PathUtil::fromJavaPath)
                    .toList());
        } catch (IOException | SecurityException e) {
            return error("list_entries", javaPath, e);
        }
    }

    public static Result<Boolean> delete(Path path) {
        var javaPath = PathUtil.toJavaPath(path);
        try {
            return success(Files.deleteIfExists(javaPath));
        } catch (IOException | SecurityException e) {
            return error("delete", javaPath, e);
        }
    }

    public static Result<Path> copy(Path source, Path target) {
        return copyWithOptions(source, target, "copy");
    }

    public static Result<Path> copyReplace(Path source, Path target) {
        return copyWithOptions(source, target, "copy_replace", StandardCopyOption.REPLACE_EXISTING);
    }

    public static Result<Path> move(Path source, Path target) {
        return moveWithOptions(source, target, "move");
    }

    public static Result<Path> moveReplace(Path source, Path target) {
        return moveWithOptions(source, target, "move_replace", StandardCopyOption.REPLACE_EXISTING);
    }

    private static Result<Path> copyWithOptions(Path source, Path target, String operation, CopyOption... options) {
        var javaSource = PathUtil.toJavaPath(source);
        var javaTarget = PathUtil.toJavaPath(target);
        try {
            return success(PathUtil.fromJavaPath(Files.copy(javaSource, javaTarget, options)));
        } catch (IOException | SecurityException e) {
            return error(operation, javaSource + " -> " + javaTarget, e);
        }
    }

    private static Result<Path> moveWithOptions(Path source, Path target, String operation, CopyOption... options) {
        var javaSource = PathUtil.toJavaPath(source);
        var javaTarget = PathUtil.toJavaPath(target);
        try {
            return success(PathUtil.fromJavaPath(Files.move(javaSource, javaTarget, options)));
        } catch (IOException | SecurityException e) {
            return error(operation, javaSource + " -> " + javaTarget, e);
        }
    }

    private static <T> Result<T> success(T value) {
        return new Result.Success<>(value);
    }

    private static <T> Result<T> error(String operation, java.nio.file.Path path, Exception e) {
        return error(operation, path.toString(), e);
    }

    private static <T> Result<T> error(String operation, String path, Exception e) {
        var message = operation + " failed for `" + path + "`: " + e.getMessage();
        return new Result.Error<>(new CapybaraException(message, e));
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
