package dev.capylang;

import java.io.IOException;
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
import java.util.List;

public final class IOUtil {
    private IOUtil() {
    }

    public static Object readText(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return ResultUtil.success(Files.readString(javaPath, StandardCharsets.UTF_8));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("read_text", pathText(javaPath, path), e);
        }
    }

    public static Object readLines(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return ResultUtil.success(Files.readAllLines(javaPath, StandardCharsets.UTF_8));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("read_lines", pathText(javaPath, path), e);
        }
    }

    public static Object readBytes(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return ResultUtil.success(toByteList(Files.readAllBytes(javaPath)));
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
            return ResultUtil.success(text);
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
            return ResultUtil.success(lines);
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
            return ResultUtil.success(bytes);
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
            return ResultUtil.success(text);
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
            return ResultUtil.success(lines);
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
            return ResultUtil.success(bytes);
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
            return ResultUtil.success(Files.size(javaPath));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("size", pathText(javaPath, path), e);
        }
    }

    public static Object createFile(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return ResultUtil.success(PathUtil.fromJavaPath(Files.createFile(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("create_file", pathText(javaPath, path), e);
        }
    }

    public static Object createDirectory(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return ResultUtil.success(PathUtil.fromJavaPath(Files.createDirectory(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("create_directory", pathText(javaPath, path), e);
        }
    }

    public static Object createDirectories(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            return ResultUtil.success(PathUtil.fromJavaPath(Files.createDirectories(javaPath)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error("create_directories", pathText(javaPath, path), e);
        }
    }

    public static Object listEntries(Object path) {
        java.nio.file.Path javaPath = null;
        try {
            javaPath = PathUtil.toJavaPath(path);
            try (var entries = Files.list(javaPath)) {
                return ResultUtil.success(entries
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
            return ResultUtil.success(Files.deleteIfExists(javaPath));
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
            return ResultUtil.success(PathUtil.fromJavaPath(Files.copy(javaSource, javaTarget, options)));
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
            return ResultUtil.success(PathUtil.fromJavaPath(Files.move(javaSource, javaTarget, options)));
        } catch (IOException | SecurityException | InvalidPathException e) {
            return error(operation, pathText(javaSource, source) + " -> " + pathText(javaTarget, target), e);
        }
    }

    private static String pathText(java.nio.file.Path path, Object fallback) {
        return path == null ? String.valueOf(fallback) : path.toString();
    }

    private static Object error(String operation, String path, Exception e) {
        var message = operation + " failed for `" + path + "`: " + e.getMessage();
        return ResultUtil.error(kind(operation, e), message, e);
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
