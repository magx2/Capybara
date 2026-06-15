package dev.capylang;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public final class PathUtil {
    private PathUtil() {
    }

    @SuppressWarnings("unchecked")
    public static java.nio.file.Path toJavaPath(Object path) {
        var fields = (Map<String, Object>) path;
        var root = dataType(fields.get("root"));
        var segments = (List<String>) fields.getOrDefault("segments", List.of());
        var prefix = optionalString(fields.get("prefix"));
        var javaRoot = switch (root) {
            case "ABSOLUTE" -> prefix
                    .map(Paths::get)
                    .orElseGet(() -> Paths.get(java.nio.file.Path.of("/").toString()));
            case "HOME" -> Paths.get(System.getProperty("user.home"));
            default -> Paths.get("");
        };
        return appendSegments(javaRoot, segments).normalize();
    }

    public static Object fromJavaPath(java.nio.file.Path path) {
        var normalizedPath = path.normalize();
        var homePath = Paths.get(System.getProperty("user.home")).normalize();
        var nativeRoot = normalizedPath.getRoot();
        var root = normalizedPath.startsWith(homePath)
                ? root("HOME")
                : normalizedPath.isAbsolute() ? root("ABSOLUTE") : root("RELATIVE");
        var prefix = dataType(root).equals("ABSOLUTE") && nativeRoot != null
                ? Optional.of(nativeRoot.toString())
                : Optional.<String>empty();
        var segments = normalizedPath.startsWith(homePath)
                ? segments(homePath.relativize(normalizedPath))
                : segments(normalizedPath);
        return Map.of(
                "__type", "Path",
                "root", root,
                "prefix", prefix,
                "segments", segments
        );
    }

    public static Object fromString(String pathString) {
        var root = pathString.startsWith("/")
                ? root("ABSOLUTE")
                : pathString.startsWith("~") ? root("HOME") : root("RELATIVE");
        var value = pathString.startsWith("/")
                ? pathString.substring(1)
                : pathString.startsWith("~/") ? pathString.substring(2)
                : pathString.startsWith("~") ? pathString.substring(1)
                : pathString;
        return Map.of(
                "__type", "Path",
                "root", root,
                "prefix", Optional.empty(),
                "segments", normalizeSegments(root, splitSegments(value))
        );
    }

    private static Object root(String name) {
        return Map.of("__type", name, "name", name, "order", switch (name) {
            case "ABSOLUTE" -> 1;
            case "HOME" -> 2;
            default -> 0;
        });
    }

    private static List<String> splitSegments(String value) {
        var result = new ArrayList<String>();
        for (var segment : value.split("/")) {
            if (!segment.isEmpty()) {
                result.add(segment);
            }
        }
        return List.copyOf(result);
    }

    private static List<String> normalizeSegments(Object root, List<String> segments) {
        var absolute = !"RELATIVE".equals(dataType(root));
        var result = new ArrayList<String>();
        for (var segment : segments) {
            if (segment.isEmpty() || ".".equals(segment)) {
                continue;
            }
            if ("..".equals(segment)) {
                if (!result.isEmpty()) {
                    result.removeLast();
                } else if (!absolute) {
                    result.add(segment);
                }
                continue;
            }
            result.add(segment);
        }
        return List.copyOf(result);
    }

    @SuppressWarnings("unchecked")
    private static String dataType(Object value) {
        return String.valueOf(((Map<String, Object>) value).get("__type"));
    }

    @SuppressWarnings("unchecked")
    private static Optional<String> optionalString(Object value) {
        if (value instanceof Optional<?> optional) {
            return optional.map(String.class::cast);
        }
        if (value instanceof Map<?, ?> map && "Some".equals(map.get("__type"))) {
            return Optional.ofNullable((String) ((Map<String, Object>) value).get("value"));
        }
        return Optional.empty();
    }

    private static java.nio.file.Path appendSegments(java.nio.file.Path root, List<String> segments) {
        var current = root;
        for (var segment : segments) {
            current = current.resolve(segment);
        }
        return current.normalize();
    }

    private static List<String> segments(java.nio.file.Path path) {
        var segments = new ArrayList<String>();
        for (var segment : path) {
            segments.add(segment.toString());
        }
        return List.copyOf(segments);
    }
}
