package dev.capylang;

import capy.io.Path;
import capy.io.PathRoot;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public final class PathUtil {
    private PathUtil() {
    }

    public static java.nio.file.Path toJavaPath(Path path) {
        var root = path.root();
        var segments = path.segments();
        var javaRoot = switch (root) {
            case ABSOLUTE -> path.prefix().isPresent()
                    ? Paths.get((String) path.prefix().orElseThrow())
                    : Paths.get(java.nio.file.Path.of("/").toString());
            case HOME -> Paths.get(System.getProperty("user.home"));
            case RELATIVE -> Paths.get("");
        };
        return appendSegments(javaRoot, segments).normalize();
    }

    public static Path fromJavaPath(java.nio.file.Path path) {
        var normalizedPath = path.normalize();
        var homePath = Paths.get(System.getProperty("user.home")).normalize();
        var nativeRoot = normalizedPath.getRoot();
        var root = normalizedPath.startsWith(homePath)
                ? PathRoot.HOME
                : normalizedPath.isAbsolute() ? PathRoot.ABSOLUTE : PathRoot.RELATIVE;
        var prefix = root == PathRoot.ABSOLUTE && nativeRoot != null
                ? Optional.of(nativeRoot.toString())
                : Optional.<String>empty();
        var segments = normalizedPath.startsWith(homePath)
                ? segments(homePath.relativize(normalizedPath))
                : segments(normalizedPath);
        return new Path(root, prefix, segments);
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
