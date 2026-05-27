package dev.capylang.compiler;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;

public final class NativeImplementationScanner {
    private static final Pattern JAVA_PACKAGE = Pattern.compile("(?m)^\\s*package\\s+([A-Za-z_][\\w.]*)\\s*;");
    private static final Pattern JAVA_IMPORT = Pattern.compile("(?m)^\\s*import\\s+([A-Za-z_][\\w.]*)\\s*;");
    private static final Pattern JAVA_NATIVE_IMPLEMENTATION = Pattern.compile(
            "@(?:[A-Za-z_][\\w.]*\\.)?NativeImplementation\\s*(?:\\(([^)]*)\\))?\\s*"
            + "(?:(?:public|protected|private|abstract|final|static|sealed|non-sealed)\\s+)*"
            + "class\\s+([A-Za-z_][\\w]*)\\s*(?:extends\\s+[^\\{]+?)?\\s*(?:implements\\s+([^\\{]+))?",
            Pattern.MULTILINE
    );
    private static final Pattern JS_REQUIRE = Pattern.compile(
            "(?:const|let|var)\\s*\\{([^}]+)}\\s*=\\s*require\\(\\s*(['\"])([^'\"]+)\\2\\s*\\)"
    );
    private static final Pattern JS_NATIVE_IMPLEMENTATION = Pattern.compile(
            "(?m)^\\s*@NativeImplementation\\s*(?:\\(([^\\r\\n)]*)\\))?\\s*\\R\\s*"
            + "(?:export\\s+)?class\\s+([A-Za-z_$][\\w$]*)\\s+extends\\s+([A-Za-z_$][\\w$]*(?:\\.[A-Za-z_$][\\w$]*)?)"
    );
    private static final Pattern PY_IMPORT = Pattern.compile("(?m)^\\s*from\\s+([A-Za-z_][\\w.]*)\\s+import\\s+([^\\n]+)");
    private static final Pattern PY_NATIVE_IMPLEMENTATION = Pattern.compile(
            "(?m)^\\s*@NativeImplementation\\s*(?:\\(([^)]*)\\))?\\s*\\R\\s*class\\s+([A-Za-z_][\\w]*)\\s*\\(([^)]*)\\)\\s*:"
    );
    private static final Pattern QUALIFIER_ARGUMENT = Pattern.compile("\\bqualifier\\b\\s*[:=]\\s*(['\"])(.*?)\\1");
    private static final Pattern POSITIONAL_STRING_ARGUMENT = Pattern.compile("^\\s*(['\"])(.*?)\\1\\s*$");

    private NativeImplementationScanner() {
    }

    public static NativeProviderManifest scan(Path capybaraInputDir) throws IOException {
        if (capybaraInputDir == null) {
            return NativeProviderManifest.empty();
        }
        var candidates = new ArrayList<NativeImplementationCandidate>();
        for (var root : nativeSourceRoots(capybaraInputDir, NativeProviderBackend.JAVA)) {
            candidates.addAll(scanJavaRoot(root));
        }
        for (var root : nativeSourceRoots(capybaraInputDir, NativeProviderBackend.JAVASCRIPT)) {
            candidates.addAll(scanJavaScriptRoot(root));
        }
        for (var root : nativeSourceRoots(capybaraInputDir, NativeProviderBackend.PYTHON)) {
            candidates.addAll(scanPythonRoot(root));
        }
        return toManifest(candidates);
    }

    private static List<Path> nativeSourceRoots(Path capybaraInputDir, NativeProviderBackend backend) {
        var roots = new ArrayList<Path>();
        var parent = capybaraInputDir.toAbsolutePath().normalize().getParent();
        var names = switch (backend) {
            case JAVA -> List.of("java");
            case JAVASCRIPT -> List.of("js", "javascript");
            case PYTHON -> List.of("py", "python");
        };
        for (var name : names) {
            if (parent != null) {
                addExistingDirectory(roots, parent.resolve(name));
                addExistingDirectory(roots, parent.resolve("native").resolve(name));
            }
            addExistingDirectory(roots, capybaraInputDir.resolve(name));
            addExistingDirectory(roots, capybaraInputDir.resolve("native").resolve(name));
        }
        return roots.stream().distinct().toList();
    }

    private static void addExistingDirectory(List<Path> roots, Path path) {
        var normalized = path.toAbsolutePath().normalize();
        if (Files.isDirectory(normalized)) {
            roots.add(normalized);
        }
    }

    private static List<NativeImplementationCandidate> scanJavaRoot(Path root) throws IOException {
        return scanRoot(root, ".java", NativeImplementationScanner::scanJavaFile);
    }

    private static List<NativeImplementationCandidate> scanJavaScriptRoot(Path root) throws IOException {
        return scanRoot(root, ".js", NativeImplementationScanner::scanJavaScriptFile);
    }

    private static List<NativeImplementationCandidate> scanPythonRoot(Path root) throws IOException {
        return scanRoot(root, ".py", NativeImplementationScanner::scanPythonFile);
    }

    private static List<NativeImplementationCandidate> scanRoot(
            Path root,
            String extension,
            FileScanner scanner
    ) throws IOException {
        try (var stream = Files.walk(root)) {
            return stream
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(extension))
                    .flatMap(path -> {
                        try {
                            return scanner.scan(root, path).stream();
                        } catch (IOException e) {
                            throw new UncheckedIOException(e);
                        }
                    })
                    .toList();
        } catch (UncheckedIOException e) {
            throw e.getCause();
        }
    }

    private static List<NativeImplementationCandidate> scanJavaFile(Path root, Path file) throws IOException {
        var source = Files.readString(file);
        var packageName = firstGroup(JAVA_PACKAGE, source).orElse("");
        var imports = javaImports(source);
        var candidates = new ArrayList<NativeImplementationCandidate>();
        var matcher = JAVA_NATIVE_IMPLEMENTATION.matcher(source);
        while (matcher.find()) {
            var className = matcher.group(2);
            var interfaceName = firstType(matcher.group(3)).orElse(null);
            if (interfaceName == null) {
                continue;
            }
            var qualifier = qualifier(matcher.group(1));
            var implementationClassName = packageName.isBlank() ? className : packageName + "." + className;
            var interfaceClassName = resolveJavaType(interfaceName, packageName, imports);
            candidates.add(new NativeImplementationCandidate(
                    new ImplementationKey(javaInterfaceId(interfaceClassName), qualifier),
                    NativeProviderBackend.JAVA,
                    new NativeProviderBackendBinding(implementationClassName, null, null, "constructor"),
                    file
            ));
        }
        return candidates;
    }

    private static Map<String, String> javaImports(String source) {
        var imports = new LinkedHashMap<String, String>();
        var matcher = JAVA_IMPORT.matcher(source);
        while (matcher.find()) {
            var imported = matcher.group(1);
            imports.put(simpleName(imported), imported);
        }
        return imports;
    }

    private static String resolveJavaType(String typeName, String packageName, Map<String, String> imports) {
        var normalized = eraseGenericType(typeName);
        if (normalized.contains(".")) {
            return normalized;
        }
        var imported = imports.get(normalized);
        if (imported != null) {
            return imported;
        }
        return packageName.isBlank() ? normalized : packageName + "." + normalized;
    }

    private static String javaInterfaceId(String className) {
        return "/" + className.replace('.', '/');
    }

    private static List<NativeImplementationCandidate> scanJavaScriptFile(Path root, Path file) throws IOException {
        var source = Files.readString(file);
        var relativeFile = normalizeRelativePath(root.relativize(file));
        var imports = javaScriptImports(relativeFile, source);
        var candidates = new ArrayList<NativeImplementationCandidate>();
        var matcher = JS_NATIVE_IMPLEMENTATION.matcher(source);
        while (matcher.find()) {
            var className = matcher.group(2);
            var interfaceName = simpleName(matcher.group(3));
            var interfaceId = imports.get(interfaceName);
            if (interfaceId == null) {
                interfaceId = javaScriptSiblingInterfaceId(relativeFile, interfaceName);
            }
            var qualifier = qualifier(matcher.group(1));
            candidates.add(new NativeImplementationCandidate(
                    new ImplementationKey(interfaceId, qualifier),
                    NativeProviderBackend.JAVASCRIPT,
                    new NativeProviderBackendBinding(null, javaScriptModuleName(relativeFile), className, "new"),
                    file
            ));
        }
        return candidates;
    }

    private static Map<String, String> javaScriptImports(Path relativeFile, String source) {
        var imports = new LinkedHashMap<String, String>();
        var matcher = JS_REQUIRE.matcher(source);
        while (matcher.find()) {
            var importSpec = matcher.group(1);
            var modulePath = matcher.group(3);
            var targetModule = normalizeRelativePath(relativeFile.getParent() == null
                    ? Path.of(modulePath)
                    : relativeFile.getParent().resolve(modulePath));
            for (var imported : importSpec.split(",")) {
                var localName = imported.trim();
                if (localName.isBlank()) {
                    continue;
                }
                var aliasParts = localName.split(":");
                var exportedName = aliasParts[0].trim();
                localName = aliasParts[aliasParts.length - 1].trim();
                var interfaceId = javaScriptInterfaceId(targetModule, exportedName);
                imports.put(localName, interfaceId);
            }
        }
        return imports;
    }

    private static String javaScriptSiblingInterfaceId(Path relativeFile, String interfaceName) {
        var parent = relativeFile.getParent();
        var target = parent == null ? Path.of(interfaceName + ".js") : parent.resolve(interfaceName + ".js");
        return javaScriptInterfaceId(target);
    }

    private static String javaScriptInterfaceId(Path modulePath) {
        var withoutExtension = removeExtension(normalizeRelativePath(modulePath), ".js");
        return "/" + withoutExtension.toString().replace('\\', '/');
    }

    private static String javaScriptInterfaceId(Path modulePath, String typeName) {
        var base = javaScriptInterfaceId(modulePath);
        var moduleName = simplePathName(base);
        return moduleName.equals(typeName) ? base : base + "." + typeName;
    }

    private static String javaScriptModuleName(Path relativeFile) {
        var fromModule = Path.of("dev", "capylang", "native_providers.js");
        var fromDir = Optional.ofNullable(fromModule.getParent()).orElse(Path.of(""));
        var relative = fromDir.relativize(normalizeRelativePath(relativeFile)).toString().replace('\\', '/');
        return relative.startsWith(".") ? relative : "./" + relative;
    }

    private static List<NativeImplementationCandidate> scanPythonFile(Path root, Path file) throws IOException {
        var source = Files.readString(file);
        var relativeFile = normalizeRelativePath(root.relativize(file));
        var imports = pythonImports(source);
        var candidates = new ArrayList<NativeImplementationCandidate>();
        var matcher = PY_NATIVE_IMPLEMENTATION.matcher(source);
        while (matcher.find()) {
            var className = matcher.group(2);
            var interfaceName = firstType(matcher.group(3)).map(NativeImplementationScanner::simpleName).orElse(null);
            if (interfaceName == null) {
                continue;
            }
            var interfaceId = imports.get(interfaceName);
            if (interfaceId == null) {
                interfaceId = pythonSiblingInterfaceId(relativeFile, interfaceName);
            }
            var qualifier = qualifier(matcher.group(1));
            candidates.add(new NativeImplementationCandidate(
                    new ImplementationKey(interfaceId, qualifier),
                    NativeProviderBackend.PYTHON,
                    new NativeProviderBackendBinding(className, pythonModuleName(relativeFile), null, "call"),
                    file
            ));
        }
        return candidates;
    }

    private static Map<String, String> pythonImports(String source) {
        var imports = new LinkedHashMap<String, String>();
        var matcher = PY_IMPORT.matcher(source);
        while (matcher.find()) {
            var moduleName = matcher.group(1);
            for (var imported : matcher.group(2).split(",")) {
                var localName = imported.trim();
                if (localName.isBlank() || "*".equals(localName)) {
                    continue;
                }
                var aliasParts = localName.split("\\s+as\\s+");
                var importedName = aliasParts[0].trim();
                localName = aliasParts[aliasParts.length - 1].trim();
                var interfaceId = "/" + moduleName.replace('.', '/');
                if (!simpleName(moduleName).equals(importedName)) {
                    interfaceId = interfaceId + "." + importedName;
                }
                imports.put(localName, interfaceId);
            }
        }
        return imports;
    }

    private static String pythonSiblingInterfaceId(Path relativeFile, String interfaceName) {
        var parent = relativeFile.getParent();
        var target = parent == null ? Path.of(interfaceName + ".py") : parent.resolve(interfaceName + ".py");
        return "/" + removeExtension(normalizeRelativePath(target), ".py").toString().replace('\\', '/');
    }

    private static String pythonModuleName(Path relativeFile) {
        return removeExtension(normalizeRelativePath(relativeFile), ".py").toString().replace('\\', '.').replace('/', '.');
    }

    private static NativeProviderManifest toManifest(List<NativeImplementationCandidate> candidates) {
        var bindings = new LinkedHashMap<ImplementationKey, BackendBindings>();
        for (var candidate : candidates) {
            var backendBindings = bindings.computeIfAbsent(candidate.key(), ignored -> new BackendBindings());
            backendBindings.put(candidate);
        }
        return new NativeProviderManifest(bindings.entrySet().stream()
                .map(entry -> new NativeProviderBinding(
                        entry.getKey().interfaceId(),
                        entry.getKey().qualifier(),
                        entry.getValue().javaBinding,
                        entry.getValue().javascriptBinding,
                        entry.getValue().pythonBinding
                ))
                .toList(), "native source annotations");
    }

    private static Optional<String> firstGroup(Pattern pattern, String source) {
        var matcher = pattern.matcher(source);
        return matcher.find() ? Optional.ofNullable(matcher.group(1)) : Optional.empty();
    }

    private static Optional<String> firstType(String types) {
        if (types == null || types.isBlank()) {
            return Optional.empty();
        }
        return Optional.of(eraseGenericType(types.split(",")[0].trim()));
    }

    private static String qualifier(String arguments) {
        if (arguments == null || arguments.isBlank()) {
            return "";
        }
        var named = QUALIFIER_ARGUMENT.matcher(arguments);
        if (named.find()) {
            return named.group(2);
        }
        var positional = POSITIONAL_STRING_ARGUMENT.matcher(arguments);
        if (positional.find()) {
            return positional.group(2);
        }
        return "";
    }

    private static String eraseGenericType(String value) {
        var normalized = value.trim();
        var genericStart = normalized.indexOf('<');
        if (genericStart >= 0) {
            normalized = normalized.substring(0, genericStart);
        }
        return normalized.trim();
    }

    private static String simpleName(String qualifiedName) {
        var normalized = qualifiedName.trim();
        var dot = normalized.lastIndexOf('.');
        return dot < 0 ? normalized : normalized.substring(dot + 1);
    }

    private static String simplePathName(String qualifiedName) {
        var normalized = qualifiedName.trim().replace('\\', '/');
        var slash = normalized.lastIndexOf('/');
        return slash < 0 ? normalized : normalized.substring(slash + 1);
    }

    private static Path normalizeRelativePath(Path path) {
        return path.normalize();
    }

    private static Path removeExtension(Path path, String extension) {
        var value = path.toString().replace('\\', '/');
        if (value.endsWith(extension)) {
            value = value.substring(0, value.length() - extension.length());
        }
        return Path.of(value);
    }

    @FunctionalInterface
    private interface FileScanner {
        List<NativeImplementationCandidate> scan(Path root, Path file) throws IOException;
    }

    private record NativeImplementationCandidate(
            ImplementationKey key,
            NativeProviderBackend backend,
            NativeProviderBackendBinding binding,
            Path sourceFile
    ) {
    }

    private record ImplementationKey(String interfaceId, String qualifier) {
    }

    private static final class BackendBindings {
        private NativeProviderBackendBinding javaBinding;
        private NativeProviderBackendBinding javascriptBinding;
        private NativeProviderBackendBinding pythonBinding;

        void put(NativeImplementationCandidate candidate) {
            switch (candidate.backend()) {
                case JAVA -> {
                    if (javaBinding != null) {
                        throw duplicate(candidate);
                    }
                    javaBinding = candidate.binding();
                }
                case JAVASCRIPT -> {
                    if (javascriptBinding != null) {
                        throw duplicate(candidate);
                    }
                    javascriptBinding = candidate.binding();
                }
                case PYTHON -> {
                    if (pythonBinding != null) {
                        throw duplicate(candidate);
                    }
                    pythonBinding = candidate.binding();
                }
            }
        }

        private IllegalArgumentException duplicate(NativeImplementationCandidate candidate) {
            return new IllegalArgumentException(
                    "DuplicateProvider: Duplicate @NativeImplementation for interface `"
                    + candidate.key().interfaceId() + "` with qualifier `" + candidate.key().qualifier()
                    + "` and backend `" + candidate.backend().jsonValue() + "` in source `" + candidate.sourceFile() + "`"
            );
        }
    }
}
