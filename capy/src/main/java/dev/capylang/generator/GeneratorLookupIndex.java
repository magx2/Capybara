package dev.capylang.generator;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/** Bounded identity indexes for repeated lookups inside self-hosted generators. */
public final class GeneratorLookupIndex {
    private static final int MAX_INDEXED_LISTS = 64;
    private static final Map<List<?>, Map<String, List<Object>>> FUNCTIONS_BY_NAME = new IdentityHashMap<>();
    private static final Map<List<?>, Map<String, List<Object>>> FUNCTIONS_BY_PREFIX = new IdentityHashMap<>();
    private static final Map<List<?>, Map<String, Object>> MODULES_BY_PATH = new IdentityHashMap<>();

    private GeneratorLookupIndex() {
    }

    public static synchronized List<Object> functionsNamed(List<Object> functions, String name) {
        return functionsByName(functions).getOrDefault(name, List.of());
    }

    public static synchronized List<Object> functionsNamedWithPrefix(List<Object> functions, String prefix) {
        var prefixes = FUNCTIONS_BY_PREFIX.computeIfAbsent(functions, ignored -> new LinkedHashMap<>());
        var cached = prefixes.get(prefix);
        if (cached != null) {
            return cached;
        }
        if (FUNCTIONS_BY_PREFIX.size() >= MAX_INDEXED_LISTS) {
            FUNCTIONS_BY_PREFIX.clear();
            prefixes = new LinkedHashMap<>();
            FUNCTIONS_BY_PREFIX.put(functions, prefixes);
        }
        var matches = new ArrayList<Object>();
        for (var function : functions) {
            if (function instanceof Map<?, ?> data) {
                var name = data.get("name");
                if (name != null && name.toString().startsWith(prefix)) {
                    matches.add(function);
                }
            }
        }
        var result = List.copyOf(matches);
        prefixes.put(prefix, result);
        return result;
    }

    public static synchronized Optional<Object> moduleByPath(String modulePath, List<Object> modules) {
        var index = MODULES_BY_PATH.get(modules);
        if (index == null) {
            if (MODULES_BY_PATH.size() >= MAX_INDEXED_LISTS) {
                MODULES_BY_PATH.clear();
            }
            index = indexModules(modules);
            MODULES_BY_PATH.put(modules, index);
        }
        return Optional.ofNullable(index.get(normalizePath(modulePath)));
    }

    private static Map<String, List<Object>> indexFunctions(List<Object> functions) {
        var mutable = new LinkedHashMap<String, List<Object>>();
        for (var function : functions) {
            if (!(function instanceof Map<?, ?> data)) {
                continue;
            }
            var nameValue = data.get("name");
            var name = nameValue == null ? "" : nameValue.toString();
            mutable.computeIfAbsent(name, ignored -> new ArrayList<>()).add(function);
        }
        var result = new LinkedHashMap<String, List<Object>>();
        mutable.forEach((name, values) -> result.put(name, List.copyOf(values)));
        return Map.copyOf(result);
    }

    private static Map<String, List<Object>> functionsByName(List<Object> functions) {
        var index = FUNCTIONS_BY_NAME.get(functions);
        if (index == null) {
            if (FUNCTIONS_BY_NAME.size() >= MAX_INDEXED_LISTS) {
                FUNCTIONS_BY_NAME.clear();
            }
            index = indexFunctions(functions);
            FUNCTIONS_BY_NAME.put(functions, index);
        }
        return index;
    }

    private static Map<String, Object> indexModules(List<Object> modules) {
        var result = new LinkedHashMap<String, Object>();
        for (var module : modules) {
            if (!(module instanceof Map<?, ?> data)) {
                continue;
            }
            var pathValue = data.get("path");
            var nameValue = data.get("name");
            var path = normalizePath(pathValue == null ? "" : pathValue.toString());
            var name = nameValue == null ? "" : nameValue.toString();
            var modulePath = path.isBlank() ? name : path + "/" + name;
            result.putIfAbsent(normalizePath(modulePath), module);
        }
        return Map.copyOf(result);
    }

    private static String normalizePath(String path) {
        var normalized = path.replace('\\', '/');
        var start = 0;
        var end = normalized.length();
        while (start < end && normalized.charAt(start) == '/') {
            start++;
        }
        while (end > start && normalized.charAt(end - 1) == '/') {
            end--;
        }
        return normalized.substring(start, end);
    }
}
