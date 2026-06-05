package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

final class Scope {
    private static final Logger LOG = Logger.getLogger(Scope.class.getName());
    //    private static final String SEPARATOR = "_L";
//    private static final Pattern LAST_NUMBER_PATTERN = Pattern.compile("(.+)" + SEPARATOR + "(\\d+)");
    static final Scope EMPTY = new Scope(Map.of(), Map.of(), Set.of());

    private final Map<String, CompiledType> localValues;
    private final Map<String, String> variableNameToUniqueName;
    private final Set<String> emptyShapeCompatibleValues;

    Scope(
            Map<String, CompiledType> localValues,
            Map<String, String> variableNameToUniqueName,
            Set<String> emptyShapeCompatibleValues
    ) {
        this.localValues = localValues;
        this.variableNameToUniqueName = variableNameToUniqueName;
        this.emptyShapeCompatibleValues = emptyShapeCompatibleValues;
    }

    Map<String, CompiledType> localValues() {
        return localValues;
    }

    Map<String, String> variableNameToUniqueName() {
        return variableNameToUniqueName;
    }

    boolean isEmptyShapeCompatibleValue(String name) {
        return emptyShapeCompatibleValues.contains(name);
    }

    public Scope add(String name, CompiledType value) {
        return add(name, value, false);
    }

    public Scope add(String name, CompiledType value, boolean emptyShapeCompatible) {
        var updatedVariableNameToUniqueName = new HashMap<>(variableNameToUniqueName);
//        if (localValues.containsKey(name)) {
//            var uniqueName = findUniqueName(name);
//            LOG.fine( "Duplicate variable name: `"+ name +"` -> `"+uniqueName+"`");
//            updatedVariableNameToUniqueName.put(name, uniqueName);
//        }

        var updatedValues = new HashMap<>(localValues);
        updatedValues.put(name, value);
        var updatedEmptyShapeCompatibleValues = new java.util.LinkedHashSet<>(emptyShapeCompatibleValues);
        if (emptyShapeCompatible) {
            updatedEmptyShapeCompatibleValues.add(name);
        } else {
            updatedEmptyShapeCompatibleValues.remove(name);
        }
        return new Scope(
                Map.copyOf(updatedValues),
                Map.copyOf(updatedVariableNameToUniqueName),
                Set.copyOf(updatedEmptyShapeCompatibleValues)
        );
    }

//    private String findUniqueName(String name) {
//        var idx = 0L;
//        var rawName = name;
//        var matcher = LAST_NUMBER_PATTERN.matcher(name);
//        if (matcher.matches()) {
//            idx = parseLong(matcher.group(2));
//            name = matcher.group(1);
//        }
//        var uniqueName = name + SEPARATOR + (idx + 1);
//        LOG.fine("findUniqueName: " + name + " -> " + uniqueName);
//        return uniqueName;
//    }
}
