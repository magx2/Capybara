package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import static java.lang.Long.parseLong;

record Scope(Map<String, LinkedType> localValues, Map<String, String> variableNameToUniqueName) {
    private static final Logger LOG = Logger.getLogger(Scope.class.getName());
    private static final String SEPARATOR = "_L";
    private static final Pattern LAST_NUMBER_PATTERN = Pattern.compile("(.+)" + SEPARATOR + "(\\d+)");
    static final Scope EMPTY = new Scope(Map.of(), Map.of());

    public Scope add(String name, LinkedType value) {
        var updatedVariableNameToUniqueName = new HashMap<>(variableNameToUniqueName);
        if (localValues.containsKey(name)) {
            var uniqueName = findUniqueName(name);
            updatedVariableNameToUniqueName.put(name, uniqueName);
        }

        var updatedValues = new HashMap<>(localValues);
        updatedValues.put(name, value);
        return new Scope(Map.copyOf(updatedValues), Map.copyOf(updatedVariableNameToUniqueName));
    }

    private String findUniqueName(String name) {
        var idx = 0L;
        var rawName = name;
        var matcher = LAST_NUMBER_PATTERN.matcher(name);
        if (matcher.matches()) {
            idx = parseLong(matcher.group(2));
            name = matcher.group(1);
        }
        var uniqueName = name + SEPARATOR + (idx + 1);
        LOG.fine("findUniqueName: " + name + " -> " + uniqueName);
        return uniqueName;
    }
}
