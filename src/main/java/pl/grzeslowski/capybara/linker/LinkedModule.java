package pl.grzeslowski.capybara.linker;

import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

public record LinkedModule(
        Map<String, GenericDataType> types,
        Set<LinkedFunction> functions) {
}
