package pl.grzeslowski.capybara.compiler;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public record ImportDeclaration(String moduleName, List<String> symbols, List<String> excludedSymbols) {
    public boolean isStarImport() {
        return symbols.contains("*");
    }

    public Set<String> selectedSymbols(Collection<String> availableSymbols) {
        var selected = new LinkedHashSet<String>();
        if (isStarImport()) {
            selected.addAll(availableSymbols);
        } else {
            selected.addAll(symbols);
        }
        selected.removeAll(excludedSymbols);
        return selected;
    }
}
