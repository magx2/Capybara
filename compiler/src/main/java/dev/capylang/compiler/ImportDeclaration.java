package dev.capylang.compiler;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public record ImportDeclaration(String moduleName, List<String> symbols, List<String> excludedSymbols, boolean qualifiedOnly) {
    public ImportDeclaration(String moduleName, List<String> symbols, List<String> excludedSymbols) {
        this(moduleName, symbols, excludedSymbols, false);
    }

    public static ImportDeclaration qualified(String moduleName) {
        return new ImportDeclaration(moduleName, List.of(), List.of(), true);
    }

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
