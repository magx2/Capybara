package dev.capylang.compiler.parser;

import dev.capylang.compiler.ImportDeclaration;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public final class ParserImportPreprocessor {
    private static final String MODULE_NAME_PATTERN = "[A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][A-Za-z0-9_]*(?:/[A-Za-z_][A-Za-z0-9_]*)+";
    private static final Pattern FROM_IMPORT_PATTERN = Pattern.compile(
            "^\\s*from\\s+(" + MODULE_NAME_PATTERN + ")\\s+import\\s*\\{\\s*([^}]*)\\s*}(?:\\s+except\\s*\\{\\s*([^}]*)\\s*})?\\s*$"
    );
    private static final Pattern QUALIFIED_IMPORT_PATTERN = Pattern.compile(
            "^\\s*import\\s+(" + MODULE_NAME_PATTERN + ")\\s*$"
    );

    private ParserImportPreprocessor() {
    }

    public static ExtractedSource extract(String source) {
        var imports = new ArrayList<ImportDeclaration>();
        var importLines = new ArrayList<ImportLine>();
        var bodyLines = new ArrayList<String>();
        var lineNumber = 1;
        for (var line : source.split("\\R", -1)) {
            var matcher = FROM_IMPORT_PATTERN.matcher(line);
            var qualifiedMatcher = QUALIFIED_IMPORT_PATTERN.matcher(line);
            if (matcher.matches()) {
                var module = matcher.group(1);
                var symbols = Stream.of(matcher.group(2).split(","))
                        .map(String::trim)
                        .filter(symbol -> !symbol.isBlank())
                        .toList();
                var excludedSymbols = matcher.group(3) == null
                        ? List.<String>of()
                        : Stream.of(matcher.group(3).split(","))
                                .map(String::trim)
                                .filter(symbol -> !symbol.isBlank())
                                .toList();
                var importDeclaration = new ImportDeclaration(module, symbols, excludedSymbols);
                imports.add(importDeclaration);
                importLines.add(new ImportLine(lineNumber, importDeclaration));
                bodyLines.add("");
            } else if (qualifiedMatcher.matches()) {
                var importDeclaration = ImportDeclaration.qualified(qualifiedMatcher.group(1));
                imports.add(importDeclaration);
                importLines.add(new ImportLine(lineNumber, importDeclaration));
                bodyLines.add("");
            } else {
                bodyLines.add(line);
            }
            lineNumber++;
        }
        return new ExtractedSource(
                String.join(System.lineSeparator(), bodyLines),
                List.copyOf(imports),
                List.copyOf(importLines)
        );
    }

    public record ExtractedSource(String source, List<ImportDeclaration> imports, List<ImportLine> importLines) {
    }

    public record ImportLine(int line, ImportDeclaration importDeclaration) {
    }
}
