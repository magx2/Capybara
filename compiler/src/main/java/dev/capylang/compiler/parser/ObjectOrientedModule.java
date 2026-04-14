package dev.capylang.compiler.parser;

import dev.capylang.compiler.ImportDeclaration;

import java.util.List;

public record ObjectOrientedModule(
        String name,
        String path,
        ObjectOriented objectOriented,
        List<ImportDeclaration> imports,
        SourceKind sourceKind
) {
}
