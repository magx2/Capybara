package dev.capylang.compiler.parser;

import dev.capylang.compiler.ImportDeclaration;

import java.util.List;

public record Module(String name, String path, Functional functional, List<ImportDeclaration> imports, SourceKind sourceKind) {
    public Module(String name, String path, Functional functional) {
        this(name, path, functional, List.of(), SourceKind.FUNCTIONAL);
    }
}


