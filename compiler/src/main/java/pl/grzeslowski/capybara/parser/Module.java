package pl.grzeslowski.capybara.parser;

import pl.grzeslowski.capybara.compiler.ImportDeclaration;

import java.util.List;

public record Module(String name, String path, Functional functional, List<ImportDeclaration> imports) {
    public Module(String name, String path, Functional functional) {
        this(name, path, functional, List.of());
    }
}


