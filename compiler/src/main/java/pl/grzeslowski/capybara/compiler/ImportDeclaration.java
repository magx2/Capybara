package pl.grzeslowski.capybara.compiler;

import java.util.List;

public record ImportDeclaration(String moduleName, List<String> symbols) {
}
