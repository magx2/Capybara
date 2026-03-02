package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.parser.Functional;

public record Module(String name, String path, Functional functional) {
}
