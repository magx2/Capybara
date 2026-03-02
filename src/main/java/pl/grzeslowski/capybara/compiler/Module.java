package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.parser.CapybaraParser;

public record Module(String name, String path, CapybaraParser.Functional functional) {
}
