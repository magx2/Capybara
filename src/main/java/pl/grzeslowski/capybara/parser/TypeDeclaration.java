package pl.grzeslowski.capybara.parser;

import java.util.List;

public record TypeDeclaration(String name, List<String> subTypes) implements Definition {
}
