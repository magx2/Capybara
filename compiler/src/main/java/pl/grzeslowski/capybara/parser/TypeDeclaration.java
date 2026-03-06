package pl.grzeslowski.capybara.parser;

import pl.grzeslowski.capybara.parser.DataDeclaration.DataField;

import java.util.List;

public record TypeDeclaration(String name, List<String> subTypes, List<DataField> fields, SourcePosition position) implements Definition {
}

