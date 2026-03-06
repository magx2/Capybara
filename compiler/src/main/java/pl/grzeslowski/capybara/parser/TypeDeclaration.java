package pl.grzeslowski.capybara.parser;

import pl.grzeslowski.capybara.parser.DataDeclaration.DataField;

import java.util.List;
import java.util.Optional;

public record TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                              Optional<SourcePosition> position) implements Definition {
}


