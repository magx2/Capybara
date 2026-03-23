package pl.grzeslowski.capybara.compiler.parser;

import pl.grzeslowski.capybara.compiler.parser.DataDeclaration.DataField;

import java.util.List;
import java.util.Optional;

public record TypeDeclaration(String name, List<String> subTypes, List<DataField> fields,
                              List<String> typeParameters,
                              Optional<SourcePosition> position) implements Definition {
}

