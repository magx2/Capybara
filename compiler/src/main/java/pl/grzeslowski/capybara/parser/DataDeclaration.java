package pl.grzeslowski.capybara.parser;

import java.util.List;
import java.util.Optional;

public record DataDeclaration(String name, List<DataField> fields,
                              Optional<SourcePosition> position) implements Definition {
    public record DataField(String name, Type type) {
    }
}


