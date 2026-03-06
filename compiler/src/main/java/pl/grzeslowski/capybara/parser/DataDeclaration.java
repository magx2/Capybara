package pl.grzeslowski.capybara.parser;

import java.util.List;

public record DataDeclaration(String name, List<DataField> fields, SourcePosition position) implements Definition {
public record DataField(String name, Type type) {
    }
}

