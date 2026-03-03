package pl.grzeslowski.capybara.parser;

import java.util.List;

public record DataDeclaration(String name, List<DataField> fields) implements Definition {
    public record DataField(String name, Type type) {
    }
}
