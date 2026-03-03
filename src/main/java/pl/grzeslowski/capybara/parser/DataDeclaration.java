package pl.grzeslowski.capybara.parser;

import java.util.List;

public record DataDeclaration(String name, List<Field> fields) implements Definition {
    public record Field(String name, Type type) {
    }
}
