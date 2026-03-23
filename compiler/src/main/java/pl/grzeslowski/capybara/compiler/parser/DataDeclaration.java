package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record DataDeclaration(String name, List<DataField> fields,
                              List<String> extendsTypes,
                              List<String> typeParameters,
                              Optional<SourcePosition> position) implements Definition {
    public record DataField(String name, Type type) {
    }
}
