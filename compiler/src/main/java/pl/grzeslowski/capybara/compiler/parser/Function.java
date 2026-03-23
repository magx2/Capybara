package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                       Expression expression,
                       List<String> comments,
                       Optional<SourcePosition> position) implements Definition {
}


