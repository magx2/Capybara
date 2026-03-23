package pl.grzeslowski.capybara.compiler.parser;

import pl.grzeslowski.capybara.compiler.Visibility;

import java.util.List;
import java.util.Optional;

public record Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                       Expression expression,
                       List<String> comments,
                       Visibility visibility,
                       Optional<SourcePosition> position) implements Definition {
    public Function(String name, List<Parameter> parameters, Optional<Type> returnType,
                    Expression expression,
                    List<String> comments,
                    Optional<SourcePosition> position) {
        this(name, parameters, returnType, expression, comments, null, position);
    }
}
