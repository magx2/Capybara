package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record FunctionInvoke(Expression function, List<Expression> arguments, Optional<SourcePosition> position)
        implements Expression {
}
