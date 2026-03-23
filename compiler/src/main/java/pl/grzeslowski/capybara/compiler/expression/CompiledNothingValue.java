package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;
import pl.grzeslowski.capybara.compiler.parser.SourcePosition;

import java.util.Optional;

public record CompiledNothingValue(Optional<SourcePosition> position, String message) implements CompiledExpression {
    @Override
    public CompiledType type() {
        return PrimitiveLinkedType.NOTHING;
    }
}
