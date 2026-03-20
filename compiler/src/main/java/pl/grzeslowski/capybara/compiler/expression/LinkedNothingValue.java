package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;
import pl.grzeslowski.capybara.parser.SourcePosition;

import java.util.Optional;

public record LinkedNothingValue(Optional<SourcePosition> position, String message) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.NOTHING;
    }
}
