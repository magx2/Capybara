package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;
import pl.grzeslowski.capybara.parser.SourcePosition;

import java.util.Optional;

public record LinkedNothingValue(Optional<SourcePosition> position, String message) implements LinkedExpression {
    @Override
    public LinkedType type() {
        return PrimitiveLinkedType.NOTHING;
    }
}
