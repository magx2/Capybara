package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.PrimitiveLinkedType;

import java.util.List;

public record LinkedMatchExpression(LinkedExpression matchWith, List<MatchCase> cases, LinkedType type) implements LinkedExpression {
    public LinkedMatchExpression {
        if (cases.isEmpty()) {
            throw new IllegalArgumentException("Match expression must have at least one case");
        }
    }

    public record MatchCase(Pattern pattern, LinkedExpression expression) {
    }

    public sealed interface Pattern {
    }

    public record IntPattern(String value) implements Pattern {
    }

    public record StringPattern(String value) implements Pattern {
    }

    public record BoolPattern(String value) implements Pattern {
    }

    public record FloatPattern(String value) implements Pattern {
    }

    public record TypedPattern(LinkedType type, String name) implements Pattern {
    }

    public record VariablePattern(String name) implements Pattern {
    }

    public enum WildcardPattern implements Pattern {
        WILDCARD
    }

    public record ConstructorPattern(String constructorName, List<String> names) implements Pattern {
    }

    public LinkedMatchExpression(LinkedExpression matchWith, List<MatchCase> cases) {
        this(matchWith, cases, PrimitiveLinkedType.ANY);
    }
}
