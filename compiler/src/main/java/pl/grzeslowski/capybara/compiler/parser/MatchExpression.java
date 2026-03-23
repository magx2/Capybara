package pl.grzeslowski.capybara.compiler.parser;

import java.util.List;
import java.util.Optional;

public record MatchExpression(Expression matchWith, List<MatchCase> cases,
                              Optional<SourcePosition> position) implements Expression {
    public MatchExpression {
        if (cases.isEmpty()) {
            throw new IllegalArgumentException("Match expression must have at least one case");
        }
        // todo check if all cases have the same type
    }

    public record MatchCase(Pattern pattern, Expression expression) {
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

    public record TypedPattern(Type type, String name) implements Pattern {
    }

    public record VariablePattern(String name) implements Pattern {
    }

    public enum WildcardPattern implements Pattern {
        WILDCARD
    }

    public record WildcardBindingPattern(String name) implements Pattern {
    }

    public record ConstructorPattern(String constructorName, List<Pattern> fieldPatterns) implements Pattern {
    }
}


