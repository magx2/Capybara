package pl.grzeslowski.capybara.parser;

import java.util.List;

public record MatchExpression(Expression matchWith, List<MatchCase> cases) implements Expression {
    public MatchExpression {
        if (cases.isEmpty()) {
            throw new IllegalArgumentException("Match expression must have at least one case");
        }
        // todo check if all cases have the same type
    }

    @Override
    public Type type() {
        return cases.get(0).expression.type();
    }

    @Override
    public Expression value() {
        throw new UnsupportedOperationException("MatchExpression.value()");
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

    public record VariablePattern(String name) implements Pattern {
    }

    public enum WildcardPattern implements Pattern {
        WILDCARD
    }

    public record ConstructorPattern(String constructorName, List<String> names) implements Pattern {
    }
}
