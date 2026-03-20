package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;
import pl.grzeslowski.capybara.compiler.PrimitiveLinkedType;

import java.util.List;

public record CompiledMatchExpression(CompiledExpression matchWith, List<MatchCase> cases, CompiledType type) implements CompiledExpression {
    public CompiledMatchExpression {
        if (cases.isEmpty()) {
            throw new IllegalArgumentException("Match expression must have at least one case");
        }
    }

    public record MatchCase(Pattern pattern, CompiledExpression expression) {
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

    public record TypedPattern(CompiledType type, String name) implements Pattern {
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

    public CompiledMatchExpression(CompiledExpression matchWith, List<MatchCase> cases) {
        this(matchWith, cases, PrimitiveLinkedType.ANY);
    }
}
