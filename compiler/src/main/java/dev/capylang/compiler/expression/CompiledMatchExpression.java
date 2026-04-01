package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.PrimitiveLinkedType;

import java.util.List;
import java.util.Optional;

public record CompiledMatchExpression(CompiledExpression matchWith, List<MatchCase> cases, CompiledType type) implements CompiledExpression {
    public CompiledMatchExpression {
        if (cases.isEmpty()) {
            throw new IllegalArgumentException("Match expression must have at least one case");
        }
    }

    public record MatchCase(Pattern pattern, Optional<CompiledExpression> guard, CompiledExpression expression) {
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
