package pl.grzeslowski.capybara.parser;

import static java.util.Objects.requireNonNull;

public record IfExpression(BoolExpression condition, Expression thenBranch,
                           Expression elseBranch) implements Expression {
    public IfExpression {
        requireNonNull(condition);
        requireNonNull(thenBranch);
        requireNonNull(elseBranch);
        if (!thenBranch.type().equals(elseBranch.type())) {
            throw new IllegalArgumentException("If expressions must have the same type");
        }
    }

    @Override
    public Type type() {
        return thenBranch.type();
    }

    @Override
    public Expression value() {
        if (condition.value() == BooleanValue.TRUE) {
            return thenBranch.value();
        }
        return elseBranch.value();
    }
}
