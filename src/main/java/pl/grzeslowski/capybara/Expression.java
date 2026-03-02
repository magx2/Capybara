package pl.grzeslowski.capybara;

import static java.util.Objects.requireNonNull;

public sealed interface Expression {
    Type type();

    Expression value();

    public sealed interface BoolExpression extends Expression {
        @Override
        BoolExpression value();

        @Override
        default Type type() {
            return Type.BOOL;
        }
    }

    public enum BooleanValue implements BoolExpression {
        TRUE, FALSE;

        @Override
        public BoolExpression value() {
            return this;
        }
    }

    public record IntValue(String intValue) implements Expression {
        @Override
        public Type type() {
            return Type.INT;
        }

        @Override
        public Expression value() {
            return this;
        }
    }

    public record StringValue(String stringValue) implements Expression {
        @Override
        public Type type() {
            return Type.STRING;
        }

        @Override
        public Expression value() {
            return this;
        }
    }

    public record BoolInfixExpression(Expression left, BoolInfixOperator operator,
                                      Expression right) implements BoolExpression {

        @Override
        public BoolExpression value() {
            // todo implement it
            //noinspection ConstantValue
            return switch (operator) {
                case GT -> null;
                case LT -> null;
                case EQUAL -> null;
                case NOTEQUAL -> null;
                case LE -> null;
                case GE -> null;
            };
        }
    }

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

    public record Variable(String name) implements Expression {
        @Override
        public Type type() {
            // todo
            return null;
        }

        @Override
        public Expression value() {
            // todo implement it
            return null;
        }
    }
}


