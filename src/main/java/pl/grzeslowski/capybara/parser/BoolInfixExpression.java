package pl.grzeslowski.capybara.parser;

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
