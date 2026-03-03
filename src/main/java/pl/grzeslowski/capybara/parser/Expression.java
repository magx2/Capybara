package pl.grzeslowski.capybara.parser;

import static java.util.Objects.requireNonNull;

public sealed interface Expression permits BoolExpression, FloatValue, FunctionCall, IfExpression, InfixExpression, IntValue, MatchExpression, NewData, StringValue, Variable {
    Type type();

    Expression value();

}

