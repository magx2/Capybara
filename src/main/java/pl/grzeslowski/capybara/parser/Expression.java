package pl.grzeslowski.capybara.parser;

import java.util.List;

import static java.util.Objects.requireNonNull;

public sealed interface Expression permits BoolExpression, FunctionCall, IfExpression, IntValue, NewData, StringValue, Variable {
    Type type();

    Expression value();

}

