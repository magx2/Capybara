package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public sealed interface Expression permits BooleanValue, FloatValue, FunctionCall, IfExpression, InfixExpression, IntValue, LetExpression, MatchExpression, NewData, NewListExpression, NewSetExpression, StringValue, Value {
    Optional<SourcePosition> position();
}

