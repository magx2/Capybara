package pl.grzeslowski.capybara.parser;

import java.util.Optional;

public sealed interface Expression permits BooleanValue, FieldAccess, FloatValue, FunctionCall, FunctionReference, IfExpression, InfixExpression, IntValue, LambdaExpression, LetExpression, MatchExpression, NewData, NewDictExpression, NewListExpression, NewSetExpression, NothingValue, ReduceExpression, StringValue, Value {
    Optional<SourcePosition> position();
}

