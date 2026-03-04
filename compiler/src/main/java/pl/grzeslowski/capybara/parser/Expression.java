package pl.grzeslowski.capybara.parser;

public sealed interface Expression permits BooleanValue, FloatValue, FunctionCall, IfExpression, InfixExpression, IntValue, LetExpression, MatchExpression, NewData, StringValue, Value {
}
