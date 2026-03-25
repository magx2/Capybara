package dev.capylang.compiler.parser;

import java.util.Optional;

public sealed interface Expression permits BooleanValue, ByteValue, DoubleValue, FieldAccess, FloatValue, FunctionCall, FunctionInvoke, FunctionReference, IfExpression, IndexExpression, InfixExpression, IntValue, LambdaExpression, LetExpression, LongValue, MatchExpression, NewData, NewDictExpression, NewListExpression, NewSetExpression, NothingValue, ReduceExpression, SliceExpression, StringValue, TupleExpression, Value {
    Optional<SourcePosition> position();
}

