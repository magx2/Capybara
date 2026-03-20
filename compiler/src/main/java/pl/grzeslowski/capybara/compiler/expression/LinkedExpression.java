package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.LinkedType;

public sealed interface LinkedExpression permits LinkedBooleanValue,
        LinkedByteValue,
        LinkedDoubleValue,
        LinkedFieldAccess,
        LinkedFloatValue,
        LinkedFunctionCall,
        LinkedFunctionInvoke,
        LinkedIfExpression,
        LinkedIndexExpression,
        LinkedInfixExpression,
        LinkedIntValue,
        LinkedLambdaExpression,
        LinkedLetExpression,
        LinkedLongValue,
        LinkedMatchExpression,
        LinkedNothingValue,
        LinkedPipeAllExpression,
        LinkedPipeAnyExpression,
        LinkedPipeFlatMapExpression,
        LinkedPipeFilterOutExpression,
        LinkedPipeExpression,
        LinkedPipeReduceExpression,
        LinkedSliceExpression,
        LinkedTupleExpression,
        LinkedNewDict,
        LinkedNewList,
        LinkedNewSet,
        LinkedNewData,
        LinkedStringValue,
        LinkedVariable {
    LinkedType type();
}
