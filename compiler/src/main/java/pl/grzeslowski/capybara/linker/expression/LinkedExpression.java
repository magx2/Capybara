package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public sealed interface LinkedExpression permits LinkedBooleanValue,
        LinkedByteValue,
        LinkedDoubleValue,
        LinkedFieldAccess,
        LinkedFloatValue,
        LinkedFunctionCall,
        LinkedFunctionInvoke,
        LinkedIfExpression,
        LinkedInfixExpression,
        LinkedIntValue,
        LinkedLambdaExpression,
        LinkedLetExpression,
        LinkedLongValue,
        LinkedMatchExpression,
        LinkedNothingValue,
        LinkedPipeFlatMapExpression,
        LinkedPipeFilterOutExpression,
        LinkedPipeExpression,
        LinkedPipeReduceExpression,
        LinkedNewDict,
        LinkedNewList,
        LinkedNewSet,
        LinkedNewData,
        LinkedStringValue,
        LinkedVariable {
    LinkedType type();
}
