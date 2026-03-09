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
        LinkedIndexExpression,
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
        LinkedSliceExpression,
        LinkedNewDict,
        LinkedNewList,
        LinkedNewSet,
        LinkedNewData,
        LinkedStringValue,
        LinkedVariable {
    LinkedType type();
}
