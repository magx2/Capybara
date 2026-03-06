package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.LinkedType;

public sealed interface LinkedExpression permits LinkedBooleanValue,
        LinkedFloatValue,
        LinkedFunctionCall,
        LinkedIfExpression,
        LinkedInfixExpression,
        LinkedIntValue,
        LinkedLetExpression,
        LinkedMatchExpression,
        LinkedNewDict,
        LinkedNewList,
        LinkedNewSet,
        LinkedNewData,
        LinkedStringValue,
        LinkedVariable {
    LinkedType type();
}
