package pl.grzeslowski.capybara.compiler.expression;

import pl.grzeslowski.capybara.compiler.CompiledType;

public sealed interface CompiledExpression permits CompiledBooleanValue,
        CompiledByteValue,
        CompiledDoubleValue,
        CompiledFieldAccess,
        CompiledFloatValue,
        CompiledFunctionCall,
        CompiledFunctionInvoke,
        CompiledIfExpression,
        CompiledIndexExpression,
        CompiledInfixExpression,
        CompiledIntValue,
        CompiledLambdaExpression,
        CompiledLetExpression,
        CompiledLongValue,
        CompiledMatchExpression,
        CompiledNothingValue,
        CompiledPipeAllExpression,
        CompiledPipeAnyExpression,
        CompiledPipeFlatMapExpression,
        CompiledPipeFilterOutExpression,
        CompiledPipeExpression,
        CompiledPipeReduceExpression,
        CompiledSliceExpression,
        CompiledTupleExpression,
        CompiledNewDict,
        CompiledNewList,
        CompiledNewSet,
        CompiledNewData,
        CompiledStringValue,
        CompiledVariable {
    CompiledType type();
}
