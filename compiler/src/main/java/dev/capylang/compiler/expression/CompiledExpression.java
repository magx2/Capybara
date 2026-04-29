package dev.capylang.compiler.expression;

import dev.capylang.compiler.CompiledType;

public sealed interface CompiledExpression permits CompiledBooleanValue,
        CompiledByteValue,
        CompiledDoubleValue,
        CompiledEffectBindExpression,
        CompiledEffectExpression,
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
        CompiledNumericWidening,
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
