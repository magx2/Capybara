package dev.capylang.generator.java;

import dev.capylang.compiler.expression.*;

public class ValueNameRewriter {
    public static CompiledExpression rewriteValueInExpression(String name, String uniqueName, CompiledExpression expression) {
        return switch (expression) {
            case CompiledBooleanValue linkedBooleanValue -> linkedBooleanValue;
            case CompiledByteValue linkedByteValue -> linkedByteValue;
            case CompiledDoubleValue linkedDoubleValue -> linkedDoubleValue;
            case CompiledFieldAccess linkedFieldAccess -> rewriteValueInLinkedFieldAccess(name, uniqueName, linkedFieldAccess);
            case CompiledFloatValue linkedFloatValue -> linkedFloatValue;
            case CompiledFunctionCall linkedFunctionCall ->
                    rewriteValueInLinkedFunctionCall(name, uniqueName, linkedFunctionCall);
            case CompiledFunctionInvoke linkedFunctionInvoke ->
                    rewriteValueInLinkedFunctionInvoke(name, uniqueName, linkedFunctionInvoke);
            case CompiledIfExpression linkedIfExpression ->
                    rewriteValueInLinkedIfExpression(name, uniqueName, linkedIfExpression);
            case CompiledIndexExpression linkedIndexExpression ->
                    rewriteValueInLinkedIndexExpression(name, uniqueName, linkedIndexExpression);
            case CompiledInfixExpression linkedInfixExpression ->
                    rewriteValueInLinkedInfixExpression(name, uniqueName, linkedInfixExpression);
            case CompiledIntValue linkedIntValue -> linkedIntValue;
            case CompiledLambdaExpression linkedLambdaExpression ->
                    rewriteValueInLinkedLambdaExpression(name, uniqueName, linkedLambdaExpression);
            case CompiledLetExpression linkedLetExpression ->
                    rewriteValueInLinkedLetExpression(name, uniqueName, linkedLetExpression);
            case CompiledLongValue linkedLongValue -> linkedLongValue;
            case CompiledMatchExpression linkedMatchExpression ->
                    rewriteValueInLinkedMatchExpression(name, uniqueName, linkedMatchExpression);
            case CompiledNothingValue linkedNothingValue -> linkedNothingValue;
            case CompiledPipeAllExpression linkedPipeAllExpression ->
                    rewriteValueInLinkedPipeAllExpression(name, uniqueName, linkedPipeAllExpression);
            case CompiledPipeAnyExpression linkedPipeAnyExpression ->
                    rewriteValueInLinkedPipeAnyExpression(name, uniqueName, linkedPipeAnyExpression);
            case CompiledPipeFlatMapExpression linkedPipeFlatMapExpression ->
                    rewriteValueInLinkedPipeFlatMapExpression(name, uniqueName, linkedPipeFlatMapExpression);
            case CompiledPipeFilterOutExpression linkedPipeFilterOutExpression ->
                    rewriteValueInLinkedPipeFilterOutExpression(name, uniqueName, linkedPipeFilterOutExpression);
            case CompiledPipeExpression linkedPipeExpression ->
                    rewriteValueInLinkedPipeExpression(name, uniqueName, linkedPipeExpression);
            case CompiledPipeReduceExpression linkedPipeReduceExpression ->
                    rewriteValueInLinkedPipeReduceExpression(name, uniqueName, linkedPipeReduceExpression);
            case CompiledSliceExpression linkedSliceExpression ->
                    rewriteValueInLinkedSliceExpression(name, uniqueName, linkedSliceExpression);
            case CompiledTupleExpression linkedTupleExpression ->
                    rewriteValueInLinkedTupleExpression(name, uniqueName, linkedTupleExpression);
            case CompiledNewDict linkedNewDict -> rewriteValueInLinkedNewDict(name, uniqueName, linkedNewDict);
            case CompiledNewList linkedNewList -> rewriteValueInLinkedNewList(name, uniqueName, linkedNewList);
            case CompiledNewSet linkedNewSet -> rewriteValueInLinkedNewSet(name, uniqueName, linkedNewSet);
            case CompiledNewData linkedNewData -> rewriteValueInLinkedNewData(name, uniqueName, linkedNewData);
            case CompiledStringValue linkedStringValue -> linkedStringValue;
            case CompiledVariable linkedVariable -> rewriteValueInLinkedVariable(name, uniqueName, linkedVariable);
        };
    }

    private static CompiledExpression rewriteValueInLinkedFunctionCall(String name, String uniqueName, CompiledFunctionCall expression) {
        return new CompiledFunctionCall(
                expression.name(),
                expression.arguments().stream()
                        .map(ar -> rewriteValueInExpression(name, uniqueName, ar))
                        .toList(),
                expression.returnType()
        );
    }

    private static CompiledExpression rewriteValueInLinkedFunctionInvoke(String name, String uniqueName, CompiledFunctionInvoke expression) {
        return new CompiledFunctionInvoke(
                rewriteValueInExpression(name, uniqueName, expression.function()),
                expression.arguments().stream()
                        .map(ar -> rewriteValueInExpression(name, uniqueName, ar))
                        .toList(),
                expression.returnType()
        );
    }

    private static CompiledExpression rewriteValueInLinkedFieldAccess(String name, String uniqueName, CompiledFieldAccess expression) {
        return new CompiledFieldAccess(
                rewriteValueInExpression(name, uniqueName, expression.source()),
                expression.field(),
                expression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedIfExpression(String name, String uniqueName, CompiledIfExpression expression) {
        return new CompiledIfExpression(
                rewriteValueInExpression(name, uniqueName, expression.condition()),
                rewriteValueInExpression(name, uniqueName, expression.thenBranch()),
                rewriteValueInExpression(name, uniqueName, expression.elseBranch()),
                expression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedInfixExpression(String name, String uniqueName, CompiledInfixExpression expression) {
        return new CompiledInfixExpression(
                rewriteValueInExpression(name, uniqueName, expression.left()),
                expression.operator(),
                rewriteValueInExpression(name, uniqueName, expression.right()),
                expression.type());
    }

    private static CompiledExpression rewriteValueInLinkedIndexExpression(
            String name,
            String uniqueName,
            CompiledIndexExpression expression
    ) {
        return new CompiledIndexExpression(
                rewriteValueInExpression(name, uniqueName, expression.source()),
                rewriteValueInExpression(name, uniqueName, expression.index()),
                expression.elementType(),
                expression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedLambdaExpression(String name, String uniqueName, CompiledLambdaExpression expression) {
        if (expression.argumentName().equals(name)) {
            return expression;
        }
        return new CompiledLambdaExpression(
                expression.argumentName(),
                rewriteValueInExpression(name, uniqueName, expression.expression()),
                expression.functionType()
        );
    }


    private static CompiledExpression rewriteValueInLinkedLetExpression(String name, String uniqueName, CompiledLetExpression expression) {
        if (expression.name().equals(name)) {
            return new CompiledLetExpression(
                    uniqueName,
                    expression.value(),
                    expression.declaredType(),
                    rewriteValueInExpression(name, uniqueName, expression.rest()));
        }

        return new CompiledLetExpression(
                expression.name(),
                expression.value(),
                expression.declaredType(),
                rewriteValueInExpression(name, uniqueName, expression.rest()));
    }

    private static CompiledExpression rewriteValueInLinkedMatchExpression(String name, String uniqueName, CompiledMatchExpression linkedMatchExpression) {
        // todo
        return linkedMatchExpression;
    }

    private static CompiledExpression rewriteValueInLinkedPipeExpression(String name, String uniqueName, CompiledPipeExpression linkedPipeExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeExpression.source());
        if (linkedPipeExpression.argumentName().equals(name)) {
            return new CompiledPipeExpression(source, linkedPipeExpression.argumentName(), linkedPipeExpression.mapper(), linkedPipeExpression.type());
        }
        return new CompiledPipeExpression(
                source,
                linkedPipeExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeExpression.mapper()),
                linkedPipeExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedPipeFlatMapExpression(String name, String uniqueName,
                                                                               CompiledPipeFlatMapExpression linkedPipeFlatMapExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeFlatMapExpression.source());
        if (linkedPipeFlatMapExpression.argumentName().equals(name)) {
            return new CompiledPipeFlatMapExpression(source, linkedPipeFlatMapExpression.argumentName(),
                    linkedPipeFlatMapExpression.mapper(), linkedPipeFlatMapExpression.type());
        }
        return new CompiledPipeFlatMapExpression(
                source,
                linkedPipeFlatMapExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeFlatMapExpression.mapper()),
                linkedPipeFlatMapExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedPipeFilterOutExpression(String name, String uniqueName,
                                                                                 CompiledPipeFilterOutExpression linkedPipeFilterOutExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeFilterOutExpression.source());
        if (linkedPipeFilterOutExpression.argumentName().equals(name)) {
            return new CompiledPipeFilterOutExpression(
                    source,
                    linkedPipeFilterOutExpression.argumentName(),
                    linkedPipeFilterOutExpression.predicate(),
                    linkedPipeFilterOutExpression.type()
            );
        }
        return new CompiledPipeFilterOutExpression(
                source,
                linkedPipeFilterOutExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeFilterOutExpression.predicate()),
                linkedPipeFilterOutExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedPipeReduceExpression(String name, String uniqueName,
                                                                             CompiledPipeReduceExpression linkedPipeReduceExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeReduceExpression.source());
        var initialValue = rewriteValueInExpression(name, uniqueName, linkedPipeReduceExpression.initialValue());
        var dictReduceArgs = linkedPipeReduceExpression.accumulatorName().contains("::")
                ? linkedPipeReduceExpression.accumulatorName().split("::", 2)
                : new String[0];
        if (linkedPipeReduceExpression.accumulatorName().equals(name)
            || (dictReduceArgs.length == 2 && (dictReduceArgs[0].equals(name) || dictReduceArgs[1].equals(name)))
            || linkedPipeReduceExpression.valueName().equals(name)
            || linkedPipeReduceExpression.keyName().filter(name::equals).isPresent()) {
            return new CompiledPipeReduceExpression(
                    source,
                    initialValue,
                    linkedPipeReduceExpression.accumulatorName(),
                    linkedPipeReduceExpression.keyName(),
                    linkedPipeReduceExpression.valueName(),
                    linkedPipeReduceExpression.reducerExpression(),
                    linkedPipeReduceExpression.type()
            );
        }
        return new CompiledPipeReduceExpression(
                source,
                initialValue,
                linkedPipeReduceExpression.accumulatorName(),
                linkedPipeReduceExpression.keyName(),
                linkedPipeReduceExpression.valueName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeReduceExpression.reducerExpression()),
                linkedPipeReduceExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedNewList(String name, String uniqueName, CompiledNewList linkedNewList) {
        return new CompiledNewList(
                linkedNewList.values().stream()
                        .map(ex -> rewriteValueInExpression(name, uniqueName, ex))
                        .toList(),
                linkedNewList.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedNewSet(String name, String uniqueName, CompiledNewSet linkedNewSet) {
        return new CompiledNewSet(
                linkedNewSet.values().stream()
                        .map(ex -> rewriteValueInExpression(name, uniqueName, ex))
                        .toList(),
                linkedNewSet.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedNewDict(String name, String uniqueName, CompiledNewDict linkedNewDict) {
        return new CompiledNewDict(
                linkedNewDict.entries().stream()
                        .map(entry -> new CompiledNewDict.Entry(
                                rewriteValueInExpression(name, uniqueName, entry.key()),
                                rewriteValueInExpression(name, uniqueName, entry.value())
                        ))
                        .toList(),
                linkedNewDict.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedNewData(String name, String uniqueName, CompiledNewData linkedNewData) {
        // todo
        return linkedNewData;
    }

    private static CompiledExpression rewriteValueInLinkedPipeAnyExpression(
            String name,
            String uniqueName,
            CompiledPipeAnyExpression linkedPipeAnyExpression
    ) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeAnyExpression.source());
        if (linkedPipeAnyExpression.argumentName().equals(name)) {
            return new CompiledPipeAnyExpression(
                    source,
                    linkedPipeAnyExpression.argumentName(),
                    linkedPipeAnyExpression.predicate(),
                    linkedPipeAnyExpression.type()
            );
        }
        return new CompiledPipeAnyExpression(
                source,
                linkedPipeAnyExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeAnyExpression.predicate()),
                linkedPipeAnyExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedPipeAllExpression(
            String name,
            String uniqueName,
            CompiledPipeAllExpression linkedPipeAllExpression
    ) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeAllExpression.source());
        if (linkedPipeAllExpression.argumentName().equals(name)) {
            return new CompiledPipeAllExpression(
                    source,
                    linkedPipeAllExpression.argumentName(),
                    linkedPipeAllExpression.predicate(),
                    linkedPipeAllExpression.type()
            );
        }
        return new CompiledPipeAllExpression(
                source,
                linkedPipeAllExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeAllExpression.predicate()),
                linkedPipeAllExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedSliceExpression(
            String name,
            String uniqueName,
            CompiledSliceExpression linkedSliceExpression
    ) {
        return new CompiledSliceExpression(
                rewriteValueInExpression(name, uniqueName, linkedSliceExpression.source()),
                linkedSliceExpression.start().map(ex -> rewriteValueInExpression(name, uniqueName, ex)),
                linkedSliceExpression.end().map(ex -> rewriteValueInExpression(name, uniqueName, ex)),
                linkedSliceExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedTupleExpression(
            String name,
            String uniqueName,
            CompiledTupleExpression linkedTupleExpression
    ) {
        return new CompiledTupleExpression(
                linkedTupleExpression.values().stream()
                        .map(ex -> rewriteValueInExpression(name, uniqueName, ex))
                        .toList(),
                linkedTupleExpression.type()
        );
    }

    private static CompiledExpression rewriteValueInLinkedVariable(String name, String uniqueName, CompiledVariable linkedVariable) {
        if (linkedVariable.name().equals(name)) {
            return new CompiledVariable(uniqueName, linkedVariable.type());
        }
        return linkedVariable;
    }
}
