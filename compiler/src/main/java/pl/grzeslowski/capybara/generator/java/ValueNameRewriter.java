package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.linker.expression.*;

public class ValueNameRewriter {
    public static LinkedExpression rewriteValueInExpression(String name, String uniqueName, LinkedExpression expression) {
        return switch (expression) {
            case LinkedBooleanValue linkedBooleanValue -> linkedBooleanValue;
            case LinkedByteValue linkedByteValue -> linkedByteValue;
            case LinkedDoubleValue linkedDoubleValue -> linkedDoubleValue;
            case LinkedFieldAccess linkedFieldAccess -> rewriteValueInLinkedFieldAccess(name, uniqueName, linkedFieldAccess);
            case LinkedFloatValue linkedFloatValue -> linkedFloatValue;
            case LinkedFunctionCall linkedFunctionCall ->
                    rewriteValueInLinkedFunctionCall(name, uniqueName, linkedFunctionCall);
            case LinkedFunctionInvoke linkedFunctionInvoke ->
                    rewriteValueInLinkedFunctionInvoke(name, uniqueName, linkedFunctionInvoke);
            case LinkedIfExpression linkedIfExpression ->
                    rewriteValueInLinkedIfExpression(name, uniqueName, linkedIfExpression);
            case LinkedIndexExpression linkedIndexExpression ->
                    rewriteValueInLinkedIndexExpression(name, uniqueName, linkedIndexExpression);
            case LinkedInfixExpression linkedInfixExpression ->
                    rewriteValueInLinkedInfixExpression(name, uniqueName, linkedInfixExpression);
            case LinkedIntValue linkedIntValue -> linkedIntValue;
            case LinkedLambdaExpression linkedLambdaExpression ->
                    rewriteValueInLinkedLambdaExpression(name, uniqueName, linkedLambdaExpression);
            case LinkedLetExpression linkedLetExpression ->
                    rewriteValueInLinkedLetExpression(name, uniqueName, linkedLetExpression);
            case LinkedLongValue linkedLongValue -> linkedLongValue;
            case LinkedMatchExpression linkedMatchExpression ->
                    rewriteValueInLinkedMatchExpression(name, uniqueName, linkedMatchExpression);
            case LinkedNothingValue linkedNothingValue -> linkedNothingValue;
            case LinkedPipeAllExpression linkedPipeAllExpression ->
                    rewriteValueInLinkedPipeAllExpression(name, uniqueName, linkedPipeAllExpression);
            case LinkedPipeAnyExpression linkedPipeAnyExpression ->
                    rewriteValueInLinkedPipeAnyExpression(name, uniqueName, linkedPipeAnyExpression);
            case LinkedPipeFlatMapExpression linkedPipeFlatMapExpression ->
                    rewriteValueInLinkedPipeFlatMapExpression(name, uniqueName, linkedPipeFlatMapExpression);
            case LinkedPipeFilterOutExpression linkedPipeFilterOutExpression ->
                    rewriteValueInLinkedPipeFilterOutExpression(name, uniqueName, linkedPipeFilterOutExpression);
            case LinkedPipeExpression linkedPipeExpression ->
                    rewriteValueInLinkedPipeExpression(name, uniqueName, linkedPipeExpression);
            case LinkedPipeReduceExpression linkedPipeReduceExpression ->
                    rewriteValueInLinkedPipeReduceExpression(name, uniqueName, linkedPipeReduceExpression);
            case LinkedSliceExpression linkedSliceExpression ->
                    rewriteValueInLinkedSliceExpression(name, uniqueName, linkedSliceExpression);
            case LinkedTupleExpression linkedTupleExpression ->
                    rewriteValueInLinkedTupleExpression(name, uniqueName, linkedTupleExpression);
            case LinkedNewDict linkedNewDict -> rewriteValueInLinkedNewDict(name, uniqueName, linkedNewDict);
            case LinkedNewList linkedNewList -> rewriteValueInLinkedNewList(name, uniqueName, linkedNewList);
            case LinkedNewSet linkedNewSet -> rewriteValueInLinkedNewSet(name, uniqueName, linkedNewSet);
            case LinkedNewData linkedNewData -> rewriteValueInLinkedNewData(name, uniqueName, linkedNewData);
            case LinkedStringValue linkedStringValue -> linkedStringValue;
            case LinkedVariable linkedVariable -> rewriteValueInLinkedVariable(name, uniqueName, linkedVariable);
        };
    }

    private static LinkedExpression rewriteValueInLinkedFunctionCall(String name, String uniqueName, LinkedFunctionCall expression) {
        return new LinkedFunctionCall(
                expression.name(),
                expression.arguments().stream()
                        .map(ar -> rewriteValueInExpression(name, uniqueName, ar))
                        .toList(),
                expression.returnType()
        );
    }

    private static LinkedExpression rewriteValueInLinkedFunctionInvoke(String name, String uniqueName, LinkedFunctionInvoke expression) {
        return new LinkedFunctionInvoke(
                rewriteValueInExpression(name, uniqueName, expression.function()),
                expression.arguments().stream()
                        .map(ar -> rewriteValueInExpression(name, uniqueName, ar))
                        .toList(),
                expression.returnType()
        );
    }

    private static LinkedExpression rewriteValueInLinkedFieldAccess(String name, String uniqueName, LinkedFieldAccess expression) {
        return new LinkedFieldAccess(
                rewriteValueInExpression(name, uniqueName, expression.source()),
                expression.field(),
                expression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedIfExpression(String name, String uniqueName, LinkedIfExpression expression) {
        return new LinkedIfExpression(
                rewriteValueInExpression(name, uniqueName, expression.condition()),
                rewriteValueInExpression(name, uniqueName, expression.thenBranch()),
                rewriteValueInExpression(name, uniqueName, expression.elseBranch()),
                expression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedInfixExpression(String name, String uniqueName, LinkedInfixExpression expression) {
        return new LinkedInfixExpression(
                rewriteValueInExpression(name, uniqueName, expression.left()),
                expression.operator(),
                rewriteValueInExpression(name, uniqueName, expression.right()),
                expression.type());
    }

    private static LinkedExpression rewriteValueInLinkedIndexExpression(
            String name,
            String uniqueName,
            LinkedIndexExpression expression
    ) {
        return new LinkedIndexExpression(
                rewriteValueInExpression(name, uniqueName, expression.source()),
                rewriteValueInExpression(name, uniqueName, expression.index()),
                expression.elementType(),
                expression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedLambdaExpression(String name, String uniqueName, LinkedLambdaExpression expression) {
        if (expression.argumentName().equals(name)) {
            return expression;
        }
        return new LinkedLambdaExpression(
                expression.argumentName(),
                rewriteValueInExpression(name, uniqueName, expression.expression()),
                expression.functionType()
        );
    }


    private static LinkedExpression rewriteValueInLinkedLetExpression(String name, String uniqueName, LinkedLetExpression expression) {
        if (expression.name().equals(name)) {
            return new LinkedLetExpression(
                    uniqueName,
                    expression.value(),
                    rewriteValueInExpression(name, uniqueName, expression.rest()));
        }

        return new LinkedLetExpression(
                expression.name(),
                expression.value(),
                rewriteValueInExpression(name, uniqueName, expression.rest()));
    }

    private static LinkedExpression rewriteValueInLinkedMatchExpression(String name, String uniqueName, LinkedMatchExpression linkedMatchExpression) {
        // todo
        return linkedMatchExpression;
    }

    private static LinkedExpression rewriteValueInLinkedPipeExpression(String name, String uniqueName, LinkedPipeExpression linkedPipeExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeExpression.source());
        if (linkedPipeExpression.argumentName().equals(name)) {
            return new LinkedPipeExpression(source, linkedPipeExpression.argumentName(), linkedPipeExpression.mapper(), linkedPipeExpression.type());
        }
        return new LinkedPipeExpression(
                source,
                linkedPipeExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeExpression.mapper()),
                linkedPipeExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedPipeFlatMapExpression(String name, String uniqueName,
                                                                               LinkedPipeFlatMapExpression linkedPipeFlatMapExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeFlatMapExpression.source());
        if (linkedPipeFlatMapExpression.argumentName().equals(name)) {
            return new LinkedPipeFlatMapExpression(source, linkedPipeFlatMapExpression.argumentName(),
                    linkedPipeFlatMapExpression.mapper(), linkedPipeFlatMapExpression.type());
        }
        return new LinkedPipeFlatMapExpression(
                source,
                linkedPipeFlatMapExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeFlatMapExpression.mapper()),
                linkedPipeFlatMapExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedPipeFilterOutExpression(String name, String uniqueName,
                                                                                 LinkedPipeFilterOutExpression linkedPipeFilterOutExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeFilterOutExpression.source());
        if (linkedPipeFilterOutExpression.argumentName().equals(name)) {
            return new LinkedPipeFilterOutExpression(
                    source,
                    linkedPipeFilterOutExpression.argumentName(),
                    linkedPipeFilterOutExpression.predicate(),
                    linkedPipeFilterOutExpression.type()
            );
        }
        return new LinkedPipeFilterOutExpression(
                source,
                linkedPipeFilterOutExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeFilterOutExpression.predicate()),
                linkedPipeFilterOutExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedPipeReduceExpression(String name, String uniqueName,
                                                                             LinkedPipeReduceExpression linkedPipeReduceExpression) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeReduceExpression.source());
        var initialValue = rewriteValueInExpression(name, uniqueName, linkedPipeReduceExpression.initialValue());
        var dictReduceArgs = linkedPipeReduceExpression.accumulatorName().contains("::")
                ? linkedPipeReduceExpression.accumulatorName().split("::", 2)
                : new String[0];
        if (linkedPipeReduceExpression.accumulatorName().equals(name)
            || (dictReduceArgs.length == 2 && (dictReduceArgs[0].equals(name) || dictReduceArgs[1].equals(name)))
            || linkedPipeReduceExpression.valueName().equals(name)
            || linkedPipeReduceExpression.keyName().filter(name::equals).isPresent()) {
            return new LinkedPipeReduceExpression(
                    source,
                    initialValue,
                    linkedPipeReduceExpression.accumulatorName(),
                    linkedPipeReduceExpression.keyName(),
                    linkedPipeReduceExpression.valueName(),
                    linkedPipeReduceExpression.reducerExpression(),
                    linkedPipeReduceExpression.type()
            );
        }
        return new LinkedPipeReduceExpression(
                source,
                initialValue,
                linkedPipeReduceExpression.accumulatorName(),
                linkedPipeReduceExpression.keyName(),
                linkedPipeReduceExpression.valueName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeReduceExpression.reducerExpression()),
                linkedPipeReduceExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedNewList(String name, String uniqueName, LinkedNewList linkedNewList) {
        return new LinkedNewList(
                linkedNewList.values().stream()
                        .map(ex -> rewriteValueInExpression(name, uniqueName, ex))
                        .toList(),
                linkedNewList.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedNewSet(String name, String uniqueName, LinkedNewSet linkedNewSet) {
        return new LinkedNewSet(
                linkedNewSet.values().stream()
                        .map(ex -> rewriteValueInExpression(name, uniqueName, ex))
                        .toList(),
                linkedNewSet.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedNewDict(String name, String uniqueName, LinkedNewDict linkedNewDict) {
        return new LinkedNewDict(
                linkedNewDict.entries().stream()
                        .map(entry -> new LinkedNewDict.Entry(
                                rewriteValueInExpression(name, uniqueName, entry.key()),
                                rewriteValueInExpression(name, uniqueName, entry.value())
                        ))
                        .toList(),
                linkedNewDict.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedNewData(String name, String uniqueName, LinkedNewData linkedNewData) {
        // todo
        return linkedNewData;
    }

    private static LinkedExpression rewriteValueInLinkedPipeAnyExpression(
            String name,
            String uniqueName,
            LinkedPipeAnyExpression linkedPipeAnyExpression
    ) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeAnyExpression.source());
        if (linkedPipeAnyExpression.argumentName().equals(name)) {
            return new LinkedPipeAnyExpression(
                    source,
                    linkedPipeAnyExpression.argumentName(),
                    linkedPipeAnyExpression.predicate(),
                    linkedPipeAnyExpression.type()
            );
        }
        return new LinkedPipeAnyExpression(
                source,
                linkedPipeAnyExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeAnyExpression.predicate()),
                linkedPipeAnyExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedPipeAllExpression(
            String name,
            String uniqueName,
            LinkedPipeAllExpression linkedPipeAllExpression
    ) {
        var source = rewriteValueInExpression(name, uniqueName, linkedPipeAllExpression.source());
        if (linkedPipeAllExpression.argumentName().equals(name)) {
            return new LinkedPipeAllExpression(
                    source,
                    linkedPipeAllExpression.argumentName(),
                    linkedPipeAllExpression.predicate(),
                    linkedPipeAllExpression.type()
            );
        }
        return new LinkedPipeAllExpression(
                source,
                linkedPipeAllExpression.argumentName(),
                rewriteValueInExpression(name, uniqueName, linkedPipeAllExpression.predicate()),
                linkedPipeAllExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedSliceExpression(
            String name,
            String uniqueName,
            LinkedSliceExpression linkedSliceExpression
    ) {
        return new LinkedSliceExpression(
                rewriteValueInExpression(name, uniqueName, linkedSliceExpression.source()),
                linkedSliceExpression.start().map(ex -> rewriteValueInExpression(name, uniqueName, ex)),
                linkedSliceExpression.end().map(ex -> rewriteValueInExpression(name, uniqueName, ex)),
                linkedSliceExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedTupleExpression(
            String name,
            String uniqueName,
            LinkedTupleExpression linkedTupleExpression
    ) {
        return new LinkedTupleExpression(
                linkedTupleExpression.values().stream()
                        .map(ex -> rewriteValueInExpression(name, uniqueName, ex))
                        .toList(),
                linkedTupleExpression.type()
        );
    }

    private static LinkedExpression rewriteValueInLinkedVariable(String name, String uniqueName, LinkedVariable linkedVariable) {
        if (linkedVariable.name().equals(name)) {
            return new LinkedVariable(uniqueName, linkedVariable.type());
        }
        return linkedVariable;
    }
}
