package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.linker.expression.*;

public class ValueNameRewriter {
    public static LinkedExpression rewriteValueInExpression(String name, String uniqueName, LinkedExpression expression) {
        return switch (expression) {
            case LinkedBooleanValue linkedBooleanValue -> linkedBooleanValue;
            case LinkedFloatValue linkedFloatValue -> linkedFloatValue;
            case LinkedFunctionCall linkedFunctionCall ->
                    rewriteValueInLinkedFunctionCall(name, uniqueName, linkedFunctionCall);
            case LinkedIfExpression linkedIfExpression ->
                    rewriteValueInLinkedIfExpression(name, uniqueName, linkedIfExpression);
            case LinkedInfixExpression linkedInfixExpression ->
                    rewriteValueInLinkedInfixExpression(name, uniqueName, linkedInfixExpression);
            case LinkedIntValue linkedIntValue -> linkedIntValue;
            case LinkedLetExpression linkedLetExpression ->
                    rewriteValueInLinkedLetExpression(name, uniqueName, linkedLetExpression);
            case LinkedMatchExpression linkedMatchExpression ->
                    rewriteValueInLinkedMatchExpression(name, uniqueName, linkedMatchExpression);
            case LinkedPipeFlatMapExpression linkedPipeFlatMapExpression ->
                    rewriteValueInLinkedPipeFlatMapExpression(name, uniqueName, linkedPipeFlatMapExpression);
            case LinkedPipeFilterOutExpression linkedPipeFilterOutExpression ->
                    rewriteValueInLinkedPipeFilterOutExpression(name, uniqueName, linkedPipeFilterOutExpression);
            case LinkedPipeExpression linkedPipeExpression ->
                    rewriteValueInLinkedPipeExpression(name, uniqueName, linkedPipeExpression);
            case LinkedPipeReduceExpression linkedPipeReduceExpression ->
                    rewriteValueInLinkedPipeReduceExpression(name, uniqueName, linkedPipeReduceExpression);
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
        if (linkedPipeReduceExpression.accumulatorName().equals(name) || linkedPipeReduceExpression.valueName().equals(name)) {
            return new LinkedPipeReduceExpression(
                    source,
                    initialValue,
                    linkedPipeReduceExpression.accumulatorName(),
                    linkedPipeReduceExpression.valueName(),
                    linkedPipeReduceExpression.reducerExpression(),
                    linkedPipeReduceExpression.type()
            );
        }
        return new LinkedPipeReduceExpression(
                source,
                initialValue,
                linkedPipeReduceExpression.accumulatorName(),
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

    private static LinkedExpression rewriteValueInLinkedVariable(String name, String uniqueName, LinkedVariable linkedVariable) {
        if (linkedVariable.name().equals(name)) {
            return new LinkedVariable(uniqueName, linkedVariable.type());
        }
        return linkedVariable;
    }
}
