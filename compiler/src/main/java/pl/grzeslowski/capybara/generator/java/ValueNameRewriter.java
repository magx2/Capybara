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
