package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.linker.expression.*;

import static java.lang.System.lineSeparator;
import static java.util.stream.Collectors.joining;

public class LinkedExpressionPrinter {
    public static final String LINE_BREAK = lineSeparator();

    public static void printExpression(LinkedExpression expression) {
        System.out.print("Your expression:");
        System.out.println(printExpression(expression, 0));
    }

    public static String printExpression(LinkedExpression expression, int level) {
        return switch (expression) {
            case LinkedBooleanValue linkedBooleanValue -> printLinkedBooleanValue(linkedBooleanValue, level);
            case LinkedFloatValue linkedFloatValue -> printLinkedFloatValue(linkedFloatValue, level);
            case LinkedFunctionCall linkedFunctionCall -> printLinkedFunctionCall(linkedFunctionCall, level);
            case LinkedIfExpression linkedIfExpression -> printLinkedIfExpression(linkedIfExpression, level);
            case LinkedInfixExpression linkedInfixExpression ->
                    printLinkedInfixExpression(linkedInfixExpression, level);
            case LinkedIntValue linkedIntValue -> printLinkedIntValue(linkedIntValue, level);
            case LinkedLetExpression linkedLetExpression -> printLinkedLetExpression(linkedLetExpression, level);
            case LinkedMatchExpression linkedMatchExpression ->
                    printLinkedMatchExpression(linkedMatchExpression, level);
            case LinkedNewList linkedNewList -> printLinkedNewList(linkedNewList, level);
            case LinkedNewSet linkedNewSet -> printLinkedNewSet(linkedNewSet, level);
            case LinkedNewData linkedNewData -> printLinkedNewData(linkedNewData, level);
            case LinkedStringValue linkedStringValue -> printLinkedStringValue(linkedStringValue, level);
            case LinkedVariable linkedVariable -> printLinkedVariable(linkedVariable, level);
        };
    }

    private static String printLinkedBooleanValue(LinkedBooleanValue linkedBooleanValue, int level) {
        return linkedBooleanValue.name();
    }

    private static String printLinkedFloatValue(LinkedFloatValue linkedFloatValue, int level) {
        return linkedFloatValue.floatValue();
    }

    private static String printLinkedFunctionCall(LinkedFunctionCall linkedFunctionCall, int level) {
        return LINE_BREAK + tabs(level) + linkedFunctionCall.name() + "(\n"
               + linkedFunctionCall.arguments().stream().map(ex -> printExpression(ex, level + 1)).collect(joining())
               + LINE_BREAK + tabs(level + 1) + "): " + linkedFunctionCall.returnType();
    }

    private static String printLinkedIfExpression(LinkedIfExpression linkedIfExpression, int level) {
        return LINE_BREAK + tabs(level) + "if"
               + printExpression(linkedIfExpression.condition(), level + 1)
               + LINE_BREAK + tabs(level) + "then"
               + printExpression(linkedIfExpression.thenBranch(), level + 1)
               + LINE_BREAK + tabs(level) + "else"
               + printExpression(linkedIfExpression.elseBranch(), level + 1);
    }

    private static String printLinkedInfixExpression(LinkedInfixExpression linkedInfixExpression, int level) {
        return printExpression(linkedInfixExpression.left(), level)
               + " " + linkedInfixExpression.operator().symbol()
               + " " + printExpression(linkedInfixExpression.right(), level);
    }

    private static String printLinkedIntValue(LinkedIntValue linkedIntValue, int level) {
        return linkedIntValue.intValue();
    }

    private static String printLinkedLetExpression(LinkedLetExpression linkedLetExpression, int level) {
        return LINE_BREAK + tabs(level) + "let " + linkedLetExpression.name() + " = " + printExpression(linkedLetExpression.value(), level + 1)
               + LINE_BREAK + tabs(level) + printExpression(linkedLetExpression.rest(), level + 1);
    }

    private static String printLinkedMatchExpression(LinkedMatchExpression linkedMatchExpression, int level) {
        return LINE_BREAK + tabs(level) + "match";
    }

    private static String printLinkedNewData(LinkedNewData linkedNewData, int level) {
        return LINE_BREAK + tabs(level) + linkedNewData.type() + "{"
               + linkedNewData.assignments().stream().map(fa -> printExpression(fa.value(), level + 1) + " / " + fa.name()).collect(joining())
               + LINE_BREAK + tabs(level) + "}";
    }

    private static String printLinkedNewList(LinkedNewList linkedNewList, int level) {
        return "[" + linkedNewList.values().stream().map(ex -> printExpression(ex, level + 1)).collect(joining(", ")) + "]";
    }

    private static String printLinkedNewSet(LinkedNewSet linkedNewSet, int level) {
        return "{" + linkedNewSet.values().stream().map(ex -> printExpression(ex, level + 1)).collect(joining(", ")) + "}";
    }

    private static String printLinkedStringValue(LinkedStringValue linkedStringValue, int level) {
        return linkedStringValue.stringValue();
    }

    private static String printLinkedVariable(LinkedVariable linkedVariable, int level) {
        return "(" + linkedVariable.name() + ": " + linkedVariable.type() + ")";
    }

    private static String tabs(int level) {
        return "\t".repeat(Math.max(0, level));
    }
}
