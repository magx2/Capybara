package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.compiler.expression.*;

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
            case LinkedByteValue linkedByteValue -> printLinkedByteValue(linkedByteValue, level);
            case LinkedDoubleValue linkedDoubleValue -> printLinkedDoubleValue(linkedDoubleValue, level);
            case LinkedFieldAccess linkedFieldAccess -> printLinkedFieldAccess(linkedFieldAccess, level);
            case LinkedFloatValue linkedFloatValue -> printLinkedFloatValue(linkedFloatValue, level);
            case LinkedFunctionCall linkedFunctionCall -> printLinkedFunctionCall(linkedFunctionCall, level);
            case LinkedFunctionInvoke linkedFunctionInvoke -> printLinkedFunctionInvoke(linkedFunctionInvoke, level);
            case LinkedIfExpression linkedIfExpression -> printLinkedIfExpression(linkedIfExpression, level);
            case LinkedIndexExpression linkedIndexExpression -> printLinkedIndexExpression(linkedIndexExpression, level);
            case LinkedInfixExpression linkedInfixExpression ->
                    printLinkedInfixExpression(linkedInfixExpression, level);
            case LinkedIntValue linkedIntValue -> printLinkedIntValue(linkedIntValue, level);
            case LinkedLambdaExpression linkedLambdaExpression -> printLinkedLambdaExpression(linkedLambdaExpression, level);
            case LinkedLetExpression linkedLetExpression -> printLinkedLetExpression(linkedLetExpression, level);
            case LinkedLongValue linkedLongValue -> printLinkedLongValue(linkedLongValue, level);
            case LinkedMatchExpression linkedMatchExpression ->
                    printLinkedMatchExpression(linkedMatchExpression, level);
            case LinkedNothingValue linkedNothingValue -> printLinkedNothingValue(linkedNothingValue, level);
            case LinkedPipeAllExpression linkedPipeAllExpression -> printLinkedPipeAllExpression(linkedPipeAllExpression, level);
            case LinkedPipeAnyExpression linkedPipeAnyExpression -> printLinkedPipeAnyExpression(linkedPipeAnyExpression, level);
            case LinkedPipeFlatMapExpression linkedPipeFlatMapExpression -> printLinkedPipeFlatMapExpression(linkedPipeFlatMapExpression, level);
            case LinkedPipeFilterOutExpression linkedPipeFilterOutExpression -> printLinkedPipeFilterOutExpression(linkedPipeFilterOutExpression, level);
            case LinkedPipeExpression linkedPipeExpression -> printLinkedPipeExpression(linkedPipeExpression, level);
            case LinkedPipeReduceExpression linkedPipeReduceExpression -> printLinkedPipeReduceExpression(linkedPipeReduceExpression, level);
            case LinkedSliceExpression linkedSliceExpression -> printLinkedSliceExpression(linkedSliceExpression, level);
            case LinkedTupleExpression linkedTupleExpression -> printLinkedTupleExpression(linkedTupleExpression, level);
            case LinkedNewDict linkedNewDict -> printLinkedNewDict(linkedNewDict, level);
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

    private static String printLinkedByteValue(LinkedByteValue linkedByteValue, int level) {
        return linkedByteValue.byteValue();
    }

    private static String printLinkedDoubleValue(LinkedDoubleValue linkedDoubleValue, int level) {
        return linkedDoubleValue.doubleValue();
    }

    private static String printLinkedFloatValue(LinkedFloatValue linkedFloatValue, int level) {
        return linkedFloatValue.floatValue();
    }

    private static String printLinkedFieldAccess(LinkedFieldAccess linkedFieldAccess, int level) {
        return printExpression(linkedFieldAccess.source(), level) + "." + linkedFieldAccess.field();
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

    private static String printLinkedFunctionInvoke(LinkedFunctionInvoke linkedFunctionInvoke, int level) {
        return printExpression(linkedFunctionInvoke.function(), level)
               + "(\n"
               + linkedFunctionInvoke.arguments().stream().map(ex -> printExpression(ex, level + 1)).collect(joining())
               + LINE_BREAK + tabs(level + 1) + "): " + linkedFunctionInvoke.returnType();
    }

    private static String printLinkedInfixExpression(LinkedInfixExpression linkedInfixExpression, int level) {
        return printExpression(linkedInfixExpression.left(), level)
               + " " + linkedInfixExpression.operator().symbol()
               + " " + printExpression(linkedInfixExpression.right(), level);
    }

    private static String printLinkedIndexExpression(LinkedIndexExpression linkedIndexExpression, int level) {
        return printExpression(linkedIndexExpression.source(), level)
               + "[" + printExpression(linkedIndexExpression.index(), level) + "]";
    }

    private static String printLinkedIntValue(LinkedIntValue linkedIntValue, int level) {
        return linkedIntValue.intValue();
    }

    private static String printLinkedLongValue(LinkedLongValue linkedLongValue, int level) {
        return linkedLongValue.longValue();
    }

    private static String printLinkedLambdaExpression(LinkedLambdaExpression linkedLambdaExpression, int level) {
        return linkedLambdaExpression.argumentName()
               + " => "
               + printExpression(linkedLambdaExpression.expression(), level);
    }

    private static String printLinkedLetExpression(LinkedLetExpression linkedLetExpression, int level) {
        return LINE_BREAK + tabs(level) + "let " + linkedLetExpression.name() + " = " + printExpression(linkedLetExpression.value(), level + 1)
               + LINE_BREAK + tabs(level) + printExpression(linkedLetExpression.rest(), level + 1);
    }

    private static String printLinkedMatchExpression(LinkedMatchExpression linkedMatchExpression, int level) {
        return LINE_BREAK + tabs(level) + "match";
    }

    private static String printLinkedPipeExpression(LinkedPipeExpression linkedPipeExpression, int level) {
        return printExpression(linkedPipeExpression.source(), level)
               + " | "
               + linkedPipeExpression.argumentName()
               + " => "
               + printExpression(linkedPipeExpression.mapper(), level);
    }

    private static String printLinkedPipeAnyExpression(LinkedPipeAnyExpression linkedPipeAnyExpression, int level) {
        return printExpression(linkedPipeAnyExpression.source(), level)
               + " |any? "
               + linkedPipeAnyExpression.argumentName()
               + " => "
               + printExpression(linkedPipeAnyExpression.predicate(), level);
    }

    private static String printLinkedPipeAllExpression(LinkedPipeAllExpression linkedPipeAllExpression, int level) {
        return printExpression(linkedPipeAllExpression.source(), level)
               + " |all? "
               + linkedPipeAllExpression.argumentName()
               + " => "
               + printExpression(linkedPipeAllExpression.predicate(), level);
    }

    private static String printLinkedPipeFilterOutExpression(LinkedPipeFilterOutExpression linkedPipeFilterOutExpression, int level) {
        return printExpression(linkedPipeFilterOutExpression.source(), level)
               + " |- "
               + linkedPipeFilterOutExpression.argumentName()
               + " => "
               + printExpression(linkedPipeFilterOutExpression.predicate(), level);
    }

    private static String printLinkedPipeFlatMapExpression(LinkedPipeFlatMapExpression linkedPipeFlatMapExpression, int level) {
        return printExpression(linkedPipeFlatMapExpression.source(), level)
               + " |* "
               + linkedPipeFlatMapExpression.argumentName()
               + " => "
               + printExpression(linkedPipeFlatMapExpression.mapper(), level);
    }

    private static String printLinkedPipeReduceExpression(LinkedPipeReduceExpression linkedPipeReduceExpression, int level) {
        var args = linkedPipeReduceExpression.keyName()
                .map(keyName -> linkedPipeReduceExpression.accumulatorName() + "," + keyName + "," + linkedPipeReduceExpression.valueName())
                .orElse(linkedPipeReduceExpression.accumulatorName() + "," + linkedPipeReduceExpression.valueName());
        return printExpression(linkedPipeReduceExpression.source(), level)
               + " |> "
               + printExpression(linkedPipeReduceExpression.initialValue(), level)
               + ", (" + args + ") => "
               + printExpression(linkedPipeReduceExpression.reducerExpression(), level);
    }

    private static String printLinkedSliceExpression(LinkedSliceExpression linkedSliceExpression, int level) {
        var from = linkedSliceExpression.start().map(fromEx -> printExpression(fromEx, level)).orElse("");
        var to = linkedSliceExpression.end().map(toEx -> printExpression(toEx, level)).orElse("");
        return printExpression(linkedSliceExpression.source(), level) + "[" + from + ":" + to + "]";
    }

    private static String printLinkedTupleExpression(LinkedTupleExpression linkedTupleExpression, int level) {
        return "(" + linkedTupleExpression.values().stream()
                .map(ex -> printExpression(ex, level + 1))
                .collect(joining(", ")) + ")";
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

    private static String printLinkedNewDict(LinkedNewDict linkedNewDict, int level) {
        return "{" + linkedNewDict.entries().stream()
                .map(entry -> printExpression(entry.key(), level + 1) + ": " + printExpression(entry.value(), level + 1))
                .collect(joining(", ")) + "}";
    }

    private static String printLinkedStringValue(LinkedStringValue linkedStringValue, int level) {
        return linkedStringValue.stringValue();
    }

    private static String printLinkedVariable(LinkedVariable linkedVariable, int level) {
        return "(" + linkedVariable.name() + ": " + linkedVariable.type() + ")";
    }

    private static String printLinkedNothingValue(LinkedNothingValue linkedNothingValue, int level) {
        return "??? (" + linkedNothingValue.message() + linkedNothingValue.position().map(p -> " @ " + p.line() + ":" + p.column()).orElse("") + ")";
    }

    private static String tabs(int level) {
        return "\t".repeat(Math.max(0, level));
    }
}
