package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.compiler.expression.*;

import static java.lang.System.lineSeparator;
import static java.util.stream.Collectors.joining;

public class CompiledExpressionPrinter {
    public static final String LINE_BREAK = lineSeparator();

    public static void printExpression(CompiledExpression expression) {
        System.out.print("Your expression:");
        System.out.println(printExpression(expression, 0));
    }

    public static String printExpression(CompiledExpression expression, int level) {
        return switch (expression) {
            case CompiledBooleanValue linkedBooleanValue -> printLinkedBooleanValue(linkedBooleanValue, level);
            case CompiledByteValue linkedByteValue -> printLinkedByteValue(linkedByteValue, level);
            case CompiledDoubleValue linkedDoubleValue -> printLinkedDoubleValue(linkedDoubleValue, level);
            case CompiledFieldAccess linkedFieldAccess -> printLinkedFieldAccess(linkedFieldAccess, level);
            case CompiledFloatValue linkedFloatValue -> printLinkedFloatValue(linkedFloatValue, level);
            case CompiledFunctionCall linkedFunctionCall -> printLinkedFunctionCall(linkedFunctionCall, level);
            case CompiledFunctionInvoke linkedFunctionInvoke -> printLinkedFunctionInvoke(linkedFunctionInvoke, level);
            case CompiledIfExpression linkedIfExpression -> printLinkedIfExpression(linkedIfExpression, level);
            case CompiledIndexExpression linkedIndexExpression -> printLinkedIndexExpression(linkedIndexExpression, level);
            case CompiledInfixExpression linkedInfixExpression ->
                    printLinkedInfixExpression(linkedInfixExpression, level);
            case CompiledIntValue linkedIntValue -> printLinkedIntValue(linkedIntValue, level);
            case CompiledLambdaExpression linkedLambdaExpression -> printLinkedLambdaExpression(linkedLambdaExpression, level);
            case CompiledLetExpression linkedLetExpression -> printLinkedLetExpression(linkedLetExpression, level);
            case CompiledLongValue linkedLongValue -> printLinkedLongValue(linkedLongValue, level);
            case CompiledMatchExpression linkedMatchExpression ->
                    printLinkedMatchExpression(linkedMatchExpression, level);
            case CompiledNothingValue linkedNothingValue -> printLinkedNothingValue(linkedNothingValue, level);
            case CompiledPipeAllExpression linkedPipeAllExpression -> printLinkedPipeAllExpression(linkedPipeAllExpression, level);
            case CompiledPipeAnyExpression linkedPipeAnyExpression -> printLinkedPipeAnyExpression(linkedPipeAnyExpression, level);
            case CompiledPipeFlatMapExpression linkedPipeFlatMapExpression -> printLinkedPipeFlatMapExpression(linkedPipeFlatMapExpression, level);
            case CompiledPipeFilterOutExpression linkedPipeFilterOutExpression -> printLinkedPipeFilterOutExpression(linkedPipeFilterOutExpression, level);
            case CompiledPipeExpression linkedPipeExpression -> printLinkedPipeExpression(linkedPipeExpression, level);
            case CompiledPipeReduceExpression linkedPipeReduceExpression -> printLinkedPipeReduceExpression(linkedPipeReduceExpression, level);
            case CompiledSliceExpression linkedSliceExpression -> printLinkedSliceExpression(linkedSliceExpression, level);
            case CompiledTupleExpression linkedTupleExpression -> printLinkedTupleExpression(linkedTupleExpression, level);
            case CompiledNewDict linkedNewDict -> printLinkedNewDict(linkedNewDict, level);
            case CompiledNewList linkedNewList -> printLinkedNewList(linkedNewList, level);
            case CompiledNewSet linkedNewSet -> printLinkedNewSet(linkedNewSet, level);
            case CompiledNewData linkedNewData -> printLinkedNewData(linkedNewData, level);
            case CompiledStringValue linkedStringValue -> printLinkedStringValue(linkedStringValue, level);
            case CompiledVariable linkedVariable -> printLinkedVariable(linkedVariable, level);
        };
    }

    private static String printLinkedBooleanValue(CompiledBooleanValue linkedBooleanValue, int level) {
        return linkedBooleanValue.name();
    }

    private static String printLinkedByteValue(CompiledByteValue linkedByteValue, int level) {
        return linkedByteValue.byteValue();
    }

    private static String printLinkedDoubleValue(CompiledDoubleValue linkedDoubleValue, int level) {
        return linkedDoubleValue.doubleValue();
    }

    private static String printLinkedFloatValue(CompiledFloatValue linkedFloatValue, int level) {
        return linkedFloatValue.floatValue();
    }

    private static String printLinkedFieldAccess(CompiledFieldAccess linkedFieldAccess, int level) {
        return printExpression(linkedFieldAccess.source(), level) + "." + linkedFieldAccess.field();
    }

    private static String printLinkedFunctionCall(CompiledFunctionCall linkedFunctionCall, int level) {
        return LINE_BREAK + tabs(level) + linkedFunctionCall.name() + "(\n"
               + linkedFunctionCall.arguments().stream().map(ex -> printExpression(ex, level + 1)).collect(joining())
               + LINE_BREAK + tabs(level + 1) + "): " + linkedFunctionCall.returnType();
    }

    private static String printLinkedIfExpression(CompiledIfExpression linkedIfExpression, int level) {
        return LINE_BREAK + tabs(level) + "if"
               + printExpression(linkedIfExpression.condition(), level + 1)
               + LINE_BREAK + tabs(level) + "then"
               + printExpression(linkedIfExpression.thenBranch(), level + 1)
               + LINE_BREAK + tabs(level) + "else"
               + printExpression(linkedIfExpression.elseBranch(), level + 1);
    }

    private static String printLinkedFunctionInvoke(CompiledFunctionInvoke linkedFunctionInvoke, int level) {
        return printExpression(linkedFunctionInvoke.function(), level)
               + "(\n"
               + linkedFunctionInvoke.arguments().stream().map(ex -> printExpression(ex, level + 1)).collect(joining())
               + LINE_BREAK + tabs(level + 1) + "): " + linkedFunctionInvoke.returnType();
    }

    private static String printLinkedInfixExpression(CompiledInfixExpression linkedInfixExpression, int level) {
        return printExpression(linkedInfixExpression.left(), level)
               + " " + linkedInfixExpression.operator().symbol()
               + " " + printExpression(linkedInfixExpression.right(), level);
    }

    private static String printLinkedIndexExpression(CompiledIndexExpression linkedIndexExpression, int level) {
        return printExpression(linkedIndexExpression.source(), level)
               + "[" + printExpression(linkedIndexExpression.index(), level) + "]";
    }

    private static String printLinkedIntValue(CompiledIntValue linkedIntValue, int level) {
        return linkedIntValue.intValue();
    }

    private static String printLinkedLongValue(CompiledLongValue linkedLongValue, int level) {
        return linkedLongValue.longValue();
    }

    private static String printLinkedLambdaExpression(CompiledLambdaExpression linkedLambdaExpression, int level) {
        return linkedLambdaExpression.argumentName()
               + " => "
               + printExpression(linkedLambdaExpression.expression(), level);
    }

    private static String printLinkedLetExpression(CompiledLetExpression linkedLetExpression, int level) {
        return LINE_BREAK + tabs(level) + "let " + linkedLetExpression.name() + " = " + printExpression(linkedLetExpression.value(), level + 1)
               + LINE_BREAK + tabs(level) + printExpression(linkedLetExpression.rest(), level + 1);
    }

    private static String printLinkedMatchExpression(CompiledMatchExpression linkedMatchExpression, int level) {
        return LINE_BREAK + tabs(level) + "match";
    }

    private static String printLinkedPipeExpression(CompiledPipeExpression linkedPipeExpression, int level) {
        return printExpression(linkedPipeExpression.source(), level)
               + " | "
               + linkedPipeExpression.argumentName()
               + " => "
               + printExpression(linkedPipeExpression.mapper(), level);
    }

    private static String printLinkedPipeAnyExpression(CompiledPipeAnyExpression linkedPipeAnyExpression, int level) {
        return printExpression(linkedPipeAnyExpression.source(), level)
               + " |any? "
               + linkedPipeAnyExpression.argumentName()
               + " => "
               + printExpression(linkedPipeAnyExpression.predicate(), level);
    }

    private static String printLinkedPipeAllExpression(CompiledPipeAllExpression linkedPipeAllExpression, int level) {
        return printExpression(linkedPipeAllExpression.source(), level)
               + " |all? "
               + linkedPipeAllExpression.argumentName()
               + " => "
               + printExpression(linkedPipeAllExpression.predicate(), level);
    }

    private static String printLinkedPipeFilterOutExpression(CompiledPipeFilterOutExpression linkedPipeFilterOutExpression, int level) {
        return printExpression(linkedPipeFilterOutExpression.source(), level)
               + " |- "
               + linkedPipeFilterOutExpression.argumentName()
               + " => "
               + printExpression(linkedPipeFilterOutExpression.predicate(), level);
    }

    private static String printLinkedPipeFlatMapExpression(CompiledPipeFlatMapExpression linkedPipeFlatMapExpression, int level) {
        return printExpression(linkedPipeFlatMapExpression.source(), level)
               + " |* "
               + linkedPipeFlatMapExpression.argumentName()
               + " => "
               + printExpression(linkedPipeFlatMapExpression.mapper(), level);
    }

    private static String printLinkedPipeReduceExpression(CompiledPipeReduceExpression linkedPipeReduceExpression, int level) {
        var args = linkedPipeReduceExpression.keyName()
                .map(keyName -> linkedPipeReduceExpression.accumulatorName() + "," + keyName + "," + linkedPipeReduceExpression.valueName())
                .orElse(linkedPipeReduceExpression.accumulatorName() + "," + linkedPipeReduceExpression.valueName());
        return printExpression(linkedPipeReduceExpression.source(), level)
               + " |> "
               + printExpression(linkedPipeReduceExpression.initialValue(), level)
               + ", (" + args + ") => "
               + printExpression(linkedPipeReduceExpression.reducerExpression(), level);
    }

    private static String printLinkedSliceExpression(CompiledSliceExpression linkedSliceExpression, int level) {
        var from = linkedSliceExpression.start().map(fromEx -> printExpression(fromEx, level)).orElse("");
        var to = linkedSliceExpression.end().map(toEx -> printExpression(toEx, level)).orElse("");
        return printExpression(linkedSliceExpression.source(), level) + "[" + from + ":" + to + "]";
    }

    private static String printLinkedTupleExpression(CompiledTupleExpression linkedTupleExpression, int level) {
        return "(" + linkedTupleExpression.values().stream()
                .map(ex -> printExpression(ex, level + 1))
                .collect(joining(", ")) + ")";
    }

    private static String printLinkedNewData(CompiledNewData linkedNewData, int level) {
        return LINE_BREAK + tabs(level) + linkedNewData.type() + "{"
               + linkedNewData.assignments().stream().map(fa -> printExpression(fa.value(), level + 1) + " / " + fa.name()).collect(joining())
               + LINE_BREAK + tabs(level) + "}";
    }

    private static String printLinkedNewList(CompiledNewList linkedNewList, int level) {
        return "[" + linkedNewList.values().stream().map(ex -> printExpression(ex, level + 1)).collect(joining(", ")) + "]";
    }

    private static String printLinkedNewSet(CompiledNewSet linkedNewSet, int level) {
        return "{" + linkedNewSet.values().stream().map(ex -> printExpression(ex, level + 1)).collect(joining(", ")) + "}";
    }

    private static String printLinkedNewDict(CompiledNewDict linkedNewDict, int level) {
        return "{" + linkedNewDict.entries().stream()
                .map(entry -> printExpression(entry.key(), level + 1) + ": " + printExpression(entry.value(), level + 1))
                .collect(joining(", ")) + "}";
    }

    private static String printLinkedStringValue(CompiledStringValue linkedStringValue, int level) {
        return linkedStringValue.stringValue();
    }

    private static String printLinkedVariable(CompiledVariable linkedVariable, int level) {
        return "(" + linkedVariable.name() + ": " + linkedVariable.type() + ")";
    }

    private static String printLinkedNothingValue(CompiledNothingValue linkedNothingValue, int level) {
        return "??? (" + linkedNothingValue.message() + linkedNothingValue.position().map(p -> " @ " + p.line() + ":" + p.column()).orElse("") + ")";
    }

    private static String tabs(int level) {
        return "\t".repeat(Math.max(0, level));
    }
}
