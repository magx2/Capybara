package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.parser.*;

import static java.util.stream.Collectors.joining;
import static pl.grzeslowski.capybara.parser.GenericOperator.CARET;

public class JavaExpressionEvaluator {
    public static String evaluateExpression(Expression expression) {
        return switch (expression) {
            case BoolExpression boolExpression -> evaluateBoolExpression(boolExpression);
            case FloatValue floatValue -> evaluateFloatValue(floatValue);
            case FunctionCall functionCall -> evaluateFunctionCall(functionCall);
            case IfExpression ifExpression -> evaluateIfExpression(ifExpression);
            case InfixExpression<?> infixExpression -> evaluateInfixExpression(infixExpression);
            case IntValue intValue -> evaluateIntValue(intValue);
            case MatchExpression matchExpression -> evaluateMatchExpression(matchExpression);
            case NewData newData -> evaluateNewData(newData);
            case StringValue stringValue -> evaluateStringValue(stringValue);
            case Variable variable -> evaluateVariable(variable);
        };

    }

    private static String evaluateBoolExpression(BoolExpression expression) {
        return switch (expression) {
            case BoolInfixExpression boolInfixExpression -> evaluateBoolInfixExpression(boolInfixExpression);
            case BooleanValue booleanValue -> evaluateBooleanValue(booleanValue);
        };
    }

    private static String evaluateBoolInfixExpression(BoolInfixExpression expression) {
        return evaluateExpression(expression.left())
               + " "
               + expression.operator().symbol()
               + " "
               + evaluateExpression(expression.right());
    }

    private static String evaluateBooleanValue(BooleanValue expression) {
        return switch (expression) {
            case TRUE -> "true";
            case FALSE -> "false";
        };
    }

    private static String evaluateFloatValue(FloatValue expression) {
        throw new UnsupportedOperationException("WIP");
    }

    private static String evaluateFunctionCall(FunctionCall expression) {
        String arguments = expression.arguments().stream().map(JavaExpressionEvaluator::evaluateExpression).collect(joining(", "));
        return expression.name() + "(" +
               arguments +
               ")";
    }

    private static String evaluateIfExpression(IfExpression expression) {
        return "(" +
               evaluateBoolExpression(expression.condition()) +
               ") ? (" +
               evaluateExpression(expression.thenBranch()) +
               ") : (" +
               evaluateExpression(expression.elseBranch()) +
               ")";
    }

    private static String evaluateInfixExpression(InfixExpression<?> expression) {
        if (expression.operator() == CARET) {
            return "pl.grzeslowski.capybara.CapybaraUtil.power(" +
                   evaluateExpression(expression.left()) +
                   "," +
                   evaluateExpression(expression.right()) +
                   ")";
        }
        return evaluateExpression(expression.left())
               + " " + expression.operator().symbol()
               + " " + evaluateExpression(expression.right());

    }

    private static String evaluateIntValue(IntValue expression) {
        return expression.intValue();
    }

    private static String evaluateMatchExpression(MatchExpression expression) {
        throw new UnsupportedOperationException("WIP");
    }

    private static String evaluateNewData(NewData expression) {
        throw new UnsupportedOperationException("WIP");
    }

    private static String evaluateStringValue(StringValue stringValue) {
        return stringValue.stringValue();
    }

    private static String evaluateVariable(Variable variable) {
        return variable.name();
    }
}
