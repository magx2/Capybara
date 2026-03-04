package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.GenericDataType;
import pl.grzeslowski.capybara.linker.LinkedFunction;
import pl.grzeslowski.capybara.linker.LinkedType;
import pl.grzeslowski.capybara.linker.ValueOrError;
import pl.grzeslowski.capybara.parser.*;

import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BOOL;
import static pl.grzeslowski.capybara.linker.expression.CapybaraTypeFinder.findHigherType;

public class CapybaraExpressionLinker {
    private static final Logger LOG = Logger.getLogger(CapybaraExpressionLinker.class.getName());
    private final List<LinkedFunction.LinkedFunctionParameter> parameters;
    private final Map<String, GenericDataType> dataTypes;

    public CapybaraExpressionLinker(List<LinkedFunction.LinkedFunctionParameter> parameters, Map<String, GenericDataType> dataTypes) {
        this.parameters = parameters;
        this.dataTypes = dataTypes;
    }

    public ValueOrError<LinkedExpression> linkExpression(Expression expression) {
        return linkExpression(expression, Scope.EMPTY);
    }

    private ValueOrError<LinkedExpression> linkExpression(Expression expression, Scope scope) {
        return switch (expression) {
            case BooleanValue booleanValue -> linkBooleanValue(booleanValue, scope);
            case FloatValue floatValue -> linkFloatValue(floatValue, scope);
            case FunctionCall functionCall -> linkFunctionCall(functionCall, scope);
            case IfExpression ifExpression -> linkIfExpression(ifExpression, scope);
            case InfixExpression infixExpression -> linkInfixExpression(infixExpression, scope);
            case IntValue intValue -> linkIntValue(intValue, scope);
            case MatchExpression matchExpression -> linkMatchExpression(matchExpression, scope);
            case NewData newData -> linkNewData(newData, scope);
            case StringValue stringValue -> linkStringValue(stringValue, scope);
            case Value value -> linkValue(value, scope);
            //
            case LetExpression letExpression -> linkLetExpression(letExpression, scope);
        };
    }

    private ValueOrError<LinkedExpression> linkBooleanValue(BooleanValue booleanValue, Scope scope) {
        return new ValueOrError.Value<>(
                switch (booleanValue) {
                    case TRUE -> LinkedBooleanValue.TRUE;
                    case FALSE -> LinkedBooleanValue.FALSE;
                });
    }

    private ValueOrError<LinkedExpression> linkFloatValue(FloatValue floatValue, Scope scope) {
        return new ValueOrError.Value<>(new LinkedFloatValue(floatValue.floatValue()));
    }

    private ValueOrError<LinkedExpression> linkFunctionCall(FunctionCall functionCall, Scope scope) {
        // todo check if types matche data declaration
        // todo check if there is enough assignments
        return functionCall.arguments()
                .stream()
                .map((Expression expression) -> linkExpression(expression, scope))
                .reduce(
                        (ValueOrError<List<LinkedExpression>>) new ValueOrError.Value<List<LinkedExpression>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join)
                .map(args -> new LinkedFunctionCall(
                        functionCall.name(),
                        args, // todo has to check if args are correct
                        ANY // todo has to check proper function return type
                ));
    }

    private ValueOrError<LinkedExpression> linkIfExpression(IfExpression ifExpression, Scope scope) {
        return linkExpression(ifExpression.condition(), scope)
                .flatMap(c -> {
                    if (c.type() != BOOL) {
                        return new ValueOrError.Error<>("condition in if statement has to have type `" + BOOL + "`, was `" + c.type() + "`");
                    }
                    return linkExpression(ifExpression.thenBranch(), scope)
                            .flatMap(t ->
                                    linkExpression(ifExpression.elseBranch(), scope)
                                            .map(e -> new LinkedIfExpression(c, t, e, ANY)));
                });
    }

    private ValueOrError<LinkedExpression> linkInfixExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left ->
                        linkExpression(expression.right(), scope)
                                .map(right ->
                                        getLinkedInfixExpression(left, expression.operator(), right)));
    }

    private static LinkedInfixExpression getLinkedInfixExpression(LinkedExpression left, InfixOperator operator, LinkedExpression right) {
        LinkedType type = switch (operator) {
            case PLUS, MINUS, MUL, DIV, CARET -> findHigherType(left.type(), right.type());
            // bool operators
            case GT, LT, EQUAL, NOTEQUAL, LE, GE -> BOOL;
        };
        return new LinkedInfixExpression(left, operator, right, type);
    }

    private ValueOrError<LinkedExpression> linkIntValue(IntValue intValue, Scope scope) {
        return new ValueOrError.Value<>(new LinkedIntValue(intValue.intValue()));
    }

    private ValueOrError<LinkedExpression> linkMatchExpression(MatchExpression matchExpression, Scope scope) {
        throw new UnsupportedOperationException("CapybaraExpressionLinker.linkMatchExpression(matchExpression)");
    }

    private ValueOrError<LinkedExpression> linkNewData(NewData newData, Scope scope) {
        return linkType(newData.type(), dataTypes)
                .flatMap(type ->
                        linkFieldAssignment(newData.assignments(), scope)
                                .map(assignments ->
                                        new LinkedNewData(
                                                type,
                                                assignments)));
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> linkFieldAssignment(List<NewData.FieldAssignment> assignments, Scope scope) {
        // todo check if types matche data declaration
        // todo check if there is enough assignments
        return assignments.stream()
                .map(a -> linkExpression(a.value(), scope)
                        .map(ex -> new LinkedNewData.FieldAssignment(a.name(), ex)))
                .reduce(
                        (ValueOrError<List<LinkedNewData.FieldAssignment>>) new ValueOrError.Value<List<LinkedNewData.FieldAssignment>>(List.of()),
                        ValueOrError::joinWithList,
                        ValueOrError::join);
    }

    private ValueOrError<LinkedExpression> linkStringValue(StringValue value, Scope scope) {
        return new ValueOrError.Value<>(new LinkedStringValue(value.stringValue()));
    }

    private ValueOrError<LinkedExpression> linkValue(Value value, Scope scope) {
        if (scope.localValues().containsKey(value.name())) {
            // found local value
            // has to check if there were a rewrite
            String finalName;
            if (scope.variableNameToUniqueName().containsKey(value.name())) {
                finalName = scope.variableNameToUniqueName().get(value.name());
            } else {
                finalName = value.name();
            }
            LOG.fine("Value `" + value.name() + "` is already defined in local scope. Renaming it to `" + finalName + "`");
            return new ValueOrError.Value<>(new LinkedVariable(finalName, scope.localValues().get(value.name())));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(value.name()))
                .findAny()
                .map(p -> new LinkedVariable(p.name(), p.type()))
                .map(v -> (ValueOrError<LinkedExpression>) new ValueOrError.Value<LinkedExpression>(v))
                .orElseGet(() -> new ValueOrError.Error<>("Variable " + value.name() + " not found"));
    }

    private ValueOrError<LinkedExpression> linkLetExpression(LetExpression expression, Scope scope) {
        return linkExpression(expression.value(), scope)
                .flatMap(value ->
                        linkExpression(expression.rest(), scope.add(expression.name(), value.type()))
                                .map(rest ->
                                        new LinkedLetExpression(
                                                expression.name(),
                                                value,
                                                rest
                                        )));
    }
}
