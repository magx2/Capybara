package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.parser.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;
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
            case NewListExpression newListExpression -> linkNewListExpression(newListExpression, scope);
            case NewData newData -> linkNewData(newData, scope);
            case StringValue stringValue -> linkStringValue(stringValue, scope);
            case Value value -> linkValue(value, scope);
            //
            case LetExpression letExpression -> linkLetExpression(letExpression, scope);
        };
    }

    private ValueOrError<LinkedExpression> linkBooleanValue(BooleanValue booleanValue, Scope scope) {
        return ValueOrError.success(booleanValue.value() ? LinkedBooleanValue.TRUE : LinkedBooleanValue.FALSE);
    }

    private ValueOrError<LinkedExpression> linkFloatValue(FloatValue floatValue, Scope scope) {
        return ValueOrError.success(new LinkedFloatValue(floatValue.floatValue()));
    }

    private ValueOrError<LinkedExpression> linkFunctionCall(FunctionCall functionCall, Scope scope) {
        // todo check if types matche data declaration
        // todo check if there is enough assignments
        return functionCall.arguments()
                .stream()
                .map((Expression expression) -> linkExpression(expression, scope))
                .collect(new ValueOrErrorCollectionCollector<>())
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
                        return withPosition(
                                ValueOrError.error("condition in if statement has to have type `" + BOOL + "`, was `" + c.type() + "`"),
                                ifExpression.condition().position()
                        );
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
            case PLUS, MINUS, MUL, DIV, CARET, POWER -> findHigherType(left.type(), right.type());
            // bool operators
            case GT, LT, EQUAL, NOTEQUAL, LE, GE -> BOOL;
        };
        return new LinkedInfixExpression(left, operator, right, type);
    }

    private ValueOrError<LinkedExpression> linkIntValue(IntValue intValue, Scope scope) {
        return ValueOrError.success(new LinkedIntValue(intValue.intValue()));
    }

    private ValueOrError<LinkedExpression> linkMatchExpression(MatchExpression matchExpression, Scope scope) {
        throw new UnsupportedOperationException("CapybaraExpressionLinker.linkMatchExpression(matchExpression)");
    }

    private ValueOrError<LinkedExpression> linkNewListExpression(NewListExpression expression, Scope scope) {
        return expression.values().stream()
                .map(value -> linkExpression(value, scope))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(values -> {
                    var elementType = values.stream()
                            .map(LinkedExpression::type)
                            .reduce(CapybaraTypeFinder::findHigherType)
                            .orElse(ANY);
                    return (LinkedExpression) new LinkedNewList(values, new LinkedList(elementType));
                });
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
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedExpression> linkStringValue(StringValue value, Scope scope) {
        return ValueOrError.success(new LinkedStringValue(value.stringValue()));
    }

    private ValueOrError<LinkedExpression> linkValue(Value value, Scope scope) {
        if (scope.localValues().containsKey(value.name())) {
            // found local value
            // has to check if there were a rewrite
            String finalName;
//            if (scope.variableNameToUniqueName().containsKey(value.name())) {
//                finalName = scope.variableNameToUniqueName().get(value.name());
//            } else {
            finalName = value.name();
//            }
            LOG.fine("Value `" + value.name() + "` is already defined in local scope. Renaming it to `" + finalName + "`");
            return ValueOrError.success(new LinkedVariable(finalName, scope.localValues().get(value.name())));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(value.name()))
                .findAny()
                .map(p -> new LinkedVariable(p.name(), p.type()))
                .map(ValueOrError::<LinkedExpression>success)
                .orElseGet(() -> withPosition(
                        ValueOrError.error("Variable " + value.name() + " not found"),
                        value.position()
                ));
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

    private static <T> ValueOrError<T> withPosition(ValueOrError<T> valueOrError, Optional<pl.grzeslowski.capybara.parser.SourcePosition> position) {
        if (valueOrError instanceof ValueOrError.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new ValueOrError.Error<>(error.errors()
                    .stream()
                    .map(ValueOrError.Error.SingleError::message)
                    .map(msg -> "line %d, column %d: %s".formatted(pos.line(), pos.column(), msg))
                    .map(ValueOrError.Error.SingleError::new)
                    .toList());
        }
        return valueOrError;
    }
}
