package pl.grzeslowski.capybara.linker.expression;

import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.parser.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;

import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.ANY;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.BOOL;
import static pl.grzeslowski.capybara.linker.PrimitiveLinkedType.STRING;
import static pl.grzeslowski.capybara.linker.expression.CapybaraTypeFinder.findHigherType;

public class CapybaraExpressionLinker {
    private static final Logger LOG = Logger.getLogger(CapybaraExpressionLinker.class.getName());
    private final List<LinkedFunction.LinkedFunctionParameter> parameters;
    private final Map<String, GenericDataType> dataTypes;
    private final List<FunctionSignature> functionSignatures;

    public CapybaraExpressionLinker(
            List<LinkedFunction.LinkedFunctionParameter> parameters,
            Map<String, GenericDataType> dataTypes,
            List<FunctionSignature> functionSignatures
    ) {
        this.parameters = parameters;
        this.dataTypes = dataTypes;
        this.functionSignatures = functionSignatures;
    }

    public ValueOrError<LinkedExpression> linkExpression(Expression expression) {
        return linkExpression(expression, Scope.EMPTY);
    }

    private ValueOrError<LinkedExpression> linkExpression(Expression expression, Scope scope) {
        return switch (expression) {
            case BooleanValue booleanValue -> linkBooleanValue(booleanValue, scope);
            case FieldAccess fieldAccess -> linkFieldAccess(fieldAccess, scope);
            case FloatValue floatValue -> linkFloatValue(floatValue, scope);
            case FunctionCall functionCall -> linkFunctionCall(functionCall, scope);
            case FunctionReference functionReference -> withPosition(
                    ValueOrError.error("Function reference can only be used as the right side of `|`"),
                    functionReference.position()
            );
            case IfExpression ifExpression -> linkIfExpression(ifExpression, scope);
            case InfixExpression infixExpression -> linkInfixExpression(infixExpression, scope);
            case IntValue intValue -> linkIntValue(intValue, scope);
            case LambdaExpression lambdaExpression -> withPosition(
                    ValueOrError.error("Lambda expression can only be used as the right side of `|`, `|-` or `|*`"),
                    lambdaExpression.position()
            );
            case ReduceExpression reduceExpression -> withPosition(
                    ValueOrError.error("Reduce expression can only be used as the right side of `|>`"),
                    reduceExpression.position()
            );
            case MatchExpression matchExpression -> linkMatchExpression(matchExpression, scope);
            case NewDictExpression newDictExpression -> linkNewDictExpression(newDictExpression, scope);
            case NewListExpression newListExpression -> linkNewListExpression(newListExpression, scope);
            case NewSetExpression newSetExpression -> linkNewSetExpression(newSetExpression, scope);
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

    private ValueOrError<LinkedExpression> linkFieldAccess(FieldAccess fieldAccess, Scope scope) {
        return linkExpression(fieldAccess.source(), scope)
                .flatMap(source -> {
                    if (!(source.type() instanceof GenericDataType dataType)) {
                        return withPosition(
                                ValueOrError.error("Field access requires data type, was `" + source.type() + "`"),
                                fieldAccess.position()
                        );
                    }
                    return dataType.fields().stream()
                            .filter(field -> field.name().equals(fieldAccess.field()))
                            .findFirst()
                            .<ValueOrError<LinkedExpression>>map(field ->
                                    ValueOrError.success(new LinkedFieldAccess(source, field.name(), field.type())))
                            .orElseGet(() -> withPosition(
                                    ValueOrError.error("Field `" + fieldAccess.field() + "` not found in type `" + dataType.name() + "`"),
                                    fieldAccess.position()
                            ));
                });
    }

    private ValueOrError<LinkedExpression> linkFloatValue(FloatValue floatValue, Scope scope) {
        return ValueOrError.success(new LinkedFloatValue(floatValue.floatValue()));
    }

    private ValueOrError<LinkedExpression> linkFunctionCall(FunctionCall functionCall, Scope scope) {
        var functionVariable = resolveFunctionVariable(functionCall.name(), scope);
        if (functionVariable.isPresent()) {
            return resolveFunctionInvoke(functionCall, scope, functionVariable.get());
        }
        return resolveGlobalFunctionCall(functionCall, scope);
    }

    private ValueOrError<LinkedExpression> resolveGlobalFunctionCall(FunctionCall functionCall, Scope scope) {
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionCall.name()))
                .filter(signature -> signature.parameterTypes().size() == functionCall.arguments().size())
                .toList();
        if (candidates.isEmpty()) {
            return functionCall.arguments()
                    .stream()
                    .map((Expression expression) -> linkExpression(expression, scope))
                    .collect(new ValueOrErrorCollectionCollector<>())
                    .map(args -> (LinkedExpression) new LinkedFunctionCall(functionCall.name(), args, ANY));
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var maybeResolved = linkArgumentsForExpectedTypes(functionCall.arguments(), scope, candidate.parameterTypes());
            if (!(maybeResolved instanceof ValueOrError.Value<CoercedArguments> resolvedValue)) {
                continue;
            }
            var resolved = resolvedValue.value();
            if (best == null || resolved.coercions() < best.coercions()) {
                best = new ResolvedFunctionCall(candidate, resolved.arguments(), resolved.coercions());
            }
        }
        if (best == null) {
            var actualTypes = new java.util.ArrayList<String>();
            for (var argument : functionCall.arguments()) {
                var linked = linkExpression(argument, scope);
                if (linked instanceof ValueOrError.Value<LinkedExpression> value) {
                    actualTypes.add(value.value().type().name());
                }
            }
            return withPosition(
                    ValueOrError.error(
                            "No matching function `" + functionCall.name() + "` for argument types " + actualTypes
                    ),
                    functionCall.position()
            );
        }
        return ValueOrError.success(new LinkedFunctionCall(
                functionCall.name(),
                best.arguments(),
                best.signature().returnType()
        ));
    }

    private Optional<LinkedVariable> resolveFunctionVariable(String name, Scope scope) {
        if (scope.localValues().get(name) instanceof LinkedFunctionType functionType) {
            return Optional.of(new LinkedVariable(name, functionType));
        }
        return parameters.stream()
                .filter(parameter -> parameter.name().equals(name))
                .filter(parameter -> parameter.type() instanceof LinkedFunctionType)
                .findFirst()
                .map(parameter -> new LinkedVariable(parameter.name(), parameter.type()));
    }

    private ValueOrError<LinkedExpression> resolveFunctionInvoke(
            FunctionCall functionCall,
            Scope scope,
            LinkedVariable function
    ) {
        if (!(function.type() instanceof LinkedFunctionType functionType)) {
            return withPosition(ValueOrError.error("Variable `" + function.name() + "` is not callable"), functionCall.position());
        }
        if (functionCall.arguments().size() != 1) {
            return withPosition(
                    ValueOrError.error("Function variable `" + function.name() + "` requires exactly one argument"),
                    functionCall.position()
            );
        }

        return linkArgumentForExpectedType(functionCall.arguments().get(0), scope, functionType.argumentType())
                .map(coerced -> (LinkedExpression) new LinkedFunctionInvoke(
                        function,
                        List.of(coerced.expression()),
                        functionType.returnType()
                ));
    }

    private ValueOrError<CoercedArguments> linkArgumentsForExpectedTypes(
            List<Expression> arguments,
            Scope scope,
            List<LinkedType> expectedTypes
    ) {
        var coerced = new java.util.ArrayList<LinkedExpression>(arguments.size());
        var coercions = 0;
        for (var i = 0; i < arguments.size(); i++) {
            var argument = arguments.get(i);
            var expected = expectedTypes.get(i);
            var maybeCoerced = linkArgumentForExpectedType(argument, scope, expected);
            if (maybeCoerced instanceof ValueOrError.Error<CoercedArgument>) {
                return ValueOrError.error(List.of());
            }
            var value = ((ValueOrError.Value<CoercedArgument>) maybeCoerced).value();
            coerced.add(value.expression());
            coercions += value.coercions();
        }
        return ValueOrError.success(new CoercedArguments(List.copyOf(coerced), coercions));
    }

    private ValueOrError<CoercedArgument> linkArgumentForExpectedType(Expression argument, Scope scope, LinkedType expected) {
        if (argument instanceof LambdaExpression lambdaExpression) {
            if (expected instanceof LinkedFunctionType functionType) {
                return linkLambdaExpression(lambdaExpression, scope, functionType)
                        .map(linkedLambda -> new CoercedArgument(linkedLambda, 0));
            }
            return withPosition(
                    ValueOrError.error("Lambda expression can only be used where function type is expected"),
                    argument.position()
            );
        }

        return linkExpression(argument, scope)
                .flatMap(linkedArgument -> {
                    var maybeCoerced = coerceArgument(linkedArgument, expected);
                    if (maybeCoerced == null) {
                        return withPosition(
                                ValueOrError.error("Expected `" + expected + "`, got `" + linkedArgument.type() + "`"),
                                argument.position()
                        );
                    }
                    return ValueOrError.success(maybeCoerced);
                });
    }

    private ValueOrError<LinkedLambdaExpression> linkLambdaExpression(
            LambdaExpression lambdaExpression,
            Scope scope,
            LinkedFunctionType expectedType
    ) {
        var lambdaScope = scope.add(lambdaExpression.argumentName(), expectedType.argumentType());
        return linkExpression(lambdaExpression.expression(), lambdaScope)
                .flatMap(linkedBody -> {
                    var maybeCoerced = coerceArgument(linkedBody, expectedType.returnType());
                    if (maybeCoerced == null) {
                        return withPosition(
                                ValueOrError.error(
                                        "Lambda has to return `" + expectedType.returnType()
                                        + "`, got `" + linkedBody.type() + "`"
                                ),
                                lambdaExpression.position()
                        );
                    }
                    return ValueOrError.success(new LinkedLambdaExpression(
                            lambdaExpression.argumentName(),
                            maybeCoerced.expression(),
                            expectedType
                    ));
                });
    }

    private CoercedArgument coerceArgument(LinkedExpression argument, LinkedType expected) {
        if (argument.type().equals(expected)) {
            return new CoercedArgument(argument, 0);
        }
        if (expected instanceof LinkedDataType expectedDataType
            && argument.type() instanceof LinkedDataType argumentDataType
            && isSubtype(argumentDataType, expectedDataType.name(), new java.util.HashSet<>())) {
            var assignments = expectedDataType.fields().stream()
                    .map(field -> new LinkedNewData.FieldAssignment(
                            field.name(),
                            new LinkedFieldAccess(argument, field.name(), field.type())
                    ))
                    .toList();
            return new CoercedArgument(new LinkedNewData(expectedDataType, assignments), 1);
        }
        return null;
    }

    private boolean isSubtype(LinkedDataType candidate, String expectedTypeName, java.util.Set<String> visited) {
        if (!visited.add(candidate.name())) {
            return false;
        }
        if (candidate.extendedTypes().contains(expectedTypeName)) {
            return true;
        }
        return candidate.extendedTypes().stream()
                .map(dataTypes::get)
                .filter(LinkedDataType.class::isInstance)
                .map(LinkedDataType.class::cast)
                .anyMatch(parent -> parent.name().equals(expectedTypeName) || isSubtype(parent, expectedTypeName, visited));
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
        if (expression.operator() == InfixOperator.PIPE) {
            return linkPipeExpression(expression, scope);
        }
        if (expression.operator() == InfixOperator.PIPE_MINUS) {
            return linkPipeFilterOutExpression(expression, scope);
        }
        if (expression.operator() == InfixOperator.PIPE_FLATMAP) {
            return linkPipeFlatMapExpression(expression, scope);
        }
        if (expression.operator() == InfixOperator.PIPE_REDUCE) {
            return linkPipeReduceExpression(expression, scope);
        }
        return linkExpression(expression.left(), scope)
                .flatMap(left ->
                        linkExpression(expression.right(), scope)
                                .flatMap(right ->
                                        getLinkedInfixExpression(left, expression.operator(), right, expression.position())
                                                .map(linked -> (LinkedExpression) linked)
                                ));
    }

    private ValueOrError<LinkedExpression> linkPipeExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = switch (left.type()) {
                        case LinkedList linkedList -> linkedList.elementType();
                        case LinkedSet linkedSet -> linkedSet.elementType();
                        case LinkedDict linkedDict -> linkedDict.valueType();
                        default -> null;
                    };
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `|` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        if (expression.right() instanceof FunctionReference functionReference) {
                            return resolvePipeFunctionReference(functionReference, elementType)
                                    .map(linked -> (LinkedExpression) new LinkedPipeExpression(
                                            left,
                                            linked.argumentName(),
                                            linked.expression(),
                                            left.type() instanceof LinkedSet
                                                    ? new LinkedSet(linked.expression().type())
                                                    : new LinkedList(linked.expression().type())
                                    ));
                        }
                        return withPosition(
                                ValueOrError.error("Right side of `|` has to be a lambda expression or function reference"),
                                expression.right().position()
                        );
                    }

                    var lambdaScope = scope.add(lambdaExpression.argumentName(), elementType);
                    return linkExpression(lambdaExpression.expression(), lambdaScope)
                            .map(mapper -> (LinkedExpression) new LinkedPipeExpression(
                                    left,
                                    lambdaExpression.argumentName(),
                                    mapper,
                                    left.type() instanceof LinkedSet
                                            ? new LinkedSet(mapper.type())
                                            : new LinkedList(mapper.type())
                            ));
                });
    }

    private ValueOrError<PipeMapper> resolvePipeFunctionReference(FunctionReference functionReference, LinkedType inputType) {
        var argumentName = "it";
        var argument = new LinkedVariable(argumentName, inputType);
        var candidates = functionSignatures.stream()
                .filter(signature -> signature.name().equals(functionReference.name()))
                .filter(signature -> signature.parameterTypes().size() == 1)
                .toList();
        if (candidates.isEmpty()) {
            return withPosition(
                    ValueOrError.error("Function `" + functionReference.name() + "` with one argument not found"),
                    functionReference.position()
            );
        }

        ResolvedFunctionCall best = null;
        for (var candidate : candidates) {
            var coerced = coerceArgument(argument, candidate.parameterTypes().get(0));
            if (coerced == null) {
                continue;
            }
            var resolved = new ResolvedFunctionCall(
                    candidate,
                    List.of(coerced.expression()),
                    coerced.coercions()
            );
            if (best == null || resolved.coercions() < best.coercions()) {
                best = resolved;
            }
        }
        if (best == null) {
            return withPosition(
                    ValueOrError.error("Function reference `" + functionReference.name() + "` is not compatible with `" + inputType + "`"),
                    functionReference.position()
            );
        }
        return ValueOrError.success(new PipeMapper(
                argumentName,
                new LinkedFunctionCall(functionReference.name(), best.arguments(), best.signature().returnType())
        ));
    }

    private ValueOrError<LinkedExpression> linkPipeReduceExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = collectionElementType(left.type());
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `|>` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof ReduceExpression reduceExpression)) {
                        return withPosition(
                                ValueOrError.error("Right side of `|>` has to be a reduce expression"),
                                expression.right().position()
                        );
                    }
                    return linkExpression(reduceExpression.initialValue(), scope)
                            .flatMap(initial -> {
                                var reduceScope = scope
                                        .add(reduceExpression.accumulatorName(), initial.type())
                                        .add(reduceExpression.valueName(), elementType);
                                return linkExpression(reduceExpression.reducerExpression(), reduceScope)
                                        .flatMap(reducer -> {
                                            if (reducer.type() != initial.type()) {
                                                return withPosition(
                                                        ValueOrError.error(
                                                                "Reducer in `|>` has to return `" + initial.type() + "`, was `" + reducer.type() + "`"
                                                        ),
                                                        reduceExpression.position()
                                                );
                                            }
                                            return ValueOrError.success((LinkedExpression) new LinkedPipeReduceExpression(
                                                    left,
                                                    initial,
                                                    reduceExpression.accumulatorName(),
                                                    reduceExpression.valueName(),
                                                    reducer,
                                                    initial.type()
                                            ));
                                        });
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkPipeFlatMapExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = collectionElementType(left.type());
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `|*` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        return withPosition(
                                ValueOrError.error("Right side of `|*` has to be a lambda expression"),
                                expression.right().position()
                        );
                    }

                    var lambdaScope = scope.add(lambdaExpression.argumentName(), elementType);
                    return linkExpression(lambdaExpression.expression(), lambdaScope)
                            .flatMap(mapper -> {
                                var mappedElementType = collectionElementType(mapper.type());
                                if (mappedElementType == null) {
                                    return withPosition(
                                            ValueOrError.error("Lambda in `|*` has to return collection type, was `" + mapper.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return ValueOrError.success((LinkedExpression) new LinkedPipeFlatMapExpression(
                                        left,
                                        lambdaExpression.argumentName(),
                                        mapper,
                                        left.type() instanceof LinkedSet
                                                ? new LinkedSet(mappedElementType)
                                                : new LinkedList(mappedElementType)
                                ));
                            });
                });
    }

    private ValueOrError<LinkedExpression> linkPipeFilterOutExpression(InfixExpression expression, Scope scope) {
        return linkExpression(expression.left(), scope)
                .flatMap(left -> {
                    var elementType = switch (left.type()) {
                        case LinkedList linkedList -> linkedList.elementType();
                        case LinkedSet linkedSet -> linkedSet.elementType();
                        case LinkedDict linkedDict -> linkedDict.valueType();
                        default -> null;
                    };
                    if (elementType == null) {
                        return withPosition(
                                ValueOrError.error("Left side of `|-` has to be a collection, was `" + left.type() + "`"),
                                expression.left().position()
                        );
                    }
                    if (!(expression.right() instanceof LambdaExpression lambdaExpression)) {
                        return withPosition(
                                ValueOrError.error("Right side of `|-` has to be a lambda expression"),
                                expression.right().position()
                        );
                    }

                    var lambdaScope = scope.add(lambdaExpression.argumentName(), elementType);
                    return linkExpression(lambdaExpression.expression(), lambdaScope)
                            .flatMap(predicate -> {
                                if (predicate.type() != BOOL) {
                                    return withPosition(
                                            ValueOrError.error("Lambda in `|-` has to return `BOOL`, was `" + predicate.type() + "`"),
                                            lambdaExpression.position()
                                    );
                                }
                                return ValueOrError.success((LinkedExpression) new LinkedPipeFilterOutExpression(
                                        left,
                                        lambdaExpression.argumentName(),
                                        predicate,
                                        left.type() instanceof LinkedSet
                                                ? new LinkedSet(elementType)
                                                : new LinkedList(elementType)
                                ));
                            });
                });
    }

    private static ValueOrError<LinkedInfixExpression> getLinkedInfixExpression(
            LinkedExpression left,
            InfixOperator operator,
            LinkedExpression right,
            Optional<pl.grzeslowski.capybara.parser.SourcePosition> position
    ) {
        LinkedType type = switch (operator) {
            case PLUS -> findPlusType(left.type(), right.type());
            case MINUS -> findMinusType(left.type(), right.type());
            case MUL, DIV, CARET, POWER -> findHigherType(left.type(), right.type());
            // bool operators
            case GT, LT, EQUAL, NOTEQUAL, LE, GE -> BOOL;
            case QUESTION -> findQuestionType(left.type(), right.type());
            case PIPE, PIPE_MINUS, PIPE_FLATMAP, PIPE_REDUCE -> null;
        };
        if (type == null) {
            var op = operator.symbol();
            return withPosition(ValueOrError.error("Cannot apply `" + op + "` to `" + left.type() + "` and `" + right.type() + "`"), position);
        }
        return ValueOrError.success(new LinkedInfixExpression(left, operator, right, type));
    }

    private static LinkedType findPlusType(LinkedType left, LinkedType right) {
        if (left instanceof LinkedList leftList) {
            if (right instanceof LinkedList rightList) {
                return new LinkedList(findHigherType(leftList.elementType(), rightList.elementType()));
            }
            return new LinkedList(findHigherType(leftList.elementType(), right));
        }
        if (left instanceof LinkedSet leftSet) {
            if (right instanceof LinkedSet rightSet) {
                return new LinkedSet(findHigherType(leftSet.elementType(), rightSet.elementType()));
            }
            return new LinkedSet(findHigherType(leftSet.elementType(), right));
        }
        if (left instanceof LinkedDict leftDict) {
            if (right instanceof LinkedDict rightDict) {
                return new LinkedDict(findHigherType(leftDict.valueType(), rightDict.valueType()));
            }
            return null;
        }
        return findHigherType(left, right);
    }

    private static LinkedType findMinusType(LinkedType left, LinkedType right) {
        if (left instanceof LinkedList leftList) {
            if (right instanceof LinkedList rightList) {
                return new LinkedList(findHigherType(leftList.elementType(), rightList.elementType()));
            }
            return new LinkedList(findHigherType(leftList.elementType(), right));
        }
        if (left instanceof LinkedSet leftSet) {
            if (right instanceof LinkedSet rightSet) {
                return new LinkedSet(findHigherType(leftSet.elementType(), rightSet.elementType()));
            }
            return new LinkedSet(findHigherType(leftSet.elementType(), right));
        }
        if (left instanceof LinkedDict leftDict) {
            if (right instanceof LinkedDict rightDict) {
                return new LinkedDict(findHigherType(leftDict.valueType(), rightDict.valueType()));
            }
            if (right == STRING) {
                return new LinkedDict(leftDict.valueType());
            }
            return null;
        }
        return findHigherType(left, right);
    }

    private static LinkedType findQuestionType(LinkedType left, LinkedType right) {
        if (left instanceof LinkedList leftList) {
            if (right instanceof LinkedList) {
                return null;
            }
            var elementType = findHigherType(leftList.elementType(), right);
            return elementType == ANY ? null : BOOL;
        }
        if (left instanceof LinkedSet leftSet) {
            if (right instanceof LinkedSet) {
                return null;
            }
            var elementType = findHigherType(leftSet.elementType(), right);
            return elementType == ANY ? null : BOOL;
        }
        if (left instanceof LinkedDict) {
            return right == STRING ? BOOL : null;
        }
        return null;
    }

    private static LinkedType collectionElementType(LinkedType type) {
        return switch (type) {
            case LinkedList linkedList -> linkedList.elementType();
            case LinkedSet linkedSet -> linkedSet.elementType();
            case LinkedDict linkedDict -> linkedDict.valueType();
            default -> null;
        };
    }

    private ValueOrError<LinkedExpression> linkIntValue(IntValue intValue, Scope scope) {
        return ValueOrError.success(new LinkedIntValue(intValue.intValue()));
    }

    private ValueOrError<LinkedExpression> linkMatchExpression(MatchExpression matchExpression, Scope scope) {
        return linkExpression(matchExpression.matchWith(), scope)
                .flatMap(matchWith -> matchExpression.cases().stream()
                        .map(matchCase -> linkMatchCase(matchCase, matchWith, scope))
                        .collect(new ValueOrErrorCollectionCollector<>())
                        .map(cases -> {
                            var matchType = cases.stream()
                                    .map(LinkedMatchExpression.MatchCase::expression)
                                    .map(LinkedExpression::type)
                                    .reduce(CapybaraTypeFinder::findHigherType)
                                    .orElse(ANY);
                            return (LinkedExpression) new LinkedMatchExpression(matchWith, cases, matchType);
                        }));
    }

    private ValueOrError<LinkedMatchExpression.MatchCase> linkMatchCase(
            MatchExpression.MatchCase matchCase,
            LinkedExpression matchWith,
            Scope scope
    ) {
        return linkPattern(matchCase.pattern(), matchWith.type(), scope)
                .flatMap(patternAndScope -> linkExpression(matchCase.expression(), patternAndScope.scope())
                        .map(expression -> new LinkedMatchExpression.MatchCase(patternAndScope.pattern(), expression)));
    }

    private ValueOrError<PatternAndScope> linkPattern(MatchExpression.Pattern pattern, LinkedType matchType, Scope scope) {
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern -> validateLiteralPattern(intPattern, matchType, scope, PrimitiveLinkedType.INT);
            case MatchExpression.StringPattern stringPattern -> validateLiteralPattern(stringPattern, matchType, scope, PrimitiveLinkedType.STRING);
            case MatchExpression.BoolPattern boolPattern -> validateLiteralPattern(boolPattern, matchType, scope, PrimitiveLinkedType.BOOL);
            case MatchExpression.FloatPattern floatPattern -> validateLiteralPattern(floatPattern, matchType, scope, PrimitiveLinkedType.FLOAT);
            case MatchExpression.VariablePattern variablePattern -> linkVariablePattern(variablePattern, matchType, scope);
            case MatchExpression.WildcardPattern wildcardPattern ->
                    ValueOrError.success(new PatternAndScope(LinkedMatchExpression.WildcardPattern.WILDCARD, scope));
            case MatchExpression.ConstructorPattern constructorPattern -> linkConstructorPattern(constructorPattern, matchType, scope);
        };
    }

    private ValueOrError<PatternAndScope> validateLiteralPattern(
            MatchExpression.Pattern pattern,
            LinkedType matchType,
            Scope scope,
            PrimitiveLinkedType expected
    ) {
        if (matchType != expected) {
            return ValueOrError.error("Cannot match `" + matchType + "` with literal of type `" + expected + "`");
        }
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.IntPattern(intPattern.value()), scope));
            case MatchExpression.StringPattern stringPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.StringPattern(stringPattern.value()), scope));
            case MatchExpression.BoolPattern boolPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.BoolPattern(boolPattern.value()), scope));
            case MatchExpression.FloatPattern floatPattern ->
                    ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.FloatPattern(floatPattern.value()), scope));
            default -> throw new IllegalStateException("Unexpected literal pattern: " + pattern);
        };
    }

    private ValueOrError<PatternAndScope> linkVariablePattern(
            MatchExpression.VariablePattern variablePattern,
            LinkedType matchType,
            Scope scope
    ) {
        if (matchType instanceof LinkedDataParentType parentType) {
            return findSubtype(variablePattern.name(), parentType)
                    .map(ignored -> new PatternAndScope(new LinkedMatchExpression.VariablePattern(variablePattern.name()), scope));
        }
        if (matchType instanceof LinkedDataType dataType && dataType.name().equals(variablePattern.name())) {
            return ValueOrError.success(new PatternAndScope(new LinkedMatchExpression.VariablePattern(variablePattern.name()), scope));
        }
        return ValueOrError.error("Cannot match `" + matchType + "` with constructor `" + variablePattern.name() + "`");
    }

    private ValueOrError<PatternAndScope> linkConstructorPattern(
            MatchExpression.ConstructorPattern constructorPattern,
            LinkedType matchType,
            Scope scope
    ) {
        return findConstructorType(constructorPattern.constructorName(), matchType)
                .flatMap(constructorType -> {
                    if (constructorType.fields().size() != constructorPattern.names().size()) {
                        return ValueOrError.error("Constructor `" + constructorPattern.constructorName() + "` expects "
                                                  + constructorType.fields().size() + " argument(s), got "
                                                  + constructorPattern.names().size());
                    }
                    var updatedScope = scope;
                    for (int i = 0; i < constructorPattern.names().size(); i++) {
                        updatedScope = updatedScope.add(constructorPattern.names().get(i), constructorType.fields().get(i).type());
                    }
                    return ValueOrError.success(new PatternAndScope(
                            new LinkedMatchExpression.ConstructorPattern(
                                    constructorPattern.constructorName(),
                                    constructorPattern.names()
                            ),
                            updatedScope
                    ));
                });
    }

    private ValueOrError<LinkedDataType> findConstructorType(String constructorName, LinkedType matchType) {
        if (matchType instanceof LinkedDataType linkedDataType) {
            if (linkedDataType.name().equals(constructorName)) {
                return ValueOrError.success(linkedDataType);
            }
            return ValueOrError.error("Type `" + linkedDataType.name() + "` cannot match constructor `" + constructorName + "`");
        }
        if (matchType instanceof LinkedDataParentType parentType) {
            return findSubtype(constructorName, parentType);
        }
        return ValueOrError.error("Type `" + matchType + "` is not matchable with constructor `" + constructorName + "`");
    }

    private ValueOrError<LinkedDataType> findSubtype(String constructorName, LinkedDataParentType parentType) {
        return parentType.subTypes().stream()
                .filter(subType -> subType.name().equals(constructorName))
                .findFirst()
                .map(ValueOrError::success)
                .orElseGet(() -> ValueOrError.error("Constructor `" + constructorName + "` not found in type `" + parentType.name() + "`"));
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

    private ValueOrError<LinkedExpression> linkNewSetExpression(NewSetExpression expression, Scope scope) {
        return expression.values().stream()
                .map(value -> linkExpression(value, scope))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(values -> {
                    var elementType = values.stream()
                            .map(LinkedExpression::type)
                            .reduce(CapybaraTypeFinder::findHigherType)
                            .orElse(ANY);
                    return (LinkedExpression) new LinkedNewSet(values, new LinkedSet(elementType));
                });
    }

    private ValueOrError<LinkedExpression> linkNewDictExpression(NewDictExpression expression, Scope scope) {
        return expression.entries().stream()
                .map(entry -> ValueOrError.join(
                        LinkedNewDict.Entry::new,
                        linkExpression(entry.key(), scope),
                        linkExpression(entry.value(), scope)
                ))
                .collect(new ValueOrErrorCollectionCollector<>())
                .flatMap(entries -> {
                    var invalidKey = entries.stream()
                            .map(LinkedNewDict.Entry::key)
                            .filter(key -> key.type() != STRING)
                            .findFirst();
                    if (invalidKey.isPresent()) {
                        return withPosition(
                                ValueOrError.error("dict keys must be of type `STRING`"),
                                expression.position()
                        );
                    }

                    var valueType = entries.stream()
                            .map(LinkedNewDict.Entry::value)
                            .map(LinkedExpression::type)
                            .reduce(CapybaraTypeFinder::findHigherType)
                            .orElse(ANY);
                    return ValueOrError.success((LinkedExpression) new LinkedNewDict(entries, new LinkedDict(valueType)));
                });
    }

    private ValueOrError<LinkedExpression> linkNewData(NewData newData, Scope scope) {
        return linkType(newData.type(), dataTypes)
                .flatMap(type ->
                        linkSpreadAssignments(newData.spreads(), scope)
                                .flatMap(spreadAssignments ->
                                        linkFieldAssignment(newData.assignments(), scope)
                                                .map(assignments -> {
                                                    var allAssignments = new java.util.ArrayList<LinkedNewData.FieldAssignment>(
                                                            spreadAssignments.size() + assignments.size()
                                                    );
                                                    allAssignments.addAll(spreadAssignments);
                                                    allAssignments.addAll(assignments);
                                                    return new LinkedNewData(type, List.copyOf(allAssignments));
                                                })));
    }

    private ValueOrError<List<LinkedNewData.FieldAssignment>> linkSpreadAssignments(List<Expression> spreads, Scope scope) {
        var assignments = new java.util.ArrayList<LinkedNewData.FieldAssignment>();
        for (var spread : spreads) {
            var linkedSpread = linkExpression(spread, scope);
            if (linkedSpread instanceof ValueOrError.Error<LinkedExpression> error) {
                return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
            }
            var spreadExpression = ((ValueOrError.Value<LinkedExpression>) linkedSpread).value();
            if (!(spreadExpression.type() instanceof GenericDataType dataType)) {
                return withPosition(
                        ValueOrError.error("Spread assignment requires data type, was `" + spreadExpression.type() + "`"),
                        spread.position()
                );
            }
            for (var field : dataType.fields()) {
                assignments.add(new LinkedNewData.FieldAssignment(
                        field.name(),
                        new LinkedFieldAccess(spreadExpression, field.name(), field.type())
                ));
            }
        }
        return ValueOrError.success(List.copyOf(assignments));
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

    private record PatternAndScope(LinkedMatchExpression.Pattern pattern, Scope scope) {
    }

    public record FunctionSignature(String name, List<LinkedType> parameterTypes, LinkedType returnType) {
    }

    private record CoercedArgument(LinkedExpression expression, int coercions) {
    }

    private record CoercedArguments(List<LinkedExpression> arguments, int coercions) {
    }

    private record ResolvedFunctionCall(FunctionSignature signature, List<LinkedExpression> arguments, int coercions) {
    }

    private record PipeMapper(String argumentName, LinkedExpression expression) {
    }
}
