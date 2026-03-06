package pl.grzeslowski.capybara.parser;


import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import pl.grzeslowski.capybara.parser.antlr.FunctionalParser;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.toSet;

public class CapybaraParser {
    public static final CapybaraParser INSTANCE = new CapybaraParser();
    private static final Pattern COLLECTION_LIST_PATTERN = Pattern.compile("list\\[(.+?)]");
    private static final Pattern COLLECTION_SET_PATTERN = Pattern.compile("set\\[(.+?)]");
    private static final Pattern COLLECTION_DICT_PATTERN = Pattern.compile("dict\\[(.+?)]");

    public Functional parseFunctional(String input) {
        var lexer = new pl.grzeslowski.capybara.parser.antlr.FunctionalLexer(CharStreams.fromString(input));
        var tokens = new CommonTokenStream(lexer);
        tokens.fill();
        var parser = new pl.grzeslowski.capybara.parser.antlr.FunctionalParser(tokens);
        var definitions = parser.program().definition().stream().map(this::definition).collect(toSet());
        return new Functional(definitions);
    }

    private Definition definition(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.DefinitionContext context) {
        var functionDeclaration = context.functionDeclaration();
        if (functionDeclaration != null) {
            return functionDeclaration(functionDeclaration);
        }

        var dataDeclaration = context.dataDeclaration();
        if (dataDeclaration != null) {
            return dataDeclaration(dataDeclaration);
        }

        var singleDeclaration = context.singleDeclaration();
        if (singleDeclaration != null) {
            return singleDeclaration(singleDeclaration);
        }

        var typeDeclaration = context.typeDeclaration();
        if (typeDeclaration != null) {
            return typeDeclaration(typeDeclaration);
        }

        throw new IllegalStateException("Unknown definition: " + context.getText());
    }

    private TypeDeclaration typeDeclaration(FunctionalParser.TypeDeclarationContext context) {
        var declaredType = context.genericTypeDeclaration(0);
        var fieldDeclarationList = Optional.of(context)
                .map(FunctionalParser.TypeDeclarationContext::fieldDeclarationList)
                .map(this::fieldDeclarationList)
                .stream()
                .flatMap(Collection::stream)
                .toList();

        return new TypeDeclaration(
                genericTypeName(declaredType),
                context.genericTypeDeclaration().stream().skip(1).map(CapybaraParser::genericTypeName).toList(),
                fieldDeclarationList,
                genericTypeParameters(declaredType),
                position(context)
        );
    }

    private DataDeclaration dataDeclaration(FunctionalParser.DataDeclarationContext context) {
        var declaration = context.genericTypeDeclaration();
        var dataFields = dataFieldDeclarationList(context.fieldDeclarationList());
        return new DataDeclaration(
                genericTypeName(declaration),
                dataFields.fields(),
                dataFields.extendsTypes(),
                genericTypeParameters(declaration),
                position(context)
        );
    }

    private SingleDeclaration singleDeclaration(FunctionalParser.SingleDeclarationContext context) {
        return new SingleDeclaration(context.TYPE().getText(), position(context));
    }

    private List<DataDeclaration.DataField> fieldDeclarationList(FunctionalParser.FieldDeclarationListContext context) {
        return context.fieldDeclaration()
                .stream()
                .map(this::fieldDeclaration)
                .toList();
    }

    private DataDeclaration.DataField fieldDeclaration(FunctionalParser.FieldDeclarationContext context) {
        if (context.SPREAD() != null) {
            throw new IllegalStateException("Spread field declaration is not allowed in this context: " + context.getText());
        }
        return new DataDeclaration.DataField(fieldName(context.NAME(), context.STRING_LITERAL()), type(context.type()));
    }

    private Function functionDeclaration(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionDeclarationContext functionDeclarationContext) {
        var parameters = functionDeclarationContext.parameters() == null
                ? List.<Parameter>of()
                : functionDeclarationContext.parameters()
                .parameter()
                .stream()
                .map(this::parameter)
                .toList();
        return new Function(
                functionDeclarationContext.NAME().getText(),
                parameters,
                functionType(functionDeclarationContext.functionType()),
                expression(functionDeclarationContext.expression()),
                position(functionDeclarationContext)
        );
    }

    private Optional<Type> functionType(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext context) {
        return Optional.ofNullable(context)
                .map(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext::type)
                .map(CapybaraParser::type);
    }

    private static Type type(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.TypeContext context) {
        if (context.COLLECTION() != null) {
            var collection = context.COLLECTION().getText();
            var inner = type(context.type(0));
            return switch (collection) {
                case "list" -> new CollectionType.ListType(inner);
                case "set" -> new CollectionType.SetType(inner);
                case "dict" -> new CollectionType.DictType(inner);
                default -> throw new IllegalStateException("Unknown collection type: " + collection);
            };
        }
        if (context.ARROW() != null) {
            if ("(".equals(context.getChild(0).getText())) {
                var parts = context.type();
                var returnType = type(parts.get(parts.size() - 1));
                var functionType = returnType;
                for (var i = parts.size() - 2; i >= 0; i--) {
                    functionType = new FunctionType(type(parts.get(i)), functionType);
                }
                return functionType;
            }
            return new FunctionType(type(context.type(0)), type(context.type(1)));
        }
        return type(context.getText());
    }

    private static Type type(String name) {
        return PrimitiveType.find(name)
                .map(Type.class::cast)
                .or(() -> findCollectionType(name))
                .or(() -> findParameterizedDataType(name))
                .orElseGet(() -> new DataType(name));
    }

    private static Optional<Type> findCollectionType(String name) {
        return findCollectionType(name, COLLECTION_LIST_PATTERN, CollectionType.ListType::new)
                .or(() -> findCollectionType(name, COLLECTION_SET_PATTERN, CollectionType.SetType::new))
                .or(() -> findCollectionType(name, COLLECTION_DICT_PATTERN, CollectionType.DictType::new));
    }

    private static Optional<Type> findParameterizedDataType(String name) {
        var idx = name.indexOf('[');
        if (idx > 0 && name.endsWith("]")) {
            return Optional.of(new DataType(name.substring(0, idx)));
        }
        return Optional.empty();
    }

    private static Optional<Type> findCollectionType(String name, Pattern pattern, java.util.function.Function<Type, CollectionType> creator) {
        return Optional.of(pattern)
                .map(p -> p.matcher(name))
                .filter(Matcher::matches)
                .map(m -> m.group(1))
                .map(CapybaraParser::type)
                .map(creator);
    }

    private Expression expression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ExpressionContext expression) {
        var result = expressionNoLet(expression.expressionNoLet());
        var letExpressions = expression.letExpression();
        for (var i = letExpressions.size() - 1; i >= 0; i--) {
            var letExpression = letExpressions.get(i);
            result = new LetExpression(
                    letExpression.NAME().getText(),
                    expressionNoLet(letExpression.expressionNoLet()),
                    result,
                    position(letExpression)
            );
        }
        return result;
    }

    private Expression expressionNoLet(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ExpressionNoLetContext expression) {
        if (expression.ifExpression() != null) {
            var ifExpression = expression.ifExpression();
            var condition = expression(ifExpression.expression(0));
            var thenBranch = expression(ifExpression.expression(1));
            var elseBranch = expression(ifExpression.expression(2));
            return new IfExpression(condition, thenBranch, elseBranch, position(expression));
        }

        if (expression.lambdaExpression() != null) {
            return lambdaExpression(expression.lambdaExpression());
        }
        if (expression.reduceExpression() != null) {
            return reduceExpression(expression.reduceExpression());
        }
        if (expression.functionReference() != null) {
            return functionReference(expression.functionReference());
        }

        if (expression.functionCall() != null) {
            return functionCall(expression.functionCall());
        }
        if (expression.new_list() != null) {
            return newListExpression(expression.new_list());
        }
        if (expression.new_dict() != null) {
            return newDictExpression(expression.new_dict());
        }
        if (expression.new_set() != null) {
            return newSetExpression(expression.new_set());
        }

        var value = expression.value();
        if (value != null) {
            var literal = value.literal();
            if (literal != null) {
                if (literal.BOOL_LITERAL() != null) {
                    return boolLiteral(literal.BOOL_LITERAL());
                }
                if (literal.INT_LITERAL() != null) {
                    return new IntValue(literal.INT_LITERAL().getText(), position(literal.INT_LITERAL()));
                }
                if (literal.STRING_LITERAL() != null) {
                    return new StringValue(literal.STRING_LITERAL().getText(), position(literal.STRING_LITERAL()));
                }
                if (literal.FLOAT_LITERAL() != null) {
                    return new FloatValue(literal.FLOAT_LITERAL().getText(), position(literal.FLOAT_LITERAL()));
                }
            }

            if (value.identifier() != null) {
                return new Value(identifier(value.identifier()), position(value.identifier()));
            }
        }

        var newData = expression.newData();
        if (newData != null) {
            var assignments = fieldAssignments(newData.fieldAssignmentList());
            return new NewData(
                    type(newData.type()),
                    assignments.assignments(),
                    assignments.spreads(),
                    position(newData)
            );
        }

        var matchExpression = expression.matchExpression();
        if (matchExpression != null) {
            return matchExpression(matchExpression);
        }

        if (expression.DOT() != null || isFieldAccess(expression)) {
            return new FieldAccess(
                    expressionNoLet(expression.expressionNoLet(0)),
                    expression.NAME().getText(),
                    position(expression)
            );
        }

        if (expression.MINUS() != null && expression.infixOperator() == null && expression.expressionNoLet().size() == 1) {
            return new InfixExpression(
                    new IntValue("0", position(expression)),
                    InfixOperator.MINUS,
                    expressionNoLet(expression.expressionNoLet(0)),
                    position(expression)
            );
        }

        var infixOperator = expression.infixOperator();
        if (infixOperator != null) {
            var leftContext = expression.expressionNoLet(0);
            var operator = InfixOperator.fromSymbol(infixOperator.getText());
            var rightContext = expression.expressionNoLet(1);
            return rebalanceInfixByPrecedence(
                    expressionNoLet(leftContext),
                    isGrouped(leftContext),
                    operator,
                    expressionNoLet(rightContext),
                    position(expression)
            );
        }

        var subExpression = expression.expression();
        if (subExpression != null) {
            return expression(subExpression);
        }

        throw new IllegalStateException("Unknown expression: " + expression.getText());
    }

    private LambdaExpression lambdaExpression(FunctionalParser.LambdaExpressionContext context) {
        return new LambdaExpression(
                context.identifier().stream().map(CapybaraParser::identifier).toList(),
                expressionNoLetNoPipe(context.expressionNoLetNoPipe()),
                position(context)
        );
    }

    private ReduceExpression reduceExpression(FunctionalParser.ReduceExpressionContext context) {
        return new ReduceExpression(
                expressionNoLetNoPipe(context.expressionNoLetNoPipe(0)),
                context.NAME(0).getText(),
                context.NAME(1).getText(),
                expressionNoLetNoPipe(context.expressionNoLetNoPipe(1)),
                position(context)
        );
    }

    private FunctionReference functionReference(FunctionalParser.FunctionReferenceContext context) {
        return new FunctionReference(identifier(context.identifier()), position(context));
    }

    private Expression expressionNoLetNoPipe(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        if (expression.ifExpression() != null) {
            var ifExpression = expression.ifExpression();
            var condition = expression(ifExpression.expression(0));
            var thenBranch = expression(ifExpression.expression(1));
            var elseBranch = expression(ifExpression.expression(2));
            return new IfExpression(condition, thenBranch, elseBranch, position(expression));
        }

        if (expression.functionCall() != null) {
            return functionCall(expression.functionCall());
        }
        if (expression.functionReference() != null) {
            return functionReference(expression.functionReference());
        }
        if (expression.new_list() != null) {
            return newListExpression(expression.new_list());
        }
        if (expression.new_dict() != null) {
            return newDictExpression(expression.new_dict());
        }
        if (expression.new_set() != null) {
            return newSetExpression(expression.new_set());
        }

        var value = expression.value();
        if (value != null) {
            var literal = value.literal();
            if (literal != null) {
                if (literal.BOOL_LITERAL() != null) {
                    return boolLiteral(literal.BOOL_LITERAL());
                }
                if (literal.INT_LITERAL() != null) {
                    return new IntValue(literal.INT_LITERAL().getText(), position(literal.INT_LITERAL()));
                }
                if (literal.STRING_LITERAL() != null) {
                    return new StringValue(literal.STRING_LITERAL().getText(), position(literal.STRING_LITERAL()));
                }
                if (literal.FLOAT_LITERAL() != null) {
                    return new FloatValue(literal.FLOAT_LITERAL().getText(), position(literal.FLOAT_LITERAL()));
                }
            }

            if (value.identifier() != null) {
                return new Value(identifier(value.identifier()), position(value.identifier()));
            }
        }

        var newData = expression.newData();
        if (newData != null) {
            var assignments = fieldAssignments(newData.fieldAssignmentList());
            return new NewData(
                    type(newData.type()),
                    assignments.assignments(),
                    assignments.spreads(),
                    position(newData)
            );
        }

        var matchExpression = expression.matchExpression();
        if (matchExpression != null) {
            return matchExpression(matchExpression);
        }

        if (expression.DOT() != null || isFieldAccess(expression)) {
            return new FieldAccess(
                    expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)),
                    expression.NAME().getText(),
                    position(expression)
            );
        }

        if (expression.MINUS() != null && expression.infixOperatorNoPipe() == null && expression.expressionNoLetNoPipe().size() == 1) {
            return new InfixExpression(
                    new IntValue("0", position(expression)),
                    InfixOperator.MINUS,
                    expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)),
                    position(expression)
            );
        }

        var infixOperator = expression.infixOperatorNoPipe();
        if (infixOperator != null) {
            var leftContext = expression.expressionNoLetNoPipe(0);
            var operator = InfixOperator.fromSymbol(infixOperator.getText());
            var rightContext = expression.expressionNoLetNoPipe(1);
            return rebalanceInfixByPrecedence(
                    expressionNoLetNoPipe(leftContext),
                    false,
                    operator,
                    expressionNoLetNoPipe(rightContext),
                    position(expression)
            );
        }

        if (!expression.expressionNoLetNoPipe().isEmpty()) {
            return expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0));
        }

        throw new IllegalStateException("Unknown expression: " + expression.getText());
    }

    private static boolean isGrouped(FunctionalParser.ExpressionNoLetContext context) {
        return context.expression() != null;
    }

    private static Expression rebalanceInfixByPrecedence(Expression left,
                                                         boolean leftGrouped,
                                                         InfixOperator operator,
                                                         Expression right,
                                                         Optional<SourcePosition> position) {
        if (!leftGrouped &&
            left instanceof InfixExpression leftInfix &&
            operator.precedence() > leftInfix.operator().precedence()) {
            return new InfixExpression(
                    leftInfix.left(),
                    leftInfix.operator(),
                    rebalanceInfixByPrecedence(leftInfix.right(), false, operator, right, leftInfix.position()),
                    leftInfix.position()
            );
        }
        return new InfixExpression(left, operator, right, position);
    }

    private MatchExpression matchExpression(FunctionalParser.MatchExpressionContext context) {
        return new MatchExpression(
                expression(context.expression()),
                context.matchCaseList()
                        .stream()
                        .map(FunctionalParser.MatchCaseListContext::matchCase)
                        .flatMap(Collection::stream)
                        .map(this::matchCase)
                        .toList(),
                position(context)
        );
    }

    private MatchExpression.MatchCase matchCase(FunctionalParser.MatchCaseContext context) {
        return new MatchExpression.MatchCase(
                matchExpressionPattern(context.pattern()),
                expression(context.expression())

        );
    }

    private MatchExpression.Pattern matchExpressionPattern(FunctionalParser.PatternContext context) {
        var type = context.TYPE();
        if (type != null) {
            return new MatchExpression.VariablePattern(type.getText());
        }

        var boolLiteral = context.BOOL_LITERAL();
        if (boolLiteral != null) {
            return new MatchExpression.BoolPattern(boolLiteral.getText());
        }

        var intLiteral = context.INT_LITERAL();
        if (intLiteral != null) {
            return new MatchExpression.IntPattern(intLiteral.getText());
        }

        var stringLiteral = context.STRING_LITERAL();
        if (stringLiteral != null) {
            return new MatchExpression.StringPattern(stringLiteral.getText());
        }

        var floatLiteral = context.FLOAT_LITERAL();
        if (floatLiteral != null) {
            return new MatchExpression.FloatPattern(floatLiteral.getText());
        }

        var wildcard = context.UNDERSCORE();
        if (wildcard != null) {
            return MatchExpression.WildcardPattern.WILDCARD;
        }

        var constructorPattern = context.constructorPattern();
        if (constructorPattern != null) {
            return new MatchExpression.ConstructorPattern(
                    constructorPattern.TYPE().getText(),
                    constructorPattern.fieldPatternList().NAME().stream().map(TerminalNode::getText).toList()
            );
        }

        throw new IllegalStateException("Unknown pattern: " + context.getText());
    }

    private Expression functionCall(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionCallContext context) {
        var arguments = context.argumentList() == null
                ? List.<Expression>of()
                : context.argumentList().expression().stream().map(this::expression).toList();
        return new FunctionCall(identifier(context.identifier()), arguments, position(context));
    }

    private Expression newListExpression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.New_listContext context) {
        return new NewListExpression(
                context.expression().stream().map(this::expression).toList(),
                position(context)
        );
    }

    private Expression newDictExpression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.New_dictContext context) {
        return new NewDictExpression(
                context.dict_entry().stream()
                        .map(this::dictEntry)
                        .toList(),
                position(context)
        );
    }

    private NewDictExpression.Entry dictEntry(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.Dict_entryContext context) {
        return new NewDictExpression.Entry(
                expression(context.expression(0)),
                expression(context.expression(1))
        );
    }

    private Expression newSetExpression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.New_setContext context) {
        return new NewSetExpression(
                context.expression().stream().map(this::expression).toList(),
                position(context)
        );
    }

    private NewData.FieldAssignment fieldAssignment(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FieldAssignmentContext context) {
        if (context.SPREAD() != null) {
            throw new IllegalStateException("Spread field assignment is not allowed in this context: " + context.getText());
        }
        return new NewData.FieldAssignment(fieldName(context.NAME(), context.STRING_LITERAL()), expression(context.expression()));
    }

    private DataFieldDeclarations dataFieldDeclarationList(FunctionalParser.FieldDeclarationListContext context) {
        if (context == null) {
            return new DataFieldDeclarations(List.of(), List.of());
        }
        var fields = new java.util.ArrayList<DataDeclaration.DataField>();
        var extendsTypes = new java.util.ArrayList<String>();
        for (var declaration : context.fieldDeclaration()) {
            if (declaration.SPREAD() != null) {
                extendsTypes.add(declaration.TYPE().getText());
            } else {
                fields.add(fieldDeclaration(declaration));
            }
        }
        return new DataFieldDeclarations(List.copyOf(fields), List.copyOf(extendsTypes));
    }

    private NewDataFieldAssignments fieldAssignments(FunctionalParser.FieldAssignmentListContext context) {
        if (context == null) {
            return new NewDataFieldAssignments(List.of(), List.of());
        }
        var assignments = new java.util.ArrayList<NewData.FieldAssignment>();
        var spreads = new java.util.ArrayList<Expression>();
        for (var assignment : context.fieldAssignment()) {
            if (assignment.SPREAD() != null) {
                spreads.add(expression(assignment.expression()));
            } else {
                assignments.add(fieldAssignment(assignment));
            }
        }
        return new NewDataFieldAssignments(List.copyOf(assignments), List.copyOf(spreads));
    }

    private static BooleanValue boolLiteral(TerminalNode node) {
        return switch (node.getText()) {
            case "true" -> new BooleanValue(true, position(node));
            case "false" -> new BooleanValue(false, position(node));
            default -> throw new IllegalStateException("Unexpected value: " + node.getText());
        };
    }

    private Parameter parameter(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ParameterContext context) {
        return new Parameter(type(context.type()), identifier(context.identifier()), position(context));
    }

    private static Optional<SourcePosition> position(ParserRuleContext context) {
        return SourcePosition.of(context);
    }

    private static Optional<SourcePosition> position(TerminalNode node) {
        return Optional.of(SourcePosition.of(node));
    }

    private static String fieldName(TerminalNode name, TerminalNode stringLiteral) {
        if (name != null) {
            return name.getText();
        }
        if (stringLiteral != null) {
            var raw = stringLiteral.getText();
            return raw.substring(1, raw.length() - 1);
        }
        throw new IllegalStateException("Missing field name");
    }

    private static String genericTypeName(FunctionalParser.GenericTypeDeclarationContext context) {
        return context.TYPE(0).getText();
    }

    private static String identifier(FunctionalParser.IdentifierContext context) {
        return context.getText();
    }

    private static List<String> genericTypeParameters(FunctionalParser.GenericTypeDeclarationContext context) {
        return context.TYPE().stream().skip(1).map(TerminalNode::getText).toList();
    }

    private static boolean isFieldAccess(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.getChildCount() == 3 && ".".equals(expression.getChild(1).getText()) && expression.NAME() != null;
    }

    private static boolean isFieldAccess(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.getChildCount() == 3 && ".".equals(expression.getChild(1).getText()) && expression.NAME() != null;
    }

    private record DataFieldDeclarations(List<DataDeclaration.DataField> fields, List<String> extendsTypes) {
    }

    private record NewDataFieldAssignments(List<NewData.FieldAssignment> assignments, List<Expression> spreads) {
    }

}

