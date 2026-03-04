package pl.grzeslowski.capybara.parser;


import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;
import pl.grzeslowski.capybara.parser.antlr.FunctionalParser;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static java.util.stream.Collectors.toSet;

public class CapybaraParser {
    public static final CapybaraParser INSTANCE = new CapybaraParser();

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

        var typeDeclaration = context.typeDeclaration();
        if (typeDeclaration != null) {
            return typeDeclaration(typeDeclaration);
        }

        throw new IllegalStateException("Unknown definition: " + context.getText());
    }

    private TypeDeclaration typeDeclaration(FunctionalParser.TypeDeclarationContext context) {
        var fieldDeclarationList = Optional.of(context)
                .map(FunctionalParser.TypeDeclarationContext::fieldDeclarationList)
                .map(this::fieldDeclarationList)
                .stream()
                .flatMap(Collection::stream)
                .toList();

        return new TypeDeclaration(
                context.TYPE().get(0).getText(),
                context.TYPE().stream().skip(1).map(TerminalNode::getText).toList(),
                fieldDeclarationList
        );
    }

    private DataDeclaration dataDeclaration(FunctionalParser.DataDeclarationContext context) {
        return new DataDeclaration(
                context.TYPE().getText(),
                fieldDeclarationList(context.fieldDeclarationList())
        );
    }

    private List<DataDeclaration.DataField> fieldDeclarationList(FunctionalParser.FieldDeclarationListContext context) {
        return context.fieldDeclaration()
                .stream()
                .map(this::fieldDeclaration)
                .toList();
    }

    private DataDeclaration.DataField fieldDeclaration(FunctionalParser.FieldDeclarationContext context) {
        return new DataDeclaration.DataField(context.NAME().getText(), type(context.type()));
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
                functionType(functionDeclarationContext.functionType()).orElse(null),
                expression(functionDeclarationContext.expression())
        );
    }

    private Optional<Type> functionType(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext context) {
        return Optional.of(context)
                .map(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext::type)
                .map(CapybaraParser::type);
    }

    private static Type type(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.TypeContext context) {
        var name = context.getText();
        return PrimitiveType.find(name)
                .map(Type.class::cast)
                .orElseGet(() -> new DataType(name));
    }

    private Expression expression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ExpressionContext expression) {
        var result = expressionNoLet(expression.expressionNoLet());
        var letExpressions = expression.letExpression();
        for (var i = letExpressions.size() - 1; i >= 0; i--) {
            var letExpression = letExpressions.get(i);
            result = new LetExpression(
                    letExpression.NAME().getText(),
                    expressionNoLet(letExpression.expressionNoLet()),
                    result
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
            return new IfExpression(condition, thenBranch, elseBranch);
        }

        if (expression.functionCall() != null) {
            return functionCall(expression.functionCall());
        }

        var value = expression.value();
        if (value != null) {
            var literal = value.literal();
            if (literal != null) {
                if (literal.BOOL_LITERAL() != null) {
                    return boolLiteral(literal.BOOL_LITERAL());
                }
                if (literal.INT_LITERAL() != null) {
                    return new IntValue(literal.INT_LITERAL().getText());
                }
                if (literal.STRING_LITERAL() != null) {
                    return new StringValue(literal.STRING_LITERAL().getText());
                }
                if (literal.FLOAT_LITERAL() != null) {
                    return new FloatValue(literal.FLOAT_LITERAL().getText());
                }
            }

            if (value.NAME() != null) {
                return new Value(value.NAME().getText());
            }
        }

        var newData = expression.newData();
        if (newData != null) {
            return new NewData(type(newData.type()), newData.fieldAssignmentList().fieldAssignment().stream().map(this::fieldAssignment).toList());
        }

        var matchExpression = expression.matchExpression();
        if (matchExpression != null) {
            return matchExpression(matchExpression);
        }

        var infixOperator = expression.infixOperator();
        if (infixOperator != null) {
            var left = expression.expressionNoLet(0);
            var operator = InfixOperator.fromSymbol(infixOperator.getText());
            var right = expression.expressionNoLet(1);
            return new InfixExpression(expressionNoLet(left), operator, expressionNoLet(right));
        }

        var subExpression = expression.expression();
        if (subExpression != null) {
            return expression(subExpression);
        }

        throw new IllegalStateException("Unknown expression: " + expression.getText());
    }

    private MatchExpression matchExpression(FunctionalParser.MatchExpressionContext context) {
        return new MatchExpression(
                expression(context.expression()),
                context.matchCaseList()
                        .stream()
                        .map(FunctionalParser.MatchCaseListContext::matchCase)
                        .flatMap(Collection::stream)
                        .map(this::matchCase)
                        .toList()
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
        return new FunctionCall(context.NAME().getText(), arguments);
    }

    private NewData.FieldAssignment fieldAssignment(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FieldAssignmentContext context) {
        return new NewData.FieldAssignment(context.NAME().getText(), expression(context.expression()));
    }

    private static BooleanValue boolLiteral(TerminalNode node) {
        return switch (node.getText()) {
            case "true" -> BooleanValue.TRUE;
            case "false" -> BooleanValue.FALSE;
            default -> throw new IllegalStateException("Unexpected value: " + node.getText());
        };
    }

    private Parameter parameter(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ParameterContext context) {
        return new Parameter(type(context.type()), context.NAME().getText());
    }

}
