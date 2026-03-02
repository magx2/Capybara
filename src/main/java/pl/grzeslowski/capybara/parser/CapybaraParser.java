package pl.grzeslowski.capybara.parser;


import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

public class CapybaraParser {
    public static final CapybaraParser INSTANCE = new CapybaraParser();

    public Functional parseFuntional(String input) {
        var lexer = new pl.grzeslowski.capybara.parser.antlr.FunctionalLexer(CharStreams.fromString(input));
        var tokens = new CommonTokenStream(lexer);
        tokens.fill();
        var parser = new pl.grzeslowski.capybara.parser.antlr.FunctionalParser(tokens);
        var definitions = parser.program().definition().stream().map(this::definition).collect(toSet());
        return new Functional(definitions);
    }

    private Definition definition(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.DefinitionContext definitionContext) {
      return functionDeclaration(definitionContext.functionDeclaration());
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
                expression(functionDeclarationContext.expression())
        );
    }

    private Optional<Type> functionType(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext context) {
        return Optional.of(context)
                .map(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext::type)
                .map(CapybaraParser::type);
    }

    private static Type type(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.TypeContext functionDeclarationContext) {
        return new Type(functionDeclarationContext.getText());
    }

    private Expression expression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ExpressionContext expression) {
        if (expression.ifExpression() != null) {
            var ifExpression = expression.ifExpression();
            var condition = boolExpression(ifExpression.boolExperssion());
            var thenBranch = expression(ifExpression.expression(0));
            var elseBranch = expression(ifExpression.expression(1));
            return new Expression.IfExpression(condition, thenBranch, elseBranch);
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
                    return new Expression.IntValue(literal.INT_LITERAL().getText());
                }
                if (literal.STRING_LITERAL() != null) {
                    return new Expression.StringValue(literal.STRING_LITERAL().getText());
                }
            }

            if (value.NAME() != null) {
                return new Expression.Variable(value.NAME().getText());
            }
        }

        var newData = expression.newData();
        if (newData != null) {
            return new Expression.NewData(type(newData.type()), newData.fieldAssignmentList().fieldAssignment().stream().map(this::fieldAssignment).toList());
        }

        throw new IllegalStateException("Unknown expression: " + expression.getText());
    }

    private Expression functionCall(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionCallContext context) {
        var arguments = context.argumentList() == null
                ? List.<Expression>of()
                : context.argumentList().expression().stream().map(this::expression).toList();
        return new Expression.FunctionCall(context.NAME().getText(), arguments);
    }

    private Expression.NewData.FieldAssignment fieldAssignment(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FieldAssignmentContext context) {
        return new Expression.NewData.FieldAssignment(context.NAME().getText(), expression(context.expression()));
    }

    private Expression.BoolExpression boolExpression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.BoolExperssionContext context) {
        if (context.BOOL_LITERAL() != null) {
            return boolLiteral(context.BOOL_LITERAL());
        }
        if (context.boolInfixOperator() != null) {
            var left = expression(context.expression(0));
            var operator = BoolInfixOperator.fromSymbol(context.boolInfixOperator().getText());
            var right = expression(context.expression(1));
            return new Expression.BoolInfixExpression(left, operator, right);
        }
        throw new IllegalStateException("Unknown bool expression: " + context.getText());
    }

    private static Expression.BoolExpression boolLiteral(TerminalNode node) {
        return switch (node.getText()) {
            case "true" -> Expression.BooleanValue.TRUE;
            case "false" -> Expression.BooleanValue.FALSE;
            default -> throw new IllegalStateException("Unexpected value: " + node.getText());
        };
    }

    private Parameter parameter(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ParameterContext context) {
        return new Parameter(type(context.type()), context.NAME().getText());
    }

    public record Functional(Set<Definition> definitions) {
        public Functional reduce(Functional other) {
            var sum = new HashSet<>(definitions);
            sum.addAll(other.definitions);
            return new Functional(sum);
        }
    }
    public sealed interface Definition permits Function{}
}
