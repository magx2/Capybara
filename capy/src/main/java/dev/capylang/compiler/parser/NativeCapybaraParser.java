package dev.capylang.compiler.parser;

import dev.capylang.NativeImplementation;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

@NativeImplementation
public final class NativeCapybaraParser implements CapybaraParser {
    private static final String MODULE_NAME_PATTERN =
            "[A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][a-zA-Z0-9_]*(?:/[A-Za-z_][a-zA-Z0-9_]*)+";
    private static final Pattern FROM_IMPORT_PATTERN = Pattern.compile(
            "^\\s*from\\s+(" + MODULE_NAME_PATTERN + ")\\s+import\\s*\\{\\s*([^}]*)\\s*}(?:\\s+except\\s*\\{\\s*([^}]*)\\s*})?\\s*$"
    );
    private static final Pattern QUALIFIED_IMPORT_PATTERN = Pattern.compile(
            "^\\s*import\\s+(" + MODULE_NAME_PATTERN + ")\\s*$"
    );

    @Override
    public ParsedProgram parse(List<RawModule> modules) {
        Objects.requireNonNull(modules, "modules");

        var parsedModules = new ArrayList<ParsedModule>();
        for (var module : modules) {
            parsedModules.add(parseModule(module));
        }
        return new ParsedProgram(List.copyOf(parsedModules));
    }

    private ParsedModule parseModule(RawModule module) {
        Objects.requireNonNull(module, "module");

        var source = stripImports(module.input());
        var definitions = switch (module.sourceKind()) {
            case FUNCTIONAL -> parseFunctional(module, source.body());
            case OBJECT_ORIENTED -> {
                parseObjectOriented(module, source.body());
                yield List.<Definition>of();
            }
        };
        var functional = new Functional(definitions);
        return new ParsedModule(
                module.name(),
                module.path(),
                functional,
                definitions,
                source.imports(),
                module.sourceKind()
        );
    }

    private List<Definition> parseFunctional(RawModule module, String source) {
        var lexer = new dev.capylang.parser.antlr.FunctionalLexer(CharStreams.fromString(source));
        var tokens = new CommonTokenStream(lexer);
        var parser = new dev.capylang.parser.antlr.FunctionalParser(tokens);
        var errors = new ArrayList<SyntaxError>();
        var listener = errorListener(errors);

        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(listener);
        parser.addErrorListener(listener);

        var program = parser.program();
        throwIfInvalid(module, errors);

        var definitions = new ArrayList<Definition>();
        for (var definition : program.definition()) {
            definitions.add(functionalDefinition(definition));
        }
        return List.copyOf(definitions);
    }

    private void parseObjectOriented(RawModule module, String source) {
        var lexer = new dev.capylang.parser.antlr.ObjectOrientedLexer(CharStreams.fromString(source));
        var tokens = new CommonTokenStream(lexer);
        var parser = new dev.capylang.parser.antlr.ObjectOrientedParser(tokens);
        var errors = new ArrayList<SyntaxError>();
        var listener = errorListener(errors);

        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(listener);
        parser.addErrorListener(listener);

        parser.program();
        throwIfInvalid(module, errors);
    }

    private static BaseErrorListener errorListener(List<SyntaxError> errors) {
        return new BaseErrorListener() {
            @Override
            public void syntaxError(
                    Recognizer<?, ?> recognizer,
                    Object offendingSymbol,
                    int line,
                    int charPositionInLine,
                    String msg,
                    RecognitionException e
            ) {
                errors.add(new SyntaxError(line, charPositionInLine, msg));
            }
        };
    }

    private static void throwIfInvalid(RawModule module, List<SyntaxError> errors) {
        if (errors.isEmpty()) {
            return;
        }

        var error = errors.getFirst();
        throw new IllegalArgumentException(
                "%s:%d:%d: ParserError: %s".formatted(
                        moduleFile(module),
                        error.line(),
                        error.column(),
                        error.message()
                )
        );
    }

    private static ParsedSource stripImports(String source) {
        var imports = new ArrayList<ImportDeclaration>();
        var body = new ArrayList<String>();
        var lineNumber = 1;

        for (var line : source.split("\\R", -1)) {
            var fromImport = FROM_IMPORT_PATTERN.matcher(line);
            var qualifiedImport = QUALIFIED_IMPORT_PATTERN.matcher(line);
            if (fromImport.matches()) {
                imports.add(new ImportDeclaration(
                        fromImport.group(1),
                        importNames(fromImport.group(2)),
                        importWildcard(fromImport.group(2)),
                        false,
                        new SourceLocation(lineNumber, leadingWhitespace(line))
                ));
                body.add("");
            } else if (qualifiedImport.matches()) {
                imports.add(new ImportDeclaration(
                        qualifiedImport.group(1),
                        List.of(),
                        false,
                        true,
                        new SourceLocation(lineNumber, leadingWhitespace(line))
                ));
                body.add("");
            } else {
                body.add(line);
            }
            lineNumber++;
        }
        return new ParsedSource(String.join(System.lineSeparator(), body), List.copyOf(imports));
    }

    private static List<String> importNames(String rawNames) {
        if (importWildcard(rawNames)) {
            return List.of();
        }
        var names = new ArrayList<String>();
        for (var rawName : rawNames.split(",")) {
            var name = rawName.trim();
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        return List.copyOf(names);
    }

    private static boolean importWildcard(String rawNames) {
        return rawNames.trim().equals("*");
    }

    private static Definition functionalDefinition(dev.capylang.parser.antlr.FunctionalParser.DefinitionContext definition) {
        if (definition.annotationDeclaration() != null) {
            return Definition.AnnotationDeclaration.INSTANCE;
        }
        if (definition.dataDeclaration() != null) {
            return Definition.DataDeclaration.INSTANCE;
        }
        if (definition.deriverDeclaration() != null) {
            return Definition.DeriverDeclaration.INSTANCE;
        }
        if (definition.enumDeclaration() != null) {
            return Definition.EnumDeclaration.INSTANCE;
        }
        if (definition.functionDeclaration() != null) {
            return new Definition.FunctionDefinition(functionDeclaration(definition.functionDeclaration()));
        }
        if (definition.constDeclaration() != null) {
            return new Definition.ConstantDefinition(constantDeclaration(definition.constDeclaration()));
        }
        if (definition.primitiveBackedTypeDeclaration() != null) {
            return Definition.PrimitiveBackedTypeDeclaration.INSTANCE;
        }
        if (definition.typeDeclaration() != null) {
            return Definition.TypeDeclaration.INSTANCE;
        }
        return Definition.UnsupportedDefinition.INSTANCE;
    }

    private static ConstantDeclaration constantDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.ConstDeclarationContext ctx
    ) {
        var visibility = ctx.VISIBILITY() == null ? "public" : ctx.VISIBILITY().getText();
        var type = ctx.type() == null ? missingType() : typeReference(ctx.type());
        return new ConstantDeclaration(
                ctx.TYPE().getText(),
                visibility,
                type,
                expressionNoLet(ctx.expressionNoLet()),
                location(ctx)
        );
    }

    private static FunctionDeclaration functionDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.FunctionDeclarationContext ctx
    ) {
        var visibility = ctx.VISIBILITY() == null ? "public" : ctx.VISIBILITY().getText();
        var parameters = new ArrayList<FunctionParameter>();
        if (ctx.parameters() != null) {
            for (var parameter : ctx.parameters().parameter()) {
                parameters.add(functionParameter(parameter));
            }
        }
        var returnType = ctx.functionType() == null
                ? missingType()
                : typeReference(ctx.functionType().type());
        return new FunctionDeclaration(
                ctx.functionNameDeclaration().getText(),
                visibility,
                List.copyOf(parameters),
                returnType,
                functionBody(ctx.functionBody()),
                location(ctx)
        );
    }

    private static FunctionParameter functionParameter(dev.capylang.parser.antlr.FunctionalParser.ParameterContext ctx) {
        return new FunctionParameter(
                ctx.identifier().getText(),
                typeReference(ctx.type()),
                location(ctx)
        );
    }

    private static Expression functionBody(dev.capylang.parser.antlr.FunctionalParser.FunctionBodyContext ctx) {
        if (ctx.localDefinition().isEmpty()) {
            return expression(ctx.expression());
        }

        var bindings = new ArrayList<Expression.LetBinding>();
        for (var definition : ctx.localDefinition()) {
            if (definition.localConstDeclaration() == null) {
                return unsupported(ctx);
            }
            bindings.add(localConstBinding(definition.localConstDeclaration()));
        }
        return new Expression.BlockExpression(List.copyOf(bindings), expression(ctx.expression()), location(ctx));
    }

    private static Expression.LetBinding localConstBinding(
            dev.capylang.parser.antlr.FunctionalParser.LocalConstDeclarationContext ctx
    ) {
        var type = ctx.type() == null ? missingType() : typeReference(ctx.type());
        return new Expression.LetBinding(
                ctx.privateLocalConstName().getText(),
                type,
                expressionNoLet(ctx.expressionNoLet()),
                location(ctx)
        );
    }

    private static Expression expression(dev.capylang.parser.antlr.FunctionalParser.ExpressionContext ctx) {
        var bindings = new ArrayList<Expression.LetBinding>();
        for (var let : ctx.letExpression()) {
            bindings.add(letBinding(let));
        }
        var result = expressionNoLet(ctx.expressionNoLet());
        if (bindings.isEmpty()) {
            return result;
        }
        return new Expression.BlockExpression(List.copyOf(bindings), result, location(ctx));
    }

    private static Expression.LetBinding letBinding(dev.capylang.parser.antlr.FunctionalParser.LetExpressionContext ctx) {
        var type = ctx.type() == null ? missingType() : typeReference(ctx.type());
        return new Expression.LetBinding(
                ctx.identifier().getText(),
                type,
                expressionNoLet(ctx.expressionNoLet()),
                location(ctx)
        );
    }

    private static Expression expressionNoLet(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext ctx) {
        if (ctx.ifExpression() != null) {
            return ifExpression(ctx.ifExpression());
        }
        if (ctx.lambdaExpression() != null) {
            return lambdaExpression(ctx.lambdaExpression());
        }
        if (ctx.functionCall() != null) {
            return functionCall(ctx.functionCall());
        }
        if (ctx.new_list() != null) {
            return listLiteral(ctx.new_list());
        }
        if (ctx.new_dict() != null) {
            return dictLiteral(ctx.new_dict());
        }
        if (ctx.tupleLiteral() != null) {
            return tupleLiteral(ctx.tupleLiteral());
        }
        if (ctx.expression() != null) {
            return expression(ctx.expression());
        }
        if (ctx.new_set() != null) {
            return setLiteral(ctx.new_set());
        }
        if (ctx.newData() != null) {
            return dataLiteral(ctx.newData());
        }
        if (ctx.matchExpression() != null) {
            return matchExpression(ctx.matchExpression());
        }
        if (ctx.value() != null) {
            return value(ctx.value());
        }
        if (ctx.infixOperator() != null && ctx.expressionNoLet().size() == 2) {
            if (ctx.infixOperator().getText().equals("|>") && ctx.expressionNoLet(1).reduceExpression() != null) {
                return reduceExpression(
                        expressionNoLet(ctx.expressionNoLet(0)),
                        ctx.expressionNoLet(1).reduceExpression(),
                        location(ctx)
                );
            }
            return binaryExpression(
                    ctx.infixOperator().getText(),
                    expressionNoLet(ctx.expressionNoLet(0)),
                    expressionNoLet(ctx.expressionNoLet(1)),
                    location(ctx),
                    isGrouped(ctx.expressionNoLet(0))
            );
        }
        if (ctx.identifier() != null && ctx.expressionNoLet().size() == 1 && hasChild(ctx, ".")) {
            return new Expression.FieldAccessExpression(
                    expressionNoLet(ctx.expressionNoLet(0)),
                    ctx.identifier().getText(),
                    location(ctx)
            );
        }
        if (ctx.methodIdentifier() != null && ctx.expressionNoLet().size() == 1) {
            return new Expression.MethodCallExpression(
                    expressionNoLet(ctx.expressionNoLet(0)),
                    ctx.methodIdentifier().getText(),
                    methodArguments(ctx.methodArgumentList()),
                    location(ctx)
            );
        }
        if (isIndexExpression(ctx)) {
            return new Expression.IndexExpression(
                    expressionNoLet(ctx.expressionNoLet(0)),
                    ctx.argumentList() == null || ctx.argumentList().expression().isEmpty()
                            ? unsupported(ctx)
                            : expression(ctx.argumentList().expression(0)),
                    location(ctx)
            );
        }
        if (isUnary(ctx)) {
            var operator = ctx.getChild(0).getText();
            return new Expression.UnaryExpression(operator, unaryOperand(operator, ctx.expressionNoLet(0)), location(ctx));
        }
        return unsupported(ctx);
    }

    private static Expression expressionNoPipe(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoPipeContext ctx) {
        if (!ctx.letExpressionNoPipe().isEmpty()) {
            return unsupported(ctx);
        }
        return expressionNoLetNoPipe(ctx.expressionNoLetNoPipe());
    }

    private static Expression expressionNoLetNoPipe(
            dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetNoPipeContext ctx
    ) {
        if (ctx.ifExpression() != null) {
            return ifExpression(ctx.ifExpression());
        }
        if (ctx.lambdaExpression() != null) {
            return lambdaExpression(ctx.lambdaExpression());
        }
        if (ctx.functionCall() != null) {
            return functionCall(ctx.functionCall());
        }
        if (ctx.new_list() != null) {
            return listLiteral(ctx.new_list());
        }
        if (ctx.new_dict() != null) {
            return dictLiteral(ctx.new_dict());
        }
        if (ctx.tupleLiteral() != null) {
            return tupleLiteral(ctx.tupleLiteral());
        }
        if (ctx.expression() != null) {
            return expression(ctx.expression());
        }
        if (ctx.new_set() != null) {
            return setLiteral(ctx.new_set());
        }
        if (ctx.newData() != null) {
            return dataLiteral(ctx.newData());
        }
        if (ctx.matchExpressionNoPipe() != null) {
            return matchExpressionNoPipe(ctx.matchExpressionNoPipe());
        }
        if (ctx.value() != null) {
            return value(ctx.value());
        }
        if (ctx.infixOperatorNoPipe() != null && ctx.expressionNoLetNoPipe().size() == 2) {
            return binaryExpression(
                    ctx.infixOperatorNoPipe().getText(),
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)),
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(1)),
                    location(ctx),
                    isGrouped(ctx.expressionNoLetNoPipe(0))
            );
        }
        if (ctx.identifier() != null && ctx.expressionNoLetNoPipe().size() == 1 && hasChild(ctx, ".")) {
            return new Expression.FieldAccessExpression(
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)),
                    ctx.identifier().getText(),
                    location(ctx)
            );
        }
        if (ctx.methodIdentifier() != null && ctx.expressionNoLetNoPipe().size() == 1) {
            return new Expression.MethodCallExpression(
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)),
                    ctx.methodIdentifier().getText(),
                    methodArguments(ctx.methodArgumentList()),
                    location(ctx)
            );
        }
        if (ctx.argumentList() != null && ctx.expressionNoLetNoPipe().size() == 1 && hasChild(ctx, "[")) {
            return new Expression.IndexExpression(
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)),
                    ctx.argumentList().expression().isEmpty()
                            ? unsupported(ctx)
                            : expression(ctx.argumentList().expression(0)),
                    location(ctx)
            );
        }
        if (isUnaryNoPipe(ctx)) {
            var operator = ctx.getChild(0).getText();
            return new Expression.UnaryExpression(operator, unaryNoPipeOperand(operator, ctx.expressionNoLetNoPipe(0)), location(ctx));
        }
        return unsupported(ctx);
    }

    private static Expression unaryOperand(
            String operator,
            dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext operand
    ) {
        if (operator.equals("-")) {
            var minLiteral = negatableMinLiteral(operand.value(), location(operand));
            if (minLiteral != null) {
                return minLiteral;
            }
        }
        return expressionNoLet(operand);
    }

    private static Expression unaryNoPipeOperand(
            String operator,
            dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetNoPipeContext operand
    ) {
        if (operator.equals("-")) {
            var minLiteral = negatableMinLiteral(operand.value(), location(operand));
            if (minLiteral != null) {
                return minLiteral;
            }
        }
        return expressionNoLetNoPipe(operand);
    }

    private static Expression negatableMinLiteral(
            dev.capylang.parser.antlr.FunctionalParser.ValueContext value,
            SourceLocation location
    ) {
        if (value == null || value.literal() == null) {
            return null;
        }
        var literal = value.literal();
        var source = literal.getText();
        if (literal.INT_LITERAL() != null && cleanNumber(source).equals("2147483648")) {
            return new Expression.IntLiteral(Integer.MIN_VALUE, source, location);
        }
        if (literal.LONG_LITERAL() != null) {
            var cleaned = cleanNumber(source.substring(0, source.length() - 1));
            if (cleaned.equals("9223372036854775808")) {
                return new Expression.LongLiteral(Long.MIN_VALUE, source, location);
            }
        }
        return null;
    }

    private static Expression binaryExpression(String operator, Expression left, Expression right, SourceLocation location, boolean leftGrouped) {
        if (!leftGrouped
                && left instanceof Expression.BinaryExpression leftBinary
                && precedence(operator) > precedence(leftBinary.operator())) {
            return new Expression.BinaryExpression(
                    leftBinary.operator(),
                    leftBinary.left(),
                    binaryExpression(operator, leftBinary.right(), right, location, false),
                    leftBinary.location()
            );
        }
        return new Expression.BinaryExpression(operator, left, right, location);
    }

    private static boolean isGrouped(ParserRuleContext ctx) {
        return ctx.getText().startsWith("(");
    }

    private static int precedence(String operator) {
        return switch (operator) {
            case "^" -> 4;
            case "*", "/", "%" -> 3;
            case "+", "-" -> 2;
            case ">", "<", ">=", "<=", "==", "!=" -> 1;
            default -> 0;
        };
    }

    private static Expression ifExpression(dev.capylang.parser.antlr.FunctionalParser.IfExpressionContext ctx) {
        return new Expression.IfExpression(
                expression(ctx.expression(0)),
                expression(ctx.expression(1)),
                expression(ctx.expression(2)),
                location(ctx)
        );
    }

    private static Expression lambdaExpression(dev.capylang.parser.antlr.FunctionalParser.LambdaExpressionContext ctx) {
        var parameters = new ArrayList<String>();
        for (var parameter : ctx.lambdaArgument()) {
            parameters.add(parameter.getText());
        }
        return new Expression.LambdaExpression(
                List.copyOf(parameters),
                expressionNoPipe(ctx.expressionNoPipe()),
                location(ctx)
        );
    }

    private static Expression reduceExpression(
            Expression receiver,
            dev.capylang.parser.antlr.FunctionalParser.ReduceExpressionContext ctx,
            SourceLocation location
    ) {
        if (ctx.lambdaArgument().size() < 2) {
            return new Expression.UnsupportedExpression(ctx.getText(), location);
        }
        return new Expression.ReduceExpression(
                receiver,
                expressionNoLetNoPipe(ctx.expressionNoLetNoPipe()),
                ctx.lambdaArgument(0).getText(),
                ctx.lambdaArgument(1).getText(),
                expressionNoPipe(ctx.expressionNoPipe()),
                location
        );
    }

    private static Expression functionCall(dev.capylang.parser.antlr.FunctionalParser.FunctionCallContext ctx) {
        var name = ctx.TYPE() != null && ctx.identifier() != null
                ? ctx.TYPE().getText() + "." + ctx.identifier().getText()
                : ctx.getChild(0).getText();
        return new Expression.FunctionCallExpression(name, arguments(ctx.argumentList()), location(ctx));
    }

    private static List<Expression> arguments(dev.capylang.parser.antlr.FunctionalParser.ArgumentListContext ctx) {
        if (ctx == null) {
            return List.of();
        }
        var arguments = new ArrayList<Expression>();
        for (var expression : ctx.expression()) {
            arguments.add(expression(expression));
        }
        return List.copyOf(arguments);
    }

    private static List<Expression> methodArguments(
            dev.capylang.parser.antlr.FunctionalParser.MethodArgumentListContext ctx
    ) {
        if (ctx == null) {
            return List.of();
        }
        var arguments = new ArrayList<Expression>();
        for (var argument : ctx.methodArgument()) {
            if (argument.expression() == null) {
                arguments.add(unsupported(argument));
            } else {
                arguments.add(expression(argument.expression()));
            }
        }
        return List.copyOf(arguments);
    }

    private static Expression listLiteral(dev.capylang.parser.antlr.FunctionalParser.New_listContext ctx) {
        var values = new ArrayList<Expression>();
        for (var expression : ctx.expression()) {
            values.add(expression(expression));
        }
        return new Expression.ListLiteral(List.copyOf(values), location(ctx));
    }

    private static Expression setLiteral(dev.capylang.parser.antlr.FunctionalParser.New_setContext ctx) {
        var values = new ArrayList<Expression>();
        for (var expression : ctx.expression()) {
            values.add(expression(expression));
        }
        return new Expression.SetLiteral(List.copyOf(values), location(ctx));
    }

    private static Expression dictLiteral(dev.capylang.parser.antlr.FunctionalParser.New_dictContext ctx) {
        var entries = new ArrayList<Expression.DictEntry>();
        for (var entry : ctx.dict_entry()) {
            entries.add(new Expression.DictEntry(
                    expression(entry.expression(0)),
                    expression(entry.expression(1)),
                    location(entry)
            ));
        }
        return new Expression.DictLiteral(List.copyOf(entries), location(ctx));
    }

    private static Expression tupleLiteral(dev.capylang.parser.antlr.FunctionalParser.TupleLiteralContext ctx) {
        var values = new ArrayList<Expression>();
        for (var expression : ctx.expression()) {
            values.add(expression(expression));
        }
        return new Expression.TupleLiteral(List.copyOf(values), location(ctx));
    }

    private static Expression dataLiteral(dev.capylang.parser.antlr.FunctionalParser.NewDataContext ctx) {
        return new Expression.DataLiteral(
                ctx.type().getText(),
                dataFields(ctx.fieldAssignmentList()),
                location(ctx)
        );
    }

    private static List<Expression.DataField> dataFields(
            dev.capylang.parser.antlr.FunctionalParser.FieldAssignmentListContext ctx
    ) {
        if (ctx == null) {
            return List.of();
        }
        var fields = new ArrayList<Expression.DataField>();
        var positionalIndex = 0;
        for (var assignment : ctx.fieldAssignment()) {
            if (assignment.namedFieldAssignment() != null) {
                fields.add(namedDataField(assignment.namedFieldAssignment()));
            } else if (assignment.positionalFieldAssignment() != null) {
                fields.add(new Expression.DataField(
                        "$" + positionalIndex,
                        expression(assignment.positionalFieldAssignment().expression()),
                        location(assignment)
                ));
                positionalIndex++;
            } else {
                fields.add(new Expression.DataField(
                        "$unsupported",
                        unsupported(assignment),
                        location(assignment)
                ));
            }
        }
        return List.copyOf(fields);
    }

    private static Expression.DataField namedDataField(
            dev.capylang.parser.antlr.FunctionalParser.NamedFieldAssignmentContext ctx
    ) {
        var name = ctx.identifier() == null
                ? unquote(ctx.STRING_LITERAL().getText())
                : ctx.identifier().getText();
        return new Expression.DataField(name, expression(ctx.expression()), location(ctx));
    }

    private static Expression matchExpression(dev.capylang.parser.antlr.FunctionalParser.MatchExpressionContext ctx) {
        var cases = new ArrayList<Expression.MatchCase>();
        for (var caseList : ctx.matchCaseList()) {
            for (var matchCase : caseList.matchCase()) {
                cases.add(matchCase(matchCase));
            }
        }
        return new Expression.MatchExpression(expression(ctx.expression()), List.copyOf(cases), location(ctx));
    }

    private static Expression matchExpressionNoPipe(
            dev.capylang.parser.antlr.FunctionalParser.MatchExpressionNoPipeContext ctx
    ) {
        var cases = new ArrayList<Expression.MatchCase>();
        for (var caseList : ctx.matchCaseNoPipeList()) {
            for (var matchCase : caseList.matchCaseNoPipe()) {
                cases.add(matchCaseNoPipe(matchCase));
            }
        }
        return new Expression.MatchExpression(expressionNoPipe(ctx.expressionNoPipe()), List.copyOf(cases), location(ctx));
    }

    private static Expression.MatchCase matchCase(dev.capylang.parser.antlr.FunctionalParser.MatchCaseContext ctx) {
        var pattern = ctx.pattern().isEmpty() ? null : ctx.pattern(0);
        return new Expression.MatchCase(
                patternTypeName(pattern),
                patternBindings(pattern),
                expression(ctx.body),
                location(ctx)
        );
    }

    private static Expression.MatchCase matchCaseNoPipe(
            dev.capylang.parser.antlr.FunctionalParser.MatchCaseNoPipeContext ctx
    ) {
        var pattern = ctx.pattern().isEmpty() ? null : ctx.pattern(0);
        return new Expression.MatchCase(
                patternTypeName(pattern),
                patternBindings(pattern),
                expressionNoPipe(ctx.body),
                location(ctx)
        );
    }

    private static String patternTypeName(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        if (ctx == null) {
            return "";
        }
        if (ctx.constructorPattern() != null) {
            return ctx.constructorPattern().TYPE().getText();
        }
        if (ctx.TYPE() != null) {
            return ctx.TYPE().getText();
        }
        return "";
    }

    private static List<String> patternBindings(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        if (ctx == null || ctx.constructorPattern() == null || ctx.constructorPattern().fieldPatternList() == null) {
            return List.of();
        }
        var bindings = new ArrayList<String>();
        for (var fieldPattern : ctx.constructorPattern().fieldPatternList().pattern()) {
            if (fieldPattern.identifier() != null) {
                bindings.add(fieldPattern.identifier().getText());
            }
        }
        return List.copyOf(bindings);
    }

    private static Expression value(dev.capylang.parser.antlr.FunctionalParser.ValueContext ctx) {
        if (ctx.identifier() != null) {
            return new Expression.VariableExpression(ctx.identifier().getText(), location(ctx));
        }
        if (ctx.qualifiedType() != null) {
            return new Expression.VariableExpression(ctx.qualifiedType().getText(), location(ctx));
        }
        var literal = ctx.literal();
        var source = literal.getText();
        if (literal.BYTE_LITERAL() != null) {
            try {
                return new Expression.IntLiteral(Integer.decode(cleanNumber(source)), source, location(ctx));
            } catch (NumberFormatException exception) {
                return unsupported(ctx);
            }
        }
        if (literal.INT_LITERAL() != null) {
            try {
                return new Expression.IntLiteral(Integer.parseInt(cleanNumber(source)), source, location(ctx));
            } catch (NumberFormatException exception) {
                return unsupported(ctx);
            }
        }
        if (literal.LONG_LITERAL() != null) {
            try {
                return new Expression.LongLiteral(Long.parseLong(cleanNumber(source.substring(0, source.length() - 1))), source, location(ctx));
            } catch (NumberFormatException exception) {
                return unsupported(ctx);
            }
        }
        if (literal.FLOAT_LITERAL() != null) {
            return new Expression.FloatLiteral(Float.parseFloat(cleanFloat(source)), source, location(ctx));
        }
        if (literal.DOUBLE_LITERAL() != null) {
            return new Expression.DoubleLiteral(Double.parseDouble(cleanDouble(source)), source, location(ctx));
        }
        if (literal.BOOL_LITERAL() != null) {
            return new Expression.BoolLiteral(Boolean.parseBoolean(source), source, location(ctx));
        }
        if (literal.STRING_LITERAL() != null) {
            return new Expression.StringLiteral(unquote(source), source, location(ctx));
        }
        return unsupported(ctx);
    }

    private static TypeReference typeReference(dev.capylang.parser.antlr.FunctionalParser.TypeContext ctx) {
        if (ctx == null) {
            return missingType();
        }
        var text = ctx.getText();
        var bracket = text.indexOf('[');
        if (bracket < 0) {
            return new TypeReference(text, List.of());
        }
        var name = text.substring(0, bracket);
        var argsText = text.substring(bracket + 1, text.length() - 1);
        return new TypeReference(name, splitTypeArguments(argsText).stream()
                .map(NativeCapybaraParser::typeReference)
                .toList());
    }

    private static TypeReference typeReference(String text) {
        var bracket = text.indexOf('[');
        if (bracket < 0) {
            return new TypeReference(text, List.of());
        }
        return new TypeReference(
                text.substring(0, bracket),
                splitTypeArguments(text.substring(bracket + 1, text.length() - 1)).stream()
                        .map(NativeCapybaraParser::typeReference)
                        .toList()
        );
    }

    private static List<String> splitTypeArguments(String text) {
        var args = new ArrayList<String>();
        var depth = 0;
        var start = 0;
        for (var i = 0; i < text.length(); i++) {
            var c = text.charAt(i);
            if (c == '[') {
                depth++;
            } else if (c == ']') {
                depth--;
            } else if (c == ',' && depth == 0) {
                args.add(text.substring(start, i).trim());
                start = i + 1;
            }
        }
        var tail = text.substring(start).trim();
        if (!tail.isEmpty()) {
            args.add(tail);
        }
        return args;
    }

    private static TypeReference missingType() {
        return new TypeReference("", List.of());
    }

    private static boolean isIndexExpression(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext ctx) {
        return ctx.argumentList() != null && ctx.expressionNoLet().size() == 1 && hasChild(ctx, "[");
    }

    private static boolean isUnary(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext ctx) {
        return ctx.expressionNoLet().size() == 1
                && (firstChildText(ctx).equals("!") || firstChildText(ctx).equals("-") || firstChildText(ctx).equals(".not."));
    }

    private static boolean isUnaryNoPipe(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetNoPipeContext ctx) {
        return ctx.expressionNoLetNoPipe().size() == 1
                && (firstChildText(ctx).equals("!") || firstChildText(ctx).equals("-") || firstChildText(ctx).equals(".not."));
    }

    private static boolean hasChild(ParserRuleContext ctx, String text) {
        for (var i = 0; i < ctx.getChildCount(); i++) {
            if (ctx.getChild(i).getText().equals(text)) {
                return true;
            }
        }
        return false;
    }

    private static String firstChildText(ParserRuleContext ctx) {
        return ctx.getChildCount() == 0 ? "" : ctx.getChild(0).getText();
    }

    private static Expression.UnsupportedExpression unsupported(ParserRuleContext ctx) {
        return new Expression.UnsupportedExpression(ctx.getText(), location(ctx));
    }

    private static SourceLocation location(ParserRuleContext ctx) {
        var start = ctx.getStart();
        return new SourceLocation(start.getLine(), start.getCharPositionInLine());
    }

    private static int leadingWhitespace(String line) {
        var idx = 0;
        while (idx < line.length() && Character.isWhitespace(line.charAt(idx))) {
            idx++;
        }
        return idx;
    }

    private static String cleanNumber(String value) {
        return value.replace("_", "");
    }

    private static String cleanFloat(String value) {
        var cleaned = cleanNumber(value);
        var last = cleaned.charAt(cleaned.length() - 1);
        return last == 'f' || last == 'F' ? cleaned.substring(0, cleaned.length() - 1) : cleaned;
    }

    private static String cleanDouble(String value) {
        var cleaned = cleanNumber(value);
        var last = cleaned.charAt(cleaned.length() - 1);
        return last == 'd' || last == 'D' ? cleaned.substring(0, cleaned.length() - 1) : cleaned;
    }

    private static String unquote(String value) {
        if (value.length() < 2) {
            return value;
        }
        return value.substring(1, value.length() - 1)
                .replace("\\\"", "\"")
                .replace("\\'", "'")
                .replace("\\n", "\n")
                .replace("\\r", "\r")
                .replace("\\t", "\t")
                .replace("\\\\", "\\");
    }

    private static String moduleFile(RawModule module) {
        var extension = switch (module.sourceKind()) {
            case FUNCTIONAL -> ".cfun";
            case OBJECT_ORIENTED -> ".coo";
        };
        return (module.path().isBlank() ? "" : module.path() + "/") + module.name() + extension;
    }

    private record ParsedSource(String body, List<ImportDeclaration> imports) {
    }

    private record SyntaxError(int line, int column, String message) {
    }
}
