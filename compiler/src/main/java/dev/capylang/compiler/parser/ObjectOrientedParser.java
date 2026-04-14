package dev.capylang.compiler.parser;

import dev.capylang.compiler.ImportDeclaration;
import dev.capylang.compiler.Result;
import dev.capylang.parser.antlr.ObjectOrientedLexer;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public final class ObjectOrientedParser {
    public static final ObjectOrientedParser INSTANCE = new ObjectOrientedParser();
    private static final Pattern IMPORT_PATTERN = Pattern.compile(
            "^\\s*from\\s+([A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][A-Za-z0-9_]*(?:/[A-Za-z_][A-Za-z0-9_]*)+)\\s+import\\s*\\{\\s*([^}]*)\\s*}(?:\\s+except\\s*\\{\\s*([^}]*)\\s*})?\\s*$"
    );

    public Result<ObjectOrientedModule> parseModule(RawModule module) {
        try {
            var parsedSource = parseSource(module.input());
            var lexer = new ObjectOrientedLexer(CharStreams.fromString(parsedSource.source()));
            var tokens = new CommonTokenStream(lexer);
            var parser = new dev.capylang.parser.antlr.ObjectOrientedParser(tokens);
            var syntaxErrors = new ArrayList<SyntaxError>();
            var errorListener = new org.antlr.v4.runtime.BaseErrorListener() {
                @Override
                public void syntaxError(
                        Recognizer<?, ?> recognizer,
                        Object offendingSymbol,
                        int line,
                        int charPositionInLine,
                        String msg,
                        RecognitionException e
                ) {
                    syntaxErrors.add(new SyntaxError(line, charPositionInLine, msg));
                }
            };
            lexer.removeErrorListeners();
            parser.removeErrorListeners();
            lexer.addErrorListener(errorListener);
            parser.addErrorListener(errorListener);

            var program = parser.program();
            if (!syntaxErrors.isEmpty()) {
                return new Result.Error<>(formatSyntaxError(module, syntaxErrors.getFirst()));
            }

            var definitions = program.definition().stream()
                    .map(this::definition)
                    .toList();
            return Result.success(new ObjectOrientedModule(
                    module.name(),
                    module.path(),
                    new ObjectOriented(definitions),
                    parsedSource.imports(),
                    module.sourceKind()
            ));
        } catch (RuntimeException exception) {
            return new Result.Error<>(new Result.Error.SingleError(0, 0, module.file(), String.valueOf(exception.getMessage())));
        }
    }

    public Result<List<ObjectOrientedModule>> parseModules(Collection<RawModule> modules) {
        var parsedModules = new ArrayList<ObjectOrientedModule>();
        var errors = new java.util.TreeSet<Result.Error.SingleError>();
        for (var module : modules) {
            var parsed = parseModule(module);
            if (parsed instanceof Result.Success<ObjectOrientedModule> success) {
                parsedModules.add(success.value());
            } else if (parsed instanceof Result.Error<ObjectOrientedModule> error) {
                errors.addAll(error.errors());
            }
        }
        return errors.isEmpty() ? Result.success(List.copyOf(parsedModules)) : new Result.Error<>(errors);
    }

    private ParsedSource parseSource(String source) {
        var imports = new ArrayList<ImportDeclaration>();
        var bodyLines = new ArrayList<String>();
        for (var line : source.split("\\R", -1)) {
            var matcher = IMPORT_PATTERN.matcher(line);
            if (matcher.matches()) {
                var module = matcher.group(1);
                var symbols = Stream.of(matcher.group(2).split(","))
                        .map(String::trim)
                        .filter(symbol -> !symbol.isBlank())
                        .toList();
                var excludedSymbols = matcher.group(3) == null
                        ? List.<String>of()
                        : Stream.of(matcher.group(3).split(","))
                                .map(String::trim)
                                .filter(symbol -> !symbol.isBlank())
                                .toList();
                imports.add(new ImportDeclaration(module, symbols, excludedSymbols));
                bodyLines.add("");
            } else {
                bodyLines.add(line);
            }
        }
        return new ParsedSource(String.join(System.lineSeparator(), bodyLines), List.copyOf(imports));
    }

    private Result.Error.SingleError formatSyntaxError(RawModule module, SyntaxError syntaxError) {
        var details = "line %d:%d: %s".formatted(syntaxError.line(), syntaxError.column(), syntaxError.message());
        return new Result.Error.SingleError(syntaxError.line(), syntaxError.column(), module.file(), details);
    }

    private ObjectOriented.TypeDeclaration definition(dev.capylang.parser.antlr.ObjectOrientedParser.DefinitionContext context) {
        if (context.classDeclaration() != null) {
            return classDeclaration(context.classDeclaration());
        }
        if (context.traitDeclaration() != null) {
            return traitDeclaration(context.traitDeclaration());
        }
        return interfaceDeclaration(context.interfaceDeclaration());
    }

    private ObjectOriented.ClassDeclaration classDeclaration(dev.capylang.parser.antlr.ObjectOrientedParser.ClassDeclarationContext context) {
        return new ObjectOriented.ClassDeclaration(
                context.TYPE().getText(),
                parameters(context.constructorParameters() == null ? null : context.constructorParameters().parameters()),
                typeReferences(context.inheritanceClause() == null ? null : context.inheritanceClause().qualifiedType()),
                members(context.typeBody().memberDeclaration()),
                context.classModifier().stream().map(org.antlr.v4.runtime.RuleContext::getText).toList()
        );
    }

    private ObjectOriented.TraitDeclaration traitDeclaration(dev.capylang.parser.antlr.ObjectOrientedParser.TraitDeclarationContext context) {
        return new ObjectOriented.TraitDeclaration(
                context.TYPE().getText(),
                typeReferences(context.inheritanceClause() == null ? null : context.inheritanceClause().qualifiedType()),
                members(context.typeBody().memberDeclaration())
        );
    }

    private ObjectOriented.InterfaceDeclaration interfaceDeclaration(dev.capylang.parser.antlr.ObjectOrientedParser.InterfaceDeclarationContext context) {
        var members = context.interfaceBody().interfaceMemberDeclaration().stream()
                .<ObjectOriented.MemberDeclaration>map(interfaceMember -> methodDeclaration(interfaceMember.interfaceMethodDeclaration()))
                .toList();
        return new ObjectOriented.InterfaceDeclaration(
                context.TYPE().getText(),
                typeReferences(context.inheritanceClause() == null ? null : context.inheritanceClause().qualifiedType()),
                members
        );
    }

    private List<ObjectOriented.MemberDeclaration> members(List<dev.capylang.parser.antlr.ObjectOrientedParser.MemberDeclarationContext> members) {
        return members.stream()
                .<ObjectOriented.MemberDeclaration>map(member -> {
                    if (member.fieldDeclaration() != null) {
                        return fieldDeclaration(member.fieldDeclaration());
                    }
                    if (member.methodDeclaration() != null) {
                        return methodDeclaration(member.methodDeclaration());
                    }
                    return initBlock(member.initBlock());
                })
                .toList();
    }

    private ObjectOriented.FieldDeclaration fieldDeclaration(dev.capylang.parser.antlr.ObjectOrientedParser.FieldDeclarationContext context) {
        return new ObjectOriented.FieldDeclaration(
                context.identifier().getText(),
                context.type().getText(),
                context.visibility() == null ? "public" : context.visibility().getText(),
                context.expression() == null ? java.util.Optional.empty() : java.util.Optional.of(context.expression().getText())
        );
    }

    private ObjectOriented.MethodDeclaration methodDeclaration(dev.capylang.parser.antlr.ObjectOrientedParser.MethodDeclarationContext context) {
        return new ObjectOriented.MethodDeclaration(
                context.identifier().getText(),
                parameters(context.parameters()),
                context.functionType().type().getText(),
                context.visibility() == null ? "public" : context.visibility().getText(),
                context.methodModifier().stream().map(org.antlr.v4.runtime.RuleContext::getText).toList(),
                context.methodBody() == null ? java.util.Optional.empty() : java.util.Optional.of(methodBody(context.methodBody()))
        );
    }

    private ObjectOriented.MethodDeclaration methodDeclaration(dev.capylang.parser.antlr.ObjectOrientedParser.InterfaceMethodDeclarationContext context) {
        return new ObjectOriented.MethodDeclaration(
                context.identifier().getText(),
                parameters(context.parameters()),
                context.functionType().type().getText(),
                context.visibility() == null ? "public" : context.visibility().getText(),
                context.methodModifier().stream().map(org.antlr.v4.runtime.RuleContext::getText).toList(),
                java.util.Optional.empty()
        );
    }

    private ObjectOriented.InitBlock initBlock(dev.capylang.parser.antlr.ObjectOrientedParser.InitBlockContext context) {
        return new ObjectOriented.InitBlock(statementBlock(context.statementBlock()));
    }

    private ObjectOriented.MethodBody methodBody(dev.capylang.parser.antlr.ObjectOrientedParser.MethodBodyContext context) {
        if (context.expression() != null) {
            return new ObjectOriented.ExpressionBody(context.expression().getText());
        }
        return statementBlock(context.statementBlock());
    }

    private ObjectOriented.StatementBlock statementBlock(dev.capylang.parser.antlr.ObjectOrientedParser.StatementBlockContext context) {
        return new ObjectOriented.StatementBlock(context.statement().stream().map(this::statement).toList());
    }

    private ObjectOriented.Statement statement(dev.capylang.parser.antlr.ObjectOrientedParser.StatementContext context) {
        if (context.letStatement() != null) {
            return letStatement(context.letStatement());
        }
        if (context.returnStatement() != null) {
            return returnStatement(context.returnStatement());
        }
        if (context.ifStatement() != null) {
            return ifStatement(context.ifStatement());
        }
        return statementBlock(context.statementBlock());
    }

    private ObjectOriented.LetStatement letStatement(dev.capylang.parser.antlr.ObjectOrientedParser.LetStatementContext context) {
        return new ObjectOriented.LetStatement(
                context.identifier().getText(),
                context.type() == null ? java.util.Optional.empty() : java.util.Optional.of(context.type().getText()),
                context.expression().getText()
        );
    }

    private ObjectOriented.ReturnStatement returnStatement(dev.capylang.parser.antlr.ObjectOrientedParser.ReturnStatementContext context) {
        return new ObjectOriented.ReturnStatement(context.expression().getText());
    }

    private ObjectOriented.IfStatement ifStatement(dev.capylang.parser.antlr.ObjectOrientedParser.IfStatementContext context) {
        java.util.Optional<ObjectOriented.Statement> elseBranch = context.ifStatement() != null
                ? java.util.Optional.of(ifStatement(context.ifStatement()))
                : context.statementBlock().size() > 1
                        ? java.util.Optional.of(statementBlock(context.statementBlock(1)))
                        : java.util.Optional.empty();
        return new ObjectOriented.IfStatement(
                context.expression().getText(),
                statementBlock(context.statementBlock(0)),
                elseBranch
        );
    }

    private List<ObjectOriented.Parameter> parameters(dev.capylang.parser.antlr.ObjectOrientedParser.ParametersContext context) {
        if (context == null) {
            return List.of();
        }
        return context.parameter().stream()
                .map(parameter -> new ObjectOriented.Parameter(parameter.identifier().getText(), parameter.type().getText()))
                .toList();
    }

    private List<ObjectOriented.TypeReference> typeReferences(List<dev.capylang.parser.antlr.ObjectOrientedParser.QualifiedTypeContext> contexts) {
        if (contexts == null) {
            return List.of();
        }
        return contexts.stream()
                .map(context -> new ObjectOriented.TypeReference(context.getText()))
                .toList();
    }

    private record ParsedSource(String source, List<ImportDeclaration> imports) {
    }

    private record SyntaxError(int line, int column, String message) {
    }
}
