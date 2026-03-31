package dev.capylang.compiler.parser;


import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.TerminalNode;
import dev.capylang.compiler.ImportDeclaration;
import dev.capylang.parser.antlr.FunctionalParser;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toSet;

public class CapybaraParser {
    public static final CapybaraParser INSTANCE = new CapybaraParser();
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String METHOD_INVOKE_PREFIX = "__invoke__";
    private static final Pattern COLLECTION_LIST_PATTERN = Pattern.compile("list\\[(.+?)]");
    private static final Pattern COLLECTION_SET_PATTERN = Pattern.compile("set\\[(.+?)]");
    private static final Pattern COLLECTION_DICT_PATTERN = Pattern.compile("dict\\[(.+?)]");
    private static final Pattern NO_VIABLE_ALTERNATIVE_PATTERN = Pattern.compile("no viable alternative at input '(.+)'");
    private static final Pattern MISMATCHED_INPUT_PATTERN = Pattern.compile("mismatched input '(.+)' expecting '(.+)'");
    private static final Pattern CONST_NAME_PATTERN = Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");
    private static final Pattern ENUM_VALUE_NAME_PATTERN = Pattern.compile("^[A-Z]+(?:_[A-Z]+)*$");
    private static final Pattern IMPORT_PATTERN = Pattern.compile(
            "^\\s*from\\s+([A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][A-Za-z0-9_]*(?:/[A-Za-z_][A-Za-z0-9_]*)+)\\s+import\\s*\\{\\s*([^}]*)\\s*}(?:\\s+except\\s*\\{\\s*([^}]*)\\s*})?\\s*$"
    );

    public Program parseModule(Collection<RawModule> rawModules) {
        var modules = rawModules.stream().map(this::parseModule).toList();
        return new Program(modules);
    }

    public Module parseModule(RawModule module) {
        var parsedSource = parseSource(module.input());
        var lexer = new dev.capylang.parser.antlr.FunctionalLexer(CharStreams.fromString(parsedSource.source()));
        var tokens = new CommonTokenStream(lexer);
        tokens.fill();
        var parser = new dev.capylang.parser.antlr.FunctionalParser(tokens);
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
            throw new IllegalStateException(formatSyntaxError(syntaxErrors.get(0)));
        }

        var definitions = program.definition().stream()
                .map(this::definition)
                .flatMap(Collection::stream)
                .collect(toSet());
        return new Module(module.name(),module.path(), new Functional(definitions), parsedSource.imports());
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
                // Keep source line numbers stable for parser/linker diagnostics.
                bodyLines.add("");
            } else {
                bodyLines.add(line);
            }
        }
        return new ParsedSource(String.join(System.lineSeparator(), bodyLines), List.copyOf(imports));
    }

    private String formatSyntaxError(SyntaxError syntaxError) {
        if (syntaxError.message().contains("no viable alternative at input 'match")
            && syntaxError.message().contains("=")) {
            return "line %d:%d: Expected `->`, found `=`".formatted(syntaxError.line(), syntaxError.column());
        }
        var noViableAlternativeMatcher = NO_VIABLE_ALTERNATIVE_PATTERN.matcher(syntaxError.message());
        if (noViableAlternativeMatcher.matches()) {
            var offendingInput = noViableAlternativeMatcher.group(1)
                    .replaceAll("\\s+", " ")
                    .trim();
            var preview = offendingInput.length() > 80
                    ? offendingInput.substring(0, 77) + "..."
                    : offendingInput;
            return "line %d:%d: Syntax error near `%s`".formatted(syntaxError.line(), syntaxError.column(), preview);
        }
        if (syntaxError.message().contains("mismatched input '=' expecting {']', ','}")
            || syntaxError.message().equals("Expected `]`, found `=`")) {
            return "line %d:%d: Expected `]`, found `=`".formatted(syntaxError.line(), syntaxError.column());
        }
        if (syntaxError.message().contains("mismatched input '=' expecting '->'")
            || syntaxError.message().equals("Expected `->`, found `=`")) {
            return "line %d:%d: Expected `->`, found `=`".formatted(syntaxError.line(), syntaxError.column());
        }
        var mismatchedInputMatcher = MISMATCHED_INPUT_PATTERN.matcher(syntaxError.message());
        if (mismatchedInputMatcher.matches()) {
            var found = mismatchedInputMatcher.group(1);
            var expected = mismatchedInputMatcher.group(2);
            return "line %d:%d: Expected `%s`, found `%s`".formatted(syntaxError.line(), syntaxError.column(), expected, found);
        }
        return "line %d:%d: %s".formatted(syntaxError.line(), syntaxError.column(), syntaxError.message());
    }

    private record ParsedSource(String source, List<ImportDeclaration> imports) {
    }

    private record SyntaxError(int line, int column, String message) {
    }

    private List<Definition> definition(dev.capylang.parser.antlr.FunctionalParser.DefinitionContext context) {
        var functionDeclaration = context.functionDeclaration();
        if (functionDeclaration != null) {
            return functionDeclaration(functionDeclaration);
        }

        var dataDeclaration = context.dataDeclaration();
        if (dataDeclaration != null) {
            return List.of(dataDeclaration(dataDeclaration));
        }

        var enumDeclaration = context.enumDeclaration();
        if (enumDeclaration != null) {
            return List.of(enumDeclaration(enumDeclaration));
        }

        var singleDeclaration = context.singleDeclaration();
        if (singleDeclaration != null) {
            return List.of(singleDeclaration(singleDeclaration));
        }

        var constDeclaration = context.constDeclaration();
        if (constDeclaration != null) {
            return List.of(constDeclaration(constDeclaration));
        }

        var typeDeclaration = context.typeDeclaration();
        if (typeDeclaration != null) {
            return List.of(typeDeclaration(typeDeclaration));
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
                context.docComment().stream()
                        .map(comment -> stripDocComment(comment.getText()))
                        .toList(),
                context.VISIBILITY() != null ? dev.capylang.compiler.Visibility.LOCAL : null,
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
                context.docComment().stream()
                        .map(comment -> stripDocComment(comment.getText()))
                        .toList(),
                context.VISIBILITY() != null ? dev.capylang.compiler.Visibility.LOCAL : null,
                position(context)
        );
    }

    private EnumDeclaration enumDeclaration(FunctionalParser.EnumDeclarationContext context) {
        var values = context.TYPE().stream().skip(1).map(TerminalNode::getText).toList();
        values.forEach(CapybaraParser::validateEnumValueName);
        return new EnumDeclaration(
                context.TYPE(0).getText(),
                values,
                position(context)
        );
    }

    private SingleDeclaration singleDeclaration(FunctionalParser.SingleDeclarationContext context) {
        return new SingleDeclaration(context.TYPE().getText(), position(context));
    }

    private Function constDeclaration(FunctionalParser.ConstDeclarationContext context) {
        var name = context.TYPE().getText();
        validateConstName(name);
        var constExpression = expressionNoLet(context.expressionNoLet());
        return new Function(
                name,
                List.of(),
                Optional.ofNullable(context.type()).map(CapybaraParser::type).or(() -> inferConstType(constExpression)),
                constExpression,
                List.of(),
                context.VISIBILITY() != null ? dev.capylang.compiler.Visibility.LOCAL : null,
                position(context)
        );
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
        return new DataDeclaration.DataField(fieldName(context.identifier(), context.STRING_LITERAL()), type(context.type()));
    }

    private List<Definition> functionDeclaration(dev.capylang.parser.antlr.FunctionalParser.FunctionDeclarationContext functionDeclarationContext) {
        var functionNameDeclaration = functionDeclarationContext.functionNameDeclaration();
        var methodOwner = functionNameDeclaration.genericTypeDeclaration();
        var methodName = functionName(functionNameDeclaration);
        var parameters = functionDeclarationContext.parameters() == null
                ? List.<Parameter>of()
                : functionDeclarationContext.parameters()
                .parameter()
                .stream()
                .map(this::parameter)
                .toList();
        if (methodOwner != null) {
            var ownerType = type(methodOwner.getText());
            var thisParameter = new Parameter(ownerType, "this", position(functionNameDeclaration));
            var methodParameters = new java.util.ArrayList<Parameter>(parameters.size() + 1);
            methodParameters.add(thisParameter);
            methodParameters.addAll(parameters);
            parameters = List.copyOf(methodParameters);
            methodName = METHOD_DECL_PREFIX + genericTypeName(methodOwner) + "__" + methodName;
        }
        var localDefinitions = functionDeclarationContext.functionBody().localDefinition();
        var localFunctionNameMap = new java.util.LinkedHashMap<String, String>();
        var localTypeNameMap = new java.util.LinkedHashMap<String, String>();
        var localConstNameMap = new java.util.LinkedHashMap<String, String>();
        var localFunctionIndex = 0;
        var localTypeIndex = 0;
        var localConstIndex = 0;
        for (var localDefinition : localDefinitions) {
            var localFunction = localDefinition.localFunctionDeclaration();
            if (localFunction != null) {
                var localName = localFunction.NAME().getText();
                if (!localName.startsWith("__")) {
                    throw new IllegalStateException("Local function name has to start with `__`: " + localName);
                }
                if (localFunctionNameMap.containsKey(localName)) {
                    throw new IllegalStateException("Duplicate local function name: " + localName);
                }
                localFunctionNameMap.put(
                        localName,
                        "__" + methodName + "__local_fun_" + localFunctionIndex + "_" + localName.substring(2)
                );
                localFunctionIndex++;
            }
            var localType = localDefinition.localTypeDeclaration();
            if (localType != null) {
                var localTypeName = genericTypeName(localType.genericTypeDeclaration(0));
                if (!localTypeName.startsWith("__")) {
                    throw new IllegalStateException("Local type name has to start with `__`: " + localTypeName);
                }
                if (localTypeNameMap.containsKey(localTypeName)) {
                    throw new IllegalStateException("Duplicate local type/data name: " + localTypeName);
                }
                localTypeNameMap.put(
                        localTypeName,
                        "__" + methodName + "__local_type_" + localTypeIndex + "_" + localTypeName.substring(2)
                );
                localTypeIndex++;
            }
            var localData = localDefinition.localDataDeclaration();
            if (localData != null) {
                var localDataName = genericTypeName(localData.genericTypeDeclaration());
                if (!localDataName.startsWith("__")) {
                    throw new IllegalStateException("Local data name has to start with `__`: " + localDataName);
                }
                if (localTypeNameMap.containsKey(localDataName)) {
                    throw new IllegalStateException("Duplicate local type/data name: " + localDataName);
                }
                localTypeNameMap.put(
                        localDataName,
                        "__" + methodName + "__local_type_" + localTypeIndex + "_" + localDataName.substring(2)
                );
                localTypeIndex++;
            }
            var localConst = localDefinition.localConstDeclaration();
            if (localConst != null) {
                var localConstName = localConst.TYPE().getText();
                if (!localConstName.startsWith("__")) {
                    throw new IllegalStateException("Local const name has to start with `__`: " + localConstName);
                }
                validateConstName(localConstName);
                if (localConstNameMap.containsKey(localConstName)) {
                    throw new IllegalStateException("Duplicate local const name: " + localConstName);
                }
                localConstNameMap.put(
                        localConstName,
                        "__" + methodName + "__local_const_" + localConstIndex + "_" + localConstName.substring(2)
                );
                localConstIndex++;
            }
        }

        var extractedLocalDefinitions = new java.util.ArrayList<Definition>();
        for (var localDefinition : localDefinitions) {
            if (localDefinition.localFunctionDeclaration() != null) {
                extractedLocalDefinitions.add(
                        localFunctionDeclaration(
                                localDefinition.localFunctionDeclaration(),
                                localFunctionNameMap,
                                localTypeNameMap,
                                localConstNameMap
                        )
                );
            } else if (localDefinition.localTypeDeclaration() != null) {
                extractedLocalDefinitions.add(
                        localTypeDeclaration(localDefinition.localTypeDeclaration(), localTypeNameMap)
                );
            } else if (localDefinition.localDataDeclaration() != null) {
                extractedLocalDefinitions.add(
                        dataDeclaration(localDefinition.localDataDeclaration(), localTypeNameMap)
                );
            } else if (localDefinition.localConstDeclaration() != null) {
                extractedLocalDefinitions.add(
                        localConstDeclaration(
                                localDefinition.localConstDeclaration(),
                                localFunctionNameMap,
                                localTypeNameMap,
                                localConstNameMap
                        )
                );
            } else {
                throw new IllegalStateException("Unknown local definition: " + localDefinition.getText());
            }
        }

        parameters = parameters.stream()
                .map(parameter -> new Parameter(
                        rewriteLocalTypeNames(parameter.type(), localTypeNameMap),
                        parameter.name(),
                        parameter.position()))
                .toList();
        var rewrittenReturnType = functionType(functionDeclarationContext.functionType())
                .map(type -> rewriteLocalTypeNames(type, localTypeNameMap));

        var expression = rewriteLocalNames(
                expression(functionDeclarationContext.functionBody().expression()),
                localFunctionNameMap,
                localTypeNameMap,
                localConstNameMap
        );
        var topLevelFunction = new Function(
                methodName,
                parameters,
                rewrittenReturnType,
                expression,
                functionDeclarationContext.docComment().stream()
                        .map(comment -> stripDocComment(comment.getText()))
                        .toList(),
                functionDeclarationContext.VISIBILITY() != null ? dev.capylang.compiler.Visibility.LOCAL : null,
                position(functionDeclarationContext)
        );
        var allDefinitions = new java.util.ArrayList<Definition>(1 + extractedLocalDefinitions.size());
        allDefinitions.add(topLevelFunction);
        allDefinitions.addAll(extractedLocalDefinitions);
        return List.copyOf(allDefinitions);
    }

    private Function localFunctionDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.LocalFunctionDeclarationContext context,
            java.util.Map<String, String> localFunctionNameMap,
            java.util.Map<String, String> localTypeNameMap,
            java.util.Map<String, String> localConstNameMap
    ) {
        var localName = context.NAME().getText();
        var mappedName = localFunctionNameMap.get(localName);
        if (mappedName == null) {
            throw new IllegalStateException("Unknown local function mapping for: " + localName);
        }
        var parameters = context.parameters() == null
                ? List.<Parameter>of()
                : context.parameters().parameter().stream().map(this::parameter).toList();
        parameters = parameters.stream()
                .map(parameter -> new Parameter(
                        rewriteLocalTypeNames(parameter.type(), localTypeNameMap),
                        parameter.name(),
                        parameter.position()))
                .toList();
        var expression = rewriteLocalNames(expression(context.expression()), localFunctionNameMap, localTypeNameMap, localConstNameMap);
        var rewrittenReturnType = functionType(context.functionType())
                .map(type -> rewriteLocalTypeNames(type, localTypeNameMap));
        return new Function(
                mappedName,
                parameters,
                rewrittenReturnType,
                expression,
                context.docComment().stream()
                        .map(comment -> stripDocComment(comment.getText()))
                        .toList(),
                position(context)
        );
    }

    private Function localConstDeclaration(
            FunctionalParser.LocalConstDeclarationContext context,
            java.util.Map<String, String> localFunctionNameMap,
            java.util.Map<String, String> localTypeNameMap,
            java.util.Map<String, String> localConstNameMap
    ) {
        var localName = context.TYPE().getText();
        var mappedName = localConstNameMap.get(localName);
        if (mappedName == null) {
            throw new IllegalStateException("Unknown local const mapping for: " + localName);
        }
        var constExpression = rewriteLocalNames(expressionNoLet(context.expressionNoLet()), localFunctionNameMap, localTypeNameMap, localConstNameMap);
        return new Function(
                mappedName,
                List.of(),
                Optional.ofNullable(context.type())
                        .map(CapybaraParser::type)
                        .map(type -> rewriteLocalTypeNames(type, localTypeNameMap))
                        .or(() -> inferConstType(constExpression)),
                constExpression,
                List.of(),
                position(context)
        );
    }

    private TypeDeclaration localTypeDeclaration(
            FunctionalParser.LocalTypeDeclarationContext context,
            java.util.Map<String, String> localTypeNameMap
    ) {
        var declaredType = context.genericTypeDeclaration(0);
        var declaredTypeName = genericTypeName(declaredType);
        var mappedTypeName = localTypeNameMap.get(declaredTypeName);
        if (mappedTypeName == null) {
            throw new IllegalStateException("Unknown local type mapping for: " + declaredTypeName);
        }
        var fieldDeclarationList = Optional.of(context)
                .map(FunctionalParser.LocalTypeDeclarationContext::fieldDeclarationList)
                .map(this::fieldDeclarationList)
                .stream()
                .flatMap(Collection::stream)
                .map(field -> new DataDeclaration.DataField(
                        field.name(),
                        rewriteLocalTypeNames(field.type(), localTypeNameMap)))
                .toList();
        var subTypes = context.genericTypeDeclaration().stream()
                .skip(1)
                .map(CapybaraParser::genericTypeName)
                .map(name -> rewriteLocalTypeName(name, localTypeNameMap))
                .toList();

        return new TypeDeclaration(
                mappedTypeName,
                subTypes,
                fieldDeclarationList,
                genericTypeParameters(declaredType),
                List.of(),
                position(context)
        );
    }

    private DataDeclaration dataDeclaration(
            FunctionalParser.LocalDataDeclarationContext context,
            java.util.Map<String, String> localTypeNameMap
    ) {
        var declaration = context.genericTypeDeclaration();
        var localDataName = genericTypeName(declaration);
        var mappedDataName = localTypeNameMap.get(localDataName);
        if (mappedDataName == null) {
            throw new IllegalStateException("Unknown local data mapping for: " + localDataName);
        }
        var dataFields = dataFieldDeclarationList(context.fieldDeclarationList());
        return new DataDeclaration(
                mappedDataName,
                dataFields.fields().stream()
                        .map(field -> new DataDeclaration.DataField(
                                field.name(),
                                rewriteLocalTypeNames(field.type(), localTypeNameMap)
                        ))
                        .toList(),
                dataFields.extendsTypes().stream()
                        .map(name -> rewriteLocalTypeName(name, localTypeNameMap))
                        .toList(),
                genericTypeParameters(declaration),
                List.of(),
                position(context)
        );
    }

    private Expression rewriteLocalNames(
            Expression expression,
            java.util.Map<String, String> localFunctionNameMap,
            java.util.Map<String, String> localTypeNameMap,
            java.util.Map<String, String> localConstNameMap
    ) {
        return switch (expression) {
            case FunctionCall functionCall -> {
                if (functionCall.moduleName().isEmpty() && localConstNameMap.containsKey(functionCall.name())) {
                    yield new FunctionCall(
                            Optional.empty(),
                            localConstNameMap.get(functionCall.name()),
                            functionCall.arguments().stream()
                                    .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                                    .toList(),
                            functionCall.position()
                    );
                }
                if (functionCall.moduleName().isEmpty() && localFunctionNameMap.containsKey(functionCall.name())) {
                    yield new FunctionCall(
                            Optional.empty(),
                            localFunctionNameMap.get(functionCall.name()),
                            functionCall.arguments().stream()
                                    .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                                    .toList(),
                            functionCall.position()
                    );
                }
                var rewrittenModuleName = functionCall.moduleName()
                        .map(name -> rewriteLocalTypeName(name, localTypeNameMap));
                yield new FunctionCall(
                        rewrittenModuleName,
                        functionCall.name(),
                        functionCall.arguments().stream()
                                .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                                .toList(),
                        functionCall.position()
                );
            }
            case Value value -> {
                var mappedConst = localConstNameMap.get(value.name());
                if (mappedConst != null) {
                    yield new FunctionCall(Optional.empty(), mappedConst, List.of(), value.position());
                }
                yield value;
            }
            case FunctionReference functionReference -> {
                var mapped = localFunctionNameMap.get(functionReference.name());
                if (mapped != null) {
                    yield new FunctionReference(mapped, functionReference.position());
                }
                yield functionReference;
            }
            case FunctionInvoke functionInvoke -> new FunctionInvoke(
                    rewriteLocalNames(functionInvoke.function(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    functionInvoke.arguments().stream()
                            .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                            .toList(),
                    functionInvoke.position()
            );
            case LetExpression letExpression -> new LetExpression(
                    letExpression.name(),
                    letExpression.declaredType().map(type -> rewriteLocalTypeNames(type, localTypeNameMap)),
                    rewriteLocalNames(letExpression.value(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    rewriteLocalNames(letExpression.rest(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    letExpression.position()
            );
            case IfExpression ifExpression -> new IfExpression(
                    rewriteLocalNames(ifExpression.condition(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    rewriteLocalNames(ifExpression.thenBranch(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    rewriteLocalNames(ifExpression.elseBranch(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    ifExpression.position()
            );
            case InfixExpression infixExpression -> new InfixExpression(
                    rewriteLocalNames(infixExpression.left(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    infixExpression.operator(),
                    rewriteLocalNames(infixExpression.right(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    infixExpression.position()
            );
            case FieldAccess fieldAccess -> new FieldAccess(
                    rewriteLocalNames(fieldAccess.source(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    fieldAccess.field(),
                    fieldAccess.position()
            );
            case LambdaExpression lambdaExpression -> new LambdaExpression(
                    lambdaExpression.argumentNames(),
                    rewriteLocalNames(lambdaExpression.expression(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    lambdaExpression.position()
            );
            case ReduceExpression reduceExpression -> new ReduceExpression(
                    rewriteLocalNames(reduceExpression.initialValue(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    reduceExpression.accumulatorName(),
                    reduceExpression.keyName(),
                    reduceExpression.valueName(),
                    rewriteLocalNames(reduceExpression.reducerExpression(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    reduceExpression.position()
            );
            case IndexExpression indexExpression -> new IndexExpression(
                    rewriteLocalNames(indexExpression.source(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    rewriteLocalNames(indexExpression.index(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    indexExpression.position()
            );
            case SliceExpression sliceExpression -> new SliceExpression(
                    rewriteLocalNames(sliceExpression.source(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    sliceExpression.start().map(start -> rewriteLocalNames(start, localFunctionNameMap, localTypeNameMap, localConstNameMap)),
                    sliceExpression.end().map(end -> rewriteLocalNames(end, localFunctionNameMap, localTypeNameMap, localConstNameMap)),
                    sliceExpression.position()
            );
            case MatchExpression matchExpression -> new MatchExpression(
                    rewriteLocalNames(matchExpression.matchWith(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                    matchExpression.cases().stream()
                            .map(matchCase -> new MatchExpression.MatchCase(
                                    rewriteLocalTypeNames(matchCase.pattern(), localTypeNameMap),
                                    rewriteLocalNames(matchCase.expression(), localFunctionNameMap, localTypeNameMap, localConstNameMap)
                            ))
                            .toList(),
                    matchExpression.position()
            );
            case NewData newData -> new NewData(
                    rewriteLocalTypeNames(newData.type(), localTypeNameMap),
                    newData.assignments().stream()
                            .map(assignment -> new NewData.FieldAssignment(
                                    assignment.name(),
                                    rewriteLocalNames(assignment.value(), localFunctionNameMap, localTypeNameMap, localConstNameMap)
                            ))
                            .toList(),
                    newData.positionalArguments().stream()
                            .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                            .toList(),
                    newData.spreads().stream()
                            .map(spread -> rewriteLocalNames(spread, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                            .toList(),
                    newData.position()
            );
            case NewListExpression newListExpression -> new NewListExpression(
                    newListExpression.values().stream()
                            .map(value -> rewriteLocalNames(value, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                            .toList(),
                    newListExpression.position()
            );
            case NewSetExpression newSetExpression -> new NewSetExpression(
                    newSetExpression.values().stream()
                            .map(value -> rewriteLocalNames(value, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                            .toList(),
                    newSetExpression.position()
            );
            case NewDictExpression newDictExpression -> new NewDictExpression(
                    newDictExpression.entries().stream()
                            .map(entry -> new NewDictExpression.Entry(
                                    rewriteLocalNames(entry.key(), localFunctionNameMap, localTypeNameMap, localConstNameMap),
                                    rewriteLocalNames(entry.value(), localFunctionNameMap, localTypeNameMap, localConstNameMap)
                            ))
                            .toList(),
                    newDictExpression.position()
            );
            case TupleExpression tupleExpression -> new TupleExpression(
                    tupleExpression.values().stream()
                            .map(value -> rewriteLocalNames(value, localFunctionNameMap, localTypeNameMap, localConstNameMap))
                            .toList(),
                    tupleExpression.position()
            );
            default -> expression;
        };
    }

    private MatchExpression.Pattern rewriteLocalTypeNames(
            MatchExpression.Pattern pattern,
            java.util.Map<String, String> localTypeNameMap
    ) {
        return switch (pattern) {
            case MatchExpression.TypedPattern typedPattern -> new MatchExpression.TypedPattern(
                    rewriteLocalTypeNames(typedPattern.type(), localTypeNameMap),
                    typedPattern.name()
            );
            case MatchExpression.ConstructorPattern constructorPattern -> new MatchExpression.ConstructorPattern(
                    rewriteLocalTypeName(constructorPattern.constructorName(), localTypeNameMap),
                    constructorPattern.fieldPatterns().stream()
                            .map(fieldPattern -> rewriteLocalTypeNames(fieldPattern, localTypeNameMap))
                            .toList()
            );
            case MatchExpression.VariablePattern variablePattern -> {
                var mapped = localTypeNameMap.get(variablePattern.name());
                if (mapped != null) {
                    yield new MatchExpression.VariablePattern(mapped);
                }
                yield variablePattern;
            }
            default -> pattern;
        };
    }

    private Type rewriteLocalTypeNames(Type type, java.util.Map<String, String> localTypeNameMap) {
        return switch (type) {
            case DataType dataType -> new DataType(rewriteLocalTypeName(dataType.name(), localTypeNameMap));
            case CollectionType.ListType listType -> new CollectionType.ListType(
                    rewriteLocalTypeNames(listType.elementType(), localTypeNameMap)
            );
            case CollectionType.SetType setType -> new CollectionType.SetType(
                    rewriteLocalTypeNames(setType.elementType(), localTypeNameMap)
            );
            case CollectionType.DictType dictType -> new CollectionType.DictType(
                    rewriteLocalTypeNames(dictType.valueType(), localTypeNameMap)
            );
            case FunctionType functionType -> new FunctionType(
                    rewriteLocalTypeNames(functionType.argumentType(), localTypeNameMap),
                    rewriteLocalTypeNames(functionType.returnType(), localTypeNameMap)
            );
            case TupleType tupleType -> new TupleType(
                    tupleType.elementTypes().stream()
                            .map(elementType -> rewriteLocalTypeNames(elementType, localTypeNameMap))
                            .toList()
            );
            default -> type;
        };
    }

    private String rewriteLocalTypeName(String typeName, java.util.Map<String, String> localTypeNameMap) {
        var genericStart = typeName.indexOf('[');
        if (genericStart < 0) {
            return localTypeNameMap.getOrDefault(typeName, typeName);
        }
        var baseName = typeName.substring(0, genericStart);
        var mappedBaseName = localTypeNameMap.get(baseName);
        var rewrittenBaseName = mappedBaseName == null ? baseName : mappedBaseName;
        var genericArguments = typeName.substring(genericStart + 1, typeName.length() - 1);
        var rewrittenArguments = splitTopLevelGenericArguments(genericArguments).stream()
                .map(argument -> rewriteLocalTypeName(argument, localTypeNameMap))
                .toList();
        return rewrittenBaseName + "[" + String.join(", ", rewrittenArguments) + "]";
    }

    private List<String> splitTopLevelGenericArguments(String genericArguments) {
        if (genericArguments.isBlank()) {
            return List.of();
        }
        var arguments = new java.util.ArrayList<String>();
        var current = new StringBuilder();
        var depth = 0;
        for (int i = 0; i < genericArguments.length(); i++) {
            var ch = genericArguments.charAt(i);
            if (ch == '[') {
                depth++;
                current.append(ch);
                continue;
            }
            if (ch == ']') {
                depth--;
                current.append(ch);
                continue;
            }
            if (ch == ',' && depth == 0) {
                arguments.add(current.toString().trim());
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        arguments.add(current.toString().trim());
        return List.copyOf(arguments);
    }

    private Optional<Type> functionType(dev.capylang.parser.antlr.FunctionalParser.FunctionTypeContext context) {
        return Optional.ofNullable(context)
                .map(dev.capylang.parser.antlr.FunctionalParser.FunctionTypeContext::type)
                .map(CapybaraParser::type);
    }

    private static Type type(dev.capylang.parser.antlr.FunctionalParser.TypeContext context) {
        if (context.getChildCount() > 0 && "tuple".equals(context.getChild(0).getText())) {
            return new TupleType(context.type().stream().map(CapybaraParser::type).toList());
        }
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
        if (context.FAT_ARROW() != null) {
            if ("(".equals(context.getChild(0).getText())) {
                var parts = context.type();
                if (parts.size() == 1) {
                    return new FunctionType(PrimitiveType.NOTHING, type(parts.getFirst()));
                }
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
            return Optional.of(new DataType(name));
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

    private Expression expression(dev.capylang.parser.antlr.FunctionalParser.ExpressionContext expression) {
        var result = expressionNoLet(expression.expressionNoLet());
        var lets = expression.letExpression();
        for (var i = lets.size() - 1; i >= 0; i--) {
            var letExpression = lets.get(i);
            result = new LetExpression(
                    letExpression.NAME().getText(),
                    Optional.ofNullable(letExpression.type()).map(CapybaraParser::type),
                    expressionNoLet(letExpression.expressionNoLet()),
                    result,
                    position(letExpression)
            );
        }
        return result;
    }

    private Expression expressionNoPipe(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoPipeContext expression) {
        var result = expressionNoLetNoPipe(expression.expressionNoLetNoPipe());
        var lets = expression.letExpressionNoPipe();
        for (var i = lets.size() - 1; i >= 0; i--) {
            var letExpression = lets.get(i);
            result = new LetExpression(
                    letExpression.NAME().getText(),
                    Optional.ofNullable(letExpression.type()).map(CapybaraParser::type),
                    expressionNoLet(letExpression.expressionNoLet()),
                    result,
                    position(letExpression)
            );
        }
        return result;
    }

    private Expression expressionNoLet(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext expression) {
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
            return normalizeReduceExpression(reduceExpression(expression.reduceExpression()));
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
        if (expression.tupleLiteral() != null) {
            return tupleExpression(expression.tupleLiteral());
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
                if (literal.BYTE_LITERAL() != null) {
                    return new ByteValue(literal.BYTE_LITERAL().getText(), position(literal.BYTE_LITERAL()));
                }
                if (literal.INT_LITERAL() != null) {
                    return new IntValue(literal.INT_LITERAL().getText(), position(literal.INT_LITERAL()));
                }
                if (literal.LONG_LITERAL() != null) {
                    return new LongValue(literal.LONG_LITERAL().getText(), position(literal.LONG_LITERAL()));
                }
                if (literal.DOUBLE_LITERAL() != null) {
                    return new DoubleValue(literal.DOUBLE_LITERAL().getText(), position(literal.DOUBLE_LITERAL()));
                }
                if (literal.STRING_LITERAL() != null) {
                    return stringLiteralExpression(literal.STRING_LITERAL());
                }
                if (literal.FLOAT_LITERAL() != null) {
                    return new FloatValue(literal.FLOAT_LITERAL().getText(), position(literal.FLOAT_LITERAL()));
                }
                if (literal.NOTHING_LITERAL() != null) {
                    return new NothingValue(position(literal.NOTHING_LITERAL()));
                }
            }

            if (value.identifier() != null) {
                return new Value(identifier(value.identifier()), position(value.identifier()));
            }
            if (value.qualifiedType() != null) {
                return new FunctionCall(Optional.empty(), value.qualifiedType().getText(), List.of(), position(value.qualifiedType()));
            }
        }

        var newData = expression.newData();
        if (newData != null) {
            var assignments = fieldAssignments(newData.fieldAssignmentList());
            return new NewData(
                    type(newData.type()),
                    assignments.assignments(),
                    assignments.positionalArguments(),
                    assignments.spreads(),
                    position(newData)
            );
        }

        var matchExpression = expression.matchExpression();
        if (matchExpression != null) {
            return matchExpression(matchExpression);
        }

        if (isIndex(expression)) {
            var sourceContext = expression.expressionNoLet(0);
            if (isUnaryBang(sourceContext)) {
                return negate(indexExpression(sourceContext.expressionNoLet(0), expression.indexLiteral(), position(expression)), position(expression));
            }
            return indexExpression(expression);
        }

        if (isSlice(expression)) {
            var sourceContext = expression.expressionNoLet(0);
            if (isUnaryBang(sourceContext)) {
                return negate(sliceExpression(sourceContext.expressionNoLet(0), expression.sliceIndexLiteral(), expression.COLON().getSymbol().getTokenIndex(), position(expression)), position(expression));
            }
            return sliceExpression(expression);
        }

        if (isFieldAccess(expression)) {
            var sourceContext = expression.expressionNoLet(0);
            if (isUnaryBang(sourceContext)) {
                return negate(new FieldAccess(
                        expressionNoLet(sourceContext.expressionNoLet(0)),
                        identifier(expression.identifier()),
                        position(expression)
                ), position(expression));
            }
            return new FieldAccess(
                    expressionNoLet(sourceContext),
                    identifier(expression.identifier()),
                    position(expression)
            );
        }

        if (isMethodCall(expression)) {
            var receiverContext = expression.expressionNoLet(0);
            if (isUnaryBang(receiverContext)) {
                return negate(methodCall(receiverContext.expressionNoLet(0), expression.methodIdentifier(), expression.argumentList(), position(expression)), position(expression));
            }
            var receiver = expressionNoLet(receiverContext);
            var methodName = methodIdentifier(expression.methodIdentifier());
            var args = expression.argumentList() == null
                    ? new java.util.ArrayList<Expression>()
                    : new java.util.ArrayList<>(expression.argumentList().expression().stream().map(this::expression).toList());
            args.add(0, receiver);
            return new FunctionCall(Optional.empty(), METHOD_INVOKE_PREFIX + methodName, List.copyOf(args), position(expression));
        }

        if (isFunctionInvoke(expression)) {
            var functionContext = expression.expressionNoLet(0);
            if (isUnaryBang(functionContext)) {
                return negate(functionInvoke(functionContext.expressionNoLet(0), expression.argumentList(), position(expression)), position(expression));
            }
            var function = expressionNoLet(functionContext);
            var args = expression.argumentList() == null
                    ? List.<Expression>of()
                    : expression.argumentList().expression().stream().map(this::expression).toList();
            return new FunctionInvoke(function, args, position(expression));
        }

        if (expression.INFIX_METHOD_LITERAL() != null && expression.expressionNoLet().size() == 2) {
            var text = expression.INFIX_METHOD_LITERAL().getText();
            var methodName = text.substring(1, text.length() - 1);
            return new FunctionCall(
                    Optional.empty(),
                    METHOD_INVOKE_PREFIX + methodName,
                    List.of(expressionNoLet(expression.expressionNoLet(0)), expressionNoLet(expression.expressionNoLet(1))),
                    position(expression)
            );
        }

        if (expression.BANG() != null && expression.infixOperator() == null && expression.expressionNoLet().size() == 1) {
            return negate(expressionNoLet(expression.expressionNoLet(0)), position(expression));
        }

        if (expression.BITWISE_NOT() != null && expression.infixOperator() == null && expression.expressionNoLet().size() == 1) {
            return new InfixExpression(
                    expressionNoLet(expression.expressionNoLet(0)),
                    InfixOperator.BITWISE_NOT,
                    new IntValue("0", position(expression)),
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
                    position(infixOperator)
            );
        }

        var subExpression = expression.expression();
        if (subExpression != null) {
            return expression(subExpression);
        }

        throw new IllegalStateException("Unknown expression: " + expression.getText());
    }

    private LambdaExpression lambdaExpression(FunctionalParser.LambdaExpressionContext context) {
        var argumentNames = context.lambdaArgument().stream()
                .map(this::lambdaArgument)
                .toList();
        return new LambdaExpression(
                argumentNames,
                expressionNoPipe(context.expressionNoPipe()),
                position(context)
        );
    }

    private ReduceExpression reduceExpression(FunctionalParser.ReduceExpressionContext context) {
        var names = context.lambdaArgument().stream()
                .map(this::lambdaArgument)
                .toList();
        if (names.size() < 2 || names.size() > 4) {
            throw new IllegalStateException("Reduce expression has to define two, three or four arguments");
        }
        var accumulatorName = names.size() == 4
                ? names.get(0) + "::" + names.get(1)
                : names.get(0);
        var keyName = names.size() == 3
                ? Optional.of(names.get(1))
                : names.size() == 4
                    ? Optional.of(names.get(2))
                    : Optional.<String>empty();
        var valueName = names.size() == 2
                ? names.get(1)
                : names.size() == 3
                    ? names.get(2)
                    : names.get(3);
        return new ReduceExpression(
                expressionNoLetNoPipe(context.expressionNoLetNoPipe()),
                accumulatorName,
                keyName,
                valueName,
                expressionNoPipe(context.expressionNoPipe()),
                position(context)
        );
    }

    private String lambdaArgument(FunctionalParser.LambdaArgumentContext context) {
        if (context.identifier() != null) {
            return identifier(context.identifier());
        }
        if (context.UNDERSCORE() != null) {
            return context.UNDERSCORE().getText();
        }
        throw new IllegalStateException("Unknown lambda argument: " + context.getText());
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
        if (expression.lambdaExpression() != null) {
            return lambdaExpression(expression.lambdaExpression());
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
        if (expression.tupleLiteral() != null) {
            return tupleExpression(expression.tupleLiteral());
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
                if (literal.BYTE_LITERAL() != null) {
                    return new ByteValue(literal.BYTE_LITERAL().getText(), position(literal.BYTE_LITERAL()));
                }
                if (literal.INT_LITERAL() != null) {
                    return new IntValue(literal.INT_LITERAL().getText(), position(literal.INT_LITERAL()));
                }
                if (literal.LONG_LITERAL() != null) {
                    return new LongValue(literal.LONG_LITERAL().getText(), position(literal.LONG_LITERAL()));
                }
                if (literal.DOUBLE_LITERAL() != null) {
                    return new DoubleValue(literal.DOUBLE_LITERAL().getText(), position(literal.DOUBLE_LITERAL()));
                }
                if (literal.STRING_LITERAL() != null) {
                    return stringLiteralExpression(literal.STRING_LITERAL());
                }
                if (literal.FLOAT_LITERAL() != null) {
                    return new FloatValue(literal.FLOAT_LITERAL().getText(), position(literal.FLOAT_LITERAL()));
                }
                if (literal.NOTHING_LITERAL() != null) {
                    return new NothingValue(position(literal.NOTHING_LITERAL()));
                }
            }

            if (value.identifier() != null) {
                return new Value(identifier(value.identifier()), position(value.identifier()));
            }
            if (value.qualifiedType() != null) {
                return new FunctionCall(Optional.empty(), value.qualifiedType().getText(), List.of(), position(value.qualifiedType()));
            }
        }

        var newData = expression.newData();
        if (newData != null) {
            var assignments = fieldAssignments(newData.fieldAssignmentList());
            return new NewData(
                    type(newData.type()),
                    assignments.assignments(),
                    assignments.positionalArguments(),
                    assignments.spreads(),
                    position(newData)
            );
        }

        var matchExpression = expression.matchExpression();
        if (matchExpression != null) {
            return matchExpression(matchExpression);
        }

        if (isIndex(expression)) {
            var sourceContext = expression.expressionNoLetNoPipe(0);
            if (isUnaryBang(sourceContext)) {
                return negate(indexExpression(sourceContext.expressionNoLetNoPipe(0), expression.indexNoPipeLiteral(), position(expression)), position(expression));
            }
            return indexExpression(expression);
        }

        if (isSlice(expression)) {
            var sourceContext = expression.expressionNoLetNoPipe(0);
            if (isUnaryBang(sourceContext)) {
                return negate(sliceExpression(sourceContext.expressionNoLetNoPipe(0), expression.sliceIndexNoPipeLiteral(), expression.COLON().getSymbol().getTokenIndex(), position(expression)), position(expression));
            }
            return sliceExpression(expression);
        }

        if (isFieldAccess(expression)) {
            var sourceContext = expression.expressionNoLetNoPipe(0);
            if (isUnaryBang(sourceContext)) {
                return negate(new FieldAccess(
                        expressionNoLetNoPipe(sourceContext.expressionNoLetNoPipe(0)),
                        identifier(expression.identifier()),
                        position(expression)
                ), position(expression));
            }
            return new FieldAccess(
                    expressionNoLetNoPipe(sourceContext),
                    identifier(expression.identifier()),
                    position(expression)
            );
        }

        if (isMethodCall(expression)) {
            var receiverContext = expression.expressionNoLetNoPipe(0);
            if (isUnaryBang(receiverContext)) {
                return negate(methodCall(receiverContext.expressionNoLetNoPipe(0), expression.methodIdentifier(), expression.argumentList(), position(expression)), position(expression));
            }
            var receiver = expressionNoLetNoPipe(receiverContext);
            var methodName = methodIdentifier(expression.methodIdentifier());
            var args = expression.argumentList() == null
                    ? new java.util.ArrayList<Expression>()
                    : new java.util.ArrayList<>(expression.argumentList().expression().stream().map(this::expression).toList());
            args.add(0, receiver);
            return new FunctionCall(Optional.empty(), METHOD_INVOKE_PREFIX + methodName, List.copyOf(args), position(expression));
        }

        if (isFunctionInvoke(expression)) {
            var functionContext = expression.expressionNoLetNoPipe(0);
            if (isUnaryBang(functionContext)) {
                return negate(functionInvoke(functionContext.expressionNoLetNoPipe(0), expression.argumentList(), position(expression)), position(expression));
            }
            var function = expressionNoLetNoPipe(functionContext);
            var args = expression.argumentList() == null
                    ? List.<Expression>of()
                    : expression.argumentList().expression().stream().map(this::expression).toList();
            return new FunctionInvoke(function, args, position(expression));
        }

        if (expression.INFIX_METHOD_LITERAL() != null && expression.expressionNoLetNoPipe().size() == 2) {
            var text = expression.INFIX_METHOD_LITERAL().getText();
            var methodName = text.substring(1, text.length() - 1);
            return new FunctionCall(
                    Optional.empty(),
                    METHOD_INVOKE_PREFIX + methodName,
                    List.of(
                            expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)),
                            expressionNoLetNoPipe(expression.expressionNoLetNoPipe(1))
                    ),
                    position(expression)
            );
        }

        if (expression.BANG() != null && expression.infixOperatorNoPipe() == null && expression.expressionNoLetNoPipe().size() == 1) {
            return negate(expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)), position(expression));
        }

        if (expression.BITWISE_NOT() != null && expression.infixOperatorNoPipe() == null && expression.expressionNoLetNoPipe().size() == 1) {
            return new InfixExpression(
                    expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)),
                    InfixOperator.BITWISE_NOT,
                    new IntValue("0", position(expression)),
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
                    position(infixOperator)
            );
        }

        var subExpression = expression.expression();
        if (subExpression != null) {
            return expression(subExpression);
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

    private static Expression normalizeReduceExpression(ReduceExpression reduceExpression) {
        if (reduceExpression.initialValue() instanceof InfixExpression infixExpression
            && infixExpression.operator() == InfixOperator.PIPE_REDUCE) {
            return new InfixExpression(
                    infixExpression.left(),
                    InfixOperator.PIPE_REDUCE,
                    new ReduceExpression(
                            infixExpression.right(),
                            reduceExpression.accumulatorName(),
                            reduceExpression.keyName(),
                            reduceExpression.valueName(),
                            reduceExpression.reducerExpression(),
                            reduceExpression.position()
                    ),
                    reduceExpression.position()
            );
        }
        return reduceExpression;
    }

    private MatchExpression matchExpression(FunctionalParser.MatchExpressionContext context) {
        return new MatchExpression(
                expression(context.expression()),
                context.matchCaseList()
                        .stream()
                        .map(FunctionalParser.MatchCaseListContext::matchCase)
                        .flatMap(Collection::stream)
                        .flatMap(matchCase -> matchCase.pattern()
                                .stream()
                                .map(pattern -> matchCase(pattern, matchCase.expressionNoPipe())))
                        .toList(),
                position(context)
        );
    }

    private MatchExpression.MatchCase matchCase(
            FunctionalParser.PatternContext pattern,
            FunctionalParser.ExpressionNoPipeContext expression
    ) {
        return new MatchExpression.MatchCase(
                matchExpressionPattern(pattern),
                expressionNoPipe(expression)

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
            return new MatchExpression.StringPattern(normalizeStringLiteral(stringLiteral.getText()));
        }

        var floatLiteral = context.FLOAT_LITERAL();
        if (floatLiteral != null) {
            return new MatchExpression.FloatPattern(floatLiteral.getText());
        }

        var typedPattern = context.typedPattern();
        if (typedPattern != null) {
            var patternName = typedPattern.NAME() != null
                    ? typedPattern.NAME().getText()
                    : "__ignored";
            return new MatchExpression.TypedPattern(
                    type(typedPattern.patternType().getText()),
                    patternName
            );
        }

        var wildcardPattern = context.wildcardPattern();
        if (wildcardPattern != null) {
            var wildcardName = wildcardPattern.NAME();
            if (wildcardName == null) {
                return MatchExpression.WildcardPattern.WILDCARD;
            }
            return new MatchExpression.WildcardBindingPattern(wildcardName.getText());
        }

        var identifier = context.identifier();
        if (identifier != null) {
            return new MatchExpression.VariablePattern(this.identifier(identifier));
        }

        var constructorPattern = context.constructorPattern();
        if (constructorPattern != null) {
            return new MatchExpression.ConstructorPattern(
                    constructorPattern.TYPE().getText(),
                    constructorPattern.fieldPatternList() == null
                            ? List.of()
                            : constructorPattern.fieldPatternList().pattern().stream()
                                    .map(this::matchExpressionPattern)
                                    .toList()
            );
        }

        throw new IllegalStateException("Unknown pattern: " + context.getText());
    }

    private Expression functionCall(dev.capylang.parser.antlr.FunctionalParser.FunctionCallContext context) {
        var arguments = context.argumentList() == null
                ? List.<Expression>of()
                : context.argumentList().expression().stream().map(this::expression).toList();
        var moduleName = context.TYPE() == null ? Optional.<String>empty() : Optional.of(context.TYPE().getText());
        var functionName = context.identifier() != null
                ? identifier(context.identifier())
                : context.NAME() != null
                        ? context.NAME().getText()
                        : context.COLLECTION() != null
                                ? context.COLLECTION().getText()
                                : context.getChild(0).getText();
        return new FunctionCall(moduleName, functionName, arguments, position(context));
    }

    private Expression newListExpression(dev.capylang.parser.antlr.FunctionalParser.New_listContext context) {
        return new NewListExpression(
                context.expression().stream().map(this::expression).toList(),
                position(context)
        );
    }

    private Expression newDictExpression(dev.capylang.parser.antlr.FunctionalParser.New_dictContext context) {
        return new NewDictExpression(
                context.dict_entry().stream()
                        .map(this::dictEntry)
                        .toList(),
                position(context)
        );
    }

    private NewDictExpression.Entry dictEntry(dev.capylang.parser.antlr.FunctionalParser.Dict_entryContext context) {
        return new NewDictExpression.Entry(
                expression(context.expression(0)),
                expression(context.expression(1))
        );
    }

    private Expression newSetExpression(dev.capylang.parser.antlr.FunctionalParser.New_setContext context) {
        return new NewSetExpression(
                context.expression().stream().map(this::expression).toList(),
                position(context)
        );
    }

    private Expression tupleExpression(dev.capylang.parser.antlr.FunctionalParser.TupleLiteralContext context) {
        var values = context.expression().stream().map(this::expression).toList();
        if (values.size() == 2
            && values.get(0) instanceof InfixExpression first
            && first.operator() == InfixOperator.PIPE_REDUCE
            && !(first.right() instanceof ReduceExpression)
            && values.get(1) instanceof LambdaExpression lambda
            && lambda.argumentNames().size() >= 2
            && lambda.argumentNames().size() <= 4) {
            var names = lambda.argumentNames();
            var accumulatorName = names.size() == 4
                    ? names.get(0) + "::" + names.get(1)
                    : names.get(0);
            var keyName = names.size() == 3
                    ? Optional.of(names.get(1))
                    : names.size() == 4
                        ? Optional.of(names.get(2))
                        : Optional.<String>empty();
            var valueName = names.size() == 2
                    ? names.get(1)
                    : names.size() == 3
                        ? names.get(2)
                        : names.get(3);
            var reduce = new ReduceExpression(
                    first.right(),
                    accumulatorName,
                    keyName,
                    valueName,
                    lambda.expression(),
                    lambda.position()
            );
            return new InfixExpression(
                    first.left(),
                    InfixOperator.PIPE_REDUCE,
                    reduce,
                    position(context)
            );
        }
        return new TupleExpression(values, position(context));
    }

    private NewData.FieldAssignment fieldAssignment(dev.capylang.parser.antlr.FunctionalParser.FieldAssignmentContext context) {
        if (context.spreadFieldAssignment() != null) {
            throw new IllegalStateException("Spread field assignment is not allowed in this context: " + context.getText());
        }
        var named = context.namedFieldAssignment();
        if (named == null) {
            throw new IllegalStateException("Named field assignment is not available in this context: " + context.getText());
        }
        return new NewData.FieldAssignment(fieldName(named.identifier(), named.STRING_LITERAL()), expression(named.expression()));
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
            return new NewDataFieldAssignments(List.of(), List.of(), List.of());
        }
        var assignments = new java.util.ArrayList<NewData.FieldAssignment>();
        var positionalArguments = new java.util.ArrayList<Expression>();
        var spreads = new java.util.ArrayList<Expression>();
        for (var assignment : context.fieldAssignment()) {
            if (assignment.spreadFieldAssignment() != null) {
                spreads.add(expression(assignment.spreadFieldAssignment().expression()));
            } else if (assignment.namedFieldAssignment() != null) {
                assignments.add(fieldAssignment(assignment));
            } else {
                positionalArguments.add(expression(assignment.positionalFieldAssignment().expression()));
            }
        }
        return new NewDataFieldAssignments(
                List.copyOf(assignments),
                List.copyOf(positionalArguments),
                List.copyOf(spreads)
        );
    }

    private static BooleanValue boolLiteral(TerminalNode node) {
        return switch (node.getText()) {
            case "true" -> new BooleanValue(true, position(node));
            case "false" -> new BooleanValue(false, position(node));
            default -> throw new IllegalStateException("Unexpected value: " + node.getText());
        };
    }

    private Parameter parameter(dev.capylang.parser.antlr.FunctionalParser.ParameterContext context) {
        return new Parameter(type(context.type()), identifier(context.identifier()), position(context));
    }

    private static Optional<SourcePosition> position(ParserRuleContext context) {
        return SourcePosition.of(context);
    }

    private static Optional<SourcePosition> position(TerminalNode node) {
        return Optional.of(SourcePosition.of(node));
    }

    private static String fieldName(FunctionalParser.IdentifierContext identifier, TerminalNode stringLiteral) {
        if (identifier != null) {
            return identifier(identifier);
        }
        return fieldName((TerminalNode) null, stringLiteral);
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

    private Expression stringLiteralExpression(TerminalNode stringLiteral) {
        var raw = stringLiteral.getText();
        var position = SourcePosition.of(stringLiteral);
        if (!isDoubleQuoted(raw) || !hasInterpolation(raw)) {
            return new StringValue(normalizeStringLiteral(raw), Optional.of(position));
        }
        return interpolatedStringExpression(raw, position);
    }

    private Expression interpolatedStringExpression(String raw, SourcePosition position) {
        var content = raw.substring(1, raw.length() - 1);
        var parts = new ArrayList<Expression>();
        var current = new StringBuilder();
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch != '{' || isEscaped(content, i)) {
                current.append(ch);
                continue;
            }
            if (current.length() > 0) {
                parts.add(new StringValue(quoteDoubleQuotedSegment(normalizeDoubleQuotedContent(current.toString())), Optional.of(position)));
                current.setLength(0);
            }
            var end = findInterpolationEnd(content, i + 1);
            if (end < 0) {
                throw interpolationError(position, i, "missing `}`");
            }
            var expressionText = content.substring(i + 1, end);
            if (expressionText.isBlank()) {
                throw interpolationError(position, i, "Cannot evaluate expression");
            }
            parts.add(parseInterpolationExpression(expressionText, position, i));
            i = end;
        }
        if (current.length() > 0) {
            parts.add(new StringValue(quoteDoubleQuotedSegment(normalizeDoubleQuotedContent(current.toString())), Optional.of(position)));
        }
        if (parts.isEmpty()) {
            return new StringValue("\"\"", Optional.of(position));
        }
        Expression result = parts.getFirst();
        if (!(result instanceof StringValue)) {
            result = new InfixExpression(
                    new StringValue("\"\"", Optional.of(position)),
                    InfixOperator.PLUS,
                    result,
                    Optional.of(position)
            );
        }
        for (var i = 1; i < parts.size(); i++) {
            result = new InfixExpression(result, InfixOperator.PLUS, parts.get(i), Optional.of(position));
        }
        return result;
    }

    private Expression parseInterpolationExpression(String expressionText, SourcePosition stringPosition, int interpolationOffset) {
        var lexer = new dev.capylang.parser.antlr.FunctionalLexer(CharStreams.fromString(expressionText));
        var tokens = new CommonTokenStream(lexer);
        var parser = new dev.capylang.parser.antlr.FunctionalParser(tokens);
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

        var parsed = parser.expressionNoLet();
        if (!syntaxErrors.isEmpty()) {
            var first = syntaxErrors.getFirst();
            var details = formatSyntaxError(first).replaceFirst("^line \\d+:\\d+:\\s*", "");
            throw interpolationError(stringPosition, interpolationOffset + first.column(), details);
        }
        if (parser.getCurrentToken().getType() != Token.EOF) {
            throw interpolationError(stringPosition, interpolationOffset, "Cannot evaluate expression");
        }
        return shiftInterpolationPositions(expressionNoLet(parsed), stringPosition, interpolationOffset);
    }

    private Expression shiftInterpolationPositions(Expression expression, SourcePosition stringPosition, int interpolationOffset) {
        return switch (expression) {
            case BooleanValue value -> new BooleanValue(value.value(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case ByteValue value -> new ByteValue(value.byteValue(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case DoubleValue value -> new DoubleValue(value.doubleValue(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case FieldAccess value -> new FieldAccess(
                    shiftInterpolationPositions(value.source(), stringPosition, interpolationOffset),
                    value.field(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case FloatValue value -> new FloatValue(value.floatValue(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case FunctionCall value -> new FunctionCall(
                    value.moduleName(),
                    value.name(),
                    value.arguments().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case FunctionInvoke value -> new FunctionInvoke(
                    shiftInterpolationPositions(value.function(), stringPosition, interpolationOffset),
                    value.arguments().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case FunctionReference value -> new FunctionReference(value.name(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case IfExpression value -> new IfExpression(
                    shiftInterpolationPositions(value.condition(), stringPosition, interpolationOffset),
                    shiftInterpolationPositions(value.thenBranch(), stringPosition, interpolationOffset),
                    shiftInterpolationPositions(value.elseBranch(), stringPosition, interpolationOffset),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case IndexExpression value -> new IndexExpression(
                    shiftInterpolationPositions(value.source(), stringPosition, interpolationOffset),
                    shiftInterpolationPositions(value.index(), stringPosition, interpolationOffset),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case InfixExpression value -> new InfixExpression(
                    shiftInterpolationPositions(value.left(), stringPosition, interpolationOffset),
                    value.operator(),
                    shiftInterpolationPositions(value.right(), stringPosition, interpolationOffset),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case IntValue value -> new IntValue(value.intValue(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case LambdaExpression value -> new LambdaExpression(
                    value.argumentNames(),
                    shiftInterpolationPositions(value.expression(), stringPosition, interpolationOffset),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case LetExpression value -> new LetExpression(
                    value.name(),
                    value.declaredType(),
                    shiftInterpolationPositions(value.value(), stringPosition, interpolationOffset),
                    shiftInterpolationPositions(value.rest(), stringPosition, interpolationOffset),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case LongValue value -> new LongValue(value.longValue(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case MatchExpression value -> new MatchExpression(
                    shiftInterpolationPositions(value.matchWith(), stringPosition, interpolationOffset),
                    value.cases().stream().map(matchCase -> new MatchExpression.MatchCase(
                            matchCase.pattern(),
                            shiftInterpolationPositions(matchCase.expression(), stringPosition, interpolationOffset)
                    )).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case NewData value -> new NewData(
                    value.type(),
                    value.assignments().stream().map(assignment -> new NewData.FieldAssignment(
                            assignment.name(),
                            shiftInterpolationPositions(assignment.value(), stringPosition, interpolationOffset)
                    )).toList(),
                    value.positionalArguments().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    value.spreads().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case NewDictExpression value -> new NewDictExpression(
                    value.entries().stream().map(entry -> new NewDictExpression.Entry(
                            shiftInterpolationPositions(entry.key(), stringPosition, interpolationOffset),
                            shiftInterpolationPositions(entry.value(), stringPosition, interpolationOffset)
                    )).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case NewListExpression value -> new NewListExpression(
                    value.values().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case NewSetExpression value -> new NewSetExpression(
                    value.values().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case NothingValue value -> new NothingValue(shiftPosition(value.position(), stringPosition, interpolationOffset));
            case ReduceExpression value -> new ReduceExpression(
                    shiftInterpolationPositions(value.initialValue(), stringPosition, interpolationOffset),
                    value.accumulatorName(),
                    value.keyName(),
                    value.valueName(),
                    shiftInterpolationPositions(value.reducerExpression(), stringPosition, interpolationOffset),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case SliceExpression value -> new SliceExpression(
                    shiftInterpolationPositions(value.source(), stringPosition, interpolationOffset),
                    value.start().map(start -> shiftInterpolationPositions(start, stringPosition, interpolationOffset)),
                    value.end().map(end -> shiftInterpolationPositions(end, stringPosition, interpolationOffset)),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case StringValue value -> new StringValue(value.stringValue(), shiftPosition(value.position(), stringPosition, interpolationOffset));
            case TupleExpression value -> new TupleExpression(
                    value.values().stream().map(argument -> shiftInterpolationPositions(argument, stringPosition, interpolationOffset)).toList(),
                    shiftPosition(value.position(), stringPosition, interpolationOffset)
            );
            case Value value -> new Value(value.name(), shiftPosition(value.position(), stringPosition, interpolationOffset));
        };
    }

    private Optional<SourcePosition> shiftPosition(Optional<SourcePosition> position, SourcePosition stringPosition, int interpolationOffset) {
        return position.map(sourcePosition -> new SourcePosition(
                stringPosition.line(),
                stringPosition.column() + interpolationOffset + 1 + sourcePosition.column(),
                sourcePosition.length()
        ));
    }
    private static boolean isDoubleQuoted(String raw) {
        return raw.length() >= 2 && raw.charAt(0) == '"' && raw.charAt(raw.length() - 1) == '"';
    }

    private static boolean hasInterpolation(String raw) {
        var content = raw.substring(1, raw.length() - 1);
        for (var i = 0; i < content.length(); i++) {
            if (isInterpolationStart(content, i)) {
                return true;
            }
        }
        return false;
    }
    private static boolean isInterpolationStart(String content, int index) {
        if (index < 0 || index >= content.length() || content.charAt(index) != '{' || isEscaped(content, index)) {
            return false;
        }
        if (index + 1 >= content.length()) {
            return false;
        }
        var next = content.charAt(index + 1);
        if (next == '}' && !isEscaped(content, index + 1)) {
            return true;
        }
        return Character.isLetterOrDigit(next)
               || next == '_'
               || next == '('
               || next == '['
               || next == '{'
               || next == '"'
               || next == '\''
               || next == '!'
               || next == '-'
               || next == '/';
    }
    private static int findInterpolationEnd(String content, int startInclusive) {
        for (var i = startInclusive; i < content.length(); i++) {
            if (content.charAt(i) == '}' && !isEscaped(content, i)) {
                return i;
            }
        }
        return -1;
    }

    private static boolean isEscaped(String content, int index) {
        var backslashes = 0;
        for (var i = index - 1; i >= 0 && content.charAt(i) == '\\'; i--) {
            backslashes++;
        }
        return backslashes % 2 == 1;
    }

    private static String quoteDoubleQuotedSegment(String content) {
        return "\"" + content
                .replace("\\", "\\\\")
                .replace("\"", "\\\"") + "\"";
    }

    private static String normalizeDoubleQuotedContent(String content) {
        var normalized = new StringBuilder(content.length());
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch == '\\' && i + 1 < content.length()) {
                var next = content.charAt(i + 1);
                if (next == '"' || next == '\\') {
                    normalized.append(next);
                    i++;
                    continue;
                }
                if (next == '{') {
                    normalized.append('\\').append('{');
                    i++;
                    continue;
                }
            }
            normalized.append(ch);
        }
        return normalized.toString();
    }

    private static IllegalStateException interpolationError(SourcePosition position, int interpolationOffset, String details) {
        return new IllegalStateException(
                "line %d:%d: %s".formatted(position.line(), position.column() + interpolationOffset + 1, details)
        );
    }

    private static String normalizeStringLiteral(String raw) {
        if (raw.length() < 2) {
            return raw;
        }
        if (raw.charAt(0) == '"' && raw.charAt(raw.length() - 1) == '"') {
            var content = normalizeDoubleQuotedContent(raw.substring(1, raw.length() - 1));
            return "\"" + content
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"") + "\"";
        }
        if (raw.charAt(0) == '\'' && raw.charAt(raw.length() - 1) == '\'') {
            var content = raw.substring(1, raw.length() - 1)
                    .replace("\"", "\\\"");
            return "\"" + content + "\"";
        }
        return raw;
    }

    private static String genericTypeName(FunctionalParser.GenericTypeDeclarationContext context) {
        return context.TYPE(0).getText();
    }

    private static String identifier(FunctionalParser.IdentifierContext context) {
        return context.getText();
    }

    private static String functionName(FunctionalParser.FunctionNameDeclarationContext context) {
        if (context.identifier() != null) {
            return identifier(context.identifier());
        }
        var methodIdentifier = context.methodIdentifier();
        if (methodIdentifier == null) {
            throw new IllegalStateException("Missing function name");
        }
        if (methodIdentifier.identifier() != null) {
            return identifier(methodIdentifier.identifier());
        }
        var infixLiteral = methodIdentifier.INFIX_METHOD_LITERAL();
        if (infixLiteral != null) {
            var text = infixLiteral.getText();
            return text.substring(1, text.length() - 1);
        }
        throw new IllegalStateException("Unknown function name declaration: " + context.getText());
    }

    private static String stripDocComment(String text) {
        if (!text.startsWith("///")) {
            return text;
        }
        var raw = text.substring(3);
        return raw.startsWith(" ") ? raw.substring(1) : raw;
    }

    private static void validateConstName(String name) {
        if (!CONST_NAME_PATTERN.matcher(name).matches()) {
            throw new IllegalStateException("Invalid const name: " + name);
        }
    }

    private static void validateEnumValueName(String name) {
        if (!ENUM_VALUE_NAME_PATTERN.matcher(name).matches()) {
            throw new IllegalStateException("Invalid enum value name: " + name);
        }
    }

    private static Optional<Type> inferConstType(Expression expression) {
        return switch (expression) {
            case BooleanValue ignored -> Optional.of(PrimitiveType.BOOL);
            case ByteValue ignored -> Optional.of(PrimitiveType.BYTE);
            case IntValue ignored -> Optional.of(PrimitiveType.INT);
            case LongValue ignored -> Optional.of(PrimitiveType.LONG);
            case DoubleValue ignored -> Optional.of(PrimitiveType.DOUBLE);
            case FloatValue ignored -> Optional.of(PrimitiveType.FLOAT);
            case StringValue ignored -> Optional.of(PrimitiveType.STRING);
            case NothingValue ignored -> Optional.of(PrimitiveType.NOTHING);
            default -> Optional.empty();
        };
    }

    private static String methodIdentifier(FunctionalParser.MethodIdentifierContext context) {
        if (context.identifier() != null) {
            return identifier(context.identifier());
        }
        var infixLiteral = context.INFIX_METHOD_LITERAL();
        if (infixLiteral != null) {
            var text = infixLiteral.getText();
            return text.substring(1, text.length() - 1);
        }
        throw new IllegalStateException("Unknown method identifier: " + context.getText());
    }

    private static List<String> genericTypeParameters(FunctionalParser.GenericTypeDeclarationContext context) {
        return context.TYPE().stream().skip(1).map(TerminalNode::getText).toList();
    }

    private static boolean isFieldAccess(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.getChildCount() == 3 && ".".equals(expression.getChild(1).getText()) && expression.identifier() != null;
    }

    private static boolean isFieldAccess(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.getChildCount() == 3 && ".".equals(expression.getChild(1).getText()) && expression.identifier() != null;
    }

    private static boolean isUnaryBang(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.BANG() != null
               && expression.infixOperator() == null
               && expression.expressionNoLet().size() == 1;
    }

    private static boolean isUnaryBang(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.BANG() != null
               && expression.infixOperatorNoPipe() == null
               && expression.expressionNoLetNoPipe().size() == 1;
    }

    private static boolean isMethodCall(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.DOT() != null
               && expression.methodIdentifier() != null
               && expression.LPAREN() != null
               && expression.RPAREN() != null
               && expression.expressionNoLet().size() == 1;
    }

    private static boolean isMethodCall(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.DOT() != null
               && expression.methodIdentifier() != null
               && expression.LPAREN() != null
               && expression.RPAREN() != null
               && expression.expressionNoLetNoPipe().size() == 1;
    }

    private static boolean isFunctionInvoke(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.LPAREN() != null
               && expression.RPAREN() != null
               && expression.DOT() == null
               && expression.functionCall() == null
               && expression.expressionNoLet().size() == 1;
    }

    private static boolean isFunctionInvoke(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.LPAREN() != null
               && expression.RPAREN() != null
               && expression.DOT() == null
               && expression.functionCall() == null
               && expression.expressionNoLetNoPipe().size() == 1;
    }

    private SliceExpression sliceExpression(FunctionalParser.ExpressionNoLetContext expression) {
        var source = expressionNoLet(expression.expressionNoLet(0));
        var bounds = parseSliceBounds(
                expression.sliceIndexLiteral().stream().map(this::sliceIndexExpression).toList(),
                expression.sliceIndexLiteral().stream().map(index -> index.start.getTokenIndex()).toList(),
                expression.COLON().getSymbol().getTokenIndex()
        );
        return new SliceExpression(source, bounds.start(), bounds.end(), position(expression));
    }

    private IndexExpression indexExpression(FunctionalParser.ExpressionNoLetContext expression) {
        var source = expressionNoLet(expression.expressionNoLet(0));
        var index = indexExpression(expression.indexLiteral());
        return new IndexExpression(source, index, position(expression));
    }

    private IndexExpression indexExpression(
            FunctionalParser.ExpressionNoLetContext sourceContext,
            FunctionalParser.IndexLiteralContext indexContext,
            Optional<SourcePosition> position
    ) {
        return new IndexExpression(expressionNoLet(sourceContext), indexExpression(indexContext), position);
    }

    private IndexExpression indexExpression(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        var source = expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0));
        var index = indexExpression(expression.indexNoPipeLiteral());
        return new IndexExpression(source, index, position(expression));
    }

    private IndexExpression indexExpression(
            FunctionalParser.ExpressionNoLetNoPipeContext sourceContext,
            FunctionalParser.IndexNoPipeLiteralContext indexContext,
            Optional<SourcePosition> position
    ) {
        return new IndexExpression(expressionNoLetNoPipe(sourceContext), indexExpression(indexContext), position);
    }

    private Expression indexExpression(FunctionalParser.IndexLiteralContext index) {
        var sign = index.MINUS() == null ? "" : "-";
        return new IntValue(sign + index.INT_LITERAL().getText(), position(index));
    }

    private Expression indexExpression(FunctionalParser.IndexNoPipeLiteralContext index) {
        var sign = index.MINUS() == null ? "" : "-";
        return new IntValue(sign + index.INT_LITERAL().getText(), position(index));
    }

    private SliceExpression sliceExpression(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        var source = expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0));
        var bounds = parseSliceBounds(
                expression.sliceIndexNoPipeLiteral().stream().map(this::sliceIndexExpression).toList(),
                expression.sliceIndexNoPipeLiteral().stream().map(index -> index.start.getTokenIndex()).toList(),
                expression.COLON().getSymbol().getTokenIndex()
        );
        return new SliceExpression(source, bounds.start(), bounds.end(), position(expression));
    }

    private SliceExpression sliceExpression(
            FunctionalParser.ExpressionNoLetContext sourceContext,
            List<FunctionalParser.SliceIndexLiteralContext> indexContexts,
            int colonTokenIndex,
            Optional<SourcePosition> position
    ) {
        var bounds = parseSliceBounds(
                indexContexts.stream().map(this::sliceIndexExpression).toList(),
                indexContexts.stream().map(index -> index.start.getTokenIndex()).toList(),
                colonTokenIndex
        );
        return new SliceExpression(expressionNoLet(sourceContext), bounds.start(), bounds.end(), position);
    }

    private SliceExpression sliceExpression(
            FunctionalParser.ExpressionNoLetNoPipeContext sourceContext,
            List<FunctionalParser.SliceIndexNoPipeLiteralContext> indexContexts,
            int colonTokenIndex,
            Optional<SourcePosition> position
    ) {
        var bounds = parseSliceBounds(
                indexContexts.stream().map(this::sliceIndexExpression).toList(),
                indexContexts.stream().map(index -> index.start.getTokenIndex()).toList(),
                colonTokenIndex
        );
        return new SliceExpression(expressionNoLetNoPipe(sourceContext), bounds.start(), bounds.end(), position);
    }

    private FunctionCall methodCall(
            FunctionalParser.ExpressionNoLetContext receiverContext,
            FunctionalParser.MethodIdentifierContext methodIdentifierContext,
            FunctionalParser.ArgumentListContext argumentListContext,
            Optional<SourcePosition> position
    ) {
        var receiver = expressionNoLet(receiverContext);
        var methodName = methodIdentifier(methodIdentifierContext);
        var args = argumentListContext == null
                ? new java.util.ArrayList<Expression>()
                : new java.util.ArrayList<>(argumentListContext.expression().stream().map(this::expression).toList());
        args.add(0, receiver);
        return new FunctionCall(Optional.empty(), METHOD_INVOKE_PREFIX + methodName, List.copyOf(args), position);
    }

    private FunctionCall methodCall(
            FunctionalParser.ExpressionNoLetNoPipeContext receiverContext,
            FunctionalParser.MethodIdentifierContext methodIdentifierContext,
            FunctionalParser.ArgumentListContext argumentListContext,
            Optional<SourcePosition> position
    ) {
        var receiver = expressionNoLetNoPipe(receiverContext);
        var methodName = methodIdentifier(methodIdentifierContext);
        var args = argumentListContext == null
                ? new java.util.ArrayList<Expression>()
                : new java.util.ArrayList<>(argumentListContext.expression().stream().map(this::expression).toList());
        args.add(0, receiver);
        return new FunctionCall(Optional.empty(), METHOD_INVOKE_PREFIX + methodName, List.copyOf(args), position);
    }

    private FunctionInvoke functionInvoke(
            FunctionalParser.ExpressionNoLetContext functionContext,
            FunctionalParser.ArgumentListContext argumentListContext,
            Optional<SourcePosition> position
    ) {
        var function = expressionNoLet(functionContext);
        var args = argumentListContext == null
                ? List.<Expression>of()
                : argumentListContext.expression().stream().map(this::expression).toList();
        return new FunctionInvoke(function, args, position);
    }

    private FunctionInvoke functionInvoke(
            FunctionalParser.ExpressionNoLetNoPipeContext functionContext,
            FunctionalParser.ArgumentListContext argumentListContext,
            Optional<SourcePosition> position
    ) {
        var function = expressionNoLetNoPipe(functionContext);
        var args = argumentListContext == null
                ? List.<Expression>of()
                : argumentListContext.expression().stream().map(this::expression).toList();
        return new FunctionInvoke(function, args, position);
    }

    private static InfixExpression negate(Expression expression, Optional<SourcePosition> position) {
        return new InfixExpression(expression, InfixOperator.EQUAL, new BooleanValue(false, position), position);
    }

    private Expression sliceIndexExpression(FunctionalParser.SliceIndexLiteralContext index) {
        var sign = index.MINUS() == null ? "" : "-";
        return new IntValue(sign + index.INT_LITERAL().getText(), position(index));
    }

    private Expression sliceIndexExpression(FunctionalParser.SliceIndexNoPipeLiteralContext index) {
        var sign = index.MINUS() == null ? "" : "-";
        return new IntValue(sign + index.INT_LITERAL().getText(), position(index));
    }

    private static SliceBounds parseSliceBounds(
            List<Expression> indexes,
            List<Integer> tokenIndexes,
            int colonTokenIndex
    ) {
        if (indexes.isEmpty()) {
            return new SliceBounds(Optional.empty(), Optional.empty());
        }
        if (indexes.size() == 1) {
            return tokenIndexes.getFirst() < colonTokenIndex
                    ? new SliceBounds(Optional.of(indexes.getFirst()), Optional.empty())
                    : new SliceBounds(Optional.empty(), Optional.of(indexes.getFirst()));
        }
        return new SliceBounds(Optional.of(indexes.get(0)), Optional.of(indexes.get(1)));
    }

    private static boolean isSlice(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.LBRACK() != null
               && expression.RBRACK() != null
               && expression.COLON() != null
               && expression.expressionNoLet().size() == 1
               && expression.sliceIndexLiteral().size() <= 2;
    }

    private static boolean isIndex(FunctionalParser.ExpressionNoLetContext expression) {
        return expression.LBRACK() != null
               && expression.RBRACK() != null
               && expression.COLON() == null
               && expression.expressionNoLet().size() == 1
               && expression.indexLiteral() != null;
    }

    private static boolean isSlice(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.LBRACK() != null
               && expression.RBRACK() != null
               && expression.COLON() != null
               && expression.expressionNoLetNoPipe().size() == 1
               && expression.sliceIndexNoPipeLiteral().size() <= 2;
    }

    private static boolean isIndex(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.LBRACK() != null
               && expression.RBRACK() != null
               && expression.COLON() == null
               && expression.expressionNoLetNoPipe().size() == 1
               && expression.indexNoPipeLiteral() != null;
    }

    private record DataFieldDeclarations(List<DataDeclaration.DataField> fields, List<String> extendsTypes) {
    }

    private record NewDataFieldAssignments(
            List<NewData.FieldAssignment> assignments,
            List<Expression> positionalArguments,
            List<Expression> spreads
    ) {
    }

    private record SliceBounds(Optional<Expression> start, Optional<Expression> end) {
    }

}






















