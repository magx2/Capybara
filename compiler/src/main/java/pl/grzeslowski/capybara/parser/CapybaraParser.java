package pl.grzeslowski.capybara.parser;


import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.TerminalNode;
import pl.grzeslowski.capybara.parser.antlr.FunctionalParser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.toSet;

public class CapybaraParser {
    public static final CapybaraParser INSTANCE = new CapybaraParser();
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String METHOD_INVOKE_PREFIX = "__invoke__";
    private static final Pattern COLLECTION_LIST_PATTERN = Pattern.compile("list\\[(.+?)]");
    private static final Pattern COLLECTION_SET_PATTERN = Pattern.compile("set\\[(.+?)]");
    private static final Pattern COLLECTION_DICT_PATTERN = Pattern.compile("dict\\[(.+?)]");

    public Functional parseFunctional(String input) {
        var malformedMatchArrow = malformedMatchCaseArrow(input);
        if (malformedMatchArrow.isPresent()) {
            throw new IllegalStateException(formatSyntaxError(malformedMatchArrow.get()));
        }
        var lexer = new pl.grzeslowski.capybara.parser.antlr.FunctionalLexer(CharStreams.fromString(input));
        var tokens = new CommonTokenStream(lexer);
        tokens.fill();
        var parser = new pl.grzeslowski.capybara.parser.antlr.FunctionalParser(tokens);
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
        return new Functional(definitions);
    }

    private String formatSyntaxError(SyntaxError syntaxError) {
        if (syntaxError.message().contains("mismatched input '=' expecting '=>'")
            || syntaxError.message().equals("Expected `=>`, found `=`")) {
            return "line %d:%d: Expected `=>`, found `=`".formatted(syntaxError.line(), syntaxError.column());
        }
        return "line %d:%d: %s".formatted(syntaxError.line(), syntaxError.column(), syntaxError.message());
    }

    private Optional<SyntaxError> malformedMatchCaseArrow(String input) {
        var lines = input.split("\\R", -1);
        for (var i = 0; i < lines.length; i++) {
            var line = lines[i];
            var trimmed = line.stripLeading();
            if (!trimmed.startsWith("|")) {
                continue;
            }
            var leadingSpaces = line.length() - trimmed.length();
            var pipe = line.indexOf('|', leadingSpaces);
            if (pipe < 0) {
                continue;
            }
            var arrow = line.indexOf("=>", pipe);
            var equal = line.indexOf('=', pipe);
            if (equal >= 0 && (arrow < 0 || equal < arrow)) {
                return Optional.of(new SyntaxError(i + 1, equal, "Expected `=>`, found `=`"));
            }
        }
        return Optional.empty();
    }

    private record SyntaxError(int line, int column, String message) {
    }

    private List<Definition> definition(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.DefinitionContext context) {
        var functionDeclaration = context.functionDeclaration();
        if (functionDeclaration != null) {
            return functionDeclaration(functionDeclaration);
        }

        var dataDeclaration = context.dataDeclaration();
        if (dataDeclaration != null) {
            return List.of(dataDeclaration(dataDeclaration));
        }

        var singleDeclaration = context.singleDeclaration();
        if (singleDeclaration != null) {
            return List.of(singleDeclaration(singleDeclaration));
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

    private List<Definition> functionDeclaration(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionDeclarationContext functionDeclarationContext) {
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
        var localFunctionIndex = 0;
        var localTypeIndex = 0;
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
        }

        var extractedLocalDefinitions = new java.util.ArrayList<Definition>();
        for (var localDefinition : localDefinitions) {
            if (localDefinition.localFunctionDeclaration() != null) {
                extractedLocalDefinitions.add(
                        localFunctionDeclaration(
                                localDefinition.localFunctionDeclaration(),
                                localFunctionNameMap,
                                localTypeNameMap
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
                localTypeNameMap
        );
        var topLevelFunction = new Function(
                methodName,
                parameters,
                rewrittenReturnType,
                expression,
                functionDeclarationContext.docComment().stream()
                        .map(comment -> stripDocComment(comment.getText()))
                        .toList(),
                position(functionDeclarationContext)
        );
        var allDefinitions = new java.util.ArrayList<Definition>(1 + extractedLocalDefinitions.size());
        allDefinitions.add(topLevelFunction);
        allDefinitions.addAll(extractedLocalDefinitions);
        return List.copyOf(allDefinitions);
    }

    private Function localFunctionDeclaration(
            pl.grzeslowski.capybara.parser.antlr.FunctionalParser.LocalFunctionDeclarationContext context,
            java.util.Map<String, String> localFunctionNameMap,
            java.util.Map<String, String> localTypeNameMap
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
        var expression = rewriteLocalNames(expressionNoLet(context.expressionNoLet()), localFunctionNameMap, localTypeNameMap);
        var rewrittenReturnType = functionType(context.functionType())
                .map(type -> rewriteLocalTypeNames(type, localTypeNameMap));
        return new Function(
                mappedName,
                parameters,
                rewrittenReturnType,
                expression,
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
                position(context)
        );
    }

    private Expression rewriteLocalNames(
            Expression expression,
            java.util.Map<String, String> localFunctionNameMap,
            java.util.Map<String, String> localTypeNameMap
    ) {
        return switch (expression) {
            case FunctionCall functionCall -> {
                if (functionCall.moduleName().isEmpty() && localFunctionNameMap.containsKey(functionCall.name())) {
                    yield new FunctionCall(
                            Optional.empty(),
                            localFunctionNameMap.get(functionCall.name()),
                            functionCall.arguments().stream()
                                    .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap))
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
                                .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap))
                                .toList(),
                        functionCall.position()
                );
            }
            case FunctionReference functionReference -> {
                var mapped = localFunctionNameMap.get(functionReference.name());
                if (mapped != null) {
                    yield new FunctionReference(mapped, functionReference.position());
                }
                yield functionReference;
            }
            case LetExpression letExpression -> new LetExpression(
                    letExpression.name(),
                    letExpression.declaredType().map(type -> rewriteLocalTypeNames(type, localTypeNameMap)),
                    rewriteLocalNames(letExpression.value(), localFunctionNameMap, localTypeNameMap),
                    rewriteLocalNames(letExpression.rest(), localFunctionNameMap, localTypeNameMap),
                    letExpression.position()
            );
            case IfExpression ifExpression -> new IfExpression(
                    rewriteLocalNames(ifExpression.condition(), localFunctionNameMap, localTypeNameMap),
                    rewriteLocalNames(ifExpression.thenBranch(), localFunctionNameMap, localTypeNameMap),
                    rewriteLocalNames(ifExpression.elseBranch(), localFunctionNameMap, localTypeNameMap),
                    ifExpression.position()
            );
            case InfixExpression infixExpression -> new InfixExpression(
                    rewriteLocalNames(infixExpression.left(), localFunctionNameMap, localTypeNameMap),
                    infixExpression.operator(),
                    rewriteLocalNames(infixExpression.right(), localFunctionNameMap, localTypeNameMap),
                    infixExpression.position()
            );
            case FieldAccess fieldAccess -> new FieldAccess(
                    rewriteLocalNames(fieldAccess.source(), localFunctionNameMap, localTypeNameMap),
                    fieldAccess.field(),
                    fieldAccess.position()
            );
            case LambdaExpression lambdaExpression -> new LambdaExpression(
                    lambdaExpression.argumentNames(),
                    rewriteLocalNames(lambdaExpression.expression(), localFunctionNameMap, localTypeNameMap),
                    lambdaExpression.position()
            );
            case ReduceExpression reduceExpression -> new ReduceExpression(
                    rewriteLocalNames(reduceExpression.initialValue(), localFunctionNameMap, localTypeNameMap),
                    reduceExpression.accumulatorName(),
                    reduceExpression.keyName(),
                    reduceExpression.valueName(),
                    rewriteLocalNames(reduceExpression.reducerExpression(), localFunctionNameMap, localTypeNameMap),
                    reduceExpression.position()
            );
            case IndexExpression indexExpression -> new IndexExpression(
                    rewriteLocalNames(indexExpression.source(), localFunctionNameMap, localTypeNameMap),
                    rewriteLocalNames(indexExpression.index(), localFunctionNameMap, localTypeNameMap),
                    indexExpression.position()
            );
            case SliceExpression sliceExpression -> new SliceExpression(
                    rewriteLocalNames(sliceExpression.source(), localFunctionNameMap, localTypeNameMap),
                    sliceExpression.start().map(start -> rewriteLocalNames(start, localFunctionNameMap, localTypeNameMap)),
                    sliceExpression.end().map(end -> rewriteLocalNames(end, localFunctionNameMap, localTypeNameMap)),
                    sliceExpression.position()
            );
            case MatchExpression matchExpression -> new MatchExpression(
                    rewriteLocalNames(matchExpression.matchWith(), localFunctionNameMap, localTypeNameMap),
                    matchExpression.cases().stream()
                            .map(matchCase -> new MatchExpression.MatchCase(
                                    rewriteLocalTypeNames(matchCase.pattern(), localTypeNameMap),
                                    rewriteLocalNames(matchCase.expression(), localFunctionNameMap, localTypeNameMap)
                            ))
                            .toList(),
                    matchExpression.position()
            );
            case NewData newData -> new NewData(
                    rewriteLocalTypeNames(newData.type(), localTypeNameMap),
                    newData.assignments().stream()
                            .map(assignment -> new NewData.FieldAssignment(
                                    assignment.name(),
                                    rewriteLocalNames(assignment.value(), localFunctionNameMap, localTypeNameMap)
                            ))
                            .toList(),
                    newData.positionalArguments().stream()
                            .map(argument -> rewriteLocalNames(argument, localFunctionNameMap, localTypeNameMap))
                            .toList(),
                    newData.spreads().stream()
                            .map(spread -> rewriteLocalNames(spread, localFunctionNameMap, localTypeNameMap))
                            .toList(),
                    newData.position()
            );
            case NewListExpression newListExpression -> new NewListExpression(
                    newListExpression.values().stream()
                            .map(value -> rewriteLocalNames(value, localFunctionNameMap, localTypeNameMap))
                            .toList(),
                    newListExpression.position()
            );
            case NewSetExpression newSetExpression -> new NewSetExpression(
                    newSetExpression.values().stream()
                            .map(value -> rewriteLocalNames(value, localFunctionNameMap, localTypeNameMap))
                            .toList(),
                    newSetExpression.position()
            );
            case NewDictExpression newDictExpression -> new NewDictExpression(
                    newDictExpression.entries().stream()
                            .map(entry -> new NewDictExpression.Entry(
                                    rewriteLocalNames(entry.key(), localFunctionNameMap, localTypeNameMap),
                                    rewriteLocalNames(entry.value(), localFunctionNameMap, localTypeNameMap)
                            ))
                            .toList(),
                    newDictExpression.position()
            );
            case TupleExpression tupleExpression -> new TupleExpression(
                    tupleExpression.values().stream()
                            .map(value -> rewriteLocalNames(value, localFunctionNameMap, localTypeNameMap))
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
        var suffix = typeName.substring(genericStart);
        var mappedBaseName = localTypeNameMap.get(baseName);
        return mappedBaseName == null ? typeName : mappedBaseName + suffix;
    }

    private Optional<Type> functionType(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext context) {
        return Optional.ofNullable(context)
                .map(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionTypeContext::type)
                .map(CapybaraParser::type);
    }

    private static Type type(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.TypeContext context) {
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

    private Expression expression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.ExpressionContext expression) {
        var result = expressionNoLet(expression.expressionNoLet());
        var letExpressions = expression.letExpression();
        for (var i = letExpressions.size() - 1; i >= 0; i--) {
            var letExpression = letExpressions.get(i);
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
                    return new StringValue(normalizeStringLiteral(literal.STRING_LITERAL().getText()), position(literal.STRING_LITERAL()));
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
            return indexExpression(expression);
        }

        if (isSlice(expression)) {
            return sliceExpression(expression);
        }

        if (isFieldAccess(expression)) {
            return new FieldAccess(
                    expressionNoLet(expression.expressionNoLet(0)),
                    expression.NAME().getText(),
                    position(expression)
            );
        }

        if (isMethodCall(expression)) {
            var receiver = expressionNoLet(expression.expressionNoLet(0));
            var methodName = methodIdentifier(expression.methodIdentifier());
            var args = expression.argumentList() == null
                    ? new java.util.ArrayList<Expression>()
                    : new java.util.ArrayList<>(expression.argumentList().expression().stream().map(this::expression).toList());
            args.add(0, receiver);
            return new FunctionCall(Optional.empty(), METHOD_INVOKE_PREFIX + methodName, List.copyOf(args), position(expression));
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
            return new InfixExpression(
                    expressionNoLet(expression.expressionNoLet(0)),
                    InfixOperator.EQUAL,
                    new BooleanValue(false, position(expression)),
                    position(expression)
            );
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
        return new LambdaExpression(
                context.identifier().stream().map(CapybaraParser::identifier).toList(),
                expressionNoLetNoPipe(context.expressionNoLetNoPipe()),
                position(context)
        );
    }

    private ReduceExpression reduceExpression(FunctionalParser.ReduceExpressionContext context) {
        var names = context.NAME();
        if (names.size() < 2 || names.size() > 4) {
            throw new IllegalStateException("Reduce expression has to define two, three or four arguments");
        }
        var accumulatorName = names.size() == 4
                ? names.get(0).getText() + "::" + names.get(1).getText()
                : names.get(0).getText();
        var keyName = names.size() == 3
                ? Optional.of(names.get(1).getText())
                : names.size() == 4
                    ? Optional.of(names.get(2).getText())
                    : Optional.<String>empty();
        var valueName = names.size() == 2
                ? names.get(1).getText()
                : names.size() == 3
                    ? names.get(2).getText()
                    : names.get(3).getText();
        return new ReduceExpression(
                expressionNoLetNoPipe(context.expressionNoLetNoPipe(0)),
                accumulatorName,
                keyName,
                valueName,
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
                    return new StringValue(normalizeStringLiteral(literal.STRING_LITERAL().getText()), position(literal.STRING_LITERAL()));
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
            return indexExpression(expression);
        }

        if (isSlice(expression)) {
            return sliceExpression(expression);
        }

        if (isFieldAccess(expression)) {
            return new FieldAccess(
                    expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)),
                    expression.NAME().getText(),
                    position(expression)
            );
        }

        if (isMethodCall(expression)) {
            var receiver = expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0));
            var methodName = methodIdentifier(expression.methodIdentifier());
            var args = expression.argumentList() == null
                    ? new java.util.ArrayList<Expression>()
                    : new java.util.ArrayList<>(expression.argumentList().expression().stream().map(this::expression).toList());
            args.add(0, receiver);
            return new FunctionCall(Optional.empty(), METHOD_INVOKE_PREFIX + methodName, List.copyOf(args), position(expression));
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
            return new InfixExpression(
                    expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0)),
                    InfixOperator.EQUAL,
                    new BooleanValue(false, position(expression)),
                    position(expression)
            );
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
                        .flatMap(matchCase -> matchCase.pattern()
                                .stream()
                                .map(pattern -> matchCase(pattern, matchCase.expression())))
                        .toList(),
                position(context)
        );
    }

    private MatchExpression.MatchCase matchCase(
            FunctionalParser.PatternContext pattern,
            FunctionalParser.ExpressionContext expression
    ) {
        return new MatchExpression.MatchCase(
                matchExpressionPattern(pattern),
                expression(expression)

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
            return new MatchExpression.TypedPattern(
                    type(typedPattern.patternType().getText()),
                    typedPattern.NAME().getText()
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

        var name = context.NAME();
        if (name != null) {
            return new MatchExpression.VariablePattern(name.getText());
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

    private Expression functionCall(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FunctionCallContext context) {
        var arguments = context.argumentList() == null
                ? List.<Expression>of()
                : context.argumentList().expression().stream().map(this::expression).toList();
        var moduleName = context.TYPE() == null ? Optional.<String>empty() : Optional.of(context.TYPE().getText());
        return new FunctionCall(moduleName, identifier(context.identifier()), arguments, position(context));
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

    private Expression tupleExpression(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.TupleLiteralContext context) {
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

    private NewData.FieldAssignment fieldAssignment(pl.grzeslowski.capybara.parser.antlr.FunctionalParser.FieldAssignmentContext context) {
        if (context.spreadFieldAssignment() != null) {
            throw new IllegalStateException("Spread field assignment is not allowed in this context: " + context.getText());
        }
        var named = context.namedFieldAssignment();
        if (named == null) {
            throw new IllegalStateException("Named field assignment is not available in this context: " + context.getText());
        }
        return new NewData.FieldAssignment(fieldName(named.NAME(), named.STRING_LITERAL()), expression(named.expression()));
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

    private static String normalizeStringLiteral(String raw) {
        if (raw.length() < 2) {
            return raw;
        }
        if (raw.charAt(0) == '"' && raw.charAt(raw.length() - 1) == '"') {
            return raw;
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
        return expression.getChildCount() == 3 && ".".equals(expression.getChild(1).getText()) && expression.NAME() != null;
    }

    private static boolean isFieldAccess(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        return expression.getChildCount() == 3 && ".".equals(expression.getChild(1).getText()) && expression.NAME() != null;
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

    private IndexExpression indexExpression(FunctionalParser.ExpressionNoLetNoPipeContext expression) {
        var source = expressionNoLetNoPipe(expression.expressionNoLetNoPipe(0));
        var index = indexExpression(expression.indexNoPipeLiteral());
        return new IndexExpression(source, index, position(expression));
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



