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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
            if (definition.dataDeclaration() != null) {
                definitions.addAll(dataDeclarationDefinitions(definition.dataDeclaration()));
            } else if (definition.typeDeclaration() != null) {
                definitions.addAll(typeDeclarationDefinitions(definition.typeDeclaration()));
            } else if (definition.primitiveBackedTypeDeclaration() != null) {
                definitions.addAll(primitiveBackedTypeDeclarationDefinitions(definition.primitiveBackedTypeDeclaration()));
            } else if (definition.functionDeclaration() != null) {
                definitions.addAll(functionDefinitions(definition.functionDeclaration()));
            } else {
                definitions.add(functionalDefinition(definition));
            }
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
                        importNames(fromImport.group(3)),
                        importWildcard(fromImport.group(2)),
                        false,
                        new SourceLocation(lineNumber, leadingWhitespace(line))
                ));
                body.add("");
            } else if (qualifiedImport.matches()) {
                imports.add(new ImportDeclaration(
                        qualifiedImport.group(1),
                        List.of(),
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
        if (rawNames == null) {
            return List.of();
        }
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
            return annotationDeclaration(definition.annotationDeclaration());
        }
        if (definition.dataDeclaration() != null) {
            return Definition.UnsupportedDefinition.INSTANCE;
        }
        if (definition.deriverDeclaration() != null) {
            return Definition.DeriverDeclaration.INSTANCE;
        }
        if (definition.enumDeclaration() != null) {
            return enumDeclaration(definition.enumDeclaration());
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

    private static Definition.AnnotationDeclaration annotationDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationDeclarationContext ctx
    ) {
        var visibility = ctx.VISIBILITY() == null ? "public" : ctx.VISIBILITY().getText();
        return new Definition.AnnotationDeclaration(
                ctx.TYPE().getText(),
                visibility,
                ctx.multipleModifier() != null,
                annotationTargets(ctx.annotationTargetClause()),
                annotationFields(ctx.annotationBody()),
                definitionAnnotationApplications(ctx.annotationBlock()),
                location(ctx)
        );
    }

    private static List<String> annotationTargets(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationTargetClauseContext ctx
    ) {
        var targets = new ArrayList<String>();
        for (var target : ctx.annotationTarget()) {
            targets.add(target.getText());
        }
        return List.copyOf(targets);
    }

    private static List<Definition.AnnotationFieldDeclaration> annotationFields(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationBodyContext ctx
    ) {
        var fields = new ArrayList<Definition.AnnotationFieldDeclaration>();
        for (var field : ctx.annotationFieldDeclaration()) {
            fields.add(annotationField(field));
        }
        return List.copyOf(fields);
    }

    private static Definition.AnnotationFieldDeclaration annotationField(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationFieldDeclarationContext ctx
    ) {
        var hasDefault = ctx.annotationValue() != null;
        return new Definition.AnnotationFieldDeclaration(
                ctx.identifier().getText(),
                annotationTypeReference(ctx.annotationFieldType().annotationTypeReference()),
                hasDefault ? definitionAnnotationValue(ctx.annotationValue()) : new Definition.AnnotationNothingValue("", location(ctx)),
                hasDefault,
                location(ctx)
        );
    }

    private static TypeReference annotationTypeReference(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationTypeReferenceContext ctx
    ) {
        return new TypeReference(ctx.getText(), List.of());
    }

    private static List<Definition.AnnotationApplication> definitionAnnotationApplications(
            List<dev.capylang.parser.antlr.FunctionalParser.AnnotationBlockContext> blocks
    ) {
        var applications = new ArrayList<Definition.AnnotationApplication>();
        for (var block : blocks) {
            applications.add(definitionAnnotationApplication(block));
        }
        return List.copyOf(applications);
    }

    private static Definition.AnnotationApplication definitionAnnotationApplication(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationBlockContext ctx
    ) {
        return new Definition.AnnotationApplication(
                ctx.annotationName().getText(),
                definitionAnnotationArguments(ctx.annotationArgumentList()),
                location(ctx)
        );
    }

    private static List<Definition.AnnotationArgument> definitionAnnotationArguments(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationArgumentListContext ctx
    ) {
        if (ctx == null) {
            return List.of();
        }
        var arguments = new ArrayList<Definition.AnnotationArgument>();
        for (var argument : ctx.annotationArgument()) {
            arguments.add(new Definition.AnnotationArgument(
                    argument.identifier().getText(),
                    definitionAnnotationValue(argument.annotationValue()),
                    location(argument)
            ));
        }
        return List.copyOf(arguments);
    }

    private static Definition.AnnotationValue definitionAnnotationValue(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationValueContext ctx
    ) {
        var source = ctx.getText();
        var location = location(ctx);
        if (ctx.STRING_LITERAL() != null) {
            return new Definition.AnnotationStringValue(unquote(source), source, location);
        }
        if (ctx.INT_LITERAL() != null) {
            return new Definition.AnnotationIntValue(Integer.parseInt(cleanNumber(source)), source, location);
        }
        if (ctx.LONG_LITERAL() != null) {
            return new Definition.AnnotationLongValue(Long.parseLong(cleanNumber(source.substring(0, source.length() - 1))), source, location);
        }
        if (ctx.DOUBLE_LITERAL() != null) {
            return new Definition.AnnotationDoubleValue(Double.parseDouble(cleanDouble(source)), source, location);
        }
        if (ctx.FLOAT_LITERAL() != null) {
            return new Definition.AnnotationFloatValue(Float.parseFloat(cleanFloat(source)), source, location);
        }
        if (ctx.BOOL_LITERAL() != null) {
            return new Definition.AnnotationBoolValue(Boolean.parseBoolean(source), source, location);
        }
        if (ctx.NOTHING_LITERAL() != null) {
            return new Definition.AnnotationNothingValue(source, location);
        }
        if (ctx.annotationTypeReference() != null) {
            return new Definition.AnnotationTypeNameValue(ctx.annotationTypeReference().getText(), source, location);
        }
        return new Definition.AnnotationNothingValue(source, location);
    }

    private static List<AnnotationApplication> annotationApplications(
            List<dev.capylang.parser.antlr.FunctionalParser.AnnotationBlockContext> blocks
    ) {
        var applications = new ArrayList<AnnotationApplication>();
        for (var block : blocks) {
            applications.add(annotationApplication(block));
        }
        return List.copyOf(applications);
    }

    private static AnnotationApplication annotationApplication(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationBlockContext ctx
    ) {
        return new AnnotationApplication(
                ctx.annotationName().getText(),
                annotationArguments(ctx.annotationArgumentList()),
                location(ctx)
        );
    }

    private static List<AnnotationArgument> annotationArguments(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationArgumentListContext ctx
    ) {
        if (ctx == null) {
            return List.of();
        }
        var arguments = new ArrayList<AnnotationArgument>();
        for (var argument : ctx.annotationArgument()) {
            arguments.add(new AnnotationArgument(
                    argument.identifier().getText(),
                    annotationValue(argument.annotationValue()),
                    location(argument)
            ));
        }
        return List.copyOf(arguments);
    }

    private static AnnotationValue annotationValue(
            dev.capylang.parser.antlr.FunctionalParser.AnnotationValueContext ctx
    ) {
        var source = ctx.getText();
        var location = location(ctx);
        if (ctx.STRING_LITERAL() != null) {
            return new AnnotationStringValue(unquote(source), source, location);
        }
        if (ctx.INT_LITERAL() != null) {
            return new AnnotationIntValue(Integer.parseInt(cleanNumber(source)), source, location);
        }
        if (ctx.LONG_LITERAL() != null) {
            return new AnnotationLongValue(Long.parseLong(cleanNumber(source.substring(0, source.length() - 1))), source, location);
        }
        if (ctx.DOUBLE_LITERAL() != null) {
            return new AnnotationDoubleValue(Double.parseDouble(cleanDouble(source)), source, location);
        }
        if (ctx.FLOAT_LITERAL() != null) {
            return new AnnotationFloatValue(Float.parseFloat(cleanFloat(source)), source, location);
        }
        if (ctx.BOOL_LITERAL() != null) {
            return new AnnotationBoolValue(Boolean.parseBoolean(source), source, location);
        }
        if (ctx.NOTHING_LITERAL() != null) {
            return new AnnotationNothingValue(source, location);
        }
        if (ctx.annotationTypeReference() != null) {
            return new AnnotationTypeNameValue(ctx.annotationTypeReference().getText(), source, location);
        }
        return new AnnotationNothingValue(source, location);
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

    private static Definition.EnumDeclaration enumDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.EnumDeclarationContext ctx
    ) {
        var types = ctx.TYPE();
        var values = new ArrayList<Definition.EnumValueDeclaration>();
        for (var i = 1; i < types.size(); i++) {
            var token = types.get(i).getSymbol();
            values.add(new Definition.EnumValueDeclaration(token.getText(), location(token)));
        }
        return new Definition.EnumDeclaration(
                types.getFirst().getText(),
                List.copyOf(values),
                location(ctx)
        );
    }

    private static List<Definition> functionDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.FunctionDeclarationContext ctx
    ) {
        var definitions = new ArrayList<Definition>();
        var localNames = localFunctionNames(ctx);
        if (ctx.functionBody() != null) {
            for (var definition : ctx.functionBody().localDefinition()) {
                if (definition.localDataDeclaration() != null) {
                    definitions.addAll(localDataDeclarationDefinitions(definition.localDataDeclaration()));
                } else if (definition.localTypeDeclaration() != null) {
                    definitions.addAll(localTypeDeclarationDefinitions(definition.localTypeDeclaration()));
                }
            }
            for (var definition : ctx.functionBody().localDefinition()) {
                if (definition.localFunctionDeclaration() != null) {
                    definitions.add(new Definition.FunctionDefinition(localFunctionDeclaration(
                            definition.localFunctionDeclaration(),
                            localNames
                    )));
                }
            }
        }
        definitions.add(new Definition.FunctionDefinition(functionDeclaration(ctx, localNames)));
        return List.copyOf(definitions);
    }

    private static Map<String, String> localFunctionNames(
            dev.capylang.parser.antlr.FunctionalParser.FunctionDeclarationContext ctx
    ) {
        if (ctx.functionBody() == null) {
            return Map.of();
        }
        var names = new LinkedHashMap<String, String>();
        var outerName = ctx.functionNameDeclaration().getText();
        for (var definition : ctx.functionBody().localDefinition()) {
            if (definition.localFunctionDeclaration() != null) {
                var local = definition.localFunctionDeclaration();
                var localName = local.localFunctionNameDeclaration().getText();
                names.put(localName, outerName + "__local__" + localName + "__" + location(local).line() + "_" + location(local).column());
            }
        }
        return Map.copyOf(names);
    }

    private static FunctionDeclaration functionDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.FunctionDeclarationContext ctx
    ) {
        return functionDeclaration(ctx, Map.of());
    }

    private static FunctionDeclaration functionDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.FunctionDeclarationContext ctx,
            Map<String, String> localNames
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
                functionBody(ctx.functionBody(), localNames),
                annotationApplications(ctx.annotationBlock()),
                location(ctx)
        );
    }

    private static FunctionDeclaration localFunctionDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.LocalFunctionDeclarationContext ctx,
            Map<String, String> localNames
    ) {
        var parameters = new ArrayList<FunctionParameter>();
        if (ctx.parameters() != null) {
            for (var parameter : ctx.parameters().parameter()) {
                parameters.add(functionParameter(parameter));
            }
        }
        var returnType = ctx.functionType() == null
                ? missingType()
                : typeReference(ctx.functionType().type());
        var name = localNames.getOrDefault(ctx.localFunctionNameDeclaration().getText(), ctx.localFunctionNameDeclaration().getText());
        return new FunctionDeclaration(
                name,
                "private",
                List.copyOf(parameters),
                returnType,
                rewriteLocalFunctionCalls(expression(ctx.expression()), localNames),
                annotationApplications(ctx.annotationBlock()),
                location(ctx)
        );
    }

    private static List<Definition> dataDeclarationDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.DataDeclarationContext ctx
    ) {
        var declaration = ctx.genericTypeDeclaration();
        var visibility = ctx.VISIBILITY() == null ? "public" : ctx.VISIBILITY().getText();
        return dataDeclarationDefinitions(
                declaration,
                visibility,
                ctx.dataBody(),
                ctx.constructorClause(),
                definitionAnnotationApplications(ctx.annotationBlock()),
                location(ctx)
        );
    }

    private static List<Definition> localDataDeclarationDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.LocalDataDeclarationContext ctx
    ) {
        var declaration = ctx.genericTypeDeclaration();
        return dataDeclarationDefinitions(declaration, "private", ctx.dataBody(), ctx.constructorClause(), List.of(), location(ctx));
    }

    private static List<Definition> typeDeclarationDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.TypeDeclarationContext ctx
    ) {
        return typeDeclarationDefinitions(
                ctx.genericTypeDeclaration(),
                ctx.fieldDeclarationList(),
                ctx.constructorClause(),
                location(ctx)
        );
    }

    private static List<Definition> localTypeDeclarationDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.LocalTypeDeclarationContext ctx
    ) {
        return typeDeclarationDefinitions(
                ctx.genericTypeDeclaration(),
                ctx.fieldDeclarationList(),
                ctx.constructorClause(),
                location(ctx)
        );
    }

    private static List<Definition> typeDeclarationDefinitions(
            List<dev.capylang.parser.antlr.FunctionalParser.GenericTypeDeclarationContext> declarations,
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationListContext parentFields,
            dev.capylang.parser.antlr.FunctionalParser.ConstructorClauseContext constructorClause,
            SourceLocation location
    ) {
        if (declarations.isEmpty()) {
            return List.of();
        }

        var unionDeclaration = declarations.getFirst();
        var name = dataTypeName(unionDeclaration);
        var definitions = new ArrayList<Definition>();
        definitions.add(schemaConstantDefinition("__capy_schema_type|" + name, name, location));

        var typeParameters = dataTypeParameters(unionDeclaration);
        for (var i = 0; i < typeParameters.size(); i++) {
            definitions.add(schemaConstantDefinition(
                    "__capy_schema_param|" + name + "|" + i,
                    typeParameters.get(i),
                    location
            ));
        }

        var fields = fieldDeclarationFields(parentFields);
        for (var i = 0; i < fields.size(); i++) {
            definitions.add(schemaConstantDefinition(
                    "__capy_schema_field|" + name + "|" + i,
                    fields.get(i),
                    location
            ));
        }

        for (var declarationIndex = 1; declarationIndex < declarations.size(); declarationIndex++) {
            var variantName = dataTypeName(declarations.get(declarationIndex));
            definitions.add(schemaConstantDefinition(
                    "__capy_schema_parent|" + variantName + "|" + (declarationIndex - 1),
                    name,
                    location
            ));
            for (var fieldIndex = 0; fieldIndex < fields.size(); fieldIndex++) {
                definitions.add(schemaConstantDefinition(
                        "__capy_schema_field|" + variantName + "|parent|" + fieldIndex,
                        fields.get(fieldIndex),
                        location
                ));
            }
        }

        if (constructorClause != null) {
            definitions.add(constructorFunctionDefinition(name, constructorParameters(parentFields), constructorClause));
        }

        return List.copyOf(definitions);
    }

    private static List<Definition> dataDeclarationDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.GenericTypeDeclarationContext declaration,
            String visibility,
            dev.capylang.parser.antlr.FunctionalParser.DataBodyContext dataBody,
            dev.capylang.parser.antlr.FunctionalParser.ConstructorClauseContext constructorClause,
            List<Definition.AnnotationApplication> annotations,
            SourceLocation location
    ) {
        var name = dataTypeName(declaration);
        var definitions = new ArrayList<Definition>();
        definitions.add(new Definition.DataDeclaration(
                name,
                visibility,
                dataTypeParameters(declaration),
                dataDeclarationOwnFields(dataBody),
                dataDeclarationParents(dataBody),
                annotations,
                location
        ));
        if (constructorClause != null) {
            definitions.add(constructorFunctionDefinition(name, constructorParameters(dataBody), constructorClause));
        }
        return List.copyOf(definitions);
    }

    private static List<Definition> primitiveBackedTypeDeclarationDefinitions(
            dev.capylang.parser.antlr.FunctionalParser.PrimitiveBackedTypeDeclarationContext ctx
    ) {
        var name = ctx.primitiveBackedTypeName().getText();
        var backingType = primitiveBackingTypeReference(ctx.primitiveBackingType());
        var location = location(ctx);
        var definitions = new ArrayList<Definition>();
        definitions.add(schemaConstantDefinition("__capy_schema_type|" + name, name, location));
        definitions.add(schemaConstantDefinition("__capy_schema_primitive|" + name, backingType.name(), location));
        definitions.add(schemaConstantDefinition(
                "__capy_schema_field|" + name + "|0",
                "value|" + backingType.name(),
                location
        ));
        if (ctx.constructorClause() != null) {
            definitions.add(constructorFunctionDefinition(
                    name,
                    List.of(new FunctionParameter("value", backingType, location(ctx.primitiveBackingType()))),
                    ctx.constructorClause()
            ));
        }
        return List.copyOf(definitions);
    }

    private static Definition constructorFunctionDefinition(
            String name,
            List<FunctionParameter> parameters,
            dev.capylang.parser.antlr.FunctionalParser.ConstructorClauseContext constructorClause
    ) {
        return new Definition.FunctionDefinition(new FunctionDeclaration(
                "__capy_constructor|" + name,
                "private",
                parameters,
                new TypeReference("any", List.of()),
                rewriteConstructorData(expression(constructorClause.expression()), name),
                List.of(),
                location(constructorClause)
        ));
    }

    private static List<FunctionParameter> constructorParameters(
            dev.capylang.parser.antlr.FunctionalParser.DataBodyContext dataBody
    ) {
        if (dataBody == null || dataBody.fieldDeclarationList() == null) {
            return List.of();
        }
        var parameters = new ArrayList<FunctionParameter>();
        for (var field : dataBody.fieldDeclarationList().fieldDeclaration()) {
            if (!dataParentDeclaration(field)) {
                parameters.add(constructorParameter(field));
            }
        }
        return List.copyOf(parameters);
    }

    private static List<FunctionParameter> constructorParameters(
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationListContext fields
    ) {
        if (fields == null) {
            return List.of();
        }
        var parameters = new ArrayList<FunctionParameter>();
        for (var field : fields.fieldDeclaration()) {
            if (!dataParentDeclaration(field)) {
                parameters.add(constructorParameter(field));
            }
        }
        return List.copyOf(parameters);
    }

    private static FunctionParameter constructorParameter(
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationContext field
    ) {
        var name = field.identifier() == null
                ? unquote(field.STRING_LITERAL().getText())
                : field.identifier().getText();
        return new FunctionParameter(name, typeReference(field.type()), location(field));
    }

    private static Definition schemaConstantDefinition(String name, String value, SourceLocation location) {
        return new Definition.ConstantDefinition(new ConstantDeclaration(
                name,
                "schema",
                stringType(),
                new Expression.StringLiteral(value, quote(value), location),
                location
        ));
    }

    private static TypeReference stringType() {
        return new TypeReference("String", List.of());
    }

    private static TypeReference primitiveBackingTypeReference(
            dev.capylang.parser.antlr.FunctionalParser.PrimitiveBackingTypeContext ctx
    ) {
        return new TypeReference(ctx.getText(), List.of());
    }

    private static String quote(String value) {
        return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
    }

    private static String dataTypeName(
            dev.capylang.parser.antlr.FunctionalParser.GenericTypeDeclarationContext ctx
    ) {
        return ctx.TYPE(0).getText();
    }

    private static List<String> dataTypeParameters(
            dev.capylang.parser.antlr.FunctionalParser.GenericTypeDeclarationContext ctx
    ) {
        var parameters = new ArrayList<String>();
        for (var i = 1; i < ctx.TYPE().size(); i++) {
            parameters.add(ctx.TYPE(i).getText());
        }
        return List.copyOf(parameters);
    }

    private static List<String> dataDeclarationFields(
            dev.capylang.parser.antlr.FunctionalParser.DataBodyContext ctx
    ) {
        if (ctx == null || ctx.fieldDeclarationList() == null) {
            return List.of();
        }
        return fieldDeclarationFields(ctx.fieldDeclarationList());
    }

    private static List<Definition.DataFieldDeclaration> dataDeclarationOwnFields(
            dev.capylang.parser.antlr.FunctionalParser.DataBodyContext ctx
    ) {
        if (ctx == null || ctx.fieldDeclarationList() == null) {
            return List.of();
        }
        var fields = new ArrayList<Definition.DataFieldDeclaration>();
        for (var field : ctx.fieldDeclarationList().fieldDeclaration()) {
            if (!dataParentDeclaration(field)) {
                fields.add(dataFieldDeclarationDto(field));
            }
        }
        return List.copyOf(fields);
    }

    private static Definition.DataFieldDeclaration dataFieldDeclarationDto(
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationContext ctx
    ) {
        var name = ctx.identifier() != null
                ? ctx.identifier().getText()
                : unquote(ctx.STRING_LITERAL().getText());
        return new Definition.DataFieldDeclaration(
                name,
                typeReference(ctx.type()),
                definitionAnnotationApplications(ctx.annotationBlock()),
                location(ctx)
        );
    }

    private static List<Definition.DataParentDeclaration> dataDeclarationParents(
            dev.capylang.parser.antlr.FunctionalParser.DataBodyContext ctx
    ) {
        if (ctx == null || ctx.fieldDeclarationList() == null) {
            return List.of();
        }
        var parents = new ArrayList<Definition.DataParentDeclaration>();
        for (var field : ctx.fieldDeclarationList().fieldDeclaration()) {
            if (dataParentDeclaration(field)) {
                parents.add(new Definition.DataParentDeclaration(
                        new TypeReference(field.TYPE().getText(), List.of()),
                        location(field)
                ));
            }
        }
        return List.copyOf(parents);
    }

    private static boolean dataParentDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationContext ctx
    ) {
        return ctx.SPREAD() != null && ctx.TYPE() != null;
    }

    private static List<String> fieldDeclarationFields(
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationListContext ctx
    ) {
        if (ctx == null) {
            return List.of();
        }
        var fields = new ArrayList<String>();
        for (var field : ctx.fieldDeclaration()) {
            fields.add(dataFieldDeclaration(field));
        }
        return List.copyOf(fields);
    }

    private static String dataFieldDeclaration(
            dev.capylang.parser.antlr.FunctionalParser.FieldDeclarationContext ctx
    ) {
        if (ctx.identifier() != null) {
            return dataFieldSchema(ctx.identifier().getText(), ctx.type());
        }
        if (ctx.STRING_LITERAL() != null) {
            return dataFieldSchema(unquote(ctx.STRING_LITERAL().getText()), ctx.type());
        }
        return "$unsupported|";
    }

    private static String dataFieldSchema(
            String name,
            dev.capylang.parser.antlr.FunctionalParser.TypeContext type
    ) {
        return name + "|" + type.getText();
    }

    private static FunctionParameter functionParameter(dev.capylang.parser.antlr.FunctionalParser.ParameterContext ctx) {
        return new FunctionParameter(
                ctx.identifier().getText(),
                typeReference(ctx.type()),
                location(ctx)
        );
    }

    private static Expression functionBody(dev.capylang.parser.antlr.FunctionalParser.FunctionBodyContext ctx) {
        return functionBody(ctx, Map.of());
    }

    private static Expression functionBody(
            dev.capylang.parser.antlr.FunctionalParser.FunctionBodyContext ctx,
            Map<String, String> localNames
    ) {
        if (ctx.localDefinition().isEmpty()) {
            return rewriteLocalFunctionCalls(expression(ctx.expression()), localNames);
        }

        var bindings = new ArrayList<Expression.LetBinding>();
        for (var definition : ctx.localDefinition()) {
            if (definition.localConstDeclaration() != null) {
                bindings.add(localConstBinding(definition.localConstDeclaration(), localNames));
            }
        }
        var result = rewriteLocalFunctionCalls(expression(ctx.expression()), localNames);
        if (bindings.isEmpty()) {
            return result;
        }
        return new Expression.BlockExpression(List.copyOf(bindings), result, location(ctx));
    }

    private static Expression.LetBinding localConstBinding(
            dev.capylang.parser.antlr.FunctionalParser.LocalConstDeclarationContext ctx
    ) {
        return localConstBinding(ctx, Map.of());
    }

    private static Expression.LetBinding localConstBinding(
            dev.capylang.parser.antlr.FunctionalParser.LocalConstDeclarationContext ctx,
            Map<String, String> localNames
    ) {
        var type = ctx.type() == null ? missingType() : typeReference(ctx.type());
        return new Expression.LetBinding(
                ctx.privateLocalConstName().getText(),
                type,
                "=",
                rewriteLocalFunctionCalls(expressionNoLet(ctx.expressionNoLet()), localNames),
                location(ctx)
        );
    }

    private static Expression rewriteLocalFunctionCalls(Expression expression, Map<String, String> localNames) {
        if (localNames.isEmpty()) {
            return expression;
        }
        if (expression instanceof Expression.IfExpression value) {
            return new Expression.IfExpression(
                    rewriteLocalFunctionCalls(value.condition(), localNames),
                    rewriteLocalFunctionCalls(value.thenBranch(), localNames),
                    rewriteLocalFunctionCalls(value.elseBranch(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.BinaryExpression value) {
            return new Expression.BinaryExpression(
                    value.operator(),
                    rewriteLocalFunctionCalls(value.left(), localNames),
                    rewriteLocalFunctionCalls(value.right(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.UnaryExpression value) {
            return new Expression.UnaryExpression(
                    value.operator(),
                    rewriteLocalFunctionCalls(value.expression(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.FunctionCallExpression value) {
            return new Expression.FunctionCallExpression(
                    localNames.getOrDefault(value.name(), value.name()),
                    rewriteLocalFunctionCalls(value.arguments(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.FunctionReferenceExpression value) {
            return new Expression.FunctionReferenceExpression(
                    localNames.getOrDefault(value.name(), value.name()),
                    value.location()
            );
        }
        if (expression instanceof Expression.MethodCallExpression value) {
            return new Expression.MethodCallExpression(
                    rewriteLocalFunctionCalls(value.receiver(), localNames),
                    value.name(),
                    rewriteLocalFunctionCalls(value.arguments(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.WithExpression value) {
            return new Expression.WithExpression(
                    rewriteLocalFunctionCalls(value.receiver(), localNames),
                    rewriteDataFields(value.fields(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.LambdaExpression value) {
            return new Expression.LambdaExpression(
                    value.parameters(),
                    rewriteLocalFunctionCalls(value.body(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.ListLiteral value) {
            return new Expression.ListLiteral(rewriteLocalFunctionCalls(value.values(), localNames), value.location());
        }
        if (expression instanceof Expression.SetLiteral value) {
            return new Expression.SetLiteral(rewriteLocalFunctionCalls(value.values(), localNames), value.location());
        }
        if (expression instanceof Expression.DictLiteral value) {
            return new Expression.DictLiteral(rewriteDictEntries(value.entries(), localNames), value.location());
        }
        if (expression instanceof Expression.TupleLiteral value) {
            return new Expression.TupleLiteral(rewriteLocalFunctionCalls(value.values(), localNames), value.location());
        }
        if (expression instanceof Expression.IndexExpression value) {
            return new Expression.IndexExpression(
                    rewriteLocalFunctionCalls(value.receiver(), localNames),
                    rewriteLocalFunctionCalls(value.index(), localNames),
                    rewriteLocalFunctionCalls(value.endIndex(), localNames),
                    value.hasEndIndex(),
                    value.location()
            );
        }
        if (expression instanceof Expression.DataLiteral value) {
            return new Expression.DataLiteral(
                    value.typeName(),
                    rewriteDataFields(value.fields(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.FieldAccessExpression value) {
            return new Expression.FieldAccessExpression(
                    rewriteLocalFunctionCalls(value.receiver(), localNames),
                    value.name(),
                    value.location()
            );
        }
        if (expression instanceof Expression.ReduceExpression value) {
            return new Expression.ReduceExpression(
                    rewriteLocalFunctionCalls(value.receiver(), localNames),
                    rewriteLocalFunctionCalls(value.initial(), localNames),
                    value.accumulatorName(),
                    value.keyName(),
                    value.valueName(),
                    rewriteLocalFunctionCalls(value.body(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.MatchExpression value) {
            return new Expression.MatchExpression(
                    rewriteLocalFunctionCalls(value.value(), localNames),
                    rewriteMatchCases(value.cases(), localNames),
                    value.location()
            );
        }
        if (expression instanceof Expression.BlockExpression value) {
            return new Expression.BlockExpression(
                    rewriteLetBindings(value.bindings(), localNames),
                    rewriteLocalFunctionCalls(value.result(), localNames),
                    value.location()
            );
        }
        return expression;
    }

    private static List<Expression> rewriteLocalFunctionCalls(List<Expression> expressions, Map<String, String> localNames) {
        return expressions.stream()
                .map(expression -> rewriteLocalFunctionCalls(expression, localNames))
                .toList();
    }

    private static List<Expression.DictEntry> rewriteDictEntries(
            List<Expression.DictEntry> entries,
            Map<String, String> localNames
    ) {
        return entries.stream()
                .map(entry -> new Expression.DictEntry(
                        rewriteLocalFunctionCalls(entry.key(), localNames),
                        rewriteLocalFunctionCalls(entry.value(), localNames),
                        entry.location()
                ))
                .toList();
    }

    private static List<Expression.DataField> rewriteDataFields(
            List<Expression.DataField> fields,
            Map<String, String> localNames
    ) {
        return fields.stream()
                .map(field -> new Expression.DataField(
                        field.name(),
                        rewriteLocalFunctionCalls(field.value(), localNames),
                        field.spread(),
                        field.location()
                ))
                .toList();
    }

    private static List<Expression.MatchCase> rewriteMatchCases(
            List<Expression.MatchCase> cases,
            Map<String, String> localNames
    ) {
        return cases.stream()
                .map(matchCase -> new Expression.MatchCase(
                        matchCase.typeName(),
                        matchCase.bindings(),
                        matchCase.bindsWholeValue(),
                        rewriteLocalFunctionCalls(matchCase.literal(), localNames),
                        matchCase.hasLiteral(),
                        matchCase.wildcard(),
                        rewriteLocalFunctionCalls(matchCase.guard(), localNames),
                        matchCase.hasGuard(),
                        rewriteLocalFunctionCalls(matchCase.body(), localNames),
                        matchCase.location()
                ))
                .toList();
    }

    private static List<Expression.LetBinding> rewriteLetBindings(
            List<Expression.LetBinding> bindings,
            Map<String, String> localNames
    ) {
        return bindings.stream()
                .map(binding -> new Expression.LetBinding(
                        binding.name(),
                        binding.typeReference(),
                        binding.operator(),
                        rewriteLocalFunctionCalls(binding.value(), localNames),
                        binding.location()
                ))
                .toList();
    }

    private static Expression rewriteConstructorData(Expression expression, String dataTypeName) {
        if (expression instanceof Expression.IfExpression value) {
            return new Expression.IfExpression(
                    rewriteConstructorData(value.condition(), dataTypeName),
                    rewriteConstructorData(value.thenBranch(), dataTypeName),
                    rewriteConstructorData(value.elseBranch(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.BinaryExpression value) {
            return new Expression.BinaryExpression(
                    value.operator(),
                    rewriteConstructorData(value.left(), dataTypeName),
                    rewriteConstructorData(value.right(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.UnaryExpression value) {
            return new Expression.UnaryExpression(
                    value.operator(),
                    rewriteConstructorData(value.expression(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.FunctionCallExpression value) {
            return new Expression.FunctionCallExpression(
                    value.name(),
                    rewriteConstructorData(value.arguments(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.MethodCallExpression value) {
            return new Expression.MethodCallExpression(
                    rewriteConstructorData(value.receiver(), dataTypeName),
                    value.name(),
                    rewriteConstructorData(value.arguments(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.WithExpression value) {
            return new Expression.WithExpression(
                    rewriteConstructorData(value.receiver(), dataTypeName),
                    rewriteConstructorDataFields(value.fields(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.LambdaExpression value) {
            return new Expression.LambdaExpression(
                    value.parameters(),
                    rewriteConstructorData(value.body(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.ListLiteral value) {
            return new Expression.ListLiteral(rewriteConstructorData(value.values(), dataTypeName), value.location());
        }
        if (expression instanceof Expression.SetLiteral value) {
            return new Expression.SetLiteral(rewriteConstructorData(value.values(), dataTypeName), value.location());
        }
        if (expression instanceof Expression.DictLiteral value) {
            return new Expression.DictLiteral(rewriteConstructorDataDictEntries(value.entries(), dataTypeName), value.location());
        }
        if (expression instanceof Expression.TupleLiteral value) {
            return new Expression.TupleLiteral(rewriteConstructorData(value.values(), dataTypeName), value.location());
        }
        if (expression instanceof Expression.IndexExpression value) {
            return new Expression.IndexExpression(
                    rewriteConstructorData(value.receiver(), dataTypeName),
                    rewriteConstructorData(value.index(), dataTypeName),
                    rewriteConstructorData(value.endIndex(), dataTypeName),
                    value.hasEndIndex(),
                    value.location()
            );
        }
        if (expression instanceof Expression.DataLiteral value) {
            var typeName = value.typeName().equals("*") ? "__capy_raw|" + dataTypeName : value.typeName();
            return new Expression.DataLiteral(
                    typeName,
                    rewriteConstructorDataFields(value.fields(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.FieldAccessExpression value) {
            return new Expression.FieldAccessExpression(
                    rewriteConstructorData(value.receiver(), dataTypeName),
                    value.name(),
                    value.location()
            );
        }
        if (expression instanceof Expression.ReduceExpression value) {
            return new Expression.ReduceExpression(
                    rewriteConstructorData(value.receiver(), dataTypeName),
                    rewriteConstructorData(value.initial(), dataTypeName),
                    value.accumulatorName(),
                    value.keyName(),
                    value.valueName(),
                    rewriteConstructorData(value.body(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.MatchExpression value) {
            return new Expression.MatchExpression(
                    rewriteConstructorData(value.value(), dataTypeName),
                    rewriteConstructorDataMatchCases(value.cases(), dataTypeName),
                    value.location()
            );
        }
        if (expression instanceof Expression.BlockExpression value) {
            return new Expression.BlockExpression(
                    rewriteConstructorDataLetBindings(value.bindings(), dataTypeName),
                    rewriteConstructorData(value.result(), dataTypeName),
                    value.location()
            );
        }
        return expression;
    }

    private static List<Expression> rewriteConstructorData(List<Expression> expressions, String dataTypeName) {
        return expressions.stream()
                .map(expression -> rewriteConstructorData(expression, dataTypeName))
                .toList();
    }

    private static List<Expression.DictEntry> rewriteConstructorDataDictEntries(
            List<Expression.DictEntry> entries,
            String dataTypeName
    ) {
        return entries.stream()
                .map(entry -> new Expression.DictEntry(
                        rewriteConstructorData(entry.key(), dataTypeName),
                        rewriteConstructorData(entry.value(), dataTypeName),
                        entry.location()
                ))
                .toList();
    }

    private static List<Expression.DataField> rewriteConstructorDataFields(
            List<Expression.DataField> fields,
            String dataTypeName
    ) {
        return fields.stream()
                .map(field -> new Expression.DataField(
                        field.name(),
                        rewriteConstructorData(field.value(), dataTypeName),
                        field.spread(),
                        field.location()
                ))
                .toList();
    }

    private static List<Expression.MatchCase> rewriteConstructorDataMatchCases(
            List<Expression.MatchCase> cases,
            String dataTypeName
    ) {
        return cases.stream()
                .map(matchCase -> new Expression.MatchCase(
                        matchCase.typeName(),
                        matchCase.bindings(),
                        matchCase.bindsWholeValue(),
                        rewriteConstructorData(matchCase.literal(), dataTypeName),
                        matchCase.hasLiteral(),
                        matchCase.wildcard(),
                        rewriteConstructorData(matchCase.guard(), dataTypeName),
                        matchCase.hasGuard(),
                        rewriteConstructorData(matchCase.body(), dataTypeName),
                        matchCase.location()
                ))
                .toList();
    }

    private static List<Expression.LetBinding> rewriteConstructorDataLetBindings(
            List<Expression.LetBinding> bindings,
            String dataTypeName
    ) {
        return bindings.stream()
                .map(binding -> new Expression.LetBinding(
                        binding.name(),
                        binding.typeReference(),
                        binding.operator(),
                        rewriteConstructorData(binding.value(), dataTypeName),
                        binding.location()
                ))
                .toList();
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
                ctx.letBindingOperator().getText(),
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
        if (ctx.functionReference() != null) {
            return new Expression.FunctionReferenceExpression(
                    ctx.functionReference().identifier().getText(),
                    location(ctx.functionReference())
            );
        }
        if (ctx.placeholder() != null) {
            return placeholder(ctx.placeholder());
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
        if (ctx.constructorData() != null) {
            return constructorDataLiteral(ctx.constructorData());
        }
        if (ctx.matchExpression() != null) {
            return matchExpression(ctx.matchExpression());
        }
        if (ctx.value() != null) {
            return value(ctx.value());
        }
        if (ctx.infixMethodLiteral() != null && ctx.expressionNoLet().size() == 2) {
            return binaryExpression(
                    ctx.infixMethodLiteral().getText(),
                    expressionNoLet(ctx.expressionNoLet(0)),
                    expressionNoLet(ctx.expressionNoLet(1)),
                    location(ctx),
                    isGrouped(ctx.expressionNoLet(0))
            );
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
            return methodCallExpression(
                    expressionNoLet(ctx.expressionNoLet(0)),
                    ctx.methodIdentifier().getText(),
                    ctx.methodArgumentList(),
                    location(ctx)
            );
        }
        if (isInvocationExpression(ctx)) {
            return invocationExpression(expressionNoLet(ctx.expressionNoLet(0)), ctx.argumentList(), location(ctx));
        }
        if (isSliceExpression(ctx)) {
            return sliceExpression(expressionNoLet(ctx.expressionNoLet(0)), location(ctx), ctx);
        }
        if (isIndexExpression(ctx)) {
            return indexExpression(expressionNoLet(ctx.expressionNoLet(0)), ctx.argumentList(), location(ctx), ctx);
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
        if (ctx.functionReference() != null) {
            return new Expression.FunctionReferenceExpression(
                    ctx.functionReference().identifier().getText(),
                    location(ctx.functionReference())
            );
        }
        if (ctx.placeholder() != null) {
            return placeholder(ctx.placeholder());
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
        if (ctx.constructorData() != null) {
            return constructorDataLiteral(ctx.constructorData());
        }
        if (ctx.matchExpressionNoPipe() != null) {
            return matchExpressionNoPipe(ctx.matchExpressionNoPipe());
        }
        if (ctx.value() != null) {
            return value(ctx.value());
        }
        if (ctx.infixMethodLiteral() != null && ctx.expressionNoLetNoPipe().size() == 2) {
            return binaryExpression(
                    ctx.infixMethodLiteral().getText(),
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)),
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(1)),
                    location(ctx),
                    isGrouped(ctx.expressionNoLetNoPipe(0))
            );
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
            return methodCallExpression(
                    expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)),
                    ctx.methodIdentifier().getText(),
                    ctx.methodArgumentList(),
                    location(ctx)
            );
        }
        if (isInvocationExpressionNoPipe(ctx)) {
            return invocationExpression(expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)), ctx.argumentList(), location(ctx));
        }
        if (isSliceExpressionNoPipe(ctx)) {
            return sliceExpression(expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)), location(ctx), ctx);
        }
        if (ctx.argumentList() != null && ctx.expressionNoLetNoPipe().size() == 1 && hasChild(ctx, "[")) {
            return indexExpression(expressionNoLetNoPipe(ctx.expressionNoLetNoPipe(0)), ctx.argumentList(), location(ctx), ctx);
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
        if (ctx.getChildCount() != 3) {
            return false;
        }
        var open = ctx.getChild(0).getText();
        var close = ctx.getChild(2).getText();
        return (open.equals("(") && close.equals(")"))
                || (open.equals("{") && close.equals("}"));
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
        var keyName = ctx.lambdaArgument().size() > 2 ? ctx.lambdaArgument(1).getText() : "";
        var valueName = ctx.lambdaArgument().size() > 2 ? ctx.lambdaArgument(2).getText() : ctx.lambdaArgument(1).getText();
        return new Expression.ReduceExpression(
                receiver,
                expressionNoLetNoPipe(ctx.expressionNoLetNoPipe()),
                ctx.lambdaArgument(0).getText(),
                keyName,
                valueName,
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

    private static Expression methodCallExpression(
            Expression receiver,
            String name,
            dev.capylang.parser.antlr.FunctionalParser.MethodArgumentListContext arguments,
            SourceLocation location
    ) {
        if (name.equals("with")) {
            return withExpression(receiver, arguments, location);
        }
        var reduce = methodReduceExpression(receiver, name, arguments, location);
        if (reduce != null) {
            return reduce;
        }
        return new Expression.MethodCallExpression(receiver, name, methodArguments(arguments), location);
    }

    private static Expression invocationExpression(
            Expression receiver,
            dev.capylang.parser.antlr.FunctionalParser.ArgumentListContext arguments,
            SourceLocation location
    ) {
        return new Expression.MethodCallExpression(receiver, "__capy_call", arguments(arguments), location);
    }

    private static Expression withExpression(
            Expression receiver,
            dev.capylang.parser.antlr.FunctionalParser.MethodArgumentListContext arguments,
            SourceLocation location
    ) {
        return new Expression.WithExpression(receiver, withFields(arguments), location);
    }

    private static Expression methodReduceExpression(
            Expression receiver,
            String name,
            dev.capylang.parser.antlr.FunctionalParser.MethodArgumentListContext arguments,
            SourceLocation location
    ) {
        if (!(name.equals("reduce") || name.equals("reduce_left")) || arguments == null || arguments.methodArgument().size() != 1) {
            return null;
        }
        var argument = arguments.methodArgument().get(0);
        if (argument.expression() == null || argument.expression().expressionNoLet().reduceExpression() == null) {
            return null;
        }
        return reduceExpression(receiver, argument.expression().expressionNoLet().reduceExpression(), location);
    }

    private static Expression placeholder(dev.capylang.parser.antlr.FunctionalParser.PlaceholderContext ctx) {
        return unsupported(ctx);
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

    private static List<Expression.DataField> withFields(
            dev.capylang.parser.antlr.FunctionalParser.MethodArgumentListContext ctx
    ) {
        if (ctx == null) {
            return List.of();
        }
        var fields = new ArrayList<Expression.DataField>();
        for (var argument : ctx.methodArgument()) {
            if (argument.namedMethodArgument() != null) {
                var named = argument.namedMethodArgument();
                fields.add(new Expression.DataField(
                        named.identifier().getText(),
                        expression(named.expression()),
                        false,
                        location(named)
                ));
            } else {
                fields.add(new Expression.DataField(
                        "$unsupported",
                        unsupported(argument),
                        false,
                        location(argument)
                ));
            }
        }
        return List.copyOf(fields);
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

    private static Expression indexExpression(
            Expression receiver,
            dev.capylang.parser.antlr.FunctionalParser.ArgumentListContext arguments,
            SourceLocation location,
            ParserRuleContext fallback
    ) {
        if (arguments == null || arguments.expression().isEmpty()) {
            return new Expression.IndexExpression(receiver, unsupported(fallback), unsupported(fallback), false, location);
        }
        var index = expression(arguments.expression(0));
        if (arguments.expression().size() > 1) {
            return new Expression.IndexExpression(receiver, index, expression(arguments.expression(1)), true, location);
        }
        return new Expression.IndexExpression(receiver, index, unsupported(fallback), false, location);
    }

    private static Expression sliceExpression(Expression receiver, SourceLocation location, ParserRuleContext ctx) {
        Expression start = omittedSliceStart(location);
        Expression end = omittedSliceEnd(location);
        var beforeColon = true;
        for (var idx = 0; idx < ctx.getChildCount(); idx++) {
            var child = ctx.getChild(idx);
            var text = child.getText();
            if (text.equals(":")) {
                beforeColon = false;
            } else if (isSliceIndexLiteral(child)) {
                if (beforeColon) {
                    start = sliceIndexLiteral(child, location((ParserRuleContext) child));
                } else {
                    end = sliceIndexLiteral(child, location((ParserRuleContext) child));
                }
            }
        }
        return new Expression.IndexExpression(receiver, start, end, true, location);
    }

    private static boolean isSliceIndexLiteral(ParseTree tree) {
        return tree instanceof dev.capylang.parser.antlr.FunctionalParser.SliceIndexLiteralContext
                || tree instanceof dev.capylang.parser.antlr.FunctionalParser.SliceIndexNoPipeLiteralContext;
    }

    private static Expression sliceIndexLiteral(ParseTree tree, SourceLocation location) {
        var source = tree.getText();
        return new Expression.IntLiteral(Integer.parseInt(cleanNumber(source)), source, location);
    }

    private static Expression omittedSliceStart(SourceLocation location) {
        return new Expression.IntLiteral(0, "__capy_slice_start__", location);
    }

    private static Expression omittedSliceEnd(SourceLocation location) {
        return new Expression.IntLiteral(0, "__capy_slice_end__", location);
    }

    private static Expression dataLiteral(dev.capylang.parser.antlr.FunctionalParser.NewDataContext ctx) {
        var typeName = ctx.BANG() == null
                ? ctx.type().getText()
                : "__capy_raw|" + ctx.type().getText();
        return new Expression.DataLiteral(
                typeName,
                dataFields(ctx.fieldAssignmentList()),
                location(ctx)
        );
    }

    private static Expression constructorDataLiteral(
            dev.capylang.parser.antlr.FunctionalParser.ConstructorDataContext ctx
    ) {
        return new Expression.DataLiteral(
                "*",
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
                positionalIndex++;
            } else if (assignment.spreadFieldAssignment() != null) {
                fields.add(new Expression.DataField(
                        "",
                        expression(assignment.spreadFieldAssignment().expression()),
                        true,
                        location(assignment)
                ));
            } else if (assignment.positionalFieldAssignment() != null) {
                fields.add(new Expression.DataField(
                        "$" + positionalIndex,
                        expression(assignment.positionalFieldAssignment().expression()),
                        false,
                        location(assignment)
                ));
                positionalIndex++;
            } else {
                fields.add(new Expression.DataField(
                        "$unsupported",
                        unsupported(assignment),
                        false,
                        location(assignment)
                ));
                positionalIndex++;
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
        return new Expression.DataField(name, expression(ctx.expression()), false, location(ctx));
    }

    private static Expression matchExpression(dev.capylang.parser.antlr.FunctionalParser.MatchExpressionContext ctx) {
        var cases = new ArrayList<Expression.MatchCase>();
        for (var caseList : ctx.matchCaseList()) {
            for (var matchCase : caseList.matchCase()) {
                cases.addAll(matchCases(matchCase));
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
                cases.addAll(matchCasesNoPipe(matchCase));
            }
        }
        return new Expression.MatchExpression(expressionNoPipe(ctx.expressionNoPipe()), List.copyOf(cases), location(ctx));
    }

    private static List<Expression.MatchCase> matchCases(dev.capylang.parser.antlr.FunctionalParser.MatchCaseContext ctx) {
        if (ctx.pattern().isEmpty()) {
            return List.of(matchCase(ctx, null));
        }
        var cases = new ArrayList<Expression.MatchCase>();
        for (var pattern : ctx.pattern()) {
            cases.add(matchCase(ctx, pattern));
        }
        return List.copyOf(cases);
    }

    private static Expression.MatchCase matchCase(
            dev.capylang.parser.antlr.FunctionalParser.MatchCaseContext ctx,
            dev.capylang.parser.antlr.FunctionalParser.PatternContext pattern
    ) {
        return new Expression.MatchCase(
                patternTypeName(pattern),
                patternBindings(pattern),
                patternBindsWholeValue(pattern),
                patternLiteral(pattern),
                patternHasLiteral(pattern),
                patternWildcard(pattern),
                ctx.guard == null ? unsupported(ctx) : expression(ctx.guard),
                ctx.guard != null,
                expression(ctx.body),
                location(ctx)
        );
    }

    private static List<Expression.MatchCase> matchCasesNoPipe(
            dev.capylang.parser.antlr.FunctionalParser.MatchCaseNoPipeContext ctx
    ) {
        if (ctx.pattern().isEmpty()) {
            return List.of(matchCaseNoPipe(ctx, null));
        }
        var cases = new ArrayList<Expression.MatchCase>();
        for (var pattern : ctx.pattern()) {
            cases.add(matchCaseNoPipe(ctx, pattern));
        }
        return List.copyOf(cases);
    }

    private static Expression.MatchCase matchCaseNoPipe(
            dev.capylang.parser.antlr.FunctionalParser.MatchCaseNoPipeContext ctx,
            dev.capylang.parser.antlr.FunctionalParser.PatternContext pattern
    ) {
        return new Expression.MatchCase(
                patternTypeName(pattern),
                patternBindings(pattern),
                patternBindsWholeValue(pattern),
                patternLiteral(pattern),
                patternHasLiteral(pattern),
                patternWildcard(pattern),
                ctx.guard == null ? unsupported(ctx) : expressionNoPipe(ctx.guard),
                ctx.guard != null,
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
        if (ctx.typedPattern() != null) {
            return ctx.typedPattern().patternType().getText();
        }
        if (ctx.TYPE() != null) {
            return ctx.TYPE().getText();
        }
        return "";
    }

    private static List<String> patternBindings(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        if (ctx == null) {
            return List.of();
        }
        if (ctx.typedPattern() != null && ctx.typedPattern().NAME() != null) {
            return List.of(ctx.typedPattern().NAME().getText());
        }
        if (ctx.wildcardPattern() != null && ctx.wildcardPattern().NAME() != null) {
            return List.of(ctx.wildcardPattern().NAME().getText());
        }
        if (ctx.constructorPattern() == null || ctx.constructorPattern().fieldPatternList() == null) {
            return List.of();
        }
        var bindings = new ArrayList<String>();
        var idx = 0;
        for (var fieldPattern : ctx.constructorPattern().fieldPatternList().pattern()) {
            if (fieldPattern.identifier() != null) {
                bindings.add(fieldPattern.identifier().getText());
            } else if (fieldPattern.wildcardPattern() != null && fieldPattern.wildcardPattern().NAME() != null) {
                bindings.add(fieldPattern.wildcardPattern().NAME().getText());
            } else if (fieldPattern.wildcardPattern() != null) {
                bindings.add("__capy_ignore_" + idx);
            }
            idx++;
        }
        return List.copyOf(bindings);
    }

    private static boolean patternWildcard(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        return ctx != null && ctx.wildcardPattern() != null;
    }

    private static boolean patternBindsWholeValue(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        return ctx != null && (ctx.typedPattern() != null || ctx.wildcardPattern() != null);
    }

    private static boolean patternHasLiteral(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        return ctx != null
                && (ctx.INT_LITERAL() != null
                || ctx.LONG_LITERAL() != null
                || ctx.BOOL_LITERAL() != null
                || ctx.STRING_LITERAL() != null
                || ctx.FLOAT_LITERAL() != null);
    }

    private static Expression patternLiteral(dev.capylang.parser.antlr.FunctionalParser.PatternContext ctx) {
        if (ctx == null) {
            return unsupportedLiteral();
        }
        var location = location(ctx);
        if (ctx.INT_LITERAL() != null) {
            var source = ctx.INT_LITERAL().getText();
            return new Expression.IntLiteral(Integer.parseInt(cleanNumber(source)), source, location);
        }
        if (ctx.LONG_LITERAL() != null) {
            var source = ctx.LONG_LITERAL().getText();
            return new Expression.LongLiteral(Long.parseLong(cleanNumber(source.substring(0, source.length() - 1))), source, location);
        }
        if (ctx.FLOAT_LITERAL() != null) {
            var source = ctx.FLOAT_LITERAL().getText();
            return new Expression.FloatLiteral(Float.parseFloat(cleanFloat(source)), source, location);
        }
        if (ctx.BOOL_LITERAL() != null) {
            var source = ctx.BOOL_LITERAL().getText();
            return new Expression.BoolLiteral(Boolean.parseBoolean(source), source, location);
        }
        if (ctx.STRING_LITERAL() != null) {
            return stringLiteralExpression(ctx.STRING_LITERAL().getText(), location);
        }
        return unsupportedLiteral();
    }

    private static Expression.UnsupportedExpression unsupportedLiteral() {
        return new Expression.UnsupportedExpression("", new SourceLocation(0, 0));
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
            return stringLiteralExpression(source, location(ctx));
        }
        if (literal.REGEX_LITERAL() != null) {
            return regexLiteralExpression(source, location(ctx));
        }
        return unsupported(ctx);
    }

    private static Expression regexLiteralExpression(String source, SourceLocation location) {
        var closingSlash = regexLiteralClosingSlash(source);
        if (closingSlash < "regex/".length()) {
            return new Expression.UnsupportedExpression(source, location);
        }
        var pattern = unescapeRegexContent(source.substring("regex/".length(), closingSlash));
        var flags = source.substring(closingSlash + 1);
        return new Expression.DataLiteral(
                "Regex",
                List.of(
                        new Expression.DataField(
                                "pattern",
                                new Expression.StringLiteral(pattern, quote(pattern), location),
                                false,
                                location
                        ),
                        new Expression.DataField(
                                "flags",
                                new Expression.StringLiteral(flags, quote(flags), location),
                                false,
                                location
                        )
                ),
                location
        );
    }

    private static int regexLiteralClosingSlash(String source) {
        var idx = source.length() - 1;
        while (idx >= 0 && "ims".indexOf(source.charAt(idx)) >= 0) {
            idx--;
        }
        return idx;
    }

    private static String unescapeRegexContent(String value) {
        var result = new StringBuilder();
        for (var idx = 0; idx < value.length(); idx++) {
            var current = value.charAt(idx);
            if (current == '\\' && idx + 1 < value.length()) {
                var next = value.charAt(idx + 1);
                if (next == '/' || next == '\\') {
                    result.append(next);
                    idx++;
                    continue;
                }
            }
            result.append(current);
        }
        return result.toString();
    }

    private static Expression stringLiteralExpression(String source, SourceLocation location) {
        if (!source.startsWith("\"")) {
            return new Expression.StringLiteral(unquote(source), source, location);
        }
        return interpolatedStringLiteralExpression(source, location);
    }

    private static Expression interpolatedStringLiteralExpression(String source, SourceLocation location) {
        var content = source.substring(1, source.length() - 1);
        var parts = new ArrayList<Expression>();
        var segment = new StringBuilder();
        var segmentStart = 0;
        var foundInterpolation = false;

        for (var i = 0; i < content.length(); ) {
            var current = content.charAt(i);
            if (current == '\\' && i + 1 < content.length()) {
                if (content.charAt(i + 1) == '{' && i + 2 < content.length() && content.charAt(i + 2) == '{') {
                    segment.append('{');
                    i += 2;
                } else {
                    segment.append(current);
                    segment.append(content.charAt(i + 1));
                    i += 2;
                }
                continue;
            }
            if (current == '{') {
                var end = interpolationEnd(content, i + 1);
                if (end >= 0 && !content.substring(i + 1, end).isBlank()) {
                    if (parts.isEmpty() && segment.isEmpty()) {
                        parts.add(stringSegment("", location, 0));
                    } else {
                        addStringSegment(parts, segment, location, segmentStart);
                    }
                    segment.setLength(0);
                    parts.add(interpolationExpression(
                            content.substring(i + 1, end),
                            stringContentLocation(location, i + 1)
                    ));
                    foundInterpolation = true;
                    i = end + 1;
                    segmentStart = i;
                    continue;
                }
            }
            segment.append(current);
            i++;
        }

        if (!foundInterpolation) {
            return new Expression.StringLiteral(unquote(source), source, location);
        }

        addStringSegment(parts, segment, location, segmentStart);
        return concatenated(parts, location);
    }

    private static int interpolationEnd(String content, int start) {
        var depth = 0;
        var quote = '\0';
        for (var i = start; i < content.length(); i++) {
            var current = content.charAt(i);
            if (current == '\\') {
                i++;
                continue;
            }
            if (quote != '\0') {
                if (current == quote) {
                    quote = '\0';
                }
                continue;
            }
            if (current == '"' || current == '\'') {
                quote = current;
            } else if (current == '{') {
                depth++;
            } else if (current == '}') {
                if (depth == 0) {
                    return i;
                }
                depth--;
            }
        }
        return -1;
    }

    private static void addStringSegment(
            List<Expression> parts,
            StringBuilder segment,
            SourceLocation literalLocation,
            int segmentStart
    ) {
        if (!segment.isEmpty()) {
            parts.add(stringSegment(segment.toString(), literalLocation, segmentStart));
        }
    }

    private static Expression.StringLiteral stringSegment(String rawSegment, SourceLocation literalLocation, int segmentStart) {
        var value = unescapeStringContent(rawSegment);
        return new Expression.StringLiteral(value, quote(value), stringContentLocation(literalLocation, segmentStart));
    }

    private static SourceLocation stringContentLocation(SourceLocation literalLocation, int contentOffset) {
        return new SourceLocation(literalLocation.line(), literalLocation.column() + 1 + contentOffset);
    }

    private static Expression concatenated(List<Expression> parts, SourceLocation location) {
        if (parts.isEmpty()) {
            return new Expression.StringLiteral("", "\"\"", location);
        }
        var result = parts.getFirst();
        for (var i = 1; i < parts.size(); i++) {
            result = new Expression.BinaryExpression("+", result, parts.get(i), location);
        }
        return result;
    }

    private static Expression interpolationExpression(String source, SourceLocation location) {
        var lexer = new dev.capylang.parser.antlr.FunctionalLexer(CharStreams.fromString(source));
        var tokens = new CommonTokenStream(lexer);
        var parser = new dev.capylang.parser.antlr.FunctionalParser(tokens);
        var errors = new ArrayList<SyntaxError>();
        var listener = errorListener(errors);

        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(listener);
        parser.addErrorListener(listener);

        var expression = parser.expression();
        if (tokens.LA(1) != Token.EOF) {
            errors.add(new SyntaxError(1, tokens.get(tokens.index()).getCharPositionInLine(), "extraneous input"));
        }
        if (!errors.isEmpty()) {
            return new Expression.UnsupportedExpression(source, location);
        }
        return offsetExpression(expression(expression), location.line() - 1, location.column());
    }

    private static Expression offsetExpression(Expression expression, int lineOffset, int columnOffset) {
        if (expression instanceof Expression.UnsupportedExpression value) {
            return new Expression.UnsupportedExpression(value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.IntLiteral value) {
            return new Expression.IntLiteral(value.value(), value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.LongLiteral value) {
            return new Expression.LongLiteral(value.value(), value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.FloatLiteral value) {
            return new Expression.FloatLiteral(value.value(), value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.DoubleLiteral value) {
            return new Expression.DoubleLiteral(value.value(), value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.BoolLiteral value) {
            return new Expression.BoolLiteral(value.value(), value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.StringLiteral value) {
            return new Expression.StringLiteral(value.value(), value.source(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.VariableExpression value) {
            return new Expression.VariableExpression(value.name(), offsetLocation(value.location(), lineOffset, columnOffset));
        }
        if (expression instanceof Expression.IfExpression value) {
            return new Expression.IfExpression(
                    offsetExpression(value.condition(), lineOffset, columnOffset),
                    offsetExpression(value.thenBranch(), lineOffset, columnOffset),
                    offsetExpression(value.elseBranch(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.BinaryExpression value) {
            return new Expression.BinaryExpression(
                    value.operator(),
                    offsetExpression(value.left(), lineOffset, columnOffset),
                    offsetExpression(value.right(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.UnaryExpression value) {
            return new Expression.UnaryExpression(
                    value.operator(),
                    offsetExpression(value.expression(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.FunctionCallExpression value) {
            return new Expression.FunctionCallExpression(
                    value.name(),
                    offsetExpressions(value.arguments(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.FunctionReferenceExpression value) {
            return new Expression.FunctionReferenceExpression(
                    value.name(),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.MethodCallExpression value) {
            return new Expression.MethodCallExpression(
                    offsetExpression(value.receiver(), lineOffset, columnOffset),
                    value.name(),
                    offsetExpressions(value.arguments(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.WithExpression value) {
            return new Expression.WithExpression(
                    offsetExpression(value.receiver(), lineOffset, columnOffset),
                    offsetDataFields(value.fields(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.LambdaExpression value) {
            return new Expression.LambdaExpression(
                    value.parameters(),
                    offsetExpression(value.body(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.ListLiteral value) {
            return new Expression.ListLiteral(
                    offsetExpressions(value.values(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.SetLiteral value) {
            return new Expression.SetLiteral(
                    offsetExpressions(value.values(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.DictLiteral value) {
            return new Expression.DictLiteral(
                    offsetDictEntries(value.entries(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.TupleLiteral value) {
            return new Expression.TupleLiteral(
                    offsetExpressions(value.values(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.IndexExpression value) {
            return new Expression.IndexExpression(
                    offsetExpression(value.receiver(), lineOffset, columnOffset),
                    offsetExpression(value.index(), lineOffset, columnOffset),
                    offsetExpression(value.endIndex(), lineOffset, columnOffset),
                    value.hasEndIndex(),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.DataLiteral value) {
            return new Expression.DataLiteral(
                    value.typeName(),
                    offsetDataFields(value.fields(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.FieldAccessExpression value) {
            return new Expression.FieldAccessExpression(
                    offsetExpression(value.receiver(), lineOffset, columnOffset),
                    value.name(),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.ReduceExpression value) {
            return new Expression.ReduceExpression(
                    offsetExpression(value.receiver(), lineOffset, columnOffset),
                    offsetExpression(value.initial(), lineOffset, columnOffset),
                    value.accumulatorName(),
                    value.keyName(),
                    value.valueName(),
                    offsetExpression(value.body(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.MatchExpression value) {
            return new Expression.MatchExpression(
                    offsetExpression(value.value(), lineOffset, columnOffset),
                    offsetMatchCases(value.cases(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        if (expression instanceof Expression.BlockExpression value) {
            return new Expression.BlockExpression(
                    offsetLetBindings(value.bindings(), lineOffset, columnOffset),
                    offsetExpression(value.result(), lineOffset, columnOffset),
                    offsetLocation(value.location(), lineOffset, columnOffset)
            );
        }
        return expression;
    }

    private static List<Expression> offsetExpressions(List<Expression> expressions, int lineOffset, int columnOffset) {
        return expressions.stream()
                .map(expression -> offsetExpression(expression, lineOffset, columnOffset))
                .toList();
    }

    private static List<Expression.DictEntry> offsetDictEntries(
            List<Expression.DictEntry> entries,
            int lineOffset,
            int columnOffset
    ) {
        return entries.stream()
                .map(entry -> new Expression.DictEntry(
                        offsetExpression(entry.key(), lineOffset, columnOffset),
                        offsetExpression(entry.value(), lineOffset, columnOffset),
                        offsetLocation(entry.location(), lineOffset, columnOffset)
                ))
                .toList();
    }

    private static List<Expression.DataField> offsetDataFields(
            List<Expression.DataField> fields,
            int lineOffset,
            int columnOffset
    ) {
        return fields.stream()
                .map(field -> new Expression.DataField(
                        field.name(),
                        offsetExpression(field.value(), lineOffset, columnOffset),
                        field.spread(),
                        offsetLocation(field.location(), lineOffset, columnOffset)
                ))
                .toList();
    }

    private static List<Expression.MatchCase> offsetMatchCases(
            List<Expression.MatchCase> cases,
            int lineOffset,
            int columnOffset
    ) {
        return cases.stream()
                .map(matchCase -> new Expression.MatchCase(
                        matchCase.typeName(),
                        matchCase.bindings(),
                        matchCase.bindsWholeValue(),
                        offsetExpression(matchCase.literal(), lineOffset, columnOffset),
                        matchCase.hasLiteral(),
                        matchCase.wildcard(),
                        offsetExpression(matchCase.guard(), lineOffset, columnOffset),
                        matchCase.hasGuard(),
                        offsetExpression(matchCase.body(), lineOffset, columnOffset),
                        offsetLocation(matchCase.location(), lineOffset, columnOffset)
                ))
                .toList();
    }

    private static List<Expression.LetBinding> offsetLetBindings(
            List<Expression.LetBinding> bindings,
            int lineOffset,
            int columnOffset
    ) {
        return bindings.stream()
                .map(binding -> new Expression.LetBinding(
                        binding.name(),
                        binding.typeReference(),
                        binding.operator(),
                        offsetExpression(binding.value(), lineOffset, columnOffset),
                        offsetLocation(binding.location(), lineOffset, columnOffset)
                ))
                .toList();
    }

    private static SourceLocation offsetLocation(SourceLocation location, int lineOffset, int columnOffset) {
        var line = location.line() + lineOffset;
        var column = location.line() == 1 ? location.column() + columnOffset : location.column();
        return new SourceLocation(line, column);
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

    private static boolean isInvocationExpression(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext ctx) {
        return ctx.argumentList() != null && ctx.expressionNoLet().size() == 1 && hasChild(ctx, "(");
    }

    private static boolean isSliceExpression(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetContext ctx) {
        return ctx.expressionNoLet().size() == 1 && hasChild(ctx, "[") && hasChild(ctx, ":");
    }

    private static boolean isSliceExpressionNoPipe(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetNoPipeContext ctx) {
        return ctx.expressionNoLetNoPipe().size() == 1 && hasChild(ctx, "[") && hasChild(ctx, ":");
    }

    private static boolean isInvocationExpressionNoPipe(dev.capylang.parser.antlr.FunctionalParser.ExpressionNoLetNoPipeContext ctx) {
        return ctx.argumentList() != null && ctx.expressionNoLetNoPipe().size() == 1 && hasChild(ctx, "(");
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

    private static SourceLocation location(Token token) {
        return new SourceLocation(token.getLine(), token.getCharPositionInLine());
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
        return unescapeStringContent(value.substring(1, value.length() - 1));
    }

    private static String unescapeStringContent(String value) {
        return value
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
