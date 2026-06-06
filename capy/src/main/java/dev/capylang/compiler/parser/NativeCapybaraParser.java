package dev.capylang.compiler.parser;

import dev.capylang.NativeImplementation;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

@NativeImplementation
public final class NativeCapybaraParser implements CapybaraParser {
    private static final String MODULE_NAME_PATTERN =
            "[A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][A-Za-z0-9_]*(?:/[A-Za-z_][A-Za-z0-9_]*)+";
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
        switch (module.sourceKind()) {
            case FUNCTIONAL -> parseFunctional(module, source.body());
            case OBJECT_ORIENTED -> parseObjectOriented(module, source.body());
        }
        return new ParsedModule(
                module.name(),
                module.path(),
                new Functional(functionalDefinitions(source.body())),
                source.imports(),
                module.sourceKind()
        );
    }

    private void parseFunctional(RawModule module, String source) {
        var lexer = new dev.capylang.parser.antlr.FunctionalLexer(CharStreams.fromString(source));
        var tokens = new CommonTokenStream(lexer);
        var parser = new dev.capylang.parser.antlr.FunctionalParser(tokens);
        var errors = new ArrayList<SyntaxError>();
        var listener = errorListener(errors);

        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(listener);
        parser.addErrorListener(listener);

        parser.program();
        throwIfInvalid(module, errors);
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

        for (var line : source.split("\\R", -1)) {
            if (FROM_IMPORT_PATTERN.matcher(line).matches() || QUALIFIED_IMPORT_PATTERN.matcher(line).matches()) {
                imports.add(ImportDeclaration.INSTANCE);
                body.add("");
            } else {
                body.add(line);
            }
        }
        return new ParsedSource(String.join(System.lineSeparator(), body), List.copyOf(imports));
    }

    private static List<Definition> functionalDefinitions(String source) {
        var lexer = new dev.capylang.parser.antlr.FunctionalLexer(CharStreams.fromString(source));
        var tokens = new CommonTokenStream(lexer);
        var parser = new dev.capylang.parser.antlr.FunctionalParser(tokens);
        var definitions = new ArrayList<Definition>();

        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        var program = parser.program();
        for (var definition : program.definition()) {
            definitions.add(functionalDefinition(definition));
        }
        return List.copyOf(definitions);
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
        if (definition.functionDeclaration() != null || definition.constDeclaration() != null) {
            return Definition.Function.INSTANCE;
        }
        if (definition.primitiveBackedTypeDeclaration() != null) {
            return Definition.PrimitiveBackedTypeDeclaration.INSTANCE;
        }
        if (definition.typeDeclaration() != null) {
            return Definition.TypeDeclaration.INSTANCE;
        }
        throw new IllegalArgumentException("ParserError: unsupported functional definition: " + definition.getText());
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
