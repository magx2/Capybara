package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.generator.CompiledModule;
import pl.grzeslowski.capybara.generator.Generator;
import pl.grzeslowski.capybara.linker.CapybaraLinker;
import pl.grzeslowski.capybara.linker.LinkedProgram;
import pl.grzeslowski.capybara.linker.ValueOrError;
import pl.grzeslowski.capybara.parser.CapybaraParser;
import pl.grzeslowski.capybara.parser.DataDeclaration;
import pl.grzeslowski.capybara.parser.Definition;
import pl.grzeslowski.capybara.parser.Function;
import pl.grzeslowski.capybara.parser.SingleDeclaration;
import pl.grzeslowski.capybara.parser.TypeDeclaration;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import static java.nio.file.Files.isRegularFile;
import static java.util.stream.Collectors.joining;

public class Compiler {
    public static final Compiler INSTANCE = new Compiler();
    private static final Logger log = Logger.getLogger(Compiler.class.getName());
    private static final Pattern IMPORT_PATTERN = Pattern.compile(
            "^\\s*from\\s+([A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][A-Za-z0-9_]*(?:/[A-Za-z_][A-Za-z0-9_]*)+)\\s+import\\s*\\{\\s*([^}]*)\\s*}(?:\\s+except\\s*\\{\\s*([^}]*)\\s*})?\\s*$"
    );

    public void compile(Arguments args) throws IOException {
        if (Files.notExists(args.output())) {
            log.info("Creating output directory: " + args.output());
            Files.createDirectories(args.output());
        } else if (!Files.isDirectory(args.output())) {
            throw new IllegalArgumentException("Output path is not a directory: " + args.output());
        } else {
            log.info("Using existing output directory: " + args.output());
        }
        for (var input : args.inputs()) {
            if (!input.toFile().exists()) {
                throw new IllegalArgumentException("Input file does not exist: " + input);
            }
            if (!Files.isDirectory(input)) {
                throw new IllegalArgumentException("Input path is not a directory: " + input);
            }
        }
        log.info("Compiling files: " + args.inputs().stream().map(f -> f.toFile().getAbsolutePath()).collect(joining(", ")));

        // 1. Parse the source code into an AST (Abstract Syntax Tree)
        var modules = args.inputs()
                .stream()
                .map(this::listSourceFiles)
                .flatMap(Collection::stream)
                .filter(sf -> isRegularFile(sf.path))
                .filter(sf -> sf.path.getFileName().toString().endsWith(".cfun"))
                .map(this::buildModule)
                .toList();
        var program = new Program(modules);

        // 2. Perform semantic analysis on the AST (e.g., type checking, scope resolution)
        var linkingResult = CapybaraLinker.INSTANCE.link(program);
        if (linkingResult instanceof ValueOrError.Error<?> le) {
            log.severe("Linking failed with " + le.errors().size() + " error(s):");
            le.errors().forEach(error -> log.severe(error.toString()));
            return;
        }
        var linkedProgram = ((ValueOrError.Value<LinkedProgram>) linkingResult).value();

        // 3. Optimize the generated code if necessary
        // TODO implement optimizations

        // 4. Generate bytecode or machine code from the AST
        var generator = Generator.findGenerator(args.outputType());
        log.info("Using generator: " + generator.getClass().getSimpleName());
        var compiledProgram = generator.generate(linkedProgram);

        // 5. Write the generated code to the output directory
        log.info("Writing compiled program to output directory: " + args.output());
        compiledProgram.modules()
                .forEach(module -> write(args.output(), module));
    }

    private void write(Path output, CompiledModule module) {
        var absolutePath = output.resolve(module.relativePath());
        try {
            var parent = absolutePath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            log.info("Writing module to file: " + absolutePath);
            Files.writeString(
                    absolutePath,
                    module.code(),
                    StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
            );
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write file: " + absolutePath, e);
        }
    }

    private Module buildModule(SourceFile sourceFile) {
        log.info("Building module from file: " + sourceFile.path);
        var fileName = sourceFile.path.getFileName().toString();
        var fileNameWithoutExtension = fileName.substring(0, fileName.lastIndexOf('.'));
        var parsedSource = parseSource(readFile(sourceFile.path));
        return new Module(
                fileNameWithoutExtension,
                findModulePath(sourceFile),
                CapybaraParser.INSTANCE.parseFunctional(parsedSource.source()),
                parsedSource.imports()
        );
    }

    private List<Module> resolveImports(List<Module> modules) {
        var modulesByName = new HashMap<String, Module>();
        for (var module : modules) {
            var previous = modulesByName.putIfAbsent(module.name(), module);
            if (previous != null) {
                throw new IllegalArgumentException("Duplicate module name `" + module.name() + "` for import resolution");
            }
        }

        var resolved = new HashMap<String, Module>();
        var visiting = new HashSet<String>();
        for (var module : modules) {
            resolveModule(module, modulesByName, resolved, visiting);
        }
        return modules.stream().map(module -> resolved.get(module.name())).toList();
    }

    private Module resolveModule(
            Module module,
            Map<String, Module> modulesByName,
            Map<String, Module> resolved,
            Set<String> visiting
    ) {
        var cached = resolved.get(module.name());
        if (cached != null) {
            return cached;
        }
        if (!visiting.add(module.name())) {
            throw new IllegalArgumentException("Circular imports detected for module `" + module.name() + "`");
        }

        var mergedDefinitions = new ArrayList<Definition>(module.functional().definitions());

        for (var importDeclaration : module.imports()) {
            var importedModule = modulesByName.get(importDeclaration.moduleName());
            if (importedModule == null) {
                throw new IllegalArgumentException(
                        "Module `" + module.name() + "` imports unknown module `" + importDeclaration.moduleName() + "`"
                );
            }
            var resolvedImportedModule = resolveModule(importedModule, modulesByName, resolved, visiting);
            var importedBySymbol = resolvedImportedModule.functional().definitions().stream()
                    .collect(java.util.stream.Collectors.toMap(this::definitionSymbol, java.util.function.Function.identity(), (first, second) -> first));
            for (var excludedSymbol : importDeclaration.excludedSymbols()) {
                if (!importedBySymbol.containsKey(excludedSymbol)) {
                    throw new IllegalArgumentException(
                            "Module `" + module.name() + "` excludes unknown symbol `" + excludedSymbol
                            + "` from module `" + importDeclaration.moduleName() + "`"
                    );
                }
            }
            var selectedSymbols = importDeclaration.selectedSymbols(importedBySymbol.keySet());
            if (!importDeclaration.isStarImport()) {
                for (var symbol : importDeclaration.symbols()) {
                    if (!importedBySymbol.containsKey(symbol)) {
                        throw new IllegalArgumentException(
                                "Module `" + module.name() + "` imports unknown symbol `" + symbol
                                + "` from module `" + importDeclaration.moduleName() + "`"
                        );
                    }
                }
            }
            for (var symbol : selectedSymbols) {
                var importedDefinition = importedBySymbol.get(symbol);
                if (importedDefinition == null) {
                    throw new IllegalArgumentException(
                            "Module `" + module.name() + "` imports unknown symbol `" + symbol
                            + "` from module `" + importDeclaration.moduleName() + "`"
                    );
                }
                var localHasSymbol = mergedDefinitions.stream()
                        .anyMatch(definition -> definitionMatchesSymbol(definition, symbol));
                if (localHasSymbol) {
                    continue;
                }
                if (!mergedDefinitions.contains(importedDefinition)) {
                    mergedDefinitions.add(importedDefinition);
                }
            }
        }

        visiting.remove(module.name());
        var mergedModule = new Module(
                module.name(),
                module.path(),
                new pl.grzeslowski.capybara.parser.Functional(Set.copyOf(mergedDefinitions)),
                List.of()
        );
        resolved.put(module.name(), mergedModule);
        return mergedModule;
    }

    private String definitionSymbol(Definition definition) {
        return switch (definition) {
            case Function function -> function.name();
            case TypeDeclaration typeDeclaration -> typeDeclaration.name();
            case DataDeclaration dataDeclaration -> dataDeclaration.name();
            case SingleDeclaration singleDeclaration -> singleDeclaration.name();
        };
    }

    private boolean definitionMatchesSymbol(Definition definition, String symbol) {
        return definitionSymbol(definition).equals(symbol);
    }

    private static String findModulePath(SourceFile sourceFile) {
        var relativePath = sourceFile.rootPath.relativize(sourceFile.path).getParent();
        return relativePath.toString();
    }

    private String readFile(Path path) {
        try {
            return Files.readString(path);
        } catch (IOException e) {
            throw new UnsupportedOperationException("Unable to read file: " + path, e);
        }
    }

    private List<SourceFile> listSourceFiles(Path directory) {
        try (var stream = Files.walk(directory)) {
            return stream
                    .map(x -> new SourceFile(directory, x))
                    // have to do toList to force the stream read files
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to list files rest directory: " + directory, e);
        }
    }

    private record SourceFile(Path rootPath, Path path) {
    }

    private ParsedSource parseSource(String source) {
        var imports = new ArrayList<ImportDeclaration>();
        var bodyLines = new ArrayList<String>();
        source.lines().forEach(line -> {
            var matcher = IMPORT_PATTERN.matcher(line);
            if (matcher.matches()) {
                var module = matcher.group(1);
                var symbols = List.of(matcher.group(2).split(","))
                        .stream()
                        .map(String::trim)
                        .filter(symbol -> !symbol.isBlank())
                        .toList();
                var excludedSymbols = matcher.group(3) == null
                        ? List.<String>of()
                        : List.of(matcher.group(3).split(","))
                                .stream()
                                .map(String::trim)
                                .filter(symbol -> !symbol.isBlank())
                                .toList();
                imports.add(new ImportDeclaration(module, symbols, excludedSymbols));
            } else {
                bodyLines.add(line);
            }
        });
        return new ParsedSource(String.join(System.lineSeparator(), bodyLines), List.copyOf(imports));
    }

    private record ParsedSource(String source, List<ImportDeclaration> imports) {
    }
}
