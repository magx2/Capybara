package pl.grzeslowski.capybara.compiler;

import pl.grzeslowski.capybara.Main;
import pl.grzeslowski.capybara.generator.CompiledModule;
import pl.grzeslowski.capybara.generator.JavaGenerator;
import pl.grzeslowski.capybara.generator.JavaScriptGenerator;
import pl.grzeslowski.capybara.generator.PythonGenerator;
import pl.grzeslowski.capybara.linker.CapybaraLinker;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;

import static java.nio.file.Files.isRegularFile;
import static java.util.stream.Collectors.joining;

public class Compiler {
    public static final Compiler INSTANCE = new Compiler();
    private static final Logger log = Logger.getLogger(Compiler.class.getName());

    public void compile(Main.Arguments args) throws IOException {
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
        var linkedProgram = CapybaraLinker.INSTANCE.link(program);

        // 3. Optimize the generated code if necessary
        // TODO implement optimizations

        // 4. Generate bytecode or machine code from the AST
        var generator = switch (args.outputType()) {
            case JAVA -> new JavaGenerator();
            case JAVASCRIPT -> new JavaScriptGenerator();
            case PYTHON -> new PythonGenerator();
        };
        log.info("Using generator: " + generator.getClass().getSimpleName());
        var compiledProgram = generator.generate(linkedProgram);

        // 5. Write the generated code to the output directory
        log.info("Writing compiled program to output directory: " + args.output());
        compiledProgram.modules()
                .forEach(module -> write(args.output(), module));
    }

    private void write(Path output, CompiledModule module) {
        var path = output
                .resolve(module.path())
                .resolve(module.name());
        try {
            log.info("Writing module: " + module.name() + " to file: " + path);
            Files.write(path, module.code().getBytes());
        } catch (IOException e) {
            throw new UncheckedIOException("Unable to write file: " + path, e);
        }
    }

    private Module buildModule(SourceFile sourceFile) {
        log.info("Building module from file: " + sourceFile.path);
        var fileName = sourceFile.path.getFileName().toString();
        var fileNameWithoutExtension = fileName.substring(0, fileName.lastIndexOf('.'));
        return new Module(
                fileNameWithoutExtension,
                findModulePath(sourceFile),
                CapybaraParser.INSTANCE.parseFunctional(readFile(sourceFile.path)));
    }

    private static String findModulePath(SourceFile sourceFile) {
        var relativePath = sourceFile.rootPath.relativize(sourceFile.path);
        return relativePath.toString().substring(0, relativePath.toString().lastIndexOf('.'));
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
            throw new UncheckedIOException("Unable to list files in directory: " + directory, e);
        }
    }

    private record SourceFile(Path rootPath, Path path) {
    }
}
