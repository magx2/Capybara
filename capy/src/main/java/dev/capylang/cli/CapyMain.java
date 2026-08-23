package dev.capylang.cli;

import dev.capylang.compiler.BackendCompilationContext;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Optional;

/** Executable launcher that reports the version packaged by the current build. */
public final class CapyMain {
    private CapyMain() {
    }

    public static void main(String... arguments) {
        try {
            run(arguments);
        } catch (IllegalArgumentException exception) {
            System.err.println(exception.getMessage());
            System.exit(1);
        }
    }

    private static void run(String... arguments) {
        if (arguments.length == 1 && arguments[0].equals("--version")) {
            System.out.println("Capybara compiler version: " + packagedVersion());
            return;
        }
        var sharedOutputs = sharedInputOutputs(arguments);
        var compilerArguments = sharedOutputs
                .map(ignored -> withoutOptions(arguments, "--test-input", "--test-output"))
                .orElse(arguments);
        BackendCompilationContext.withOutputType(outputType(arguments), () -> Capy.main(compilerArguments));
        sharedOutputs.ifPresent(CapyMain::copyGeneratedOutput);
    }

    static Optional<SharedInputOutputs> sharedInputOutputs(String[] arguments) {
        if (arguments.length < 2 || !arguments[0].equals("compile-generate")) {
            return Optional.empty();
        }
        var input = optionValue(arguments, "--input");
        var testInput = optionValue(arguments, "--test-input");
        var output = optionValue(arguments, "--output");
        var testOutput = optionValue(arguments, "--test-output");
        if (input.isEmpty() || testInput.isEmpty() || output.isEmpty() || testOutput.isEmpty()) {
            return Optional.empty();
        }
        var inputPath = Path.of(input.get()).toAbsolutePath().normalize();
        var testInputPath = Path.of(testInput.get()).toAbsolutePath().normalize();
        if (!inputPath.equals(testInputPath)) {
            return Optional.empty();
        }
        var outputPath = Path.of(output.get()).toAbsolutePath().normalize();
        var testOutputPath = Path.of(testOutput.get()).toAbsolutePath().normalize();
        if (Files.exists(testOutputPath) && !Files.isDirectory(testOutputPath)) {
            return Optional.empty();
        }
        return Optional.of(new SharedInputOutputs(outputPath, testOutputPath));
    }

    private static Optional<String> optionValue(String[] arguments, String option) {
        for (var index = 0; index + 1 < arguments.length; index++) {
            if (arguments[index].equals(option)) {
                return Optional.of(arguments[index + 1]);
            }
        }
        return Optional.empty();
    }

    private static String[] withoutOptions(String[] arguments, String... omittedOptions) {
        var result = new ArrayList<String>();
        for (var index = 0; index < arguments.length; index++) {
            var omit = false;
            for (var option : omittedOptions) {
                if (arguments[index].equals(option)) {
                    omit = true;
                    break;
                }
            }
            if (omit) {
                index++;
            } else {
                result.add(arguments[index]);
            }
        }
        return result.toArray(String[]::new);
    }

    private static void copyGeneratedOutput(SharedInputOutputs outputs) {
        if (outputs.output().equals(outputs.testOutput())) {
            return;
        }
        try {
            if (Files.exists(outputs.testOutput()) && !Files.isDirectory(outputs.testOutput())) {
                throw new IllegalStateException("Test output path is not a directory: " + outputs.testOutput());
            }
            if (Files.exists(outputs.testOutput())) {
                try (var paths = Files.walk(outputs.testOutput())) {
                    for (var path : paths.sorted(Comparator.reverseOrder()).toList()) {
                        Files.delete(path);
                    }
                }
            }
            try (var paths = Files.walk(outputs.output())) {
                for (var source : paths.toList()) {
                    var target = outputs.testOutput().resolve(outputs.output().relativize(source));
                    if (Files.isDirectory(source)) {
                        Files.createDirectories(target);
                    } else {
                        Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                    }
                }
            }
        } catch (IOException exception) {
            throw new IllegalStateException("Unable to copy generated main output to the matching test output.", exception);
        }
    }

    private static String outputType(String[] arguments) {
        if (arguments.length >= 2 && arguments[0].equals("compile-generate")) {
            return arguments[1];
        }
        return "";
    }

    private static String packagedVersion() {
        try (var input = CapyMain.class.getResourceAsStream("/capybara-version.txt")) {
            if (input == null) {
                throw new IllegalStateException("Missing capybara-version.txt");
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8).trim();
        } catch (IOException exception) {
            throw new IllegalStateException("Unable to read packaged Capybara version", exception);
        }
    }

    record SharedInputOutputs(Path output, Path testOutput) {
    }
}
