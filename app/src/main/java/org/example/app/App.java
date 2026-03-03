package org.example.app;

import pl.grzeslowski.capybara.compiler.Arguments;
import pl.grzeslowski.capybara.compiler.Compiler;
import pl.grzeslowski.capybara.compiler.OutputType;

import java.io.IOException;
import java.nio.file.Path;

public class App {
    public static void main(String[] args) throws IOException {
        Compiler.INSTANCE.compile(parseArguments(args));
    }

    private static Arguments parseArguments(String[] args) {
        if (args.length < 3) {
            throw new IllegalArgumentException(
                    "Usage: java -jar capybara.jar {JAVA,PYTHON,JAVASCRIPT} <input1> <input2> ... <output>"
            );
        }
        var outputType = OutputType.valueOf(args[0].toUpperCase());
        var inputs = java.util.Arrays.stream(args, 1, args.length - 1)
                .map(Path::of)
                .toList();
        var output = Path.of(args[args.length - 1]);
        return new Arguments(inputs, output, outputType);
    }
}
