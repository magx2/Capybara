package pl.grzeslowski.capybara.java;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import pl.grzeslowski.capybara.compiler.Compiler;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.Files.writeString;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static pl.grzeslowski.capybara.compiler.OutputType.JAVA;

public class JavaCompilatorTest {
    Compiler compiler = new Compiler();

    @ParameterizedTest(name = "{index}: should {0}")
    @MethodSource
    void compile(String code) throws IOException {
        // given
        var inputDir = Files.createTempDirectory("capybara-java-test");
        inputDir.toFile().deleteOnExit();

        {
            var packagePath = inputDir
                    .resolve("pl")
                    .resolve("grzeslowski")
                    .resolve("capybara")
                    .resolve("java")
                    .resolve("test")
                    .resolve("code");
            Files.createDirectories(packagePath);
            var input = packagePath.resolve("Main.cfun");
            writeString(input, code);
        }

        var output = Path.of("build/test-output/java");
        Files.createDirectories(output);

        var args = new pl.grzeslowski.capybara.compiler.Arguments(List.of(inputDir), output, JAVA);

        // when
        compiler.compile(args);

        // then
        System.out.println("Generated Java code:");
        List<Path> generatedFiles;
        try (var stream = Files.walk(output)) {
            generatedFiles = stream
                    .filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".java"))
                    .toList();
        }
        assertFalse(generatedFiles.isEmpty(), "No Java file generated.");
        for (var generatedFile : generatedFiles) {
            System.out.println("File: " + output.relativize(generatedFile));
            var generatedCode = Files.readString(generatedFile, UTF_8);
            System.out.println(generatedCode);
        }
    }

    static Stream<Arguments> compile() {
        return Stream.of(
                Arguments.of("""
                                // algebraic type
                                type Shape = Circle | Rectangle
                                data Circle { radius: float }
                                data Rectangle { width: float, height: float }
                                
                                fun area(shape: Shape): float =
                                    match shape with
                                    | Circle { radius } => 3.14 * radius ^ 2
                                    | Rectangle { width, height } => width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    | Circle { radius } => Rectangle { width : radius * 2, height : radius * 2 }
                                    | Rectangle { width, height } => Circle { radius : (width + height) / 4 }
                                
                                // type with common value
                                type Person { name: string, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: string }
                                """,
                        """
                                fun classify(x: int): string =
                                    if x > 0 then "positive"
                                    else "non-positive"
                                fun always_true(): bool = true
                                
                                // algebraic type
                                type Shape = Circle | Rectangle
                                data Circle { radius: float }
                                data Rectangle { width: float, height: float }
                                
                                fun area(shape: Shape): float =
                                    match shape with
                                    | Circle { radius } => 3.14 * radius * radius
                                    | Rectangle { width, height } => width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    | Circle { radius } => Rectangle { width = radius * 2, height = radius * 2 }
                                    | Rectangle { width, height } => Circle { radius = (width + height) / 4 }
                                """,
                        """
                                // algebraic type
                                type Shape = Circle | Rectangle
                                data Circle { radius: float }
                                data Rectangle { width: float, height: float }
                                
                                fun area(shape: Shape): float =
                                    match shape with
                                    | Circle { radius } => 3.14 * radius ^ 2
                                    | Rectangle { width, height } => width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    | Circle { radius } => Rectangle { width : radius * 2, height : radius * 2 }
                                    | Rectangle { width, height } => Circle { radius : (width + height) / 4 }
                                """,
                        """
                                // type with common value
                                type Person { name: string, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: string }
                                """)
        );
    }
}
