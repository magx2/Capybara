package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;
import java.util.TreeSet;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;

class ProgramMainSourceGeneratorTest {
    @Test
    void shouldGenerateJavaSourceForOnlyEffectProgramMainEntrypoint() {
        var generated = new JavaGenerator().generate(programWithEffectMainAndDirectProgramMain());

        var main = generatedCode(generated, Path.of("foo", "Main.java"));
        assertThat(main)
                .contains("public static capy.lang.Effect<Program> main(java.util.List<java.lang.String> args)")
                .containsSubsequence(
                        "public static final void main(String... args)",
                        "var __capybaraArgsList = java.util.List.of(args);",
                        "Program __capybaraProgram = main(__capybaraArgsList).unsafeRun();",
                        "if (__capybaraProgram instanceof Program.Failed __capybaraFailed)",
                        "System.exit(__capybaraFailed.exit_code());"
                )
                .doesNotContain("results")
                .doesNotContain("exitCode")
                .doesNotContain("forEach(System.out::println)");

        var directProgramMain = generatedCode(generated, Path.of("foo", "SecondaryMain.java"));
        assertThat(directProgramMain)
                .contains("public static capy.lang.Program main(java.util.List<java.lang.String> args)")
                .doesNotContain("public static final void main(String... args)")
                .doesNotContain("unsafeRun()")
                .doesNotContain("System.exit(");
    }

    @Test
    void shouldGenerateJavaScriptSourceForOnlyEffectProgramMainEntrypoint() {
        var generated = new JavaScriptGenerator().generate(programWithEffectMainAndDirectProgramMain());

        var main = generatedCode(generated, Path.of("foo", "Main.js"));
        assertThat(main)
                .contains("function main(args)")
                .containsSubsequence(
                        "if (require.main === module)",
                        "const value = main(process.argv.slice(2)).unsafe_run();",
                        "capy.writeProgramResult(value);"
                )
                .doesNotContain("capy.isEffect(result)")
                .doesNotContain("value !== undefined")
                .doesNotContain("console.log");

        var directProgramMain = generatedCode(generated, Path.of("foo", "SecondaryMain.js"));
        assertThat(directProgramMain)
                .contains("function main(args)")
                .doesNotContain("if (require.main === module)")
                .doesNotContain("unsafe_run()")
                .doesNotContain("capy.writeProgramResult");
    }

    @Test
    void shouldGeneratePythonSourceForOnlyEffectProgramMainEntrypoint() {
        var generated = new PythonGenerator().generate(programWithEffectMainAndDirectProgramMain());

        var main = generatedCode(generated, Path.of("foo", "Main.py"));
        assertThat(main)
                .contains("def main(args):")
                .containsSubsequence(
                        "if __name__ == '__main__':",
                        "value = main(__capy_sys.argv[1:]).unsafe_run()",
                        "capy.write_program_result(value)"
                )
                .doesNotContain("capy.is_effect")
                .doesNotContain("if value is not None")
                .doesNotContain("print(");

        var directProgramMain = generatedCode(generated, Path.of("foo", "SecondaryMain.py"));
        assertThat(directProgramMain)
                .contains("def main(args):")
                .doesNotContain("if __name__ == '__main__':")
                .doesNotContain("unsafe_run()")
                .doesNotContain("capy.write_program_result");
    }

    private static CompiledProgram programWithEffectMainAndDirectProgramMain() {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(
                        new RawModule("Main", "/foo", """
                                from /capy/lang/Effect import { * }
                                from /capy/lang/Program import { * }
                                from /capy/collection/List import { * }

                                fun main(args: List[String]): Effect[Program] =
                                    if args.size() > 0
                                    then pure(Success {})
                                    else pure(Failed { exit_code: DEFAULT_FAILED_EXIT_CODE })
                                """),
                        new RawModule("SecondaryMain", "/foo", """
                                from /capy/collection/List import { * }

                                fun main(args: List[String]): /capy/lang/Program =
                                    /capy/lang/Program.Success {}
                                """)
                ),
                new TreeSet<>()
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().stream()
                    .map(Result.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static String generatedCode(GeneratedProgram generated, Path relativePath) {
        return generated.modules().stream()
                .filter(module -> module.relativePath().equals(relativePath))
                .map(GeneratedModule::code)
                .findFirst()
                .orElseThrow();
    }
}
