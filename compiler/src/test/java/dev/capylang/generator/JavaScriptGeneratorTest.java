package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.TreeSet;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class JavaScriptGeneratorTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldGenerateCommonJsAndRunFunctionalModule() throws Exception {
        var program = compileProgram("""
                from /capy/lang/Option import { * }
                from /capy/collection/List import { * }

                data User { name: String }

                fun greet(): String =
                    match User { "Ada" } with
                    case User { name } -> name

                fun maybe(flag: bool): Option[String] =
                    if flag then Some { "ok" } else None {}

                fun read(flag: bool): String =
                    match maybe(flag) with
                    case Some { value } -> value
                    case None -> "none"

                fun total(): int = [1, 2, 3].reduce(0, (acc, value) => acc + value)
                """);

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(
                        Path.of("foo", "Main.js"),
                        Path.of("dev", "capylang", "capybara.js"),
                        Path.of("capy", "lang", "Option.js")
                );

        var output = runNode("""
                const m = require('./foo/Main.js');
                console.log([m.greet(), m.read(true), m.read(false), m.total()].join('|'));
                """);

        assertThat(output).isEqualTo("Ada|ok|none|6");
    }

    @Test
    void shouldRejectObjectOrientedModules() {
        var objectOrientedModule = new ObjectOrientedModule(
                "User",
                "/foo",
                new ObjectOriented(List.of(new ObjectOriented.ClassDeclaration(
                        "User",
                        List.of(),
                        List.of(),
                        List.of(),
                        List.of(),
                        List.of()
                ))),
                List.of(),
                SourceKind.OBJECT_ORIENTED
        );

        assertThatThrownBy(() -> new JavaScriptGenerator().generate(new CompiledProgram(List.of(), List.of(objectOrientedModule))))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("Object-oriented `.coo` generation is only supported for JAVA");
    }

    private static CompiledProgram compileProgram(String source) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("Main", "/foo", source)),
                new TreeSet<>()
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().stream()
                    .map(Result.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private void writeGenerated(GeneratedProgram program) throws Exception {
        for (var module : program.modules()) {
            var target = tempDir.resolve(module.relativePath());
            var parent = target.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.writeString(target, module.code(), StandardCharsets.UTF_8);
        }
    }

    private String runNode(String script) throws Exception {
        var process = new ProcessBuilder("node", "-e", script)
                .directory(tempDir.toFile())
                .redirectErrorStream(true)
                .start();
        var output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var exit = process.waitFor();
        assertThat(exit).as(output).isEqualTo(0);
        return output;
    }
}
