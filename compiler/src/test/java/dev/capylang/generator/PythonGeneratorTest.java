package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;

class PythonGeneratorTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldGeneratePythonAndRunFunctionalModule() throws Exception {
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

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(
                        Path.of("foo", "Main.py"),
                        Path.of("dev", "capylang", "capybara.py"),
                        Path.of("capy", "lang", "Option.py")
                );

        var output = runPython("""
                import foo.Main as m
                print('|'.join([m.greet(), m.read(True), m.read(False), str(m.total())]))
                """);

        assertThat(output).isEqualTo("Ada|ok|none|6");
    }

    @Test
    void shouldGenerateAndRunObjectOrientedModules() throws Exception {
        var program = compileProgram(List.of(new RawModule(
                "User",
                "/foo",
                """
                        open class Base {
                            field prefix: String = "hello"

                            open def describe(name: String): String = prefix + " " + name
                        }

                        trait Bracket {
                            def bracket(name: String): String = "[" + name + "]"
                        }

                        class User(name: String): Base, Bracket {
                            field name: String = name

                            def greet(): String = this.describe(this.name)

                            def print(): String {
                                let label: String = Base.describe(this.name)
                                return label + "!"
                            }

                            def local_increment(x: int): int {
                                let step: int = 1
                                def inc(y: int): int = y + step
                                return inc(x)
                            }

                            def parity(x: int): bool {
                                def is_even(n: int): bool {
                                    if n == 0 {
                                        return true
                                    } else {
                                        return is_odd(n - 1)
                                    }
                                }
                                def is_odd(n: int): bool {
                                    if n == 0 {
                                        return false
                                    } else {
                                        return is_even(n - 1)
                                    }
                                }
                                return is_even(x)
                            }

                            def inverted(flag: bool): bool = !flag

                            def not_one(value: int): bool = value != 1

                            def bang(): String = "!"

                            def second(values: String[]): String = values[1]

                            def names(): String[] = String[]{"zero", "one"}

                            def slots(size: int): int[] = int[size]

                            def recover(flag: bool): String {
                                try {
                                    if flag {
                                        throw "boom"
                                    }
                                    return "ok"
                                } catch error {
                                    return error.getMessage()
                                }
                            }

                            def catch_index(values: String[]): String {
                                try {
                                    return values[3]
                                } catch error {
                                    return error.getClass().getSimpleName()
                                }
                            }
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        )));

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(
                        Path.of("foo", "Base.py"),
                        Path.of("foo", "Bracket.py"),
                        Path.of("foo", "User.py")
                );

        var output = runPython("""
                from foo.User import User
                user = User('Ada')
                print('|'.join([
                    user.greet(),
                    user.bracket(user.name),
                    user.print(),
                    str(user.local_increment(7)),
                    str(user.parity(12)).lower(),
                    str(user.parity(15)).lower(),
                    str(user.inverted(False)).lower(),
                    str(user.not_one(2)).lower(),
                    user.bang(),
                    user.second(['zero', 'one']),
                    ','.join(user.names()),
                    str(len(user.slots(4))),
                    user.recover(True),
                    user.catch_index(['zero'])
                ]))
                """);

        assertThat(output).isEqualTo("hello Ada|[Ada]|hello Ada!|8|true|false|true|true|!|one|zero,one|4|boom|ArrayIndexOutOfBoundsException");
    }

    @Test
    void shouldWrapIntArithmeticLikeJava() throws Exception {
        var program = compileProgram("""
                const MAX_INT: int = 2147483647
                const MIN_INT: int = -2147483648

                fun add_overflow(): int = MAX_INT + 1
                fun subtract_overflow(): int = MIN_INT - 1
                fun multiply_overflow(): int = 65536 * 65536
                fun divide_overflow(): int = MIN_INT / -1
                fun remainder_overflow(): int = MIN_INT % -1
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                print('|'.join([
                    str(m.addOverflow()),
                    str(m.subtractOverflow()),
                    str(m.multiplyOverflow()),
                    str(m.divideOverflow()),
                    str(m.remainderOverflow())
                ]))
                """);

        assertThat(output).isEqualTo("-2147483648|2147483647|0|-2147483648|0");
    }

    @Test
    void shouldResolveImportedEnumQualifierStaticCall() throws Exception {
        var program = compileProgram(List.of(
                new RawModule("Modes", "/foo/lib", """
                        enum RoundMode { FLOOR, HALF_EVEN }
                        """),
                new RawModule("Consumer", "/foo/app", """
                        from /foo/lib/Modes import { RoundMode }
                        from /capy/lang/Result import { * }

                        fun parsed_name(): String =
                            match RoundMode.parse('HALF_EVEN') with
                            case Success _ -> 'parsed'
                            case Error _ -> 'error'
                        """)
        ));

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var consumer = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "app", "Consumer.py")))
                .findFirst()
                .orElseThrow();
        assertThat(consumer.code()).contains("capy_module_foo_lib_Modes.RoundMode.parse(\"HALF_EVEN\")");

        var output = runPython("""
                import foo.app.Consumer as m
                print(m.parsedName())
                """);

        assertThat(output).isEqualTo("parsed");
    }

    private static CompiledProgram compileProgram(String source) {
        return compileProgram(List.of(new RawModule("Main", "/foo", source)));
    }

    private static CompiledProgram compileProgram(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(
                modules,
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

    private String runPython(String script) throws Exception {
        var process = new ProcessBuilder("python3", "-c", script)
                .directory(tempDir.toFile())
                .redirectErrorStream(true)
                .start();
        var output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var exit = process.waitFor();
        assertThat(exit).as(output).isEqualTo(0);
        return output;
    }
}
