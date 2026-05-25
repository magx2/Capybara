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
    void shouldExposeFunctionalAnnotationReflectionMetadataInPython() throws Exception {
        var program = compileProgram("""
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

                annotation TypeMarker on data {
                    label: String
                    order: int = 1
                }

                annotation FieldMarker on field {
                    value: String
                }

                @TypeMarker(label: "entity", order: 7)
                data User {
                    @FieldMarker(value: "identifier")
                    id: String
                }

                fun reflected(user: User): DataValueInfo = reflection(user)
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                info = m.reflected(m.User({'id': 'U-1'}))
                print('|'.join([
                    info.annotations[0].name,
                    info.annotations[0].arguments[0].value.kind,
                    str(info.annotations[0].arguments[1].value.value),
                    info.fields[0].annotations[0].name,
                    info.fields[0].annotations[0].arguments[0].value.value,
                    info.fields[0].type.name,
                    info.pkg.path
                ]))
                """);

        assertThat(output).isEqualTo("TypeMarker|string|7|FieldMarker|identifier|String|foo/Main");
    }

    @Test
    void shouldExposeObjectOrientedAnnotationReflectionMetadataInPython() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "Markers",
                        "/foo",
                        """
                                annotation TypeMarker on class {
                                    label: String
                                }

                                annotation FieldMarker on field {
                                    value: String
                                }

                                annotation MethodMarker on method {
                                    value: String
                                }
                                """,
                        SourceKind.FUNCTIONAL
                ),
                new RawModule(
                        "Thing",
                        "/foo",
                        """
                                from /foo/Markers import { TypeMarker, FieldMarker, MethodMarker }

                                @TypeMarker(label: "entity")
                                class Thing(name: String) {
                                    @FieldMarker(value: "display_name")
                                    field name: String = name

                                    @MethodMarker(value: "greet")
                                    def greet(): String = this.name
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ));

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                from foo.Thing import Thing
                info = Thing.type()
                print('|'.join([
                    info.annotations[0].name,
                    info.annotations[0].arguments[0].value.value,
                    info.fields[0].annotations[0].name,
                    info.fields[0].annotations[0].arguments[0].value.value,
                    info.methods[0].annotations[0].name,
                    info.methods[0].annotations[0].arguments[0].value.value
                ]))
                """);

        assertThat(output).isEqualTo("TypeMarker|entity|FieldMarker|display_name|MethodMarker|greet");
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
    void shouldRunNativeMathDigitsInPython() throws Exception {
        var program = compileProgram("""
                from /capy/lang/Math import { digits }

                fun zero_digits(): int = digits(0)
                fun positive_digits(): int = digits(2147483647)
                fun min_int_digits(): int = digits(-2147483648)
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                print('|'.join([
                    str(m.zeroDigits()),
                    str(m.positiveDigits()),
                    str(m.minIntDigits())
                ]))
                """);

        assertThat(output).isEqualTo("1|10|10");
    }

    @Test
    void shouldPreserveUpperSnakeConstNames() throws Exception {
        var program = compileProgram("""
                const FOO_BOO_X_Y: String = "foo"

                fun read(): String = FOO_BOO_X_Y
                """);

        var generated = new PythonGenerator().generate(program);
        var main = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Main.py")))
                .map(GeneratedModule::code)
                .findFirst()
                .orElseThrow();
        assertThat(main).contains("FOO_BOO_X_Y = \"foo\"");
        assertThat(main).contains("return FOO_BOO_X_Y");
        assertThat(main).doesNotContain("fOOBOOXY");

        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                print('|'.join([m.FOO_BOO_X_Y, m.read()]))
                """);

        assertThat(output).isEqualTo("foo|foo");
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

    @Test
    void shouldGenerateExecutableNativeRandomSeedExport() throws Exception {
        var program = compileProgram(List.of(new RawModule("Random", "/capy/lang", """
                type seed -> long

                fun seed(): seed = <native>
                """)));

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var random = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("capy", "lang", "Random.py")))
                .findFirst()
                .orElseThrow();
        assertThat(random.code())
                .contains("return capy.current_millis()")
                .doesNotContain("unsupported");

        var output = runPython("""
                import capy.lang.Random as random
                print(type(random.seed()).__name__)
                """);

        assertThat(output).isEqualTo("int");
    }

    @Test
    void shouldRunPythonSystemAndClockEffects() throws Exception {
        var program = compileProgram("""
                from /capy/date_time/Clock import { now }
                from /capy/date_time/DateTime import { * }
                from /capy/lang/Effect import { * }
                from /capy/lang/System import { current_millis, nano_time }

                fun clock_now_iso(): Effect[String] =
                    let current <- now()
                    current.to_iso_8601()

                fun current_millis_value(): Effect[long] =
                    current_millis()

                fun nano_time_value(): Effect[long] =
                    nano_time()
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                millis = m.currentMillisValue().unsafe_run()
                nanos = m.nanoTimeValue().unsafe_run()
                iso = m.clockNowIso().unsafe_run()
                print('|'.join([
                    str(isinstance(millis, int)).lower(),
                    str(isinstance(nanos, int)).lower(),
                    str('T' in iso).lower()
                ]))
                """);

        assertThat(output).isEqualTo("true|true|true");
    }

    @Test
    void shouldRunPythonConsoleEffects() throws Exception {
        var program = compileProgram("""
                from /capy/io/Console import { * }
                from /capy/lang/Effect import { * }
                from /capy/lang/Option import { * }

                fun emit(bytes: List[byte]): Effect[String] =
                    let out <- print("out")
                    let out_line <- println(" line")
                    let err <- print_error("err")
                    let err_line <- println_error(" line")
                    let out_bytes <- println(bytes)
                    let err_bytes <- println_error(bytes)
                    pure(out + out_line + err + err_line)

                fun read_pair(): Effect[String] =
                    let first <- read_line()
                    let second <- read_line()
                    pure(line_or_eof(first) + "|" + line_or_eof(second))

                private fun line_or_eof(line: Option[String]): String =
                    match line with
                    case Some { value } -> value
                    case None -> "EOF"
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var run = runPythonCommandWithInput("Capy\n", "-c", """
                import foo.Main as m
                print("EMIT:" + m.emit([65, 90]).unsafe_run())
                print("READ:" + m.readPair().unsafe_run())
                """);

        assertThat(run.exitCode()).isEqualTo(0);
        assertThat(run.stdout()).isEqualTo("out line\nAZ\nEMIT:out lineerr line\nREAD:Capy|EOF");
        assertThat(run.stderr()).isEqualTo("err line\nAZ");
    }

    @Test
    void shouldEmitConstsBeforeDependentConsts() throws Exception {
        var program = compileProgram("""
                const Z_BASE: long = 21L
                const A_DERIVED: long = Z_BASE * 2L

                fun derived(): long = A_DERIVED
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var main = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Main.py")))
                .findFirst()
                .orElseThrow();
        assertThat(main.code()).containsSubsequence(
                "Z_BASE = 21",
                "A_DERIVED = capy.long_mul(Z_BASE, 2)"
        );

        var output = runPython("""
                import foo.Main as m
                print(str(m.derived()))
                """);

        assertThat(output).isEqualTo("42");
    }

    @Test
    void shouldExecuteOnlyEffectProgramMainAndUseProgramExitCodeContract() throws Exception {
        var generated = new PythonGenerator().generate(compileProgram(List.of(
                new RawModule("Main", "/foo", """
                        from /capy/lang/Effect import { * }
                        from /capy/lang/Program import { DEFAULT_FAILED_EXIT_CODE }
                        from /capy/collection/List import { * }

                        fun main(args: List[String]): Effect[/capy/lang/Program] =
                            if args.size() > 0
                            then pure(/capy/lang/Program.Success {})
                            else pure(/capy/lang/Program.Failed { exit_code: DEFAULT_FAILED_EXIT_CODE })
                        """),
                new RawModule("SecondaryMain", "/foo", """
                        from /capy/collection/List import { * }

                        fun main(args: List[String]): /capy/lang/Program =
                            /capy/lang/Program.Success {}
                        """)
        )));
        writeGenerated(generated);

        var successRun = runPythonCommand("-m", "foo.Main", "ok");
        assertThat(successRun.exitCode()).isEqualTo(0);
        assertThat(successRun.stdout()).isBlank();
        assertThat(successRun.stderr()).isBlank();

        var failedRun = runPythonCommand("-m", "foo.Main");
        assertThat(failedRun.exitCode()).isEqualTo(1);
        assertThat(failedRun.stdout()).isBlank();
        assertThat(failedRun.stderr()).isBlank();

        var nonExecutableRun = runPythonCommand("-m", "foo.SecondaryMain");
        assertThat(nonExecutableRun.exitCode()).isEqualTo(0);
        assertThat(nonExecutableRun.stdout()).isBlank();
        assertThat(nonExecutableRun.stderr()).isBlank();
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

    private ProcessResult runPythonCommand(String... args) throws Exception {
        var command = new java.util.ArrayList<String>();
        command.add("python3");
        command.addAll(List.of(args));
        var process = new ProcessBuilder(command)
                .directory(tempDir.toFile())
                .start();
        var stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var stderr = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var exit = process.waitFor();
        return new ProcessResult(exit, stdout, stderr);
    }

    private ProcessResult runPythonCommandWithInput(String input, String... args) throws Exception {
        var command = new java.util.ArrayList<String>();
        command.add("python3");
        command.addAll(List.of(args));
        var process = new ProcessBuilder(command)
                .directory(tempDir.toFile())
                .start();
        process.getOutputStream().write(input.getBytes(StandardCharsets.UTF_8));
        process.getOutputStream().close();
        var stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var stderr = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var exit = process.waitFor();
        return new ProcessResult(exit, stdout, stderr);
    }

    private record ProcessResult(int exitCode, String stdout, String stderr) {
    }
}
