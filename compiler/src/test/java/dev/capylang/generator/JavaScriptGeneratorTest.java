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

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(
                        Path.of("foo", "Base.js"),
                        Path.of("foo", "Bracket.js"),
                        Path.of("foo", "User.js")
                );

        var output = runNode("""
                const { User } = require('./foo/User.js');
                const user = new User('Ada');
                console.log([
                    user.greet(),
                    user.bracket(user.name),
                    user.print(),
                    user.local_increment(7),
                    user.parity(12),
                    user.parity(15),
                    user.second(['zero', 'one']),
                    user.names().join(','),
                    user.slots(4).length,
                    user.recover(true),
                    user.catch_index(['zero'])
                ].join('|'));
                """);

        assertThat(output).isEqualTo("hello Ada|[Ada]|hello Ada!|8|true|false|one|zero,one|4|boom|ArrayIndexOutOfBoundsException");
    }

    @Test
    void shouldGenerateAndRunObjectOrientedFunctionalInteropAndReflection() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "ObjectOrientedFpInterop",
                        "/foo",
                        """
                                type InteropPet = InteropDog | InteropCat
                                data InteropDog { name: String }
                                data InteropCat { age: int }

                                fun make_dog(name: String): InteropDog = InteropDog { name: name }

                                fun pet_text(pet: InteropPet): String =
                                    match pet with
                                    case InteropDog { name } -> "dog:" + name
                                    case InteropCat { age } -> "cat:" + age
                                """,
                        SourceKind.FUNCTIONAL
                ),
                new RawModule(
                        "PetInteractor",
                        "/foo",
                        """
                                from ObjectOrientedFpInterop import { InteropPet, InteropDog, InteropCat }

                                interface Printable {
                                    def print(): String
                                }

                                trait Label {
                                    def label(name: String): String = "[" + name + "]"
                                }

                                class PetInteractor: Printable, Label {
                                    def invoke_fp_function(name: String): String =
                                        ObjectOrientedFpInterop.petText(ObjectOrientedFpInterop.makeDog(name))

                                    def create_fp_data(name: String): InteropPet =
                                        InteropDog { name: name }

                                    def match_fp_type(pet_name: String): String {
                                        let pet: InteropPet = InteropDog { name: pet_name }
                                        return match pet with
                                        case InteropDog { name } -> ("dog:" + name)
                                        case InteropCat { age } -> ("cat:" + age)
                                    }

                                    override def print(): String = this.label("pet")
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ));

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var output = runNode("""
                const { PetInteractor } = require('./foo/PetInteractor.js');
                const { InteropDog } = require('./foo/ObjectOrientedFpInterop.js');
                const pet = new PetInteractor();
                const dog = pet.create_fp_data('Bara');
                const printable = require('./foo/Printable.js').Printable.type();
                const label = require('./foo/Label.js').Label.type();
                const interactor = PetInteractor.type();
                console.log([
                    pet.invoke_fp_function('Capy'),
                    dog instanceof InteropDog,
                    dog.name,
                    pet.match_fp_type('Mochi'),
                    pet.print(),
                    printable.methods.map(method => method.name).join(','),
                    label.methods.map(method => method.name).join(','),
                    interactor.parents.map(parent => parent.name).sort().join(',')
                ].join('|'));
                """);

        assertThat(output).isEqualTo("dog:Capy|true|Bara|dog:Mochi|[pet]|print|label|Label,Printable");
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

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var output = runNode("""
                const m = require('./foo/Main.js');
                console.log([
                    m.addOverflow(),
                    m.subtractOverflow(),
                    m.multiplyOverflow(),
                    m.divideOverflow(),
                    m.remainderOverflow()
                ].join('|'));
                """);

        assertThat(output).isEqualTo("-2147483648|2147483647|0|-2147483648|0");
    }

    @Test
    void shouldSkipRuntimeProvidedStdlibSources() {
        var generated = new JavaScriptGenerator().generate(new CompiledProgram(List.of(
                runtimeModule("List", "/capy/collection"),
                runtimeModule("String", "/capy/lang"),
                runtimeModule("Primitives", "/capy/lang"),
                runtimeModule("IO", "/capy/io"),
                runtimeModule("Date", "/capy/date_time"),
                runtimeModule("Regex", "/capy/lang")
        )));

        var paths = generated.modules().stream()
                .map(GeneratedModule::relativePath)
                .toList();

        assertThat(paths)
                .contains(
                        Path.of("capy", "collection", "List.js"),
                        Path.of("capy", "lang", "String.js"),
                        Path.of("capy", "lang", "Primitives.js"),
                        Path.of("capy", "io", "IO.js"),
                        Path.of("capy", "date_time", "DateModule.js"),
                        Path.of("capy", "lang", "RegexModule.js")
                )
                .doesNotContain(
                        Path.of("capy", "date_time", "Date.js"),
                        Path.of("capy", "lang", "Regex.js")
                )
                .doesNotHaveDuplicates();
    }

    @Test
    void shouldKeepSourceBackedStdlibModulesWhenRuntimeIsOnlyAHelper() {
        var generated = new JavaScriptGenerator().generate(new CompiledProgram(List.of(
                runtimeModule("Math", "/capy/lang"),
                runtimeModule("Seq", "/capy/lang")
        )));

        var paths = generated.modules().stream()
                .map(GeneratedModule::relativePath)
                .toList();

        assertThat(paths).filteredOn(Path.of("capy", "lang", "Math.js")::equals).hasSize(2);
        assertThat(paths).filteredOn(Path.of("capy", "lang", "Seq.js")::equals).hasSize(2);
    }

    private static CompiledProgram compileProgram(String source) {
        return compileProgram(List.of(new RawModule("Main", "/foo", source)));
    }

    private static dev.capylang.compiler.CompiledModule runtimeModule(String name, String path) {
        return new dev.capylang.compiler.CompiledModule(name, path, Map.of(), List.of(), List.of());
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
