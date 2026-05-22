package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderBinding;
import dev.capylang.compiler.NativeProviderManifest;
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
    void shouldEmitPathRuntimeReflectionPackageMetadata() throws Exception {
        var generated = new JavaScriptGenerator().generate(compileProgram("fun value(): int = 1"));
        writeGenerated(generated);

        var output = runNode("""
                const path = require('./capy/io/PathModule.js').fromString('tmp');
                const info = path.capybaraDataValueInfo();
                console.log([info.name, info.pkg.name, info.pkg.path].join('|'));
                """);

        assertThat(output).isEqualTo("Path|capy.io|capy/io/Path");
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
    void shouldLowerObjectOrientedNativeProviderCallToJavaScriptBootstrap() {
        var program = compileProgram(List.of(new RawModule(
                "Providers",
                "",
                """
                        interface Clock {
                            def now_millis(): long
                        }

                        native provider system_clock: Clock key "system"

                        class App {
                            def clock(): Clock = system_clock()
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        )), providerManifest(javascriptProviderBinding("/Providers.Clock", "system")));

        var generated = new JavaScriptGenerator().generate(program);
        var app = generated.modules().stream()
                .filter(module -> module.relativePath().endsWith("App.js"))
                .findFirst()
                .orElseThrow();
        var bootstrap = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("dev", "capylang", "native_providers.js")))
                .findFirst()
                .orElseThrow();

        assertThat(app.code())
                .contains("const __capy_native = require('./dev/capylang/native_providers.js');")
                .contains("return __capy_native.system_clock();")
                .doesNotContain("host-clock")
                .doesNotContain("SystemClock");
        assertThat(bootstrap.code())
                .contains("const __capy_provider_system_clock_module = require('host-clock');")
                .contains("new __capy_provider_system_clock_module['SystemClock']()")
                .contains("'now_millis'");
    }

    @Test
    void shouldGenerateAndRunObjectOrientedFunctionalInteropAndReflection() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "ObjectOrientedFpInterop",
                        "/foo",
                        """
                                union InteropPet = InteropDog | InteropCat
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
    void shouldExposeFunctionalAnnotationReflectionMetadataInCommonJs() throws Exception {
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

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var main = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Main.js")))
                .findFirst()
                .orElseThrow();
        assertThat(main.code())
                .contains("const capy = require(")
                .contains("module.exports =")
                .doesNotContain("import ")
                .doesNotContain("export ");

        var output = runNode("""
                const m = require('./foo/Main.js');
                const info = m.reflected(new m.User({ id: 'U-1' }));
                console.log([
                    typeof require,
                    typeof module.exports,
                    info.annotations[0].name,
                    info.annotations[0].arguments[0].value.kind,
                    info.annotations[0].arguments[1].value.value,
                    info.fields[0].annotations[0].name,
                    info.fields[0].annotations[0].arguments[0].value.value,
                    info.fields[0].type.name
                ].join('|'));
                """);

        assertThat(output).isEqualTo("function|object|TypeMarker|string|7|FieldMarker|identifier|String");
    }

    @Test
    void shouldExposeObjectOrientedAnnotationReflectionMetadataInCommonJs() throws Exception {
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

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var thing = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Thing.js")))
                .findFirst()
                .orElseThrow();
        assertThat(thing.code())
                .contains("const capy = require(")
                .contains("module.exports =")
                .doesNotContain("import ")
                .doesNotContain("export ");

        var output = runNode("""
                const { Thing } = require('./foo/Thing.js');
                const info = Thing.type();
                console.log([
                    info.annotations[0].name,
                    info.annotations[0].arguments[0].value.value,
                    info.fields[0].annotations[0].name,
                    info.fields[0].annotations[0].arguments[0].value.value,
                    info.methods[0].annotations[0].name,
                    info.methods[0].annotations[0].arguments[0].value.value
                ].join('|'));
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
    void shouldPreserveUpperSnakeConstNames() throws Exception {
        var program = compileProgram("""
                const FOO_BOO_X_Y: String = "foo"

                fun read(): String = FOO_BOO_X_Y
                """);

        var generated = new JavaScriptGenerator().generate(program);
        var main = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Main.js")))
                .map(GeneratedModule::code)
                .findFirst()
                .orElseThrow();
        assertThat(main).contains("const FOO_BOO_X_Y = \"foo\";");
        assertThat(main).contains("return FOO_BOO_X_Y;");
        assertThat(main).doesNotContain("fOOBOOXY");

        writeGenerated(generated);

        var output = runNode("""
                const m = require('./foo/Main.js');
                console.log([m.FOO_BOO_X_Y, m.read()].join('|'));
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

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var consumer = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "app", "Consumer.js")))
                .findFirst()
                .orElseThrow();
        assertThat(consumer.code()).contains("__module_foo_lib_Modes.RoundMode.parse(\"HALF_EVEN\")");

        var output = runNode("""
                const m = require('./foo/app/Consumer.js');
                console.log(m.parsedName());
                """);

        assertThat(output).isEqualTo("parsed");
    }

    @Test
    void shouldResolveDottedQualifiedDataConstructorOwnerBeforeExportFallback() throws Exception {
        var program = compileProgram(List.of(
                new RawModule("A", "/foo", """
                        data Value { amount: int }
                        fun value(x: int): Value = Value { amount: x }
                        """),
                new RawModule("Pipe", "/foo", """
                        data Value { x: int }
                        """),
                new RawModule("Main", "/foo", """
                        import /foo/A
                        import /foo/Pipe

                        fun qualified_import_data(x: int): A.Value = A.Value { amount: x }
                        """)
        ));

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var main = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Main.js")))
                .findFirst()
                .orElseThrow();
        assertThat(main.code()).contains("new __module_foo_A.Value({ amount: x })");

        var output = runNode("""
                const m = require('./foo/Main.js');
                console.log(m.qualifiedImportData(7).amount);
                """);

        assertThat(output).isEqualTo("7");
    }

    @Test
    void shouldGenerateExecutableNativeRandomSeedExport() throws Exception {
        var program = compileProgram(List.of(new RawModule("Random", "/capy/lang", """
                type seed -> long

                fun seed(): seed = <native>
                """)));

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var random = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("capy", "lang", "Random.js")))
                .findFirst()
                .orElseThrow();
        assertThat(random.code())
                .contains("return capy.toLong(Date.now());")
                .doesNotContain("unsupported");

        var output = runNode("""
                const random = require('./capy/lang/Random.js');
                console.log(typeof random.seed());
                """);

        assertThat(output).isEqualTo("bigint");
    }

    @Test
    void shouldEmitConstsBeforeDependentConsts() throws Exception {
        var program = compileProgram("""
                const Z_BASE: long = 21L
                const A_DERIVED: long = Z_BASE * 2L

                fun derived(): long = A_DERIVED
                """);

        var generated = new JavaScriptGenerator().generate(program);
        writeGenerated(generated);

        var main = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("foo", "Main.js")))
                .findFirst()
                .orElseThrow();
        assertThat(main.code()).containsSubsequence(
                "const Z_BASE = 21n;",
                "const A_DERIVED = capy.longMul(Z_BASE, 2n);"
        );

        var output = runNode("""
                const m = require('./foo/Main.js');
                console.log(String(m.derived()));
                """);

        assertThat(output).isEqualTo("42");
    }

    @Test
    void shouldExecuteOnlyEffectProgramMainAndUseProgramExitCodeContract() throws Exception {
        var generated = new JavaScriptGenerator().generate(compileProgram(List.of(
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

        var successRun = runNodeCommand("./foo/Main.js", "ok");
        assertThat(successRun.exitCode()).isEqualTo(0);
        assertThat(successRun.stdout()).isBlank();
        assertThat(successRun.stderr()).isBlank();

        var failedRun = runNodeCommand("./foo/Main.js");
        assertThat(failedRun.exitCode()).isEqualTo(1);
        assertThat(failedRun.stdout()).isBlank();
        assertThat(failedRun.stderr()).isBlank();

        var nonExecutableRun = runNodeCommand("./foo/SecondaryMain.js");
        assertThat(nonExecutableRun.exitCode()).isEqualTo(0);
        assertThat(nonExecutableRun.stdout()).isBlank();
        assertThat(nonExecutableRun.stderr()).isBlank();
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
        return compileProgram(modules, NativeProviderManifest.empty());
    }

    private static CompiledProgram compileProgram(List<RawModule> modules, NativeProviderManifest nativeProviders) {
        var result = CapybaraCompiler.INSTANCE.compile(
                modules,
                new TreeSet<>(),
                nativeProviders
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().stream()
                    .map(Result.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static NativeProviderManifest providerManifest(NativeProviderBinding... bindings) {
        return new NativeProviderManifest(List.of(bindings));
    }

    private static NativeProviderBinding javascriptProviderBinding(String interfaceId, String qualifier) {
        return new NativeProviderBinding(
                interfaceId,
                qualifier,
                "singleton",
                null,
                new NativeProviderBackendBinding(null, "host-clock", "SystemClock", "new"),
                null
        );
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

    private ProcessResult runNodeCommand(String... args) throws Exception {
        var command = new java.util.ArrayList<String>();
        command.add("node");
        command.addAll(List.of(args));
        var process = new ProcessBuilder(command)
                .directory(tempDir.toFile())
                .start();
        var stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var stderr = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var exit = process.waitFor();
        return new ProcessResult(exit, stdout, stderr);
    }

    private record ProcessResult(int exitCode, String stdout, String stderr) {
    }
}
