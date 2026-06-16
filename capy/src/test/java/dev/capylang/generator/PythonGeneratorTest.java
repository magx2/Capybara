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
    void shouldEmitPathRuntimeReflectionPackageMetadata() throws Exception {
        var generated = new PythonGenerator().generate(compileProgram("fun value(): int = 1"));
        writeGenerated(generated);

        var output = runPython("""
                from capy.io.PathModule import fromString
                info = fromString("tmp").capybaraDataValueInfo()
                print('|'.join([info.name, info.pkg.name, info.pkg.path]))
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
    void shouldLowerNativeProviderFunctionToPythonEffectBackedBootstrapCall() {
        var program = compileProgram(nativeProviderModules("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Providers import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """), providerManifest(pythonProviderBinding("/Providers.Clock", "system")));

        var generated = new PythonGenerator().generate(program);
        assertThat(generated.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(Path.of("dev", "capylang", "native_providers.py"));
        var providerModule = generated.modules().stream()
                .filter(module -> module.relativePath().endsWith("ProvidersNative.py"))
                .findFirst()
                .orElseThrow();
        var bootstrap = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("dev", "capylang", "native_providers.py")))
                .findFirst()
                .orElseThrow();

        assertThat(providerModule.code())
                .contains("import dev.capylang.native_providers as capy_module_dev_capylang_native_providers")
                .contains("return capy.delay(lambda: capy_module_dev_capylang_native_providers.system_clock())")
                .doesNotContain("host_clock")
                .doesNotContain("SystemClock");
        assertThat(bootstrap.code())
                .contains("import dev.capylang.capybara as capy")
                .contains("__capy_provider_system_clock_class = capy.require_native_provider_class(module_name='host_clock', class_name='SystemClock', metadata={")
                .contains("_providers = capy.define_native_providers({")
                .contains("interface_id='/Providers.Clock'")
                .contains("qualifier='system'")
                .contains("factory='call'")
                .contains("create=lambda: __capy_provider_system_clock_class()")
                .contains("{'name': 'now_millis', 'arity': 0}")
                .contains("def system_clock():")
                .contains("return _providers.resolve('/Providers.Clock', 'system', 'system_clock', 'python', '/ProvidersNative.cfun')");
    }

    @Test
    void shouldFailPythonNativeProviderStartupWhenModuleIsMissing() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def now_millis(): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.missing_clock",
                        "SystemClock",
                        "call"
                )
        ));
        writeGenerated(generated);

        var result = runPythonCommand("-c", "import dev.capylang.native_providers");

        assertThat(result.exitCode()).isNotZero();
        assertThat(result.stderr())
                .contains("NativeProviderError")
                .contains("/Providers.Clock")
                .contains("system")
                .contains("system_clock")
                .contains("python")
                .contains("nativeinterop.missing_clock");
    }

    @Test
    void shouldRewriteObjectOrientedNativeProviderCallsToPythonBootstrap() {
        var modules = new java.util.ArrayList<>(nativeProviderModules("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Providers import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """));
        modules.add(new RawModule("ClockConsumer", "", """
                from Providers import { Clock }
                from ProvidersNative import { system_clock }

                class ClockConsumer {
                    def current(): Clock = system_clock()
                }
                """, SourceKind.OBJECT_ORIENTED));
        var program = compileProgram(modules, providerManifest(pythonProviderBinding("/Providers.Clock", "system")));

        var generated = new PythonGenerator().generate(program);
        var consumerModule = generated.modules().stream()
                .filter(module -> module.relativePath().equals(Path.of("ClockConsumer.py")))
                .findFirst()
                .orElseThrow();

        assertThat(consumerModule.code())
                .contains("import dev.capylang.native_providers as __capy_native")
                .contains("return __capy_native.system_clock()")
                .doesNotContain("ProvidersNative.system_clock()");
    }

    @Test
    void shouldRunPythonNativeProviderWithStructuralInterfaceValidation() throws Exception {
        var program = compileProgram(nativeProviderModules("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Providers import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """), providerManifest(pythonProviderBinding(
                "/Providers.Clock",
                "system",
                "nativeinterop.system_clock",
                "SystemClock",
                "call"
        )));

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                class SystemClock:
                    def __init__(self):
                        self.offset = 9000

                    def now_millis(self):
                        return self.offset + 123
                """);

        var output = runPython("""
                import dev.capylang.capybara as capy
                import dev.capylang.native_providers as native_providers
                import ProvidersNative
                first = ProvidersNative.systemClock().unsafe_run()
                second = ProvidersNative.systemClock().unsafe_run()
                print('|'.join([
                    str(first.now_millis()),
                    str(capy.is_type(first, 'Clock')).lower(),
                    str(first is second).lower(),
                ]))
                """);

        assertThat(output).isEqualTo("9123|true|false");
    }

    @Test
    void shouldCreateNewPythonNativeProviderInstance() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def now_millis(): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.system_clock",
                        "SystemClock",
                        "call"
                )
        ));
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                class SystemClock:
                    def now_millis(self):
                        return 1
                """);

        var output = runPython("""
                import dev.capylang.native_providers as native_providers
                print(str(native_providers.system_clock() is native_providers.system_clock()).lower())
                """);

        assertThat(output).isEqualTo("false");
    }

    @Test
    void shouldFailPythonNativeProviderStartupWhenClassIsMissing() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def now_millis(): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.system_clock",
                        "SystemClock",
                        "call"
                )
        ));
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                class OtherClock:
                    pass
                """);

        var result = runPythonCommand("-c", "import dev.capylang.native_providers");

        assertThat(result.exitCode()).isNotZero();
        assertThat(result.stderr())
                .contains("NativeProviderError")
                .contains("/Providers.Clock")
                .contains("system")
                .contains("system_clock")
                .contains("python")
                .contains("SystemClock")
                .contains("nativeinterop.system_clock");
    }

    @Test
    void shouldFailPythonNativeProviderValidationWhenMethodIsMissing() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def now_millis(): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.system_clock",
                        "SystemClock",
                        "call"
                )
        ));
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                class SystemClock:
                    pass
                """);

        var result = runPythonCommand("-c", "import dev.capylang.native_providers as providers; providers.system_clock()");

        assertThat(result.exitCode()).isNotZero();
        assertThat(result.stderr())
                .contains("/Providers.Clock")
                .contains("system")
                .contains("missing method `now_millis`");
    }

    @Test
    void shouldFailPythonNativeProviderValidationWhenMethodIsNotCallable() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def now_millis(): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.system_clock",
                        "SystemClock",
                        "call"
                )
        ));
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                class SystemClock:
                    now_millis = 1
                """);

        var result = runPythonCommand("-c", "import dev.capylang.native_providers as providers; providers.system_clock()");

        assertThat(result.exitCode()).isNotZero();
        assertThat(result.stderr())
                .contains("/Providers.Clock")
                .contains("system")
                .contains("method `now_millis` must be callable");
    }

    @Test
    void shouldFailPythonNativeProviderValidationWhenMethodArityIsTooSmall() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def plus(value: long): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.system_clock",
                        "SystemClock",
                        "call"
                )
        ));
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                class SystemClock:
                    def plus(self):
                        return 1
                """);

        var result = runPythonCommand("-c", "import dev.capylang.native_providers as providers; providers.system_clock()");

        assertThat(result.exitCode()).isNotZero();
        assertThat(result.stderr())
                .contains("/Providers.Clock")
                .contains("system")
                .contains("requires arity at least 1, got 0");
    }

    @Test
    void shouldFailPythonNativeProviderValidationWhenFactoryReturnsNone() throws Exception {
        var generated = new PythonGenerator().generate(nativeProviderProgram(
                "def now_millis(): long",
                pythonProviderBinding(
                        "/Providers.Clock",
                        "system",
                        "nativeinterop.system_clock",
                        "make_clock",
                        "call"
                )
        ));
        writeGenerated(generated);
        writeHostModule("nativeinterop/system_clock.py", """
                def make_clock():
                    return None
                """);

        var result = runPythonCommand("-c", "import dev.capylang.native_providers as providers; providers.system_clock()");

        assertThat(result.exitCode()).isNotZero();
        assertThat(result.stderr())
                .contains("/Providers.Clock")
                .contains("system")
                .contains("returned None");
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
    void shouldRunSeqNamedMethodsOnPythonListRuntime() throws Exception {
        var program = compileProgram("""
                from /capy/collection/Seq import { * }
                from /capy/collection/List import { * }

                fun expand(value: int): Seq[int] = to_seq([value, value + 1])
                fun seq_map(values: List[int]): List[int] = to_seq(values).map(x => x * 3).as_list()
                fun seq_flat_map(values: List[int]): List[int] = to_seq(values).flat_map(x => expand(x)).as_list()
                fun seq_filter(values: List[int]): List[int] = to_seq(values).filter(x => x > 1).as_list()
                fun seq_reject(values: List[int]): List[int] = to_seq(values).reject(x => x > 1).as_list()
                fun seq_reduce(values: List[int]): int = to_seq(values).reduce(0, (acc, value) => acc + value)
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                print('|'.join([
                    ','.join([str(value) for value in m.seqMap([1, 2, 3])]),
                    ','.join([str(value) for value in m.seqFlatMap([1, 2])]),
                    ','.join([str(value) for value in m.seqFilter([1, 2, 3])]),
                    ','.join([str(value) for value in m.seqReject([1, 2, 3])]),
                    str(m.seqReduce([1, 2, 3]))
                ]))
                """);

        assertThat(output).isEqualTo("3,6,9|1,2,2,3|2,3|1|6");
    }

    @Test
    void shouldRunRegexRuntimeInPython() throws Exception {
        var program = compileProgram("""
                from /capy/lang/Regex import { * }
                from /capy/lang/Option import { * }
                from /capy/collection/Seq import { * }

                fun matches_named(input: String): bool = regex/foo/.matches(input)
                fun matches_alias(input: String): bool = regex/foo/ ? input

                fun find_named(input: String): bool =
                    match regex/foo/.find(input) with
                    case Some { _ } -> true
                    case None -> false

                fun find_alias(input: String): bool =
                    match regex/foo/ ~ input with
                    case Some { _ } -> true
                    case None -> false

                fun find_all_named_count(input: String): int =
                    if regex/foo/.find_all(input).any(_ => true)
                    then 1
                    else 0

                fun find_all_alias_count(input: String): int =
                    if (regex/foo/ ~~ input).any(_ => true)
                    then 1
                    else 0

                fun replace_named(input: String): String = regex/1/.replace("#")(input)
                fun replace_alias(input: String): String = (regex/1/ ~> "#")(input)

                fun split_named(input: String): List[String] = regex/,/.split(input)
                fun split_alias(input: String): List[String] = regex/,/ /> input
                fun split_multi_char(input: String): List[String] = regex/--/.split(input)

                fun escaped_slash_match(): bool = regex/a\\/b/ ? "--a/b--"
                """);

        var generated = new PythonGenerator().generate(program);
        writeGenerated(generated);

        var output = runPython("""
                import foo.Main as m
                print('|'.join([
                    str(m.matchesNamed('xxfooyy')).lower(),
                    str(m.matchesNamed('xxbaryy')).lower(),
                    str(m.matchesAlias('xxfooyy')).lower(),
                    str(m.matchesAlias('xxbaryy')).lower(),
                    str(m.findNamed('xxfooyy')).lower(),
                    str(m.findNamed('xxbaryy')).lower(),
                    str(m.findAlias('xxfooyy')).lower(),
                    str(m.findAlias('xxbaryy')).lower(),
                    str(m.findAllNamedCount('xxfooyy')),
                    str(m.findAllNamedCount('xxbaryy')),
                    str(m.findAllAliasCount('xxfooyy')),
                    str(m.findAllAliasCount('xxbaryy')),
                    m.replaceNamed('a1b11'),
                    m.replaceAlias('a1b11'),
                    ','.join(m.splitNamed('a,b,c')),
                    ','.join(m.splitAlias('a,b,c')),
                    ','.join(m.splitMultiChar('a--b--c')),
                    str(m.escapedSlashMatch()).lower()
                ]))
                """);

        assertThat(output).isEqualTo("true|false|true|false|true|false|true|false|1|0|1|0|a#b##|a#b##|a,b,c|a,b,c|a,b,c|true");
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
        return compileProgram(modules, NativeProviderManifest.empty());
    }

    private static CompiledProgram compileProgram(List<RawModule> modules, NativeProviderManifest nativeProviders) {
        var result = CapybaraCompiler.INSTANCE.compile(
                modulesWithNativeProviderAnnotation(modules),
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

    private static NativeProviderBinding pythonProviderBinding(String interfaceId, String qualifier) {
        return pythonProviderBinding(interfaceId, qualifier, "host_clock", "SystemClock", "call");
    }

    private static List<RawModule> nativeProviderModules(String providerSource) {
        return List.of(
                new RawModule(
                        "Providers",
                        "",
                        """
                                interface Clock {
                                    def now_millis(): long
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                ),
                new RawModule("ProvidersNative", "", providerSource)
        );
    }

    private static NativeProviderBinding pythonProviderBinding(
            String interfaceId,
            String qualifier,
            String moduleName,
            String className,
            String factory
    ) {
        return new NativeProviderBinding(
                interfaceId,
                qualifier,
                null,
                null,
                new NativeProviderBackendBinding(className, moduleName, null, factory)
        );
    }

    private static CompiledProgram nativeProviderProgram(String methodDeclaration, NativeProviderBinding binding) {
        return compileProgram(List.of(
                new RawModule(
                        "Providers",
                        "",
                        """
                                interface Clock {
                                    %s
                                }
                                """.formatted(methodDeclaration),
                        SourceKind.OBJECT_ORIENTED
                ),
                new RawModule("ProvidersNative", "", """
                        from /capy/lang/Effect import { Effect }
                        from /capy/meta_prog/NativeProvider import { NativeProvider }
                        from Providers import { Clock }

                        @NativeProvider(qualifier: "system")
                        fun system_clock(): Effect[Clock] = <native>
                        """)
        ), providerManifest(binding));
    }

    private static List<RawModule> modulesWithNativeProviderAnnotation(List<RawModule> modules) {
        if (modules.stream().noneMatch(module -> module.input().contains("NativeProvider"))) {
            return modules;
        }
        var allModules = new java.util.ArrayList<RawModule>();
        allModules.add(nativeProviderAnnotationModule());
        allModules.addAll(modules);
        return allModules;
    }

    private static RawModule nativeProviderAnnotationModule() {
        return new RawModule("NativeProvider", "/capy/meta_prog", """
                annotation NativeProvider on fun {
                    qualifier: String = ""
                }
                """);
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

    private void writeHostModule(String relativePath, String code) throws Exception {
        var target = tempDir.resolve(relativePath);
        var parent = target.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        Files.writeString(target, code, StandardCharsets.UTF_8);
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
