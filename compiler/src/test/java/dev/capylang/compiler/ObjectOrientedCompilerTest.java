package dev.capylang.compiler;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.expression.CompiledEffectBindExpression;
import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledObjectConstruction;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ObjectOrientedCompilerTest {
    @Test
    void shouldCompileObjectOrientedModules() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                class User {
                                    def greet(): String = "hello"
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThat(program.modules()).isEmpty();
        assertThat(program.objectOrientedModules())
                .singleElement()
                .satisfies(module -> {
                    assertThat(module.name()).isEqualTo("User");
                    assertThat(module.path()).isEqualTo("/foo/boo");
                });
    }


    @Test
    void shouldAllowNestedCallsAsStandAloneStatements() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "User",
                        "/foo/boo",
                        """
                                class User {
                                    def format(name: String): String = name

                                    def print(value: String): void {
                                    }

                                    def greet(name: String): void {
                                        print(format(name))
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
    }

    @Test
    void shouldIgnoreUnsafeRunInsideStringLiteralInObjectOrientedMethod() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule(
                        "Message",
                        "/foo/boo",
                        """
                                class Message {
                                    def run(): String = "effect.unsafe_run()"
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
    }

    @Test
    void shouldCompileNativeProviderDeclarationTargetingLocalInterface() {
        var program = compileProviderSuccess(
                List.of(
                        clockObjectInterfaceModule(),
                        clockProviderModule()
                ),
                providerManifest(providerBinding("/dev/capylang/test/Clock", "system"))
        );

        assertThat(program.nativeProviderCatalog().declarations())
                .singleElement()
                .satisfies(declaration -> {
                    assertThat(declaration.providerName()).isEqualTo("system_clock");
                    assertThat(declaration.sourceModulePath()).isEqualTo("/dev/capylang/test");
                    assertThat(declaration.sourceModuleName()).isEqualTo("ClockProvider");
                    assertThat(declaration.targetTypeName()).isEqualTo("Clock");
                    assertThat(declaration.interfaceId()).isEqualTo("/dev/capylang/test/Clock");
                    assertThat(declaration.qualifier()).isEqualTo("system");
                    assertThat(declaration.sourceFile()).isEqualTo("/dev/capylang/test/ClockProvider.cfun");
                });
        assertThat(program.nativeProviderCatalog().bindings())
                .singleElement()
                .satisfies(binding -> {
                    assertThat(binding.interfaceId()).isEqualTo("/dev/capylang/test/Clock");
                    assertThat(binding.qualifier()).isEqualTo("system");
                    assertThat(binding.javaBinding().className()).isEqualTo("dev.capylang.test.SystemClock");
                });

        assertThat(program.objectOrientedModules())
                .singleElement()
                .satisfies(module -> assertThat(module.name()).isEqualTo("Clock"));
    }

    @Test
    void shouldCompileNativeProviderBackendWiringFromNativeImplementationAnnotation() {
        var program = compileProviderSuccess(List.of(clockObjectInterfaceModule(), new RawModule("ClockProvider", "/dev/capylang/test", """
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """), nativeImplementationModule("system")), NativeProviderManifest.empty());

        assertThat(program.nativeProviderCatalog().declarations())
                .singleElement()
                .satisfies(declaration -> {
                    assertThat(declaration.providerName()).isEqualTo("system_clock");
                    assertThat(declaration.interfaceId()).isEqualTo("/dev/capylang/test/Clock");
                    assertThat(declaration.qualifier()).isEqualTo("system");
                });
        assertThat(program.nativeProviderCatalog().bindings())
                .singleElement()
                .satisfies(binding -> {
                    assertThat(binding.interfaceId()).isEqualTo("/dev/capylang/test/Clock");
                    assertThat(binding.qualifier()).isEqualTo("system");
                    assertThat(binding.javaBinding().className()).isEqualTo("dev.capylang.test.SystemClock");
                    assertThat(binding.javaBinding().factory()).isEqualTo("constructor");
                    assertThat(binding.javascriptBinding().moduleName()).isEqualTo("./test/SystemClock.js");
                    assertThat(binding.javascriptBinding().exportName()).isEqualTo("SystemClock");
                    assertThat(binding.javascriptBinding().factory()).isEqualTo("new");
                    assertThat(binding.pythonBinding().moduleName()).isEqualTo("dev.capylang.test.SystemClock");
                    assertThat(binding.pythonBinding().className()).isEqualTo("SystemClock");
                    assertThat(binding.pythonBinding().factory()).isEqualTo("call");
                });
    }

    @Test
    void shouldCompileNativeProviderAnnotationOnImportedInterfaceModule() {
        var program = compileProviderSuccess(
                List.of(
                        clockInterfaceModule(),
                        new RawModule("Providers", "/dev/capylang/app", """
                                from /capy/lang/Effect import { Effect }
                                from /capy/meta_prog/NativeProvider import { NativeProvider }
                                from /dev/capylang/time/Clock import { Clock }

                                @NativeProvider(qualifier: "system")
                                fun app_clock(): Effect[Clock] = <native>
                                """)
                ),
                providerManifest(providerBinding("/dev/capylang/time/Clock", "system"))
        );

        assertThat(program.nativeProviderCatalog().declarations())
                .singleElement()
                .satisfies(declaration -> {
                    assertThat(declaration.providerName()).isEqualTo("app_clock");
                    assertThat(declaration.targetTypeName()).isEqualTo("Clock");
                    assertThat(declaration.interfaceId()).isEqualTo("/dev/capylang/time/Clock");
                    assertThat(declaration.sourceFile()).isEqualTo("/dev/capylang/app/Providers.cfun");
                });
    }

    @Test
    void shouldRejectUnknownNativeProviderAnnotation() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(clockObjectInterfaceModule(), new RawModule("ClockProvider", "/dev/capylang/test", """
                from /capy/lang/Effect import { Effect }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """)), new TreeSet<>(), NativeProviderManifest.empty());

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Unknown annotation NativeProvider"));
    }

    @Test
    void shouldRejectNativeProviderNameArgument() {
        var result = compileProviders("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(name: "system_clock", qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """, providerManifest(providerBinding("/dev/capylang/test/Clock", "system")));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Unknown annotation argument name for NativeProvider"));
    }

    @Test
    void shouldRejectNativeProviderAnnotationOnObjectOrientedTargets() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                nativeProviderAnnotationModule(),
                new RawModule("Clock", "/dev/capylang/test", """
                from /capy/meta_prog/NativeProvider import { NativeProvider }

                @NativeProvider(qualifier: "system")
                class ClockClass {
                }

                @NativeProvider(qualifier: "system")
                trait ClockTrait {
                    def now(zone: String): String = zone
                }
                """, SourceKind.OBJECT_ORIENTED)
        ), new TreeSet<>(), NativeProviderManifest.empty());

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message).contains("Annotation NativeProvider").contains("is not valid on class declarations"))
                .anySatisfy(message -> assertThat(message).contains("Annotation NativeProvider").contains("is not valid on trait declarations"));
    }

    @Test
    void shouldRejectNativeProviderFunctionWithParameters() {
        var result = compileProviders("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(zone: String): Effect[Clock] = <native>
                """, NativeProviderManifest.empty());

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Native provider `system_clock`")
                        .contains("must not declare parameters")
                        .contains("/dev/capylang/test/ClockProvider.cfun"));
    }

    @Test
    void shouldRejectDuplicateNativeProviderDeclarationsForSameInterfaceAndQualifier() {
        var result = compileProviders("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>

                @NativeProvider(qualifier: "system")
                fun duplicate_system_clock(): Effect[Clock] = <native>
                """, providerManifest(providerBinding("/dev/capylang/test/Clock", "system")));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Duplicate native provider declaration")
                        .contains("interface `/dev/capylang/test/Clock`")
                        .contains("qualifier `system`")
                        .contains("/dev/capylang/test/ClockProvider.cfun"));
    }

    @Test
    void shouldRejectDuplicateNativeProviderManifestEntries() {
        var result = compileProviders(clockProviderSource(), providerManifest(
                providerBinding("/dev/capylang/test/Clock", "system"),
                providerBinding("/dev/capylang/test/Clock", "system")
        ));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Duplicate native provider manifest entry")
                        .contains("/dev/capylang/test/Clock")
                        .contains("system"));
    }

    @Test
    void shouldRejectMissingNativeProviderManifestEntry() {
        var result = compileProviders(clockProviderSource(), NativeProviderManifest.empty());

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Native provider `system_clock`")
                        .contains("interface `/dev/capylang/test/Clock`")
                        .contains("no matching @NativeImplementation declaration or manifest entry")
                        .contains("/dev/capylang/test/ClockProvider.cfun"));
    }

    @Test
    void shouldRejectNativeProviderLifetimeArgument() {
        var result = compileProviders("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system", lifetime: "singleton")
                fun system_clock(): Effect[Clock] = <native>
                """, providerManifest(providerBinding("/dev/capylang/test/Clock", "system")));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Unknown annotation argument lifetime for NativeProvider"));
    }

    @Test
    void shouldRejectNativeProviderFunctionReturningNonEffect() {
        var result = compileProviders("""
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Clock = <native>
                """, providerManifest(providerBinding("/dev/capylang/test/Clock", "system")));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("TypeMismatch")
                        .contains("Native provider `system_clock`")
                        .contains("must return `Effect[Interface]"))
                .anySatisfy(message -> assertThat(message)
                        .contains("/dev/capylang/test/ClockProvider.cfun"));
    }

    @Test
    void shouldRejectNativeProviderFunctionReturningEffectOfNonInterface() {
        var result = compileProviders("""
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[String] = <native>
                """, providerManifest(providerBinding("/dev/capylang/test/Clock", "system")));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("target type `String`")
                        .contains("is not an interface"));
    }

    @Test
    void shouldRejectUnsupportedNativeProviderBackendFactory() {
        var result = compileProviders(clockProviderSource(), providerManifest(
                providerBinding("/dev/capylang/test/Clock", "system", "call")
        ));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Native provider `system_clock`")
                        .contains("unsupported java factory `call`")
                        .contains("Supported values: constructor"));
    }

    @Test
    void shouldRejectUnusedNativeProviderManifestEntries() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule("Clock", "/dev/capylang/test", """
                        interface Clock {
                            def now(zone: String): String
                        }
                        """, SourceKind.OBJECT_ORIENTED)
        ), new TreeSet<>(), providerManifest(providerBinding("/dev/capylang/test/Clock", "system")));

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("manifest entry")
                        .contains("/dev/capylang/test/Clock")
                        .contains("no matching provider declaration"));
    }

    @Test
    void shouldRoundTripCompiledProgramWithNativeProviderCatalogThroughJackson() throws Exception {
        var program = compileProviderSuccess(
                List.of(clockObjectInterfaceModule(), clockProviderModule()),
                providerManifest(providerBinding("/dev/capylang/test/Clock", "system"))
        );
        var mapper = objectMapper();

        var json = mapper.writeValueAsString(program);
        var roundTripped = mapper.readValue(json, CompiledProgram.class);

        assertThat(roundTripped.nativeProviderCatalog().declarations())
                .usingRecursiveFieldByFieldElementComparator()
                .containsExactlyElementsOf(program.nativeProviderCatalog().declarations());
        assertThat(roundTripped.nativeProviderCatalog().bindings())
                .usingRecursiveFieldByFieldElementComparator()
                .containsExactlyElementsOf(program.nativeProviderCatalog().bindings());
    }

    @Test
    void shouldTypeImportedObjectConstructionAsEffect() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                effectModule(),
                constructiblesModule(),
                new RawModule(
                        "ObjectUse",
                        "/foo/app",
                        """
                                from /capy/lang/Effect import { * }
                                from Constructibles import { Person, Printable }

                                fun make_person(name: String): Effect[Person] =
                                    Person(name)

                                fun make_printable(name: String): Effect[Printable] =
                                    Person(name)

                                fun bind_person(name: String): Effect[Person] =
                                    let person <- Person(name)
                                    person
                                """
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        var makePerson = compiledFunction(program, "ObjectUse", "make_person");
        assertThat(makePerson.returnType()).isInstanceOfSatisfying(CompiledDataParentType.class, effect ->
                assertThat(effect.typeParameters()).containsExactly("Person"));
        assertThat(makePerson.expression()).isInstanceOf(CompiledObjectConstruction.class);

        var bindPerson = compiledFunction(program, "ObjectUse", "bind_person");
        assertThat(bindPerson.expression()).isInstanceOf(CompiledEffectBindExpression.class);
    }

    @Test
    void shouldRejectPureObjectConstruction() {
        var result = compileInvalid("""
                from Constructibles import { Person }

                fun bad(name: String): Person =
                    Person(name)
                """);

        assertThat(errorMessages(result))
                .anySatisfy(message -> assertThat(message)
                        .contains("Object construction of Person returns Effect[Person], but Person was expected"));
    }

    @Test
    void shouldFallbackToCallableConstWhenTypeLikeInvokeIsNotObjectConstruction() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule("CallableConstUse", "/foo/app", """
                        const PARSER: String => String = input => input

                        fun parse(input: String): String =
                            PARSER(input)
                        """)
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
    }

    @Test
    void shouldTypeImportedObjectOrientedReflectionInfoCalls() {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                reflectionModule(),
                constructiblesModule(),
                new RawModule(
                        "ObjectUse",
                        "/foo/app",
                        """
                                from /capy/meta_prog/Reflection import { * }
                                from Constructibles import { Person, Printable, NamedTrait }

                                fun person_info(): ObjectInfo =
                                    Person.type()

                                fun printable_info(): InterfaceInfo =
                                    Printable.type()

                                fun trait_info(): TraitInfo =
                                    NamedTrait.type()
                                """
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();

        var personInfo = compiledFunction(program, "ObjectUse", "person_info");
        assertThat(personInfo.returnType().name()).endsWith("ObjectInfo");
        assertThat(personInfo.expression()).isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                assertThat(call.name()).isEqualTo("foo.app.Person.type"));

        var printableInfo = compiledFunction(program, "ObjectUse", "printable_info");
        assertThat(printableInfo.returnType().name()).endsWith("InterfaceInfo");
        assertThat(printableInfo.expression()).isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                assertThat(call.name()).isEqualTo("foo.app.Printable.type"));

        var traitInfo = compiledFunction(program, "ObjectUse", "trait_info");
        assertThat(traitInfo.returnType().name()).endsWith("TraitInfo");
        assertThat(traitInfo.expression()).isInstanceOfSatisfying(CompiledFunctionCall.class, call ->
                assertThat(call.name()).isEqualTo("foo.app.NamedTrait.type"));
    }

    @Test
    void shouldRejectInvalidObjectConstructionTargetsAndArguments() {
        assertThat(errorMessages(compileInvalid("""
                fun bad(): any =
                    Missing()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Expression is not callable, was `ANY`"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { Person }

                fun bad(): Effect[Person] =
                    Person()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Constructor `Person` expects 1 argument(s), got 0"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { Person }

                fun bad(): Effect[Person] =
                    Person(1)
                """))).anySatisfy(message -> assertThat(message)
                .contains("Expected `String`, got `int`"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { Printable }

                fun bad(): Effect[Printable] =
                    Printable()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object construction target `Printable` is a interface and is not constructible"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { NamedTrait }

                fun bad(): Effect[NamedTrait] =
                    NamedTrait()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object construction target `NamedTrait` is a trait and is not constructible"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { AbstractPerson }

                fun bad(): Effect[AbstractPerson] =
                    AbstractPerson()
                """))).anySatisfy(message -> assertThat(message)
                .contains("Object construction target `AbstractPerson` is a abstract class and is not constructible"));

        assertThat(errorMessages(compileInvalid("""
                from Constructibles import { ArrayBox }

                fun bad(): Effect[ArrayBox] =
                    ArrayBox([])
                """))).anySatisfy(message -> assertThat(message)
                .contains("Constructor parameter `names` uses OO-only type `String[]` that is not representable in .cfun"));
    }

    private Result<CompiledProgram> compileInvalid(String source) {
        return CapybaraCompiler.INSTANCE.compile(List.of(
                effectModule(),
                constructiblesModule(),
                new RawModule("InvalidObjectUse", "/foo/app", """
                        from /capy/lang/Effect import { * }
                        """ + source)
        ), new TreeSet<>());
    }

    private List<String> errorMessages(Result<CompiledProgram> result) {
        assertThat(result).isInstanceOf(Result.Error.class);
        return ((Result.Error<CompiledProgram>) result).errors().stream()
                .map(Result.Error.SingleError::message)
                .toList();
    }

    private CompiledFunction compiledFunction(CompiledProgram program, String moduleName, String functionName) {
        return program.modules().stream()
                .filter(module -> module.name().equals(moduleName))
                .flatMap(module -> module.functions().stream())
                .filter(function -> function.name().equals(functionName))
                .findFirst()
                .orElseThrow();
    }

    private Result<CompiledProgram> compileProviders(String source, NativeProviderManifest manifest) {
        return CapybaraCompiler.INSTANCE.compile(List.of(
                nativeProviderAnnotationModule(),
                nativeImplementationAnnotationModule(),
                clockObjectInterfaceModule(),
                new RawModule("ClockProvider", "/dev/capylang/test", source)
        ), new TreeSet<>(), manifest);
    }

    private CompiledProgram compileProviderSuccess(List<RawModule> modules, NativeProviderManifest manifest) {
        var allModules = new java.util.ArrayList<RawModule>();
        allModules.add(nativeProviderAnnotationModule());
        allModules.add(nativeImplementationAnnotationModule());
        allModules.addAll(modules);
        var result = CapybaraCompiler.INSTANCE.compile(allModules, new TreeSet<>(), manifest);
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError("Expected success, got " + error.errors());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private NativeProviderManifest providerManifest(NativeProviderBinding... bindings) {
        return new NativeProviderManifest(List.of(bindings));
    }

    private NativeProviderBinding providerBinding(String interfaceId, String qualifier) {
        return providerBinding(interfaceId, qualifier, "constructor");
    }

    private NativeProviderBinding providerBinding(String interfaceId, String qualifier, String javaFactory) {
        return new NativeProviderBinding(
                interfaceId,
                qualifier,
                new NativeProviderBackendBinding("dev.capylang.test.SystemClock", null, null, javaFactory),
                null,
                null
        );
    }

    private RawModule nativeImplementationModule(String qualifier) {
        return new RawModule("SystemClock", "/dev/capylang/test", """
                from /capy/meta_prog/NativeImplementation import { NativeImplementation }
                from Clock import { Clock }

                @NativeImplementation(qualifier: "%s")
                class SystemClock: Clock {
                }
                """.formatted(qualifier), SourceKind.OBJECT_ORIENTED);
    }

    private RawModule clockProviderModule() {
        return new RawModule("ClockProvider", "/dev/capylang/test", clockProviderSource());
    }

    private String clockProviderSource() {
        return """
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """;
    }

    private RawModule clockObjectInterfaceModule() {
        return new RawModule("Clock", "/dev/capylang/test", """
                interface Clock {
                    def now(zone: String): String
                }
                """, SourceKind.OBJECT_ORIENTED);
    }

    private RawModule clockInterfaceModule() {
        return new RawModule("Clock", "/dev/capylang/time", """
                interface Clock {
                    def now(zone: String): String
                }
                """, SourceKind.OBJECT_ORIENTED);
    }

    private RawModule nativeProviderAnnotationModule() {
        return new RawModule("NativeProvider", "/capy/meta_prog", """
                annotation NativeProvider on fun {
                    qualifier: String = ""
                }
                """);
    }

    private RawModule nativeImplementationAnnotationModule() {
        return new RawModule("NativeImplementation", "/capy/meta_prog", """
                annotation NativeImplementation on class {
                    qualifier: String = ""
                }
                """);
    }

    private ObjectMapper objectMapper() {
        var mapper = new ObjectMapper();
        mapper.registerModule(new Jdk8Module());
        var validator = BasicPolymorphicTypeValidator.builder()
                .allowIfSubType("dev.capylang")
                .allowIfSubType("java.util")
                .build();
        mapper.activateDefaultTyping(
                validator,
                ObjectMapper.DefaultTyping.NON_FINAL,
                JsonTypeInfo.As.PROPERTY
        );
        return mapper;
    }

    private RawModule effectModule() {
        return new RawModule("Effect", "/capy/lang", """
                union Effect[T] = UnsafeEffect[T]
                private data UnsafeEffect[T] { unsafe_thunk: () => T }
                """);
    }

    private RawModule reflectionModule() {
        return new RawModule("Reflection", "/capy/meta_prog", """
                union AnyInfo { name: String, pkg: PackageInfo } =
                    DataInfo
                    | InterfaceInfo
                    | ObjectInfo
                    | TraitInfo
                    | MethodInfo

                union AnnotationValue = AnnotationString

                data PackageInfo { name: String, path: String }
                data AnnotationString { value: String }
                data AnnotationArgumentInfo { name: String, value: AnnotationValue }
                data AnnotationInfo { name: String, pkg: PackageInfo, arguments: List[AnnotationArgumentInfo] }
                data DataInfo { name: String, pkg: PackageInfo, annotations: List[AnnotationInfo] }
                data FieldInfo { name: String, type: AnyInfo, annotations: List[AnnotationInfo] }
                data MethodInfo { name: String, pkg: PackageInfo, params: List[FieldInfo], return_type: AnyInfo, annotations: List[AnnotationInfo] }
                data InterfaceInfo { methods: List[MethodInfo], parents: Set[AnyInfo], annotations: List[AnnotationInfo] }
                data ObjectInfo { open: bool, fields: List[FieldInfo], methods: List[MethodInfo], parents: Set[AnyInfo], annotations: List[AnnotationInfo] }
                data TraitInfo { methods: List[MethodInfo], parents: Set[AnyInfo], annotations: List[AnnotationInfo] }
                """);
    }

    private RawModule constructiblesModule() {
        return new RawModule(
                "Constructibles",
                "/foo/app",
                """
                        interface Printable {
                            def label(): String
                        }

                        trait NamedTrait {
                            def label(): String = "trait"
                        }

                        abstract class AbstractPerson {
                        }

                        class Person(name: String): Printable {
                            field name: String = name

                            override def label(): String = this.name
                        }

                        class ArrayBox(names: String[]) {
                            field names: String[] = names
                        }
                        """,
                SourceKind.OBJECT_ORIENTED
        );
    }

}
