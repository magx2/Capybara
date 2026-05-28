package dev.capylang.compiler;

import dev.capylang.compiler.linking.AnnotationValidationPass;
import dev.capylang.compiler.linking.NativeProviderValidationPass;
import dev.capylang.compiler.linking.TypeLinkingPass;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ValidationPassParityTest {
    private static final String TYPE_PASS = "capybara.compiler.useCapybaraTypeLinkingPass";
    private static final String ANNOTATION_PASS = "capybara.compiler.useCapybaraAnnotationValidationPass";
    private static final String NATIVE_PROVIDER_PASS = "capybara.compiler.useCapybaraNativeProviderValidationPass";

    @Test
    void shouldExposePureTypeLinkingHelpers() {
        assertThat(TypeLinkingPass.parseTypeArgument("List[Tuple[String, int -> bool]]"))
                .isEqualTo("List[Tuple[String, (int => bool)]]");
        assertThat(TypeLinkingPass.splitTopLevelTypeArguments("String, List[int], Tuple[String, bool]"))
                .containsExactly("String", "List[int]", "Tuple[String, bool]");

        var resolution = TypeLinkingPass.resolveDataType(
                "Box[List[String]]",
                List.of("/capy/test/Box.Box", "Local"),
                List.of("/capy/test/Box.Box", "Local")
        );
        assertThat(resolution).isEqualTo(List.of(true, "/capy/test/Box.Box", "Box", List.of("List[String]"), ""));
    }

    @Test
    void shouldMatchLegacyCompileOutputForGenericTypeLinking() {
        var modules = List.of(
                new RawModule("Types", "/capy/test", """
                        data Box[T] { value: T }
                        data User { name: String }
                        """),
                new RawModule("UseTypes", "/capy/app", """
                        from /capy/test/Types import { Box, User }

                        data Envelope { value: Box[List[User]] }
                        """)
        );

        assertSameCompileResult(modules, NativeProviderManifest.empty(), TYPE_PASS);
    }

    @Test
    void shouldExposePureAnnotationValidationHelpers() {
        var annotation = AnnotationValidationPass.annotationDeclaration(
                "Retry",
                List.of(AnnotationValidationPass.annotationTarget("fun", 1, 20)),
                List.of(AnnotationValidationPass.annotationField("retries", "int", true, "STRING", 2, 4, 2, 19)),
                false,
                "Annotations",
                "/capy/test"
        );

        assertThat(AnnotationValidationPass.validateAnnotationDefinition(annotation))
                .singleElement()
                .satisfies(error -> assertThat(error)
                        .isEqualTo(List.of(2, 19, "Annotation argument retries for Retry expects int, got String")));
    }

    @Test
    void shouldMatchLegacyDiagnosticsForAnnotationValidation() {
        var modules = List.of(new RawModule("Tests", "/capy/test", """
                annotation Retry on fun {
                    retries: int
                }

                @Retry(retries: "three")
                fun run(): bool = true
                """));

        assertSameCompileResult(modules, NativeProviderManifest.empty(), ANNOTATION_PASS);
    }

    @Test
    void shouldExposePureNativeProviderValidationHelpers() {
        var provider = NativeProviderValidationPass.providerDeclaration(
                "system_clock",
                "Clock",
                "system",
                "/capy/test/ClockProvider.cfun",
                0,
                true,
                true,
                "/capy/test/Clock",
                "INTERFACE",
                "ClockProvider"
        );

        assertThat(NativeProviderValidationPass.validateProviderDeclaration(provider, List.of(), List.of()))
                .isEqualTo(List.of(true, ""));
        assertThat(NativeProviderValidationPass.callsProviderWithArguments("system_clock(\"utc\")", "system_clock"))
                .isTrue();
        assertThat(NativeProviderValidationPass.callsProviderWithArguments("clock.system_clock(\"utc\")", "system_clock"))
                .isFalse();
    }

    @Test
    void shouldMatchLegacyNativeProviderCatalogValidation() {
        var modules = List.of(
                nativeProviderAnnotationModule(),
                clockObjectInterfaceModule(),
                clockProviderModule()
        );

        assertSameCompileResult(
                modules,
                providerManifest(providerBinding("/dev/capylang/test/Clock", "system")),
                NATIVE_PROVIDER_PASS
        );
    }

    @Test
    void shouldMatchLegacyNativeProviderDiagnostics() {
        var modules = List.of(
                nativeProviderAnnotationModule(),
                clockObjectInterfaceModule(),
                new RawModule("ClockProvider", "/dev/capylang/test", """
                        from /capy/lang/Effect import { Effect }
                        from /capy/meta_prog/NativeProvider import { NativeProvider }
                        from Clock import { Clock }

                        @NativeProvider(qualifier: "system")
                        fun system_clock(): Effect[Clock] = <native>

                        @NativeProvider(qualifier: "system")
                        fun duplicate_system_clock(): Effect[Clock] = <native>
                        """)
        );

        assertSameCompileResult(
                modules,
                providerManifest(providerBinding("/dev/capylang/test/Clock", "system")),
                NATIVE_PROVIDER_PASS
        );
    }

    @Test
    void shouldAllowNativeProviderSymbolShadowingByLocalNames() {
        var result = compileWithProperty(
                List.of(
                        nativeProviderAnnotationModule(),
                        clockObjectInterfaceModule(),
                        clockProviderModule(),
                        new RawModule("ClockConsumer", "/dev/capylang/test", """
                                from ClockProvider import { system_clock }

                                class ClockConsumer {
                                    def value(system_clock: String): String = system_clock("utc")
                                }
                                """, SourceKind.OBJECT_ORIENTED)
                ),
                providerManifest(providerBinding("/dev/capylang/test/Clock", "system")),
                Map.of(NATIVE_PROVIDER_PASS, "true")
        );

        assertThat(result).isInstanceOf(Result.Success.class);
    }

    private static void assertSameCompileResult(List<RawModule> modules, NativeProviderManifest manifest, String property) {
        var capybara = compileWithProperty(modules, manifest, Map.of(property, "true"));
        var legacy = compileWithProperty(modules, manifest, Map.of(property, "false"));
        assertThat(resultSummary(capybara)).isEqualTo(resultSummary(legacy));
    }

    private static Result<CompiledProgram> compileWithProperty(
            List<RawModule> modules,
            NativeProviderManifest manifest,
            Map<String, String> properties
    ) {
        var previous = new java.util.LinkedHashMap<String, String>();
        properties.keySet().forEach(property -> previous.put(property, System.getProperty(property)));
        try {
            properties.forEach(System::setProperty);
            return CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>(), manifest);
        } finally {
            previous.forEach((property, value) -> {
                if (value == null) {
                    System.clearProperty(property);
                } else {
                    System.setProperty(property, value);
                }
            });
        }
    }

    private static Object resultSummary(Result<CompiledProgram> result) {
        if (result instanceof Result.Error<CompiledProgram> error) {
            return error.errors().stream()
                    .map(single -> single.file() + ":" + single.line() + ":" + single.column() + ":" + single.message())
                    .toList();
        }
        var program = ((Result.Success<CompiledProgram>) result).value();
        return new ProgramSummary(
                program.modules().stream()
                        .map(module -> new ModuleSummary(
                                module.name(),
                                module.path(),
                                module.types().keySet().stream().toList(),
                                module.functions().stream().map(CompiledFunction::name).toList()
                        ))
                        .toList(),
                program.nativeProviderCatalog().declarations().stream()
                        .map(declaration -> declaration.providerName() + ":" + declaration.interfaceId() + ":" + declaration.qualifier())
                        .toList(),
                program.nativeProviderCatalog().bindings().stream()
                        .map(binding -> binding.interfaceId() + ":" + binding.qualifier())
                        .toList()
        );
    }

    private static RawModule nativeProviderAnnotationModule() {
        return new RawModule("NativeProvider", "/capy/meta_prog", """
                annotation NativeProvider on fun {
                    qualifier: String = ""
                }
                """);
    }

    private static RawModule clockProviderModule() {
        return new RawModule("ClockProvider", "/dev/capylang/test", """
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """);
    }

    private static RawModule clockObjectInterfaceModule() {
        return new RawModule("Clock", "/dev/capylang/test", """
                interface Clock {
                    def now(zone: String): String
                }
                """, SourceKind.OBJECT_ORIENTED);
    }

    private static NativeProviderManifest providerManifest(NativeProviderBinding... bindings) {
        return new NativeProviderManifest(List.of(bindings));
    }

    private static NativeProviderBinding providerBinding(String interfaceId, String qualifier) {
        return new NativeProviderBinding(
                interfaceId,
                qualifier,
                new NativeProviderBackendBinding("dev.capylang.test.SystemClock", null, null, "constructor"),
                null,
                null
        );
    }

    private record ProgramSummary(List<ModuleSummary> modules, List<String> nativeDeclarations, List<String> nativeBindings) {
    }

    private record ModuleSummary(String name, String path, List<String> typeNames, List<String> functionNames) {
    }
}
