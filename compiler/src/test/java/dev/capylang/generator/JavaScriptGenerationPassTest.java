package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderBinding;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.generator.JavaScriptGenerationPass;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;
import java.util.TreeSet;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;

class JavaScriptGenerationPassTest {
    @Test
    void shouldPlanJavaScriptFilesAndRenderNativeProviderBootstrapInCapybara() {
        List<List<?>> rendered = List.of(JavaScriptGenerationPass.generatedJavascriptFile("foo/Main.js", "'use strict';\n"));
        List<List<?>> providers = List.of(JavaScriptGenerationPass.javascriptNativeProviderInfo(
                "system_clock",
                "system_clock",
                "/Providers.Clock",
                "system",
                "/ProvidersNative.cfun",
                "host-clock",
                "SystemClock",
                "new",
                List.of(JavaScriptGenerationPass.javascriptNativeProviderMethodInfo("now_millis", 0))
        ));

        var plan = JavaScriptGenerationPass.planJavascriptGeneration(rendered, providers);

        assertThat(plan)
                .extracting(JavaScriptGenerationPassTest::generatedFilePath)
                .containsExactly("foo/Main.js", "dev/capylang/native_providers.js");
        assertThat(generatedFileCode(plan.getLast())).isEqualTo("""
                'use strict';

                const capy = require('./capybara.js');
                const __capy_provider_system_clock_module = capy.requireNativeProviderModule('host-clock', __filename, { interfaceId: '/Providers.Clock', qualifier: 'system', providerSymbol: 'system_clock', backend: 'javascript', sourceFile: '/ProvidersNative.cfun', moduleName: 'host-clock', exportName: 'SystemClock' });

                const providers = capy.defineNativeProviders({
                    '/Providers.Clock#system': capy.nativeFactory({
                        interfaceId: '/Providers.Clock',
                        qualifier: 'system',
                        providerSymbol: 'system_clock',
                        backend: 'javascript',
                        sourceFile: '/ProvidersNative.cfun',
                        moduleName: 'host-clock',
                        exportName: 'SystemClock',
                        exportExists: Object.prototype.hasOwnProperty.call(__capy_provider_system_clock_module, 'SystemClock'),
                        exportValue: __capy_provider_system_clock_module.SystemClock,
                        factory: 'new',
                        metadata: {
                            methods: [{ name: 'now_millis', arity: 0 }]
                        },
                        create: () => new __capy_provider_system_clock_module.SystemClock()
                    }),
                });

                function system_clock() {
                    return providers.resolve('/Providers.Clock', 'system', 'system_clock', 'javascript', '/ProvidersNative.cfun');
                }

                module.exports = {
                    system_clock,
                };
                """);
    }

    @Test
    void shouldMatchLegacyJavaScriptGeneratorOutputInTransitionParityMode() {
        var program = compileProgram(List.of(
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
                new RawModule("ProvidersNative", "", """
                        from /capy/lang/Effect import { Effect }
                        from /capy/meta_prog/NativeProvider import { NativeProvider }
                        from Providers import { Clock }

                        @NativeProvider(qualifier: "system")
                        fun system_clock(): Effect[Clock] = <native>
                        """),
                new RawModule(
                        "ClockConsumer",
                        "",
                        """
                                from Providers import { Clock }
                                from ProvidersNative import { system_clock }

                                class ClockConsumer {
                                    def current(): Clock = system_clock()
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new NativeProviderManifest(List.of(new NativeProviderBinding(
                "/Providers.Clock",
                "system",
                null,
                new NativeProviderBackendBinding(null, "../../nativeinterop/system_clock.js", "SystemClock", "new"),
                null
        ))));

        var capybaraPlannedOutput = new JavaScriptGenerator().generate(program);
        var legacyOutput = JavaScriptGenerator.legacyParityMode().generate(program);

        assertThat(normalize(capybaraPlannedOutput.modules())).containsExactlyElementsOf(normalize(legacyOutput.modules()));
        assertThat(capybaraPlannedOutput.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(Path.of("dev", "capylang", "native_providers.js"));
    }

    private static CompiledProgram compileProgram(List<RawModule> modules, NativeProviderManifest nativeProviders) {
        var allModules = new java.util.ArrayList<RawModule>();
        allModules.add(new RawModule("NativeProvider", "/capy/meta_prog", """
                annotation NativeProvider on fun {
                    qualifier: String = ""
                }
                """));
        allModules.addAll(modules);
        var result = CapybaraCompiler.INSTANCE.compile(allModules, new TreeSet<>(), nativeProviders);
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().stream()
                    .map(Result.Error.SingleError::message)
                    .collect(joining(", ")));
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static List<GeneratedModule> normalize(List<GeneratedModule> modules) {
        return modules.stream()
                .map(module -> new GeneratedModule(module.relativePath(), module.code().replace("\r\n", "\n")))
                .toList();
    }

    private static String generatedFilePath(List<?> file) {
        return JavaScriptGenerationPass.generatedJavascriptFileRelativePath(file);
    }

    private static String generatedFileCode(List<?> file) {
        return JavaScriptGenerationPass.generatedJavascriptFileCode(file);
    }
}
