package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderBinding;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.generator.JavaGenerationPass;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class JavaGenerationPassTest {
    @Test
    void shouldPlanJavaFilesAndRenderNativeProviderBootstrapInCapybara() {
        List<List<?>> rendered = List.of(JavaGenerationPass.generatedJavaFile("foo/Main.java", "class Main {}\n"));
        List<List<?>> providers = List.of(JavaGenerationPass.javaNativeProviderInfo(
                "system_clock",
                "system_clock",
                "/foo/Clock",
                "system",
                "foo.Clock",
                "/foo/Providers.cfun",
                "dev.capylang.test.SystemClock",
                "constructor"
        ));

        var plan = JavaGenerationPass.planJavaGeneration(rendered, providers);

        assertThat(plan)
                .extracting(JavaGenerationPassTest::generatedFilePath)
                .containsExactly("foo/Main.java", "dev/capylang/NativeProviderBootstrap.java");
        assertThat(generatedFileCode(plan.getLast())).isEqualTo("""
                package dev.capylang;

                @javax.annotation.processing.Generated("Capybara Compiler")
                public final class NativeProviderBootstrap {
                    private static final NativeProviders PROVIDERS = NativeProviders.of(
                            NativeProviders.factory(
                                    "/foo/Clock",
                                    "system",
                                    "system_clock",
                                    "java",
                                    "/foo/Providers.cfun",
                                    foo.Clock.class,
                                    dev.capylang.test.SystemClock::new
                            )
                    );

                    private NativeProviderBootstrap() {
                    }

                    public static foo.Clock system_clock() {
                        return PROVIDERS.resolve("/foo/Clock", "system", "system_clock", "java", "/foo/Providers.cfun", foo.Clock.class);
                    }

                }
                """);
    }

    @Test
    void shouldMatchLegacyJavaGeneratorOutputInTransitionParityMode() {
        var program = compileProgram(List.of(
                new RawModule(
                        "Providers",
                        "/foo/boo",
                        """
                                interface Clock {
                                    def now_millis(): long
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                ),
                new RawModule(
                        "ProvidersNative",
                        "/foo/boo",
                        """
                                from /capy/lang/Effect import { Effect }
                                from /capy/meta_prog/NativeProvider import { NativeProvider }
                                from Providers import { Clock }

                                @NativeProvider(qualifier: "system")
                                fun system_clock(): Effect[Clock] = <native>
                                """
                ),
                new RawModule(
                        "ClockConsumer",
                        "/foo/boo",
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
                "/foo/boo/Providers.Clock",
                "system",
                new NativeProviderBackendBinding("dev.capylang.test.SystemClock", null, null, "constructor"),
                null,
                null
        ))));

        var capybaraPlannedOutput = new JavaGenerator().generate(program);
        var legacyOutput = JavaGenerator.legacyParityMode().generate(program);

        assertThat(capybaraPlannedOutput.modules()).containsExactlyElementsOf(legacyOutput.modules());
        assertThat(capybaraPlannedOutput.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(Path.of("dev", "capylang", "NativeProviderBootstrap.java"));
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
            throw new AssertionError(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static String generatedFilePath(List<?> file) {
        return JavaGenerationPass.generatedJavaFileRelativePath(file);
    }

    private static String generatedFileCode(List<?> file) {
        return JavaGenerationPass.generatedJavaFileCode(file);
    }
}
