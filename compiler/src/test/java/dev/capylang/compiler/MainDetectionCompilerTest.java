package dev.capylang.compiler;

import capy.lang.Result;

import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class MainDetectionCompilerTest {
    @Test
    void shouldMarkOnlyEffectProgramMainAsProgramMain() {
        var program = compileFunctional("""
                from /capy/lang/Effect import { * }
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }

                fun main(args: List[String]): Effect[/capy/lang/Program] =
                    pure(/capy/lang/Program.Success {})
                """);

        assertThat(function(program, "main").programMain()).isTrue();
    }

    @Test
    void shouldMarkEffectMainWithImportedProgramAliasAsProgramMain() {
        var program = compileFunctional("""
                from /capy/lang/Effect import { * }
                from /capy/lang/Program import { * }
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }

                fun main(args: List[String]): Effect[Program] =
                    pure(Success {})
                """);

        assertThat(function(program, "main").programMain()).isTrue();
    }

    @Test
    void shouldNotMarkDirectProgramReturnAsProgramMain() {
        var program = compileFunctional("""
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }

                fun main(args: List[String]): /capy/lang/Program =
                    /capy/lang/Program.Success {}
                """);

        assertThat(function(program, "main").programMain()).isFalse();
    }

    @Test
    void shouldNotMarkMainWithWrongArgumentsAsProgramMain() {
        var program = compileFunctional("""
                from /capy/lang/Effect import { * }
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }

                fun main(args: List[int]): Effect[/capy/lang/Program] =
                    pure(/capy/lang/Program.Success {})
                """);

        assertThat(function(program, "main").programMain()).isFalse();
    }

    @Test
    void shouldRejectSplitProgramMainSignatureAcrossOverloads() {
        var error = compileFailure("""
                from /capy/lang/Effect import { * }
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }

                fun main(args: List[String]): /capy/lang/Program =
                    /capy/lang/Program.Success {}

                fun main(args: List[int]): Effect[/capy/lang/Program] =
                    pure(/capy/lang/Program.Success {})
                """);

        assertThat(error.message())
                .contains("Invalid overloaded main functions")
                .contains("fun main(args: List[String]): Effect[/capy/lang/Program]");
    }

    @Test
    void shouldNotMarkOldCapLangMainAsProgramMain() {
        var program = compileModules(List.of(
                new RawModule("Program", "/cap/lang", """
                        union Program = Success
                        data Success {}
                        """, SourceKind.FUNCTIONAL),
                new RawModule("Effect", "/cap/lang", """
                        union Effect[T] = UnsafeEffect[T]
                        private data UnsafeEffect[T] { unsafe_thunk: () => T }

                        fun pure(value: T): Effect[T] =
                            UnsafeEffect { unsafe_thunk: () => value }
                        """, SourceKind.FUNCTIONAL),
                new RawModule("MainDetection", "/foo/bar", """
                        from /cap/lang/Effect import { * }
                        from /capy/collection/List import { * }
                        from /capy/collection/Set import { * }
                        from /capy/collection/Dict import { * }

                        fun main(args: List[String]): /cap/lang/Effect[/cap/lang/Program] =
                            pure(/cap/lang/Program.Success {})
                        """, SourceKind.FUNCTIONAL)
        ));

        assertThat(function(program, "main").programMain()).isFalse();
    }

    @Test
    void shouldNotTreatObjectOrientedMainAsProgramMain() {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule(
                        "App",
                        "/foo/bar",
                        """
                                class App {
                                    def main(args: List[String]): int = args.size()
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )),
                new TreeSet<>()
        );

        assertThat(result).isInstanceOf(Result.Success.class);
        var program = ((Result.Success<CompiledProgram>) result).value();
        assertThat(program.modules())
                .flatExtracting(CompiledModule::functions)
                .noneMatch(CompiledFunction::programMain);
    }

    private static CompiledProgram compileFunctional(String source) {
        return compileModules(List.of(new RawModule("MainDetection", "/foo/bar", source, SourceKind.FUNCTIONAL)));
    }

    private static CompiledProgram compileModules(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(
                modules,
                new TreeSet<>()
        );
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(CompilerErrors.from(error).toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private static CompilerError compileFailure(String source) {
        var result = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("MainDetection", "/foo/bar", source, SourceKind.FUNCTIONAL)),
                new TreeSet<>()
        );
        assertThat(result).isInstanceOf(Result.Error.class);
        return CompilerErrors.from((Result.Error<CompiledProgram>) result).first();
    }

    private static CompiledFunction function(CompiledProgram program, String name) {
        return program.modules().stream()
                .flatMap(module -> module.functions().stream())
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }
}
