package dev.capylang.compiler;

import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
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
                                    def greet(): string = "hello"
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
                                    def format(name: string): string = name

                                    def print(value: string): void {
                                    }

                                    def greet(name: string): void {
                                        print(format(name))
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ), new TreeSet<>());

        assertThat(result).isInstanceOf(Result.Success.class);
    }

}
