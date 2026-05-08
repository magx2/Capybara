package dev.capylang.test.compilation_error;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class LocalConstCompilationErrorTest {
    @Test
    void localConstRequiresPrivatePrefix() {
        var programResult = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("LocalConst", "/foo/boo", """
                        fun foo(x: String): bool =
                            const white_space = 1
                            ---
                            x == ""
                        """)),
                new TreeSet<>()
        );

        assertThat(programResult).isInstanceOf(Result.Error.class);
        assertThat(((Result.Error<CompiledProgram>) programResult).errors())
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/LocalConst.cfun");
                    assertThat(error.line()).isEqualTo(2);
                    assertThat(error.column()).isEqualTo(4);
                    assertThat(error.message()).contains("Local const name has to start with `__` and use a private identifier style: white_space");
                });
    }
}
