package dev.capylang.compiler.compilation_error;

import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompilerErrors;
import dev.capylang.compiler.CompiledProgram;
import capy.lang.Result;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class LocalConstCompilationErrorTest {
    @Test
    void duplicateLocalConstReportsLocations() {
        var programResult = CapybaraCompiler.INSTANCE.compile(
                List.of(new RawModule("LocalConst", "/foo/boo", """
                        fun foo(): int =
                            const white_space = 1
                            const white_space = 2
                            ---
                            white_space
                        """, SourceKind.FUNCTIONAL)),
                new TreeSet<>()
        );

        assertThat(programResult).isInstanceOf(Result.Error.class);
        assertThat(CompilerErrors.from((Result.Error<CompiledProgram>) programResult))
                .singleElement()
                .satisfies(error -> {
                    assertThat(error.file()).isEqualTo("/foo/boo/LocalConst.cfun");
                    assertThat(error.line()).isEqualTo(2);
                    assertThat(error.column()).isEqualTo(4);
                    assertThat(error.message()).contains("Duplicate local const name: white_space. Declared at: 2:4, 3:4");
                });
    }
}
