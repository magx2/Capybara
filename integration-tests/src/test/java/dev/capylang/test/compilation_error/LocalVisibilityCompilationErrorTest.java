package dev.capylang.test.compilation_error;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.ImportDeclaration;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;

class LocalVisibilityCompilationErrorTest {
    private static final String SUPPORT_MODULE_NAME = "LocalVisibilityBase";
    private static final String SUPPORT_MODULE_IMPORT = "/foo/boo/internal/LocalVisibilityBase";
    private static final RawModule LOCAL_SUPPORT_MODULE = new RawModule(
            SUPPORT_MODULE_NAME,
            "/foo/boo/internal",
            """
                    local fun local_add(x: int): int = x + 1
                    local const LOCAL_OFFSET: int = 5
                    local type LocalValue = LocalNumber
                    local data LocalNumber { value: int }
                    fun public_value(x: int): int = local_add(x) + LOCAL_OFFSET
                    """
    );

    @ParameterizedTest(name = "{index}: should fail when local visibility is violated in `{0}.cfun`")
    @MethodSource
    void compilationError(
            String moduleName,
            String code,
            List<ImportDeclaration> imports,
            String expectedFile,
            String errorMessageFragment
    ) {
        var errors = compileProgram(code, moduleName, imports);

        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll("Assertions on error",
                () -> assertThat(error.file()).isEqualTo(expectedFile),
                () -> assertThat(error.message()).contains(errorMessageFragment));
    }

    static Stream<Arguments> compilationError() {
        return Stream.of(
                Arguments.of(
                        "local_function_not_visible_from_parent_package",
                        """
                                fun foo(): int =
                                    1
                                """,
                        List.of(new ImportDeclaration(SUPPORT_MODULE_IMPORT, List.of("local_add"), List.of())),
                        "/foo/boo/local_function_not_visible_from_parent_package.cfun",
                        "imports unknown symbol `local_add` from module `" + SUPPORT_MODULE_IMPORT + "`"
                ),
                Arguments.of(
                        "local_type_not_visible_from_parent_package",
                        """
                                fun foo(): int =
                                    1
                                """,
                        List.of(new ImportDeclaration(SUPPORT_MODULE_IMPORT, List.of("LocalNumber"), List.of())),
                        "/foo/boo/local_type_not_visible_from_parent_package.cfun",
                        "imports unknown symbol `LocalNumber` from module `" + SUPPORT_MODULE_IMPORT + "`"
                )
        );
    }

    private static SortedSet<Result.Error.SingleError> compileProgram(
            String fun,
            String moduleName,
            List<ImportDeclaration> imports
    ) {
        var rawModules = new ArrayList<RawModule>();
        rawModules.add(LOCAL_SUPPORT_MODULE);
        rawModules.add(new RawModule(
                moduleName,
                "/foo/boo",
                prependImports(imports, fun)
        ));
        var programResult = CapybaraCompiler.INSTANCE.compile(rawModules, new java.util.TreeSet<>());
        if (programResult instanceof Result.Success<CompiledProgram> value) {
            throw new AssertionError("Expected compilation error but got CompiledProgram: " + value);
        }
        return ((Result.Error<?>) programResult).errors();
    }

    private static String prependImports(List<ImportDeclaration> imports, String code) {
        var importLines = imports.stream()
                .map(importDeclaration -> "from %s import { %s }".formatted(importDeclaration.moduleName(), String.join(", ", importDeclaration.symbols())))
                .toList();
        return String.join("\n", java.util.stream.Stream.concat(importLines.stream(), java.util.stream.Stream.of(code)).toList());
    }
}
