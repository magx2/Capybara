package dev.capylang;

import capy.lang.Either;
import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompilerError;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

/** Executes source-level programs which are expected to be rejected before generation. */
class BadCodeCompilationTest {
    @TestFactory
    Stream<DynamicTest> rejectsBadCapybaraPrograms() throws IOException {
        return Stream.concat(cases(Path.of("src/bad-cfun")), cases(Path.of("src/bad-coo")))
                .map(directory -> DynamicTest.dynamicTest(directory.toString(), () -> assertBadCase(directory)));
    }

    private Stream<Path> cases(Path root) throws IOException {
        if (!Files.isDirectory(root)) return Stream.empty();
        try (var paths = Files.list(root)) {
            return paths.filter(Files::isDirectory).sorted().toList().stream();
        }
    }

    private void assertBadCase(Path directory) throws IOException {
        var rawModules = new ArrayList<RawModule>();
        try (var files = Files.walk(directory)) {
            for (var file : files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".cfun") || path.toString().endsWith(".coo"))
                    .sorted().toList()) {
                var relative = directory.relativize(file);
                var parent = relative.getParent();
                var path = parent == null ? "" : parent.toString().replace('\\', '/');
                var filename = file.getFileName().toString();
                var extension = filename.endsWith(".coo") ? ".coo" : ".cfun";
                rawModules.add(new RawModule(
                        filename.substring(0, filename.length() - extension.length()),
                        path,
                        Files.readString(file, StandardCharsets.UTF_8),
                        extension.equals(".coo") ? SourceKind.OBJECT_ORIENTED : SourceKind.FUNCTIONAL
                ));
            }
        }

        var result = CapybaraCompiler.compile(
                List.copyOf(rawModules),
                new LinkedHashSet<>(),
                new NativeProviderManifest(List.of()),
                new NativeProviderManifest(List.of())
        ).unsafeRun();

        assertThat(result).as("bad source must fail during compilation").isInstanceOf(Either.Right.class);
        @SuppressWarnings("unchecked")
        var diagnostics = (List<CompilerError>) ((Either.Right<?, ?>) result).value();
        var actual = diagnostics.stream()
                .sorted(Comparator.comparing(CompilerError::moduleName)
                        .thenComparingInt(CompilerError::line)
                        .thenComparingInt(CompilerError::column)
                        .thenComparing(CompilerError::code))
                .map(BadCodeCompilationTest::format)
                .reduce((left, right) -> left + "\n" + right)
                .orElse("");
        var expected = Files.readString(directory.resolve("expected.errors"), StandardCharsets.UTF_8)
                .replace("\r\n", "\n").strip();
        assertThat(actual).isEqualTo(expected);
    }

    private static String format(CompilerError error) {
        return error.code() + "|" + error.moduleName().replace('\\', '/') + "|"
                + error.line() + "|" + error.column() + "|" + error.message();
    }
}
