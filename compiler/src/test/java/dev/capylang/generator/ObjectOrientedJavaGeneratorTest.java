package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.TreeSet;

import static java.util.stream.Collectors.joining;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class ObjectOrientedJavaGeneratorTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldGenerateAndRunJavaForObjectOrientedClasses() throws Exception {
        var program = compileProgram("""
                open class Base {
                    open def label(): string = "base"
                }

                interface Printable {
                    def print(): string
                }

                class User(name: string): Base, Printable {
                    field name: string = name

                    def greet(): string = "Hello " + this.name

                    override def print(): string {
                        let label: string = super[Base].label()
                        return label + " " + this.name
                    }
                }
                """);

        var generatedProgram = new JavaGenerator().generate(program);

        assertThat(generatedProgram.modules())
                .extracting(GeneratedModule::relativePath)
                .contains(
                        Path.of("foo", "boo", "Base.java"),
                        Path.of("foo", "boo", "Printable.java"),
                        Path.of("foo", "boo", "User.java")
                );
        assertThat(generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("User.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("public final class User extends Base implements Printable")
                .contains("public String greet()")
                .contains("return \"Hello \"+this.name;")
                .contains("String label = super.label();");

        var classesDir = compileGeneratedJava(generatedProgram);
        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL()})) {
            var userType = classLoader.loadClass("foo.boo.User");
            var constructor = userType.getConstructor(String.class);
            var user = constructor.newInstance("Capy");

            assertThat(userType.getMethod("greet").invoke(user)).isEqualTo("Hello Capy");
            assertThat(userType.getMethod("print").invoke(user)).isEqualTo("base Capy");
        }
    }

    @Test
    void shouldRejectTraitStateInJavaBackendV1() {
        var program = compileProgram("""
                trait Named {
                    field name: string = "Capy"
                }
                """);

        assertThatThrownBy(() -> new JavaGenerator().generate(program))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Trait fields are not supported");
    }

    private CompiledProgram compileProgram(String source) {
        var result = CapybaraCompiler.INSTANCE.compile(List.of(
                new RawModule("User", "/foo/boo", source, SourceKind.OBJECT_ORIENTED)
        ), new TreeSet<>());
        if (result instanceof Result.Error<CompiledProgram> error) {
            throw new AssertionError(error.errors().toString());
        }
        return ((Result.Success<CompiledProgram>) result).value();
    }

    private Path compileGeneratedJava(GeneratedProgram generatedProgram) throws Exception {
        var sourceDir = tempDir.resolve("generated");
        var classesDir = tempDir.resolve("classes");
        for (var module : generatedProgram.modules()) {
            var path = sourceDir.resolve(module.relativePath());
            Files.createDirectories(path.getParent());
            Files.writeString(path, module.code(), StandardCharsets.UTF_8);
        }

        var compiler = ToolProvider.getSystemJavaCompiler();
        assertThat(compiler).isNotNull();
        Files.createDirectories(classesDir);
        List<Path> javaFiles;
        try (var files = Files.walk(sourceDir)) {
            javaFiles = files.filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".java"))
                    .toList();
        }
        var diagnostics = new DiagnosticCollector<JavaFileObject>();
        try (var fileManager = compiler.getStandardFileManager(diagnostics, Locale.ROOT, StandardCharsets.UTF_8)) {
            var compilationUnits = fileManager.getJavaFileObjectsFromFiles(javaFiles.stream().map(Path::toFile).toList());
            var options = List.of(
                    "--release", "21",
                    "-classpath", System.getProperty("java.class.path"),
                    "-d", classesDir.toString()
            );
            var success = compiler.getTask(null, fileManager, diagnostics, options, null, compilationUnits).call();
            assertThat(success)
                    .as(diagnostics.getDiagnostics().stream().map(Objects::toString).collect(joining(System.lineSeparator())))
                    .isTrue();
        }
        return classesDir;
    }
}
