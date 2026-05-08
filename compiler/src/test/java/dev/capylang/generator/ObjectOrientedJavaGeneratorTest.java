package dev.capylang.generator;

import dev.capylang.compiler.CapybaraCompiler;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.ResourceLock;

import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
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

@ResourceLock("java.lang.System.out")
class ObjectOrientedJavaGeneratorTest {
    @TempDir
    Path tempDir;

    @Test
    void shouldGenerateAndRunStdoutCallsInExpressionAndBlockMethods() throws Exception {
        var program = compileProgram("""
                from /capy/io/Stdout import { * }

                class Speaker(name: String) {
                    field name: String = name

                    def emit_inline(): void = println(this.name)

                    def emit_block(): void {
                        print("[")
                        print(this.name)
                        println("]")
                    }

                    def emit_star(): void = print("*")
                }
                """);

        var generatedProgram = new JavaGenerator().generate(program);
        var speakerModule = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("Speaker.java"))
                .findFirst()
                .orElseThrow();

        assertThat(speakerModule.code())
                .doesNotContain("import static capy.io.Stdout.*;")
                .contains("capy.io.Stdout.println(this.name);")
                .contains("capy.io.Stdout.print(\"[\");")
                .contains("capy.io.Stdout.print(\"*\");");

        var classesDir = compileGeneratedJava(generatedProgram);
        var capybaraLibClasses = Path.of("..", "lib", "capybara-lib", "build", "classes", "java", "main").normalize().toAbsolutePath();
        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL(), capybaraLibClasses.toUri().toURL()})) {
            var speakerType = classLoader.loadClass("foo.boo.Speaker");
            var speaker = speakerType.getConstructor(String.class).newInstance("Capy");
            var originalOut = System.out;
            var out = new ByteArrayOutputStream();
            try {
                System.setOut(new PrintStream(out, true, StandardCharsets.UTF_8));
                speakerType.getMethod("emit_inline").invoke(speaker);
                speakerType.getMethod("emit_block").invoke(speaker);
                speakerType.getMethod("emit_star").invoke(speaker);
            } finally {
                System.setOut(originalOut);
            }

            var normalized = out.toString(StandardCharsets.UTF_8).replace("\r\n", "\n");
            assertThat(normalized).isEqualTo("Capy\n[Capy]\n*");
        }
    }

    @Test
    void shouldGenerateAndRunJavaForObjectOrientedClasses() throws Exception {
        var program = compileProgram("""
                /// Base type
                open class Base {
                    open def label(): String = "base"
                }

                /// Printable contract
                interface Printable {
                    def print(): String
                }

                /// User type
                class User(name: String): Base, Printable {
                    field name: String = name

                    /// Friendly greet
                    def greet(): String = "Hello " + this.name

                    def mutable(): String {
                        def x = "a"
                        x = "2"
                        return x
                    }

                    override def print(): String {
                        let label: String = Base.label()
                        return label + " " + this.name
                    }

                    def first_positive(values: List[int]): int {
                        for value in values {
                            if value > 0 {
                                return value
                            }
                        }
                        return 0
                    }

                    def first_large(values: List[int]): int {
                        foreach value: int in values {
                            if value > 10 {
                                return value
                            }
                        }
                        return 0
                    }

                    def while_flag(flag: bool): int {
                        while flag {
                            return 1
                        }
                        return 0
                    }

                    def do_once(flag: bool): int {
                        do {
                            if flag {
                                return 1
                            }
                            return 2
                        } while false
                    }

                    def second_name(values: String[]): String = values[1]

                    def first_id(values: int[]): int = values[0]

                    def copy_users(values: User[]): User[] = values

                    def names(): String[] = String[]{"zero", "one"}

                    def slots(size: int): int[] = int[size]

                    def recover(flag: bool): String {
                        try {
                            if flag {
                                throw "boom"
                            }
                            return "ok"
                        } catch error {
                            return error.getMessage()
                        }
                    }

                    def catch_index(values: String[]): String {
                        try {
                            return values[3]
                        } catch error {
                            return error.getClass().getSimpleName()
                        }
                    }

                    def local_increment(x: int): int {
                        let step: int = 1
                        /// Internal increment
                        def inc(y: int): int = y + step
                        return inc(x)
                    }

                    def parity(x: int): bool {
                        def is_even(n: int): bool {
                            if n == 0 {
                                return true
                            } else {
                                return is_odd(n - 1)
                            }
                        }
                        def is_odd(n: int): bool {
                            if n == 0 {
                                return false
                            } else {
                                return is_even(n - 1)
                            }
                        }
                        return is_even(x)
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
                .filter(module -> module.relativePath().endsWith("Base.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("/// Base type");
        assertThat(generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("Printable.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("/// Printable contract");
        assertThat(generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("User.java"))
                .findFirst()
                .orElseThrow()
                .code())
                .contains("public final class User extends Base implements Printable")
                .contains("/// User type")
                .contains("/// Friendly greet")
                .contains("/// Internal increment")
                .contains("public String greet()")
                .contains("return \"Hello \"+this.name;")
                .contains("var x = \"a\";")
                .contains("x = \"2\";")
                .contains("final String label = super.label();")
                .contains("public String second_name(String[] values)")
                .contains("public int first_id(int[] values)")
                .contains("public User[] copy_users(User[] values)")
                .contains("return new String[]{\"zero\", \"one\"};")
                .contains("return new int[size];")
                .contains("throw capybara$toException(\"boom\");")
                .contains("catch (java.lang.RuntimeException error)")
                .contains("for (var value : values)")
                .contains("for (int value : values)")
                .contains("while (flag)")
                .contains("final class __CapybaraLocalMethods")
                .contains("__capybaraLocalMethods")
                .contains("} while (false);");

        var classesDir = compileGeneratedJava(generatedProgram);
        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL()})) {
            var userType = classLoader.loadClass("foo.boo.User");
            var constructor = userType.getConstructor(String.class);
            var user = constructor.newInstance("Capy");

            assertThat(userType.getMethod("greet").invoke(user)).isEqualTo("Hello Capy");
            assertThat(userType.getMethod("mutable").invoke(user)).isEqualTo("2");
            assertThat(userType.getMethod("print").invoke(user)).isEqualTo("base Capy");
            assertThat(userType.getMethod("second_name", String[].class).invoke(user, (Object) new String[]{"A", "B", "C"})).isEqualTo("B");
            assertThat(userType.getMethod("first_id", int[].class).invoke(user, (Object) new int[]{7, 8, 9})).isEqualTo(7);
            var sameArray = java.lang.reflect.Array.newInstance(userType, 1);
            java.lang.reflect.Array.set(sameArray, 0, user);
            assertThat(userType.getMethod("copy_users", sameArray.getClass()).invoke(user, sameArray)).isSameAs(sameArray);
            assertThat((String[]) userType.getMethod("names").invoke(user)).containsExactly("zero", "one");
            assertThat((int[]) userType.getMethod("slots", int.class).invoke(user, 3)).hasSize(3);
            assertThat(userType.getMethod("recover", boolean.class).invoke(user, false)).isEqualTo("ok");
            assertThat(userType.getMethod("recover", boolean.class).invoke(user, true)).isEqualTo("boom");
            assertThat(userType.getMethod("catch_index", String[].class).invoke(user, (Object) new String[]{"A"})).isEqualTo("ArrayIndexOutOfBoundsException");
            assertThat(userType.getMethod("first_positive", java.util.List.class).invoke(user, java.util.List.of(-1, 0, 3))).isEqualTo(3);
            assertThat(userType.getMethod("first_large", java.util.List.class).invoke(user, java.util.List.of(4, 11, 15))).isEqualTo(11);
            assertThat(userType.getMethod("while_flag", boolean.class).invoke(user, true)).isEqualTo(1);
            assertThat(userType.getMethod("while_flag", boolean.class).invoke(user, false)).isEqualTo(0);
            assertThat(userType.getMethod("do_once", boolean.class).invoke(user, true)).isEqualTo(1);
            assertThat(userType.getMethod("do_once", boolean.class).invoke(user, false)).isEqualTo(2);
            assertThat(userType.getMethod("local_increment", int.class).invoke(user, 7)).isEqualTo(8);
            assertThat(userType.getMethod("parity", int.class).invoke(user, 12)).isEqualTo(true);
            assertThat(userType.getMethod("parity", int.class).invoke(user, 15)).isEqualTo(false);
        }
    }

    @Test
    void shouldRejectTraitStateInJavaBackendV1() {
        var program = compileProgram("""
                trait Named {
                    field name: String = "Capy"
                }
                """);

        assertThatThrownBy(() -> new JavaGenerator().generate(program))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Trait fields are not supported");
    }

    @Test
    void shouldGenerateJavaEntrypointForObjectOrientedMainMethod() throws Exception {
        var program = compileProgram("""
                class Main {
                    def main(args: List[String]): int = args.size()
                }
                """);

        var generatedProgram = new JavaGenerator().generate(program);
        var mainModule = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("Main.java"))
                .findFirst()
                .orElseThrow();

        assertThat(mainModule.code())
                .contains("public static int main(java.util.List<String> args)")
                .contains("public static void main(java.lang.String[] args)")
                .contains("System.exit(main(java.util.List.of(args)));")
                .doesNotContain("public Main(");

        var classesDir = compileGeneratedJava(generatedProgram);
        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL()})) {
            var mainType = classLoader.loadClass("foo.boo.Main");
            assertThat(mainType.getMethod("main", java.util.List.class).invoke(null, java.util.List.of("a", "b"))).isEqualTo(2);
            assertThat(mainType.getMethod("main", String[].class).getReturnType()).isEqualTo(void.class);
        }
    }


    @Test
    void shouldRejectObjectOrientedMainEntrypointThatRequiresConstructor() {
        var program = compileProgram("""
                class Main(name: String) {
                    def main(args: List[String]): int = args.size()
                }
                """);

        assertThatThrownBy(() -> new JavaGenerator().generate(program))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Entrypoint class `Main` cannot declare constructor state or init blocks");
    }

    @Test
    void shouldGenerateAndRunMixedObjectOrientedAndFunctionalInterop() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "ObjectOrientedFpInterop",
                        "/foo/boo",
                        """
                                type InteropPet = InteropDog | InteropCat
                                data InteropDog { name: String }
                                data InteropCat { age: int }

                                fun make_dog(name: String): InteropDog = InteropDog { name: name }

                                fun pet_text(pet: InteropPet): String =
                                    match pet with
                                    case InteropDog { name } -> "dog:" + name
                                    case InteropCat { age } -> "cat:" + age
                                """,
                        SourceKind.FUNCTIONAL
                ),
                new RawModule(
                        "PetInteractor",
                        "/foo/boo",
                        """
                                from ObjectOrientedFpInterop import { InteropPet, InteropDog, InteropCat }

                                class PetInteractor {
                                    def invoke_fp_function(name: String): String =
                                        ObjectOrientedFpInterop.petText(ObjectOrientedFpInterop.makeDog(name))

                                    def create_fp_data(name: String): InteropPet =
                                        InteropDog { name: name }

                                    def match_fp_type(pet_name: String): String {
                                        let pet: InteropPet = InteropDog { name: pet_name }
                                        return match pet with
                                        case InteropDog { name } -> ("dog:" + name)
                                        case InteropCat { age } -> ("cat:" + age)
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ));

        var generatedProgram = new JavaGenerator().generate(program);
        var interactorModule = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("PetInteractor.java"))
                .findFirst()
                .orElseThrow();

        assertThat(interactorModule.code())
                .contains("ObjectOrientedFpInterop.petText(ObjectOrientedFpInterop.makeDog(name))")
                .contains("import foo.boo.ObjectOrientedFpInterop.InteropDog;")
                .contains("return new InteropDog(name);")
                .contains("case InteropDog __capybaraCase")
                .contains("yield (\"dog:\"+name);");

        var classesDir = compileGeneratedJava(generatedProgram);
        var capybaraLibClasses = Path.of("..", "lib", "capybara-lib", "build", "classes", "java", "main").normalize().toAbsolutePath();
        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL(), capybaraLibClasses.toUri().toURL()})) {
            var interactorType = classLoader.loadClass("foo.boo.PetInteractor");
            var interopDogType = classLoader.loadClass("foo.boo.ObjectOrientedFpInterop$InteropDog");
            var interactor = interactorType.getConstructor().newInstance();

            assertThat(interactorType.getMethod("invoke_fp_function", String.class).invoke(interactor, "Capy")).isEqualTo("dog:Capy");
            var dog = interactorType.getMethod("create_fp_data", String.class).invoke(interactor, "Bara");
            assertThat(interopDogType.isInstance(dog)).isTrue();
            assertThat(interopDogType.getMethod("name").invoke(dog)).isEqualTo("Bara");
            assertThat(interactorType.getMethod("match_fp_type", String.class).invoke(interactor, "Mochi")).isEqualTo("dog:Mochi");
        }
    }

    @Test
    void shouldNotRequireImportsWhenObjectOrientedAndFunctionalFilesShareTheSameName() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "SharedInterop",
                        "/foo/boo",
                        """
                                type SharedPet = SharedDog | SharedCat
                                data SharedDog { name: String }
                                data SharedCat { age: int }

                                fun make_dog(name: String): SharedDog = SharedDog { name: name }

                                fun pet_text(pet: SharedPet): String =
                                    match pet with
                                    case SharedDog { name } -> "dog:" + name
                                    case _ -> "cat"
                                """,
                        SourceKind.FUNCTIONAL
                ),
                new RawModule(
                        "SharedInterop",
                        "/foo/boo",
                        """
                                class SharedInteractor {
                                    def invoke_fp_function(name: String): String =
                                        SharedInterop.petText(SharedInterop.makeDog(name))

                                    def create_fp_data(name: String): SharedPet =
                                        SharedDog { name: name }

                                    def match_fp_type(pet_name: String): String {
                                        let pet: SharedPet = SharedDog { name: pet_name }
                                        return match pet with
                                        case SharedDog { name } -> ("dog:" + name)
                                    }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ));

        var generatedProgram = new JavaGenerator().generate(program);
        var interactorModule = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("SharedInteractor.java"))
                .findFirst()
                .orElseThrow();

        assertThat(interactorModule.code())
                .contains("return SharedInterop.petText(SharedInterop.makeDog(name));")
                .contains("import foo.boo.SharedInterop.SharedDog;")
                .contains("import foo.boo.SharedInterop.SharedPet;")
                .contains("return new SharedDog(name);")
                .contains("final SharedPet pet = new SharedDog(pet_name);")
                .contains("case SharedDog __capybaraCase");

        var classesDir = compileGeneratedJava(generatedProgram);
        var capybaraLibClasses = Path.of("..", "lib", "capybara-lib", "build", "classes", "java", "main").normalize().toAbsolutePath();
        try (var classLoader = new URLClassLoader(new URL[]{classesDir.toUri().toURL(), capybaraLibClasses.toUri().toURL()})) {
            var interactorType = classLoader.loadClass("foo.boo.SharedInteractor");
            var sharedDogType = classLoader.loadClass("foo.boo.SharedInterop$SharedDog");
            var interactor = interactorType.getConstructor().newInstance();

            assertThat(interactorType.getMethod("invoke_fp_function", String.class).invoke(interactor, "Capy")).isEqualTo("dog:Capy");
            var dog = interactorType.getMethod("create_fp_data", String.class).invoke(interactor, "Bara");
            assertThat(sharedDogType.isInstance(dog)).isTrue();
            assertThat(sharedDogType.getMethod("name").invoke(dog)).isEqualTo("Bara");
            assertThat(interactorType.getMethod("match_fp_type", String.class).invoke(interactor, "Mochi")).isEqualTo("dog:Mochi");
        }
    }


    @Test
    void shouldUseRootModuleOwnerReferencesWithoutLeadingDot() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "SharedInterop",
                        "",
                        """
                                type SharedPet = SharedDog | SharedCat
                                data SharedDog { name: String }
                                data SharedCat { age: int }

                                fun make_dog(name: String): SharedDog = SharedDog { name: name }

                                fun pet_text(pet: SharedPet): String =
                                    match pet with
                                    case SharedDog { name } -> "dog:" + name
                                    case _ -> "cat"
                                """,
                        SourceKind.FUNCTIONAL
                ),
                new RawModule(
                        "RootConsumer",
                        "",
                        """
                                from SharedInterop import { SharedPet, SharedDog }

                                class RootConsumer {
                                    def invoke_fp_function(name: String): String =
                                        SharedInterop.petText(SharedInterop.makeDog(name))

                                    def create_fp_data(name: String): SharedPet =
                                        SharedDog { name: name }
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ));

        var generatedProgram = new JavaGenerator().generate(program);
        var consumerModule = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("RootConsumer.java"))
                .findFirst()
                .orElseThrow();

        assertThat(consumerModule.code())
                .contains("import SharedInterop.SharedDog;")
                .contains("import SharedInterop.SharedPet;")
                .contains("return SharedInterop.petText(SharedInterop.makeDog(name));")
                .doesNotContain("import .SharedInterop")
                .doesNotContain(".SharedInterop.petText");

    }


    @Test
    void shouldIgnoreSingleQuotedLiteralsDuringInferredImportScan() throws Exception {
        var program = compileProgram(List.of(
                new RawModule(
                        "LiteralConsumer",
                        "/foo/boo",
                        """
                                class LiteralConsumer {
                                    def label(): String = 'SharedDog'
                                }
                                """,
                        SourceKind.OBJECT_ORIENTED
                )
        ));

        var generatedProgram = new JavaGenerator().generate(program);
        var consumerModule = generatedProgram.modules().stream()
                .filter(module -> module.relativePath().endsWith("LiteralConsumer.java"))
                .findFirst()
                .orElseThrow();

        assertThat(consumerModule.code())
                .doesNotContain("import foo.boo.LiteralConsumer.SharedDog;")
                .doesNotContain("import SharedDog;");
    }

    @Test
    void shouldRejectObjectOrientedMainEntrypointThatUsesInstanceState() {
        var program = compileProgram("""
                class Main {
                    def helper(): int = 1

                    def main(args: List[String]): int = size(args) + this.helper()
                }
                """);

        assertThatThrownBy(() -> new JavaGenerator().generate(program))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Entrypoint method `Main.main` cannot use instance state");
    }

    private CompiledProgram compileProgram(String source) {
        return compileProgram(List.of(
                new RawModule("User", "/foo/boo", source, SourceKind.OBJECT_ORIENTED)
        ));
    }

    private CompiledProgram compileProgram(List<RawModule> modules) {
        var result = CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
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
        writeReflectionStub(sourceDir);

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
            var capybaraLibClasses = Path.of("..", "lib", "capybara-lib", "build", "classes", "java", "main").normalize().toAbsolutePath();
            var classpath = System.getProperty("java.class.path")
                             + java.io.File.pathSeparator
                             + capybaraLibClasses;
            var options = List.of(
                    "--release", "21",
                    "-classpath", classpath,
                    "-d", classesDir.toString()
            );
            var success = compiler.getTask(null, fileManager, diagnostics, options, null, compilationUnits).call();
            assertThat(success)
                    .as(diagnostics.getDiagnostics().stream().map(Objects::toString).collect(joining(System.lineSeparator())))
                    .isTrue();
        }
        return classesDir;
    }

    private void writeReflectionStub(Path sourceDir) throws Exception {
        var path = sourceDir.resolve("capy/metaProg/Reflection.java");
        Files.createDirectories(path.getParent());
        Files.writeString(path, """
                package capy.metaProg;

                public final class Reflection {
                    public sealed interface AnyInfo permits DataInfo, InterfaceInfo, ObjectInfo, TraitInfo, ListInfo, SetInfo, DictInfo, TupleInfo, FunctionTypeInfo {}

                    public record PackageInfo(String name, String path) {}
                    public record FieldInfo(String name, AnyInfo type) {}
                    public record FieldValueInfo(String name, AnyInfo type, Object value) {}
                    public record DataValueInfo(String name, PackageInfo pkg, java.util.List<FieldValueInfo> fields) {}
                    public record DataInfo(String name, PackageInfo pkg) implements AnyInfo {}
                    public record MethodInfo(String name, PackageInfo pkg, java.util.List<FieldInfo> params, AnyInfo return_type) {}
                    public record InterfaceInfo(String name, PackageInfo pkg, java.util.List<MethodInfo> methods, java.util.Set<AnyInfo> parents) implements AnyInfo {}
                    public record ObjectInfo(String name, PackageInfo pkg, boolean open, java.util.List<FieldInfo> fields, java.util.List<MethodInfo> methods, java.util.Set<AnyInfo> parents) implements AnyInfo {}
                    public record TraitInfo(String name, PackageInfo pkg, java.util.List<MethodInfo> methods, java.util.Set<AnyInfo> parents) implements AnyInfo {}
                    public record ListInfo(String name, PackageInfo pkg, AnyInfo element_type) implements AnyInfo {}
                    public record SetInfo(String name, PackageInfo pkg, AnyInfo element_type) implements AnyInfo {}
                    public record DictInfo(String name, PackageInfo pkg, AnyInfo value_type) implements AnyInfo {}
                    public record TupleInfo(String name, PackageInfo pkg, java.util.List<AnyInfo> elements) implements AnyInfo {}
                    public record FunctionTypeInfo(String name, PackageInfo pkg, java.util.List<AnyInfo> params, AnyInfo return_type) implements AnyInfo {}
                }
                """, StandardCharsets.UTF_8);
    }
}
