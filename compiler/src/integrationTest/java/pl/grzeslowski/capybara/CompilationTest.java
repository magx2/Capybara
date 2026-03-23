package pl.grzeslowski.capybara;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import pl.grzeslowski.capybara.parser.Module;
import pl.grzeslowski.capybara.compiler.OutputType;
import pl.grzeslowski.capybara.parser.Program;
import pl.grzeslowski.capybara.generator.Generator;
import pl.grzeslowski.capybara.compiler.CapybaraCompiler;
import pl.grzeslowski.capybara.compiler.CompiledProgram;
import pl.grzeslowski.capybara.compiler.Result;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

class CompilationTest {
    @ParameterizedTest(name = "{index}: should {0}")
    @MethodSource
    void test(String code) {
        // parse
        var functional = CapybaraParser.INSTANCE.parseFunctional("Main", "/capybara", code).functional();
        var module = new Module("Main", "/capybara", functional);
        var program = new Program(List.of(module));
        System.out.println(" === PARSING === ");
        System.out.println(program);

        // link
        var link = CapybaraCompiler.INSTANCE.compile(program, new java.util.TreeSet<>());
        if (link instanceof Result.Error<CompiledProgram>) {
            var errors = ((Result.Error<CompiledProgram>) link).errors();
            throw new RuntimeException("Linking failed with " + errors.size() + " error(s): " + errors);
        }
        System.out.println("\n === LINKING === ");
        System.out.println(link);

        // generate
        System.out.println("\n === GENERATION === ");
        Arrays.stream(OutputType.values())
                .parallel()
                .map(type -> {
                    var generator = Generator.findGenerator(type);
                    var linkedProgram = ((Result.Success<CompiledProgram>) link).value();
                    var compiled = generator.generate(linkedProgram);
                    return "\t === " + type + " === \n" + compiled;
                })
                .forEach(System.out::println);

    }

    static Stream<Arguments> test() {
        return Stream.of(
                Arguments.of("""
                                // algebraic type
                                type Shape = Circle | Rectangle
                                data Circle { radius: float }
                                data Rectangle { width: float, height: float }
                                
                                fun area(shape: Shape): float =
                                    match shape with
                                    | Circle { radius } -> 3.14 * radius ^ 2
                                    | Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    | Circle { radius } -> Rectangle { width : radius * 2, height : radius * 2 }
                                    | Rectangle { width, height } -> Circle { radius : (width + height) / 4 }
                                
                                // type with common value
                                type Person { name: string, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: string }
                                """,
                        """
                                fun classify(x: int): string =
                                    if x > 0 then "positive"
                                    else "non-positive"
                                fun always_true(): bool = true
                                
                                // algebraic type
                                type Shape = Circle | Rectangle
                                data Circle { radius: float }
                                data Rectangle { width: float, height: float }
                                
                                fun area(shape: Shape): float =
                                    match shape with
                                    | Circle { radius } -> 3.14 * radius * radius
                                    | Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    | Circle { radius } -> Rectangle { width = radius * 2, height = radius * 2 }
                                    | Rectangle { width, height } -> Circle { radius = (width + height) / 4 }
                                """,
                        """
                                // algebraic type
                                type Shape = Circle | Rectangle
                                data Circle { radius: float }
                                data Rectangle { width: float, height: float }
                                
                                fun area(shape: Shape): float =
                                    match shape with
                                    | Circle { radius } -> 3.14 * radius ^ 2
                                    | Rectangle { width, height } -> width * height
                                
                                fun da_vinci(shape: Shape): Shape =
                                    match shape with
                                    | Circle { radius } -> Rectangle { width : radius * 2, height : radius * 2 }
                                    | Rectangle { width, height } -> Circle { radius : (width + height) / 4 }
                                """,
                        """
                                // type with common value
                                type Person { name: string, age: int } = Student | Teacher
                                data Student { grade: int }
                                data Teacher { subject: string }
                                """,
                        """
                                type Option[T] = Some[T] | None
                                data Some[T] { value: T }
                                single None

                                fun tuple(): tuple[int, string, double] = (1, "foo", 5.0)
                                fun tuple2(): tuple[int, Option[string], double] = (1, Some { value: "foo" }, 5.0)
                                fun tuple_index(): Option[string] = (1, "foo", 5.0)[1]
                                fun tuple_index_negative(): Option[string] = (1, "foo", 5.0)[-2]
                                fun tuple_slice(): tuple[string, double] = (1, "foo", 5.0)[1:]
                                fun tuple_slice_negative(): tuple[int, string] = (1, "foo", 5.0)[:-1]
                                fun tuple_if(x: int): tuple[int, string, float, string] =
                                    (5, if x > 4 then "big" else "small", 5.1f, "foo")
                                """)
        );
    }
}





