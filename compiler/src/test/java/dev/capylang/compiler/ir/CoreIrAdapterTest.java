package dev.capylang.compiler.ir;

import dev.capylang.compiler.CompiledAnnotation;
import dev.capylang.compiler.CompiledAnnotationArgument;
import dev.capylang.compiler.CompiledAnnotationValue;
import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunction;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledNativeProviderBinding;
import dev.capylang.compiler.CompiledNativeProviderDeclaration;
import dev.capylang.compiler.CompiledPrimitiveBackedType;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderBinding;
import dev.capylang.compiler.NativeProviderCatalog;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.Result;
import dev.capylang.compiler.Visibility;
import dev.capylang.compiler.expression.CompiledInfixExpression;
import dev.capylang.compiler.expression.CompiledIntValue;
import dev.capylang.compiler.expression.CompiledNewData;
import dev.capylang.compiler.expression.CompiledStringValue;
import dev.capylang.compiler.expression.CompiledVariable;
import dev.capylang.compiler.parser.CapybaraParser;
import dev.capylang.compiler.parser.InfixOperator;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.compiler.parser.ObjectOrientedParser;
import dev.capylang.compiler.parser.Program;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.parser.facade.ParserDtoMapper;
import dev.capylang.compiler.parser.facade.ParserIrAdapters;
import dev.capylang.generator.GeneratedModule;
import dev.capylang.generator.GeneratedProgram;
import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

class CoreIrAdapterTest {
    @Test
    void functionalProgramRoundTripsThroughCoreIrSchema() {
        var legacy = parseFunctionalProgram();
        var ir = CoreIrAdapters.functionalProgram(legacy);

        var passed = CoreIrPipeline.passThrough(ir);
        var dto = CoreIrAdapters.functionalProgramDto(passed);
        var roundTrip = ParserIrAdapters.functionalProgram(dto);

        assertThat(passed).isSameAs(ir);
        assertThat(dto).isEqualTo(ParserDtoMapper.functionalProgram(legacy));
        assertThat(roundTrip).isEqualTo(legacy);
    }

    @Test
    void objectOrientedModuleRoundTripsThroughCoreIrSchema() {
        var legacy = parseObjectOrientedModule();
        var ir = CoreIrAdapters.objectOrientedModule(legacy);

        var passed = CoreIrPipeline.passThrough(ir);
        var dto = CoreIrAdapters.objectOrientedModuleDto(passed);
        var roundTrip = ParserIrAdapters.objectOrientedModule(dto);

        assertThat(passed).isSameAs(ir);
        assertThat(dto).isEqualTo(ParserDtoMapper.objectOrientedModule(legacy));
        assertThat(roundTrip).isEqualTo(legacy);
    }

    @Test
    void compiledProgramRoundTripsThroughCoreIrSchema() {
        var legacy = compiledProgram();
        var ir = CoreIrAdapters.compiledProgram(legacy);

        var passed = CoreIrPipeline.passThrough(ir);
        var dto = CoreIrAdapters.compiledProgramDto(passed);
        var roundTrip = CoreIrAdapters.compiledProgram(dto);
        var roundTripDto = CoreIrAdapters.compiledProgramDto(CoreIrAdapters.compiledProgram(roundTrip));

        assertThat(passed).isSameAs(ir);
        assertThat(dto.modules()).singleElement().satisfies(module -> {
            assertThat(module.module().name()).isEqualTo("Core");
            assertThat(module.types()).extracting(CoreIrSchema.CompiledTypeEntryDto::name)
                    .containsExactly("Ok", "Result", "User");
            assertThat(module.visiblePrimitiveBackedTypes())
                    .extracting(CoreIrSchema.CompiledPrimitiveBackedTypeDto::name)
                    .containsExactly("UserId");
            assertThat(module.functions()).singleElement().satisfies(function -> {
                assertThat(function.name()).isEqualTo("make_user");
                assertThat(function.expression()).isInstanceOf(CoreIrSchema.CompiledNewDataExpressionDto.class);
            });
        });
        assertThat(dto.nativeProviders().providers()).singleElement().satisfies(provider -> {
            assertThat(provider.interfaceId()).isEqualTo("dev.example.ClockProvider");
            assertThat(provider.bindings()).extracting(Object::getClass)
                    .containsExactly(
                            CoreIrSchema.JavaNativeProviderBindingDto.class,
                            CoreIrSchema.JavascriptNativeProviderBindingDto.class,
                            CoreIrSchema.PythonNativeProviderBindingDto.class
                    );
        });
        assertThat(roundTripDto).isEqualTo(dto);
    }

    @Test
    void generatedProgramRoundTripsThroughCoreIrSchema() {
        var legacy = new GeneratedProgram(List.of(
                new GeneratedModule(Path.of("dev/capylang/demo/Core.java"), "package dev.capylang.demo;")
        ));
        var ir = CoreIrAdapters.generatedProgram(legacy);

        var passed = CoreIrPipeline.passThrough(ir);
        var dto = CoreIrAdapters.generatedProgramDto(passed);
        var roundTrip = CoreIrAdapters.generatedProgram(dto);

        assertThat(passed).isSameAs(ir);
        assertThat(dto.modules()).singleElement().satisfies(module -> {
            assertThat(module.relativePath()).isEqualTo("dev/capylang/demo/Core.java");
            assertThat(module.code()).isEqualTo("package dev.capylang.demo;");
        });
        assertThat(roundTrip).isEqualTo(legacy);
    }

    private static Program parseFunctionalProgram() {
        var source = """
                from /capy/lang/Option import { * }

                annotation Trace on fun {}

                data User { name: String, age: int }
                union Node { label: String } = Leaf | Branch
                data Leaf { value: int }
                data Branch {}
                enum Color { RED, BLUE }

                @Trace
                fun construct(): User = User { name: "Ada", age: 1 }

                fun pipe_reduce(xs: List[int]): int =
                    xs |> 0, (acc, value) => acc + value

                fun match_guard(node: Node): int =
                    match node with
                    case Leaf { value } when value > 0 -> value
                    case Branch -> 0
                """;
        return success(CapybaraParser.INSTANCE.parseModule(List.of(new RawModule("Core", "/core", source))));
    }

    private static ObjectOrientedModule parseObjectOrientedModule() {
        var source = """
                interface Runner {
                    def run(): void
                }

                trait Named {
                    field label: String = "unnamed"
                    def label_text(): String = this.label
                }

                class Worker(values: String[]): Runner {
                    field values: String[] = values

                    init {
                        this.log("init")
                    }

                    override def run(): void {
                        let first: String = values[0]
                        this.log(first)
                    }

                    def log(message: String): void = println(message)
                }
                """;
        return success(ObjectOrientedParser.INSTANCE.parseModule(
                new RawModule("Worker", "/core", source, SourceKind.OBJECT_ORIENTED)
        ));
    }

    private static CompiledProgram compiledProgram() {
        var annotation = new CompiledAnnotation(
                "Trace",
                "Core",
                "/core",
                List.of(new CompiledAnnotationArgument("enabled", new CompiledAnnotationValue.BoolValue(true)))
        );
        var userType = new CompiledDataType(
                "User",
                List.of(
                        new CompiledDataType.CompiledField("name", PrimitiveLinkedType.STRING, List.of(annotation)),
                        new CompiledDataType.CompiledField("age", PrimitiveLinkedType.INT)
                ),
                List.of(),
                List.of(),
                List.of("user payload"),
                Visibility.LOCAL,
                false,
                false,
                false,
                List.of(annotation)
        );
        var okType = new CompiledDataType(
                "Ok",
                List.of(new CompiledDataType.CompiledField("value", userType)),
                List.of(),
                List.of("Result"),
                List.of(),
                Visibility.LOCAL,
                false
        );
        var resultType = new CompiledDataParentType(
                "Result",
                List.of(),
                List.of(okType),
                List.of("T"),
                List.of("result union"),
                Visibility.LOCAL,
                false,
                List.of(annotation)
        );
        var userIdType = new CompiledPrimitiveBackedType(
                "UserId",
                PrimitiveLinkedType.STRING,
                "String",
                List.of("native identifier"),
                Visibility.PRIVATE,
                List.of(annotation)
        );
        var expression = new CompiledNewData(
                userType,
                List.of(
                        new CompiledNewData.FieldAssignment("name", new CompiledVariable("name", PrimitiveLinkedType.STRING)),
                        new CompiledNewData.FieldAssignment("age", new CompiledInfixExpression(
                                new CompiledIntValue("1"),
                                InfixOperator.PLUS,
                                new CompiledIntValue("2"),
                                PrimitiveLinkedType.INT
                        ))
                )
        );
        var function = new CompiledFunction(
                "make_user",
                userType,
                List.of(new CompiledFunction.CompiledFunctionParameter("name", PrimitiveLinkedType.STRING)),
                expression,
                List.of("builds a user"),
                Visibility.LOCAL,
                false,
                false,
                false,
                List.of(annotation)
        );
        var module = new CompiledModule(
                "Core",
                "/core",
                Map.of(
                        "User", userType,
                        "Ok", okType,
                        "Result", resultType
                ),
                List.of(function),
                Map.of(),
                Map.of("UserId", userIdType),
                List.of(new CompiledModule.StaticImport("dev.capylang.NativeIds", "USER_ID", true))
        );
        var javaBinding = new NativeProviderBackendBinding("dev.example.ClockProvider", null, null, "constructor");
        var javascriptBinding = new NativeProviderBackendBinding(null, "clock-provider", "ClockProvider", "new");
        var pythonBinding = new NativeProviderBackendBinding("ClockProvider", "clock_provider", null, "call");
        var manifest = new NativeProviderManifest(
                List.of(new NativeProviderBinding("dev.example.ClockProvider", "system", javaBinding, javascriptBinding, pythonBinding)),
                "native-providers.json"
        );
        var catalog = new NativeProviderCatalog(
                List.of(new CompiledNativeProviderDeclaration(
                        "ClockProvider",
                        "/core",
                        "Core",
                        "Clock",
                        "dev.example.ClockProvider",
                        "system",
                        "Clock.coo"
                )),
                List.of(new CompiledNativeProviderBinding("dev.example.ClockProvider", "system", javaBinding, javascriptBinding, pythonBinding))
        );
        return new CompiledProgram(List.of(module), List.of(parseObjectOrientedModule()), manifest, catalog);
    }

    private static <T> T success(Result<T> result) {
        if (result instanceof Result.Success<T> success) {
            return success.value();
        }
        fail("Expected parse success but got <%s>", result);
        throw new IllegalStateException("unreachable");
    }
}
