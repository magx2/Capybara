package dev.capylang.parser;

import dev.capylang.compiler.Result;
import dev.capylang.compiler.parser.CapybaraParser;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.compiler.parser.ObjectOrientedParser;
import dev.capylang.compiler.parser.RawModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.parser.facade.AntlrParserFacade;
import dev.capylang.compiler.parser.facade.ParserIrAdapters;
import dev.capylang.compiler.parser.facade.ParserSchema.*;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class ParserFacadeTest {
    private static final AntlrParserFacade FACADE = AntlrParserFacade.INSTANCE;

    @Test
    @DisplayName("should expose functional parser golden syntax through stable DTOs")
    void parseFunctionalGoldenSyntax() {
        var source = """
                from /capy/lang/Option import { * }
                import /foo/Bar

                annotation A on fun {}

                data Box
                [T] { value: T }
                data User { name: String, age: int }
                union Node { label: String } = Leaf | Branch
                data Leaf { value: int }
                data Branch {}

                fun add(a: int, b: int): int = a + b

                @A
                fun construct(): User = User { name: "Ada", age: 1 }

                fun type_field(): int = User.age(User { "Ada", 1 })

                fun name_call(): int = add(1, 2)

                fun collections(): Tuple[List[int], Set[int], Dict[int]] = ([1, 2], {1, 2}, {"a": 1})

                fun pipe_reduce(xs: List[int]): int =
                    xs |> 0, (acc, value) => acc + value

                fun lambda_form(): int => int = value => value * 2

                fun match_guard(node: Node): int =
                    match node with
                    case Leaf { value } when value > 0 -> value
                    case Branch -> 0

                fun conditional(x: int): int = if x > 0 then x else -x

                fun precedence(a: int, b: int, c: int): bool = !(a + b * c > 10 & false)

                fun empty_postfix(xs: List[int]): Tuple[List[int], List[int], List[int], List[int]] =
                    (xs[], xs[:], xs[1:], xs[:2])
                """;

        var module = new SourceModuleDto("Golden", "/parser", source, SourceKindDto.FUNCTIONAL);
        var result = FACADE.parseFunctionalModule(module);

        assertThat(result.success()).isTrue();
        var dto = result.value().orElseThrow();
        assertThat(dto.imports())
                .extracting(ImportDto::moduleName)
                .containsExactly("/capy/lang/Option", "/foo/Bar");
        assertThat(dto.definitions())
                .filteredOn(DataDefinitionDto.class::isInstance)
                .extracting(definition -> ((DataDefinitionDto) definition).name())
                .contains("Box", "User", "Leaf", "Branch");

        assertThat(findFunction(dto, "construct").expression()).isInstanceOf(NewDataExpressionDto.class);
        var constructed = (NewDataExpressionDto) findFunction(dto, "construct").expression();
        assertThat(((DataTypeDto) constructed.type()).name()).isEqualTo("User");
        assertThat(constructed.assignments()).extracting(FieldAssignmentDto::name).containsExactly("name", "age");

        assertThat(findFunction(dto, "type_field").expression()).isInstanceOf(FunctionCallExpressionDto.class);
        var typeField = (FunctionCallExpressionDto) findFunction(dto, "type_field").expression();
        assertThat(typeField.moduleName()).contains("User");
        assertThat(typeField.name()).isEqualTo("age");

        assertThat(findFunction(dto, "name_call").expression()).isInstanceOf(FunctionCallExpressionDto.class);
        assertThat(findFunction(dto, "pipe_reduce").expression()).isInstanceOf(InfixExpressionDto.class);
        assertThat(((InfixExpressionDto) findFunction(dto, "pipe_reduce").expression()).operator()).isEqualTo("|>");
        assertThat(findFunction(dto, "lambda_form").expression()).isInstanceOf(LambdaExpressionDto.class);

        assertThat(findFunction(dto, "match_guard").expression()).isInstanceOf(MatchExpressionDto.class);
        var match = (MatchExpressionDto) findFunction(dto, "match_guard").expression();
        assertThat(match.cases()).hasSize(2);
        assertThat(match.cases().getFirst().guard()).isPresent();

        assertThat(findFunction(dto, "conditional").expression()).isInstanceOf(IfExpressionDto.class);
        assertThat(findFunction(dto, "precedence").expression()).isInstanceOf(InfixExpressionDto.class);

        var tuple = (TupleExpressionDto) findFunction(dto, "empty_postfix").expression();
        assertThat(tuple.values().get(0)).isInstanceOf(IndexExpressionDto.class);
        assertThat(((IndexExpressionDto) tuple.values().get(0)).arguments()).isEmpty();
        assertThat(tuple.values().get(1)).isInstanceOf(SliceExpressionDto.class);
        var emptySlice = (SliceExpressionDto) tuple.values().get(1);
        assertThat(emptySlice.start()).isEmpty();
        assertThat(emptySlice.end()).isEmpty();

        var roundTrip = ParserIrAdapters.functionalModule(dto);
        var legacy = ((Result.Success<dev.capylang.compiler.parser.Module>) new CapybaraParser()
                .parseModule(new RawModule("Golden", "/parser", source)))
                .value();
        assertThat(roundTrip).isEqualTo(legacy);
    }

    @Test
    @DisplayName("should expose object-oriented parser golden syntax through stable DTOs")
    void parseObjectOrientedGoldenSyntax() {
        var source = """
                from /capy/io/Stdout import { * }

                interface Runner {
                    def run(): void
                }

                trait Named {
                    field label: String = "unnamed"
                    def label_text(): String = this.label
                }

                open class Base {
                    def label(): String = "base"
                }

                class Worker(values: String[]): Base, Runner {
                    field values: String[] = values

                    init {
                        this.log("init")
                    }

                    override def run(): void {
                        let first: String = values[0]
                        this.log(first)
                        try {
                            throw "boom"
                        } catch error {
                            return this.log(error.getMessage())
                        }
                    }

                    def log(message: String): void = println(message)

                    def slots(size: int): int[] = int[size]

                    def names(): String[] = String[]{"zero", "one"}

                    def parent_label(): String = super.label()
                }
                """;

        var module = new SourceModuleDto("Worker", "/parser", source, SourceKindDto.OBJECT_ORIENTED);
        var result = FACADE.parseObjectOrientedModule(module);

        assertThat(result.success()).isTrue();
        var dto = result.value().orElseThrow();
        assertThat(dto.imports()).extracting(ImportDto::moduleName).containsExactly("/capy/io/Stdout");
        assertThat(dto.definitions())
                .extracting(OoTypeDeclarationDto::name)
                .containsExactly("Runner", "Named", "Base", "Worker");

        var worker = (OoClassDefinitionDto) dto.definitions().get(3);
        assertThat(worker.constructorParameters()).extracting(OoParameterDto::type).containsExactly("String[]");
        assertThat(worker.parents()).extracting(OoTypeReferenceDto::name).containsExactly("Base", "Runner");
        assertThat(worker.members().get(0)).isInstanceOf(OoFieldDefinitionDto.class);
        assertThat(worker.members().get(1)).isInstanceOf(OoInitBlockDto.class);

        var run = (OoMethodDefinitionDto) worker.members().get(2);
        assertThat(run.body()).hasValueSatisfying(body -> {
            assertThat(body).isInstanceOf(OoStatementBlockDto.class);
            var statements = ((OoStatementBlockDto) body).statements();
            assertThat(statements)
                    .extracting(Object::getClass)
                    .containsExactly(
                            OoLetStatementDto.class,
                            OoExpressionStatementDto.class,
                            OoTryCatchStatementDto.class
                    );
            var tryCatch = (OoTryCatchStatementDto) statements.get(2);
            assertThat(tryCatch.tryBlock().statements()).singleElement().isInstanceOf(OoThrowStatementDto.class);
            assertThat(tryCatch.catches()).singleElement().satisfies(catchClause ->
                    assertThat(catchClause.body().statements()).singleElement().isInstanceOf(OoReturnStatementDto.class));
        });

        assertThat(findMethod(worker, "slots").body())
                .hasValueSatisfying(body -> assertThat(((OoExpressionBodyDto) body).rawExpression()).isEqualTo("int[size]"));
        assertThat(findMethod(worker, "names").body())
                .hasValueSatisfying(body -> assertThat(((OoExpressionBodyDto) body).rawExpression()).isEqualTo("String[]{\"zero\",\"one\"}"));
        assertThat(findMethod(worker, "parent_label").body())
                .hasValueSatisfying(body -> assertThat(((OoExpressionBodyDto) body).rawExpression()).isEqualTo("super.label()"));

        var roundTrip = ParserIrAdapters.objectOrientedModule(dto);
        var legacy = ((Result.Success<ObjectOrientedModule>) ObjectOrientedParser.INSTANCE
                .parseModule(new RawModule("Worker", "/parser", source, SourceKind.OBJECT_ORIENTED)))
                .value();
        assertThat(roundTrip).isEqualTo(legacy);
    }

    @Test
    @DisplayName("should preserve import preprocessing line numbers in the facade")
    void extractImportsWithLineNumbers() {
        var module = new SourceModuleDto("Imports", "/parser", """
                from /capy/lang/Option import { Some, None } except { None }
                import /foo/Bar
                fun value(): int = broken
                """, SourceKindDto.FUNCTIONAL);

        var extracted = FACADE.extractImports(module);

        assertThat(extracted.imports()).hasSize(2);
        assertThat(extracted.importLines()).extracting(ImportLineDto::line).containsExactly(1, 2);
        assertThat(extracted.sourceWithoutImports()).startsWith(System.lineSeparator() + System.lineSeparator() + "fun value()");

        var result = FACADE.parseFunctionalModule(module);
        assertThat(result.success()).isTrue();
        var value = findFunction(result.value().orElseThrow(), "value");
        assertThat(value.position()).hasValueSatisfying(position -> assertThat(position.line()).isEqualTo(3));
    }

    @Test
    @DisplayName("should return facade diagnostics with file line column and message")
    void parserDiagnostics() {
        var functional = FACADE.parseFunctionalModule(new SourceModuleDto("Broken", "/parser", """
                fun broken(value: int): int =
                    match value with
                    case _ = value
                """, SourceKindDto.FUNCTIONAL));

        assertThat(functional.value()).isEmpty();
        assertThat(functional.diagnostics()).singleElement().satisfies(error -> {
            assertThat(error.file()).isEqualTo("/parser/Broken.cfun");
            assertThat(error.line()).isGreaterThan(0);
            assertThat(error.column()).isGreaterThanOrEqualTo(0);
            assertThat(error.message()).contains("Expected `->`, found `=`");
        });

        var objectOriented = FACADE.parseObjectOrientedModule(new SourceModuleDto("Broken", "/parser", """
                class Broken {
                    def print(): String =
                }
                """, SourceKindDto.OBJECT_ORIENTED));

        assertThat(objectOriented.value()).isEmpty();
        assertThat(objectOriented.diagnostics()).singleElement().satisfies(error -> {
            assertThat(error.file()).isEqualTo("/parser/Broken.coo");
            assertThat(error.line()).isGreaterThan(0);
            assertThat(error.column()).isGreaterThanOrEqualTo(0);
            assertThat(error.message()).contains("line");
        });
    }

    @Test
    @DisplayName("should parse future native-provider parser facade contracts")
    void parseNativeProviderFacadeContracts() {
        var ooContract = FACADE.parseObjectOrientedModule(new SourceModuleDto(
                "ParserFacade",
                "/dev/capylang/compiler/parser",
                """
                        interface ParserFacade {
                            def parse_functional_module(source: String): String
                            def parse_object_oriented_module(source: String): String
                            def extract_imports(source: String): String
                        }
                        """,
                SourceKindDto.OBJECT_ORIENTED
        ));

        assertThat(ooContract.success()).isTrue();
        assertThat(ooContract.value().orElseThrow().definitions())
                .singleElement()
                .extracting(OoTypeDeclarationDto::name)
                .isEqualTo("ParserFacade");

        var providerContract = FACADE.parseFunctionalModule(new SourceModuleDto(
                "ParserFacadeProvider",
                "/dev/capylang/compiler/parser",
                """
                        from /capy/lang/Effect import { Effect }
                        from /capy/meta_prog/NativeProvider import { NativeProvider }
                        from /dev/capylang/compiler/parser/ParserFacade import { ParserFacade }

                        @NativeProvider(qualifier: "antlr")
                        fun antlr_parser_facade(): Effect[ParserFacade] = <native>
                        """,
                SourceKindDto.FUNCTIONAL
        ));

        assertThat(providerContract.success()).isTrue();
        var provider = findFunction(providerContract.value().orElseThrow(), "antlr_parser_facade");
        assertThat(provider.annotations()).singleElement().satisfies(annotation -> {
            assertThat(annotation.name()).isEqualTo("NativeProvider");
            assertThat(annotation.arguments()).singleElement().satisfies(argument -> {
                assertThat(argument.name()).isEqualTo("qualifier");
                assertThat(argument.value()).isInstanceOf(StringAnnotationValueDto.class);
            });
        });
        assertThat(provider.expression()).isInstanceOf(NothingExpressionDto.class);
        assertThat(((NothingExpressionDto) provider.expression()).literal()).isEqualTo("<native>");
    }

    private static FunctionDefinitionDto findFunction(FunctionalModuleDto module, String name) {
        return module.definitions().stream()
                .filter(FunctionDefinitionDto.class::isInstance)
                .map(FunctionDefinitionDto.class::cast)
                .filter(function -> function.name().equals(name))
                .findFirst()
                .orElseThrow();
    }

    private static OoMethodDefinitionDto findMethod(OoClassDefinitionDto type, String name) {
        return type.members().stream()
                .filter(OoMethodDefinitionDto.class::isInstance)
                .map(OoMethodDefinitionDto.class::cast)
                .filter(method -> method.name().equals(name))
                .findFirst()
                .orElseThrow();
    }
}
