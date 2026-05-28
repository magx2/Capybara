package dev.capylang.compiler;

import dev.capylang.compiler.expression.ExpressionCompilationPass;
import dev.capylang.compiler.parser.RawModule;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.TreeSet;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionCompilationPassParityTest {
    private static final String EXPRESSION_PASS = "capybara.compiler.useCapybaraExpressionCompilationPass";

    @Test
    void shouldExposePureExpressionHelpers() {
        assertThat(ExpressionCompilationPass.expressionForms())
                .map(row -> (String) row.get(0))
                .contains("literals", "calls", "branches", "collections", "bind_unwrap", "object_interop");
        assertThat(ExpressionCompilationPass.literalType("INT")).isEqualTo("INT");
        assertThat(ExpressionCompilationPass.higherTypeDecision("ENUM", true, true, "READY", false, true))
                .isEqualTo("ENUM");
        assertThat(ExpressionCompilationPass.higherTypeDecision("DATA", true, false, "Person", false, false))
                .isEqualTo("DATA");
        assertThat(ExpressionCompilationPass.isProgramMainSignature(
                "main",
                "Effect",
                List.of("Program"),
                List.of("List[String]")
        )).isTrue();
        assertThat(ExpressionCompilationPass.isDirectSelfCall(
                "sum",
                List.of("int", "int"),
                List.of("sum", "Sample.sum"),
                List.of("int", "int")
        )).isTrue();
    }

    @Test
    void shouldAnalyzeTailRecursionInPurePass() {
        var leaf = ExpressionCompilationPass.expressionNode(0, "LEAF", "", List.of(), List.of());
        var tailCall = ExpressionCompilationPass.expressionNode(1, "CALL", "sum", List.of("int"), List.of(0));
        var condition = ExpressionCompilationPass.expressionNode(2, "LEAF", "", List.of(), List.of());
        var branch = ExpressionCompilationPass.expressionNode(3, "IF", "", List.of(), List.of(2, 0, 1));

        assertThat(ExpressionCompilationPass.analyzeTailRecursion(
                List.of(leaf, tailCall, condition, branch),
                3,
                List.of("sum"),
                List.of("int")
        )).isEqualTo(List.of(true, -1));

        var nonTailCall = ExpressionCompilationPass.expressionNode(4, "CALL", "sum", List.of("int"), List.of(0));
        var infix = ExpressionCompilationPass.expressionNode(5, "INFIX", "+", List.of(), List.of(4, 0));

        assertThat(ExpressionCompilationPass.analyzeTailRecursion(
                List.of(leaf, nonTailCall, infix),
                5,
                List.of("sum"),
                List.of("int")
        )).isEqualTo(List.of(true, 4));
    }

    @Test
    void shouldMatchLegacyCompileOutputForExpressionFamilies() {
        var modules = List.of(new RawModule("Expressions", "/dev/capylang/test", """
                from /capy/collection/List import { * }
                from /capy/collection/Set import { * }
                from /capy/collection/Dict import { * }
                from /capy/lang/Effect import { * }
                from /capy/lang/Option import { * }
                from /capy/lang/Result import { * }
                from /capy/meta_prog/Recursive import { Recursive }

                type token -> String
                data Person { name: String, age: int }

                fun make_person(name: String, age: int): Person = Person { name, age }
                fun field(person: Person): String = person.name
                fun let_if(value: int): int =
                    let doubled = value * 2
                    if doubled > 3 then doubled else 3
                fun lambda_invoke(prefix: String): String = (x => prefix + x)(1)
                fun collections(value: int): Tuple[List[int], Set[int], Dict[int], Tuple[int, String]] =
                    ([value], {value}, {"value": value}, (value, "ok"))
                fun index_value(values: List[int]): Option[int] = values[0]
                fun slice_values(values: List[int]): List[int] = values[0:1]
                fun match_value(value: Option[int]): int =
                    match value with
                    case Some { value } -> value
                    case _ -> 0
                fun unwrap_token(value: token): String = value.value
                fun bind_result(value: int): Result[int] =
                    let x <- /capy/lang/Result.Success { value }
                    /capy/lang/Result.Success { value: x + 1 }
                fun add_effects(a: int, b: int): Effect[int] =
                    let x <- pure(a)
                    let y <- delay(() => b)
                    pure(x + y)

                @Recursive
                fun sum(value: int, acc: int): int =
                    if value <= 0 then acc else sum(value - 1, acc + value)

                fun main(args: List[String]): Effect[/capy/lang/Program] =
                    pure(/capy/lang/Program.Success {})
                """));

        assertThat(resultSummary(compileWithProperty(modules, true)))
                .isEqualTo(resultSummary(compileWithProperty(modules, false)));
    }

    @Test
    void shouldMatchLegacyDiagnosticsForRecursiveContractErrors() {
        var modules = List.of(new RawModule("BadRecursion", "/dev/capylang/test", """
                from /capy/meta_prog/Recursive import { Recursive }

                @Recursive
                fun sum(value: int): int =
                    1 + sum(value - 1)
                """));

        assertThat(resultSummary(compileWithProperty(modules, true)))
                .isEqualTo(resultSummary(compileWithProperty(modules, false)));
    }

    private static Result<CompiledProgram> compileWithProperty(List<RawModule> modules, boolean enabled) {
        var previous = System.getProperty(EXPRESSION_PASS);
        try {
            System.setProperty(EXPRESSION_PASS, Boolean.toString(enabled));
            return CapybaraCompiler.INSTANCE.compile(modules, new TreeSet<>());
        } finally {
            if (previous == null) {
                System.clearProperty(EXPRESSION_PASS);
            } else {
                System.setProperty(EXPRESSION_PASS, previous);
            }
        }
    }

    private static Object resultSummary(Result<CompiledProgram> result) {
        if (result instanceof Result.Error<CompiledProgram> error) {
            return error.errors().stream()
                    .map(single -> single.file() + ":" + single.line() + ":" + single.column() + ":" + single.message())
                    .toList();
        }
        var program = ((Result.Success<CompiledProgram>) result).value();
        return program.modules().stream()
                .map(module -> new ModuleSummary(
                        module.name(),
                        module.path(),
                        module.functions().stream()
                                .map(function -> new FunctionSummary(
                                        function.name(),
                                        function.returnType().toString(),
                                        function.programMain(),
                                        function.recursive(),
                                        function.tailRecursive(),
                                        function.expression().getClass().getSimpleName()
                                ))
                                .toList()
                ))
                .toList();
    }

    private record ModuleSummary(String name, String path, List<FunctionSummary> functions) {
    }

    private record FunctionSummary(
            String name,
            String returnType,
            boolean programMain,
            boolean recursive,
            boolean tailRecursive,
            String expressionKind
    ) {
    }
}
