package dev.capylang;

import dev.capylang.cli.Capy;
import dev.capylang.cli.CapyMain;
import dev.capylang.compiler.BackendCompilationContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.junit.jupiter.api.parallel.Resources;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.tools.ToolProvider;

import static org.assertj.core.api.Assertions.assertThat;

@ResourceLock(Resources.SYSTEM_ERR)
class JavaGenerationDiagnosticsIntegrationTest {
    @TempDir
    Path tempDir;

    @Test
    void renamesLambdaParametersThatShadowEnclosingParameters() throws Exception {
        var source = writeSource("sample/LambdaShadow.cfun", """
                from /capy/collection/Seq import { Seq }
                from /capy/lang/Option import { Option }

                data Game { value: int }

                fun last_game(game: Game, games: Seq[Game]): Option[Game] =
                    games.fold((_, game) => game)
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("(__, game__lambda_1) -> game__lambda_1");
        assertJavaCompiles(generated);
    }

    @Test
    void keepsResultBindingDeclarationsInSyncWithRenamedLambdaBindings() throws Exception {
        var source = writeSource("sample/ResultLambdaShadow.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun increment(value: int): Result[int] =
                    Success { 1 } | value: int => value + 1
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("int value__lambda_1 =")
                .contains("(value__lambda_1 + 1)");
        assertJavaCompiles(generated);
    }

    @Test
    void rejectsEffectBindingBlockWithResultBeforeJavaGeneration() throws Exception {
        var source = writeSource("paper-soccer/Main.cfun", """
                from /capy/lang/Effect import { Effect, pure }
                from /capy/lang/Program import { Program }
                from /capy/lang/Result import { Result, Success }

                data UI {}

                private fun build_ui(): Effect[UI] = pure(UI {})
                private fun program(): Program = Program.Success {}
                private fun parse(): Result[int] = Success { 1 }
                private fun validate(value: int): Result[int] = Success { value }
                private fun UI.draw_field(value: int): int = value

                fun main(args: List[String]): Effect[Program] =
                    let ui <- build_ui()
                    parse()
                        .flat_map(:validate)
                        .map(value => ui.draw_field(value))
                        .map(_ => program())
                """);

        assertThat(compileGenerateStderr()).contains(
                "Function `main` returns `Effect[Result[Program]]`, but declares `Effect[Program]`."
        );
        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void generatesResultMethodChainReducedToProgram() throws Exception {
        var fieldSource = writeSource("sample/Field.cfun", """
                data Field {}
                """);
        writeSource("sample/ui/UI.coo", """
                interface UI {
                    def draw_field(game_field: Field): Unit
                }
                """);
        var source = writeSource("sample/Main.cfun", """
                from /capy/lang/Program import { Program, DEFAULT_FAILED_EXIT_CODE }
                from /capy/lang/Result import { Result, Success }
                from /sample/Field import { Field }
                from /sample/ui/UI import { UI }

                private fun parse_strings(args: List[String]): Result[String] = Success { args[0].or_else("") }
                private fun parse_int(value: String): Result[int] = value.to_int()
                private fun parse_field(value: int): Result[Field] = Success { Field {} }

                fun render(ui: UI, args: List[String]): Program =
                    parse_strings(args)
                        .flat_map(:parse_int)
                        .flat_map(:parse_field)
                        .map(field => ui.draw_field(field))
                        .map(_ => Program.Success {})
                        .or_else(Program.Failed { exit_code: DEFAULT_FAILED_EXIT_CODE })
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("ui.draw_field(((Field) field))")
                .contains("; return null; })")
                .doesNotContain("ui.draw_field(((java.lang.Object) field))")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(
                generatedPath(fieldSource),
                outputDir().resolve("sample/ui/UI.java"),
                generated
        );
    }

    @Test
    void doesNotGenerateModuleClassForInterfaceOnlySource() throws Exception {
        writeSource("paper-soccer/ui/UI.coo", """
                interface UI {
                    def draw_field(game_field: String): String
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var ui = outputDir().resolve("paper_soccer/ui/UI.java");
        var uiModule = outputDir().resolve("paper_soccer/ui/UI_.java");
        assertThat(ui)
                .exists()
                .content()
                .contains("public interface UI {");
        assertThat(uiModule).doesNotExist();
        assertJavaCompiles(ui);
    }

    @Test
    void generatesTypedUnitInterfaceSignatureWithCamelCaseParameter() throws Exception {
        writeSource("paper-soccer/Field.cfun", """
                data Field {}
                """);
        writeSource("paper-soccer/ui/UI.coo", """
                interface UI {
                    def draw_field(game_field: Field): Unit
                }

                class ConsoleUI: UI {
                    override def draw_field(game_field: Field): Unit = this.consume(game_field)

                    private def consume(game_field: Field): Field = game_field
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var field = outputDir().resolve("paper_soccer/Field.java");
        var ui = outputDir().resolve("paper_soccer/ui/UI.java");
        var uiModule = outputDir().resolve("paper_soccer/ui/UI_.java");
        assertThat(ui)
                .exists()
                .content()
                .contains("void draw_field(paper_soccer.Field gameField);")
                .doesNotContain("java.lang.Object draw_field(java.lang.Object game_field)");
        assertThat(uiModule)
                .exists()
                .content()
                .contains("void draw_field(paper_soccer.Field gameField)");

        var implementation = tempDir.resolve("implementation/paper_soccer/ui/JavaUI.java");
        Files.createDirectories(implementation.getParent());
        Files.writeString(implementation, """
                package paper_soccer.ui;

                public final class JavaUI implements UI {
                    @Override
                    public void draw_field(paper_soccer.Field gameField) {
                    }
                }
                """);
        assertJavaCompiles(field, ui, uiModule, implementation);
    }

    @Test
    void generatesModuleClassForDataDeclarationOnlySource() throws Exception {
        writeSource("sample/Models.cfun", """
                data User { name: String }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var models = outputDir().resolve("sample/Models.java");
        assertThat(models)
                .exists()
                .content()
                .contains("public final class Models")
                .contains("record User(");
        assertJavaCompiles(models);
    }

    @Test
    void generatesNominalEnumAndRecordConstantsWithoutDataHelpers() throws Exception {
        var source = writeSource("sample/Game.cfun", """
                enum Player { PLAYER_A, PLAYER_B }

                data Point { x: int, y: int }

                const CENTER_POINT = Point { x: 0, y: 0 }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public enum Player { PLAYER_A, PLAYER_B }")
                .contains("public static final Player PLAYER_A = Player.PLAYER_A;")
                .contains("public static final Player PLAYER_B = Player.PLAYER_B;")
                .contains("public static final Point CENTER_POINT = new Point(0, 0);")
                .doesNotContain("__capy_data(", "java.lang.Object PLAYER_A", "java.lang.Object CENTER_POINT");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void generatesNestedDataConstantsAsNominalRecords() throws Exception {
        var source = writeSource("sample/Path.cfun", """
                enum PathRoot { RELATIVE, ABSOLUTE }

                data Path { root: PathRoot }

                const CURRENT = Path { root: RELATIVE }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public static final Path CURRENT = new Path(")
                .doesNotContain("__capy_data(\"Path\"");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void constructsNestedGeneratedDataAsNominalRecords() throws Exception {
        var source = writeSource("sample/Games.cfun", """
                from /capy/lang/Result import { Result, Success }

                enum Player { PLAYER_A, PLAYER_B }

                data Point {
                    x: int,
                    y: int,
                }

                data Field {
                    width: int,
                    height: int,
                }

                data Game {
                    field: Field,
                    moves: List[Point],
                    to_move: Player,
                }

                const BASE_GAME = Game {
                    field: Field { width: 7, height: 9 },
                    moves: [Point { x: 1, y: 2 }],
                    to_move: Player.PLAYER_A,
                }

                const COPY_GAME = Game { ...BASE_GAME }

                fun initial_game(field: Field): Game =
                    Game {
                        field,
                        moves: [],
                        to_move: Player.PLAYER_A,
                    }

                fun game_with_move(field: Field): Game =
                    Game {
                        field,
                        moves: [Point { x: 3, y: 4 }],
                        to_move: Player.PLAYER_B,
                    }

                fun wrapped_game(field: Field): Result[Game] =
                    Success {
                        Game {
                            field,
                            moves: [Point { x: 5, y: 6 }],
                            to_move: Player.PLAYER_A,
                        }
                    }

                fun updated_nested_values(field: Field): Game =
                    let point = Point { x: 3, y: 4 }
                    Game {
                        field: field.with(width: 15, height: field.height),
                        moves: [point.with(x: 8, y: point.y)],
                        to_move: Player.PLAYER_B,
                    }

                fun copied_game(game: Game): Game = Game { ...game }

                fun field_width(game: Game): int = game.field.width
                fun first_move_x(game: Game): int = game.moves[0].or_else(Point { x: 0, y: 0 }).x
                fun current_player(game: Game): Player = game.to_move
                fun same_game(left: Game, right: Game): bool = left == right
                """);
        writeSource("sample/UI.coo", """
                from /sample/Games import { Game }

                interface UI {
                    def draw_field(game: Game): Unit
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var games = generatedPath(source);
        var ui = outputDir().resolve("sample/UI.java");
        assertThat(games)
                .content()
                .contains("public record Point(int x, int y)")
                .contains("public record Field(int width, int height)")
                .contains("public record Game(Field field, java.util.List<Point> moves, Player to_move)")
                .contains("new Game(")
                .contains("new Point(3, 4)")
                .doesNotContain("__capy_data(\"Game\"")
                .doesNotContain("__capy_data(\"Point\"");
        assertThat(ui).content().contains("void draw_field(Games.Game game);");

        var implementation = tempDir.resolve("implementation/sample/JavaUI.java");
        Files.createDirectories(implementation.getParent());
        Files.writeString(implementation, """
                package sample;

                public final class JavaUI implements UI {
                    public Games.Game received;

                    @Override
                    public void draw_field(Games.Game game) {
                        received = game;
                    }
                }
                """);

        var classes = compileJava(games, ui, implementation);
        try (var loader = new URLClassLoader(new java.net.URL[]{classes.toUri().toURL()})) {
            var gamesClass = loader.loadClass("sample.Games");
            var fieldClass = loader.loadClass("sample.Games$Field");
            var gameClass = loader.loadClass("sample.Games$Game");
            var pointClass = loader.loadClass("sample.Games$Point");
            var playerClass = loader.loadClass("sample.Games$Player");
            var field = fieldClass.getConstructor(int.class, int.class).newInstance(12, 8);

            var initial = generatedMethod(gamesClass, "initial_game__").invoke(null, field);
            assertThat(initial).isInstanceOf(gameClass);
            assertThat(gameClass.getMethod("field").invoke(initial)).isSameAs(field);
            assertThat((List<?>) gameClass.getMethod("moves").invoke(initial)).isEmpty();
            assertThat(gameClass.getMethod("to_move").invoke(initial)).isEqualTo(playerClass.getEnumConstants()[0]);
            assertThat(generatedMethod(gamesClass, "field_width__").invoke(null, initial)).isEqualTo(12);
            assertThat(generatedMethod(gamesClass, "current_player__").invoke(null, initial)).isEqualTo(playerClass.getEnumConstants()[0]);

            var withMove = generatedMethod(gamesClass, "game_with_move__").invoke(null, field);
            var moves = (List<?>) gameClass.getMethod("moves").invoke(withMove);
            assertThat(moves).singleElement().isInstanceOf(pointClass);
            assertThat(pointClass.getMethod("x").invoke(moves.getFirst())).isEqualTo(3);
            assertThat(generatedMethod(gamesClass, "first_move_x__").invoke(null, withMove)).isEqualTo(3);
            assertThat(generatedMethod(gamesClass, "same_game__").invoke(null, withMove, withMove)).isEqualTo(true);

            var updated = generatedMethod(gamesClass, "updated_nested_values__").invoke(null, field);
            var updatedField = gameClass.getMethod("field").invoke(updated);
            var updatedMoves = (List<?>) gameClass.getMethod("moves").invoke(updated);
            assertThat(updatedField).isInstanceOf(fieldClass);
            assertThat(fieldClass.getMethod("width").invoke(updatedField)).isEqualTo(15);
            assertThat(updatedMoves).singleElement().isInstanceOf(pointClass);
            assertThat(pointClass.getMethod("x").invoke(updatedMoves.getFirst())).isEqualTo(8);

            var copied = generatedMethod(gamesClass, "copied_game__").invoke(null, withMove);
            assertThat(copied).isInstanceOf(gameClass).isEqualTo(withMove);
            assertThat(gamesClass.getField("COPY_GAME").get(null)).isInstanceOf(gameClass);

            var wrapped = (java.util.Map<?, ?>) generatedMethod(gamesClass, "wrapped_game__").invoke(null, field);
            assertThat(wrapped.get("__type")).isEqualTo("Success");
            assertThat(wrapped.get("value")).isInstanceOf(gameClass);

            var uiClass = loader.loadClass("sample.UI");
            var javaUiClass = loader.loadClass("sample.JavaUI");
            var javaUi = javaUiClass.getConstructor().newInstance();
            uiClass.getMethod("draw_field", gameClass).invoke(javaUi, withMove);
            assertThat(javaUiClass.getField("received").get(javaUi)).isSameAs(withMove);
        }
    }

    @Test
    void generatesSameNamedDataAsTopLevelRecordWithStaticModuleMethods() throws Exception {
        writeSource("paper-soccer/Field.cfun", """
                from /capy/lang/Result import { Result, Success, Error }

                type field_size -> int with constructor {
                    if value < 1
                    then Error { message: "invalid field size" }
                    else Success { value }
                }

                data Field {
                    width: field_size,
                    height: field_size,
                    goal_width: field_size,
                } with constructor {
                    if goal_width >= width
                    then Error { message: "invalid goal width" }
                    else Success { * { width, height, goal_width } }
                }

                data Wrapper { value: Field } with constructor {
                    Success { value }
                }

                fun create_field(width: field_size, height: field_size, goal_width: field_size): Result[Field] =
                    Field { width, height, goal_width }

                fun create_wrapper(value: Field): Result[Wrapper] = Wrapper { value }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var field = outputDir().resolve("paper_soccer/Field.java");
        assertThat(field)
                .exists()
                .content()
                .contains("public record Field(int width, int height, int goal_width) {")
                .contains("public static java.lang.Object __capy_constructor_field_size__")
                .contains("public static java.lang.Object __capy_constructor_Field__")
                .contains("value instanceof java.lang.Record record && type.equals(record.getClass().getSimpleName())")
                .doesNotContain("public final class Field")
                .doesNotContain("private Field()");
        var classes = compileJava(field);
        try (var loader = new URLClassLoader(new java.net.URL[]{classes.toUri().toURL()})) {
            var fieldClass = loader.loadClass("paper_soccer.Field");
            var result = (java.util.Map<?, ?>) generatedMethod(fieldClass, "create_field__").invoke(null, 5, 8, 3);

            assertThat(result.get("__type")).isEqualTo("Success");
            assertThat(result.get("value")).isInstanceOf(fieldClass);

            var wrapperResult = (java.util.Map<?, ?>) generatedMethod(fieldClass, "create_wrapper__")
                    .invoke(null, result.get("value"));
            var wrapper = (java.util.Map<?, ?>) wrapperResult.get("value");
            assertThat(wrapper.get("__type")).isEqualTo("Wrapper");
            assertThat(wrapper.get("value")).isSameAs(result.get("value"));
        }
    }

    @Test
    void retainsRenamedModuleClassWhenSameNamedObjectInterfaceExists() throws Exception {
        writeSource("sample/Foo.cfun", """
                data Foo { value: int }
                """);
        writeSource("sample/Foo.coo", """
                interface Foo {
                    def value(): int
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var objectInterface = outputDir().resolve("sample/Foo.java");
        var module = outputDir().resolve("sample/Foo_.java");
        assertThat(objectInterface)
                .content()
                .contains("public interface Foo");
        assertThat(module)
                .content()
                .contains("public final class Foo_")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(objectInterface, module);
    }

    @Test
    void preservesNominalTypeInSelfReferentialTopLevelRecord() throws Exception {
        var source = writeSource("sample/Node.cfun", """
                data Node { children: List[Node] }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public record Node(java.util.List<Node> children) {")
                .doesNotContain("java.util.List<java.lang.Object> children");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void castsParameterizedTopLevelRecordFieldsThroughObject() throws Exception {
        var source = writeSource("sample/Game.cfun", """
                data Game { moves: List[Point] }
                data Point { x: int, y: int }

                fun Game.ball_position(): Option[Point] = this.moves[-1]
                fun add_move(game: Game, point: Point): List[Point] = game.moves + point
                fun Game.has_no_moves(): bool = this.moves == []
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("((java.util.List<java.lang.Object>) (java.lang.Object) __capy_data_field(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void usesGeneratedEnumTypeForFieldInTopLevelRecord() throws Exception {
        var source = writeSource("sample/Path.cfun", """
                enum PathRoot { RELATIVE, ABSOLUTE }

                data Path { root: PathRoot }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public enum PathRoot { RELATIVE, ABSOLUTE }")
                .contains("public record Path(PathRoot root) {")
                .doesNotContain("public record Path(java.lang.Object root)");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassForPrivateSameNamedData() throws Exception {
        var source = writeSource("sample/Secret.cfun", """
                private data Secret { value: int }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Secret")
                .doesNotContain("public record Secret(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassForIllegalJavaRecordComponentName() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                data Foo { wait: int }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassWhenSameNamedDataImplementsLocalUnion() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                union Parent = Foo
                data Foo { value: int }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .contains("interface Parent")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void validatesInheritedFieldsBeforeTopLevelRecordPromotion() throws Exception {
        var base = writeSource("sample/Base.cfun", """
                data Base { wait: int }
                """);
        var source = writeSource("sample/Foo.cfun", """
                from /sample/Base import { Base }

                data Foo = { ...Base }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(base), generatedPath(source));
    }

    @Test
    void retainsModuleClassWhenQuotedDataFieldCollidesWithConstant() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                data Foo { "VALUE": int }

                const VALUE: int = 1
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .contains("static final int VALUE = 1;")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassForDuplicateNormalizedRecordComponentNames() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                data Foo { "foo-bar": int, foo_bar: int }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassWhenRecordComponentNameNeedsNormalization() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                data Foo { "foo-bar": int }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassWhenRecordAccessorCollidesWithZeroArgumentMethod() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                data Foo { main: int }

                fun main(): int = 1
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .contains("static int main()")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void readsTopLevelRecordComponentsThroughAccessors() throws Exception {
        var source = writeSource("sample/Field.cfun", """
                from /capy/meta_prog/Reflection import { DataValueInfo, reflection }
                from /capy/test/Assert import { DataAssert, assert_that }

                data Field { width: int }
                data Extended { width: int, y: int }

                fun updated_width(value: Field): int = value.with(width: 2).width

                fun updated_widened(value: data): int = value.with(width: 3).width

                fun widened_width(value: data): int = value.width

                fun generic_data(value: Field): bool =
                    match value with
                    case data -> true
                    case _ -> false

                fun equals_literal(value: Field): bool = value == Field { width: 7 }

                fun equals_nested(value: Field): bool = [value] == [Field { width: 7 }]

                fun contains_record(value: Field): bool = [Field { width: 7 }] ? value

                fun set_contains_record(value: Field): bool = { Field { width: 7 }, }.contains(value)

                fun set_subset_record(value: Field): bool = { value, }.is_subset_of({ Field { width: 7 }, })

                fun remove_record(value: Field): List[Field] = [Field { width: 7 }] - value

                fun equal_assertion(value: Field): DataAssert = assert_that(value).is_equal_to(Field { width: 7 })

                fun reflected(value: data): DataValueInfo = reflection(value)

                fun spread_width(value: Field): int = Extended { ...value, y: 2 }.width
                """);
        writeSource("sample/Field.coo", """
                class Reader {
                    def width(value: Field): int = value.width

                    def matched(value: Field): int =
                        match value with
                        case Field { width } -> width
                        case _ -> 0
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("public record Field(int width)")
                .contains("((Field) __capy_record_value).width()");
        var classes = compileJava(generated);
        try (var loader = new URLClassLoader(new java.net.URL[]{classes.toUri().toURL()})) {
            var fieldClass = loader.loadClass("sample.Field");
            var field = fieldClass.getConstructor(int.class).newInstance(7);
            var readerClass = loader.loadClass("sample.Field$Reader");
            var reader = readerClass.getConstructor().newInstance();

            assertThat(readerClass.getMethod("width", Object.class).invoke(reader, field)).isEqualTo(7);
            assertThat(readerClass.getMethod("matched", Object.class).invoke(reader, field)).isEqualTo(7);
            assertThat(generatedMethod(fieldClass, "updated_width__").invoke(null, field)).isEqualTo(2);
            assertThat(generatedMethod(fieldClass, "updated_widened__").invoke(null, field)).isEqualTo(3);
            assertThat(generatedMethod(fieldClass, "widened_width__").invoke(null, field)).isEqualTo(7);
            assertThat(generatedMethod(fieldClass, "generic_data__").invoke(null, field)).isEqualTo(true);
            assertThat(generatedMethod(fieldClass, "equals_literal__").invoke(null, field)).isEqualTo(true);
            assertThat(generatedMethod(fieldClass, "equals_nested__").invoke(null, field)).isEqualTo(true);
            assertThat(generatedMethod(fieldClass, "contains_record__").invoke(null, field)).isEqualTo(true);
            assertThat(generatedMethod(fieldClass, "set_contains_record__").invoke(null, field)).isEqualTo(true);
            assertThat(generatedMethod(fieldClass, "set_subset_record__").invoke(null, field)).isEqualTo(true);
            assertThat((java.util.List<?>) generatedMethod(fieldClass, "remove_record__").invoke(null, field)).isEmpty();
            var assertion = (java.util.Map<?, ?>) generatedMethod(fieldClass, "equal_assertion__").invoke(null, field);
            var assertions = (java.util.List<?>) assertion.get("assertions");
            var assertionResult = (java.util.Map<?, ?>) ((java.util.function.Supplier<?>) assertions.getFirst()).get();
            assertThat(assertionResult.get("result")).isEqualTo(true);
            var reflected = (java.util.Map<?, ?>) generatedMethod(fieldClass, "reflected__").invoke(null, field);
            assertThat(reflected.get("name")).isEqualTo("Field");
            var reflectedFields = (java.util.List<?>) reflected.get("fields");
            assertThat(((java.util.Map<?, ?>) reflectedFields.getFirst()).get("value")).isEqualTo(7);
            assertThat(generatedMethod(fieldClass, "spread_width__").invoke(null, field)).isEqualTo(7);
        }
    }

    @Test
    void erasesGenericParametersFromTopLevelRecordAccessorCasts() throws Exception {
        var source = writeSource("sample/Box.cfun", """
                data Box[T] { value: T }

                fun Box[T].get(): T = this.value
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public record Box<T>(T value)")
                .contains("((Box) __capy_record_value).value()")
                .doesNotContain("((Box<T>) __capy_record_value)");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void retainsModuleClassWhenRecordAccessorCollidesWithObjectConstructorWrapper() throws Exception {
        var source = writeSource("sample/Foo.cfun", """
                data Foo { "Bar__1_0": int }
                """);
        writeSource("sample/Foo.coo", """
                class Bar {}
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("public final class Foo")
                .contains("Bar__1_0()")
                .doesNotContain("public record Foo(");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void generatesOneTopLevelJavaInterfaceWhenSourceFileHasADifferentName() throws Exception {
        writeSource("paper-soccer/ui/UIContract.coo", """
                interface UI {
                    def draw_field(game_field: String): String
                }

                class ConsoleUI: UI {
                    override def draw_field(game_field: String): String = game_field
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var ui = outputDir().resolve("paper_soccer/ui/UI.java");
        var uiContract = outputDir().resolve("paper_soccer/ui/UIContract.java");
        assertThat(ui)
                .exists()
                .content()
                .contains("public interface UI {");
        assertThat(uiContract)
                .exists()
                .content()
                .doesNotContain("interface UI {")
                .contains("class ConsoleUI implements paper_soccer.ui.UI");
        assertJavaCompiles(ui, uiContract);
    }

    @Test
    void generatesLegalJavaWhenOmittedVoidMethodDiscardsValueExpression() throws Exception {
        writeSource("sample/DiscardValues.coo", """
                class Discarder {
                    def discard() = 1 + 2
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var generated = outputDir().resolve("sample/DiscardValues.java");
        assertThat(generated)
                .exists()
                .content()
                .contains("((java.util.function.Supplier<java.lang.Object>) () -> (1 + 2)).get();");
        assertJavaCompiles(generated);
    }

    @Test
    void renamesModuleClassWhenTopLevelInterfaceHasTheSourceFileName() throws Exception {
        writeSource("paper-soccer/ui/UI.coo", """
                interface UI {
                    def draw_field(game_field: String): String
                }

                class ConsoleUI: UI {
                    override def draw_field(game_field: String): String = game_field

                    def copy(): ConsoleUI = this
                }
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();

        var ui = outputDir().resolve("paper_soccer/ui/UI.java");
        var uiModule = outputDir().resolve("paper_soccer/ui/UI_.java");
        assertThat(ui)
                .exists()
                .content()
                .contains("public interface UI {")
                .doesNotContain("public final class UI");
        assertThat(uiModule)
                .exists()
                .content()
                .contains("public final class UI_")
                .contains("class ConsoleUI implements paper_soccer.ui.UI")
                .contains("paper_soccer.ui.UI_.ConsoleUI copy()")
                .doesNotContain("interface UI {");
        assertJavaCompiles(ui, uiModule);
    }

    @Test
    void generatesTypedLambdasWithUserDefinedAndGenericTypesForEveryBackend() throws Exception {
        var source = writeSource("sample/TypedLambdaModels.cfun", """
                data User { name: String }
                data Box[T] { value: T }
                data UserMapper { transform: User => User }
                data NamedAccount { name: String }
                data AnonymousAccount {}
                union Account = NamedAccount | AnonymousAccount
                enum Status { READY, DONE }
                type user_id -> int

                fun apply_int(transform: int => int, value: int): int = transform(value)

                fun mapped_int(value: int): int = apply_int(item: int => item + 1, value)

                fun apply_string(transform: String => String, value: String): String = transform(value)

                fun mapped_string(value: String): String =
                    apply_string(item: String => item + "!", value)

                fun apply_user(transform: User => User, user: User): User = transform(user)

                fun identity_user(user: User): User = user

                fun mapped_user(user: User): User =
                    apply_user(value: User => User { name: value.name + "!" }, user)

                fun referenced_user(user: User): User = apply_user(:identity_user, user)

                fun user_factory(): User => User = value: User => value

                fun user_mapper(): UserMapper = UserMapper { transform: value: User => value }

                fun User.map_name(transform: String => String): User = User { name: transform(this.name) }

                fun mapped_user_method(user: User): User =
                    user.map_name(value: String => value + "!")

                fun nested_user(user: User): User =
                    apply_user(value: User => apply_user(inner: User => inner, value), user)

                fun apply_account(transform: Account => Account, account: Account): Account = transform(account)

                fun mapped_account(account: Account): Account =
                    apply_account(value: Account => value, account)

                fun apply_status(transform: Status => Status, status: Status): Status = transform(status)

                fun mapped_status(status: Status): Status = apply_status(value: Status => value, status)

                fun apply_id(transform: user_id => user_id, value: user_id): user_id = transform(value)

                fun mapped_id(value: user_id): user_id = apply_id(item: user_id => item, value)

                fun map_box(transform: T => T, box: Box[T]): Box[T] = Box { transform(box.value) }

                fun apply_generic(transform: T => T, value: T): T = transform(value)

                fun mapped_generic(value: T): T = apply_generic(item: T => item, value)

                fun mapped_box(box: Box[User]): Box[User] =
                    map_box(value: User => User { name: value.name }, box)

                fun choose_user(transform: (User, User, User) => User, first: User, second: User, third: User): User =
                    transform(first, second, third)

                fun chosen_user(first: User, second: User, third: User): User =
                    choose_user((left, _, _) => left, first, second, third)
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();
        assertThat(generatedPath(source))
                .content()
                .contains("record User(")
                .contains("interface Account")
                .contains("java.util.function.Function<java.lang.Object, java.lang.Object>")
                .contains("__CapyFunction3<java.lang.Object")
                .doesNotContain("__capy_typed_lambda|");
        assertJavaCompiles(generatedPath(source));

        assertThat(compileGenerateStderr("javascript")).isEmpty();
        assertThat(generatedPath(source, ".js"))
                .content()
                .doesNotContain("__capy_typed_lambda|");

        assertThat(compileGenerateStderr("python")).isEmpty();
        assertThat(generatedPath(source, ".py"))
                .content()
                .doesNotContain("__capy_typed_lambda|");
    }

    @ParameterizedTest
    @ValueSource(strings = {"java", "javascript", "python"})
    void rejectsIncompatibleTypedLambdasAndFunctionReferencesBeforeGeneration(String outputType) throws Exception {
        var source = writeSource("sample/InvalidTypedLambda.cfun", """
                data User { name: String }
                data UserMapper { transform: User => User }

                fun apply_user(transform: User => User, user: User): User = transform(user)

                fun string_identity(value: String): String = value

                fun bad_parameter(user: User): User =
                    apply_user(value: String => User { name: value }, user)

                fun bad_return(user: User): User =
                    apply_user(value: User => value.name, user)

                fun bad_reference(user: User): User = apply_user(:string_identity, user)

                fun bad_field(): UserMapper =
                    UserMapper { transform: value: String => User { name: value } }
                """);

        assertThat(compileGenerateStderr(outputType))
                .contains("expects parameter 1 to accept `User`, but the lambda declares `String`")
                .contains("expects the lambda to return `User`, but it returns `String`")
                .contains("expects parameter 1 to accept `User`, but function `string_identity` declares `String`")
                .contains("Field `transform` of data `UserMapper` expects parameter 1 to accept `User`, but the lambda declares `String`");
        assertThat(generatedPath(source, switch (outputType) {
            case "javascript" -> ".js";
            case "python" -> ".py";
            default -> ".java";
        })).doesNotExist();
    }

    @ParameterizedTest
    @ValueSource(strings = {"java", "javascript", "python"})
    void rejectsFunctionCallAsTestPipeRightOperandBeforeGeneration(String outputType) throws Exception {
        writeSource("sample/Main.cfun", "fun value(): int = 1");
        var testSource = writeTestSource("sample/Main.test.cfun", """
                fun broken(value: Result[int]): bool = value | diff()

                private fun diff(): int = 1
                """);

        assertThat(compileGenerateWithTestsStderr(outputType))
                .contains("Compilation failed with 1 error(s):")
                .contains("Operator `|` requires a lambda or function reference on its right-hand side; "
                        + "found a function call.");
        var extension = switch (outputType) {
            case "javascript" -> ".js";
            case "python" -> ".py";
            default -> ".java";
        };
        assertThat(generatedTestPath(testSource, extension)).doesNotExist();
    }

    @Test
    void compilesJavaForRootModulesWithAbsoluteImports() throws Exception {
        var support = writeSource("Support.cfun", """
                from /capy/collection/Seq import { Seq, Cons, End }

                data Counter { value: int }

                fun increment(value: int): int = value + 1

                fun Counter.doubled(): int = this.value * 2

                fun countdown(value: int): Cons[int] =
                    @/capy/meta_prog/Recursive
                    fun rest(value: int): Seq[int] =
                        if value <= 0
                        then End {}
                        else Cons { value: value, rest: () => rest(value - 1) }
                    ---
                    Cons { value: value, rest: () => rest(value - 1) }
                """);
        var main = writeSource("main.cfun", """
                from /capy/collection/Seq import { Seq }
                from /Support import { Counter, increment, countdown }

                fun answer(): int = increment(41)

                fun doubled(counter: Counter): int = counter.doubled()

                fun values(): Seq[int] = countdown(3)

                fun render(counter: Counter): String = render(counter.value)

                fun render(value: int): String = "value"
                """);

        compileGenerate();

        assertThat(generatedPath(main))
                .content()
                .doesNotContain("import static Support.")
                .contains("Support.increment")
                .contains("Support.Counter_doubled")
                .doesNotContain("__capy_tail_counter");
        var seqRuntime = tempDir.resolve("java-runtime/capy/collection/Seq.java");
        Files.createDirectories(seqRuntime.getParent());
        Files.writeString(seqRuntime, """
                package capy.collection;

                import java.util.List;
                import java.util.function.Supplier;

                public interface Seq<T> {
                    record Cons<T>(T value, Supplier<Seq<T>> rest) implements Seq<T> {}
                    enum End implements Seq<Object> { INSTANCE }
                    static <T> Seq<T> toSeq(Object value) { return (Seq<T>) End.INSTANCE; }
                    default List<T> asList() { return List.of(); }
                }
                """);
        assertJavaCompiles(
                seqRuntime,
                generatedPath(support),
                generatedPath(main)
        );
    }

    @Test
    void generatesQualifiedStandardLibraryCallsThroughCompileGenerate() throws Exception {
        var source = writeSource("sample/QualifiedEffect.cfun", """
                import /capy/lang/Effect
                import /capy/io/Console
                import /capy/lang/Primitives

                fun qualified_pure(value: String): Effect[String] =
                    Effect.pure(value)

                fun qualified_print(value: String): Effect[String] =
                    Console.println(value)

                fun qualified_parse(value: String): /capy/lang/Result[int] =
                    Primitives.to_int(value)
                """);

        compileGenerate();

        assertThat(generatedPath(source))
                .content()
                .contains("return capy.lang.Effect.pure(value);")
                .contains("dev.capylang.ConsoleUtil.println")
                .contains("return __capy_parse_int(value);")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsMalformedPipeExpressionAndDoesNotWriteJavaOutput() throws Exception {
        var source = writeSource("sample/PipeFailure.cfun", """
                fun broken(values: List[int]): List[int] =
                    values |* value.to_string()
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Operator `|*` requires a lambda or function reference on its right-hand side; "
                        + "found a method call.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsExtensionMethodArityMismatchBeforeJavaGeneration() throws Exception {
        writeSource("paper-soccer/Game.cfun", """
                data Field {}
                data Game {
                    game_field: Field,
                    moves: List[Point],
                    to_move: Player,
                }

                data Point { x: int, y: int }
                enum Player { PLAYER_A, PLAYER_B }
                enum Move { UP, DOWN }

                fun Game.ball_position(game: Game): Point = Point { x: 0, y: 0 }
                fun Point.move(move: Move): Point = this

                /// Applies a move to the game.
                fun move(game: Game, move: Move): Result[Game] =
                    Success {
                        Game {
                            game_field: game.game_field,
                            moves: game.moves + game.ball_position().move(move),
                            to_move: game.to_move
                        }
                    }
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Method `ball_position` on receiver type `Game` expects 1 argument, but received 0.")
                .doesNotContain("the function contains an expression unsupported by the Java backend");

        assertThat(outputDir().resolve("paper_soccer/Game.java")).doesNotExist();
    }

    @Test
    void generatesJavaForExtensionMethodCallWithMatchingArity() throws Exception {
        writeSource("paper-soccer/Game.cfun", """
                data Game {}
                data Point {}

                fun Game.ball_position(game: Game): Point = Point {}

                fun move(game: Game): Point = game.ball_position(game)
                """);

        assertThat(compileGenerateStderr()).isEmpty();
        assertThat(outputDir().resolve("paper_soccer/Game.java"))
                .exists()
                .content()
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsUnresolvedCallAndDoesNotWriteJavaOutput() throws Exception {
        var source = writeSource("paper-soccer/CallFailure.cfun", """
                /// Calls a function that is not in scope.
                fun broken(): int =
                    missing()
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /paper-soccer/CallFailure.cfun:3:4: Unresolved function call `missing`.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsWrongFunctionArityAndDoesNotWriteJavaOutput() throws Exception {
        var source = writeSource("sample/FunctionArityFailure.cfun", """
                fun identity(value: int): int = value

                fun broken(): int = identity()
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Function `identity` does not accept 0 argument(s).");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void doesNotReportImportedConsoleFunctionAsUnresolved() throws Exception {
        var source = writeSource("sample/ImportedConsoleFailure.cfun", """
                from /capy/io/Console import { println }
                from /capy/lang/Effect import { Effect }

                fun broken(value: String): String =
                    let printed: Effect[String] = println(value)
                    missing()
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Unresolved function call `missing`.")
                .doesNotContain("Unresolved function call `println`.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsNonCallableFlatMapArgument() throws Exception {
        var source = writeSource("sample/FlatMapFailure.cfun", """
                from /capy/io/Console import { println }
                from /capy/lang/Effect import { Effect, pure }

                fun broken(values: List[String]): Effect[String] =
                    values | value => println(value)
                    |> pure(''), (acc, print_effect) => acc.flat_map(print_effect)
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Method `flat_map` requires a callable mapper; variable `print_effect` "
                        + "is not callable in this context.")
                .doesNotContain("Unresolved function call `println`.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void acceptsUntypedLambdaBindingAsCallableMapper() throws Exception {
        var source = writeSource("sample/LocalLambdaMapper.cfun", """
                from /capy/lang/Result import { Result }

                fun map_result(result: Result[int]): Result[int] =
                    let mapper = value => value + 1
                    result.map(mapper)
                """);

        assertThat(compileGenerateStderr()).isEmpty();
        assertThat(generatedPath(source))
                .exists()
                .content()
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesEffectFlatMapCall() throws Exception {
        var source = writeSource("sample/EffectFlatMap.cfun", """
                import /capy/lang/Effect

                fun chained(effect: Effect[int], next: Effect[String]): Effect[String] =
                    effect.flat_map(_ => next)
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("flatMap((__capy_ignored_0) -> next)")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsPipelineVariablesOutsideTheirLambdaScopeDuringCompilation() throws Exception {
        var source = writeSource("sample/UnboundPipelineVariables.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun broken(values: List[Result[int]]): Result[List[int]] =
                    values
                    |> Success { [] }, (acc, int_result) =>
                        acc
                        | acc_list =>
                            int_result
                            | digit => Success { acc_list + digit }
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 2 error(s):")
                .contains("Unresolved variable `int_result`.")
                .contains("Unresolved variable `acc_list`.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void generatesNestedPipelinesWhenLambdaBodiesAreGrouped() throws Exception {
        var source = writeSource("sample/GroupedPipelineVariables.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun accumulate(values: List[Result[int]]): Result[List[int]] =
                    values
                    |> Success { [] }, (acc, int_result) => {
                        acc
                        | acc_list => {
                            int_result
                            | digit => Success { acc_list + digit }
                        }
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesImportedToIntInsideStringPipe() throws Exception {
        var source = writeSource("sample/StringToInts.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result }

                fun parse_chars(value: String): Seq[Result[int]] =
                    value | char => to_int(char)
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesGroupedResultReductionFromString() throws Exception {
        var source = writeSource("sample/StringResultReduction.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }

                fun parse_digits(value: String): Result[List[int]] =
                    value
                    | char => to_int(char)
                    |> Success { [] }, (acc, int_result) => {
                        acc
                        | acc_list => {
                            int_result
                            | digit => Success { acc_list + digit }
                        }
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesResultMapAsResultInsideCollection() throws Exception {
        var source = writeSource("sample/ResultMapCollection.cfun", """
                from /capy/lang/Result import { Result }

                fun map_results(values: List[Result[int]]): List[Result[List[int]]] =
                    (values | result => result.map(value => [value])).as_list()
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("return __capy_data(\"Success\", __capy_value_fields(java.util.List.of(((java.lang.Integer) value))))")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generated);
    }

    @Test
    void generatesChainedResultBindingsInJava() throws Exception {
        var source = writeSource("sample/ResultBindings.cfun", """
                from /capy/lang/Result import { Result, Success }

                private fun success(value: int): Result[int] = Success { value }

                fun total(): Result[int] =
                    let first: int <- success(1)
                    let second: int <- success(2)
                    let third: int <- success(3)
                    first + second + third
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("__capy_bind_first")
                .contains("__capy_bind_second")
                .contains("__capy_bind_third")
                .contains("if (!__capy_result_is_success(")
                .contains("return __capy_data(\"Success\"")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generated);
    }

    @Test
    void generatesChainedResultBindingsWithImportedConstantInTypedList() throws Exception {
        var values = writeSource("sample/Values.cfun", """
                data Value { value: int }

                const FINAL: Value = Value { value: 4 }
                """);
        var source = writeSource("sample/ImportedConstantResultBindings.cfun", """
                from /sample/Values import { Value, FINAL }
                from /capy/lang/Result import { Result, Success }
                from /capy/test/Assert import { Assert, assert_that }

                private fun success(value: int): Result[Value] = Success { Value { value } }

                fun compare(): Result[Assert] =
                    let first: Value <- success(1)
                    let second: Value <- success(2)
                    let third: Value <- success(3)
                    let actual: List[Value] = [first, second, third, FINAL]
                    assert_that(actual).is_equal_to([first, second, third, FINAL])
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        var generated = generatedPath(source);
        assertThat(generated)
                .content()
                .contains("__capy_bind_first")
                .contains("Values.FINAL")
                .contains("__capy_assert_equal(")
                .contains("return __capy_data(\"Success\"")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generatedPath(values), generated);
    }

    @Test
    void generatesResultBindingWithPrimitiveBackedFieldAssertions() throws Exception {
        var valueSource = writeSource("sample/Kaprekar.cfun", """
                from /capy/lang/Primitives import { digit, ONE_DIGIT, TWO_DIGIT, EIGHT_DIGIT, NINE_DIGIT }
                from /capy/lang/Result import { Result, Success }

                data Kaprekar {
                    first: digit,
                    second: digit,
                    third: digit,
                    fourth: digit
                } with constructor {
                    Success { * { first, second, third, fourth } }
                }

                fun Kaprekar.diff(): Kaprekar = this
                """);
        var source = writeTestSource("sample/PrimitiveBackedAssertions.test.cfun", """
                from /sample/Kaprekar import { Kaprekar }
                from /capy/lang/Primitives import { ONE_DIGIT, TWO_DIGIT, EIGHT_DIGIT, NINE_DIGIT }
                from /capy/lang/Result import { Result }
                from /capy/test/Assert import { Assert, assert_all, assert_that }

                private fun kaprekar_new_instance(): Result[Assert] =
                    let kaprekar: Kaprekar <- Kaprekar {
                        first: ONE_DIGIT,
                        second: NINE_DIGIT,
                        third: EIGHT_DIGIT,
                        fourth: TWO_DIGIT
                    }
                    assert_all([
                        assert_that(kaprekar.first).is_equal_to(ONE_DIGIT),
                        assert_that(kaprekar.second).is_equal_to(NINE_DIGIT),
                        assert_that(kaprekar.third).is_equal_to(EIGHT_DIGIT),
                        assert_that(kaprekar.fourth).is_equal_to(TWO_DIGIT),
                    ])

                const K1234: Result[Kaprekar] = Kaprekar {
                    first: ONE_DIGIT,
                    second: TWO_DIGIT,
                    third: EIGHT_DIGIT,
                    fourth: NINE_DIGIT
                }

                private fun mapped_assertion(): Assert =
                    assert_that(K1234.map(k => k.diff())).is_equal_to(K1234)
                """);

        assertThat(compileGenerateWithTestsStderr()).isEmpty();

        var generated = generatedTestPath(source);
        assertThat(generated)
                .content()
                .contains("__capy_bind_kaprekar")
                .contains("__capy_assert_equal(")
                .contains("Kaprekar.Kaprekar_diff__")
                .contains("return __capy_data(\"Success\"")
                .doesNotContain(".diff()")
                .doesNotContain(".isEqualTo(")
                .doesNotContain("unsupported(\"assert function call\")")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generatedPath(valueSource), generated);
    }

    @Test
    void generatesPrimitiveBackedConstructorFromResultValue() throws Exception {
        var source = writeSource("sample/PrimitiveResult.cfun", """
                from /capy/lang/Result import { Result, Success }

                type digit -> int with constructor {
                    Success { value }
                }

                fun convert(value: Result[int]): Result[digit] =
                    value | int_value => digit { int_value }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void rejectsPrimitiveLiteralForPrimitiveBackedDataFieldDuringCompilation() throws Exception {
        var source = writeSource("sample/PrimitiveBackedField.cfun", """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }

                data Code { value: digit }

                const CODE: Code = Code! { value: 6 }
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Field `value` of data `Code` has type `int`, but requires primitive-backed type `digit`")
                .contains("construct `digit` explicitly and unwrap its `Result` before constructing `Code`");

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void generatesDigitReductionFromString() throws Exception {
        var source = writeSource("sample/StringDigitReduction.cfun", """
                import /capy/collection/Seq
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }

                type digit -> int with constructor {
                    Success { value }
                }

                fun parse_digits(value: String): Result[Seq[digit]] =
                    value
                    | char => to_int(char)
                    |> Success { Seq.to_seq([]) }, (acc, int_char) => {
                        acc.flat_map(acc_list => {
                            int_char.flat_map(int_value => {
                                digit { int_value }.map(digit => acc_list + digit)
                            })
                        })
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesImportedDigitSequenceReductionFromStringMethod() throws Exception {
        var source = writeSource("sample/ImportedStringDigitReduction.cfun", """
                import /capy/collection/Seq
                import /capy/lang/String
                from /capy/lang/Primitives import { digit }
                from /capy/lang/Result import { Result, Success }

                fun parse_digits(value: String): Result[Seq[digit]] =
                    value
                    | char => char.to_int()
                    |> Success { Seq.to_seq([]) }, (acc, int_char) => {
                        acc.flat_map(acc_list => {
                            int_char.flat_map(int_value => {
                                digit { int_value }.map(digit => acc_list + digit)
                            })
                        })
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();
        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .contains("__capy_seq_append(")
                .doesNotContain("Unsupported CFUN expression at");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void generatesImportedPrimitiveBackedConstructor() throws Exception {
        var importedSource = writeSource("sample/Digit.cfun", """
                from /capy/lang/Result import { Success }

                type digit -> int with constructor {
                    Success { value }
                }
                """);
        var source = writeSource("sample/ImportedDigit.cfun", """
                from /capy/lang/Primitives import { to_int }
                from /capy/lang/Result import { Result, Success }
                from /sample/Digit import { digit }

                fun parse_digits(value: String): Result[List[digit]] =
                    value
                    | char => to_int(char)
                    |> Success { [] }, (acc, int_char) => {
                        acc
                        | acc_list => {
                            int_char
                            | int_value => {
                                digit { int_value }
                                | digit => Success { acc_list + digit }
                            }
                        }
                    }
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(importedSource))
                .content()
                .contains("public static java.lang.Object __capy_constructor_digit");

        assertThat(generatedPath(source))
                .content()
                .contains("__capy_parse_int")
                .contains("sample.Digit.")
                .contains("java.lang.Object digit =")
                .contains("__capy_list_append(((java.util.List<java.lang.Integer>) acc_list), ((java.lang.Integer) digit))")
                .doesNotContain("__capy_list_append(acc_list, ((java.lang.Integer) __capy_data_field(digit")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesExtensionMethodOwnedByExplicitlyImportedType() throws Exception {
        var importedSource = writeSource("sample/Item.cfun", """
                data Item { value: String }

                fun Item.render(): String = this.value
                """);
        var source = writeSource("sample/ImportedItemMethod.cfun", """
                from /sample/Item import { Item }

                fun render(item: Item): String = item.render()
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(importedSource))
                .content()
                .contains("public static java.lang.String Item_render");

        assertThat(generatedPath(source))
                .content()
                .contains("import static sample.Item.")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void reportsUnsupportedMethodForAnotherKnownReceiverType() throws Exception {
        var source = writeSource("sample/StringMethodFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun broken(value: Result[int]): Result[int] =
                    value.not_a_java_method()
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/StringMethodFailure.cfun:4:4: Method `not_a_java_method` is not defined for receiver type `Result[int]`.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsUnsupportedListMethodDuringCompilation() throws Exception {
        var source = writeSource("sample/ListMethodFailure.cfun", """
                fun broken(values: List[int]): bool =
                    values.length() == 4
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/ListMethodFailure.cfun:2:4: Method `length` on `List` is not supported by the Java backend.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsNonIndexableReceiverDuringCompilationForEveryBackend() throws Exception {
        var source = writeSource("sample/ResultIndexFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun broken(args: Result[Tuple[String, String, String]]): Result[int] =
                    args[0].to_int()
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 1 error(s):
                    /sample/ResultIndexFailure.cfun:4:4: Index access is not defined for receiver type `Result[Tuple[String, String, String]]`; extract a `Tuple` value before indexing.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void allowsIndexingParameterizedFieldFromLinkedDataForEveryBackend() throws Exception {
        var source = writeSource("sample/LinkedDataFieldIndex.cfun", """
                from /capy/collection/List import { * }
                from /capy/lang/Option import { Option }
                from /capy/meta_prog/Reflection import { DataValueInfo, FieldValueInfo }

                fun first_field(info: DataValueInfo): Option[FieldValueInfo] = info.fields[0]
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEmpty();
            var extension = switch (outputType) {
                case "java" -> ".java";
                case "javascript" -> ".js";
                case "python" -> ".py";
                default -> throw new IllegalStateException("Unexpected output type: " + outputType);
            };
            assertThat(generatedPath(source, extension)).exists();
        }
    }

    @Test
    void allowsIndexingThroughPrimitiveBackedReceiverForEveryBackend() throws Exception {
        var source = writeSource("sample/PrimitiveBackedIndex.cfun", """
                from /capy/lang/Option import { Option }

                type token -> String

                fun first(value: token): Option[char] = value[0]
        """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEmpty();
            var extension = switch (outputType) {
                case "java" -> ".java";
                case "javascript" -> ".js";
                case "python" -> ".py";
                default -> throw new IllegalStateException("Unexpected output type: " + outputType);
            };
            assertThat(generatedPath(source, extension)).exists();
        }
    }

    @Test
    void reportsNestedLambdaArgumentTypeDuringCompilation() throws Exception {
        var source = writeSource("sample/NestedLambdaTypeFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun render(result: Result[List[int]]): String = ""

                fun broken(result: Result[List[int]]): String =
                    render(result.map(value => item => item))
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/NestedLambdaTypeFailure.cfun:6:4: Argument 1 of function `render` has type `Result[function]`, but `Result[List[int]]` is required.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void reportsLocalFunctionArgumentTypeDuringCompilationForEveryBackend() throws Exception {
        var source = writeSource("sample/LocalFunctionTypeFailure.cfun", """
                from /capy/collection/Seq import { Seq, Cons, End }

                data Item { value: String }

                private fun render(result: Cons[Item]): String =
                    @/capy/meta_prog/Recursive
                    fun render(result: Seq[Item], acc: String): String =
                        match result with
                        case Cons cons -> render(cons.rest(), acc)
                        case End -> acc
                    ---
                    render(result.rest(), result.value)
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 1 error(s):
                    /sample/LocalFunctionTypeFailure.cfun:12:4: Argument 2 of function `render` has type `Item`, but `String` is required.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void reportsInvalidImportedFunctionCallsInTestSourcesDuringCompilationForEveryBackend() throws Exception {
        writeSource("Kaprekar.cfun", """
                data Kaprekar { value: int }

                const KAPREKAR: Kaprekar = Kaprekar { 6174 }
                """);
        var testSource = writeTestSource("Kaprekar.test.cfun", """
                from /Kaprekar import { Kaprekar, KAPREKAR }
                from /capy/collection/Seq import { Seq, to_seq }

                fun wrong_argument_count(): Seq[Kaprekar] =
                    to_seq(KAPREKAR, KAPREKAR, KAPREKAR, KAPREKAR)
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateWithTestsStderr(outputType))
                    .contains("Compilation failed with 1 error(s):")
                    .contains("Function `to_seq` does not accept 4 argument(s).");
        }

        assertThat(generatedTestPath(testSource)).doesNotExist();
    }

    @Test
    void reportsResultFlatMapReturningPlainValueDuringCompilationForEveryBackend() throws Exception {
        writeSource("sample/Values.cfun", """
                fun find_values(value: int): List[int] = [value]
                """);
        var source = writeSource("sample/ResultFlatMapTypeFailure.cfun", """
                from /capy/lang/Result import { Result }
                from /sample/Values import { find_values }

                fun broken(result: Result[int]): Result[List[int]] =
                    result.flat_map(value => find_values(value))
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 1 error(s):
                    /sample/ResultFlatMapTypeFailure.cfun:5:4: Result.flat_map mapper must return `Result`, but it returns `List[int]`.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void reportsEffectFlatMapReturningResultDuringCompilationForEveryBackend() throws Exception {
        var source = writeSource("sample/EffectFlatMapTypeFailure.cfun", """
                from /capy/lang/Effect import { Effect }
                from /capy/lang/Result import { Result, Success }

                private fun validate(value: int): Result[int] = Success { value }

                fun broken_lambda(effect: Effect[int]): Effect[int] =
                    effect.flat_map(value => Success { value })

                fun broken_reference(effect: Effect[int]): Effect[int] =
                    effect.flat_map(:validate)
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 2 error(s):
                    /sample/EffectFlatMapTypeFailure.cfun:7:4: Effect.flat_map mapper must return `Effect`, but it returns `Result[int]`.
                    /sample/EffectFlatMapTypeFailure.cfun:10:4: Effect.flat_map mapper must return `Effect`, but it returns `Result[int]`.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void acceptsValidEffectCallbacksDuringCompilationForEveryBackend() throws Exception {
        var source = writeSource("sample/ValidEffectCallbacks.cfun", """
                from /capy/lang/Effect import { Effect, pure }
                from /capy/lang/Result import { Result, Success }

                private fun effect_callback(value: int): Effect[int] = pure(value)
                private fun result_callback(value: int): Result[int] = Success { value }
                private fun nothing_callback(value: int): nothing = nothing_callback(value)

                fun flat_map_effect(effect: Effect[int]): Effect[int] =
                    effect.flat_map(:effect_callback)

                fun map_result(effect: Effect[int]): Effect[Result[int]] =
                    effect.map(:result_callback)

                fun flat_map_nothing(effect: Effect[int]): Effect[int] =
                    effect.flat_map(:nothing_callback)
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEmpty();
            assertThat(generatedPath(source, switch (outputType) {
                case "javascript" -> ".js";
                case "python" -> ".py";
                default -> ".java";
            })).exists();
        }
    }

    @Test
    void rejectsImplicitConversionsBetweenSeqAndListForEveryBackend() throws Exception {
        var source = writeSource("sample/DistinctCollections.cfun", """
                fun list_as_seq(values: List[int]): Seq[int] = values

                fun seq_as_list(values: Seq[int]): List[int] = values

                fun consume_list(values: List[int]): String = "done"

                fun wrong_argument(values: Seq[int]): String = consume_list(values)

                fun wrong_binding(values: Seq[int]): List[int] =
                    let copied: List[int] = values
                    copied
                """);

        for (var outputType : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateStderr(outputType)).isEqualTo("""
                    Compilation failed with 4 error(s):
                    /sample/DistinctCollections.cfun:1:0: Function `list_as_seq` returns `List[int]`, but declares `Seq[int]`; use an explicit `to_seq` or `as_list` conversion.
                    /sample/DistinctCollections.cfun:3:0: Function `seq_as_list` returns `Seq[int]`, but declares `List[int]`; use an explicit `to_seq` or `as_list` conversion.
                    /sample/DistinctCollections.cfun:7:47: Argument 1 of function `consume_list` has type `Seq[int]`, but `List[int]` is required; use an explicit `to_seq` or `as_list` conversion.
                    /sample/DistinctCollections.cfun:10:4: Binding `copied` has type `Seq[int]`, but declares `List[int]`; use an explicit `to_seq` or `as_list` conversion.
                    """);
        }

        assertThat(generatedPath(source)).doesNotExist();
        assertThat(generatedPath(source, ".js")).doesNotExist();
        assertThat(generatedPath(source, ".py")).doesNotExist();
    }

    @Test
    void supportsAppendingSingleElementToSeqForEveryBackend() throws Exception {
        var source = writeSource("sample/SeqAppend.cfun", """
                fun append(values: Seq[int], value: int): Seq[int] = values + value
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();
        assertThat(generatedPath(source)).content().contains("__capy_seq_append(");

        assertThat(compileGenerateStderr("javascript")).isEmpty();
        assertThat(generatedPath(source, ".js")).content().contains("__capy_add(");

        assertThat(compileGenerateStderr("python")).isEmpty();
        assertThat(generatedPath(source, ".py")).content().contains("__capy_add(");
    }

    @Test
    void compilesExplicitSeqToListConversionInsideAsyncEffect() throws Exception {
        var source = writeSource("sample/AsyncEffectBind.cfun", """
                import /capy/collection/Seq
                import /capy/lang/Async
                import /capy/lang/Effect
                from /capy/lang/Result import { Result }

                data Item { value: int }

                fun find_items(items: Seq[Item]): List[Item] = items.as_list()

                fun compute_items(result: Result[Seq[Item]]): Async[String] =
                    Async.compute(() => {
                        let mapped: Result[List[Item]] = result.map(items => find_items(items))
                        "done"
                    })

                fun collect(tasks: Seq[Async[String]]): Effect[String] =
                    Async.all(tasks).map(_ => "done")
                """);

        assertThat(compileGenerateStderr()).isEmpty();
        assertThat(generatedPath(source))
                .content()
                .contains("java.lang.Object items = __capy_result_success_value(")
                .contains("capy.lang.Async.all(")
                .contains("tasks).asList())");
        assertJavaCompiles(generatedPath(source));
    }

    @Test
    void generatesSupportedListMethodWithoutFreeFunctionSymbol() throws Exception {
        var source = writeSource("sample/ListSize.cfun", """
                fun count(values: List[int]): int =
                    values.size().value
                """);

        assertThat(compileGenerateStderr()).isEmpty();

        assertThat(generatedPath(source))
                .content()
                .contains("values.size()")
                .doesNotContain("Unsupported CFUN expression at");
    }

    @Test
    void generatesQualifiedAssertFailInsideResultRecoverForEveryBackend() throws Exception {
        writeSource("sample/RecoverValue.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun value(): Result[int] = Success { 1 }
                """);
        var testSource = writeTestSource("sample/RecoverValue.test.cfun", """
                import /capy/test/Assert
                from /sample/RecoverValue import { value }
                from /capy/test/CapyTest import { test_file, test }
                from /capy/test/Assert import { assert_that }

                fun tests() =
                    test_file("/sample/RecoverValue.cfun", [
                        test("recover result assertion", () =>
                            value()
                                .map(number => assert_that(number).is_equal_to(1))
                                .recover(error => Assert.fail("[{error.kind}] {error.message}"))
                        )
                    ])
                """);

        assertThat(compileGenerateWithTestsStderr("java")).isEmpty();
        assertThat(generatedTestPath(testSource))
                .content()
                .contains("__capy_result_recover_")
                .contains("new capy.test.Assert.TechnicalAssert")
                .doesNotContain(".recover(")
                .doesNotContain("capy.test.Assert.fail(");
        assertJavaCompiles(generatedPath(inputDir().resolve("sample/RecoverValue.cfun")), generatedTestPath(testSource));

        assertThat(compileGenerateWithTestsStderr("javascript")).isEmpty();
        assertThat(generatedTestPath(testSource, ".js"))
                .content()
                .contains("__capy_result_recover(")
                .contains("fail(")
                .doesNotContain(".recover(");
        assertThat(testOutputDir().resolve("capy/test/CapyTestRuntime.js"))
                .content()
                .contains("__capy_gather_module(\"../../sample/RecoverValue.test\")");

        assertThat(compileGenerateWithTestsStderr("python")).isEmpty();
        assertThat(generatedTestPath(testSource, ".py"))
                .content()
                .contains("__capy_result_recover(")
                .contains("fail(")
                .doesNotContain(".recover(");
        assertThat(testOutputDir().resolve("capy/test/CapyTestRuntime.py"))
                .content()
                .contains("_gather_module(\"sample.RecoverValue.test\")");
    }

    @Test
    void generatesResultAssertSucceedsAndDiscoversTheTestForEveryBackend() throws Exception {
        writeSource("sample/ResultValue.cfun", """
                from /capy/lang/Result import { Result, Success }

                fun value(): Result[String] = Success { "1234" }
                """);
        var testSource = writeTestSource("sample/ResultValue.test.cfun", """
                from /sample/ResultValue import { value }
                from /capy/test/CapyTest import { test_file, test }
                from /capy/test/Assert import { assert_that }

                fun tests(): Effect[TestFile] =
                    test_file("/sample/ResultValue.cfun", [
                        test("result succeeds", () =>
                            assert_that(value().map(item => item)).succeeds("1234")
                        )
                    ])
                """);

        assertThat(compileGenerateWithTestsStderr("java")).isEmpty();
        assertThat(generatedTestPath(testSource))
                .content()
                .contains("__capy_assert_result_succeeds(")
                .doesNotContain(").succeeds(");
        assertJavaCompiles(
                generatedPath(inputDir().resolve("sample/ResultValue.cfun")),
                generatedTestPath(testSource)
        );

        assertThat(compileGenerateWithTestsStderr("javascript")).isEmpty();
        assertThat(generatedTestPath(testSource, ".js"))
                .content()
                .contains(".succeeds(\"1234\")");
        assertThat(testOutputDir().resolve("capy/test/CapyTestRuntime.js"))
                .content()
                .contains("__capy_gather_module(\"../../sample/ResultValue.test\")");

        assertThat(compileGenerateWithTestsStderr("python")).isEmpty();
        assertThat(generatedTestPath(testSource, ".py"))
                .content()
                .contains(".succeeds(\"1234\")");
        assertThat(testOutputDir().resolve("capy/test/CapyTestRuntime.py"))
                .content()
                .contains("_gather_module(\"sample.ResultValue.test\")");
    }

    @Test
    void emitsMainModulesOnlyToTheMainOutputForEveryBackend() throws Exception {
        writeSource("sample/MainOnly.cfun", """
                fun value(): int = 42
                """);
        writeTestSource("sample/MainOnly.test.cfun", """
                from /sample/MainOnly import { value }
                from /capy/test/CapyTest import { test_file, test }
                from /capy/test/Assert import { assert_that }

                fun tests(): Effect[TestFile] =
                    test_file("/sample/MainOnly.cfun", [
                        test("main value", () => assert_that(value()).is_equal_to(42))
                    ])
                """);

        for (var backend : List.of("java", "javascript", "python")) {
            assertThat(compileGenerateWithTestsStderr(backend)).isEmpty();
            var extension = switch (backend) {
                case "java" -> ".java";
                case "javascript" -> ".js";
                case "python" -> ".py";
                default -> throw new IllegalStateException("Unexpected backend: " + backend);
            };
            assertThat(outputDir().resolve("sample/MainOnly" + extension)).exists();
            assertThat(testOutputDir().resolve("sample/MainOnly" + extension)).doesNotExist();
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"java", "javascript", "python"})
    void reusesGeneratedOutputWhenMainAndTestInputsMatch(String backend) throws Exception {
        var source = writeSource("sample/SharedInput.cfun", "fun value(): int = 42");

        assertThat(compileGenerateWithSharedInputStderr(backend)).isEmpty();

        var extension = switch (backend) {
            case "java" -> ".java";
            case "javascript" -> ".js";
            case "python" -> ".py";
            default -> throw new IllegalStateException("Unexpected backend: " + backend);
        };
        var mainOutput = generatedPath(source, extension);
        var testOutput = testOutputDir().resolve("sample/SharedInput" + extension);
        assertThat(mainOutput).exists();
        assertThat(testOutput).hasSameTextualContentAs(mainOutput);
    }

    @ParameterizedTest
    @ValueSource(strings = {"javascript", "python"})
    void isolatesGeneratorLookupContextBetweenCompileGenerateInvocations(String backend) throws Exception {
        var shared = writeSource("sample/Shared.cfun", "fun value(): int = 1");
        assertThat(compileGenerateStderr(backend)).isEmpty();

        Files.writeString(shared, "fun value(): int = 2");
        var independentTest = writeSource("sample/Independent.test.cfun", "fun marker(): int = 3");
        assertThat(compileGenerateStderr(backend)).isEmpty();

        var extension = backend.equals("javascript") ? ".js" : ".py";
        assertThat(generatedPath(shared, extension)).content().contains("return 2");
        assertThat(generatedPath(independentTest, extension)).exists();
    }

    @Test
    void discoversJavaScriptTestsUsingInferredExtensionReceiversAndFailureKinds() throws Exception {
        writeSource("sample/OrderedValue.cfun", """
                data OrderedValue { value: int }

                data Value { value: int } with constructor {
                    if value < 0
                    then Error { kind: "value.invalid", message: "value must be non-negative" }
                    else Success { * { value } }
                }

                private fun Value.order(): OrderedValue = OrderedValue { this.value }
                private fun OrderedValue.to_int(): int = this.value
                private fun OrderedValue.to_inverted_int(): int = 0 - this.value

                fun Value.diff(): int =
                    let ordered = this.order()
                    ordered.to_int() - ordered.to_inverted_int()
                """);
        var testSource = writeTestSource("sample/OrderedValue.test.cfun", """
                from /sample/OrderedValue import { Value }
                from /capy/test/CapyTest import { test_file, test }
                from /capy/test/Assert import { assert_that }

                fun tests(): Effect[TestFile] =
                    test_file("/sample/OrderedValue.cfun", [
                        test("inferred extension receiver", () => assert_that(Value! { 2 }.diff()).is_equal_to(4)),
                        test("failure kind", () => invalid_value())
                    ])

                private fun invalid_value(): Assert =
                    assert_that(Value { -1 }).fails_with_kind("value.invalid")
                """);

        assertThat(compileGenerateWithTestsStderr("javascript")).isEmpty();
        assertThat(generatedTestPath(testSource, ".js")).exists();
        assertThat(testOutputDir().resolve("capy/test/CapyTestRuntime.js"))
                .content()
                .contains("__capy_gather_module(\"../../sample/OrderedValue.test\")");
    }

    @Test
    void specializesImportedGenericFunctionResultsForChainedMethodsInEveryBackend() throws Exception {
        var boxSource = writeSource("sample/GenericBox.cfun", """
                data Box[T] { value: T }

                fun box(value: T): Box[T] = Box { value }

                fun Box[T].unwrap(): T = this.value
                """);
        var consumerSource = writeSource("sample/UseGenericBox.cfun", """
                from /sample/GenericBox import { Box, box }

                fun text(): String = box("generic").unwrap()
                """);

        assertThat(compileGenerateStderr("java")).isEmpty();
        assertThat(generatedPath(consumerSource))
                .content()
                .contains("GenericBox.Box_T__unwrap__")
                .doesNotContain(".unwrap(");
        assertJavaCompiles(generatedPath(boxSource), generatedPath(consumerSource));

        assertThat(compileGenerateStderr("javascript")).isEmpty();
        assertThat(generatedPath(consumerSource, ".js"))
                .content()
                .contains("Box_T__unwrap__")
                .doesNotContain(".unwrap(");

        assertThat(compileGenerateStderr("python")).isEmpty();
        assertThat(generatedPath(consumerSource, ".py"))
                .content()
                .contains("Box_T__unwrap__")
                .doesNotContain(".unwrap(");
    }

    @Test
    void generatesImportedBinaryExtensionOperatorInTestModule() throws Exception {
        var mainSource = writeSource("sample/ComparableValue.cfun", """
                data Value { number: int }

                fun Value.`==`(other: Value): bool = this.number == other.number
                """);
        var testSource = writeTestSource("sample/ComparableValue.test.cfun", """
                from /sample/ComparableValue import { Value }
                from /capy/test/CapyTest import { test_file, test }
                from /capy/test/Assert import { assert_that }

                fun tests(): Effect[TestFile] =
                    test_file("/sample/ComparableValue.cfun", [
                        test("uses imported equality", () => assert_that(Value { 1 } == Value { 1 }).is_true())
                    ])
                """);

        assertThat(compileGenerateWithTestsStderr()).isEmpty();
        assertThat(generatedTestPath(testSource))
                .content()
                .doesNotContain("unsupported(\"match\")");
        assertJavaCompiles(generatedPath(mainSource), generatedTestPath(testSource));
    }

    @Test
    void generatesPrimitiveBackedArithmeticInTestModuleWithoutRecursiveImportScanning() throws Exception {
        var mainSource = writeSource("sample/Digits.cfun", """
                from /capy/lang/Primitives import { digit, ONE_DIGIT, TWO_DIGIT, THREE_DIGIT, FOUR_DIGIT }

                data Digits { first: digit, second: digit, third: digit, fourth: digit }

                const EXPECTED: Digits = Digits! {
                    ONE_DIGIT,
                    TWO_DIGIT,
                    THREE_DIGIT,
                    FOUR_DIGIT
                }

                fun total(value: Digits): int =
                    value.first * 1000 + value.second * 100 + value.third * 10 + value.fourth

                fun Digits.same(other: Digits): bool =
                    this.first == other.first & this.second == other.second &
                    this.third == other.third & this.fourth == other.fourth
                """);
        var testSource = writeTestSource("sample/Digits.test.cfun", """
                from /sample/Digits import { Digits, total }
                from /capy/lang/Primitives import { ONE_DIGIT, TWO_DIGIT, THREE_DIGIT, FOUR_DIGIT }
                from /capy/test/CapyTest import { test_file, test }
                from /capy/test/Assert import { assert_that }

                fun tests(): Effect[TestFile] =
                    test_file("/sample/Digits.cfun", [
                        test("primitive-backed arithmetic", () =>
                            assert_that(total(Digits! { ONE_DIGIT, TWO_DIGIT, THREE_DIGIT, FOUR_DIGIT }))
                                .is_equal_to(1234)
                        )
                    ])
                """);

        assertThat(compileGenerateWithTestsStderr()).isEmpty();
        assertThat(generatedPath(mainSource))
                .content()
                .contains("public static final Digits EXPECTED = new Digits(1, 2, 3, 4);")
                .doesNotContain("__capy_data(\"Digits\"");
        assertThat(generatedTestPath(testSource))
                .content()
                .doesNotContain("unsupported(\"match\")");
        assertJavaCompiles(generatedPath(mainSource), generatedTestPath(testSource));
    }

    @Test
    void validatesTestProgramBeforeJavaGeneration() throws Exception {
        writeSource("sample/Main.cfun", """
                fun count(values: List[int]): int = values.size().value
                """);
        var testSource = writeTestSource("sample/MainTest.cfun", """
                fun broken(values: List[int]): bool =
                    values.length() == 4
                """);

        assertThat(compileGenerateWithTestsStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/MainTest.cfun:2:4: Method `length` on `List` is not supported by the Java backend.
                """);

        assertThat(generatedTestPath(testSource)).doesNotExist();
    }

    @Test
    void rejectsUnknownExplicitStandardLibraryImportsBeforeGeneration() throws Exception {
        var source = writeSource("sample/UnknownPrimitive.cfun", """
                from /capy/lang/Primitives import { ONE }

                const VALUE: int = ONE
                """);

        assertThat(compileGenerateStderr()).isEqualTo("""
                Compilation failed with 1 error(s):
                /sample/UnknownPrimitive.cfun:1:0: Module `/capy/lang/Primitives` does not export `ONE`.
                """);

        assertThat(generatedPath(source)).doesNotExist();
    }

    @Test
    void allowsJavaUnsupportedMethodForJavaScriptTarget() throws Exception {
        var source = writeSource("sample/EffectFlatMapJavaScript.cfun", """
                import /capy/lang/Effect

                fun supported(effect: Effect[String]): Effect[String] =
                    effect.flat_map(value => Effect.pure(value))
                """);

        assertThat(compileGenerateStderr("javascript")).isEmpty();

        assertThat(generatedPath(source, ".js")).exists();
    }

    @Test
    void reportsCollectionFlatMapReturningResult() throws Exception {
        var source = writeSource("sample/CollectionFlatMapFailure.cfun", """
                from /capy/lang/Result import { Result }

                fun broken(values: List[Result[int]]): Seq[Result[int]] =
                    values |* result => result.map(value => value + 1)
                """);

        assertThat(compileGenerateStderr())
                .contains("Compilation failed with 1 error(s):")
                .contains("Collection operator `|*` requires its mapper to return a collection; "
                        + "`Result.map` returns `Result`.");

        assertThat(generatedPath(source)).doesNotExist();
    }

    private void compileGenerate() {
        compileGenerate("java");
    }

    private void compileGenerate(String outputType) {
        BackendCompilationContext.withOutputType(outputType, () -> Capy.runCompileGenerate(
                new Capy.CompileGenerateOptions(
                outputType,
                inputDir().toString(),
                outputDir().toString(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                false,
                false,
                Capy.LogLevel.WARN
        )).unsafeRun());
    }

    private String compileGenerateStderr() {
        return compileGenerateStderr("java");
    }

    private String compileGenerateStderr(String outputType) {
        var originalError = System.err;
        var buffer = new ByteArrayOutputStream();
        try (var errorStream = new PrintStream(buffer, true, StandardCharsets.UTF_8)) {
            System.setErr(errorStream);
            compileGenerate(outputType);
        } finally {
            System.setErr(originalError);
        }
        return normalizedDiagnostics(buffer);
    }

    private String compileGenerateWithTestsStderr() {
        return compileGenerateWithTestsStderr("java");
    }

    private String compileGenerateWithTestsStderr(String outputType) {
        var originalError = System.err;
        var buffer = new ByteArrayOutputStream();
        try (var errorStream = new PrintStream(buffer, true, StandardCharsets.UTF_8)) {
            System.setErr(errorStream);
            BackendCompilationContext.withOutputType(outputType, () -> Capy.runCompileGenerate(
                    new Capy.CompileGenerateOptions(
                            outputType,
                            inputDir().toString(),
                            outputDir().toString(),
                            Optional.empty(),
                            Optional.of(testInputDir().toString()),
                            Optional.of(testOutputDir().toString()),
                            Optional.empty(),
                            Optional.empty(),
                            false,
                            false,
                            Capy.LogLevel.WARN
                    )).unsafeRun());
        } finally {
            System.setErr(originalError);
        }
        return normalizedDiagnostics(buffer);
    }

    private String compileGenerateWithSharedInputStderr(String outputType) {
        var originalError = System.err;
        var buffer = new ByteArrayOutputStream();
        try (var errorStream = new PrintStream(buffer, true, StandardCharsets.UTF_8)) {
            System.setErr(errorStream);
            CapyMain.main(
                    "compile-generate",
                    outputType,
                    "--input", inputDir().toString(),
                    "--output", outputDir().toString(),
                    "--test-input", inputDir().toString(),
                    "--test-output", testOutputDir().toString(),
                    "--log", "WARN"
            );
        } finally {
            System.setErr(originalError);
        }
        return normalizedDiagnostics(buffer);
    }

    private String normalizedDiagnostics(ByteArrayOutputStream buffer) {
        var diagnostics = buffer.toString(StandardCharsets.UTF_8).replace("\r\n", "\n");
        return diagnostics.isBlank() ? "" : diagnostics.stripTrailing() + "\n";
    }

    private Path writeSource(String relativePath, String source) throws IOException {
        var path = inputDir().resolve(relativePath);
        Files.createDirectories(path.getParent());
        Files.writeString(path, source);
        return path;
    }

    private Path writeTestSource(String relativePath, String source) throws IOException {
        var path = testInputDir().resolve(relativePath);
        Files.createDirectories(path.getParent());
        Files.writeString(path, source);
        return path;
    }

    private Path generatedPath(Path source) {
        return generatedPath(source, ".java");
    }

    private Path generatedPath(Path source, String extension) {
        var relative = inputDir().relativize(source);
        var fileName = relative.getFileName().toString().replaceFirst("\\.cfun$", extension);
        return outputDir().resolve(relative).resolveSibling(fileName);
    }

    private Path generatedTestPath(Path source) {
        return generatedTestPath(source, ".java");
    }

    private Path generatedTestPath(Path source, String extension) {
        var relative = testInputDir().relativize(source);
        var fileName = relative.getFileName().toString().replaceFirst("\\.cfun$", extension);
        if (extension.equals(".java")) {
            fileName = fileName.replace('.', '_').replaceFirst("_java$", ".java");
        }
        return testOutputDir().resolve(relative).resolveSibling(fileName);
    }

    private Path inputDir() {
        return tempDir.resolve("input");
    }

    private Path outputDir() {
        return tempDir.resolve("output");
    }

    private Path testInputDir() {
        return tempDir.resolve("test-input");
    }

    private Path testOutputDir() {
        return tempDir.resolve("test-output");
    }

    private void assertJavaCompiles(Path... sources) throws IOException {
        compileJava(sources);
    }

    private Path compileJava(Path... sources) throws IOException {
        var compiler = ToolProvider.getSystemJavaCompiler();
        assertThat(compiler).as("system Java compiler").isNotNull();
        var classes = Files.createDirectories(tempDir.resolve("compiled-java"));
        var diagnostics = new ByteArrayOutputStream();
        var arguments = new ArrayList<String>();
        arguments.add("-classpath");
        arguments.add(System.getProperty("java.class.path"));
        arguments.add("-d");
        arguments.add(classes.toString());
        for (var source : sources) {
            arguments.add(source.toString());
        }
        var exitCode = compiler.run(
                null,
                diagnostics,
                diagnostics,
                arguments.toArray(String[]::new)
        );
        assertThat(exitCode)
                .as(diagnostics.toString(StandardCharsets.UTF_8))
                .isZero();
        return classes;
    }

    private java.lang.reflect.Method generatedMethod(Class<?> owner, String prefix) {
        return java.util.Arrays.stream(owner.getDeclaredMethods())
                .filter(method -> method.getName().startsWith(prefix))
                .findFirst()
                .orElseThrow();
    }
}
