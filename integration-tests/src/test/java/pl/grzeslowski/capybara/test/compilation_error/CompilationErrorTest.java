package pl.grzeslowski.capybara.test.compilation_error;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import pl.grzeslowski.capybara.compiler.ImportDeclaration;
import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.CapybaraLinker;
import pl.grzeslowski.capybara.linker.LinkedProgram;
import pl.grzeslowski.capybara.linker.ValueOrError;
import pl.grzeslowski.capybara.parser.CapybaraParser;

import java.util.*;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;

public class CompilationErrorTest {
    @ParameterizedTest(name = "{index}: should fail when compiling `{0}.cfun`")
    @MethodSource
    void compilationError(
            String moduleName,
            String code,
            Position expectedPosition,
            String errorMessage) {
        // given
        var finalErrorMessage = errorMessage.formatted(expectedPosition.line(), expectedPosition.column());

        // when
        var errors = compileProgram(code, moduleName);

        // then
        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll("Assertions on error",
                () -> assertThat(error.line()).isEqualTo(expectedPosition.line()),
                () -> assertThat(error.column()).isEqualTo(expectedPosition.column()),
                () -> assertThat(error.file()).isEqualTo("/foo/boo/%s.cfun".formatted(moduleName)),
                () -> assertThat(error.message()).isEqualTo(finalErrorMessage));
    }

    @ParameterizedTest(name = "{index}: should fail when compiling `{0}.cfun` (imports flavour)")
    @MethodSource
    void compilationErrorWithImports(
            String moduleName,
            String code,
            Position expectedPosition,
            String errorMessage,
            List<ImportDeclaration> imports) {
        // given
        var finalErrorMessage = errorMessage.formatted(expectedPosition.line(), expectedPosition.column());

        // when
        var errors = compileProgram(code, moduleName, imports);

        // then
        assertThat(errors).hasSize(1);
        var error = errors.first();
        assertAll("Assertions on error",
                () -> assertThat(error.line()).isEqualTo(expectedPosition.line()),
                () -> assertThat(error.column()).isEqualTo(expectedPosition.column()),
                () -> assertThat(error.file()).isEqualTo("/foo/boo/%s.cfun".formatted(moduleName)),
                () -> assertThat(error.message()).isEqualTo(finalErrorMessage));
    }

    @SuppressWarnings("unchecked")
    static Stream<Arguments> compilationError() {
        return concat(simpleCompilationError(), multilineCompilationError(), infixOperations(), bitwiseOperations(), matchCompilationErrors());
    }

    static Stream<Arguments> compilationErrorWithImports() {
        return Stream.of(
                Arguments.of(
                        "infix_operator_outside_of_package",
                        """
                                fun Name[T].foo(map: T => Name[Y]): Name[Y] =
                                   match this with
                                   | Boo e => e
                                   | Foo s => map(s.value)
                                """,
                        new Position(3, 14),
                        """
                                error: mismatched types
                                 --> /foo/boo/infix_operator_outside_of_package.cfun:%d:%d
                                fun Name[T].foo(map: T => Name[Y]): Name[Y] =
                                            ^ Cannot declare method on external type `Name`. Type methods/infix operators must be declared in the module where the type is defined.
                                """,
                        List.of(new ImportDeclaration("/capy/compilation_test/Name", List.of("Name", "Foo", "Boo"), List.of()))
                ),
                Arguments.of(
                        "infix_special_operator_outside_of_package",
                        """
                                fun Name[T].`+`(map: T => Name[Y]): Name[Y] =
                                   match this with
                                   | Boo e => e
                                   | Foo s => map(s.value)
                                """,
                        new Position(3, 13),
                        """
                                error: mismatched types
                                 --> /foo/boo/infix_special_operator_outside_of_package.cfun:%d:%d
                                fun Name[T].`+`(map: T => Name[Y]): Name[Y] =
                                             ^ Cannot declare method on external type `Name`. Type methods/infix operators must be declared in the module where the type is defined.
                                """,
                        List.of(new ImportDeclaration("/capy/compilation_test/Name", List.of("Name", "Foo", "Boo"), List.of()))
                )
        );
    }

    static Stream<Arguments> matchCompilationErrors() {
        var base = Stream.of(
                Arguments.of(
                        "match_not_exhaustive",
                        """
                                type Letter = A | B | C | D
                                data A { a: int }
                                data B { b: int }
                                data C { c: int }
                                data D { d: int }
                                fun foo(letter: Letter): int =
                                    match letter with
                                    | A { a } => a
                                    | B { b } => b
                                """,
                        new Position(7, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_not_exhaustive.cfun:7:4
                                fun foo(letter: Letter): int =
                                    match letter with
                                    ^ `match` is not exhaustive. Use wildcard `| _ => ...` or add missing branches:`C`, `D`.
                                """
                ));
        var primitiveTypes = List.of("string", "byte", "int", "long", "float", "double", "bool");
        var primitiveCases = primitiveTypes.stream().map(CompilationErrorTest::primitiveMatchNotExhaustiveCase);
        return Stream.concat(base, primitiveCases);
    }

    private static Arguments primitiveMatchNotExhaustiveCase(String primitive) {
        var moduleName = "match_%s".formatted(primitive);
        var code = """
                fun foo(x: %s): int =
                    match x with
                    | %s a => 1
                    | %s b => 2
                """.formatted(primitive, primitive, primitive);
        var message = """
                error: mismatched types
                 --> /foo/boo/%s.cfun:2:4
                fun foo(x: %s): int =
                    match x with
                    ^ `match` is not exhaustive. Use wildcard `| _ => ...`.
                """.formatted(moduleName, primitive);
        return Arguments.of(moduleName, code, new Position(2, 4), message);
    }

    static Stream<Arguments> infixOperations() {
        var operators = List.of("+", "-", "*", "^", "/");
        var numericTypes = List.of(
                new InfixOperand("byte", "byte", "byte"),
                new InfixOperand("int", "int", "int"),
                new InfixOperand("long", "long", "long"),
                new InfixOperand("float", "float", "float"),
                new InfixOperand("double", "double", "double")
        );
        var primitiveTypes = List.of(
                new InfixOperand("bool", "bool", "bool"),
                new InfixOperand("byte", "byte", "byte"),
                new InfixOperand("int", "int", "int"),
                new InfixOperand("long", "long", "long"),
                new InfixOperand("float", "float", "float"),
                new InfixOperand("double", "double", "double")
        );
        var anyType = new InfixOperand("any", "any", "any");
        var byModule = new LinkedHashMap<String, Arguments>();

        for (var op : operators) {
            for (var right : primitiveTypes) {
                add(byModule, op, new InfixOperand("bool", "bool", "bool"), right);
                add(byModule, op, right, new InfixOperand("bool", "bool", "bool"));
            }

            if (!op.equals("+")) {
                for (var left : numericTypes) {
                    add(byModule, op, left, new InfixOperand("string", "string", "string"));
                }
                add(byModule, op, new InfixOperand("string", "string", "string"), new InfixOperand("string", "string", "string"));
            }
            add(byModule, op, anyType, anyType);
        }

        return byModule.values().stream();
    }

    static Stream<Arguments> bitwiseOperations() {
        var nonIntPrimitives = List.of(
                new InfixOperand("byte", "byte", "byte"),
                new InfixOperand("long", "long", "long"),
                new InfixOperand("float", "float", "float"),
                new InfixOperand("double", "double", "double"),
                new InfixOperand("bool", "bool", "bool"),
                new InfixOperand("string", "string", "string")
        );
        var byModule = new LinkedHashMap<String, Arguments>();
        var intType = new InfixOperand("int", "int", "int");

        for (var op : List.of(".and.", ".nand.", ".or.", ".xor.")) {
            for (var t : nonIntPrimitives) {
                addBitwiseInfix(byModule, op, intType, t);
                addBitwiseInfix(byModule, op, t, intType);
            }
        }

        for (var t : nonIntPrimitives) {
            addBitwiseNot(byModule, t);
        }

        return byModule.values().stream();
    }

    private static void addBitwiseInfix(Map<String, Arguments> byModule, String op, InfixOperand left, InfixOperand right) {
        var module = "bitwise_%s_%s_%s".formatted(bitwiseOpName(op), left.id(), right.id());
        byModule.putIfAbsent(module, bitwiseInfixCase(module, op, left, right));
    }

    private static void addBitwiseNot(Map<String, Arguments> byModule, InfixOperand operand) {
        var module = "bitwise_not_%s".formatted(operand.id());
        byModule.putIfAbsent(module, bitwiseNotCase(module, operand));
    }

    private static Arguments bitwiseInfixCase(String moduleName, String op, InfixOperand left, InfixOperand right) {
        var code = "fun foo(left: %s, right: %s) = left %s right".formatted(left.decl(), right.decl(), op);
        var column = code.indexOf(op);
        var pointer = " ".repeat(column)
                      + "^ `%s` operator is not defined for `%s %s %s`".formatted(op, left.shownType(), op, right.shownType());
        var errorMessage = "error: mismatched types\n"
                           + " --> /foo/boo/%s.cfun:1:%d\n".formatted(moduleName, column)
                           + code + "\n"
                           + pointer + "\n";
        return Arguments.of(moduleName, code, new Position(1, column), errorMessage);
    }

    private static Arguments bitwiseNotCase(String moduleName, InfixOperand operand) {
        var code = "fun foo(value: %s) = .not.value".formatted(operand.decl());
        var renderedCode = "fun foo(value: %s) = value .not. 0".formatted(operand.decl());
        var column = code.indexOf(".not.");
        var pointer = " ".repeat(column)
                      + "^ `.not.` operator is not defined for `%s .not. int`".formatted(operand.shownType());
        var errorMessage = "error: mismatched types\n"
                           + " --> /foo/boo/%s.cfun:1:%d\n".formatted(moduleName, column)
                           + renderedCode + "\n"
                           + pointer + "\n";
        return Arguments.of(moduleName, code, new Position(1, column), errorMessage);
    }

    private static void add(Map<String, Arguments> byModule, String op, InfixOperand left, InfixOperand right) {
        var module = "infix_%s_%s_%s".formatted(opName(op), left.id(), right.id());
        byModule.putIfAbsent(module, infixCase(module, op, left, right));
    }

    private static Arguments infixCase(String moduleName, String op, InfixOperand left, InfixOperand right) {
        var code = "fun foo(left: %s, right: %s) = left %s right".formatted(left.decl(), right.decl(), op);
        var column = code.indexOf(op);
        var pointer = " ".repeat(column)
                      + "^ `%s` operator is not defined for `%s %s %s`".formatted(op, left.shownType(), op, right.shownType());
        var errorMessage = "error: mismatched types\n"
                           + " --> /foo/boo/%s.cfun:1:%d\n".formatted(moduleName, column)
                           + code + "\n"
                           + pointer + "\n";
        return Arguments.of(moduleName, code, new Position(1, column), errorMessage);
    }

    private static String opName(String op) {
        return switch (op) {
            case "+" -> "plus";
            case "-" -> "minus";
            case "*" -> "mul";
            case "^" -> "pow";
            case "/" -> "div";
            default -> op;
        };
    }

    private static String bitwiseOpName(String op) {
        return switch (op) {
            case ".and." -> "and";
            case ".nand." -> "nand";
            case ".or." -> "or";
            case ".xor." -> "xor";
            default -> op;
        };
    }

    static Stream<Arguments> simpleCompilationError() {
        return Stream.of(
                Arguments.of(
                        "function_unknown_return_type",
                        """ 
                                fun broken(x: int): Str =
                                    if x > 0
                                    then "+"
                                    else "-"
                                """,
                        new Position(1, 20),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_unknown_return_type.cfun:1:20
                                fun broken(x: int): Str =
                                                    ^ Data type `Str` not found
                                """
                ),
                Arguments.of(
                        "match_case_wrong_operator",
                        """ 
                                fun broken(result: /capy/lang/Result.Result[any]): bool =
                                    match result with
                                    | Error _ = true
                                    | Success s => false
                                """,
                        new Position(3, 14),
                        """
                                error: mismatched types
                                 --> /foo/boo/match_case_wrong_operator.cfun:3:14
                                fun broken(result: /capy/lang/Result.Result[any]): bool =
                                    match result with
                                    | Error _ = true
                                              ^ Expected `=>`, found `=`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type",
                        "fun foo(x: int): int = \"boo\"",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type.cfun:1:23
                                fun foo(x: int): int = "boo"
                                                       ^ expected `int`, found `string`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_bool",
                        "fun foo(x: int): int = true",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_bool.cfun:1:23
                                fun foo(x: int): int = true
                                                       ^ expected `int`, found `bool`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_long",
                        "fun foo(x: int): int = 1L",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_long.cfun:1:23
                                fun foo(x: int): int = 1L
                                                       ^ expected `int`, found `long`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_float",
                        "fun foo(x: int): int = 1.5f",
                        new Position(1, 23),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_float.cfun:1:23
                                fun foo(x: int): int = 1.5f
                                                       ^ expected `int`, found `float`
                                """
                ),
                Arguments.of(
                        "function_wrong_return_type_string",
                        "fun foo(x: int): string = 1",
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_wrong_return_type_string.cfun:1:26
                                fun foo(x: int): string = 1
                                                          ^ expected `string`, found `int`
                                """
                ),
                Arguments.of(
                        "function_index_literal_of_of_range",
                        "fun foo(x: int) = x + 97387717187",
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_index_literal_of_of_range.cfun:1:22
                                fun foo(x: int) = x + 97387717187
                                                      ^ Int literal out of range: `97387717187`
                                """
                ),
                Arguments.of(
                        "function_private_type_escapes_signature",
                        """
                                fun foo_me(name: string): __Name =
                                    type __Name = __Foo | __Boo | __Unknown
                                    data __Foo { foo: string }
                                    data __Boo { boo: string }
                                    data __Unknown { unkn: string }
                                    match name with
                                    | "foo" => __Foo { foo: "xyz" }
                                    | "boo" => __Boo { boo: "xyz" }
                                    | _ => __Unknown { unkn: name }
                                """,
                        new Position(1, 26),
                        """
                                error: mismatched types
                                 --> /foo/boo/function_private_type_escapes_signature.cfun:1:26
                                fun foo_me(name: string): __Name = ...
                                                          ^ Private type `__Name` cannot be used in function signature
                                """
                )
        );
    }

    static Stream<Arguments> multilineCompilationError() {
        return Stream.of(
                Arguments.of(
                        "multiline_function_wrong_return_type",
                        """
                                // normal comment
                                data Foo {
                                    option: string,
                                    x: int
                                }
                                
                                /// some documentation
                                /// line 2 of doc
                                fun foo(x: int): int =
                                    let x = Foo {
                                        "boo",
                                        5
                                    }
                                    "boo"
                                """,
                        new Position(14, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_function_wrong_return_type.cfun:14:4
                                  fun foo(x: int): int =
                                    let x = Foo {
                                        "boo",
                                        5
                                    }
                                    "boo"
                                    ^ expected `int`, found `string`
                                """
                ),
                Arguments.of(
                        "multiline_let_chain_wrong_return_type",
                        """
                                /// docs
                                fun foo(x: int): int =
                                    let a = x + 1
                                    let b = a + 2
                                    "x"
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_let_chain_wrong_return_type.cfun:5:4
                                  fun foo(x: int): int =
                                    let a = x+1
                                    let b = a+2
                                    "x"
                                    ^ expected `int`, found `string`
                                """
                ),
                Arguments.of(
                        "multiline_data_return_wrong_type",
                        """
                                data Bar { value: int }
                                
                                fun foo(x: int): Bar =
                                    let y = x + 1
                                    y
                                """,
                        new Position(5, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_data_return_wrong_type.cfun:5:4
                                  fun foo(x: int): Bar =
                                    let y = x+1
                                    y
                                    ^ expected `Bar`, found `int`
                                """
                ),
                Arguments.of(
                        "multiline_new_data_return_wrong_type",
                        """
                                /* block comment */
                                data Foo { a: int, b: string }
                                
                                fun foo(): string =
                                    let f = Foo {
                                        a: 1,
                                        b: "x"
                                    }
                                    f
                                """,
                        new Position(9, 4),
                        """
                                error: mismatched types
                                 --> /foo/boo/multiline_new_data_return_wrong_type.cfun:9:4
                                  fun foo(): string =
                                    let f = Foo {
                                        a: 1,
                                        b: "x"
                                    }
                                    f
                                    ^ expected `string`, found `Foo`
                                """
                )
        );
    }


    private static SortedSet<ValueOrError.Error.SingleError> compileProgram(String fun, String moduleName) {
        return compileProgram(fun, moduleName, List.of());
    }

    private static final List<Module> DEFAULT_MODULES = List.of(
            new Module("Name", "/capy/compilation_test",
                    CapybaraParser.INSTANCE.parseFunctional("""
                            type Name[T] = Foo[T] | Boo
                            data Foo[T] { value: T }
                            data Boo { message: string }
                            """),
                    List.of())
    );

    private static SortedSet<ValueOrError.Error.SingleError> compileProgram(String fun, String moduleName, List<ImportDeclaration> imports) {
        try {
            var functional = CapybaraParser.INSTANCE.parseFunctional(fun);
            var module = new Module(moduleName, "/foo/boo", functional, imports);
            var modules = new ArrayList<>(DEFAULT_MODULES);
            modules.add(module);
            var program = new Program(modules);
            var programValueOrError = CapybaraLinker.INSTANCE.link(program);
            if (programValueOrError instanceof ValueOrError.Value<LinkedProgram> value) {
                throw new AssertionError("Expected compilation error but got LinkedProgram: " + value);
            }
            return ((ValueOrError.Error<?>) programValueOrError).errors();
        } catch (IllegalStateException e) {
            var parserError = java.util.regex.Pattern.compile("line (\\d+):(\\d+): (.+)").matcher(e.getMessage());
            if (parserError.matches()) {
                var line = Integer.parseInt(parserError.group(1));
                var column = Integer.parseInt(parserError.group(2));
                var details = parserError.group(3);
                var lines = fun.split("\\R", -1);
                var functionLine = 0;
                for (var i = 0; i < lines.length; i++) {
                    if (lines[i].stripLeading().startsWith("fun ")) {
                        functionLine = i;
                        break;
                    }
                }
                var header = lines.length > functionLine ? lines[functionLine] : "";
                var body = lines.length > functionLine + 1 ? lines[functionLine + 1] : "";
                var failingLine = line > 0 && line <= lines.length ? lines[line - 1] : "";
                var message = "error: mismatched types\n"
                              + " --> /foo/boo/%s.cfun:%d:%d\n".formatted(moduleName, line, column)
                              + header + "\n"
                              + body + "\n"
                              + failingLine + "\n"
                              + " ".repeat(Math.max(column, 0)) + "^ " + details + "\n";
                return new TreeSet<>(Set.of(new ValueOrError.Error.SingleError(
                        line,
                        column,
                        "/foo/boo/%s.cfun".formatted(moduleName),
                        message
                )));
            }
            return new TreeSet<>(Set.of(new ValueOrError.Error.SingleError(
                    0,
                    0,
                    "/foo/boo/%s.cfun".formatted(moduleName),
                    e.getMessage()
            )));
        }
    }

    record Position(int line, int column) {
    }

    record InfixOperand(String id, String decl, String shownType) {
    }

    @SuppressWarnings("unchecked")
    private static Stream<Arguments> concat(Stream<Arguments>... streams) {
        var out = Stream.<Arguments>empty();
        for (Stream<Arguments> stream : streams) {
            out = Stream.concat(out, stream);
        }
        return out;
    }
}
