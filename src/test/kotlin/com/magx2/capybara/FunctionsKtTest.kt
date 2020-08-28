package com.magx2.capybara

import com.magx2.capybara.BasicTypes.anyType
import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.listType
import com.magx2.capybara.BasicTypes.nothingType
import com.magx2.capybara.BasicTypes.stringType
import org.assertj.core.api.Assertions.assertThat
import org.assertj.core.api.Assertions.assertThatThrownBy
import org.assertj.core.api.ThrowableAssert
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Timeout
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.*
import java.util.stream.Stream

@Suppress("MemberVisibilityCanBePrivate")
internal class FunctionsKtTest {

    companion object {
        private fun integerExpression() = IntegerExpression(Line(1, 2), Random().nextLong())
        private fun booleanExpression() = BooleanExpression(Line(1, 2), Random().nextBoolean())
        private fun stringExpression() = StringExpression(Line(1, 2), (Random().nextDouble() * Random().nextInt()).toString())

        @JvmStatic
        @Suppress("unused") // it is used in parametrized test
        fun expressionReturnType(): Stream<Arguments> =
                Stream.of(
                        // parenthesis expression
                        Arguments.of(ParenthesisExpression(Line(1, 2), integerExpression()), intType),
                        Arguments.of(ParenthesisExpression(Line(1, 2), booleanExpression()), booleanType),
                        Arguments.of(ParenthesisExpression(Line(1, 2), stringExpression()), stringType),
                        // basic expressions
                        Arguments.of(integerExpression(), intType),
                        Arguments.of(booleanExpression(), booleanType),
                        Arguments.of(stringExpression(), stringType),
                        // infix expressions - boolean
                        Arguments.of(InfixExpression(Line(1, 2), "!=", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(Line(1, 2), "==", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(Line(1, 2), "&&", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(Line(1, 2), "||", booleanExpression(), booleanExpression()), booleanType),
                        // infix expressions - int
                        Arguments.of(InfixExpression(Line(1, 2), "^", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(Line(1, 2), "*", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(Line(1, 2), "+", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(Line(1, 2), "-", integerExpression(), integerExpression()), intType),
                        // infix expressions - int
                        Arguments.of(InfixExpression(Line(1, 2), "+", stringExpression(), stringExpression()), stringType),
                        // if expression
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), integerExpression(), integerExpression()), intType),
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), stringExpression(), stringExpression()), stringType),
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), booleanExpression(), booleanExpression()), booleanType),
                        // if expression with smart casting to string
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), stringExpression(), integerExpression()), stringType),
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), integerExpression(), stringExpression()), stringType),
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), stringExpression(), booleanExpression()), stringType),
                        Arguments.of(IfExpression(Line(1, 2), booleanExpression(), booleanExpression(), stringExpression()), stringType),
                        // negate expression
                        Arguments.of(NegateExpression(Line(1, 2), booleanExpression()), booleanType),
                )

        @Suppress("unused")
        @JvmStatic
        fun stringInfixOperator(): Stream<String> = Stream.of("^", "*", "-", "&&", "||")

        @Suppress("unused")
        @JvmStatic
        fun booleanInfixOperator(): Stream<String> = Stream.of("+", "^", "*", "-", ">", "<", ">=", "<=")
    }

    val compilationContext = CompilationContext(setOf(), setOf())
    val compileUnit = CompileUnitWithImports("", listOf(), listOf(), setOf(), setOf())
    val assignments = setOf<AssigmentStatement>()

    @ParameterizedTest
    @MethodSource(value = ["expressionReturnType"])
    fun `should find return type for expression`(expression: Expression, expectedReturnType: Type) {
        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(expectedReturnType)
    }

    @Test
    fun `should throw exception for unknown infix operator`() {
        // given
        val unknownInfixOperator = "foo"
        val expression = InfixExpression(Line(1, 2), unknownInfixOperator, booleanExpression(), booleanExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @ParameterizedTest
    @MethodSource(value = ["stringInfixOperator"])
    fun `should throw exception for infix operator that cannot be applied to string type`(infixOperator: String) {
        // given
        val expression = InfixExpression(Line(1, 2), infixOperator, stringExpression(), stringExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @ParameterizedTest
    @MethodSource(value = ["booleanInfixOperator"])
    fun `should throw exception for infix operator that cannot be applied to boolean type`(infixOperator: String) {
        // given
        val expression = InfixExpression(Line(1, 2), infixOperator, booleanExpression(), booleanExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception for condition in if expression not being boolean type`() {
        // given
        val expression = IfExpression(Line(1, 2), ParameterExpression(Line(1, 2), "x", "/foo/Boo"), stringExpression(), stringExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception for condition in negate expression not being boolean type`() {
        // given
        val expression = NegateExpression(Line(1, 2), ParameterExpression(Line(1, 2), "x", "/foo/Boo"))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should find return type for value expression`() {
        // given
        val valueName = "foo"
        val expression = ValueExpression(Line(1, 2), valueName)
        val assigmentStatement = AssigmentStatement(valueName, integerExpression())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                setOf(assigmentStatement),
                expression)

        // then
        assertThat(returnType).isEqualTo(intType)
    }

    @Test
    fun `should throw exception if cannot find assigment for given value`() {
        // given
        val expression = ValueExpression(Line(1, 2), "foo")
        val assigmentStatement = AssigmentStatement("boo", integerExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    setOf(assigmentStatement),
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should find return type for local function invocation expression with no parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf())

        val function = Function(Line(1, 2), "pkg_name", functionName, null, listOf(), setOf(), stringExpression())
        val compileUnit = CompileUnitWithImports(
                "pkg_name",
                listOf(),
                listOf(function),
                setOf(),
                setOf())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type from function invocation if function has return type`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf())

        val function = Function(
                Line(1, 2),
                "pkg_name",
                functionName,
                typeToString(intType),
                listOf(),
                setOf(),
                stringExpression())
        val compileUnit = CompileUnitWithImports(
                "pkg_name",
                listOf(),
                listOf(function),
                setOf(),
                setOf())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(intType)
    }

    @Test
    fun `should find return type for local function invocation expression with parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(Line(1, 2), "pkg_name", functionName, null, parameters, setOf(), stringExpression())
        val compileUnit = CompileUnitWithImports(
                "pkg_name",
                listOf(),
                listOf(function),
                setOf(),
                setOf())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for imported function invocation expression with no parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf())

        val function = Function(Line(1, 2), "pkg_name", functionName, null, listOf(), setOf(), stringExpression())
        val compileUnit = CompileUnitWithImports(
                "pkg_name",
                listOf(),
                listOf(),
                setOf(),
                setOf(function))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for imported function invocation expression with parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(Line(1, 2), "pkg_name", functionName, null, parameters, setOf(), stringExpression())
        val compileUnit = CompileUnitWithImports(
                "pkg_name",
                listOf(),
                listOf(),
                setOf(),
                setOf(function))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for local function invocation expression even if there is imported function with same name`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf())
        val localFunction = Function(Line(1, 2), "pkg_name", functionName, null, listOf(), setOf(), stringExpression())
        val importedFunction = Function(Line(1, 2), "pkg_name", functionName, null, listOf(), setOf(), integerExpression())
        val compileUnit = CompileUnitWithImports(
                "pkg_name",
                listOf(),
                listOf(localFunction),
                setOf(),
                setOf(importedFunction))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for function invocation expression with no parameters from compilation context`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                packageName,
                functionName,
                listOf())
        val function = Function(
                Line(1, 2),
                packageName,
                functionName,
                null,
                listOf(),
                setOf(),
                stringExpression())
        val compilationContext = CompilationContext(
                setOf(),
                setOf(function))
        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for function invocation expression with parameters from compilation context`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                Line(1, 2),
                packageName,
                functionName,
                null,
                parameters,
                setOf(),
                stringExpression())
        val compilationContext = CompilationContext(
                setOf(),
                setOf(function))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should throw exception if there is not function that matches function invocation`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception if there bad number parameters in function invocations (CompilationContext)`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                Line(1, 2),
                packageName,
                functionName,
                null,
                parameters,
                setOf(),
                stringExpression())
        val compilationContext = CompilationContext(
                setOf(),
                setOf(function))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception if there bad number parameters in function invocations (CompileUnit)`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                Line(1, 2),
                "packageName",
                functionName,
                null,
                parameters,
                setOf(),
                stringExpression())
        val compileUnit = CompileUnitWithImports(
                "packageName",
                listOf(),
                listOf(function),
                setOf(),
                setOf())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception if there bad type of parameters in function invocations (CompilationContext)`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                Line(1, 2),
                packageName,
                functionName,
                null,
                parameters,
                setOf(),
                stringExpression())
        val compilationContext = CompilationContext(
                setOf(),
                setOf(function))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception if there bad type of parameters in function invocations (CompileUnit)`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                Line(1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                Line(1, 2),
                "/package/name",
                functionName,
                null,
                parameters,
                setOf(),
                stringExpression())
        val compileUnit = CompileUnitWithImports(
                "/package/name",
                listOf(),
                listOf(function),
                setOf(),
                setOf())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Timeout(100)
    @Test
    fun `should throw exception if there will be recursion problem`() {
        // given
        val function1Name = "f1"
        val function2Name = "f2"
        val expression1 = FunctionInvocationExpression(
                Line(1, 2),
                null,
                function1Name,
                listOf())
        val expression2 = FunctionInvocationExpression(
                Line(1, 2),
                null,
                function2Name,
                listOf())
        val function1 = Function(
                Line(1, 2),
                "/package/name",
                function1Name,
                null,
                listOf(),
                setOf(),
                expression2)
        val function2 = Function(
                Line(1, 2),
                "/package/name",
                function2Name,
                null,
                listOf(),
                setOf(),
                expression1)
        val compileUnit = CompileUnitWithImports(
                "/package/name",
                listOf(),
                listOf(function1, function2),
                setOf(),
                setOf())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression1)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should find return type from empty list`() {
        // given
        val expression = NewListExpression(Line(1, 2))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(addGenericType(listType, nothingType))
    }

    @Test
    fun `should find return type from list of ints`() {
        // given
        val expression = NewListExpression(
                Line(1, 2),
                listOf(
                        IntegerExpression(Line(1, 2), 1),
                        IntegerExpression(Line(1, 2), 2),
                        IntegerExpression(Line(1, 2), 3),
                ))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(addGenericType(listType, intType))
    }

    @Test
    fun `should find return type from list of somethings`() {
        // given
        val expression = NewListExpression(
                Line(1, 2),
                listOf(
                        IntegerExpression(Line(1, 2), 1),
                        StringExpression(Line(1, 2), "2"),
                        BooleanExpression(Line(1, 2), true),
                ))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression)

        // then
        assertThat(returnType).isEqualTo(addGenericType(listType, anyType))
    }
}
