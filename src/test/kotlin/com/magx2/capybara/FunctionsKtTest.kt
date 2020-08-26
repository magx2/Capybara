package com.magx2.capybara

import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.stringType
import org.assertj.core.api.Assertions.assertThat
import org.assertj.core.api.Assertions.assertThatThrownBy
import org.assertj.core.api.ThrowableAssert
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.*
import java.util.stream.Stream

@Suppress("MemberVisibilityCanBePrivate")
internal class FunctionsKtTest {
    companion object {
        private fun integerExpression() = IntegerExpression(Random().nextLong())

        private fun booleanExpression() = BooleanExpression(Random().nextBoolean())

        private fun stringExpression() = StringExpression((Random().nextDouble() * Random().nextInt()).toString())

        @JvmStatic
        @Suppress("unused") // it is used in parametrized test
        fun expressionReturnType(): Stream<Arguments> =
                Stream.of(
                        // parenthesis expression
                        Arguments.of(ParenthesisExpression(integerExpression()), intType),
                        Arguments.of(ParenthesisExpression(booleanExpression()), booleanType),
                        Arguments.of(ParenthesisExpression(stringExpression()), stringType),
                        // basic expressions
                        Arguments.of(integerExpression(), intType),
                        Arguments.of(booleanExpression(), booleanType),
                        Arguments.of(stringExpression(), stringType),
                        // infix expressions - boolean
                        Arguments.of(InfixExpression("!=", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression("==", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression("&&", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression("||", booleanExpression(), booleanExpression()), booleanType),
                        // infix expressions - int
                        Arguments.of(InfixExpression("^", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression("*", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression("+", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression("-", integerExpression(), integerExpression()), intType),
                        // infix expressions - int
                        Arguments.of(InfixExpression("+", stringExpression(), stringExpression()), stringType),
                        // if expression
                        Arguments.of(IfExpression(booleanExpression(), integerExpression(), integerExpression()), intType),
                        Arguments.of(IfExpression(booleanExpression(), stringExpression(), stringExpression()), stringType),
                        Arguments.of(IfExpression(booleanExpression(), booleanExpression(), booleanExpression()), booleanType),
                        // if expression with smart casting to string
                        Arguments.of(IfExpression(booleanExpression(), stringExpression(), integerExpression()), stringType),
                        Arguments.of(IfExpression(booleanExpression(), integerExpression(), stringExpression()), stringType),
                        Arguments.of(IfExpression(booleanExpression(), stringExpression(), booleanExpression()), stringType),
                        Arguments.of(IfExpression(booleanExpression(), booleanExpression(), stringExpression()), stringType),
                        // negate expression
                        Arguments.of(NegateExpression(booleanExpression()), booleanType),
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
        val expression = InfixExpression(unknownInfixOperator, booleanExpression(), booleanExpression())

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
        val expression = InfixExpression(infixOperator, stringExpression(), stringExpression())

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
        val expression = InfixExpression(infixOperator, booleanExpression(), booleanExpression())

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
        val expression = IfExpression(ParameterExpression("x", "/foo/Boo"), stringExpression(), stringExpression())

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
        val expression = NegateExpression(ParameterExpression("x", "/foo/Boo"))

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
        val expression = ValueExpression(valueName)
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
        val expression = ValueExpression("foo")
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
                null,
                functionName,
                listOf())

        val function = Function("pkg_name", functionName, null, listOf(), setOf(), stringExpression())
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
                null,
                functionName,
                listOf())

        val function = Function(
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
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function("pkg_name", functionName, null, parameters, setOf(), stringExpression())
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
                null,
                functionName,
                listOf())

        val function = Function("pkg_name", functionName, null, listOf(), setOf(), stringExpression())
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
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function("pkg_name", functionName, null, parameters, setOf(), stringExpression())
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
                null,
                functionName,
                listOf())
        val localFunction = Function("pkg_name", functionName, null, listOf(), setOf(), stringExpression())
        val importedFunction = Function("pkg_name", functionName, null, listOf(), setOf(), integerExpression())
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
                packageName,
                functionName,
                listOf())
        val function = Function(
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
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
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
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
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
                null,
                functionName,
                listOf(stringExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
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
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
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
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
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
}
