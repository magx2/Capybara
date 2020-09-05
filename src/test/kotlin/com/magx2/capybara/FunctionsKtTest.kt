package com.magx2.capybara

import com.magx2.capybara.BasicTypes.anyType
import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.floatType
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
        private fun integerExpression() = IntegerExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), Random().nextLong())
        private fun floatExpression() = FloatExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), Random().nextDouble() * Random().nextLong())
        private fun booleanExpression() = BooleanExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), Random().nextBoolean())
        private fun stringExpression() = StringExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), (Random().nextDouble() * Random().nextInt()).toString())
        private fun integerExpressionWithReturnType() = IntegerExpressionWithReturnType(Random().nextLong())

        @JvmStatic
        @Suppress("unused") // it is used in parametrized test
        fun expressionReturnType(): Stream<Arguments> =
                Stream.of(
                        // parenthesis expression
                        Arguments.of(ParenthesisExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), integerExpression()), intType),
                        Arguments.of(ParenthesisExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression()), booleanType),
                        Arguments.of(ParenthesisExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), stringExpression()), stringType),
                        // basic expressions
                        Arguments.of(integerExpression(), intType),
                        Arguments.of(booleanExpression(), booleanType),
                        Arguments.of(stringExpression(), stringType),
                        // infix expressions - boolean
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "!=", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "==", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "&&", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "||", booleanExpression(), booleanExpression()), booleanType),
                        // infix expressions - float
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "^", floatExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "^", integerExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "^", floatExpression(), integerExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "*", floatExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "*", integerExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "*", floatExpression(), integerExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", floatExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", integerExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", floatExpression(), integerExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "-", floatExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "-", integerExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "-", floatExpression(), integerExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "~/", floatExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "~/", integerExpression(), floatExpression()), floatType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "~/", floatExpression(), integerExpression()), floatType),
                        // infix expressions - int
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "^", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "*", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "-", integerExpression(), integerExpression()), intType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "~/", integerExpression(), integerExpression()), intType),
                        // infix expressions - string
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", stringExpression(), stringExpression()), stringType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", integerExpression(), stringExpression()), stringType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", stringExpression(), integerExpression()), stringType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", floatExpression(), stringExpression()), stringType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "+", stringExpression(), floatExpression()), stringType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "*", stringExpression(), integerExpression()), stringType),
                        Arguments.of(InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "*", integerExpression(), stringExpression()), stringType),
                        // if expression
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), integerExpression(), integerExpression()), intType),
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), stringExpression(), stringExpression()), stringType),
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), booleanExpression(), booleanExpression()), booleanType),
                        // if expression with smart casting to string
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), stringExpression(), integerExpression()), stringType),
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), integerExpression(), stringExpression()), stringType),
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), stringExpression(), booleanExpression()), stringType),
                        Arguments.of(IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression(), booleanExpression(), stringExpression()), stringType),
                        // negate expression
                        Arguments.of(NegateExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), booleanExpression()), booleanType),
                )

        @Suppress("unused")
        @JvmStatic
        fun stringInfixOperator(): Stream<String> = Stream.of("^", "-", "&&", "||", "~/")

        @Suppress("unused")
        @JvmStatic
        fun booleanInfixOperator(): Stream<String> = Stream.of("+", "^", "*", "-", ">", "<", ">=", "<=", "~/")
    }

    val compilationContext = CompilationContext(setOf(), setOf())
    val compileUnit = CompileUnitWithFlatStructs("", setOf(), setOf(), listOf(), listOf())
    val assignments = listOf<AssigmentStatementWithReturnType>()
    val fullyQualifiedStructNames = mapOf<String, Struct>()

    @ParameterizedTest
    @MethodSource(value = ["expressionReturnType"])
    fun `should find return type for expression`(expression: Expression, expectedReturnType: Type) {
        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType)
                .`as`("Expecting type `${typeToString(expectedReturnType)}` " +
                        "but got `${typeToString(returnType)}`")
                .isEqualTo(expectedReturnType)
    }

    @Test
    fun `should throw exception for unknown infix operator`() {
        // given
        val unknownInfixOperator = "foo"
        val expression = InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), unknownInfixOperator, booleanExpression(), booleanExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @ParameterizedTest
    @MethodSource(value = ["stringInfixOperator"])
    fun `should throw exception for infix operator that cannot be applied to string type`(infixOperator: String) {
        // given
        val expression = InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), infixOperator, stringExpression(), stringExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @ParameterizedTest
    @MethodSource(value = ["booleanInfixOperator"])
    fun `should throw exception for infix operator that cannot be applied to boolean type`(infixOperator: String) {
        // given
        val expression = InfixExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), infixOperator, booleanExpression(), booleanExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception for condition in if expression not being boolean type`() {
        // given
        val expression = IfExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), ParameterExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "x", "/foo/Boo"), stringExpression(), stringExpression())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception for condition in negate expression not being boolean type`() {
        // given
        val expression = NegateExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), ParameterExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "x", "/foo/Boo"))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should find return type for value expression`() {
        // given
        val valueName = "foo"
        val expression = ValueExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), valueName)
        val assigmentStatement = AssigmentStatementWithReturnType(valueName, integerExpressionWithReturnType())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                listOf(assigmentStatement),
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(intType)
    }

    @Test
    fun `should throw exception if cannot find assigment for given value`() {
        // given
        val expression = ValueExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "foo")
        val assigmentStatement = AssigmentStatementWithReturnType("boo", integerExpressionWithReturnType())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    listOf(assigmentStatement),
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should find return type for local function invocation expression with no parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf())

        val function = Function(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "pkg_name", functionName, null, listOf(), listOf(), stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "pkg_name",
                setOf(),
                setOf(function),
                listOf(),
                listOf())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type from function invocation if function has return type`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf())

        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                "pkg_name",
                functionName,
                typeToString(intType),
                listOf(),
                listOf(),
                stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "pkg_name",
                setOf(),
                setOf(function),
                listOf(),
                listOf())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(intType)
    }

    @Test
    fun `should find return type for local function invocation expression with parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "pkg_name", functionName, null, parameters, listOf(), stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "pkg_name",
                setOf(),
                setOf(function),
                listOf(),
                listOf())

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for imported function invocation expression with no parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf())

        val function = Function(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "pkg_name", functionName, null, listOf(), listOf(), stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "pkg_name",
                setOf(),
                setOf(),
                listOf(),
                listOf(function))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for imported function invocation expression with parameters`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "pkg_name", functionName, null, parameters, listOf(), stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "pkg_name",
                setOf(),
                setOf(),
                listOf(),
                listOf(function))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for local function invocation expression even if there is imported function with same name`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf())
        val localFunction = Function(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "pkg_name", functionName, null, listOf(), listOf(), stringExpression())
        val importedFunction = Function(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "pkg_name", functionName, null, listOf(), listOf(), integerExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "pkg_name",
                setOf(),
                setOf(localFunction),
                listOf(),
                listOf(importedFunction))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for function invocation expression with no parameters from compilation context`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                listOf())
        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                null,
                listOf(),
                listOf(),
                stringExpression())
        val compilationContext = CompilationContext(
                setOf(),
                setOf(function))
        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type for function invocation expression with parameters from compilation context`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                null,
                parameters,
                listOf(),
                stringExpression())
        val compilationContext = CompilationContext(
                setOf(),
                setOf(function))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should throw exception if there is not function that matches function invocation`() {
        // given
        val packageName = "function_package"
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), booleanExpression()))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
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
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                null,
                parameters,
                listOf(),
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
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception if there bad number parameters in function invocations (CompileUnit)`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                "packageName",
                functionName,
                null,
                parameters,
                listOf(),
                stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "packageName",
                setOf(),
                setOf(function),
                listOf(),
                listOf())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
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
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                listOf(stringExpression(), integerExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                packageName,
                functionName,
                null,
                parameters,
                listOf(),
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
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should throw exception if there bad type of parameters in function invocations (CompileUnit)`() {
        // given
        val functionName = "f"
        val expression = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                functionName,
                listOf(stringExpression(), integerExpression(), integerExpression()))
        val parameters = listOf(
                Parameter("foo", typeToString(stringType)),
                Parameter("boo", typeToString(intType)),
                Parameter("bar", typeToString(booleanType)))
        val function = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                "/package/name",
                functionName,
                null,
                parameters,
                listOf(),
                stringExpression())
        val compileUnit = CompileUnitWithFlatStructs(
                "/package/name",
                setOf(),
                setOf(function),
                listOf(),
                listOf())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
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
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                function1Name,
                listOf())
        val expression2 = FunctionInvocationExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                null,
                function2Name,
                listOf())
        val function1 = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                "/package/name",
                function1Name,
                null,
                listOf(),
                listOf(),
                expression2)
        val function2 = Function(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                "/package/name",
                function2Name,
                null,
                listOf(),
                listOf(),
                expression1)
        val compileUnit = CompileUnitWithFlatStructs(
                "/package/name",
                setOf(),
                setOf(function1, function2),
                listOf(),
                listOf())

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression1,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`).isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should find return type from empty list`() {
        // given
        val expression = NewListExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(addGenericType(listType, nothingType))
    }

    @Test
    fun `should find return type from list of ints`() {
        // given
        val expression = NewListExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                listOf(
                        IntegerExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), 1),
                        IntegerExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), 2),
                        IntegerExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), 3),
                ))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(addGenericType(listType, intType))
    }

    @Test
    fun `should find return type from list of somethings`() {
        // given
        val expression = NewListExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                listOf(
                        IntegerExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), 1),
                        StringExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), "2"),
                        BooleanExpression(CodeMetainfo("/home/capybara/xyz.cb", 1, 2), true),
                ))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(addGenericType(listType, anyType))
    }

    @Test
    fun `should find return type from list access with structure type`() {
        // given
        val expression = StructureAccessExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 1),
                "foo",
                integerExpression(),
                CodeMetainfo("/home/capybara/xyz.cb", 2, 3),
                typeToString(addGenericType(listType, stringType)))

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should find return type from list access without structure type`() {
        // given
        val expression = StructureAccessExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 1),
                "foo",
                integerExpression(),
                CodeMetainfo("/home/capybara/xyz.cb", 2, 3),
                typeToString(addGenericType(listType, stringType)))

        val assignments = listOf(
                AssigmentStatementWithReturnType(
                        "foo",
                        NewListExpressionWithReturnType(
                                stringType,
                                listOf(
                                        StringExpressionWithReturnType("a"),
                                        StringExpressionWithReturnType("b"),
                                        StringExpressionWithReturnType("c"),
                                ))
                )
        )

        // when
        val returnType = findReturnType(
                compilationContext,
                compileUnit,
                assignments,
                expression,
                fullyQualifiedStructNames).returnType

        // then
        assertThat(returnType).isEqualTo(stringType)
    }

    @Test
    fun `should throw exception if there is no assigment with given name`() {
        // given
        val expression = StructureAccessExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 11, 22),
                "foo",
                integerExpression(),
                CodeMetainfo("/home/capybara/xyz.cb", 2, 3),
                null)

        val assignments = listOf(
                AssigmentStatementWithReturnType(
                        "foo2",
                        NewListExpressionWithReturnType(
                                stringType,
                                listOf(
                                        StringExpressionWithReturnType("a"),
                                        StringExpressionWithReturnType("b"),
                                        StringExpressionWithReturnType("c"),
                                ))
                )
        )

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`)
                .isInstanceOf(CompilationException::class.java)
                .hasMessage("/home/capybara/xyz.cb [11:22] Cannot find value with name `foo`")
    }

    @Test
    fun `should throw exception if list is not indexed by int`() {
        // given
        val expression = StructureAccessExpression(
                CodeMetainfo("/home/capybara/xyz.cb", 11, 22),
                "foo",
                stringExpression(),
                CodeMetainfo("/home/capybara/xyz.cb", 2, 3),
                typeToString(addGenericType(listType, stringType)))

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            findReturnType(
                    compilationContext,
                    compileUnit,
                    assignments,
                    expression,
                    fullyQualifiedStructNames)
        }

        // then
        assertThatThrownBy(`when`)
                .isInstanceOf(CompilationException::class.java)
                .hasMessageStartingWith("/home/capybara/xyz.cb [2:3] List are indexed by")
    }

    private fun findReturnType(compilationContext: CompilationContext,
                               compileUnit: CompileUnitWithFlatStructs,
                               assignments: List<AssigmentStatementWithReturnType>,
                               expression: Expression,
                               fullyQualifiedStructNames: Map<String, Struct>) =
            FunctionCompiler(compilationContext, compileUnit, fullyQualifiedStructNames).findReturnType(assignments, expression)
}
