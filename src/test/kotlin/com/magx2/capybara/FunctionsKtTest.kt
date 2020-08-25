package com.magx2.capybara

import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.stringType
import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.*
import java.util.stream.Stream

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
                        Arguments.of(InfixExpression(">", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression("<", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression(">=", booleanExpression(), booleanExpression()), booleanType),
                        Arguments.of(InfixExpression("<=", booleanExpression(), booleanExpression()), booleanType),
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
    }

    @ParameterizedTest
    @MethodSource(value = ["expressionReturnType"])
    fun `should find return type for expression`(expression: Expression, expectedReturnType: Type) {
        // when
        val returnType = findReturnType(
                CompilationContext(setOf(), setOf()),
                CompileUnitWithImports("", listOf(), listOf(), setOf(), setOf()),
                setOf(),
                expression
        )

        // then
        assertThat(returnType).isEqualTo(expectedReturnType)
    }
}