package com.magx2.capybara

sealed class Expression
data class ParenthesisExpression(val expression: Expression) : Expression()
data class ParameterExpression(val valueName: String, val type: String) : Expression()
sealed class ConstantExpression : Expression()
data class IntegerExpression(val value: Long) : ConstantExpression() {
    constructor(value: String) : this(value.toLong())
}

data class BooleanExpression(val value: Boolean) : ConstantExpression() {
    constructor(value: String) : this(value.toBoolean())
}

data class StringExpression(val value: String) : ConstantExpression()
data class FunctionInvocationExpression(val packageName: String?, val functionName: String, val parameters: List<Expression>) : Expression()
data class InfixExpression(val operation: String, val left: Expression, val right: Expression) : Expression()
data class IfExpression(val condition: Expression, val trueBranch: Expression, val falseBranch: Expression) : Expression()
data class NegateExpression(val negateExpression: Expression) : Expression()
data class NewStruct(val packageName: String?, val structName: String, val fields: List<StructField>) : Expression()
data class StructField(val name: String, val value: Expression)
data class ValueExpression(val valueName: String) : Expression()
data class NewListExpression(val elements: List<Expression> = listOf()) : Expression()