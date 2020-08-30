package com.magx2.capybara

sealed class Expression(open val line: Line)
data class ParenthesisExpression(override val line: Line, val expression: Expression) : Expression(line)
data class ParameterExpression(override val line: Line, val valueName: String, val type: String) : Expression(line)
sealed class ConstantExpression(line: Line) : Expression(line)
data class IntegerExpression(override val line: Line, val value: Long) : ConstantExpression(line) {
    constructor(line: Line, value: String) : this(line, value.toLong())
}

data class BooleanExpression(override val line: Line, val value: Boolean) : ConstantExpression(line) {
    constructor(line: Line, value: String) : this(line, value.toBoolean())
}

data class StringExpression(override val line: Line, val value: String) : ConstantExpression(line)
data class FunctionInvocationExpression(override val line: Line, val packageName: String?, val functionName: String, val parameters: List<Expression>) : Expression(line)
data class InfixExpression(override val line: Line, val operation: String, val left: Expression, val right: Expression) : Expression(line)
data class IfExpression(override val line: Line, val condition: Expression, val trueBranch: Expression, val falseBranch: Expression) : Expression(line)
data class NegateExpression(override val line: Line, val negateExpression: Expression) : Expression(line)
data class NewStruct(override val line: Line, val packageName: String?, val structName: String, val fields: List<StructField>) : Expression(line)
data class StructField(val line: Line, val name: String, val value: Expression)
data class ValueExpression(override val line: Line, val valueName: String) : Expression(line)
data class NewListExpression(override val line: Line, val elements: List<Expression> = listOf()) : Expression(line)
data class StructureAccessExpression(
        override val line: Line,
        val structureName: String,
        val structureIndex: Expression,
        val structureIndexLine: Line,
        val structureType: String?) : Expression(line)
