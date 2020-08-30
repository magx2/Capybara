package com.magx2.capybara

sealed class Expression(open val codeMetainfo: CodeMetainfo)
data class ParenthesisExpression(override val codeMetainfo: CodeMetainfo, val expression: Expression) : Expression(codeMetainfo)
data class ParameterExpression(override val codeMetainfo: CodeMetainfo, val valueName: String, val type: String) : Expression(codeMetainfo)
sealed class ConstantExpression(codeMetainfo: CodeMetainfo) : Expression(codeMetainfo)
data class IntegerExpression(override val codeMetainfo: CodeMetainfo, val value: Long) : ConstantExpression(codeMetainfo) {
    constructor(codeMetainfo: CodeMetainfo, value: String) : this(codeMetainfo, value.toLong())
}

data class BooleanExpression(override val codeMetainfo: CodeMetainfo, val value: Boolean) : ConstantExpression(codeMetainfo) {
    constructor(codeMetainfo: CodeMetainfo, value: String) : this(codeMetainfo, value.toBoolean())
}

data class StringExpression(override val codeMetainfo: CodeMetainfo, val value: String) : ConstantExpression(codeMetainfo)
data class FunctionInvocationExpression(override val codeMetainfo: CodeMetainfo, val packageName: String?, val functionName: String, val parameters: List<Expression>) : Expression(codeMetainfo)
data class InfixExpression(override val codeMetainfo: CodeMetainfo, val operation: String, val left: Expression, val right: Expression) : Expression(codeMetainfo)
data class IfExpression(override val codeMetainfo: CodeMetainfo, val condition: Expression, val trueBranch: Expression, val falseBranch: Expression) : Expression(codeMetainfo)
data class NegateExpression(override val codeMetainfo: CodeMetainfo, val negateExpression: Expression) : Expression(codeMetainfo)
data class NewStruct(override val codeMetainfo: CodeMetainfo, val packageName: String?, val structName: String, val fields: List<StructField>) : Expression(codeMetainfo)
data class StructField(val codeMetainfo: CodeMetainfo, val name: String, val value: Expression)
data class ValueExpression(override val codeMetainfo: CodeMetainfo, val valueName: String) : Expression(codeMetainfo)
data class NewListExpression(override val codeMetainfo: CodeMetainfo, val elements: List<Expression> = listOf()) : Expression(codeMetainfo)
data class StructureAccessExpression(
        override val codeMetainfo: CodeMetainfo,
        val structureName: String,
        val structureIndex: Expression,
        val structureIndexCodeMetainfo: CodeMetainfo,
        val structureType: String?) : Expression(codeMetainfo)
