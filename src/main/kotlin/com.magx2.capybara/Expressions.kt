package com.magx2.capybara

import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.floatType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.nothingType
import com.magx2.capybara.BasicTypes.stringType

sealed class Expression(open val codeMetainfo: CodeMetainfo)
data class ParenthesisExpression(override val codeMetainfo: CodeMetainfo, val expression: Expression) : Expression(codeMetainfo)
data class ParameterExpression(override val codeMetainfo: CodeMetainfo, val valueName: String, val type: String) : Expression(codeMetainfo)
sealed class ConstantExpression(codeMetainfo: CodeMetainfo) : Expression(codeMetainfo)
data class IntegerExpression(override val codeMetainfo: CodeMetainfo, val value: Long) : ConstantExpression(codeMetainfo)
data class FloatExpression(override val codeMetainfo: CodeMetainfo, val value: Double) : ConstantExpression(codeMetainfo)
data class BooleanExpression(override val codeMetainfo: CodeMetainfo, val value: Boolean) : ConstantExpression(codeMetainfo)
data class StringExpression(override val codeMetainfo: CodeMetainfo, val value: String) : ConstantExpression(codeMetainfo)
data class NothingExpression(override val codeMetainfo: CodeMetainfo) : ConstantExpression(codeMetainfo)
data class FunctionInvocationExpression(override val codeMetainfo: CodeMetainfo, val packageName: String?, val functionName: String, val parameters: List<Expression>) : Expression(codeMetainfo)
data class InfixExpression(override val codeMetainfo: CodeMetainfo, val operation: String, val left: Expression, val right: Expression) : Expression(codeMetainfo)
data class IfExpression(override val codeMetainfo: CodeMetainfo, val condition: Expression, val trueBranch: Expression, val falseBranch: Expression) : Expression(codeMetainfo)
data class NegateExpression(override val codeMetainfo: CodeMetainfo, val negateExpression: Expression) : Expression(codeMetainfo)
data class NewStruct(override val codeMetainfo: CodeMetainfo, val packageName: String?, val structName: String, val fields: List<StructField>) : Expression(codeMetainfo)
data class StructField(val codeMetainfo: CodeMetainfo, val name: String, val value: Expression)
data class ValueExpression(override val codeMetainfo: CodeMetainfo, val valueName: String) : Expression(codeMetainfo)
data class LambdaExpression(override val codeMetainfo: CodeMetainfo, val expressions: List<Expression>) : Expression(codeMetainfo)
data class StructFieldAccessExpression(override val codeMetainfo: CodeMetainfo, val structureExpression: Expression, val fieldName: String) : Expression(codeMetainfo)
data class NewListExpression(override val codeMetainfo: CodeMetainfo, val elements: List<Expression> = listOf()) : Expression(codeMetainfo)
data class AssertExpression(override val codeMetainfo: CodeMetainfo, val checkExpression: Expression, val returnExpression: Expression, val messageExpression: Expression?) : Expression(codeMetainfo)
data class StructureAccessExpression(
        override val codeMetainfo: CodeMetainfo,
        val structureName: String,
        val structureIndex: Expression,
        val structureIndexCodeMetainfo: CodeMetainfo,
        val structureType: String?) : Expression(codeMetainfo)

data class IsExpression(
        override val codeMetainfo: CodeMetainfo,
        val value: String,
        val typeCodeMetainfo: CodeMetainfo,
        val type: String) : Expression(codeMetainfo)

sealed class ExpressionWithReturnType(open val returnType: Type)
data class ParameterExpressionWithReturnType(override val returnType: Type, val valueName: String) : ExpressionWithReturnType(returnType)
sealed class ConstantExpressionWithReturnType(returnType: Type) : ExpressionWithReturnType(returnType)
data class IntegerExpressionWithReturnType(val value: Long) : ConstantExpressionWithReturnType(intType)
data class FloatExpressionWithReturnType(val value: Double) : ConstantExpressionWithReturnType(floatType)
data class BooleanExpressionWithReturnType(val value: Boolean) : ConstantExpressionWithReturnType(booleanType)
data class StringExpressionWithReturnType(val value: String) : ConstantExpressionWithReturnType(stringType)
object NothingExpressionWithReturnType : ConstantExpressionWithReturnType(nothingType)
data class FunctionInvocationExpressionWithReturnType(override val returnType: Type, val packageName: String, val functionName: String, val parameters: List<ExpressionWithReturnType>) : ExpressionWithReturnType(returnType)
data class DefInvocationExpressionWithReturnType(override val returnType: Type, val packageName: String, val functionName: String, val parameters: List<ExpressionWithReturnType>) : ExpressionWithReturnType(returnType)
data class InfixExpressionWithReturnType(override val returnType: Type, val operation: String, val left: ExpressionWithReturnType, val right: ExpressionWithReturnType) : ExpressionWithReturnType(returnType)
data class IfExpressionWithReturnType(override val returnType: Type, val condition: ExpressionWithReturnType, val trueBranch: ExpressionWithReturnType, val falseBranch: ExpressionWithReturnType) : ExpressionWithReturnType(returnType)
data class NegateExpressionWithReturnType(val negateExpression: ExpressionWithReturnType) : ExpressionWithReturnType(booleanType)
data class NewStructExpressionWithReturnType(override val returnType: Type, val fields: List<StructFieldExpressionWithReturnType>) : ExpressionWithReturnType(returnType)
data class StructFieldExpressionWithReturnType(val name: String, val value: ExpressionWithReturnType)
data class LambdaExpressionWithReturnType(override val returnType: Type, val expressions: List<ExpressionWithReturnType>) : ExpressionWithReturnType(returnType)
data class ValueExpressionWithReturnType(override val returnType: Type, val valueName: String) : ExpressionWithReturnType(returnType)
data class StructFieldAccessExpressionWithReturnType(val structureExpression: ExpressionWithReturnType, val fieldName: String, val fieldType: Type) : ExpressionWithReturnType(fieldType)
data class NewListExpressionWithReturnType(override val returnType: Type, val elements: List<ExpressionWithReturnType>) : ExpressionWithReturnType(returnType)
data class AssertExpressionWithReturnType(val checkExpression: ExpressionWithReturnType, val returnExpression: ExpressionWithReturnType, val messageExpression: ExpressionWithReturnType?) : ExpressionWithReturnType(returnExpression.returnType)
data class StructureAccessExpressionWithReturnType(
        override val returnType: Type,
        val structureType: Type,
        val structureName: String,
        val structureIndex: ExpressionWithReturnType) : ExpressionWithReturnType(returnType)

data class IsExpressionWithReturnType(val value: String, val type: Type) : ExpressionWithReturnType(booleanType)
