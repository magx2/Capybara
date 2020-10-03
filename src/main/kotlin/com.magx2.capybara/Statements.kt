package com.magx2.capybara

// Statements
sealed class Statement
data class AssigmentStatement(val codeMetainfo: CodeMetainfo,
                              val name: String,
                              val expression: Expression,
                              val typeCodeMetainfo: CodeMetainfo?,
                              val type: String?) : Statement()

data class AssertStatement(val codeMetainfo: CodeMetainfo,
                           val checkExpression: Expression,
                           val messageExpression: Expression?) : Statement()

sealed class Loop : Statement()
data class WhileLoopStatement(
        val codeMetainfo: CodeMetainfo,
        val conditionCodeMetainfo: CodeMetainfo,
        val condition: Expression,
        val statements: List<Statement>) : Loop()

data class ForLoopStatement(val codeMetainfo: CodeMetainfo,
                            val assigment: AssigmentStatement?,
                            val whileCodeMetainfo: CodeMetainfo,
                            val whileExpression: Expression,
                            val eachIteration: Statement?,
                            val statements: List<Statement>) : Loop()

data class DefCallStatement(val codeMetainfo: CodeMetainfo, val packageName: String?, val defName: String, val parameters: List<Expression>) : Statement()

// Statements
sealed class StatementWithType
data class AssigmentStatementWithType(val name: String, val expression: ExpressionWithReturnType, val type: Type) : StatementWithType()
data class AssertStatementWithType(val checkExpression: ExpressionWithReturnType,
                                   val messageExpression: ExpressionWithReturnType?) : StatementWithType()

data class WhileStatementWithType(val condition: ExpressionWithReturnType, val statements: List<StatementWithType>) : StatementWithType()
data class DefCallStatementWithType(
        val packageName: String,
        val defName: String,
        val parameters: List<ExpressionWithReturnType>) : StatementWithType()