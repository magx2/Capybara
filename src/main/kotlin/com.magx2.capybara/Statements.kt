package com.magx2.capybara

// Statements
sealed class Statement
data class AssigmentStatement(val codeMetainfo: CodeMetainfo,
                              val name: String,
                              val expression: Expression,
                              val typeCodeMetainfo: CodeMetainfo?,
                              val type: String?) : Statement()

sealed class Loop : Statement()
data class WhileLoopStatement(
        val codeMetainfo: CodeMetainfo,
        val conditionCodeMetainfo: CodeMetainfo,
        val condition: Expression,
        val statements: List<Statement>) : Loop()

data class ForLoopStatement(val assigment: AssigmentStatement?,
                            val whileExpression: Expression,
                            val eachIteration: Statement?,
                            val statements: List<Statement>) : Loop()

// Statements
sealed class StatementWithType
data class AssigmentStatementWithType(val name: String, val expression: ExpressionWithReturnType, val type: Type) : StatementWithType()
data class WhileStatementWithType(val condition: ExpressionWithReturnType, val statements: List<StatementWithType>) : StatementWithType()