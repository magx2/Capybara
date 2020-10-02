package com.magx2.capybara

// Statements
sealed class Statement
data class AssigmentStatement(val codeMetainfo: CodeMetainfo, val name: String, val expression: Expression, val type: String?) : Statement()
sealed class Loop : Statement()
data class WhileLoopStatement(val whileExpression: Expression, val statements: List<Statement>) : Loop()
data class ForLoopStatement(val assigment: AssigmentStatement?,
                            val whileExpression: Expression,
                            val eachIteration: Statement?,
                            val statements: List<Statement>) : Loop()

// Statements
sealed class StatementWithReturnType
data class AssigmentStatementWithReturnType(val name: String, val expression: ExpressionWithReturnType, val type: Type) : Statement()