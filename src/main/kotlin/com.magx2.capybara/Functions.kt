package com.magx2.capybara

import kotlin.streams.toList

data class Function(val codeMetainfo: CodeMetainfo,
                    val packageName: String,
                    val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val assignments: List<AssigmentStatement>,
                    val returnExpression: Expression)

data class FunctionWithReturnType(val packageName: String,
                                  val name: String,
                                  val returnExpression: ExpressionWithReturnType,
                                  val parameters: List<TypedParameter>,
                                  val assignments: List<AssigmentStatementWithType>)

class FunctionCompiler(private val compilationContext: CompilationContext,
                       private val compileUnit: CompileUnitWithFlatStructs,
                       private val fullyQualifiedStructNames: Map<Type, Struct>) {

    fun findReturnTypeForAssignments(assignments: List<AssigmentStatement>): List<AssigmentStatementWithType> {
        val assignmentsWithReturnType = ArrayList<AssigmentStatementWithType>(assignments.size)
        for (assigment in assignments) {
            assignmentsWithReturnType.add(
                    findReturnTypeForAssignment(assigment, assignmentsWithReturnType)
            )
        }
        return assignmentsWithReturnType
    }

    private fun findReturnTypeForAssignment(
            assignment: AssigmentStatement,
            assignmentsWithReturnType: List<AssigmentStatementWithType>): AssigmentStatementWithType {
        val expression = ExpressionCompiler(assignmentsWithReturnType, compilationContext, compileUnit, fullyQualifiedStructNames)
                .findReturnType(assignment.expression)
        val type = if (assignment.type != null) {
            val types = concat(
                    (compileUnit.structs + compileUnit.importStructs).stream().map { it.type },
                    compileUnit.unions.stream().map { it.type },
                    compileUnit.importUnions.stream().map { it.type })
                    .toList()
                    .toSet()
            parseType(assignment.codeMetainfo, assignment.type, types)
        } else {
            expression.returnType
        }
        return AssigmentStatementWithType(
                assignment.name,
                expression,
                type)
    }
}
