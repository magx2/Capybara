package com.magx2.capybara

import java.util.*
import kotlin.collections.HashMap
import kotlin.collections.HashSet

class DefCompiler(private val compilationContext: CompilationContext,
                  private val compileUnit: CompileUnitWithFlatStructs,
                  private val fullyQualifiedStructNames: Map<Type, Struct>) {
    fun def(def: Def): DefWithTypes {
        val assignmentsMap = HashMap<String, Type>()
        val assignments = LinkedList<AssigmentStatementWithType>()
        val statements = def.statements.map { statement(assignments, assignmentsMap, it) }
        val returnExpression = findReturnExpression(def, assignments)
        return DefWithTypes(
                def.packageName,
                def.name,
                listOf(),// TODO
                statements,
                returnExpression
        )
    }

    private fun statement(assignments: MutableList<AssigmentStatementWithType>,
                          assignmentsMap: MutableMap<String, Type>,
                          statement: Statement): StatementWithType =
            when (statement) {
                is AssigmentStatement -> {
                    val assigment = ExpressionCompiler(
                            findPreviousAssignments(assignments),
                            compilationContext,
                            compileUnit,
                            fullyQualifiedStructNames)
                            .findReturnType(statement.expression)
                    val assigmentType = assigment.returnType
                    val previousAssigment = assignmentsMap[statement.name]
                    if (previousAssigment != null) {
                        if (!areTypesEqual(previousAssigment, assigmentType, compilationContext, compileUnit)) {
                            throw CompilationException(
                                    statement.codeMetainfo,
                                    "Cannot reassign variable `${statement.name}` to type `${typeToString(assigmentType)}`, " +
                                            "because it is defined as `${typeToString(previousAssigment)}`.")
                        }
                    } else {
                        assignmentsMap[statement.name] = if (statement.type == null) {
                            assigmentType
                        } else {
                            parseType(
                                    statement.typeCodeMetainfo!!,
                                    statement.type,
                                    compilationContext,
                                    compileUnit)
                        }
                    }
                    val assignment = AssigmentStatementWithType(
                            statement.name,
                            assigment,
                            assigmentType)
                    assignments.add(assignment)
                    assignment
                }
                else -> TODO()
            }

    private fun findReturnExpression(def: Def, assignments: List<AssigmentStatementWithType>): ExpressionWithReturnType? {
        val uniqueAssignments = findPreviousAssignments(assignments)
        return if (def.returnExpression != null) {
            ExpressionCompiler(
                    uniqueAssignments,
                    compilationContext,
                    compileUnit,
                    fullyQualifiedStructNames)
                    .findReturnType(def.returnExpression)
        } else {
            null
        }
    }

    private fun findPreviousAssignments(assignments: List<AssigmentStatementWithType>): LinkedList<AssigmentStatementWithType> {
        val vars = HashSet<String>()
        val uniqueAssignments = LinkedList<AssigmentStatementWithType>()
        for (assignment in assignments.asReversed()) {
            if (vars.contains(assignment.name).not()) {
                vars.add(assignment.name)
                uniqueAssignments.add(0, assignment)
            }
        }
        return uniqueAssignments
    }
}