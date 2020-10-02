package com.magx2.capybara

import com.magx2.capybara.BasicTypes.booleanType
import java.util.*
import java.util.stream.Stream
import kotlin.collections.HashMap
import kotlin.collections.HashSet
import kotlin.streams.toList

class DefCompiler(private val compilationContext: CompilationContext,
                  private val compileUnit: CompileUnitWithFlatStructs,
                  private val fullyQualifiedStructNames: Map<Type, Struct>) {
    fun def(def: Def): DefWithTypes {
        val assignmentsMap = HashMap<String, Type>()
        val assignments = LinkedList<AssigmentStatementWithType>()
        val statements = def.statements
                .stream()
                .flatMap { parseStatement(assignments, assignmentsMap, it) }
                .toList()
        val returnExpression = findReturnExpression(def, assignments)
        return DefWithTypes(
                def.packageName,
                def.name,
                listOf(),// TODO
                statements,
                returnExpression
        )
    }

    private fun parseStatement(assignments: MutableList<AssigmentStatementWithType>,
                               assignmentsMap: MutableMap<String, Type>,
                               statement: Statement): Stream<StatementWithType> =
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
                    Stream.of(assignment)
                }
                is WhileLoopStatement -> {
                    val condition = ExpressionCompiler(
                            findPreviousAssignments(assignments),
                            compilationContext,
                            compileUnit,
                            fullyQualifiedStructNames)
                            .findReturnType(statement.condition)
                    if (condition.returnType != booleanType) {
                        throw CompilationException(
                                statement.conditionCodeMetainfo,
                                "While condition should return `${typeToString(booleanType)}` not `${typeToString(condition.returnType)}`."
                        )
                    }
                    val whileAssignments = LinkedList(assignments)
                    val whileAssignmentsMap = HashMap(assignmentsMap)
                    Stream.of(WhileStatementWithType(
                            condition,
                            statement.statements
                                    .stream()
                                    .flatMap { parseStatement(whileAssignments, whileAssignmentsMap, it) }
                                    .toList()))
                }
                is ForLoopStatement -> {
                    var statements = Stream.empty<StatementWithType>()
                    if (statement.assigment != null) {
                        val elements = parseStatement(assignments, assignmentsMap, statement.assigment)
                        statements = concat(statements, elements)
                    }
                    val whileLoop = WhileLoopStatement(
                            statement.whileCodeMetainfo,
                            statement.whileCodeMetainfo,
                            statement.whileExpression,
                            statement.statements)
                    val whileElements = parseStatement(assignments, assignmentsMap, whileLoop)
                    statements = concat(statements, whileElements)
                    if (statement.eachIteration != null) {
                        val elements = parseStatement(assignments, assignmentsMap, statement.eachIteration)
                        statements = concat(statements, elements)
                    }
                    statements
                }
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