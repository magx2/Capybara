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
        if (def.returnType != null && def.returnExpression == null) {
            throw CompilationException(def.codeMetainfo, "Please specify return expression of type `${def.returnType}`.")
        }
        if (def.returnType == null && def.returnExpression != null) {
            throw CompilationException(def.codeMetainfo, "Please specify return type in def signature.")
        }
        val parameters = def.parameters.map { parseTypedParameter(it, compilationContext, compileUnit) }
        val assignmentsMap = HashMap<String, Type>()
        val assignments = LinkedList<AssigmentStatementWithType>()
        val statements = def.statements
                .stream()
                .flatMap { parseStatement(assignments, assignmentsMap, it) }
                .toList()
        val returnExpression = findReturnExpression(def, assignments)
        if (def.returnType != null && returnExpression != null) {
            val type = parseType(def.returnTypeCodeMetainfo!!, def.returnType, compilationContext, compileUnit)
            val returnType = returnExpression.returnType
            if (areTypesEqual(type, returnType, compilationContext, compileUnit).not()) {
                throw CompilationException(def.returnTypeCodeMetainfo, "Declared return type is not the same as actual one. " +
                        "Declared return type: `${typeToString(type)}`, actual return type: `${typeToString(returnType)}`.")
            }
        }
        return DefWithTypes(
                def.packageName,
                def.name,
                parameters,
                statements,
                returnExpression
        )
    }

    private fun parseStatement(assignments: MutableList<AssigmentStatementWithType>,
                               assignmentsMap: MutableMap<String, Type>,
                               statement: Statement): Stream<StatementWithType> =
            when (statement) {
                is AssigmentStatement -> {
                    val assigment = buildExpressionCompiler(assignments).findReturnType(statement.expression)
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
                    val condition = buildExpressionCompiler(assignments).findReturnType(statement.condition)
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
                    val whileStatements = if (statement.eachIteration != null) {
                        statement.statements + statement.eachIteration
                    } else {
                        statement.statements
                    }
                    val whileLoop = WhileLoopStatement(
                            statement.whileCodeMetainfo,
                            statement.whileCodeMetainfo,
                            statement.whileExpression,
                            whileStatements)
                    val whileElements = parseStatement(assignments, assignmentsMap, whileLoop)
                    statements = concat(statements, whileElements)
                    statements
                }
                is AssertStatement -> {
                    val assertExpression = AssertExpression(
                            statement.codeMetainfo,
                            statement.checkExpression,
                            NothingExpression(statement.codeMetainfo),
                            statement.messageExpression)
                    val x = buildExpressionCompiler(assignments).findReturnType(assertExpression) as AssertExpressionWithReturnType
                    Stream.of(AssertStatementWithType(x.checkExpression, x.messageExpression))
                }
            }

    private fun buildExpressionCompiler(assignments: MutableList<AssigmentStatementWithType>): ExpressionCompiler {
        return ExpressionCompiler(
                findPreviousAssignments(assignments),
                compilationContext,
                compileUnit,
                fullyQualifiedStructNames)
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