package com.magx2.capybara.export.python

import com.magx2.capybara.AssigmentStatementWithType
import com.magx2.capybara.ExpressionWithReturnType
import com.magx2.capybara.UnionWithType
import java.util.stream.Collectors

fun functionToPython(function: FunctionToExport,
                     assertions: Boolean,
                     unions: Set<UnionWithType>,
                     packageName: String): String =
        functionToPython(
                function.name,
                function.returnExpression,
                function.parameters,
                function.assignments,
                assertions,
                unions,
                packageName,
        )

fun functionToPython(name: String,
                     returnExpression: ExpressionWithReturnType,
                     parameters: List<ParameterToExport>,
                     assignments: List<AssigmentStatementWithType>,
                     assertions: Boolean,
                     unions: Set<UnionWithType>,
                     packageName: String,
                     indent: Int = 0,
                     depth: Int = 0): String {
    val par = parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))
    val assignmentsInPython = if (assignments.isNotEmpty()) {
        assignments
                .mapIndexed { index, assigment ->
                    assignmentToPython(assigment, assertions, unions, packageName, indent + 1, depth + index)
                }
                .stream()
                .collect(Collectors.joining("\n")) + "\n"
    } else {
        ""
    }
    val methodDoc = generateDocForDefWithTypedParameters(name, parameters, returnExpression.returnType, indent + 1)
    val returnExpressionInPython = returnExpressionToPython(returnExpression, assertions, unions, packageName, indent + 1, depth + 1 + assignments.size)

    return "def $name($par):\n" +
            methodDoc +
            assignmentsInPython +
            returnExpressionInPython
}