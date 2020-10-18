package com.magx2.capybara.export.python

import com.magx2.capybara.AssigmentStatementWithType
import com.magx2.capybara.ExpressionWithReturnType
import com.magx2.capybara.MethodToRewrite
import com.magx2.capybara.UnionWithType
import java.util.stream.Collectors

fun functionToPython(function: FunctionToExport,
                     assertions: Boolean,
                     unions: Set<UnionWithType>,
                     methodsToRewrite: Set<MethodToRewrite>,
                     packageName: String): String =
        functionToPython(
                function.name,
                function.returnExpression,
                function.parameters,
                function.assignments,
                assertions,
                unions,
                methodsToRewrite,
                packageName,
        )

fun functionToPython(name: String,
                     returnExpression: ExpressionWithReturnType,
                     parameters: List<ParameterToExport>,
                     assignments: List<AssigmentStatementWithType>,
                     assertions: Boolean,
                     unions: Set<UnionWithType>,
                     methodsToRewrite: Set<MethodToRewrite>,
                     packageName: String,
                     indent: Int = 0,
                     depth: Int = 0): String {
    val par = parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))
    val assignmentsInPython = if (assignments.isNotEmpty()) {
        assignments
                .stream()
                .map { assignmentToPython(it, assertions, unions, methodsToRewrite, packageName, indent + 1, depth + 1) }
                .collect(Collectors.joining("\n")) + "\n"
    } else {
        ""
    }
    val methodDoc = generateDocForDefWithTypedParameters(name, parameters, returnExpression.returnType, indent + 1)
    val n = name// findMethodNameFromParameter(packageName, name, parameters, methodsToRewrite) // FIXME
    val returnExpressionInPython = returnExpressionToPython(returnExpression, assertions, unions, methodsToRewrite, packageName, indent + 1, depth + 1)

    return buildIndent(indent) + "def $n($par):\n" +
            methodDoc +
            assignmentsInPython +
            returnExpressionInPython
}