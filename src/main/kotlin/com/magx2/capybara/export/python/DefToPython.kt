package com.magx2.capybara.export.python

import com.magx2.capybara.UnionWithType
import java.util.stream.Collectors

fun defToPython(def: AbstractDefToExport,
                assertions: Boolean,
                unions: Set<UnionWithType>,
                packageName: String): String {
    val parameters = def.parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))

    val methodDoc = generateDocForDefWithTypedParameters(def.name, def.parameters, def.returnType)
    val body =
            when (def) {
                is DefToExport -> defToPythonBody(def, assertions, unions, packageName)
                is NativeDefToExport -> defToPythonBody(def)
            }

    return """
    |def ${def.name}($parameters):
    |$methodDoc$body""".trimMargin()
}

private fun defToPythonBody(def: DefToExport,
                            assertions: Boolean,
                            unions: Set<UnionWithType>,
                            packageName: String): String {
    val statements = def.statements
            .stream()
            .map { statementToPython(it, assertions, unions, packageName, 1, 0) }
            .collect(Collectors.joining("\n"))

    val returnExpression = if (def.returnExpression != null) {
        "\n" + returnExpressionToPython(def.returnExpression, assertions, unions, packageName, 1, 0)
    } else {
        ""
    }

    return statements + returnExpression
}

private fun defToPythonBody(def: NativeDefToExport): String =
        buildIndent(1) + def.nativeStatements
                .stream()
                .collect(Collectors.joining("\n" + buildIndent(1)))
