package com.magx2.capybara.export.python

import com.magx2.capybara.MethodToRewrite
import com.magx2.capybara.UnionWithType
import java.util.stream.Collectors

fun defToPython(def: AbstractDefToExport,
                assertions: Boolean,
                unions: Set<UnionWithType>,
                methodsToRewrite: Set<MethodToRewrite>,
                packageName: String): String {
    val parameters = def.parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))

    val methodDoc = generateDocForDefWithTypedParameters(def.name, def.parameters, def.returnType)
    val body =
            when (def) {
                is DefToExport -> defToPythonBody(def, assertions, unions, methodsToRewrite, packageName)
                is NativeDefToExport -> defToPythonBody(def)
            }

    val name = def.name // FIXME
    /*findMethodNameFromParameter(
       def.packageName,
       def.name,
       def.parameters,
       methodsToRewrite)*/

    return """
    |def $name($parameters):
    |$methodDoc$body""".trimMargin()
}

private fun defToPythonBody(def: DefToExport,
                            assertions: Boolean,
                            unions: Set<UnionWithType>,
                            methodsToRewrite: Set<MethodToRewrite>,
                            packageName: String): String {
//    val longLambdas = generateLongLambdas(def)
//    val longLambdasAsPython = longLambdas.entries
//            .stream()
//            .map { (lambda, name) ->
//                longLambdaToPython(
//                        name,
//                        lambda,
//                        assertions,
//                        unions,
//                        methodsToRewrite,
//                        packageName,
//                        generateLongLambdas(lambda.assignments, lambda.expression, 0),
//                        1,
//                        1)
//            }
//            .collect(Collectors.joining("\n\n")) + "\n"
//    val statements = def.statements
//            .stream()
//            .map { statementToPython(it, assertions, unions, methodsToRewrite, packageName, longLambdas, 1) }
//            .collect(Collectors.joining("\n"))
//
//    val returnExpression = if (def.returnExpression != null) {
//        "\n" +
//                generateAssertStatement(assertions, def.returnExpression, unions, methodsToRewrite, packageName, longLambdas) +
//                buildIndent(1) +
//                "return " + expressionToString(def.returnExpression, assertions, unions, methodsToRewrite, packageName, longLambdas)
//    } else {
//        ""
//    }
//
//    return  statements + returnExpression
    return "def todo():\n\tpass"
}

private fun defToPythonBody(def: NativeDefToExport): String =
        "\t" + def.nativeStatements
                .stream()
                .collect(Collectors.joining("\n\t"))


//private fun findMethodNameFromParameter(methodPackageName: String,
//                                        methodName: String,
//                                        methodParameters: List<ParameterToExport>,
//                                        methodsToRewrite: Set<MethodToRewrite>): String =
//        findMethodName(methodPackageName,
//                methodName,
//                methodParameters.map { it.type },
//                methodsToRewrite)