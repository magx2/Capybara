package com.magx2.capybara

private fun statementToPython(statement: StatementWithType,
                              assertions: Boolean,
                              unions: Set<UnionWithType>,
                              methodsToRewrite: Set<MethodToRewrite>,
                              packageName: String,
                              longLambdas: Map<LambdaExpressionWithReturnType, String>,
                              indent: Int): String = "# TODO" // TODO
//        when (statement) {
//            is AssigmentStatementWithType -> {
//                val assert = if (statement.expression is AssertExpressionWithReturnType) {
//                    generateAssertStatement(assertions, statement.expression, unions, methodsToRewrite, packageName, longLambdas, indent)
//                } else {
//                    ""
//                }
//                "$assert${buildIndent(indent)}${statement.name} = ${expressionToString(statement.expression, assertions, unions, methodsToRewrite, packageName, longLambdas)}"
//            }
//            is WhileStatementWithType -> {
//                val statements = statement.statements
//                        .stream()
//                        .map { statementToPython(it, assertions, unions, methodsToRewrite, packageName, longLambdas, indent + 1) }
//                        .collect(Collectors.joining("\n"))
//                "${buildIndent(indent)}while ${expressionToString(statement.condition, assertions, unions, methodsToRewrite, packageName, longLambdas)}:\n$statements"
//            }
//            is AssertStatementWithType ->
//                TODO()
////                generateAssertStatement(
////                    assertions,
////                    statement.checkExpression,
////                    statement.messageExpression,
////                    unions,
////                    methodsToRewrite,
////                    packageName,
////                    longLambdas,
////                    indent)
//            is DefCallStatementWithType -> {
//                val parameters = statement.parameters
//                        .stream()
//                        .map { expressionToString(it, assertions, unions, methodsToRewrite, packageName, longLambdas) }
//                        .collect(Collectors.joining(", "))
//                val name = findMethodNameFromExpression(
//                        statement.packageName,
//                        statement.defName,
//                        statement.parameters,
//                        methodsToRewrite)
//                buildIndent(indent) + if (statement.packageName == packageName) {
//                    "$name($parameters)"
//                } else {
//                    "${packageToPythonPackage(statement.packageName)}.$name($parameters)"
//                }
//            }
//        }