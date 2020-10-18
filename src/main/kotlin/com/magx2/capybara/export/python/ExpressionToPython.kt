package com.magx2.capybara.export.python

import com.magx2.capybara.*
import java.util.concurrent.atomic.AtomicInteger
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

fun assignmentToPython(assignment: AssigmentStatementWithType,
                       assertions: Boolean,
                       unions: Set<UnionWithType>,
                       methodsToRewrite: Set<MethodToRewrite>,
                       packageName: String,
                       indent: Int,
                       depth: Int): String =
        buildIndent(indent) + expressionLongOrShortToString(
                assignment.expression,
                assignment.name + " = ",
                assertions,
                unions,
                methodsToRewrite,
                packageName,
                indent,
                depth
        )

fun returnExpressionToPython(expression: ExpressionWithReturnType,
                             assertions: Boolean,
                             unions: Set<UnionWithType>,
                             methodsToRewrite: Set<MethodToRewrite>,
                             packageName: String,
                             indent: Int,
                             depth: Int): String =
        buildIndent(indent) + expressionLongOrShortToString(
                expression,
                "return ",
                assertions,
                unions,
                methodsToRewrite,
                packageName,
                indent,
                depth
        )


private fun expressionLongOrShortToString(expression: ExpressionWithReturnType,
                                          expressionPrefix: String,
                                          assertions: Boolean,
                                          unions: Set<UnionWithType>,
                                          methodsToRewrite: Set<MethodToRewrite>,
                                          packageName: String,
                                          indent: Int,
                                          depth: Int): String =
        if (isOneLinerExpression(expression, assertions)) {
            expressionPrefix + oneLinerExpressionToPython(expression, assertions, unions, methodsToRewrite, packageName)
        } else {
            when (expression) {
                is IfExpressionWithReturnType -> {
                    val (condition, prefix) = if (isOneLinerExpression(expression.condition, assertions)) {
                        Pair(
                                oneLinerExpressionToPython(expression.condition, assertions, unions, methodsToRewrite, packageName),
                                ""
                        )
                    } else {
                        val conditionName = "_condition_$depth"
                        Pair(
                                conditionName,
                                expressionLongOrShortToString(
                                        expression.condition,
                                        "$conditionName = ",
                                        assertions, unions, methodsToRewrite, packageName, indent, depth + 1) + "\n" + buildIndent(indent)
                        )
                    }
                    prefix + "if " + condition + ":\n" +
                            buildIndent(indent + 1) + ifBranchToPython(expression.trueBranch, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent + 1, depth + 1) + "\n" +
                            buildIndent(indent) + "else:\n" +
                            buildIndent(indent + 1) + ifBranchToPython(expression.falseBranch, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent + 1, depth + 1)
                }
                is LambdaExpressionWithReturnType -> {
//                    val name = longLambdas[expression]
//                            ?: throw java.lang.IllegalStateException("Should not happen ; cannot find `name` for lambda `$expression`")
//                    longLambdaToPython(
//                            name,
//                            expression,
//                            assertions,
//                            unions,
//                            methodsToRewrite,
//                            packageName,
//                            generateLongLambdas(expression.assignments, expression.expression, depth + 1),
//                            indent + 1,
//                            depth + 1)
                    TODO()
                }
                is AssertExpressionWithReturnType -> {
                    val messageExpression = expression.messageExpression
                    val checkExpression = expression.checkExpression
                    val returnExpression = expression.returnExpression
                    val (message, prefix) = if (messageExpression != null) {
                        if (isOneLinerExpression(messageExpression, assertions)) {
                            Pair(oneLinerExpressionToPython(messageExpression, assertions, unions, methodsToRewrite, packageName), "")
                        } else {
                            val conditionName = "_assert_message_$depth"
                            Pair(
                                    conditionName,
                                    expressionLongOrShortToString(messageExpression, "$conditionName = ", assertions, unions, methodsToRewrite, packageName, indent, depth + 1) + "\n" + buildIndent(indent)
                            )
                        }
                    } else {
                        Pair("\"<no message>\"", "")
                    }
                    val (checkExpressionInPython, prefix2) = if (isOneLinerExpression(checkExpression, assertions)) {
                        Pair(
                                expressionLongOrShortToString(checkExpression, "", assertions, unions, methodsToRewrite, packageName, indent, depth + 1),
                                ""
                        )
                    } else {
                        val conditionName = "_check_expression_$depth"
                        Pair(
                                conditionName,
                                expressionLongOrShortToString(checkExpression, "$conditionName = ", assertions, unions, methodsToRewrite, packageName, indent, depth + 1) + "\n" + buildIndent(indent)
                        )
                    }
                    prefix +
                            prefix2 +
                            "if not " + checkExpressionInPython + ": raise AssertionError($message)\n" +
                            buildIndent(indent) + expressionLongOrShortToString(returnExpression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth + 1)
                }
                is InfixExpressionWithReturnType -> {
                    val leftName = "_left_$depth"
                    val rightName = "_right_$depth"
                    val leftExpression = expressionLongOrShortToString(expression.left, "$leftName = ", assertions, unions, methodsToRewrite, packageName, indent, depth + 1)
                    val rightExpression = expressionLongOrShortToString(expression.right, "$rightName = ", assertions, unions, methodsToRewrite, packageName, indent, depth + 1)
                    val infixExpresion = oneLinerExpressionToPython(
                            InfixExpressionWithReturnType(
                                    expression.returnType,
                                    expression.operation,
                                    ValueExpressionWithReturnType(expression.left.returnType, leftName),
                                    ValueExpressionWithReturnType(expression.right.returnType, rightName)),
                            assertions, unions, methodsToRewrite, packageName)
                    leftExpression + "\n" +
                            buildIndent(indent) + rightExpression + "\n" +
                            buildIndent(indent) + expressionPrefix + infixExpresion
                }
                is NegateExpressionWithReturnType -> TODO()
                is NewListExpressionWithReturnType -> TODO()
                else -> throw java.lang.IllegalStateException("I don't know how to handle long expression of type `${expression.javaClass.simpleName}`")
            }
        }

fun ifBranchToPython(
        branch: IfBranchWithReturnType,
        expressionPrefix: String,
        assertions: Boolean,
        unions: Set<UnionWithType>,
        methodsToRewrite: Set<MethodToRewrite>,
        packageName: String,
        indent: Int,
        depth: Int,
): String {
    val assignments = branch.assignments
            .stream()
            .map { expressionLongOrShortToString(it.expression, it.name + " = ", assertions, unions, methodsToRewrite, packageName, indent, depth + 1) }
//                .map { buildIndent(indent) + it }
            .map { it + "\n" }
            .map { it + buildIndent(indent) }
            .collect(Collectors.joining())
    val lastExpression = expressionLongOrShortToString(branch.expression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth + 1)
    return assignments + lastExpression
}

//private fun longLambdaToPython(name: String,
//                               lambda: LambdaExpressionWithReturnType,
//                               assertions: Boolean,
//                               unions: Set<UnionWithType>,
//                               methodsToRewrite: Set<MethodToRewrite>,
//                               packageName: String,
//                               indent: Int = 1,
//                               depth: Int): String {
//    val prefix = name + "_"
//    return functionToPython(
//            name,
//            rewriteValNamesInExpression(lambda.expression, prefix),
//            lambda.parameters
//                    .map { ParameterToExport(prefix + it.name, it.type) },
//            lambda.assignments.map { assignment ->
//                AssigmentStatementWithType(
//                        prefix + assignment.name,
//                        rewriteValNamesInExpression(assignment.expression, prefix),
//                        assignment.type)
//            },
//            longLambdas.filterKeys { k -> k != lambda },
//            assertions,
//            unions,
//            methodsToRewrite,
//            packageName,
//            indent,
//            depth
//    )
//}


private fun rewriteValNamesInExpression(expression: ExpressionWithReturnType, prefix: String): ExpressionWithReturnType =
        when (expression) {
            is LambdaExpressionWithReturnType,
            is IntegerExpressionWithReturnType,
            is FloatExpressionWithReturnType,
            is BooleanExpressionWithReturnType,
            is StringExpressionWithReturnType,
            NothingExpressionWithReturnType,
            is NewStructExpressionWithReturnType,
            is IsExpressionWithReturnType -> expression
            is ParameterExpressionWithReturnType -> ParameterExpressionWithReturnType(expression.returnType, prefix + expression.valueName)
            is FunctionInvocationExpressionWithReturnType ->
                FunctionInvocationExpressionWithReturnType(
                        expression.returnType,
                        when (expression.functionInvocation) {
                            is FunctionInvocationByNameWithReturnType -> expression.functionInvocation
                            is FunctionInvocationByExpressionWithReturnType ->
                                FunctionInvocationByExpressionWithReturnType(
                                        rewriteValNamesInExpression(expression.functionInvocation.expression, prefix)
                                )
                        },
                        expression.parameters
                                .stream()
                                .map { rewriteValNamesInExpression(it, prefix) }
                                .toList()
                )
            is DefInvocationExpressionWithReturnType ->
                DefInvocationExpressionWithReturnType(
                        expression.returnType,
                        expression.packageName,
                        expression.functionName,
                        expression.parameters
                                .stream()
                                .map { rewriteValNamesInExpression(it, prefix) }
                                .toList()
                )
            is InfixExpressionWithReturnType ->
                InfixExpressionWithReturnType(
                        expression.returnType,
                        expression.operation,
                        rewriteValNamesInExpression(expression.left, prefix),
                        rewriteValNamesInExpression(expression.right, prefix),
                )
            is IfExpressionWithReturnType ->
                IfExpressionWithReturnType(
                        expression.returnType,
                        rewriteValNamesInExpression(expression.condition, prefix),
                        IfBranchWithReturnType(
                                rewriteValNamesInExpression(expression.trueBranch.expression, prefix),
                                expression.trueBranch.assignments
                        ),
                        IfBranchWithReturnType(
                                rewriteValNamesInExpression(expression.falseBranch.expression, prefix),
                                expression.falseBranch.assignments
                        )
                )
            is NegateExpressionWithReturnType -> NegateExpressionWithReturnType(rewriteValNamesInExpression(expression.negateExpression, prefix))
            is ValueExpressionWithReturnType -> ValueExpressionWithReturnType(expression.returnType, prefix + expression.valueName)
            is StructFieldAccessExpressionWithReturnType ->
                StructFieldAccessExpressionWithReturnType(
                        rewriteValNamesInExpression(expression.structureExpression, prefix),
                        expression.fieldName,
                        expression.fieldType
                )
            is NewListExpressionWithReturnType ->
                NewListExpressionWithReturnType(
                        expression.returnType,
                        expression.elements
                                .map { rewriteValNamesInExpression(it, prefix) }
                )
            is AssertExpressionWithReturnType ->
                AssertExpressionWithReturnType(
                        rewriteValNamesInExpression(expression.checkExpression, prefix),
                        rewriteValNamesInExpression(expression.returnExpression, prefix),
                        if (expression.messageExpression != null) rewriteValNamesInExpression(expression.messageExpression, prefix) else null
                )
            is StructureAccessExpressionWithReturnType ->
                StructureAccessExpressionWithReturnType(
                        expression.returnType,
                        expression.structureType,
                        expression.structureName,
                        rewriteValNamesInExpression(expression.structureIndex, prefix)
                )
        }

private fun oneLinerExpressionToPython(expression: ExpressionWithReturnType,
                                       assertions: Boolean,
                                       unions: Set<UnionWithType>,
                                       methodsToRewrite: Set<MethodToRewrite>,
                                       packageName: String): String =
        when (expression) {
            is ParameterExpressionWithReturnType -> expression.valueName
            is IntegerExpressionWithReturnType -> expression.value.toString()
            is FloatExpressionWithReturnType -> expression.value.toString()
            is BooleanExpressionWithReturnType -> if (expression.value) "True" else "False"
            is StringExpressionWithReturnType -> "\"${expression.value}\""
            is NothingExpressionWithReturnType -> "None"
            is FunctionInvocationExpressionWithReturnType ->
                methodToString(
                        expression.functionInvocation,
                        expression.parameters,
                        assertions,
                        unions,
                        methodsToRewrite,
                        packageName)
            is DefInvocationExpressionWithReturnType ->
                methodToString(
                        FunctionInvocationByNameWithReturnType(
                                expression.returnType,
                                expression.packageName,
                                expression.functionName,
                        ),
                        expression.parameters,
                        assertions,
                        unions,
                        methodsToRewrite,
                        packageName)
            is InfixExpressionWithReturnType -> "(${oneLinerExpressionToPython(expression.left, assertions, unions, methodsToRewrite, packageName)}) ${mapInfixOperator(expression)} (${oneLinerExpressionToPython(expression.right, assertions, unions, methodsToRewrite, packageName)})"
            is IfExpressionWithReturnType -> "(${oneLinerExpressionToPython(expression.trueBranch.expression, assertions, unions, methodsToRewrite, packageName)}) if (${oneLinerExpressionToPython(expression.condition, assertions, unions, methodsToRewrite, packageName)}) else (${oneLinerExpressionToPython(expression.falseBranch.expression, assertions, unions, methodsToRewrite, packageName)})"
            is NegateExpressionWithReturnType -> "not ${oneLinerExpressionToPython(expression.negateExpression, assertions, unions, methodsToRewrite, packageName)}"
            is NewStructExpressionWithReturnType -> {
                val parameters = expression.fields
                        .stream()
                        .map { (name, expression) -> Pair(name, oneLinerExpressionToPython(expression, assertions, unions, methodsToRewrite, packageName)) }
                        .map { (name, value) -> "${name}=${value}" }
                        .collect(Collectors.joining(", "))
                if (expression.returnType.packageName == packageName) {
                    "${expression.returnType.name}($parameters)"
                } else {
                    "${packageToPythonPackage(expression.returnType.packageName)}.${expression.returnType.name}($parameters)"
                }
            }
            is ValueExpressionWithReturnType -> expression.valueName
            is LambdaExpressionWithReturnType -> {
                val prefix = "_short_lambda_"
                val parameters = expression.parameters
                        .stream()
                        .map { it.name }
                        .map { prefix + it }
                        .collect(Collectors.joining(", "))
                val exp = rewriteValNamesInExpression(expression.expression, prefix)
                "(lambda $parameters: " + oneLinerExpressionToPython(exp, assertions, unions, methodsToRewrite, packageName) + ")"
            }
            is NewListExpressionWithReturnType -> expression.elements
                    .stream()
                    .map { oneLinerExpressionToPython(it, assertions, unions, methodsToRewrite, packageName) }
                    .collect(Collectors.joining(", ", "[", "]"))
            is StructureAccessExpressionWithReturnType -> {
                "${expression.structureName}[${oneLinerExpressionToPython(expression.structureIndex, assertions, unions, methodsToRewrite, packageName)}]"
            }
            is IsExpressionWithReturnType -> {
                val union = unions.stream()
                        .filter { it.type == expression.type }
                        .findAny()
                if (union.isPresent) {
                    union.get()
                            .types
                            .stream()
                            .map { isInstance(expression.value, it) }
                            .collect(Collectors.joining(" or ", "(", ")"))
                } else {
                    isInstance(expression.value, expression.type)
                }
            }
            is StructFieldAccessExpressionWithReturnType -> "${oneLinerExpressionToPython(expression.structureExpression, assertions, unions, methodsToRewrite, packageName)}.${expression.fieldName}"
            is AssertExpressionWithReturnType -> {
                oneLinerExpressionToPython(expression.returnExpression, assertions, unions, methodsToRewrite, packageName)
            }
        }

private fun methodToString(
        functionInvocation: FunctionInvocationWithReturnType,
        expressionParameters: List<ExpressionWithReturnType>,
        assertions: Boolean,
        unions: Set<UnionWithType>,
        methodsToRewrite: Set<MethodToRewrite>,
        packageName: String): String {
    val parameters = expressionParameters
            .stream()
            .map { oneLinerExpressionToPython(it, assertions, unions, methodsToRewrite, packageName) }
            .collect(Collectors.joining(", "))
    return when (functionInvocation) {
        is FunctionInvocationByNameWithReturnType -> {
            val name = findMethodNameFromExpression(
                    functionInvocation.packageName,
                    functionInvocation.functionName,
                    expressionParameters,
                    methodsToRewrite)
            if (functionInvocation.packageName == packageName) {
                "$name($parameters)"
            } else {
                "${packageToPythonPackage(functionInvocation.packageName)}.$name($parameters)"
            }
        }
        is FunctionInvocationByExpressionWithReturnType -> {
            val lambda = oneLinerExpressionToPython(functionInvocation.expression, assertions, unions, methodsToRewrite, packageName)
            "($lambda)($parameters)"
        }
    }
}

private fun isInstance(expressionValue: String, expressionType: Type): String =
        if (expressionType == BasicTypes.nothingType) {
            "$expressionValue is None"
        } else {
            "isinstance($expressionValue, ${findPythonType(expressionType)})"
        }

fun findPythonType(type: Type): String =
        if (type.packageName == typePackageName) {
            when (type) {
                BasicTypes.intType -> "int"
                BasicTypes.floatType -> "float"
                BasicTypes.booleanType -> "bool"
                BasicTypes.stringType -> "str"
                BasicTypes.listType -> "list"
                else -> packageToPythonPackage(type.packageName) + "." + type.name
            }
        } else {
            packageToPythonPackage(type.packageName) + "." + type.name
        }


private fun mapInfixOperator(expression: InfixExpressionWithReturnType) = when (expression.operation) {
    "^" -> "**"
    "~/" -> "//"
    "&&" -> "and"
    "||" -> "or"
    else -> expression.operation
}

private fun isOneLinerExpression(expression: ExpressionWithReturnType, assertions: Boolean): Boolean =
        when (expression) {
            is LambdaExpressionWithReturnType -> isShortLambda(expression)
            is IfExpressionWithReturnType -> isShortIf(expression, assertions)
            is AssertExpressionWithReturnType -> assertions.not()
            is InfixExpressionWithReturnType -> isOneLinerExpression(expression.right, assertions) && isOneLinerExpression(expression.left, assertions)
            is NegateExpressionWithReturnType -> isOneLinerExpression(expression.negateExpression, assertions)
            is NewListExpressionWithReturnType ->
                expression.elements
                        .map { isOneLinerExpression(it, assertions) }
                        .all { it }
            is StructureAccessExpressionWithReturnType -> isOneLinerExpression(expression.structureIndex, assertions)
            is ParameterExpressionWithReturnType,
            is IntegerExpressionWithReturnType,
            is FloatExpressionWithReturnType,
            is BooleanExpressionWithReturnType,
            is StringExpressionWithReturnType,
            NothingExpressionWithReturnType,
            is NewStructExpressionWithReturnType,
            is ValueExpressionWithReturnType,
            is StructFieldAccessExpressionWithReturnType,
            is FunctionInvocationExpressionWithReturnType, // TODO check
            is DefInvocationExpressionWithReturnType, // TODO check
            is IsExpressionWithReturnType -> true
        }

private fun isShortIf(expression: IfExpressionWithReturnType, assertions: Boolean) =
        expression.trueBranch.assignments.isEmpty()
                && expression.falseBranch.assignments.isEmpty()
                && isOneLinerExpression(expression.trueBranch.expression, assertions)
                && isOneLinerExpression(expression.falseBranch.expression, assertions)

private fun isShortLambda(lambda: LambdaExpressionWithReturnType): Boolean =
        lambda.assignments.isEmpty() &&
                (lambda.expression !is LambdaExpressionWithReturnType || isShortLambda(lambda.expression))

private fun generateLongLambdas(expressions: List<ExpressionWithReturnType>, depth: Int): Map<LambdaExpressionWithReturnType, String> {
    val lambdaNr = AtomicInteger()
    val collect = expressions.stream()
            .flatMap { findLambdas(it).stream() }
//            .filter { it is LambdaExpressionWithReturnType }
//            .map { it as LambdaExpressionWithReturnType }
            .filter { l -> isShortLambda(l).not() }
            .collect(Collectors.groupingBy { it })
    return collect
            .entries
            .stream()
//            .map { (k, v) -> Pair(k, v.last().second) }
            .map { (k, _) ->
                Pair(
                        k,
                        "_local_lambda_" +
                                (if (depth > 0) "depth_" + depth + "_" else "") +
                                "nr_" + lambdaNr.incrementAndGet())
            }
            .collect(Collectors.toMap(
                    { (k, _) -> k },
                    { (_, v) -> v }))
}


private fun findLambdas(expresion: ExpressionWithReturnType): List<LambdaExpressionWithReturnType> =
        when (expresion) {
            is LambdaExpressionWithReturnType -> listOf(expresion)
            is FunctionInvocationExpressionWithReturnType ->
                when (expresion.functionInvocation) {
                    is FunctionInvocationByNameWithReturnType -> emptyList()
                    is FunctionInvocationByExpressionWithReturnType -> findLambdas(expresion.functionInvocation.expression)
                }
            is InfixExpressionWithReturnType -> findLambdas(expresion.left) + findLambdas(expresion.right)
            is IfExpressionWithReturnType -> findLambdas(expresion.condition) + findLambdas(expresion.trueBranch.expression) + findLambdas(expresion.falseBranch.expression)
            is NegateExpressionWithReturnType -> findLambdas(expresion.negateExpression)
            is AssertExpressionWithReturnType -> findLambdas(expresion.checkExpression) +
                    findLambdas(expresion.returnExpression) +
                    if (expresion.messageExpression != null) findLambdas(expresion.messageExpression) else emptyList()
            is DefInvocationExpressionWithReturnType,
            is StructureAccessExpressionWithReturnType,
            is NewStructExpressionWithReturnType,
            is StructFieldAccessExpressionWithReturnType,
            is NewListExpressionWithReturnType,
            is IsExpressionWithReturnType,
            is ParameterExpressionWithReturnType,
            is IntegerExpressionWithReturnType,
            is FloatExpressionWithReturnType,
            is BooleanExpressionWithReturnType,
            is StringExpressionWithReturnType,
            is ValueExpressionWithReturnType,
            NothingExpressionWithReturnType -> emptyList()
        }

private fun generateLongLambdas(function: FunctionToExport): Map<LambdaExpressionWithReturnType, String> =
        generateLongLambdas(function.assignments, function.returnExpression, 0)

private fun generateLongLambdas(assignments: List<AssigmentStatementWithType>,
                                returnExpression: ExpressionWithReturnType,
                                depth: Int): Map<LambdaExpressionWithReturnType, String> =
        generateLongLambdas(
                concat(
                        assignments.stream().map { it.expression },
                        Stream.of(returnExpression)).toList(), depth + 1)

private fun generateLongLambdas(def: DefToExport): Map<LambdaExpressionWithReturnType, String> =
        generateLongLambdas(
                concat(
                        def.statements
                                .stream()
                                .flatMap { findExpressionsInStatement(it) },
                        Stream.of(def.returnExpression).filter { it != null }.map { it!! }
                ).toList(),
                0)

private fun findMethodNameFromExpression(methodPackageName: String,
                                         methodName: String,
                                         methodParameters: List<ExpressionWithReturnType>,
                                         methodsToRewrite: Set<MethodToRewrite>): String =
        findMethodName(methodPackageName,
                methodName,
                methodParameters.map { it.returnType },
                methodsToRewrite)

private fun findMethodName(methodPackageName: String,
                           methodName: String,
                           methodParameters: List<Type>,
                           methodsToRewrite: Set<MethodToRewrite>): String {
    val isDuplicated = methodsToRewrite.stream()
            .filter { it.packageName == methodPackageName }
            .filter { it.name == it.name }
            .findAny()
            .isPresent
    return if (isDuplicated) {
        val par = methodParameters
                .stream()
                .map { it.packageName.replace("/", "_") + "_" + it.name }
                .collect(Collectors.joining("_"))
        methodName + par
    } else {
        methodName
    }
}


private fun findExpressionsInStatement(statement: StatementWithType): Stream<ExpressionWithReturnType> {
    return when (statement) {
        is AssigmentStatementWithType -> Stream.of(statement.expression)
        is AssertStatementWithType ->
            if (statement.messageExpression != null) {
                Stream.of(statement.checkExpression, statement.messageExpression)
            } else {
                Stream.of(statement.checkExpression)
            }
        is WhileStatementWithType -> concat(
                Stream.of(statement.condition),
                statement.statements
                        .stream()
                        .flatMap { findExpressionsInStatement(it) }
        )
        is DefCallStatementWithType -> Stream.empty()
    }
}