package com.magx2.capybara.export.python

import com.magx2.capybara.*
import java.util.stream.Collectors
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
                    val name = "_lambda_$depth"
                    val prefix = name + "_"
                    val lambdaInPython = functionToPython(
                            name,
                            rewriteValNamesInExpression(expression.expression, prefix),
                            expression.parameters
                                    .map { ParameterToExport(prefix + it.name, it.type) },
                            expression.assignments.map { assignment ->
                                AssigmentStatementWithType(
                                        prefix + assignment.name,
                                        rewriteValNamesInExpression(assignment.expression, prefix),
                                        assignment.type)
                            },
                            assertions,
                            unions,
                            methodsToRewrite,
                            packageName,
                            indent,
                            depth + 1
                    )
                    val newExpression = ValueExpressionWithReturnType(expression.returnType, name)
                    lambdaInPython + "\n" +
                            buildIndent(indent) + expressionLongOrShortToString(newExpression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth)
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
                is NegateExpressionWithReturnType -> {
                    val (valExpression, longExpression) = introduceNewVariable("_negate_$depth", expression.negateExpression, assertions, unions, methodsToRewrite, packageName, indent, depth)
                    longExpression + "\n" +
                            buildIndent(indent) + expressionLongOrShortToString(valExpression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth)
                }
                is NewListExpressionWithReturnType -> {
                    val elementNames = ArrayList<ValueExpressionWithReturnType>(expression.elements.size)
                    var prefix = ""
                    val prefixName = "_new_list_${depth}_"
                    for (idx in expression.elements.indices) {
                        val element = expression.elements[idx]
                        val elementName = prefixName + idx
                        val (valueExpressionWithReturnType, longExpression) = introduceNewVariable(elementName, element, assertions, unions, methodsToRewrite, packageName, indent, depth)
                        prefix += longExpression + "\n" + buildIndent(indent)
                        elementNames.add(valueExpressionWithReturnType)
                    }
                    val newExpression = NewListExpressionWithReturnType(expression.returnType, elementNames)
                    prefix + expressionLongOrShortToString(newExpression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth)
                }
                is FunctionInvocationExpressionWithReturnType -> {
                    val functionInvocation = expression.functionInvocation
                    if (functionInvocation !is FunctionInvocationByExpressionWithReturnType) {
                        throw IllegalStateException("`expression.functionInvocation` should be of type `${FunctionInvocationByExpressionWithReturnType::class.java.simpleName}`, was `${functionInvocation.javaClass.simpleName}`")
                    }
                    val (valExpression, longExpression) = introduceNewVariable("_function_invocation_$depth", functionInvocation.expression, assertions, unions, methodsToRewrite, packageName, indent, depth)
                    longExpression + "\n" +
                            buildIndent(indent) + expressionLongOrShortToString(valExpression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth)
                }
                is DefInvocationExpressionWithReturnType -> {
                    val elementNames = ArrayList<ValueExpressionWithReturnType>(expression.parameters.size)
                    val parameters = expression.parameters
                    var prefix = ""
                    for (idx in parameters.indices) {
                        val parameter = parameters[idx]
                        val (valExpression, longExpression) = introduceNewVariable("_def_inv_${depth}_$idx", parameter, assertions, unions, methodsToRewrite, packageName, indent, depth)
                        prefix += longExpression + "\n" + buildIndent(indent)
                        elementNames.add(valExpression)
                    }
                    val newExpression = DefInvocationExpressionWithReturnType(expression.returnType, expression.packageName, expression.functionName, elementNames)
                    prefix + expressionLongOrShortToString(newExpression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth)
                }
                else -> throw IllegalStateException("I don't know how to handle long expression of type `${expression.javaClass.simpleName}`")
            }
        }

private fun introduceNewVariable(valName: String,
                                 longExpression: ExpressionWithReturnType,
                                 assertions: Boolean,
                                 unions: Set<UnionWithType>,
                                 methodsToRewrite: Set<MethodToRewrite>,
                                 packageName: String,
                                 indent: Int,
                                 depth: Int): Pair<ValueExpressionWithReturnType, String> {
    val newExpression = expressionLongOrShortToString(longExpression, "$valName = ", assertions, unions, methodsToRewrite, packageName, indent, depth + 1)
    val valueExpression = ValueExpressionWithReturnType(longExpression.returnType, valName)
    return Pair(valueExpression, newExpression)
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
            .map { it + "\n" }
            .map { it + buildIndent(indent) }
            .collect(Collectors.joining())
    val lastExpression = expressionLongOrShortToString(branch.expression, expressionPrefix, assertions, unions, methodsToRewrite, packageName, indent, depth + 1)
    return assignments + lastExpression
}

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
            is FunctionInvocationExpressionWithReturnType ->
                expression.parameters
                        .map { isOneLinerExpression(it, assertions) }
                        .all { it } &&
                        (expression.functionInvocation !is FunctionInvocationByExpressionWithReturnType || isOneLinerExpression(expression.functionInvocation.expression, true))
            is DefInvocationExpressionWithReturnType ->
                expression.parameters
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
