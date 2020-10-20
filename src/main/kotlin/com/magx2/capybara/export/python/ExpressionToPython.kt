package com.magx2.capybara.export.python

import com.magx2.capybara.*
import java.util.stream.Collectors
import kotlin.streams.toList

fun assignmentToPython(assignment: AssigmentStatementWithType,
                       assertions: Boolean,
                       unions: Set<UnionWithType>,
                       packageName: String,
                       indent: Int,
                       depth: Int): String =
        buildIndent(indent) + expressionLongOrShortToString(
                assignment.expression,
                assignment.name + " = ",
                assertions,
                unions,
                packageName,
                indent,
                depth
        )

fun returnExpressionToPython(expression: ExpressionWithReturnType,
                             assertions: Boolean,
                             unions: Set<UnionWithType>,
                             packageName: String,
                             indent: Int,
                             depth: Int): String =
        buildIndent(indent) + expressionLongOrShortToString(
                expression,
                "return ",
                assertions,
                unions,
                packageName,
                indent,
                depth
        )

fun whileToPython(statement: WhileStatementWithType,
                  assertions: Boolean,
                  unions: Set<UnionWithType>,
                  packageName: String,
                  indent: Int,
                  depth: Int): String {
    val statements = statement.statements
            .stream()
            .map { statementToPython(it, assertions, unions, packageName, indent + 1, depth + 1) }
            .collect(Collectors.joining("\n"))

    val condition = statement.condition
    val whileInPython = if (isOneLinerExpression(condition, assertions)) {
        "while " + oneLinerExpressionToPython(condition, assertions, unions, packageName) + ":"
    } else {
        val (valExpression, longExpression) = introduceNewVariable("_while_condition_$depth", condition, assertions, unions, packageName, indent, depth + 1)
        longExpression + "\n"
        buildIndent(indent) + "while " + oneLinerExpressionToPython(valExpression, assertions, unions, packageName) + ":"
    }

    return buildIndent(indent) + whileInPython + "\n" + statements
}

fun defCallToPython(statement: DefCallStatementWithType,
                    assertions: Boolean,
                    unions: Set<UnionWithType>,
                    packageName: String,
                    indent: Int,
                    depth: Int): String {
    val hasNonOneLiner = statement.parameters
            .stream()
            .filter { !isOneLinerExpression(it, assertions) }
            .findAny()
            .isPresent
    val (prefix, parameters) = if (hasNonOneLiner) {
        mapListOfExpressions(statement.parameters, assertions, unions, packageName, indent, depth + 1)
    } else {
        Pair("", statement.parameters)
    }

    val parametersInPython = parameters.stream()
            .map { oneLinerExpressionToPython(it, assertions, unions, packageName) }
            .collect(Collectors.joining(", "))

    val name = statement.defName
    return buildIndent(indent) + prefix + "\n" +
            buildIndent(indent) + if (statement.packageName == packageName) {
        "$name($parametersInPython)"
    } else {
        "${packageToPythonPackage(statement.packageName)}.$name($parametersInPython)"
    }
}

fun assertStatementWithType(statement: AssertStatementWithType,
                            assertions: Boolean,
                            unions: Set<UnionWithType>,
                            packageName: String,
                            indent: Int,
                            depth: Int): String =
        if (assertions) {
            val checkExpression = statement.checkExpression
            val (oneLinerCheckExpression, checkPrefix) = if (isOneLinerExpression(checkExpression, assertions)) {
                Pair(checkExpression, "")
            } else {
                val x = introduceNewVariable("_assert_stm_$depth", checkExpression, assertions, unions, packageName, indent, depth)
                Pair(x.first, buildIndent(indent) + x.second + "\n")
            }
            val messageExpression = statement.messageExpression
            val (oneLinerMessageExpression, messagePrefix) = if (messageExpression != null) {
                if (isOneLinerExpression(messageExpression, assertions)) {
                    Pair(messageExpression, "")
                } else {
                    val x = introduceNewVariable("_assert_message_stm_$depth", messageExpression, assertions, unions, packageName, indent, depth)
                    Pair(x.first, buildIndent(indent) + x.second + "\n")
                }
            } else {
                Pair(StringExpressionWithReturnType("<no-message>"), "")
            }

            val checkExpressionInPython = oneLinerExpressionToPython(oneLinerCheckExpression, assertions, unions, packageName)
            val messageExpressionInPython = oneLinerExpressionToPython(oneLinerMessageExpression, assertions, unions, packageName)
            checkPrefix + messagePrefix +
                    buildIndent(indent) + "if not " + checkExpressionInPython + ": raise AssertionError(" + messageExpressionInPython + ")"
        } else {
            ""
        }

private fun expressionLongOrShortToString(expression: ExpressionWithReturnType,
                                          expressionPrefix: String,
                                          assertions: Boolean,
                                          unions: Set<UnionWithType>,
                                          packageName: String,
                                          indent: Int,
                                          depth: Int): String =
        if (isOneLinerExpression(expression, assertions)) {
            expressionPrefix + oneLinerExpressionToPython(expression, assertions, unions, packageName)
        } else {
            when (expression) {
                is IfExpressionWithReturnType -> {
                    val (condition, prefix) = if (isOneLinerExpression(expression.condition, assertions)) {
                        Pair(
                                oneLinerExpressionToPython(expression.condition, assertions, unions, packageName),
                                ""
                        )
                    } else {
                        val conditionName = "_condition_$depth"
                        Pair(
                                conditionName,
                                expressionLongOrShortToString(
                                        expression.condition,
                                        "$conditionName = ",
                                        assertions, unions, packageName, indent, depth + 1) + "\n" + buildIndent(indent)
                        )
                    }
                    prefix + "if " + condition + ":\n" +
                            buildIndent(indent + 1) + ifBranchToPython(expression.trueBranch, expressionPrefix, assertions, unions, packageName, indent + 1, depth + 1) + "\n" +
                            buildIndent(indent) + "else:\n" +
                            buildIndent(indent + 1) + ifBranchToPython(expression.falseBranch, expressionPrefix, assertions, unions, packageName, indent + 1, depth + 1)
                }
                is LambdaExpressionWithReturnType -> {
                    val name = "_lambda_$depth"
                    val prefix = name + "_"
                    val valuesToRewrite =
                            concat(
                                    expression.parameters.stream().map { it.name },
                                    expression.assignments.stream().map { it.name }
                            ).toList().toSet()
                    val lambdaInPython = functionToPython(
                            name,
                            rewriteValNamesInExpression(expression.expression, prefix, valuesToRewrite),
                            expression.parameters.map { ParameterToExport(prefix + it.name, it.type) },
                            expression.assignments.map { assignment ->
                                AssigmentStatementWithType(
                                        prefix + assignment.name,
                                        rewriteValNamesInExpression(assignment.expression, prefix, valuesToRewrite),
                                        assignment.type)
                            },
                            assertions,
                            unions,
                            packageName,
                            indent,
                            depth + 1
                    )
                    val newExpression = ValueExpressionWithReturnType(expression.returnType, name)
                    lambdaInPython + "\n" +
                            buildIndent(indent) + expressionLongOrShortToString(newExpression, expressionPrefix, assertions, unions, packageName, indent, depth)
                }
                is AssertExpressionWithReturnType -> {
                    val messageExpression = expression.messageExpression
                    val checkExpression = expression.checkExpression
                    val returnExpression = expression.returnExpression
                    val (message, prefix) = if (messageExpression != null) {
                        if (isOneLinerExpression(messageExpression, assertions)) {
                            Pair(oneLinerExpressionToPython(messageExpression, assertions, unions, packageName), "")
                        } else {
                            val conditionName = "_assert_message_$depth"
                            Pair(
                                    conditionName,
                                    expressionLongOrShortToString(messageExpression, "$conditionName = ", assertions, unions, packageName, indent, depth + 1) + "\n" + buildIndent(indent)
                            )
                        }
                    } else {
                        Pair("\"<no message>\"", "")
                    }
                    val (checkExpressionInPython, prefix2) = if (isOneLinerExpression(checkExpression, assertions)) {
                        Pair(
                                expressionLongOrShortToString(checkExpression, "", assertions, unions, packageName, indent, depth + 1),
                                ""
                        )
                    } else {
                        val conditionName = "_check_expression_$depth"
                        Pair(
                                conditionName,
                                expressionLongOrShortToString(checkExpression, "$conditionName = ", assertions, unions, packageName, indent, depth + 1) + "\n" + buildIndent(indent)
                        )
                    }
                    prefix +
                            prefix2 +
                            "if not " + checkExpressionInPython + ": raise AssertionError($message)\n" +
                            buildIndent(indent) + expressionLongOrShortToString(returnExpression, expressionPrefix, assertions, unions, packageName, indent, depth + 1)
                }
                is InfixExpressionWithReturnType -> {
                    val leftName = "_left_$depth"
                    val rightName = "_right_$depth"
                    val leftExpression = expressionLongOrShortToString(expression.left, "$leftName = ", assertions, unions, packageName, indent, depth + 1)
                    val rightExpression = expressionLongOrShortToString(expression.right, "$rightName = ", assertions, unions, packageName, indent, depth + 1)
                    val infixExpresion = oneLinerExpressionToPython(
                            InfixExpressionWithReturnType(
                                    expression.returnType,
                                    expression.operation,
                                    ValueExpressionWithReturnType(expression.left.returnType, leftName),
                                    ValueExpressionWithReturnType(expression.right.returnType, rightName)),
                            assertions, unions, packageName)
                    leftExpression + "\n" +
                            buildIndent(indent) + rightExpression + "\n" +
                            buildIndent(indent) + expressionPrefix + infixExpresion
                }
                is NegateExpressionWithReturnType -> {
                    val (valExpression, longExpression) = introduceNewVariable("_negate_$depth", expression.negateExpression, assertions, unions, packageName, indent, depth)
                    longExpression + "\n" +
                            buildIndent(indent) + expressionLongOrShortToString(valExpression, expressionPrefix, assertions, unions, packageName, indent, depth)
                }
                is NewListExpressionWithReturnType -> {
                    val (prefix, elementNames) = mapListOfExpressions(expression.elements, assertions, unions, packageName, indent, depth)
                    val newExpression = NewListExpressionWithReturnType(expression.returnType, elementNames)
                    prefix + expressionLongOrShortToString(newExpression, expressionPrefix, assertions, unions, packageName, indent, depth)
                }
                is FunctionInvocationExpressionWithReturnType -> {
                    val functionInvocation = expression.functionInvocation
                    if (functionInvocation !is FunctionInvocationByExpressionWithReturnType) {
                        throw IllegalStateException("`expression.functionInvocation` should be of type `${FunctionInvocationByExpressionWithReturnType::class.java.simpleName}`, was `${functionInvocation.javaClass.simpleName}`")
                    }
                    val (valExpression, longExpression) = introduceNewVariable("_function_invocation_$depth", functionInvocation.expression, assertions, unions, packageName, indent, depth)
                    longExpression + "\n" +
                            buildIndent(indent) + expressionLongOrShortToString(valExpression, expressionPrefix, assertions, unions, packageName, indent, depth)
                }
                is DefInvocationExpressionWithReturnType -> {
                    val (prefix, elementNames) = mapListOfExpressions(expression.parameters, assertions, unions, packageName, indent, depth)
                    val newExpression = DefInvocationExpressionWithReturnType(expression.returnType, expression.packageName, expression.functionName, elementNames)
                    prefix + expressionLongOrShortToString(newExpression, expressionPrefix, assertions, unions, packageName, indent, depth)
                }
                else -> throw IllegalStateException("I don't know how to handle long expression of type `${expression.javaClass.simpleName}`")
            }
        }

private fun mapListOfExpressions(expressions: List<ExpressionWithReturnType>,
                                 assertions: Boolean,
                                 unions: Set<UnionWithType>,
                                 packageName: String,
                                 indent: Int,
                                 depth: Int): Pair<String, List<ValueExpressionWithReturnType>> {
    val elementNames = ArrayList<ValueExpressionWithReturnType>(expressions.size)
    var prefix = ""
    for (idx in expressions.indices) {
        val expression = expressions[idx]
        val (valExpression, longExpression) = introduceNewVariable("_def_inv_${depth}_$idx", expression, assertions, unions, packageName, indent, depth)
        prefix += longExpression + "\n" + buildIndent(indent)
        elementNames.add(valExpression)
    }
    return Pair(prefix, elementNames)
}

private fun introduceNewVariable(valName: String,
                                 longExpression: ExpressionWithReturnType,
                                 assertions: Boolean,
                                 unions: Set<UnionWithType>,
                                 packageName: String,
                                 indent: Int,
                                 depth: Int): Pair<ValueExpressionWithReturnType, String> {
    val newExpression = expressionLongOrShortToString(longExpression, "$valName = ", assertions, unions, packageName, indent, depth + 1)
    val valueExpression = ValueExpressionWithReturnType(longExpression.returnType, valName)
    return Pair(valueExpression, newExpression)
}

fun ifBranchToPython(
        branch: IfBranchWithReturnType,
        expressionPrefix: String,
        assertions: Boolean,
        unions: Set<UnionWithType>,
        packageName: String,
        indent: Int,
        depth: Int,
): String {
    val assignments = branch.assignments
            .stream()
            .map { expressionLongOrShortToString(it.expression, it.name + " = ", assertions, unions, packageName, indent, depth + 1) }
            .map { it + "\n" }
            .map { it + buildIndent(indent) }
            .collect(Collectors.joining())
    val lastExpression = expressionLongOrShortToString(branch.expression, expressionPrefix, assertions, unions, packageName, indent, depth + 1)
    return assignments + lastExpression
}

private fun rewriteValNamesInExpression(expression: ExpressionWithReturnType, prefix: String, valuesToRewrite: Set<String>): ExpressionWithReturnType =
        when (expression) {
            is ValueExpressionWithReturnType -> if (valuesToRewrite.contains(expression.valueName)) {
                ValueExpressionWithReturnType(expression.returnType, prefix + expression.valueName)
            } else {
                expression
            }
            is ParameterExpressionWithReturnType -> if (valuesToRewrite.contains(expression.valueName)) {
                ParameterExpressionWithReturnType(expression.returnType, prefix + expression.valueName)
            } else {
                expression
            }
            is LambdaExpressionWithReturnType,
            is IntegerExpressionWithReturnType,
            is FloatExpressionWithReturnType,
            is BooleanExpressionWithReturnType,
            is StringExpressionWithReturnType,
            NothingExpressionWithReturnType,
            is NewStructExpressionWithReturnType,
            is IsExpressionWithReturnType -> expression
            is FunctionInvocationExpressionWithReturnType ->
                FunctionInvocationExpressionWithReturnType(
                        expression.returnType,
                        when (expression.functionInvocation) {
                            is FunctionInvocationByNameWithReturnType -> expression.functionInvocation
                            is FunctionInvocationByExpressionWithReturnType ->
                                FunctionInvocationByExpressionWithReturnType(
                                        rewriteValNamesInExpression(expression.functionInvocation.expression, prefix, valuesToRewrite)
                                )
                        },
                        expression.parameters
                                .stream()
                                .map { rewriteValNamesInExpression(it, prefix, valuesToRewrite) }
                                .toList()
                )
            is DefInvocationExpressionWithReturnType ->
                DefInvocationExpressionWithReturnType(
                        expression.returnType,
                        expression.packageName,
                        expression.functionName,
                        expression.parameters
                                .stream()
                                .map { rewriteValNamesInExpression(it, prefix, valuesToRewrite) }
                                .toList()
                )
            is InfixExpressionWithReturnType ->
                InfixExpressionWithReturnType(
                        expression.returnType,
                        expression.operation,
                        rewriteValNamesInExpression(expression.left, prefix, valuesToRewrite),
                        rewriteValNamesInExpression(expression.right, prefix, valuesToRewrite),
                )
            is IfExpressionWithReturnType ->
                IfExpressionWithReturnType(
                        expression.returnType,
                        rewriteValNamesInExpression(expression.condition, prefix, valuesToRewrite),
                        IfBranchWithReturnType(
                                rewriteValNamesInExpression(expression.trueBranch.expression, prefix, valuesToRewrite),
                                expression.trueBranch
                                        .assignments
                                        .stream()
                                        .map { assignment ->
                                            AssigmentStatementWithType(
                                                    prefix + assignment.name,
                                                    rewriteValNamesInExpression(assignment.expression, prefix, valuesToRewrite),
                                                    assignment.type)
                                        }
                                        .toList()
                        ),
                        IfBranchWithReturnType(
                                rewriteValNamesInExpression(expression.falseBranch.expression, prefix, valuesToRewrite),
                                expression.falseBranch
                                        .assignments
                                        .stream()
                                        .map { assignment ->
                                            AssigmentStatementWithType(
                                                    prefix + assignment.name,
                                                    rewriteValNamesInExpression(assignment.expression, prefix, valuesToRewrite),
                                                    assignment.type)
                                        }
                                        .toList()
                        )
                )
            is NegateExpressionWithReturnType -> NegateExpressionWithReturnType(rewriteValNamesInExpression(expression.negateExpression, prefix, valuesToRewrite))
            is StructFieldAccessExpressionWithReturnType ->
                StructFieldAccessExpressionWithReturnType(
                        rewriteValNamesInExpression(expression.structureExpression, prefix, valuesToRewrite),
                        expression.fieldName,
                        expression.fieldType
                )
            is NewListExpressionWithReturnType ->
                NewListExpressionWithReturnType(
                        expression.returnType,
                        expression.elements
                                .map { rewriteValNamesInExpression(it, prefix, valuesToRewrite) }
                )
            is AssertExpressionWithReturnType ->
                AssertExpressionWithReturnType(
                        rewriteValNamesInExpression(expression.checkExpression, prefix, valuesToRewrite),
                        rewriteValNamesInExpression(expression.returnExpression, prefix, valuesToRewrite),
                        if (expression.messageExpression != null) rewriteValNamesInExpression(expression.messageExpression, prefix, valuesToRewrite) else null
                )
            is StructureAccessExpressionWithReturnType ->
                StructureAccessExpressionWithReturnType(
                        expression.returnType,
                        expression.structureType,
                        expression.structureName,
                        rewriteValNamesInExpression(expression.structureIndex, prefix, valuesToRewrite)
                )
        }

private fun oneLinerExpressionToPython(expression: ExpressionWithReturnType,
                                       assertions: Boolean,
                                       unions: Set<UnionWithType>,
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
                        packageName)
            is InfixExpressionWithReturnType -> "(${oneLinerExpressionToPython(expression.left, assertions, unions, packageName)}) ${mapInfixOperator(expression)} (${oneLinerExpressionToPython(expression.right, assertions, unions, packageName)})"
            is IfExpressionWithReturnType -> "(${oneLinerExpressionToPython(expression.trueBranch.expression, assertions, unions, packageName)}) if (${oneLinerExpressionToPython(expression.condition, assertions, unions, packageName)}) else (${oneLinerExpressionToPython(expression.falseBranch.expression, assertions, unions, packageName)})"
            is NegateExpressionWithReturnType -> "not ${oneLinerExpressionToPython(expression.negateExpression, assertions, unions, packageName)}"
            is NewStructExpressionWithReturnType -> {
                val parameters = expression.fields
                        .stream()
                        .map { (name, expression) -> Pair(name, oneLinerExpressionToPython(expression, assertions, unions, packageName)) }
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
                val valuesToRewrite =
                        concat(
                                expression.parameters.stream().map { it.name },
                                expression.assignments.stream().map { it.name }
                        ).toList().toSet()
                val exp = rewriteValNamesInExpression(expression.expression, prefix, valuesToRewrite)
                "(lambda $parameters: " + oneLinerExpressionToPython(exp, assertions, unions, packageName) + ")"
            }
            is NewListExpressionWithReturnType -> expression.elements
                    .stream()
                    .map { oneLinerExpressionToPython(it, assertions, unions, packageName) }
                    .collect(Collectors.joining(", ", "[", "]"))
            is StructureAccessExpressionWithReturnType -> {
                "${expression.structureName}[${oneLinerExpressionToPython(expression.structureIndex, assertions, unions, packageName)}]"
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
            is StructFieldAccessExpressionWithReturnType -> "${oneLinerExpressionToPython(expression.structureExpression, assertions, unions, packageName)}.${expression.fieldName}"
            is AssertExpressionWithReturnType -> {
                oneLinerExpressionToPython(expression.returnExpression, assertions, unions, packageName)
            }
        }

private fun methodToString(
        functionInvocation: FunctionInvocationWithReturnType,
        expressionParameters: List<ExpressionWithReturnType>,
        assertions: Boolean,
        unions: Set<UnionWithType>,
        packageName: String): String {
    val parameters = expressionParameters
            .stream()
            .map { oneLinerExpressionToPython(it, assertions, unions, packageName) }
            .collect(Collectors.joining(", "))
    return when (functionInvocation) {
        is FunctionInvocationByNameWithReturnType -> {
            val name = functionInvocation.functionName
            if (functionInvocation.packageName == packageName) {
                "$name($parameters)"
            } else {
                "${packageToPythonPackage(functionInvocation.packageName)}.$name($parameters)"
            }
        }
        is FunctionInvocationByExpressionWithReturnType -> {
            val lambda = oneLinerExpressionToPython(functionInvocation.expression, assertions, unions, packageName)
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
            is LambdaExpressionWithReturnType -> expression.assignments.isEmpty() && isOneLinerExpression(expression.expression, assertions)
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
