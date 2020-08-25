package com.magx2.capybara

import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.stringType
import java.util.*
import java.util.stream.Collectors
import kotlin.streams.toList

data class Function(val packageName: String,
                    val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val assignments: Set<AssigmentStatement>,
                    val returnExpression: Expression)

data class FunctionWithReturnType(val packageName: String,
                                  val name: String,
                                  val returnType: Type,
                                  val parameters: List<TypedParameter>,
                                  val assignments: Set<AssigmentStatement>,
                                  val returnExpression: Expression)

fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        assignments: Set<AssigmentStatement>,
        expression: Expression): Type =
        when (expression) {
            is ParenthesisExpression -> findReturnType(compilationContext, compileUnit, assignments, expression.expression)
            is ParameterExpression -> parseType(expression.type, compileUnit.packageName)
            is IntegerExpression -> intType
            is BooleanExpression -> booleanType
            is StringExpression -> stringType
            is FunctionInvocationExpression -> {
                val function = findFunctionForGivenFunctionInvocation(compilationContext, compileUnit, assignments, expression)
                if (function.isPresent) {
                    val f = function.get()
                    if (f.returnType != null) {
                        parseType(f.returnType, compileUnit.packageName)
                    } else {
                        findReturnType(compilationContext, compileUnit, assignments, f.returnExpression)
                    }
                } else {
                    val parameters = findFunctionParametersValues(compilationContext, compileUnit, assignments, expression)
                            .stream()
                            .map { "${it.packageName}/${it.name}" }
                            .collect(Collectors.joining(", "))
                    throw CompilationException("Cant find method with signature: " +
                            "`${expression.packageName ?: compileUnit.packageName}:${expression.functionName}($parameters)`")
                }
            }
            is InfixExpression -> findReturnType(compilationContext, compileUnit, assignments, expression)
            is IfExpression -> {
                val ifReturnType = findReturnType(compilationContext, compileUnit, assignments, expression.condition)
                if (ifReturnType != booleanType) {
                    throw CompilationException("Expression in `if` should return `$booleanType` not `$ifReturnType`")
                }
                findReturnTypeFromBranchExpression(compilationContext, compileUnit, assignments, expression.trueBranch, expression.falseBranch)
            }
            is NegateExpression -> {
                val returnType = findReturnType(compilationContext, compileUnit, assignments, expression.negateExpression)
                if (returnType != booleanType) {
                    throw CompilationException("You can only negate boolean expressions. Type `$returnType` cannot be negated.")
                }
                returnType
            }
            is ValueExpression -> {
                assignments.stream()
                        .filter { it.name == expression.valueName }
                        .map { it.expression }
                        .map { findReturnType(compilationContext, compileUnit, assignments, it) }
                        .findAny()
                        .orElseThrow { CompilationException("There is no value with name `${expression.valueName}`") }
            }
            is NewStruct -> {
                val structPackageName = expression.packageName ?: compileUnit.packageName
                val structs = if (expression.packageName == null) {
                    compileUnit.structs + compileUnit.importStructs
                } else {
                    compilationContext.structs
                }

                val optional = structs.stream()
                        .filter { it.name == expression.structName }
                        .filter { it.packageName == structPackageName }
                        .findFirst()

                if (optional.isPresent) {
                    val struct = optional.get()
                    val fieldsInExpression = expression.fields
                            .stream()
                            .map { it.name }
                            .toList()
                            .toSet()
                    val missingFields = struct.fields
                            .stream()
                            .filter { fieldsInExpression.contains(it.name).not() }
                            .toList()
                    if (missingFields.isNotEmpty()) {
                        val fields = missingFields.stream()
                                .map { it.name }
                                .collect(Collectors.joining(", "))
                        throw CompilationException("You are missing [$fields] fields when creating new " +
                                "`${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                    }
                    val nameStructFields = struct.fields
                            .stream()
                            .map { it.name }
                            .toList()
                            .toSet()
                    val additionalFields = fieldsInExpression.stream()
                            .filter { nameStructFields.contains(it).not() }
                            .toList()
                    if (additionalFields.isNotEmpty()) {
                        val notRecognizedFields = additionalFields.joinToString(", ")
                        throw CompilationException("I do not recognize those fields [$notRecognizedFields] in struct " +
                                "`${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                    }
                    val declaredTypes = expression.fields
                            .stream()
                            .map { Pair(it.name, findReturnType(compilationContext, compileUnit, assignments, it.value)) }
                            .collect(Collectors.toMap(
                                    (java.util.function.Function<Pair<String, Type>, String> { it.first }),
                                    (java.util.function.Function<Pair<String, Type>, Type> { it.second })
                            ))
                    val wrongTypes = struct.fields
                            .stream()
                            .filter { declaredTypes[it.name]!! != it.type }
                            .map { Triple(it.name, it.type, declaredTypes[it.name]!!) }
                            .map {
                                "field name: `${it.first}`, " +
                                        "expected type: `${typeToString(it.second)}`, " +
                                        "actual type: `${typeToString(it.third)}`"
                            }
                            .collect(Collectors.joining(", "))
                    if (wrongTypes.isNotBlank()) {
                        throw CompilationException("Cannot create struct. Given arguments has bad types: [$wrongTypes]")
                    }
                    Type(struct.packageName, struct.name)
                } else {
                    throw CompilationException("Cannot find struct `${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                }
            }
        }

fun findFunctionForGivenFunctionInvocation(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        assignments: Set<AssigmentStatement>,
        function: FunctionInvocationExpression): Optional<Function> {
    val functions = compileUnit.functions + compileUnit.importFunctions
    val parameters = findFunctionParametersValues(compilationContext, compileUnit, assignments, function)
    return functions.stream()
            .filter { it.name == function.functionName }
            .filter { function.packageName == null }
            .filter { it.parameters.size == parameters.size }
            .filter { f ->
                var i = 0
                var equals = true
                while (i < parameters.size && equals) {
                    equals = parseType(f.parameters[i].type, compileUnit.packageName) == parameters[i]
                    i++
                }
                equals
            }
            .findFirst()
            .or {
                compilationContext.functions
                        .stream()
                        .filter { it.name == function.functionName }
                        .filter { it.packageName == function.packageName }
                        .filter { f ->
                            var i = 0
                            var equals = true
                            while (i < parameters.size && equals) {
                                equals = parseType(f.parameters[i].type, compileUnit.packageName) == parameters[i]
                                i++
                            }
                            equals
                        }
                        .findFirst()
            }
}

fun findFunctionParametersValues(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        assignments: Set<AssigmentStatement>,
        function: FunctionInvocationExpression): List<Type> =
        function.parameters
                .stream()
                .map { findReturnType(compilationContext, compileUnit, assignments, it) }
                .toList<Type>()

fun findReturnTypeFromBranchExpression(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        assignments: Set<AssigmentStatement>,
        left: Expression,
        right: Expression): Type {
    val leftType = findReturnType(compilationContext, compileUnit, assignments, left)
    val rightType = findReturnType(compilationContext, compileUnit, assignments, right)
    return if (leftType == rightType) {
        leftType
    } else {
        if (leftType == stringType || rightType == stringType) {
            val notStringType = if (leftType == stringType) rightType else leftType
            if (notStringType == intType || notStringType == booleanType) {
                stringType
            } else {
                throw CompilationException("Cannot convert type `$notStringType` to `$stringType` automatically.")
            }
        } else {
            throw CompilationException("You need to return same types from infix expression. " +
                    "Left expression returns `$leftType`, " +
                    "right expression returns `$rightType`")
        }
    }
}

fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        assignments: Set<AssigmentStatement>,
        expression: InfixExpression): Type =
        when (expression.operation) {
            ">", "<", ">=", "<=", "!=", "==", "&&", "||" -> booleanType
            "^", "*", "+", "-" -> findReturnTypeFromBranchExpression(compilationContext, compileUnit, assignments, expression.left, expression.right)
            else -> throw CompilationException("Do not know this `${expression.operation}` infix expression!")
        }
