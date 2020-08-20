package com.magx2.capybara

import com.google.gson.GsonBuilder
import java.util.*
import java.util.function.Function
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

fun main(args: Array<String>) {
    val compiler = CapybaraCompiler.instance()
    val compileUnits = args.asSequence()
            .map { compiler.compile(it) }
            .toList()

    val structs = compileUnits.stream()
            .flatMap { it.structs.stream() }
            .toList()

    // roll out spread filed in structs
    val fullyQualifiedStructNames: Map<String, Struct> = compileUnits.stream()
            .flatMap { it.structs.stream() }
            .map { struct -> Pair("${struct.packageName}/${struct.name}", struct) }
            .collect(Collectors.toMap(
                    (Function<Pair<String, Struct>, String> { it.first }),
                    (Function<Pair<String, Struct>, Struct> { it.second })
            ))

    val flatStructs = structs.stream()
            .map { struct ->
                addUnrollSpreadFieldsInStruct(struct, fullyQualifiedStructNames)
            }
            .toList()

    printlnAny("Unrolled structs:", flatStructs)

    //
    // FUNCTIONS
    //
    val functions = compileUnits.stream()
            .flatMap { it.functions.stream() }
            .toList()

    //
    // COMPILATION
    //

    // find return types for functions
    val functionsWithReturnType = functions.stream()
            .map { Pair(it, findReturnType(it.returnExpression, functions)) }
            .map { pair ->
                if (pair.first.returnType != null && pair.first.returnType != pair.second) {
                    throw CompilationException("Declared return type of function do not corresponds what it really return. " +
                            "You declared `${pair.first.returnType}` and computed was `${pair.second}`.")
                }
                pair
            }
            .map { pair ->
                FunctionWithReturnType(
                        pair.first.name,
                        pair.second,
                        pair.first.parameters,
                        pair.first.returnExpression)
            }
            .toList()
    printlnAny("Functions:", functionsWithReturnType)
}

private fun findReturnType(expression: Expression, functions: List<com.magx2.capybara.Function>): String =
        when (expression) {
            is ParenthesisExpression -> findReturnType(expression.expression, functions)
            is ParameterExpression -> expression.type
            is IntegerExpression -> intType
            is BooleanExpression -> booleanType
            is StringExpression -> stringType
            is FunctionInvocationExpression -> {
                val function = findFunctionForGivenFunctionInvocation(expression, functions)
                if (function.isPresent) {
                    val f = function.get()
                    f.returnType ?: findReturnType(f.returnExpression, functions)
                } else {
                    val parameters = findFunctionParametersValues(expression, functions)
                            .stream()
                            .collect(Collectors.joining(", "))
                    throw CompilationException("Cant find method with signature: " +
                            "`${expression.functionName}($parameters)`")
                }
            }
            is InfixExpression -> findReturnType(expression, functions)
            is IfExpression -> {
                val ifReturnType = findReturnType(expression.condition, functions)
                if (ifReturnType != booleanType) {
                    throw CompilationException("Expression in `if` should return `$booleanType` not `$ifReturnType`")
                }
                findReturnTypeFromBranchExpression(expression.trueBranch, expression.falseBranch, functions)
            }
        }

private fun findReturnType(expression: InfixExpression, functions: List<com.magx2.capybara.Function>): String =
        when (expression.operation) {
            ">", "<", ">=", "<=", "!=", "==" -> booleanType
            "^", "*", "+", "-" -> findReturnTypeFromBranchExpression(expression.left, expression.right, functions)
            else -> throw CompilationException("Do not know this `${expression.operation}` infix expression!")
        }

private fun findFunctionForGivenFunctionInvocation(
        function: FunctionInvocationExpression,
        functions: List<com.magx2.capybara.Function>): Optional<com.magx2.capybara.Function> {
    val parameters = findFunctionParametersValues(function, functions)
    return functions.stream()
            .filter { it.name == function.functionName }
            .filter { it.parameters.size == parameters.size }
            .filter { f ->
                var i = 0
                var equals = true
                while (i < parameters.size && equals) {
                    equals = f.parameters[i].type == parameters[i]
                    i++
                }
                equals
            }
            .findAny()
}

private fun findFunctionParametersValues(function: FunctionInvocationExpression, functions: List<com.magx2.capybara.Function>): List<String> =
        function.parameters
                .stream()
                .map { findReturnType(it, functions) }
                .toList<String>()

private fun findReturnTypeFromBranchExpression(left: Expression, right: Expression, functions: List<com.magx2.capybara.Function>): String {
    val leftType = findReturnType(left, functions)
    val rightType = findReturnType(right, functions)
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

const val intType = "/capybara/type/Int"
const val booleanType = "/capybara/type/Boolean"
const val stringType = "/capybara/type/String"

class CompilationException(msg: String) : RuntimeException(msg)

private fun printlnAny(header: String?, any: Any) {
    val gson = GsonBuilder().setPrettyPrinting().create()
    val json = gson.toJson(any)
    if (header != null) {
        println(header)
    }
    println(json)
}

private fun addUnrollSpreadFieldsInStruct(struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): FlatStruct {
    val newFields = struct.fields
            .stream()
            .flatMap { x(it, struct, fullyQualifiedStructNames) }
            .toList()
    return FlatStruct(
            struct.packageName,
            struct.name,
            newFields)
}

private fun x(field: Field, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): Stream<BasicField> =
        when (field) {
            is BasicField -> Stream.of(field)
            is SpreadField -> {
                mapSpreadFieldToBasicField(field, struct, fullyQualifiedStructNames)
            }
        }

private fun mapSpreadFieldToBasicField(spreadField: SpreadField, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): Stream<BasicField> {
    val spreadType = if (spreadField.spreadType.startsWith("/")) {
        spreadField.spreadType
    } else {
        "${struct.packageName}/${spreadField.spreadType}"
    }
    val spreadStruct = fullyQualifiedStructNames[spreadType]
            ?: throw IllegalStateException("Cannot find struct with name `${spreadField.spreadType}`")
    return spreadStruct.fields.stream().flatMap { x(it, struct, fullyQualifiedStructNames) }
}