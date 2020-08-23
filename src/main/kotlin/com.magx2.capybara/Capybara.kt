package com.magx2.capybara

import com.google.gson.GsonBuilder
import java.util.*
import java.util.function.BiConsumer
import java.util.function.BinaryOperator
import java.util.function.Function
import java.util.function.Supplier
import java.util.stream.Collector
import java.util.stream.Collector.Characteristics.UNORDERED
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.collections.HashMap
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

    val compileUnitsWithFlatStructs = compileUnits.stream()
            .map { unit ->
                Pair(
                        unit,
                        unit.structs
                                .stream()
                                .map { struct -> addUnrollSpreadFieldsInStruct(struct, fullyQualifiedStructNames) }
                                .toList()
                )
            }
            .map { (unit, flatStructs) ->
                CompileUnitWithFlatStructs(
                        unit.packageName,
                        unit.imports,
                        flatStructs,
                        unit.functions)
            }
            .toList()

    printlnAny("Unrolled structs:", compileUnitsWithFlatStructs.stream().flatMap { it.structs.stream() }.toList())

    //
    // EXPORTS/IMPORTS
    //
    val exports = compileUnitsWithFlatStructs.stream()
            .map { Export(it.packageName, it.structs.toSet(), it.functions.toSet()) }
            .collect(object : Collector<Export, MutableMap<String, Export>, Map<String, Export>> {
                override fun characteristics(): MutableSet<Collector.Characteristics> = setOf(UNORDERED).toMutableSet()

                override fun supplier(): Supplier<MutableMap<String, Export>> =
                        Supplier { HashMap<String, Export>() }

                override fun accumulator(): BiConsumer<MutableMap<String, Export>, Export> =
                        BiConsumer { map, export ->
                            val exportFromMap = map[export.packageName]
                            if (exportFromMap == null) {
                                map[export.packageName] = export
                            } else {
                                Export(
                                        export.packageName,
                                        export.structs + exportFromMap.structs,
                                        export.functions + exportFromMap.functions)
                            }
                        }

                override fun combiner(): BinaryOperator<MutableMap<String, Export>> =
                        BinaryOperator { map1, map2 ->
                            (map1 + map2).toMutableMap()
                        }

                override fun finisher(): Function<MutableMap<String, Export>, Map<String, Export>> = Function { it.toMap() }
            })
    val compileUnitsWithImports = compileUnitsWithFlatStructs.stream()
            .map { compileUnit ->
                val imports = compileUnit.imports.stream()
                        .map { import ->
                            val export = (exports[import.importPackage]
                                    ?: throw CompilationException("Package `${import.importPackage}` exports nothing so you can't import it."))
                            Pair(import, export)
                        }
                        .toList()
                CompileUnitWithImports(
                        compileUnit.packageName,
                        compileUnit.structs,
                        compileUnit.functions,
                        imports.stream()
                                .flatMap { (import, exportForPackage) ->
                                    if (import.subImport.isEmpty()) {
                                        exportForPackage.structs.stream()
                                    } else {
                                        exportForPackage.structs
                                                .stream()
                                                .filter { import.subImport.contains(it.name) }
                                    }
                                }
                                .toList()
                                .toSet(),
                        imports.stream()
                                .flatMap { (import, exportForPackage) ->
                                    if (import.subImport.isEmpty()) {
                                        exportForPackage.functions.stream()
                                    } else {
                                        exportForPackage.functions
                                                .stream()
                                                .filter { import.subImport.contains(it.name) }
                                    }
                                }
                                .toList()
                                .toSet()
                )
            }
            .toList()

    //
    // FUNCTIONS
    //
    val functions = compileUnitsWithImports.stream()
            .flatMap { compileUnit ->
                compileUnit.functions.stream()
                        .map { function -> Pair(compileUnit, function) }
            }
            .toList()


    //
    // COMPILATION
    //
    val compilationContext = CompilationContext(
            compileUnitsWithFlatStructs.stream()
                    .flatMap { it.structs.stream() }
                    .toList()
                    .toSet(),
            functions.stream()
                    .map { it.second }
                    .toList()
                    .toSet())

    // find return types for functions
    val functionsWithReturnType = functions.stream()
            .map { (compileUnit, function) ->
                Pair(function, findReturnType(compilationContext, compileUnit, function.returnExpression))
            }
            .map { pair ->
                if (pair.first.returnType != null && pair.first.returnType != pair.second) {
                    throw CompilationException("Declared return type of function do not corresponds what it really return. " +
                            "You declared `${pair.first.returnType}` and computed was `${pair.second}`.")
                }
                pair
            }
            .map { pair ->
                FunctionWithReturnType(
                        pair.first.packageName,
                        pair.first.name,
                        pair.second,
                        pair.first.parameters,
                        pair.first.returnExpression)
            }
            .toList()
    printlnAny("Functions:", functionsWithReturnType)
}

data class CompilationContext(val structs: Set<FlatStruct>, val functions: Set<com.magx2.capybara.Function>)

private fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        expression: Expression): String =
        when (expression) {
            is ParenthesisExpression -> findReturnType(compilationContext, compileUnit, expression.expression)
            is ParameterExpression -> expression.type
            is IntegerExpression -> intType
            is BooleanExpression -> booleanType
            is StringExpression -> stringType
            is FunctionInvocationExpression -> {
                val function = findFunctionForGivenFunctionInvocation(compilationContext, compileUnit, expression)
                if (function.isPresent) {
                    val f = function.get()
                    f.returnType ?: findReturnType(compilationContext, compileUnit, f.returnExpression)
                } else {
                    val parameters = findFunctionParametersValues(compilationContext, compileUnit, expression)
                            .stream()
                            .collect(Collectors.joining(", "))
                    throw CompilationException("Cant find method with signature: " +
                            "`${expression.packageName ?: compileUnit.packageName}:${expression.functionName}($parameters)`")
                }
            }
            is InfixExpression -> findReturnType(compilationContext, compileUnit, expression)
            is IfExpression -> {
                val ifReturnType = findReturnType(compilationContext, compileUnit, expression.condition)
                if (ifReturnType != booleanType) {
                    throw CompilationException("Expression in `if` should return `$booleanType` not `$ifReturnType`")
                }
                findReturnTypeFromBranchExpression(compilationContext, compileUnit, expression.trueBranch, expression.falseBranch)
            }
            is NegateExpression -> {
                val returnType = findReturnType(compilationContext, compileUnit, expression.negateExpression)
                if (returnType != booleanType) {
                    throw CompilationException("You can only negate boolean expressions. Type `$returnType` cannot be negated.")
                }
                returnType
            }
            is NewStruct -> {
                val structPackageName = expression.packageName ?: compileUnit.packageName
                val structs = if (expression.packageName == null) {
                    compileUnit.structs + compileUnit.importStructs
                } else {
                    compilationContext.structs
                }

                val struct = structs.stream()
                        .filter { it.name == expression.structName }
                        .filter { it.packageName == structPackageName }
                        .findFirst()

                if (struct.isPresent) {
                    // TODO add checking if every filed in stuct is set to correct type
                    struct.get().packageName + "/" + struct.get().name
                } else {
                    throw CompilationException("Cannot find struct `${expression.packageName ?: compileUnit.packageName}:${expression.structName}`")
                }
            }
        }

private fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        expression: InfixExpression): String =
        when (expression.operation) {
            ">", "<", ">=", "<=", "!=", "==", "&&", "||" -> booleanType
            "^", "*", "+", "-" -> findReturnTypeFromBranchExpression(compilationContext, compileUnit, expression.left, expression.right)
            else -> throw CompilationException("Do not know this `${expression.operation}` infix expression!")
        }

private fun findFunctionForGivenFunctionInvocation(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        function: FunctionInvocationExpression): Optional<com.magx2.capybara.Function> {
    val functions = compileUnit.functions + compileUnit.importFunctions
    val parameters = findFunctionParametersValues(compilationContext, compileUnit, function)
    return functions.stream()
            .filter { it.name == function.functionName }
            .filter { function.packageName == null }
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
                                equals = f.parameters[i].type == parameters[i]
                                i++
                            }
                            equals
                        }
                        .findFirst()
            }
}

private fun findFunctionParametersValues(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        function: FunctionInvocationExpression): List<String> =
        function.parameters
                .stream()
                .map { findReturnType(compilationContext, compileUnit, it) }
                .toList<String>()

private fun findReturnTypeFromBranchExpression(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithImports,
        left: Expression,
        right: Expression): String {
    val leftType = findReturnType(compilationContext, compileUnit, left)
    val rightType = findReturnType(compilationContext, compileUnit, right)
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