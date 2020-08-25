package com.magx2.capybara

import com.google.gson.GsonBuilder
import java.util.function.BiConsumer
import java.util.function.BinaryOperator
import java.util.function.Function
import java.util.function.Supplier
import java.util.stream.Collector
import java.util.stream.Collector.Characteristics.UNORDERED
import kotlin.streams.toList

fun main(args: Array<String>) {
    val compiler = CapybaraCompiler.instance()
    val compileUnits = args.asSequence()
            .map { compiler.compile(it) }
            .toList()

    // roll out spread filed in structs
    val fullyQualifiedStructNames: Map<String, Struct> = findFullyQualifiedStructNames(compileUnits)

    val compileUnitsWithFlatStructs = compileUnits.stream()
            .map { unit ->
                Pair(
                        unit,
                        unit.structs
                                .stream()
                                .map { struct ->
                                    addUnrollSpreadFieldsInStruct(unit.packageName, struct, fullyQualifiedStructNames)
                                }
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
                Pair(function, findReturnType(compilationContext, compileUnit, function.assignments, function.returnExpression))
            }
            .map { pair ->
                val returnType = pair.first.returnType
                if (returnType != null && parseType(returnType, pair.first.packageName) != pair.second) {
                    throw CompilationException("Declared return type of function do not corresponds what it really return. " +
                            "You declared `$returnType` and computed was `${pair.second}`.")
                }
                pair
            }
            .map { pair ->
                val defaultPackage = pair.first.packageName
                val parameters = pair.first.parameters
                        .stream()
                        .map { TypedParameter(it.name, parseType(it.type, defaultPackage)) }
                        .toList()
                FunctionWithReturnType(
                        pair.first.packageName,
                        pair.first.name,
                        pair.second,
                        parameters,
                        pair.first.assignments,
                        pair.first.returnExpression)
            }
            .toList()
    printlnAny("Functions:", functionsWithReturnType)

    //
    // DEFINITIONS
    //
    printlnAny("Definitions:", compileUnits.stream().flatMap { it.defs.stream() }.toList())
}

data class CompilationContext(val structs: Set<FlatStruct>, val functions: Set<com.magx2.capybara.Function>)

class CompilationException(msg: String) : RuntimeException(msg)

fun printlnAny(header: String?, any: Any) {
    val gson = GsonBuilder().setPrettyPrinting().create()
    val json = gson.toJson(any)
    if (header != null) {
        println(header)
    }
    println(json)
}
