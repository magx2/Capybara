package com.magx2.capybara

import org.antlr.v4.runtime.Token
import org.apache.commons.cli.HelpFormatter
import org.slf4j.LoggerFactory
import java.io.File
import java.util.function.BiConsumer
import java.util.function.BinaryOperator
import java.util.function.Function
import java.util.function.Supplier
import java.util.stream.Collector
import java.util.stream.Collector.Characteristics.UNORDERED
import java.util.stream.Collectors
import kotlin.streams.toList

private val log = LoggerFactory.getLogger(Capybara::class.java)

private class Capybara

fun main(args: Array<String>) {
    val options = parseCommandLine(args)
    if (options.debug) {
        log.info("Debug is on")
        log.info(options.toString())
    }
    if (options.filesToCompile.isEmpty()) {
        log.warn("Nothing to compile; exit")
        printHelp()
        return
    }
    if (args.isEmpty() || options.help) {
        printHelp()
        return
    }
    try {
        main(options)
    } catch (e: CompilationException) {
        if (options.debug) log.error("Compilation exception", e)
        System.err.println("Compilation exception: " + e.message)
    } catch (e: Exception) {
        if (options.debug) log.error("Generic exception", e)
        System.err.println("Generic exception: " + e.message)
    }
}

private fun printHelp() {
    val formatter = HelpFormatter()
    formatter.printHelp("capybara", buildOptions())
}

fun main(options: CommandLineOptions) {
    if (options.clearOutput && options.outputDir != null) {
        log.info("Clearing output dir `${options.outputDir}`")
        (File(options.outputDir).listFiles() ?: arrayOf())
                .asList()
                .forEach { it.deleteRecursively() }
    }

    val compiler = CapybaraCompiler.instance()
    val compileUnits = options.filesToCompile
            .stream()
            .map { compiler.compile(it) }
            .toList()

    //
    // EXPORTS/IMPORTS
    //
    val exports = compileUnits.stream()
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
            }) + (basicTypesExport.packageName to basicTypesExport)
    val compileUnitsWithImports = compileUnits.stream()
            .map { compileUnit ->
                val imports = compileUnit.imports.stream()
                        .map { import ->
                            val export = exports[import.importPackage]
                            if (export == null) {
                                val availableImports = exports.keys
                                        .stream()
                                        .collect(Collectors.joining(", ", "[", "]"))
                                throw CompilationException(import.codeMetainfo, "Package `${import.importPackage}` exports nothing so you can't import it. " +
                                        "Available packages to import: $availableImports")
                            }
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
                                .toList(),
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
                )
            }
            .toList()

    //
    // STRUCTS
    //

    // roll out spread filed in structs
    val fullyQualifiedStructNames: Map<String, Struct> = findFullyQualifiedStructNames(compileUnits)

    val compileUnitsWithFlatStructs = compileUnitsWithImports.stream()
            .map { unit ->
                Pair(
                        unit,
                        unit.structs
                                .stream()
                                .map { struct ->
                                    addUnrollSpreadFieldsInStruct(
                                            unit.structs,
                                            unit.importStructs,
                                            struct,
                                            fullyQualifiedStructNames)
                                }
                                .toList()
                                .toSet()
                )
            }
            .map { (unit, flatStructs) ->
                CompileUnitWithFlatStructs(
                        unit.packageName,
                        flatStructs,
                        unit.functions,
                        unit.importStructs,
                        unit.importFunctions
                )
            }
            .toList()

    //
    // FUNCTIONS
    //
    val functions = compileUnitsWithFlatStructs.stream()
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
    val compilationUnitsToExport = compileUnitsWithFlatStructs.stream()
            .map { unit ->
                val functionsWithReturnType = functions.stream()
                        .map { (compileUnit, function) ->
                            Triple(
                                    function,
                                    findReturnType(compilationContext, compileUnit, function.assignments, function.returnExpression, fullyQualifiedStructNames),
                                    compileUnit)
                        }
                        .map { triple ->
                            val returnType = triple.first.returnType
                            if (returnType != null && parseType(triple.first.codeMetainfo, returnType, triple.third.structs, triple.third.importStructs) != triple.second) {
                                throw CompilationException(triple.first.codeMetainfo, "Declared return type of function `${triple.first.packageName}/${triple.first.name}` do not corresponds what it really return. " +
                                        "You declared `$returnType` and computed was `${typeToString(triple.second)}`.")
                            }
                            triple
                        }
                        .map { pair ->
                            val parameters = pair.first.parameters
                                    .stream()
                                    .map { TypedParameter(it.name, parseType(pair.first.codeMetainfo, it.type, pair.third.structs, pair.third.importStructs)) }
                                    .toList()
                            FunctionWithReturnType(
                                    pair.first.codeMetainfo,
                                    pair.first.packageName,
                                    pair.first.name,
                                    pair.second,
                                    parameters,
                                    pair.first.assignments,
                                    pair.first.returnExpression)
                        }
                        .toList()
                        .toSet()
                CompileUnitToExport(
                        unit.packageName,
                        unit.structs
                                .stream()
                                .map {
                                    StructToExport(
                                            it.packageName,
                                            it.name,
                                            it.fields
                                                    .stream()
                                                    .map { FieldToExport(it.name, it.type) }
                                                    .toList()
                                    )
                                }
                                .toList()
                                .toSet(),
                        functionsWithReturnType.stream()
                                .map {
                                    FunctionToExport(
                                            it.packageName,
                                            it.name,
                                            it.returnType,
                                            it.parameters
                                                    .stream()
                                                    .map { ParameterToExport(it.name, it.type) }
                                                    .toList(),
                                            it.assignments,
                                            it.returnExpression)
                                }
                                .toList()
                                .toSet())
            }
            .toList()
            .toSet()

    //
    // DEFINITIONS
    //

    //
    // EXPORT TO FILE
    //
    if (options.outputDir != null) {
        log.info("Exporting files to ${options.outputDir}")
        PythonExport.export(options.outputDir, compilationUnitsToExport)
    } else {
        log.info("Not exporting files, because `outputDir` is not set")
    }
}

/*
    Represents every struct nad every function in compilation context (from all files)
 */
data class CompilationContext(val structs: Set<FlatStruct>, val functions: Set<com.magx2.capybara.Function>)

class CompilationException(codeMetainfo: CodeMetainfo, msg: String) : RuntimeException("${codeMetainfo.fileName} [${codeMetainfo.line}:${codeMetainfo.charInLine}] $msg")

data class CodeMetainfo(val fileName: String, val line: Int, val charInLine: Int)

fun parseCodeMetainfo(fileName: String, token: Token) = CodeMetainfo(fileName, token.line, token.charPositionInLine)
