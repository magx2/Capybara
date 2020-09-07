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
import java.util.stream.Stream
import kotlin.streams.toList
import kotlin.system.exitProcess

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
        exitProcess(100)
    } catch (e: Exception) {
        if (options.debug) log.error("Generic exception", e)
        System.err.println("Generic exception: " + e.message)
        exitProcess(1)
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
            .map { Export(it.packageName, it.structs.toSet(), it.unions, it.functions.toSet()) }
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
                                        export.unions + exportFromMap.unions,
                                        export.functions + exportFromMap.functions)
                            }
                        }

                override fun combiner(): BinaryOperator<MutableMap<String, Export>> =
                        BinaryOperator { map1, map2 ->
                            (map1 + map2).toMutableMap()
                        }

                override fun finisher(): Function<MutableMap<String, Export>, Map<String, Export>> = Function { it.toMap() }
            })
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
                        compileUnit.unions,
                        compileUnit.functions,
                        imports.stream()
                                .flatMap { (import, exportForPackage) ->
                                    if (import.subImport.isEmpty()) {
                                        exportForPackage.structs.stream()
                                    } else {
                                        exportForPackage.structs
                                                .stream()
                                                .filter { import.subImport.contains(it.type.name) }
                                    }
                                }
                                .toList(),
                        imports.stream()
                                .flatMap { (import, exportForPackage) ->
                                    if (import.subImport.isEmpty()) {
                                        exportForPackage.unions.stream()
                                    } else {
                                        exportForPackage.unions
                                                .stream()
                                                .filter { import.subImport.contains(it.type.name) }
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
    val fullyQualifiedStructNames: Map<Type, Struct> = findFullyQualifiedStructNames(compileUnits)

    val compileUnitsWithFlatStructs = compileUnitsWithImports.stream()
            .map { unit ->
                val types = getTypes(compileUnitsWithImports, unit.packageName) + importsToTypes(unit)
                Triple(
                        unit,
                        unit.structs
                                .stream()
                                .map { struct ->
                                    addUnrollSpreadFieldsInStruct(
                                            types,
                                            struct,
                                            fullyQualifiedStructNames)
                                }
                                .toList()
                                .toSet(),
                        unit.unions
                                .stream()
                                .map { union ->
                                    UnionWithType(
                                            union.type,
                                            union.types
                                                    .stream()
                                                    .map { parseType(union.codeMetainfo, it, types) }
                                                    .toList()
                                                    .toSet()
                                    )
                                }
                                .toList()
                                .toSet()
                )
            }
            .map { (unit, flatStructs, unions) ->
                CompileUnitWithFlatStructs(
                        unit.packageName,
                        flatStructs,
                        unions,
                        unit.functions,
                        unit.importStructs,
                        unit.importUnions,
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
            compileUnitsWithFlatStructs.stream()
                    .flatMap { it.unions.stream() }
                    .toList()
                    .toSet(),
            functions.stream()
                    .map { it.second }
                    .toList()
                    .toSet())

    // find return types for functions
    val compilationUnitsToExport = compileUnitsWithFlatStructs.stream()
            .map { unit ->
                val functionsWithReturnType = unit.functions
                        .stream()
                        .map { function ->
                            val functionCompiler = FunctionCompiler(compilationContext, unit, fullyQualifiedStructNames)
                            val assignments = functionCompiler.findReturnTypeForAssignments(function.assignments)
                            FunctionOnBuild(
                                    function,
                                    functionCompiler.findReturnType(assignments, function.returnExpression),
                                    unit,
                                    assignments)
                        }
                        .map { triple ->
                            val returnType = triple.function.returnType
                            if (returnType != null) {
                                val declaredReturnType = parseType(triple.function.codeMetainfo, returnType, getTypes(compilationContext, triple.unit.packageName) + importsToTypes(triple.unit))
                                if (!isSameType(declaredReturnType, triple.expression.returnType, compilationContext)) {
                                    throw CompilationException(triple.function.codeMetainfo, "Declared return type of function `${triple.function.packageName}/${triple.function.name}` do not corresponds what it really return. " +
                                            "You declared `$returnType` and computed was `${typeToString(triple.expression.returnType)}`.")
                                }
                            }
                            triple
                        }
                        .map { pair ->
                            val parameters = pair.function.parameters
                                    .stream()
                                    .map { TypedParameter(it.name, parseType(pair.function.codeMetainfo, it.type, getTypes(compilationContext, pair.unit.packageName) + importsToTypes(pair.unit))) }
                                    .toList()
                            FunctionWithReturnType(
                                    pair.function.packageName,
                                    pair.function.name,
                                    pair.expression,
                                    parameters,
                                    pair.assignments)
                        }
                        .toList()
                        .toSet()
                CompileUnitToExport(
                        unit.packageName,
                        unit.structs
                                .stream()
                                .map {
                                    StructToExport(
                                            it.type,
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
                                            it.returnExpression,
                                            it.parameters
                                                    .stream()
                                                    .map { ParameterToExport(it.name, it.type) }
                                                    .toList(),
                                            it.assignments)
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

private fun isSameType(declaredReturnType: Type, actualReturnType: Type, compilationContext: CompilationContext) =
        if (declaredReturnType == actualReturnType) {
            true
        } else {
            compilationContext.unions
                    .stream()
                    .filter { it.type == declaredReturnType }
                    .anyMatch { it.types.contains(actualReturnType) }
        }

data class FunctionOnBuild(
        val function: com.magx2.capybara.Function,
        val expression: ExpressionWithReturnType,
        val unit: CompileUnitWithFlatStructs,
        val assignments: List<AssigmentStatementWithReturnType>
)

/*
    Represents every struct nad every function in compilation context (from all files)
 */
data class CompilationContext(val structs: Set<FlatStruct>, val unions: Set<UnionWithType>, val functions: Set<com.magx2.capybara.Function>)

class CompilationException(codeMetainfo: CodeMetainfo, msg: String) : RuntimeException("${codeMetainfo.fileName} [${codeMetainfo.line}:${codeMetainfo.charInLine}] $msg")

data class CodeMetainfo(val fileName: String, val line: Int, val charInLine: Int)

fun parseCodeMetainfo(fileName: String, token: Token) = CodeMetainfo(fileName, token.line, token.charPositionInLine)

fun importsToTypes(unit: CompileUnitWithImports): Set<Type> =
        Stream.concat(
                unit.importStructs.stream().map { it.type },
                unit.importUnions.stream().map { it.type })
                .toList()
                .toSet()

fun importsToTypes(unit: CompileUnitWithFlatStructs): Set<Type> =
        Stream.concat(
                unit.importStructs.stream().map { it.type },
                unit.importUnions.stream().map { it.type })
                .toList()
                .toSet()