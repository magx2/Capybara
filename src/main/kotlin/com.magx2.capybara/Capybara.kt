package com.magx2.capybara

import org.antlr.v4.runtime.Token
import org.apache.commons.cli.HelpFormatter
import org.slf4j.LoggerFactory
import java.io.File
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
import kotlin.streams.asStream
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
    log.info("Assertions are " + if (options.disableAssertions) "disabled" else "enabled")
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
    val langFiles = langFiles()
    if (options.debug) {
        log.info("Adding capybara lang files: [{}]", java.lang.String.join(", ", langFiles))
    }
    val compileUnits = (langFiles + options.filesToCompile)
            .stream()
            .map { compiler.compile(it) }
            .toList()

    //
    // EXPORTS/IMPORTS
    //
    val exports = compileUnits.stream()
            .map { Export(it.packageName, it.structs.toSet(), it.unions, it.functions.toSet(), it.defs.toSet()) }
            .collect(object : Collector<Export, MutableMap<String, Export>, Map<String, Export>> {
                override fun characteristics(): MutableSet<Collector.Characteristics> = setOf(UNORDERED).toMutableSet()

                override fun supplier(): Supplier<MutableMap<String, Export>> =
                        Supplier { HashMap<String, Export>() }

                override fun accumulator(): BiConsumer<MutableMap<String, Export>, Export> =
                        BiConsumer { map, export ->
                            val exportFromMap = map[export.packageName]
                            map[export.packageName] = if (exportFromMap == null) {
                                export
                            } else {
                                Export(
                                        export.packageName,
                                        export.structs + exportFromMap.structs,
                                        export.unions + exportFromMap.unions,
                                        export.functions + exportFromMap.functions,
                                        export.defs + exportFromMap.defs,
                                )
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
                        compileUnit.defs,
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
                                .toList(),
                        imports.stream()
                                .flatMap { (import, exportForPackage) ->
                                    if (import.subImport.isEmpty()) {
                                        exportForPackage.defs.stream()
                                    } else {
                                        exportForPackage.defs
                                                .stream()
                                                .filter { import.subImport.contains(it.name) }
                                    }
                                }
                                .toList(),
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
                                    parseUnion(union, types)
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
                        unit.defs,
                        unit.importStructs,
                        unit.importUnions,
                        unit.importFunctions,
                        unit.importDefs,
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
                    .toSet(),
            compileUnitsWithFlatStructs.stream()
                    .flatMap { it.defs.stream() }
                    .toList()
                    .toSet(),
    )

    // find return types for functions
    val compilationUnitsToExport = compileUnitsWithFlatStructs.stream()
            .map { unit ->
                val functionsWithReturnType = unit.functions
                        .stream()
                        .map { function ->
                            val functionCompiler = FunctionCompiler(compilationContext, unit, fullyQualifiedStructNames)
                            val assignments = functionCompiler.findReturnTypeForAssignments(function.assignments)
                            val expressionCompiler = ExpressionCompiler(assignments, compilationContext, unit, fullyQualifiedStructNames)
                            FunctionOnBuild(
                                    function,
                                    expressionCompiler.findReturnType(function.returnExpression),
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
                                                    .map { f -> FieldToExport(f.name, f.type) }
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
                                                    .map { p -> ParameterToExport(p.name, p.type) }
                                                    .toList(),
                                            it.assignments)
                                }
                                .toList()
                                .toSet(),
                        emptySet(),//TODO()
                        unit.unions + unit.importUnions.map { parseUnion(it, unit) },
                )
            }
            .collect(object : Collector<CompileUnitToExport, MutableMap<String, CompileUnitToExport>, Set<CompileUnitToExport>> {
                override fun supplier(): Supplier<MutableMap<String, CompileUnitToExport>> =
                        Supplier<MutableMap<String, CompileUnitToExport>> { HashMap() }

                override fun accumulator(): BiConsumer<MutableMap<String, CompileUnitToExport>, CompileUnitToExport> =
                        BiConsumer<MutableMap<String, CompileUnitToExport>, CompileUnitToExport> { map, unit ->
                            val unitInMap = map[unit.packageName]
                            map[unit.packageName] = if (unitInMap != null) {
                                CompileUnitToExport(
                                        unit.packageName,
                                        unit.structs + unitInMap.structs,
                                        unit.functions + unitInMap.functions,
                                        unit.unions + unitInMap.unions,
                                )
                            } else {
                                unit
                            }
                        }

                override fun combiner(): BinaryOperator<MutableMap<String, CompileUnitToExport>> =
                        BinaryOperator<MutableMap<String, CompileUnitToExport>> { map1, map2 ->
                            val map = HashMap<String, CompileUnitToExport>(map1.size + map2.size)
                            map.putAll(map1)
                            map.putAll(map2)
                            map
                        }

                override fun finisher(): Function<MutableMap<String, CompileUnitToExport>, Set<CompileUnitToExport>> =
                        java.util.function.Function<MutableMap<String, CompileUnitToExport>, Set<CompileUnitToExport>> { it.values.toSet() }

                override fun characteristics(): MutableSet<Collector.Characteristics> = setOf(UNORDERED).toMutableSet()

            })

    //
    // DEFINITIONS
    //

    //
    // EXPORT TO FILE
    //
    if (options.outputDir != null) {
        log.info("Exporting files to ${options.outputDir}")
        val exporter = PythonExport(options.outputDir, options.disableAssertions.not())
        compilationUnitsToExport.forEach { exporter.export(it) }
    } else {
        log.info("Not exporting files, because `outputDir` is not set")
    }
}

fun langFiles(): List<String> =
        File({}.javaClass.getResource("/capybara").toURI())
                .walkTopDown()
                .filter { it.isFile }
                .filter { it.extension == "cb" }
                .asStream()
                .map { it.absolutePath }
                .toList()

private fun isSameType(declaredReturnType: Type, actualReturnType: Type, compilationContext: CompilationContext) =
        if (declaredReturnType == actualReturnType) {
            true
        } else {
            val declaredUnionType = findUnionType(declaredReturnType, compilationContext)
            val actualUnionType = findUnionType(actualReturnType, compilationContext)
            if (declaredUnionType.isPresent && actualUnionType.isPresent) {
                areUnionTypesTheSame(
                        declaredUnionType.get(),
                        actualUnionType.get())
            } else {
                compilationContext.unions
                        .stream()
                        .filter { it.type == declaredReturnType }
                        .anyMatch { it.types.contains(actualReturnType) }
            }
        }

fun findUnionType(type: Type, compilationContext: CompilationContext): Optional<UnionWithType> =
        compilationContext.unions
                .stream()
                .filter { it.type == type }
                .findAny()

fun areUnionTypesTheSame(union1: UnionWithType, union2: UnionWithType): Boolean = union1.types == union2.types

data class FunctionOnBuild(
        val function: com.magx2.capybara.Function,
        val expression: ExpressionWithReturnType,
        val unit: CompileUnitWithFlatStructs,
        val assignments: List<AssigmentStatementWithReturnType>
)

/*
    Represents every struct nad every function in compilation context (from all files)
 */
data class CompilationContext(
        val structs: Set<FlatStruct>,
        val unions: Set<UnionWithType>,
        val functions: Set<com.magx2.capybara.Function>,
        val defs: Set<Def>,
)

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