package com.magx2.capybara

import org.slf4j.LoggerFactory
import java.io.File
import java.lang.String.join
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

interface CapybaraExport {
    fun export(outputDir: String, compilationUnits: Set<CompileUnitToExport>)
}

private val log = LoggerFactory.getLogger(PythonExport::class.java)

object PythonExport : CapybaraExport {
    override fun export(outputDir: String, compilationUnits: Set<CompileUnitToExport>) {
        val structs = compilationUnits.stream()
                .flatMap { it.structs.stream() }
                .collect(
                        Collectors.groupingBy(
                                (java.util.function.Function<FlatStruct, String> { it.packageName })
                        )
                )
                .toMap()
        val functions = compilationUnits.stream()
                .flatMap { it.functions.stream() }
                .collect(
                        Collectors.groupingBy(
                                (java.util.function.Function<FunctionWithReturnType, String> { it.packageName })
                        )
                )
                .toMap()

        concat(
                structs.entries
                        .stream()
                        .map { (packageName, structs) ->
                            Pair(
                                    packageName,
                                    structs.stream()
                                            .map { structToPython(it) }
                                            .toList())
                        },
                functions.entries
                        .stream()
                        .map { (packageName, functions) ->
                            Pair(
                                    packageName,
                                    functions.stream()
                                            .map { functionToPython(it) }
                                            .toList())
                        })
                .forEach { (packageName, structs) ->
                    writeAllToPackageFile(outputDir, packageName, structs)
                }
    }
}

private fun writeAllToPackageFile(outputDir: String, packageName: String, texts: List<String>) {
    val text = join("\n\n", texts)
    val fileName = "$outputDir$packageName.py"
    File(fileName).also {
        it.parentFile.mkdirs()
        log.info("Saving to file `${it.absolutePath}`")
        it.writeText(text)
    }
}

private fun structToPython(struct: FlatStruct): String = """
    class ${struct.name}Struct:
        pass
""".trimIndent()

private fun functionToPython(function: FunctionWithReturnType): String = """
    def ${function.name}():
        pass
""".trimIndent()

private fun <T> concat(vararg streams: Stream<T>): Stream<T> {
    var stream = Stream.empty<T>()
    for (s in streams) {
        stream = Stream.concat(stream, s)
    }
    return stream
}