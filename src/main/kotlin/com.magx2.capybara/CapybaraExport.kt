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
//                .peek { (packageName, texts) -> println(packageName +" -> "+ texts)}
                .collect(Collectors.groupingBy(
                        (java.util.function.Function<Pair<String, List<String>>, String> { it.first })
                ))
                .forEach { (packageName, pairs) ->
                    val texts = pairs.stream()
                            .map { it.second }
                            .flatMap { it.stream() }
                            .toList()
                    writeAllToPackageFile(outputDir, packageName, texts)
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

private fun structToPython(struct: FlatStruct): String {
    val constructorParameters = struct.fields
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))

    val constructorAssigment = struct.fields
            .stream()
            .map { "self.${it.name} = ${it.name}" }
            .collect(Collectors.joining("\n|\t\t"))

    val strFields = struct.fields
            .stream()
            .map { """${it.name} = " + str(self.${it.name}) + """" }
            .collect(Collectors.joining(", "))

    return """
    |class ${struct.name}:
    |${'\t'}def __init__(self, $constructorParameters):
    |${'\t'}${'\t'}$constructorAssigment
    |
    |${'\t'}def __str__(self):
    |${'\t'}${'\t'}return "${struct.name} { $strFields }" 
    |${'\n'}""".trimMargin()
}

private fun functionToPython(function: FunctionWithReturnType): String = """
    |def ${function.name}():
    |${'\t'}pass
    |${'\n'}""".trimMargin()

private fun <T> concat(vararg streams: Stream<T>): Stream<T> {
    var stream = Stream.empty<T>()
    for (s in streams) {
        stream = Stream.concat(stream, s)
    }
    return stream
}