package com.magx2.capybara

import com.magx2.capybara.BasicTypes.listType
import com.magx2.capybara.BasicTypes.stringType
import org.slf4j.LoggerFactory
import java.io.File
import java.lang.String.join
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

interface CapybaraExport {
    fun export(outputDir: String, compilationUnits: Set<CompileUnitToExport>)
}


data class CompileUnitToExport(
        val packageName: String,
        val structs: Set<StructToExport>,
        val functions: Set<FunctionToExport>)

data class StructToExport(val type: Type,
                          val fields: List<FieldToExport>)

data class FieldToExport(val name: String, val type: Type)

data class FunctionToExport(val packageName: String,
                            val name: String,
                            val returnExpression: ExpressionWithReturnType,
                            val parameters: List<ParameterToExport>,
                            val assignments: List<AssigmentStatementWithReturnType>)

data class ParameterToExport(val name: String, val type: Type)


private val log = LoggerFactory.getLogger(PythonExport::class.java)

object PythonExport : CapybaraExport {
    override fun export(outputDir: String, compilationUnits: Set<CompileUnitToExport>) {
        val structs = compilationUnits.stream()
                .flatMap { it.structs.stream() }
                .collect(
                        Collectors.groupingBy(
                                (java.util.function.Function<StructToExport, String> { it.type.packageName })
                        )
                )
                .toMap()
        val functions = compilationUnits.stream()
                .flatMap { it.functions.stream() }
                .collect(
                        Collectors.groupingBy(
                                (java.util.function.Function<FunctionToExport, String> { it.packageName })
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

private fun structToPython(struct: StructToExport): String {
    val constructorParameters = struct.fields
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))

    val constructorAssigment = struct.fields
            .stream()
            .map { "self.${it.name} = ${it.name}" }
            .collect(Collectors.joining("\n|\t\t"))

    val attributes = struct.fields
            .stream()
            .map { "${it.name}: ${typeToString(it.type)}" }
            .collect(Collectors.joining("\n|\t"))

    val methodDoc = generateDocForDef(
            "__init__",
            struct.fields
                    .stream()
                    .map { Pair(it.name, it.type) }
                    .toList(),
            struct.type,
            2)

    val strFields = struct.fields
            .stream()
            .map { """${it.name} = " + str(self.${it.name}) + """" }
            .collect(Collectors.joining(", "))

    return """
    |class ${struct.type.name}:
    |${'\t'}${"\"\"\""}${struct.type.name} class was generated by Capybara compiler
    |${'\t'}
    |${'\t'}Attributes:
    |${'\t'}-----------
    |${'\t'}$attributes
    |${'\t'}${"\"\"\""}
    |${'\t'}def __init__(self, $constructorParameters):
    |${'\t'}${'\t'}$methodDoc
    |${'\t'}${'\t'}$constructorAssigment
    |
    |${'\t'}def __str__(self):
    |${'\t'}${'\t'}return "${struct.type.name} { $strFields }" 
    |${'\n'}""".trimMargin()
}

private fun generateDocForDef(defName: String, parameters: List<Pair<String, Type>>, returnType: Type, indent: Int = 1): String {
    val parametersString = parameters.stream()
            .map { (name, type) -> "$name: ${typeToString(type)}" }
            .collect(Collectors.joining("\n|${"\t".repeat(indent)}"))
    val header = "$defName method was generated by Capybara compiler"
    val line = "-".repeat(header.length + 3)
    return """
        |${"\"\"\""}$header
        |${"\t".repeat(indent)}
        |${"\t".repeat(indent)}$line
        |${"\t".repeat(indent)}Parameters:
        |${"\t".repeat(indent)}$line
        |${"\t".repeat(indent)}$parametersString
        |${"\t".repeat(indent)}$line
        |${"\t".repeat(indent)}Return type: ${typeToString(returnType)}
        |${"\t".repeat(indent)}$line
        |${"\t".repeat(indent)}${"\"\"\""}
        """.trimMargin()
}

private fun functionToPython(function: FunctionToExport): String {
    val parameters = function.parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))
    val assignments = function.assignments
            .stream()
            .map { "${it.name} = ${expressionToString(it.expression)}" }
            .collect(Collectors.joining("\n|\t"))

    val methodDoc = generateDocForDef(
            function.name,
            function.parameters
                    .stream()
                    .map { Pair(it.name, it.type) }
                    .toList(),
            function.returnExpression.returnType)

    val main = if (function.name == "main"
            && function.parameters.size == 1
            && function.parameters[0].type == addGenericType(listType, stringType)) {
        """
        |${'\n'}if __name__ == "__main__":
        |${'\t'}import sys
        |${'\t'}${function.name}(sys.argv)
        |${"\n".repeat(2)}""".trimMargin()
    } else {
        ""
    }

    return """
    |def ${function.name}($parameters):
    |${'\t'}$methodDoc
    |${'\t'}$assignments
    |${'\t'}return ${expressionToString(function.returnExpression)}
    |${'\n'}$main""".trimMargin()
}

private fun expressionToString(expression: ExpressionWithReturnType): String =
        when (expression) {
            is ParameterExpressionWithReturnType -> expression.valueName
            is IntegerExpressionWithReturnType -> expression.value.toString()
            is FloatExpressionWithReturnType -> expression.value.toString()
            is BooleanExpressionWithReturnType -> if (expression.value) "True" else "False"
            is StringExpressionWithReturnType -> "\"${expression.value}\""
            is FunctionInvocationExpressionWithReturnType -> {
                val parameters = expression.parameters
                        .stream()
                        .map { expressionToString(it) }
                        .collect(Collectors.joining(", "))
                "${expression.functionName}($parameters)"
            }
            is InfixExpressionWithReturnType -> "(${expressionToString(expression.left)}) ${mapInfixOperator(expression)} (${expressionToString(expression.right)})"
            is IfExpressionWithReturnType -> "(${expressionToString(expression.trueBranch)}) if (${expressionToString(expression.condition)}) else (${expressionToString(expression.falseBranch)})"
            is NegateExpressionWithReturnType -> "not ${expressionToString(expression.negateExpression)}"
            is NewStructExpressionWithReturnType -> {
                "NewStructExpressionWithReturnType()" // TODO
            }
            is ValueExpressionWithReturnType -> expression.valueName
            is NewListExpressionWithReturnType -> expression.elements
                    .stream()
                    .map { expressionToString(it) }
                    .collect(Collectors.joining(", ", "[", "]"))
            is StructureAccessExpressionWithReturnType -> {
                "${expression.structureName}[${expressionToString(expression.structureIndex)}]"
            }
        }

private fun mapInfixOperator(expression: InfixExpressionWithReturnType) = when (expression.operation) {
    "^" -> "**"
    "~/" -> "//"
    else -> expression.operation
}


private fun <T> concat(vararg streams: Stream<T>): Stream<T> {
    var stream = Stream.empty<T>()
    for (s in streams) {
        stream = Stream.concat(stream, s)
    }
    return stream
}