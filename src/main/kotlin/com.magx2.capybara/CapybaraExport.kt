package com.magx2.capybara

import com.magx2.capybara.BasicTypes.anyType
import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.floatType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.listType
import com.magx2.capybara.BasicTypes.nothingType
import com.magx2.capybara.BasicTypes.stringType
import org.slf4j.LoggerFactory
import java.io.File
import java.lang.String.join
import java.nio.file.Paths
import java.util.stream.Collectors
import kotlin.streams.toList

interface CapybaraExport {
    fun export(unit: CompileUnitToExport)
}


data class CompileUnitToExport(
        val packageName: String,
        val structs: Set<StructToExport>,
        val functions: Set<FunctionToExport>,
        val defs: Set<DefToExport>,
        val unions: Set<UnionWithType>,
)

data class StructToExport(val type: Type,
                          val fields: List<FieldToExport>)

data class FieldToExport(val name: String, val type: Type)

data class FunctionToExport(val packageName: String,
                            val name: String,
                            val returnExpression: ExpressionWithReturnType,
                            val parameters: List<ParameterToExport>,
                            val assignments: List<AssigmentStatementWithReturnType>)

data class DefToExport(val packageName: String,
                       val name: String,
                       val parameters: List<Parameter>,
                       val statements: List<Statement>,
                       val returnExpression: ExpressionWithReturnType?)

data class ParameterToExport(val name: String, val type: Type)


private val log = LoggerFactory.getLogger(PythonExport::class.java)

class PythonExport(private val outputDir: String, private val assertions: Boolean) : CapybaraExport {
    private val initFilesDirectories = HashSet<String>()

    override fun export(unit: CompileUnitToExport) {
        val imports = unit.functions
                .stream()
                .flatMap { function ->
                    concat(
                            findImports(function.returnExpression, unit.packageName).stream(),
                            function.assignments
                                    .stream()
                                    .map { findImports(it.expression, unit.packageName) }
                                    .flatMap { it.stream() }
                    )
                }
                .map { packageToPythonPackage(it) }
                .map { "import $it" }

        val structs = unit.structs
                .stream()
                .filter { it.type != intType }
                .filter { it.type != floatType }
                .filter { it.type != booleanType }
                .filter { it.type != listType }
                .filter { it.type != stringType }
                .filter { it.type != nothingType }
                .filter { it.type != anyType }
                .map { struct -> structToPython(struct) }

        val functions = unit.functions
                .stream()
                .map { function -> functionToPython(function, assertions, unit.unions, unit.packageName) }

        writeAllToPackageFile(
                outputDir,
                unit.packageName,
                concat(imports, structs, functions).toList())

        val packageName = unit.packageName
        val directory = File("$outputDir$packageName.py").parentFile
        if (initFilesDirectories.contains(directory.absolutePath).not()) {
            val init = Paths.get(directory.absolutePath, "__init__.py")
            init.toFile().createNewFile()
            initFilesDirectories.add(directory.absolutePath)
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

private fun findImports(expresion: ExpressionWithReturnType, packageName: String): List<String> =
        when (expresion) {
            is ParameterExpressionWithReturnType -> emptyList()
            is IntegerExpressionWithReturnType -> emptyList()
            is FloatExpressionWithReturnType -> emptyList()
            is BooleanExpressionWithReturnType -> emptyList()
            is StringExpressionWithReturnType -> emptyList()
            NothingExpressionWithReturnType -> emptyList()
            is FunctionInvocationExpressionWithReturnType -> {
                expresion.parameters.flatMap { findImports(it, packageName) } + if (packageName != expresion.packageName) {
                    listOf(expresion.packageName)
                } else {
                    emptyList()
                }
            }
            is InfixExpressionWithReturnType -> findImports(expresion.left, packageName) + findImports(expresion.right, packageName)
            is IfExpressionWithReturnType -> findImports(expresion.condition, packageName) + findImports(expresion.trueBranch, packageName) + findImports(expresion.falseBranch, packageName)
            is NegateExpressionWithReturnType -> findImports(expresion.negateExpression, packageName)
            is NewStructExpressionWithReturnType ->
                if (packageName != expresion.returnType.packageName) {
                    listOf(expresion.returnType.packageName)
                } else {
                    emptyList()
                }
            is ValueExpressionWithReturnType -> emptyList()
            is StructFieldAccessExpressionWithReturnType -> findImports(expresion.structureExpression, packageName)
            is NewListExpressionWithReturnType -> expresion.elements.flatMap { findImports(it, packageName) }
            is AssertExpressionWithReturnType -> findImports(expresion.checkExpression, packageName) + findImports(expresion.returnExpression, packageName) + (if (expresion.messageExpression != null) findImports(expresion.messageExpression, packageName) else emptyList())
            is StructureAccessExpressionWithReturnType -> findImports(expresion.structureIndex, packageName)
            is IsExpressionWithReturnType -> emptyList()
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
    val header = "$defName method was generated by Capybara compiler"
    val line = "-".repeat(header.length + 3)
    val parametersString = if (parameters.isNotEmpty()) {
        val p = parameters.stream()
                .map { (name, type) -> "$name: ${typeToString(type)}" }
                .collect(Collectors.joining("\n|${"\t".repeat(indent)}"))
        """ |
            |${"\t".repeat(indent)}$line
            |${"\t".repeat(indent)}Parameters:
            |${"\t".repeat(indent)}$line
            |${"\t".repeat(indent)}$p
        """.trimMargin()
    } else {
        ""
    }
    return """
        |${"\"\"\""}$header
        |${"\t".repeat(indent)}$parametersString
        |${"\t".repeat(indent)}$line
        |${"\t".repeat(indent)}Return type: ${typeToString(returnType)}
        |${"\t".repeat(indent)}$line
        |${"\t".repeat(indent)}${"\"\"\""}
        """.trimMargin()
}

private fun functionToPython(function: FunctionToExport, assertions: Boolean, unions: Set<UnionWithType>, packageName: String): String {
    val parameters = function.parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))
    val assignments = if (function.assignments.isNotEmpty()) {
        "\n\t" + function.assignments
                .stream()
                .map {
                    generateAssertStatement(assertions, it.expression, unions, packageName) +
                            "${it.name} = ${expressionToString(it.expression, assertions, unions, packageName)}"
                }
                .filter { it.isNotBlank() }
                .collect(Collectors.joining("\n|\t"))
    } else {
        ""
    }

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
    |${'\t'}$methodDoc$assignments
    |${'\t'}${generateAssertStatement(assertions, function.returnExpression, unions, packageName)}return ${expressionToString(function.returnExpression, assertions, unions, packageName)}
    |${'\n'}$main""".trimMargin()
}

private fun generateAssertStatement(assertions: Boolean, expression: ExpressionWithReturnType, unions: Set<UnionWithType>, packageName: String): String {
    return if (assertions && expression is AssertExpressionWithReturnType) {
        val message = if (expression.messageExpression != null) {
            expressionToString(expression.messageExpression, assertions, unions, packageName)
        } else {
            "\"<no message>\""
        }
        "if not ${expressionToString(expression.checkExpression, assertions, unions, packageName)}: raise AssertionError($message)\n\t"
    } else {
        ""
    }
}

private fun expressionToString(expression: ExpressionWithReturnType, assertions: Boolean, unions: Set<UnionWithType>, packageName: String): String =
        when (expression) {
            is ParameterExpressionWithReturnType -> expression.valueName
            is IntegerExpressionWithReturnType -> expression.value.toString()
            is FloatExpressionWithReturnType -> expression.value.toString()
            is BooleanExpressionWithReturnType -> if (expression.value) "True" else "False"
            is StringExpressionWithReturnType -> "\"${expression.value}\""
            is NothingExpressionWithReturnType -> "None"
            is FunctionInvocationExpressionWithReturnType -> {
                val parameters = expression.parameters
                        .stream()
                        .map { expressionToString(it, assertions, unions, packageName) }
                        .collect(Collectors.joining(", "))
                if (expression.packageName == packageName) {
                    "${expression.functionName}($parameters)"
                } else {
                    "${packageToPythonPackage(expression.packageName)}.${expression.functionName}($parameters)"
                }
            }
            is InfixExpressionWithReturnType -> "(${expressionToString(expression.left, assertions, unions, packageName)}) ${mapInfixOperator(expression)} (${expressionToString(expression.right, assertions, unions, packageName)})"
            is IfExpressionWithReturnType -> "(${expressionToString(expression.trueBranch, assertions, unions, packageName)}) if (${expressionToString(expression.condition, assertions, unions, packageName)}) else (${expressionToString(expression.falseBranch, assertions, unions, packageName)})"
            is NegateExpressionWithReturnType -> "not ${expressionToString(expression.negateExpression, assertions, unions, packageName)}"
            is NewStructExpressionWithReturnType -> {
                val parameters = expression.fields
                        .stream()
                        .map { (name, expression) -> Pair(name, expressionToString(expression, assertions, unions, packageName)) }
                        .map { (name, value) -> "${name}=${value}" }
                        .collect(Collectors.joining(", "))
                if (expression.returnType.packageName == packageName) {
                    "${expression.returnType.name}($parameters)"
                } else {
                    "${packageToPythonPackage(expression.returnType.packageName)}.${expression.returnType.name}($parameters)"
                }
            }
            is ValueExpressionWithReturnType -> expression.valueName
            is NewListExpressionWithReturnType -> expression.elements
                    .stream()
                    .map { expressionToString(it, assertions, unions, packageName) }
                    .collect(Collectors.joining(", ", "[", "]"))
            is StructureAccessExpressionWithReturnType -> {
                "${expression.structureName}[${expressionToString(expression.structureIndex, assertions, unions, packageName)}]"
            }
            is IsExpressionWithReturnType -> {
                val union = unions.stream()
                        .filter { it.type == expression.type }
                        .findAny()
                if (union.isPresent) {
                    union.get()
                            .types
                            .stream()
                            .map { "isinstance(${expression.value}, ${findPythonType(it)})" }
                            .collect(Collectors.joining(" or ", "(", ")"))
                } else {
                    "isInstance(${expression.value}, ${findPythonType(expression.type)})"
                }
            }
            is StructFieldAccessExpressionWithReturnType -> "${expressionToString(expression.structureExpression, assertions, unions, packageName)}.${expression.fieldName}"
            is AssertExpressionWithReturnType -> {
                expressionToString(expression.returnExpression, assertions, unions, packageName)
            }
        }

fun findPythonType(type: Type): String {
    if (type.packageName == typePackageName) {
        if (type == intType) {
            return "int"
        } else if (type == floatType) {
            return "float"
        } else if (type == booleanType) {
            return "bool"
        } else if (type == stringType) {
            return "str"
        } else if (type == listType) {
            return "list"
        }
    }
    return packageToPythonPackage(type.packageName) + "." + type.name
}

private fun packageToPythonPackage(packageName: String) = packageName.substring(1).replace("/", ".")

private fun mapInfixOperator(expression: InfixExpressionWithReturnType) = when (expression.operation) {
    "^" -> "**"
    "~/" -> "//"
    "&&" -> "and"
    "||" -> "or"
    else -> expression.operation
}
