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
        val defs: Set<AbstractDefToExport>,
        val unions: Set<UnionWithType>,
)

data class StructToExport(val type: Type,
                          val fields: List<FieldToExport>)

data class FieldToExport(val name: String, val type: Type)

data class FunctionToExport(val codeMetainfo: CodeMetainfo,
                            val packageName: String,
                            val name: String,
                            val returnExpression: ExpressionWithReturnType,
                            val parameters: List<ParameterToExport>,
                            val assignments: List<AssigmentStatementWithType>)

sealed class AbstractDefToExport(
        open val codeMetainfo: CodeMetainfo,
        open val packageName: String,
        open val name: String,
        open val parameters: List<ParameterToExport>,
        open val returnType: Type?)

data class DefToExport(
        override val codeMetainfo: CodeMetainfo,
        override val packageName: String,
        override val name: String,
        override val parameters: List<ParameterToExport>,
        val statements: List<StatementWithType>,
        val returnExpression: ExpressionWithReturnType?) : AbstractDefToExport(codeMetainfo, packageName, name, parameters, returnExpression?.returnType)

data class NativeDefToExport(
        override val codeMetainfo: CodeMetainfo, override val packageName: String,
        override val name: String,
        override val parameters: List<ParameterToExport>,
        override val returnType: Type?,
        val nativeStatements: List<String>) : AbstractDefToExport(codeMetainfo, packageName, name, parameters, returnType)

fun mapDef(def: AbstractDefWithTypes) =
        when (def) {
            is DefWithTypes ->
                DefToExport(
                        def.codeMetainfo,
                        def.packageName,
                        def.name,
                        def.parameters.map { ParameterToExport(it.name, it.type) },
                        def.statements,
                        def.returnExpression)
            is NativeDefWithTypes ->
                NativeDefToExport(
                        def.codeMetainfo,
                        def.packageName,
                        def.name,
                        def.parameters.map { ParameterToExport(it.name, it.type) },
                        def.returnType,
                        def.nativeStatements)
        }

data class ParameterToExport(val name: String, val type: Type)


private val log = LoggerFactory.getLogger(PythonExport::class.java)

class PythonExport(private val outputDir: String,
                   private val methodsToRewrite: Set<MethodToRewrite>,
                   private val assertions: Boolean) : CapybaraExport {
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
                .map { function -> functionToPython(function, assertions, unit.unions, methodsToRewrite, unit.packageName) }

        val defs = unit.defs
                .stream()
                .map { def -> defToPython(def, assertions, unit.unions, methodsToRewrite, unit.packageName) }

        val listOfStrings = addGenericType(listType, stringType)
        val main = unit.defs
                .stream()
                .filter { it.name == "main" }
                .filter { it.parameters.size == 1 }
                .filter { it.parameters.first().type == listOfStrings }
                .map { def ->
                    val name = findMethodNameFromParameter(
                            def.packageName,
                            def.name,
                            def.parameters,
                            methodsToRewrite)
                    """
                    |${'\n'}if __name__ == "__main__":
                    |${'\t'}import sys
                    |${'\t'}$name(sys.argv)
                    |${"\n".repeat(2)}""".trimMargin()
                }

        writeAllToPackageFile(
                outputDir,
                unit.packageName,
                concat(imports, structs, functions, defs, main).toList())

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
            is FunctionInvocationExpressionWithReturnType ->
                exportMethodInvocation(expresion.parameters, expresion.packageName, packageName)
            is DefInvocationExpressionWithReturnType ->
                exportMethodInvocation(expresion.parameters, expresion.packageName, packageName)
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

private fun exportMethodInvocation(
        parameters: List<ExpressionWithReturnType>,
        expressionPackageName: String,
        packageName: String): List<String> =
        parameters.flatMap { findImports(it, packageName) } + if (packageName != expressionPackageName) {
            listOf(expressionPackageName)
        } else {
            emptyList()
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
    |${buildIndent(1)}${"\"\"\""}`
    |${buildIndent(1)}${struct.type.name}` class was generated by Capybara compiler
    |${buildIndent(1)}
    |${buildIndent(1)}Attributes:
    |${buildIndent(1)}-----------
    |${buildIndent(1)}$attributes
    |${buildIndent(1)}${"\"\"\""}
    |${buildIndent(1)}def __init__(self, $constructorParameters):
    |$methodDoc${buildIndent(2)}$constructorAssigment
    |
    |${buildIndent(1)}def __str__(self):
    |${buildIndent(2)}return "${struct.type.name} { $strFields }" 
    |${buildIndent(1)}""".trimMargin()
}

private fun generateDocForDef(
        defName: String,
        parameters: List<Pair<String, Type>>,
        returnType: Type?,
        indent: Int = 1): String {
    val header = "\n${buildIndent(indent)}`$defName` method was generated by Capybara compiler"
    val line = "-".repeat(header.length + 3)
    val parametersString = if (parameters.isNotEmpty()) {
        val p = parameters.stream()
                .map { (name, type) -> "$name: ${typeToString(type)}" }
                .collect(Collectors.joining("\n|${"\t".repeat(indent)}"))
        """ |
            |${buildIndent(indent)}$line
            |${buildIndent(indent)}Parameters:
            |${buildIndent(indent)}$line
            |${buildIndent(indent)}$p
        """.trimMargin()
    } else {
        ""
    }

    val returnTypeString = if (returnType != null) {
        typeToString(returnType)
    } else {
        "<N/A>"
    }

    return """
        |${buildIndent(indent)}${"\"\"\""}$header
        |${buildIndent(indent)}$parametersString
        |${buildIndent(indent)}$line
        |${buildIndent(indent)}Return type: $returnTypeString
        |${buildIndent(indent)}$line
        |${buildIndent(indent)}${"\"\"\""}${'\n'}
        """.trimMargin()
}


private fun generateDocForDefWithTypedParameters(
        defName: String,
        parameters: List<ParameterToExport>,
        returnType: Type?,
        indent: Int = 1): String =
        generateDocForDef(
                defName,
                parameters.map { Pair(it.name, it.type) },
                returnType,
                indent)

private fun functionToPython(function: FunctionToExport,
                             assertions: Boolean,
                             unions: Set<UnionWithType>,
                             methodsToRewrite: Set<MethodToRewrite>,
                             packageName: String): String {
    val parameters = function.parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))
    val assignments = if (function.assignments.isNotEmpty()) {
        function.assignments
                .stream()
                .map {
                    generateAssertStatement(assertions, it.expression, unions, methodsToRewrite, packageName) +
                            "\t${it.name} = ${expressionToString(it.expression, assertions, unions, methodsToRewrite, packageName)}"
                }
                .filter { it.isNotBlank() }
                .collect(Collectors.joining("\n|")) + "\n|"
    } else {
        ""
    }

    val methodDoc = generateDocForDefWithTypedParameters(function.name, function.parameters, function.returnExpression.returnType)

    val name = findMethodNameFromParameter(
            function.packageName,
            function.name,
            function.parameters,
            methodsToRewrite)

    return """
    |def $name($parameters):
    |$methodDoc$assignments${generateAssertStatement(assertions, function.returnExpression, unions, methodsToRewrite, packageName)}${buildIndent(1)}return ${expressionToString(function.returnExpression, assertions, unions, methodsToRewrite, packageName)}""".trimMargin()
}

private fun defToPython(def: AbstractDefToExport,
                        assertions: Boolean,
                        unions: Set<UnionWithType>,
                        methodsToRewrite: Set<MethodToRewrite>,
                        packageName: String): String {
    val parameters = def.parameters
            .stream()
            .map { it.name }
            .collect(Collectors.joining(", "))

    val methodDoc = generateDocForDefWithTypedParameters(def.name, def.parameters, def.returnType)
    val body =
            when (def) {
                is DefToExport -> defToPythonBody(def, assertions, unions, methodsToRewrite, packageName)
                is NativeDefToExport -> defToPythonBody(def, assertions, unions, methodsToRewrite, packageName)
            }

    val name = findMethodNameFromParameter(
            def.packageName,
            def.name,
            def.parameters,
            methodsToRewrite)

    return """
    |def $name($parameters):
    |$methodDoc$body""".trimMargin()
}

private fun defToPythonBody(def: DefToExport,
                            assertions: Boolean,
                            unions: Set<UnionWithType>,
                            methodsToRewrite: Set<MethodToRewrite>,
                            packageName: String): String {
    val statements = def.statements
            .stream()
            .map { statementToPython(it, assertions, unions, methodsToRewrite, packageName, 1) }
            .collect(Collectors.joining("\n"))

    val returnExpression = if (def.returnExpression != null) {
        "\n|${generateAssertStatement(assertions, def.returnExpression, unions, methodsToRewrite, packageName)}${buildIndent(1)}return ${expressionToString(def.returnExpression, assertions, unions, methodsToRewrite, packageName)}"
    } else {
        ""
    }

    return statements + returnExpression
}

private fun defToPythonBody(def: NativeDefToExport,
                            assertions: Boolean,
                            unions: Set<UnionWithType>,
                            methodsToRewrite: Set<MethodToRewrite>,
                            packageName: String): String =
        "\t" + def.nativeStatements
                .stream()
                .collect(Collectors.joining("\n\t"))

private fun statementToPython(statement: StatementWithType,
                              assertions: Boolean,
                              unions: Set<UnionWithType>,
                              methodsToRewrite: Set<MethodToRewrite>,
                              packageName: String,
                              indent: Int): String =
        when (statement) {
            is AssigmentStatementWithType -> {
                val assert = if (statement.expression is AssertExpressionWithReturnType) {
                    generateAssertStatement(assertions, statement.expression, unions, methodsToRewrite, packageName, indent)
                } else {
                    ""
                }
                "$assert${buildIndent(indent)}${statement.name} = ${expressionToString(statement.expression, assertions, unions, methodsToRewrite, packageName)}"
            }
            is WhileStatementWithType -> {
                val statements = statement.statements
                        .stream()
                        .map { statementToPython(it, assertions, unions, methodsToRewrite, packageName, indent + 1) }
                        .collect(Collectors.joining("\n"))
                "${buildIndent(indent)}while ${expressionToString(statement.condition, assertions, unions, methodsToRewrite, packageName)}:\n$statements"
            }
            is AssertStatementWithType -> generateAssertStatement(
                    assertions,
                    statement.checkExpression,
                    statement.messageExpression,
                    unions,
                    methodsToRewrite,
                    packageName,
                    indent)
            is DefCallStatementWithType -> {
                val parameters = statement.parameters
                        .stream()
                        .map { expressionToString(it, assertions, unions, methodsToRewrite, packageName) }
                        .collect(Collectors.joining(", "))
                val name = findMethodNameFromExpression(
                        statement.packageName,
                        statement.defName,
                        statement.parameters,
                        methodsToRewrite)
                buildIndent(indent) + if (statement.packageName == packageName) {
                    "$name($parameters)"
                } else {
                    "${packageToPythonPackage(statement.packageName)}.$name($parameters)"
                }
            }
        }

private fun buildIndent(indent: Int) = "\t".repeat(indent)

private fun generateAssertStatement(
        assertions: Boolean,
        expression: ExpressionWithReturnType,
        unions: Set<UnionWithType>,
        methodsToRewrite: Set<MethodToRewrite>,
        packageName: String,
        indent: Int = 1): String {
    return if (expression is AssertExpressionWithReturnType) {
        generateAssertStatement(
                assertions,
                expression.checkExpression,
                expression.messageExpression,
                unions,
                methodsToRewrite,
                packageName,
                indent)
    } else {
        ""
    }
}

private fun generateAssertStatement(
        assertions: Boolean,
        checkExpression: ExpressionWithReturnType,
        messageExpression: ExpressionWithReturnType?,
        unions: Set<UnionWithType>,
        methodsToRewrite: Set<MethodToRewrite>,
        packageName: String,
        indent: Int = 0): String {
    return if (assertions) {
        val message = if (messageExpression != null) {
            expressionToString(messageExpression, assertions, unions, methodsToRewrite, packageName)
        } else {
            "\"<no message>\""
        }
        "${buildIndent(indent)}if not ${expressionToString(checkExpression, assertions, unions, methodsToRewrite, packageName)}: raise AssertionError($message)\n"
    } else {
        ""
    }
}

private fun expressionToString(expression: ExpressionWithReturnType,
                               assertions: Boolean,
                               unions: Set<UnionWithType>,
                               methodsToRewrite: Set<MethodToRewrite>,
                               packageName: String): String =
        when (expression) {
            is ParameterExpressionWithReturnType -> expression.valueName
            is IntegerExpressionWithReturnType -> expression.value.toString()
            is FloatExpressionWithReturnType -> expression.value.toString()
            is BooleanExpressionWithReturnType -> if (expression.value) "True" else "False"
            is StringExpressionWithReturnType -> "\"${expression.value}\""
            is NothingExpressionWithReturnType -> "None"
            is FunctionInvocationExpressionWithReturnType ->
                methodToString(expression.parameters, expression.packageName, expression.functionName, assertions, unions, methodsToRewrite, packageName)
            is DefInvocationExpressionWithReturnType ->
                methodToString(expression.parameters, expression.packageName, expression.functionName, assertions, unions, methodsToRewrite, packageName)
            is InfixExpressionWithReturnType -> "(${expressionToString(expression.left, assertions, unions, methodsToRewrite, packageName)}) ${mapInfixOperator(expression)} (${expressionToString(expression.right, assertions, unions, methodsToRewrite, packageName)})"
            is IfExpressionWithReturnType -> "(${expressionToString(expression.trueBranch, assertions, unions, methodsToRewrite, packageName)}) if (${expressionToString(expression.condition, assertions, unions, methodsToRewrite, packageName)}) else (${expressionToString(expression.falseBranch, assertions, unions, methodsToRewrite, packageName)})"
            is NegateExpressionWithReturnType -> "not ${expressionToString(expression.negateExpression, assertions, unions, methodsToRewrite, packageName)}"
            is NewStructExpressionWithReturnType -> {
                val parameters = expression.fields
                        .stream()
                        .map { (name, expression) -> Pair(name, expressionToString(expression, assertions, unions, methodsToRewrite, packageName)) }
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
                    .map { expressionToString(it, assertions, unions, methodsToRewrite, packageName) }
                    .collect(Collectors.joining(", ", "[", "]"))
            is StructureAccessExpressionWithReturnType -> {
                "${expression.structureName}[${expressionToString(expression.structureIndex, assertions, unions, methodsToRewrite, packageName)}]"
            }
            is IsExpressionWithReturnType -> {
                val union = unions.stream()
                        .filter { it.type == expression.type }
                        .findAny()
                if (union.isPresent) {
                    union.get()
                            .types
                            .stream()
                            .map { isInstance(expression.value, it) }
                            .collect(Collectors.joining(" or ", "(", ")"))
                } else {
                    isInstance(expression.value, expression.type)
                }
            }
            is StructFieldAccessExpressionWithReturnType -> "${expressionToString(expression.structureExpression, assertions, unions, methodsToRewrite, packageName)}.${expression.fieldName}"
            is AssertExpressionWithReturnType -> {
                expressionToString(expression.returnExpression, assertions, unions, methodsToRewrite, packageName)
            }
        }

private fun methodToString(
        expressionParameters: List<ExpressionWithReturnType>,
        expressionPackageName: String,
        expressionFunctionName: String,
        assertions: Boolean,
        unions: Set<UnionWithType>,
        methodsToRewrite: Set<MethodToRewrite>,
        packageName: String): String {
    val parameters = expressionParameters
            .stream()
            .map { expressionToString(it, assertions, unions, methodsToRewrite, packageName) }
            .collect(Collectors.joining(", "))
    val name = findMethodNameFromExpression(expressionPackageName, expressionFunctionName, expressionParameters, methodsToRewrite)
    return if (expressionPackageName == packageName) {
        "$name($parameters)"
    } else {
        "${packageToPythonPackage(expressionPackageName)}.$name($parameters)"
    }
}


private fun findMethodNameFromExpression(methodPackageName: String,
                                         methodName: String,
                                         methodParameters: List<ExpressionWithReturnType>,
                                         methodsToRewrite: Set<MethodToRewrite>): String =
        findMethodName(methodPackageName,
                methodName,
                methodParameters.map { it.returnType },
                methodsToRewrite)

private fun findMethodNameFromParameter(methodPackageName: String,
                                        methodName: String,
                                        methodParameters: List<ParameterToExport>,
                                        methodsToRewrite: Set<MethodToRewrite>): String =
        findMethodName(methodPackageName,
                methodName,
                methodParameters.map { it.type },
                methodsToRewrite)

private fun findMethodName(methodPackageName: String,
                           methodName: String,
                           methodParameters: List<Type>,
                           methodsToRewrite: Set<MethodToRewrite>): String {
    val isDuplicated = methodsToRewrite.stream()
            .filter { it.packageName == methodPackageName }
            .filter { it.name == it.name }
            .findAny()
            .isPresent
    return if (isDuplicated) {
        val par = methodParameters
                .stream()
                .map { it.packageName.replace("/", "_") + "_" + it.name }
                .collect(Collectors.joining("_"))
        methodName + par
    } else {
        methodName
    }
}

private fun isInstance(expressionValue: String, expressionType: Type): String =
        if (expressionType == nothingType) {
            "$expressionValue is None"
        } else {
            "isinstance($expressionValue, ${findPythonType(expressionType)})"
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
