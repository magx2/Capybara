package com.magx2.capybara.export.python

import com.magx2.capybara.*
import com.magx2.capybara.export.CapybaraExport
import org.slf4j.LoggerFactory
import java.io.File
import java.nio.file.Paths
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList


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
        val imports = concat(
                findImportsForFunctions(unit),
                findImportsForDefs(unit))
                .distinct()
                .map { packageToPythonPackage(it) }
                .map { "import $it" }

        val structs = unit.structs
                .stream()
                .filter { it.type != BasicTypes.intType }
                .filter { it.type != BasicTypes.floatType }
                .filter { it.type != BasicTypes.booleanType }
                .filter { it.type != BasicTypes.listType }
                .filter { it.type != BasicTypes.stringType }
                .filter { it.type != BasicTypes.nothingType }
                .filter { it.type != BasicTypes.anyType }
                .filter { it.type != BasicTypes.lambdaType }
                .map { struct -> structToPython(struct) }

        val functions = unit.functions
                .stream()
                .map { function -> functionToPython(function, assertions, unit.unions, methodsToRewrite, unit.packageName) }

        val defs = unit.defs
                .stream()
                .map { def -> defToPython(def, assertions, unit.unions, methodsToRewrite, unit.packageName) }

        // TODO add this main generation to `defToPython`
        val main = unit.defs
                .stream()
                .filter { it.name == "main" }
                .filter { it.parameters.size == 1 }
                .filter { isListOfStrings(it.parameters.first().type) }
                .map { def ->
                    val name = "abc"/*findMethodNameFromParameter(
                            def.packageName,
                            def.name,
                            def.parameters,
                            methodsToRewrite)*/
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

    private fun findImportsForFunctions(unit: CompileUnitToExport): Stream<String> =
            unit.functions
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

    private fun findImportsForDefs(unit: CompileUnitToExport): Stream<String> =
            unit.defs
                    .stream()
                    .filter { def -> def is DefToExport }
                    .map { def -> def as DefToExport }
                    .flatMap { def ->
                        concat(
                                findImports(def.returnExpression, unit.packageName).stream(),
                                def.statements
                                        .stream()
                                        .flatMap { statement -> findImportsForStatement(statement, unit.packageName) }
                        )
                    }

    private fun findImportsForStatement(statement: StatementWithType, packageName: String): Stream<String> =
            when (statement) {
                is AssigmentStatementWithType -> findImports(statement.expression, packageName).stream()
                is AssertStatementWithType -> {
                    if (assertions) {
                        concat(
                                findImports(statement.checkExpression, packageName).stream(),
                                findImports(statement.messageExpression, packageName).stream()
                        )
                    } else {
                        Stream.empty()
                    }
                }
                is WhileStatementWithType -> {
                    concat(
                            findImports(statement.condition, packageName).stream(),
                            statement.statements
                                    .stream()
                                    .flatMap { it -> findImportsForStatement(it, packageName) })
                }
                is DefCallStatementWithType ->
                    statement.parameters
                            .stream()
                            .flatMap { findImports(it, packageName).stream() }
            }
}

private fun writeAllToPackageFile(outputDir: String, packageName: String, texts: List<String>) {
    val text = java.lang.String.join("\n\n", texts)
    val fileName = "$outputDir$packageName.py"
    File(fileName).also {
        it.parentFile.mkdirs()
        log.info("Saving to file `${it.absolutePath}`")
        it.writeText(text)
    }
}

private fun findImports(expresion: ExpressionWithReturnType?, packageName: String): List<String> =
        if (expresion != null) {
            when (expresion) {
                is ParameterExpressionWithReturnType -> emptyList()
                is IntegerExpressionWithReturnType -> emptyList()
                is FloatExpressionWithReturnType -> emptyList()
                is BooleanExpressionWithReturnType -> emptyList()
                is StringExpressionWithReturnType -> emptyList()
                NothingExpressionWithReturnType -> emptyList()
                is FunctionInvocationExpressionWithReturnType ->
                    when (expresion.functionInvocation) {
                        is FunctionInvocationByNameWithReturnType -> {
                            exportMethodInvocation(expresion.parameters, expresion.functionInvocation.packageName, packageName)
                        }
                        is FunctionInvocationByExpressionWithReturnType ->
                            findImports(expresion.functionInvocation.expression, packageName)
                    }
                is DefInvocationExpressionWithReturnType ->
                    exportMethodInvocation(expresion.parameters, expresion.packageName, packageName)
                is InfixExpressionWithReturnType -> findImports(expresion.left, packageName) + findImports(expresion.right, packageName)
                is IfExpressionWithReturnType -> findImports(expresion.condition, packageName) + findImports(expresion.trueBranch.expression, packageName) + findImports(expresion.falseBranch.expression, packageName)
                is NegateExpressionWithReturnType -> findImports(expresion.negateExpression, packageName)
                is NewStructExpressionWithReturnType ->
                    if (packageName != expresion.returnType.packageName) {
                        listOf(expresion.returnType.packageName)
                    } else {
                        emptyList()
                    }
                is ValueExpressionWithReturnType -> emptyList()
                is LambdaExpressionWithReturnType ->
                    expresion.assignments
                            .stream()
                            .map { it.expression }
                            .flatMap { findImports(it, packageName).stream() }
                            .toList() + findImports(expresion.expression, packageName)
                is StructFieldAccessExpressionWithReturnType -> findImports(expresion.structureExpression, packageName)
                is NewListExpressionWithReturnType -> expresion.elements.flatMap { findImports(it, packageName) }
                is AssertExpressionWithReturnType -> findImports(expresion.checkExpression, packageName) + findImports(expresion.returnExpression, packageName) + (if (expresion.messageExpression != null) findImports(expresion.messageExpression, packageName) else emptyList())
                is StructureAccessExpressionWithReturnType -> findImports(expresion.structureIndex, packageName)
                is IsExpressionWithReturnType -> emptyList()
            }
        } else {
            emptyList()
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


fun generateDocForDef(
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


// TODO cahnge name
private fun long(assignments: List<AssigmentStatementWithType>,
                 returnExpression: ExpressionWithReturnType,
                 assertions: Boolean,
                 unions: Set<UnionWithType>,
                 methodsToRewrite: Set<MethodToRewrite>,
                 packageName: String,
                 indent: Int = 1,
                 depth: Int): String {
    val assignmentsInPython = assignments.stream()
            .map { assignmentToPython(it, assertions, unions, methodsToRewrite, packageName, indent, depth) }
            .collect(Collectors.joining("\n")) ?: "\n"
    val returnExpressionInPython = returnExpressionToPython(returnExpression, assertions, unions, methodsToRewrite, packageName, indent, depth)

    return assignmentsInPython + returnExpressionInPython
}




