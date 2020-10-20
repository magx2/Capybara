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
                   private val assertions: Boolean) : CapybaraExport {
    private val initFilesDirectories = HashSet<String>()

    override fun export(units: Set<CompileUnitToExport>) {
        val methodsToRewrite = units.stream()
                .flatMap { findMethodsToRewrite(it) }
                .toList()
                .toSet()

        units.stream()
                .map { rewriteDuplicateMethodNames(it, methodsToRewrite) }
                .forEach { export(it) }
    }

    private fun export(unit: CompileUnitToExport) {
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
                .map { function -> functionToPython(function, assertions, unit.unions, unit.packageName) }

        val defs = unit.defs
                .stream()
                .map { def -> defToPython(def, assertions, unit.unions, unit.packageName) }

        val main = unit.defs
                .stream()
                .filter { it.name == "main" || it.name == "main_capybara_type_List_capybara_type_String" }
                .filter { it.parameters.size == 1 }
                .filter { isListOfStrings(it.parameters.first().type) }
                .map { def ->
                    """
                    |${'\n'}if __name__ == "__main__":
                    |${'\t'}import sys
                    |${'\t'}${def.name}(sys.argv)
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

    private fun rewriteDuplicateMethodNames(unit: CompileUnitToExport, methodsToRewrite: Set<MethodToRewrite>): CompileUnitToExport {
        val functions = unit.functions
                .stream()
                .map { function ->
                    if (shouldRewrite(function.packageName, function.name, function.parameters.map { it.type }, methodsToRewrite)) {
                        val name = findMethodName(function.name, function.parameters)
                        function.copy(name = name)
                    } else {
                        function
                    }
                }
                .map { function ->
                    function.copy(
                            returnExpression = rewriteExpression(function.returnExpression, methodsToRewrite),
                            assignments = function.assignments.map { rewriteAssignment(it, methodsToRewrite) })
                }
                .toList()
                .toSet()
        val defs = unit.defs
                .stream()
                .map { def ->
                    if (shouldRewrite(unit.packageName, def.name, def.parameters.map { it.type }, methodsToRewrite)) {
                        val name = findMethodName(def.name, def.parameters)
                        when (def) {
                            is DefToExport -> def.copy(name = name)
                            is NativeDefToExport -> def.copy(name = name)
                        }
                    } else {
                        def
                    }
                }
                .map { def ->
                    when (def) {
                        is DefToExport -> {
                            val newReturnExpression = if (def.returnExpression != null) {
                                rewriteExpression(def.returnExpression, methodsToRewrite)
                            } else {
                                null
                            }
                            def.copy(
                                    returnExpression = newReturnExpression,
                                    statements = def.statements.map { rewriteStatement(it, methodsToRewrite) })
                        }
                        is NativeDefToExport -> def
                    }
                }
                .toList()
                .toSet()
        return unit.copy(functions = functions, defs = defs)
    }

    private fun rewriteAssignment(assigment: AssigmentStatementWithType, methodsToRewrite: Set<MethodToRewrite>) =
            assigment.copy(expression = rewriteExpression(assigment.expression, methodsToRewrite))

    private fun rewriteIfBranch(branch: IfBranchWithReturnType, methodsToRewrite: Set<MethodToRewrite>) =
            branch.copy(
                    expression = rewriteExpression(branch.expression, methodsToRewrite),
                    assignments = branch.assignments.map { rewriteAssignment(it, methodsToRewrite) })

    private fun rewriteStatement(statement: StatementWithType, methodsToRewrite: Set<MethodToRewrite>): StatementWithType =
            when (statement) {
                is AssigmentStatementWithType -> rewriteAssignment(statement, methodsToRewrite)
                is AssertStatementWithType -> {
                    val message = if (statement.messageExpression != null) {
                        rewriteExpression(statement.messageExpression, methodsToRewrite)
                    } else {
                        null
                    }
                    statement.copy(
                            checkExpression = rewriteExpression(statement.checkExpression, methodsToRewrite),
                            messageExpression = message)
                }
                is WhileStatementWithType ->
                    statement.copy(
                            condition = rewriteExpression(statement.condition, methodsToRewrite),
                            statements = statement.statements.map { rewriteStatement(it, methodsToRewrite) }
                    )
                is DefCallStatementWithType -> {
                    val newParameters = statement.parameters.map { rewriteExpression(it, methodsToRewrite) }
                    val rewriteTo = rewriteTo(statement.packageName, statement.defName, statement.parameters.map { it.returnType }, methodsToRewrite)
                    if (rewriteTo.isPresent) {
                        statement.copy(
                                defName = rewriteTo.get().newName,
                                parameters = newParameters)
                    } else {
                        statement.copy(parameters = newParameters)
                    }
                }
            }

    private fun rewriteExpression(expression: ExpressionWithReturnType, methodsToRewrite: Set<MethodToRewrite>): ExpressionWithReturnType =
            when (expression) {
                is FunctionInvocationExpressionWithReturnType -> {
                    val newParameters = expression.parameters.map { rewriteExpression(it, methodsToRewrite) }
                    when (expression.functionInvocation) {
                        is FunctionInvocationByNameWithReturnType -> {
                            val shouldRewrite = rewriteTo(
                                    expression.functionInvocation.packageName,
                                    expression.functionInvocation.functionName,
                                    expression.parameters.map { it.returnType },
                                    methodsToRewrite)
                            if (shouldRewrite.isPresent) {
                                expression.copy(
                                        parameters = newParameters,
                                        functionInvocation = expression.functionInvocation
                                                .copy(functionName = shouldRewrite.get().newName))
                            } else {
                                expression.copy(parameters = newParameters)
                            }
                        }
                        is FunctionInvocationByExpressionWithReturnType ->
                            expression.copy(
                                    parameters = newParameters,
                                    functionInvocation = expression.functionInvocation
                                            .copy(expression = rewriteExpression(expression.functionInvocation.expression, methodsToRewrite)))
                    }
                }
                is DefInvocationExpressionWithReturnType ->
                    expression.copy(parameters = expression.parameters.map { rewriteExpression(it, methodsToRewrite) })
                is InfixExpressionWithReturnType ->
                    expression.copy(
                            left = rewriteExpression(expression.left, methodsToRewrite),
                            right = rewriteExpression(expression.right, methodsToRewrite))
                is IfExpressionWithReturnType ->
                    expression.copy(
                            condition = rewriteExpression(expression.condition, methodsToRewrite),
                            trueBranch = rewriteIfBranch(expression.trueBranch, methodsToRewrite),
                            falseBranch = rewriteIfBranch(expression.falseBranch, methodsToRewrite))
                is NegateExpressionWithReturnType ->
                    expression.copy(negateExpression = rewriteExpression(expression.negateExpression, methodsToRewrite))
                is NewStructExpressionWithReturnType ->
                    expression.copy(
                            fields = expression.fields
                                    .stream()
                                    .map { it.copy(value = rewriteExpression(it.value, methodsToRewrite)) }
                                    .toList()
                    )
                is LambdaExpressionWithReturnType ->
                    expression.copy(
                            assignments = expression.assignments.map { rewriteAssignment(it, methodsToRewrite) },
                            expression = rewriteExpression(expression.expression, methodsToRewrite)
                    )
                is StructFieldAccessExpressionWithReturnType ->
                    expression.copy(
                            structureExpression = rewriteExpression(expression.structureExpression, methodsToRewrite))
                is NewListExpressionWithReturnType ->
                    expression.copy(
                            elements = expression.elements.map { rewriteExpression(it, methodsToRewrite) })
                is AssertExpressionWithReturnType ->
                    if (expression.messageExpression != null) {
                        expression.copy(
                                checkExpression = rewriteExpression(expression.checkExpression, methodsToRewrite),
                                returnExpression = rewriteExpression(expression.returnExpression, methodsToRewrite),
                                messageExpression = rewriteExpression(expression.messageExpression, methodsToRewrite))
                    } else {
                        expression.copy(
                                checkExpression = rewriteExpression(expression.checkExpression, methodsToRewrite),
                                returnExpression = rewriteExpression(expression.returnExpression, methodsToRewrite))
                    }
                is StructureAccessExpressionWithReturnType ->
                    expression.copy(structureIndex = rewriteExpression(expression.structureIndex, methodsToRewrite))
                is ParameterExpressionWithReturnType,
                is IntegerExpressionWithReturnType,
                is FloatExpressionWithReturnType,
                is BooleanExpressionWithReturnType,
                is StringExpressionWithReturnType,
                NothingExpressionWithReturnType,
                is IsExpressionWithReturnType,
                is NativeExpressionWithReturnType,
                is ValueExpressionWithReturnType -> expression
            }

    private fun rewriteTo(packageName: String, name: String, parameters: List<Type>, methodsToRewrite: Set<MethodToRewrite>) =
            methodsToRewrite.stream()
                    .filter { it.packageName == packageName }
                    .filter { it.originalName == name }
                    .filter { parametersAreEqual(it.parameters, parameters) }
                    .findAny()

    private fun shouldRewrite(packageName: String, name: String, parameters: List<Type>, methodsToRewrite: Set<MethodToRewrite>) =
            rewriteTo(packageName, name, parameters, methodsToRewrite).isPresent


    private data class MethodToRewrite(val packageName: String, val originalName: String, val newName: String, val parameters: List<Type>)

    private data class Method(val name: String, val parameters: List<ParameterToExport>)

    private fun findMethodsToRewrite(unit: CompileUnitToExport): Stream<MethodToRewrite> =
            concat(
                    unit.functions
                            .stream()
                            .map { Method(it.name, it.parameters) },
                    unit.defs
                            .stream()
                            .map { Method(it.name, it.parameters) })
                    .collect(Collectors.groupingBy { it.name })
                    .entries
                    .stream()
                    .map { it.value }
                    .filter { it.size > 1 }
                    .flatMap { it.stream() }
                    .map { MethodToRewrite(unit.packageName, it.name, findMethodName(it.name, it.parameters), it.parameters.map { p -> p.type }) }
                    .distinct()

    private fun findMethodName(methodName: String,
                               methodParameters: List<ParameterToExport>): String {
        val parameters = methodParameters
                .stream()
                .map { it.type }
                .map { typeToMethodName(it) }
                .collect(Collectors.joining("_"))
        return methodName + parameters
    }

    private fun typeToMethodName(type: Type): String =
            type.packageName.replace("/", "_") + "_" + type.name +
                    type.genericTypes.stream().map { typeToMethodName(it) }.collect(Collectors.joining("_"))

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
                is NativeExpressionWithReturnType -> emptyList()
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

private fun parametersAreEqual(first: List<Type>, second: List<Type>): Boolean =
        if (first.size == second.size) {
            var equal = true
            for (i in first.indices) {
                val firstParam = first[i]
                val secondParam = second[i]
                if (firstParam != secondParam) {
                    equal = false
                    break
                }
            }
            equal
        } else {
            false
        }