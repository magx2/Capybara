package com.magx2.capybara

import CapybaraBaseListener
import CapybaraLexer
import CapybaraParser
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.slf4j.LoggerFactory
import java.io.FileInputStream
import java.util.*
import java.util.stream.Collectors
import java.util.stream.Collectors.toSet
import java.util.stream.Stream
import kotlin.collections.ArrayList
import kotlin.streams.toList

private val log = LoggerFactory.getLogger(CapybaraCompiler::class.java)

interface CapybaraCompiler {
    companion object {
        fun instance(): CapybaraCompiler = CapybaraCompilerImpl()
    }

    fun compile(fileName: String): CompileUnit
}

data class CompileUnit(
        val originFile: String,
        val packageName: String,
        val imports: List<Import>,
        val structs: Set<Struct>,
        val unions: Set<Union>,
        val functions: Set<Function>,
        val defs: Set<AbstractDef>)

data class CompileUnitWithImports(
        val packageName: String,
        val structs: Set<Struct>,
        val unions: Set<Union>,
        val functions: Set<Function>,
        val defs: Set<AbstractDef>,
        val importStructs: List<Struct>,
        val importUnions: List<Union>,
        val importFunctions: List<Function>,
        val importDefs: List<AbstractDef>,
)

data class CompileUnitWithFlatStructs(
        val packageName: String,
        val structs: Set<FlatStruct>,
        val unions: Set<UnionWithType>,
        val functions: Set<Function>,
        val defs: Set<AbstractDef>,
        val importStructs: List<Struct>,
        val importUnions: List<Union>,
        val importFunctions: List<Function>,
        val importDefs: List<AbstractDef>,
)

fun getTypes(units: Collection<CompileUnitWithImports>, packageName: String): Set<Type> =
        units.stream()
                .filter { it.packageName == packageName }
                .flatMap { unit ->
                    Stream.concat(
                            unit.structs.stream().map { it.type },
                            unit.unions.stream().map { it.type })
                }
                .toList()
                .toSet()


fun getTypes(context: CompilationContext, packageName: String): Set<Type> =
        Stream.concat(
                context.structs.stream().map { it.type },
                context.unions.stream().map { it.type })
                .filter { it.packageName == packageName }
                .toList()
                .toSet()


data class Export(
        val packageName: String,
        val structs: Set<Struct>,
        val unions: Set<Union>,
        val functions: Set<Function>,
        val defs: Set<AbstractDef>,
)

data class Import(val codeMetainfo: CodeMetainfo, val importPackage: String, val subImport: Set<String>)

sealed class Field
data class BasicField(val codeMetainfo: CodeMetainfo, val name: String, val type: String) : Field()
data class SpreadField(val codeMetainfo: CodeMetainfo, val spreadType: String) : Field()

sealed class AbstractDef(open val codeMetainfo: CodeMetainfo,
                         open val packageName: String,
                         open val name: String,
                         open val returnTypeCodeMetainfo: CodeMetainfo?,
                         open val returnType: String?,
                         open val parameters: List<Parameter>)

data class Def(override val codeMetainfo: CodeMetainfo,
               override val packageName: String,
               override val name: String,
               override val returnTypeCodeMetainfo: CodeMetainfo?,
               override val returnType: String?,
               override val parameters: List<Parameter>,
               val statements: List<Statement>,
               val returnExpression: Expression?) : AbstractDef(codeMetainfo, packageName, name, returnTypeCodeMetainfo, returnType, parameters)

data class NativeDef(override val codeMetainfo: CodeMetainfo,
                     override val packageName: String,
                     override val name: String,
                     override val returnTypeCodeMetainfo: CodeMetainfo?,
                     override val returnType: String?,
                     override val parameters: List<Parameter>,
                     val nativeStatements: List<String>) : AbstractDef(codeMetainfo, packageName, name, returnTypeCodeMetainfo, returnType, parameters)

data class Parameter(val codeMetainfo: CodeMetainfo, val name: String, val type: String)
data class TypedParameter(val name: String, val type: Type)

fun parseTypedParameter(parameter: Parameter,
                        compilationContext: CompilationContext,
                        unit: CompileUnitWithFlatStructs): TypedParameter =
        TypedParameter(
                parameter.name,
                parseType(
                        parameter.codeMetainfo,
                        parameter.type,
                        getTypes(compilationContext, unit.packageName) + importsToTypes(unit)))


private class CapybaraCompilerImpl : CapybaraCompiler {
    override fun compile(fileName: String): CompileUnit {
        log.info("Compiling `{}`", fileName)
        val fis = FileInputStream(fileName)
        val input: CharStream = ANTLRInputStream(fis)
        val lexer = CapybaraLexer(input)
        val tokens = CommonTokenStream(lexer)
        val parser = CapybaraParser(tokens)
        val tree: ParseTree = parser.compileUnit()
        fis.close()
        val walker = ParseTreeWalker()
        val listener = Listener(fileName)
        walker.walk(listener, tree)

        return CompileUnit(
                fileName,
                listener.packageName,
                listener.imports,
                listener.structs.toSet(),
                listener.unions.toSet(),
                listener.functions.toSet(),
                listener.defs.toSet())
    }
}

private class Listener(private val fileName: String) : CapybaraBaseListener() {
    lateinit var packageName: String
    val imports = LinkedList<Import>()
    val structs = LinkedList<Struct>()
    val unions = LinkedList<Union>()
    val functions = LinkedList<Function>()
    val defs = LinkedList<AbstractDef>()

    var parameters: Map<String, String> = mapOf()
    val assignments: MutableList<AssigmentStatement> = ArrayList()
    var returnExpression: Expression? = null
    val statements = ArrayList<Statement>()

    override fun enterPackageDeclaration(ctx: CapybaraParser.PackageDeclarationContext) {
        packageName = ctx.PACKAGE().text
    }

    override fun enterImports(ctx: CapybaraParser.ImportsContext) {
        val subImport = (ctx.sub_import() ?: listOf())
                .stream()
                .map { it.struct_name ?: it.fun_name }
                .map { it.text }
                .collect(toSet())
        imports.add(Import(parseCodeMetainfo(fileName, ctx.start), ctx.package_.text, subImport))
    }

    override fun enterStruct(ctx: CapybaraParser.StructContext) {
        structs.add(Struct(parseCodeMetainfo(fileName, ctx.start), Type(packageName, ctx.name.text), LinkedList()))
    }

    override fun exitUnion(ctx: CapybaraParser.UnionContext) {
        unions.add(Union(
                parseCodeMetainfo(fileName, ctx.start),
                Type(packageName, ctx.name.text),
                ctx.unionField()
                        .stream()
                        .map { it.fullyQualifiedType() }
                        .map {
                            TypeWithoutPackage(
                                    it.type_package?.text,
                                    it.name.text
                            )
                        }
                        .toList()
                        .toSet()
        ))
    }

    override fun enterField(ctx: CapybaraParser.FieldContext) {
        val field = if (ctx.name != null) {
            BasicField(parseCodeMetainfo(fileName, ctx.start), ctx.name.text, ctx.type.text)
        } else {
            SpreadField(parseCodeMetainfo(fileName, ctx.start), ctx.spread_type.text)
        }
        structs.last().fields.add(field)
    }

    override fun enterFun_(ctx: CapybaraParser.Fun_Context) {
        this.parameters = findListOfParametersInFun(ctx.listOfParameters())
                .stream()
                .collect(Collectors.toMap(
                        (java.util.function.Function<Parameter, String> { it.name }),
                        (java.util.function.Function<Parameter, String> { it.type })))
        if (ctx.returnExpression != null) {
            returnExpression = parseExpression(ctx.returnExpression, parameters)
        }
    }

    override fun exitFun_(ctx: CapybaraParser.Fun_Context) {
        val parameters = findListOfParametersInFun(ctx.listOfParameters())
        functions.add(Function(
                CodeMetainfo(fileName, ctx.name.line, ctx.name.charPositionInLine), // you want to point to method name not `fun` keyword
                packageName,
                ctx.name.text,
                ctx.returnType?.text,
                parameters,
                ArrayList(assignments),
                returnExpression!!))
        assignments.clear()
        returnExpression = null
    }

    override fun enterDef_(ctx: CapybaraParser.Def_Context) {
        this.parameters = findListOfParametersInFun(ctx.listOfParameters())
                .stream()
                .collect(Collectors.toMap(
                        (java.util.function.Function<Parameter, String> { it.name }),
                        (java.util.function.Function<Parameter, String> { it.type })))
    }

    override fun exitDef_(ctx: CapybaraParser.Def_Context) {
        val parameters = findListOfParametersInFun(ctx.listOfParameters())
        defs.add(Def(
                parseCodeMetainfo(fileName, ctx.start),
                packageName,
                ctx.name.text,
                if (ctx.returnType != null) parseCodeMetainfo(fileName, ctx.returnType.start) else null,
                ctx.returnType?.text,
                parameters,
                statements.toList(),
                returnExpression))
        returnExpression = null
        statements.clear()
        assignments.clear()
    }

    override fun enterNative_python(ctx: CapybaraParser.Native_pythonContext) {
        val nativeCode = ctx.native_code.text.replace("{{{", "").replace("}}}", "")
        val nativeStatements = nativeCode.split("\n").filter { it.isNotBlank() }

        val parameters = findListOfParametersInFun(ctx.listOfParameters())
        defs.add(NativeDef(
                parseCodeMetainfo(fileName, ctx.start),
                packageName,
                ctx.name.text,
                if (ctx.returnType != null) parseCodeMetainfo(fileName, ctx.returnType.start) else null,
                ctx.returnType?.text,
                parameters,
                nativeStatements))
        returnExpression = null
        statements.clear()
        assignments.clear()
    }

    private fun findListOfParametersInFun(ctx: CapybaraParser.ListOfParametersContext?): List<Parameter> =
            ctx?.parameter()
                    ?.stream()
                    ?.map { newParameter(it) }
                    ?.toList() ?: listOf()

    private fun newParameter(it: CapybaraParser.ParameterContext): Parameter {
        val codeMetainfo = parseCodeMetainfo(fileName, it.start)
        val type = it.type.text
        return if (it.name != null) {
            Parameter(codeMetainfo, it.name.text, type)
        } else {
            val regex = "(?:/[a-z][a-z_0-9]*)*/?([A-Z][a-zA-Z0-9]*)".toRegex()
            val rawType = regex.find(type)?.groupValues?.get(1) ?: error("")
            Parameter(codeMetainfo, rawType.camelToSnakeCase(), type)
        }
    }

    override fun enterFunBody(ctx: CapybaraParser.FunBodyContext) {
        if (ctx.assignment()?.assign_to != null) {
            assignments.add(parseAssignment(ctx.assignment()))
        } else {
            returnExpression = parseExpression(ctx.expression(), parameters)
        }
    }

    private fun parseAssignment(ctx: CapybaraParser.AssignmentContext): AssigmentStatement =
            AssigmentStatement(
                    parseCodeMetainfo(fileName, ctx.start),
                    ctx.assign_to.text,
                    parseExpression(ctx.expression(), parameters),
                    if (ctx.assignment_type != null) parseCodeMetainfo(fileName, ctx.assignment_type?.start!!) else null,
                    ctx.assignment_type?.text)

    private fun parseExpression(ctx: CapybaraParser.ExpressionContext, parametersMap: Map<String, String>): Expression =
            when {
                ctx.in_parenthisis_expression != null -> {
                    ParenthesisExpression(parseCodeMetainfo(fileName, ctx.start), parseExpression(ctx.in_parenthisis_expression, parametersMap))
                }
                ctx.value != null -> {
                    val valueName = ctx.value.text
                    when {
                        parametersMap.containsKey(valueName) -> {
                            ParameterExpression(
                                    parseCodeMetainfo(fileName, ctx.start),
                                    valueName,
                                    parametersMap[valueName]!!
                            )
                        }
                        else -> {
                            ValueExpression(parseCodeMetainfo(fileName, ctx.start), valueName)
                        }
                    }
                }
                ctx.lambda != null -> {
                    val codeMetainfo = parseCodeMetainfo(fileName, ctx.start)
                    val assignments = ctx.lambda.lambda_body.assignment().map { parseAssigmentStatement(it, parametersMap) }
                    val expression = ctx.lambda.lambda_body.expression()
                    val parameters = findListOfParametersInFun(ctx.lambda.listOfParameters())
                    val newParameters = parametersMap +
                            parameters.stream()
                                    .collect(Collectors.toMap({ it.name }, { it.type }))
                    LambdaExpression(
                            codeMetainfo,
                            parameters,
                            assignments,
                            parseExpression(expression, newParameters))
                }
                ctx.structure_expression != null -> {
                    StructFieldAccessExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            parseExpression(ctx.structure_expression, parametersMap),
                            ctx.field_name.text)
                }
                ctx.constant() != null -> {
                    when {
                        ctx.constant().BOOLEAN() != null -> BooleanExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().BOOLEAN().text!!.toBoolean())
                        ctx.constant().FLOAT() != null -> FloatExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().FLOAT().text.toDouble())
                        ctx.constant().INTEGER() != null -> IntegerExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().INTEGER().text.toLong())
                        ctx.constant().string != null -> StringExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().string.text.substring(1 until (ctx.constant().string.text.length - 1)))
                        ctx.constant().NOTHING() != null -> NothingExpression(parseCodeMetainfo(fileName, ctx.start))
                        else -> throw IllegalStateException("I don't know how to handle this constant expression: `${ctx.constant().text}`!")
                    }
                }
                ctx.function_qualified_name != null -> {
                    val parameters = (ctx.parameters()
                            ?.expression() ?: listOf())
                            .stream()
                            .map { parseExpression(it, parametersMap) }
                            .toList()
                    FunctionInvocationExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            FunctionInvocationByName(ctx.function_qualified_name.package_?.text,
                                    ctx.function_qualified_name.function_name.text),
                            parameters)
                }
                ctx.normal_function_invocation != null -> {
                    val parameters = (ctx.parameters()
                            ?.expression() ?: listOf())
                            .stream()
                            .map { parseExpression(it, parametersMap) }
                            .toList()
                    FunctionInvocationExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            buildFunctionInvocationExpression(ctx.normal_function_invocation, parametersMap),
                            parameters)
                }
                ctx.infix_operation() != null -> {
                    InfixExpression(parseCodeMetainfo(fileName, ctx.start), ctx.infix_operation().text, parseExpression(ctx.left, parametersMap), parseExpression(ctx.right, parametersMap))
                }
                ctx.condition != null -> {
                    IfExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            parseExpression(ctx.condition, parametersMap),
                            parseIfBranch(ctx.assignments_true, parametersMap),
                            parseIfElseExpression(
                                    ctx.if_else(),
                                    ctx.assignments_false,
                                    parametersMap))
                }
                ctx.argument_to_function != null -> {
                    val argumentToFunction = ctx.argument_to_function
                    val functionExpression = ctx.apply_to_function_expression
                    when {
                        argumentToFunction.apply_to_function_qualified_name != null ->
                            FunctionInvocationExpression(
                                    parseCodeMetainfo(fileName, ctx.start),
                                    FunctionInvocationByName(ctx.apply_to_function_qualified_name.package_?.text,
                                            ctx.apply_to_function_qualified_name.function_name.text),
                                    listOf(parseExpression(argumentToFunction, parametersMap)))
                        functionExpression != null -> {
                            val codeMetainfo = parseCodeMetainfo(fileName, ctx.start)
                            FunctionInvocationExpression(
                                    codeMetainfo,
                                    buildFunctionInvocationExpression(functionExpression, parametersMap),
                                    listOf(parseExpression(argumentToFunction, parametersMap)))
                        }
                        else -> throw CompilationException(parseCodeMetainfo(fileName, argumentToFunction.start), "I don't know how to handle this!")
                    }
                }
                ctx.negate_expression != null -> NegateExpression(parseCodeMetainfo(fileName, ctx.start), parseExpression(ctx.negate_expression, parametersMap))
                ctx.struct_name != null ->
                    NewStruct(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.struct_name.type_package?.text,
                            ctx.struct_name.name.text,
                            ctx.struct_field_initializations()
                                    .struct_field_initialization()
                                    .stream()
                                    .map { StructField(parseCodeMetainfo(fileName, ctx.start), it.field_name.text, parseExpression(it.field_value, parametersMap)) }
                                    .toList()
                    )
                ctx.newListExpression() != null -> {
                    NewListExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.newListExpression()
                                    .expression()
                                    .stream()
                                    .map { parseExpression(it, parametersMap) }
                                    .toList()
                    )
                }
                ctx.structureAccessExpression() != null -> {
                    val name = ctx.structureAccessExpression().structure_name.text
                    if (parametersMap.containsKey(name)) {
                        StructureAccessExpression(
                                parseCodeMetainfo(fileName, ctx.start),
                                name,
                                parseExpression(ctx.structureAccessExpression().structure_index, parametersMap),
                                parseCodeMetainfo(fileName, ctx.structureAccessExpression().structure_index.start),
                                parametersMap[name])
                    } else {
                        StructureAccessExpression(
                                parseCodeMetainfo(fileName, ctx.start),
                                name,
                                parseExpression(ctx.structureAccessExpression().structure_index, parametersMap),
                                parseCodeMetainfo(fileName, ctx.structureAccessExpression().structure_index.start),
                                null)
                    }
                }
                ctx.is_value != null ->
                    IsExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.is_value.text,
                            parseCodeMetainfo(fileName, ctx.is_type.start),
                            ctx.is_type.text)
                ctx.check_expression != null -> AssertExpression(
                        parseCodeMetainfo(fileName, ctx.start),
                        parseExpression(ctx.check_expression, parametersMap),
                        parseExpression(ctx.return_expression, parametersMap),
                        if (ctx.message_expression != null) parseExpression(ctx.message_expression, parametersMap) else null
                )
                else -> throw IllegalStateException("I don't know how to handle `${ctx.text}`!")
            }

    private fun parseIfElseExpression(ifElse: List<CapybaraParser.If_elseContext>?,
                                      falseAssignments: CapybaraParser.AssignmentsContext,
                                      parametersMap: Map<String, String>): IfBranch =
            if (ifElse == null || ifElse.isEmpty()) {
                parseIfBranch(falseAssignments, parametersMap)
            } else {
                val first = ifElse[0]
                val assignmentsNextTrue = first.assignments_next_true
                IfBranch(
                        IfExpression(
                                parseCodeMetainfo(fileName, first.start),
                                parseExpression(first.next_condition, parametersMap),
                                parseIfBranch(assignmentsNextTrue, parametersMap),
                                parseIfElseExpression(ifElse.drop(1), falseAssignments, parametersMap)
                        ), emptyList())
            }

    private fun parseIfBranch(ctx: CapybaraParser.AssignmentsContext, parametersMap: Map<String, String>) =
            IfBranch(
                    parseExpression(ctx.expression(), parametersMap),
                    (ctx.assignment() ?: emptyList())
                            .stream()
                            .map { parseAssignment(it) }
                            .toList()
            )

    private fun buildFunctionInvocationExpression(functionExpression: CapybaraParser.ExpressionContext,
                                                  parametersMap: Map<String, String>): FunctionInvocation =
            FunctionInvocationByExpression(parseExpression(functionExpression, parametersMap))

    override fun enterDefBody(ctx: CapybaraParser.DefBodyContext) {
        if (ctx.statement() != null) {
            val sts = ctx.statement()
                    .stream()
                    .map { parseStatement(it, parameters) }
                    .toList()
            statements.addAll(sts)
        }
        if (ctx.return_expression != null) {
            returnExpression = parseExpression(ctx.return_expression, parameters)
        }
    }

    private fun parseStatement(statement: CapybaraParser.StatementContext, parametersMap: Map<String, String>): Statement =
            when {
                statement.assignment() != null -> parseAssigmentStatement(statement.assignment(), parametersMap)
                statement.while_loop() != null ->
                    WhileLoopStatement(
                            parseCodeMetainfo(fileName, statement.while_loop().start),
                            parseCodeMetainfo(fileName, statement.while_loop().while_.start),
                            parseExpression(statement.while_loop().while_, parametersMap),
                            statement.while_loop()
                                    .loop_body()
                                    .statement()
                                    .stream()
                                    .map { parseStatement(it, parametersMap) }
                                    .toList()
                    )
                statement.for_loop() != null -> {
                    val forLoop = statement.for_loop().for_loop_expression()
                    ForLoopStatement(
                            parseCodeMetainfo(fileName, statement.for_loop().start),
                            if (forLoop.assignment() != null) parseAssigmentStatement(forLoop.assignment(), parametersMap) else null,
                            parseCodeMetainfo(fileName, forLoop.while_.start),
                            parseExpression(forLoop.while_, parametersMap),
                            if (forLoop.each_iteration != null) parseStatement(forLoop.each_iteration, parametersMap) else null,
                            statement.for_loop()
                                    .loop_body()
                                    .statement()
                                    .stream()
                                    .map { parseStatement(it, parametersMap) }
                                    .toList()
                    )
                }
                statement.update_assignment() != null -> {
                    val valueName = statement.update_assignment().assign_to.text
                    AssigmentStatement(
                            parseCodeMetainfo(fileName, statement.update_assignment().start),
                            valueName,
                            InfixExpression(
                                    parseCodeMetainfo(fileName, statement.update_assignment().start),
                                    findOperation(statement.update_assignment().update_action().text),
                                    ValueExpression(parseCodeMetainfo(fileName, statement.update_assignment().start), valueName),
                                    parseExpression(statement.update_assignment().expression(), parametersMap)),
                            parseCodeMetainfo(fileName, statement.update_assignment().assign_to),
                            statement.update_assignment().assign_to.text)
                }
                statement.assert_statement() != null ->
                    AssertStatement(
                            parseCodeMetainfo(fileName, statement.assert_statement().start),
                            parseExpression(statement.assert_statement().check_expression, parametersMap),
                            if (statement.assert_statement().message_expression != null) parseExpression(statement.assert_statement().message_expression, parametersMap) else null,
                    )
                statement.def_call() != null -> {
                    val ctx = statement.def_call()
                    val parameters = (ctx.parameters()
                            ?.expression() ?: listOf())
                            .stream()
                            .map { parseExpression(it, parametersMap) }
                            .toList()
                    DefCallStatement(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.def_qualified_name.package_?.text,
                            ctx.def_qualified_name.function_name.text,
                            parameters)
                }
                else -> throw IllegalStateException("I don't know how to handle it!")
            }

    private fun findOperation(text: String): String = text[0].toString()

    private fun parseAssigmentStatement(assignment: CapybaraParser.AssignmentContext, parametersMap: Map<String, String>) =
            AssigmentStatement(
                    parseCodeMetainfo(fileName, assignment.start),
                    assignment.assign_to.text,
                    parseExpression(assignment.expression(), parametersMap),
                    if (assignment.assign_to != null) parseCodeMetainfo(fileName, assignment.assign_to) else null,
                    assignment.assignment_type?.text)
}


