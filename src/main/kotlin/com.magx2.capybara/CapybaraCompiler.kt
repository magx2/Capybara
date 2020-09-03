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
import kotlin.collections.ArrayList
import kotlin.collections.HashMap
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
        val functions: Set<Function>,
        val defs: Set<Def>)

data class CompileUnitWithImports(
        val packageName: String,
        val structs: Set<Struct>,
        val functions: Set<Function>,
        val importStructs: List<Struct>,
        val importFunctions: List<Function>)

data class CompileUnitWithFlatStructs(
        val packageName: String,
        val structs: Set<FlatStruct>,
        val functions: Set<Function>,
        val importStructs: List<Struct>,
        val importFunctions: List<Function>)

data class Export(val packageName: String, val structs: Set<Struct>, val functions: Set<Function>)

data class Import(val codeMetainfo: CodeMetainfo, val importPackage: String, val subImport: Set<String>)

sealed class Field
data class BasicField(val codeMetainfo: CodeMetainfo, val name: String, val type: String) : Field()
data class SpreadField(val codeMetainfo: CodeMetainfo, val spreadType: String) : Field()


data class Def(val packageName: String,
               val name: String,
               val returnType: String?,
               val parameters: List<Parameter>,
               val statements: List<Statement>,
               val returnExpression: Expression?)

data class DefWithReturnType(val packageName: String,
                             val name: String,
                             val returnType: Type,
                             val parameters: List<Parameter>,
                             val returnExpression: Expression?)

data class Parameter(val name: String, val type: String)
data class TypedParameter(val name: String, val type: Type)

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
                listener.functions.toSet(),
                listener.defs.toSet())
    }
}

private class Listener(private val fileName: String) : CapybaraBaseListener() {
    lateinit var packageName: String
    val imports = ArrayList<Import>()
    val structs = ArrayList<Struct>()
    val functions = ArrayList<Function>()
    val defs = ArrayList<Def>()

    var parameters: Map<String, String> = mapOf()
    var defParameters: Map<String, String> = mapOf()
    val values: HashMap<String, Expression> = HashMap()
    var returnExpression: Expression? = null
    var defReturnExpression: Expression? = null
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
        structs.add(Struct(packageName, ctx.name.text, LinkedList()))
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
            returnExpression = parseExpression(ctx.returnExpression)
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
                values.entries.stream()
                        .map { (k, v) -> AssigmentStatement(k, v) }
                        .toList()
                        .toSet(),
                returnExpression!!))
        values.clear()
        returnExpression = null
    }

    override fun enterDef_(ctx: CapybaraParser.Def_Context) {
        this.defParameters = findListOfParametersInFun(ctx.listOfParameters())
                .stream()
                .collect(Collectors.toMap(
                        (java.util.function.Function<Parameter, String> { it.name }),
                        (java.util.function.Function<Parameter, String> { it.type })))
    }

    override fun exitDef_(ctx: CapybaraParser.Def_Context) {
        val parameters = findListOfParametersInFun(ctx.listOfParameters())
        defs.add(Def(
                packageName,
                ctx.name.text,
                ctx.returnType?.text,
                parameters,
                statements,
                defReturnExpression))
        returnExpression = null
    }

    private fun findListOfParametersInFun(ctx: CapybaraParser.ListOfParametersContext?): List<Parameter> =
            ctx?.parameter()
                    ?.stream()
                    ?.map { newParameter(it) }
                    ?.toList() ?: listOf()

    private fun newParameter(it: CapybaraParser.ParameterContext): Parameter {
        val type = it.type.text
        return if (it.name != null) {
            Parameter(it.name.text, type)
        } else {
            val regex = "(?:/[a-z][a-z_0-9]*)*/?([A-Z][a-zA-Z0-9]*)".toRegex()
            val rawType = regex.find(type)?.groupValues?.get(1) ?: error("")
            Parameter(rawType.decapitalize(), type)
        }
    }

    override fun enterFunBody(ctx: CapybaraParser.FunBodyContext) {
        if (ctx.assigment()?.assign_to != null) {
            values[ctx.assigment().assign_to.text] = parseExpression(ctx.assigment().expression())
        } else {
            returnExpression = parseExpression(ctx.expression())
        }
    }

    private fun parseExpression(ctx: CapybaraParser.ExpressionContext): Expression =
            when {
                ctx.in_parenthisis_expression != null -> {
                    ParenthesisExpression(parseCodeMetainfo(fileName, ctx.start), parseExpression(ctx.in_parenthisis_expression))
                }
                ctx.value != null -> {
                    val valueName = ctx.value.text
                    when {
                        parameters.containsKey(valueName) -> {
                            ParameterExpression(
                                    parseCodeMetainfo(fileName, ctx.start),
                                    valueName,
                                    parameters[valueName]!!
                            )
                        }
                        else -> {
                            ValueExpression(parseCodeMetainfo(fileName, ctx.start), valueName)
                        }
                    }
                }
                ctx.constant() != null -> {
                    when {
                        ctx.constant().BOOLEAN() != null -> BooleanExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().BOOLEAN().text!!.toBoolean())
                        ctx.constant().FLOAT() != null -> FloatExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().FLOAT().text.toDouble())
                        ctx.constant().INTEGER() != null -> IntegerExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().INTEGER().text.toLong())
                        ctx.constant().string != null -> StringExpression(parseCodeMetainfo(fileName, ctx.start), ctx.constant().string.text.substring(1 until (ctx.constant().string.text.length - 1)))
                        else -> throw IllegalStateException("I don't know how to handle it!")
                    }
                }
                ctx.function_qualified_name != null -> {
                    val parameters = (ctx.parameters()
                            ?.expression() ?: listOf())
                            .stream()
                            .map { parseExpression(it) }
                            .toList()
                    FunctionInvocationExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.function_qualified_name.package_?.text,
                            ctx.function_qualified_name.function_name.text,
                            parameters)
                }
                ctx.infix_operation() != null -> {
                    InfixExpression(parseCodeMetainfo(fileName, ctx.start), ctx.infix_operation().text, parseExpression(ctx.left), parseExpression(ctx.right))
                }
                ctx.condition != null -> {
                    IfExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            parseExpression(ctx.condition),
                            parseExpression(ctx.true_expression),
                            parseExpression(ctx.false_expression))
                }
                ctx.argument_to_function != null -> {
                    FunctionInvocationExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.apply_to_function_qualified_name.package_?.text,
                            ctx.apply_to_function_qualified_name.function_name.text,
                            listOf(parseExpression(ctx.argument_to_function)))
                }
                ctx.negate_expression != null -> NegateExpression(parseCodeMetainfo(fileName, ctx.start), parseExpression(ctx.negate_expression))
                ctx.struct_name != null ->
                    NewStruct(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.struct_name.type_package?.text,
                            ctx.struct_name.name.text,
                            ctx.struct_field_initializations()
                                    .struct_field_initialization()
                                    .stream()
                                    .map { StructField(parseCodeMetainfo(fileName, ctx.start), it.field_name.text, parseExpression(it.field_value)) }
                                    .toList()
                    )
                ctx.newListExpression() != null -> {
                    NewListExpression(
                            parseCodeMetainfo(fileName, ctx.start),
                            ctx.newListExpression()
                                    .expression()
                                    .stream()
                                    .map { parseExpression(it) }
                                    .toList()
                    )
                }
                ctx.structureAccessExpression() != null -> {
                    val name = ctx.structureAccessExpression().structure_name.text
                    if (parameters.containsKey(name)) {
                        StructureAccessExpression(
                                parseCodeMetainfo(fileName, ctx.start),
                                name,
                                parseExpression(ctx.structureAccessExpression().structure_index),
                                parseCodeMetainfo(fileName, ctx.structureAccessExpression().structure_index.start),
                                parameters[name])
                    } else {
                        StructureAccessExpression(
                                parseCodeMetainfo(fileName, ctx.start),
                                name,
                                parseExpression(ctx.structureAccessExpression().structure_index),
                                parseCodeMetainfo(fileName, ctx.structureAccessExpression().structure_index.start),
                                null)
                    }
                }
                else -> throw IllegalStateException("I don't know how to handle it!")
            }

    override fun enterDefBody(ctx: CapybaraParser.DefBodyContext) {
        if (ctx.statement() != null) {
            statements.add(parseStatement(ctx.statement()))
        } else if (ctx.return_expression != null) {
            defReturnExpression = parseExpression(ctx.return_expression)
        }
    }

    private fun parseStatement(statement: CapybaraParser.StatementContext): Statement =
            when {
                statement.assigment() != null -> parseAssigmentStatement(statement.assigment())
                statement.while_loop() != null ->
                    WhileLoopStatement(
                            parseExpression(statement.while_loop().while_),
                            statement.while_loop()
                                    .loop_body()
                                    .statement()
                                    .stream()
                                    .map { parseStatement(it) }
                                    .toList()
                    )
                statement.for_loop() != null -> {
                    val forLoop = statement.for_loop().for_loop_expression()
                    ForLoopStatement(
                            if (forLoop.assigment() != null) parseAssigmentStatement(forLoop.assigment()) else null,
                            parseExpression(forLoop.while_),
                            if (forLoop.each_iteration != null) parseStatement(forLoop.each_iteration) else null,
                            statement.for_loop()
                                    .loop_body()
                                    .statement()
                                    .stream()
                                    .map { parseStatement(it) }
                                    .toList()
                    )
                }
                statement.update_assigment() != null -> {
                    val valueName = statement.update_assigment().assign_to.text
                    AssigmentStatement(
                            valueName,
                            InfixExpression(
                                    parseCodeMetainfo(fileName, statement.update_assigment().start),
                                    findOperation(statement.update_assigment().update_action().text),
                                    ValueExpression(parseCodeMetainfo(fileName, statement.update_assigment().start), valueName),
                                    parseExpression(statement.update_assigment().expression())))
                }
                else -> throw IllegalStateException("I don't know how to handle it!")
            }

    private fun findOperation(text: String): String = text[0].toString()

    private fun parseAssigmentStatement(assigment: CapybaraParser.AssigmentContext) =
            AssigmentStatement(assigment.assign_to.text, parseExpression(assigment.expression()))
}


// Statements
sealed class Statement
data class AssigmentStatement(val name: String, val expression: Expression) : Statement()
sealed class Loop : Statement()
data class WhileLoopStatement(val whileExpression: Expression, val statements: List<Statement>) : Loop()
data class ForLoopStatement(val assigment: AssigmentStatement?,
                            val whileExpression: Expression,
                            val eachIteration: Statement?,
                            val statements: List<Statement>) : Loop()