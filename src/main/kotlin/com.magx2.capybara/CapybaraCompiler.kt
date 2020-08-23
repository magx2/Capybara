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
        val packageName: String,
        val imports: List<Import>,
        val structs: List<Struct>,
        val functions: List<Function>,
        val defs: List<Def>)

data class CompileUnitWithFlatStructs(
        val packageName: String,
        val imports: List<Import>,
        val structs: List<FlatStruct>,
        val functions: List<Function>)

data class CompileUnitWithImports(
        val packageName: String,
        val structs: List<FlatStruct>,
        val functions: List<Function>,
        val importStructs: Set<FlatStruct>,
        val importFunctions: Set<Function>)

data class Export(val packageName: String, val structs: Set<FlatStruct>, val functions: Set<Function>)

data class Import(val importPackage: String, val subImport: Set<String>)

data class Struct(val packageName: String, val name: String, val fields: LinkedList<Field>)
data class FlatStruct(val packageName: String, val name: String, val fields: List<TypedField>)

sealed class Field
data class BasicField(val name: String, val type: String) : Field()
data class SpreadField(val spreadType: String) : Field()

data class TypedField(val name: String, val type: Type)
data class Type(val packageName: String, val name: String)

data class Function(val packageName: String,
                    val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val assignments: Set<AssigmentStatement>,
                    val returnExpression: Expression)

data class FunctionWithReturnType(val packageName: String,
                                  val name: String,
                                  val returnType: Type,
                                  val parameters: List<TypedParameter>,
                                  val assignments: Set<AssigmentStatement>,
                                  val returnExpression: Expression)

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
        val listener = Listener()
        walker.walk(listener, tree)

        return CompileUnit(
                listener.packageName,
                listener.imports,
                listener.structs,
                listener.functions,
                listener.defs)
    }
}

private class Listener : CapybaraBaseListener() {
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
        imports.add(Import(ctx.package_.text, subImport))
    }

    override fun enterStruct(ctx: CapybaraParser.StructContext) {
        structs.add(Struct(packageName, ctx.name.text, LinkedList()))
    }

    override fun enterField(ctx: CapybaraParser.FieldContext) {
        val field = if (ctx.name != null) {
            BasicField(ctx.name.text, ctx.type.text)
        } else {
            SpreadField(ctx.spread_type.text)
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

    private fun parseExpression(ctx: CapybaraParser.ExpressionContext): Expression {
        return when {
            ctx.in_parenthisis_expression != null -> {
                ParenthesisExpression(parseExpression(ctx.in_parenthisis_expression))
            }
            ctx.value != null -> {
                val valueName = ctx.value.text
                when {
                    parameters.containsKey(valueName) -> {
                        ParameterExpression(
                                valueName,
                                parameters[valueName]!!
                        )
                    }
                    else -> {
                        ValueExpression(valueName)
                    }
                }
            }
            ctx.constant() != null -> {
                when {
                    ctx.constant().BOOLEAN() != null -> BooleanExpression(ctx.constant().BOOLEAN().text)
                    ctx.constant().INTEGER() != null -> IntegerExpression(ctx.constant().INTEGER().text)
                    ctx.constant().string != null -> StringExpression(ctx.constant().string.text)
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
                        ctx.function_qualified_name.package_?.text,
                        ctx.function_qualified_name.function_name.text,
                        parameters)
            }
            ctx.infix_operation() != null -> {
                InfixExpression(ctx.infix_operation().text, parseExpression(ctx.left), parseExpression(ctx.right))
            }
            ctx.condition != null -> {
                IfExpression(
                        parseExpression(ctx.condition),
                        parseExpression(ctx.true_expression),
                        parseExpression(ctx.false_expression))
            }
            ctx.argument_to_function != null -> {
                FunctionInvocationExpression(
                        ctx.apply_to_function_qualified_name.package_?.text,
                        ctx.apply_to_function_qualified_name.function_name.text,
                        listOf(parseExpression(ctx.argument_to_function)))
            }
            ctx.negate_expression != null -> NegateExpression(parseExpression(ctx.negate_expression))
            ctx.struct_name != null ->
                NewStruct(
                        ctx.struct_name.type_package?.text,
                        ctx.struct_name.name.text,
                        ctx.struct_field_initializations()
                                .struct_field_initialization()
                                .stream()
                                .map { StructField(it.field_name.text, parseExpression(it.field_value)) }
                                .toList()
                )
            else -> throw IllegalStateException("I don't know how to handle it!")
        }
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
                                    findOperation(statement.update_assigment().update_action().text),
                                    ValueExpression(valueName),
                                    parseExpression(statement.update_assigment().expression())))
                }
                else -> throw IllegalStateException("I don't know how to handle it!")
            }

    private fun findOperation(text: String): String = text[0].toString()

    private fun parseAssigmentStatement(assigment: CapybaraParser.AssigmentContext) =
            AssigmentStatement(assigment.assign_to.text, parseExpression(assigment.expression()))
}

sealed class Expression
data class ParenthesisExpression(val expression: Expression) : Expression()
data class ParameterExpression(val valueName: String, val type: String) : Expression()
sealed class ConstantExpression : Expression()
data class IntegerExpression(val value: Long) : ConstantExpression() {
    constructor(value: String) : this(value.toLong())
}

data class BooleanExpression(val value: Boolean) : ConstantExpression() {
    constructor(value: String) : this(value.toBoolean())
}

data class StringExpression(val value: String) : ConstantExpression()
data class FunctionInvocationExpression(val packageName: String?, val functionName: String, val parameters: List<Expression>) : Expression()
data class InfixExpression(val operation: String, val left: Expression, val right: Expression) : Expression()
data class IfExpression(val condition: Expression, val trueBranch: Expression, val falseBranch: Expression) : Expression()
data class NegateExpression(val negateExpression: Expression) : Expression()
data class NewStruct(val packageName: String?, val structName: String, val fields: List<StructField>) : Expression()
data class StructField(val name: String, val value: Expression)
data class ValueExpression(val valueName: String) : Expression()

// Statements
sealed class Statement
data class AssigmentStatement(val name: String, val expression: Expression) : Statement()
sealed class Loop : Statement()
data class WhileLoopStatement(val whileExpression: Expression, val statements: List<Statement>) : Loop()
data class ForLoopStatement(val assigment: AssigmentStatement?,
                            val whileExpression: Expression,
                            val eachIteration: Statement?,
                            val statements: List<Statement>) : Loop()