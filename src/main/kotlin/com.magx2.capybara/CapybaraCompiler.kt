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
        val functions: List<Function>)

data class CompileUnitWithImports(
        val compileUnit: CompileUnit,
        val importStructs: Set<Struct>,
        val importFunctions: Set<Function>)

data class Export(val packageName: String, val structs: Set<Struct>, val functions: Set<Function>)

data class Import(val importPackage: String, val subImport: Set<String>)

data class Struct(val packageName: String, val name: String, val fields: LinkedList<Field>)
data class FlatStruct(val packageName: String, val name: String, val fields: List<BasicField>)

sealed class Field
data class BasicField(val name: String, val type: String) : Field()
data class SpreadField(val spreadType: String) : Field()

data class Function(val packageName: String,
                    val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val returnExpression: Expression)

data class FunctionWithReturnType(val packageName: String,
                                  val name: String,
                                  val returnType: String,
                                  val parameters: List<Parameter>,
                                  val returnExpression: Expression)

data class Parameter(val name: String, val type: String)

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

        return CompileUnit(listener.packageName, listener.imports, listener.structs, listener.functions)
    }
}

private class Listener : CapybaraBaseListener() {
    lateinit var packageName: String
    val imports = ArrayList<Import>()
    val structs = ArrayList<Struct>()
    val functions = ArrayList<Function>()

    var parameters: Map<String, String> = mapOf()
    val values: HashMap<String, Expression> = HashMap()
    var returnExpression: Expression? = null

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
        this.parameters = findListOfParametersInFun(ctx)
                .stream()
                .collect(Collectors.toMap(
                        (java.util.function.Function<Parameter, String> { it.name }),
                        (java.util.function.Function<Parameter, String> { it.type })))
        if (ctx.returnExpression != null) {
            returnExpression = parseExpression(ctx.returnExpression)
        }
    }

    override fun exitFun_(ctx: CapybaraParser.Fun_Context) {
        val parameters = findListOfParametersInFun(ctx)
        functions.add(Function(
                packageName,
                ctx.name.text,
                ctx.returnType?.text,
                parameters,
                returnExpression!!))
        values.clear()
        returnExpression = null
    }

    private fun findListOfParametersInFun(ctx: CapybaraParser.Fun_Context): List<Parameter> =
            ctx.listOfParameters()
                    ?.parameter()
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
        val expression = parseExpression(ctx.expression())
        if (ctx.assign_to != null) {
            values[ctx.assign_to.text] = expression
        } else {
            returnExpression = expression
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
                    values.containsKey(valueName) -> {
                        values[valueName]!!
                    }
                    else -> {
                        throw java.lang.IllegalStateException("cant find `${valueName}`")
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
                val parameters = ctx.parameters()
                        .expression()
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
            else -> throw IllegalStateException("I don't know how to handle it!")
        }
    }
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