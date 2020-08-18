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

data class CompileUnit(val packageName: String, val structs: List<Struct>, val functions: List<Function>)

data class Struct(val packageName: String, val name: String, val fields: LinkedList<Field>)
data class FlatStruct(val packageName: String, val name: String, val fields: List<BasicField>)

sealed class Field
data class BasicField(val name: String, val type: String) : Field()
data class SpreadField(val spreadType: String) : Field()

data class Function(val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val returnExpression: Expression)

sealed class FunctionBody
data class AssigmentBody(val assignTo: String, val expression: Expression)
data class ReturnBody(val expression: Expression)

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

        return CompileUnit(listener.packageName, listener.structs, listener.functions)
    }
}

private class Listener : CapybaraBaseListener() {
    lateinit var packageName: String
    val structs = ArrayList<Struct>()
    val functions = ArrayList<Function>()

    var parameters: Set<String> = setOf()
    val values: HashMap<String, Expression> = HashMap()
    var returnExpression: Expression? = null

    override fun enterPackageDeclaration(ctx: CapybaraParser.PackageDeclarationContext) {
        packageName = ctx.PACKAGE().text
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
        this.parameters = ctx.listOfParameters()
                .parameter()
                .stream()
                .map { newParameter(it) }
                .map { it.name }
                .collect(Collectors.toSet())
    }

    override fun exitFun_(ctx: CapybaraParser.Fun_Context) {
        val parameters = ctx.listOfParameters()
                .parameter()
                .stream()
                .map { newParameter(it) }
                .toList()
        this.parameters = parameters.stream()
                .map { it.name }
                .collect(Collectors.toSet())
        functions.add(Function(
                ctx.name.text,
                ctx.returnType?.text,
                parameters,
                returnExpression!!))
        values.clear()
        returnExpression = null
    }

    private fun newParameter(it: CapybaraParser.ParameterContext): Parameter {
        val type = it.type.text
        return if (it.name != null) {
            Parameter(it.name.text, type)
        } else {
            // TODO
            Parameter(type.decapitalize(), type)
        }
    }

    override fun enterFunBody(ctx: CapybaraParser.FunBodyContext) {
        println(" > ${ctx.assign_to?.text}")
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
                    parameters.contains(valueName) -> {
                        ValueExpression(valueName)
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
                    ctx.constant().string_value != null -> StringExpression(ctx.constant().string_value.text)
                    else -> throw IllegalStateException("I don't know how to handle it!")
                }
            }
            ctx.function_name != null -> {
                val parameters = ArrayList<String>()
                var tmp = ctx.parameters()
                while (tmp != null) {
                    parameters.add(tmp.name.text)
                    tmp = tmp.rest
                }
                FunctionInvocationExpression(
                        ctx.function_name.text,
                        parameters)
            }
            ctx.infix_operation() != null -> {
                InfixExpression(ctx.infix_operation().text, parseExpression(ctx.left), parseExpression(ctx.right))
            }
            else -> throw IllegalStateException("I don't know how to handle it!")
        }
    }
}

sealed class Expression
data class AssigmentExpression(val assignTo: String) : Expression()
data class ParenthesisExpression(val expression: Expression) : Expression()
data class ValueExpression(val valueName: String) : Expression()
abstract class ConstantExpression : Expression()
data class IntegerExpression(val value: Long) : ConstantExpression() {
    constructor(value: String) : this(value.toLong())
}

data class BooleanExpression(val value: Boolean) : ConstantExpression() {
    constructor(value: String) : this(value.toBoolean())
}

data class StringExpression(val value: String) : ConstantExpression()
data class FunctionInvocationExpression(val functionName: String, val parameters: List<String>) : Expression()
data class InfixExpression(val operation: String, val left: Expression, val right: Expression) : Expression()
