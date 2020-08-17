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
import kotlin.collections.ArrayList

private val log = LoggerFactory.getLogger(CapybaraCompiler::class.java)

interface CapybaraCompiler {
    companion object {
        fun instance(): CapybaraCompiler = CapybaraCompilerImpl()
    }

    fun compile(fileName: String): CompileUnit
}

data class CompileUnit(val packageName: String, val structs: List<Struct>, val functions: List<Function>)

data class Struct(val packageName: String, val name: String, val fields: LinkedList<Field>)

data class Field(val name: String?, val type: String)

data class Function(val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val body: List<Expression>)

data class Parameter(val name: String?, val type: String)

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

    val parameters = ArrayList<Parameter>()
    lateinit var expressionQueue: ArrayDeque<Expression>

    override fun enterPackageDeclaration(ctx: CapybaraParser.PackageDeclarationContext) {
        packageName = ctx.PACKAGE().text
    }

    override fun enterStruct(ctx: CapybaraParser.StructContext) {
        structs.add(Struct(packageName, ctx.name.text, LinkedList()))
    }

    override fun enterField(ctx: CapybaraParser.FieldContext) {
        structs.last().fields.add(Field(ctx.name?.text, ctx.type.text))
    }

    override fun enterDef_(ctx: CapybaraParser.Def_Context) {
        functions.add(Function(
                ctx.name.text,
                ctx.returnType?.text,
                listOf(),
                listOf()))
        expressionQueue = ArrayDeque()
    }

    override fun exitListOfParameters(ctx: CapybaraParser.ListOfParametersContext?) {
        val function = functions.last().copy(parameters = ArrayList(parameters))
        functions.removeLast()
        functions.add(function)
        parameters.clear()
    }

    override fun enterParameter(ctx: CapybaraParser.ParameterContext) {
        parameters.add(Parameter(ctx.name?.text, ctx.type.text))
    }

    override fun enterDefBody(ctx: CapybaraParser.DefBodyContext) {
        if (ctx.return_expression != null) {
            expressionQueue.add(ReturnExpression)
        }
    }

    override fun exitDefBody(ctx: CapybaraParser.DefBodyContext) {
        if (ctx.return_expression != null) {
            val last = functions.removeLast()
            functions.add(last.copy(body = expressionQueue.toList()))
        }
    }

    override fun enterExpression(ctx: CapybaraParser.ExpressionContext) {
        val expression = when {
            ctx.in_parenthisis_expression != null -> {
                ParenthesisExpression
            }
            ctx.value != null -> {
                ValueExpression(ctx.value.text)
            }
            ctx.constant() != null -> {
                val expression = when {
                    ctx.constant().BOOLEAN() != null -> BooleanExpression(ctx.constant().BOOLEAN().text)
                    ctx.constant().INTEGER() != null -> IntegerExpression(ctx.constant().INTEGER().text)
                    ctx.constant().string_value != null -> StringExpression(ctx.constant().string_value.text)
                    else -> throw IllegalStateException("I don't know how to handle it!")
                }
                expression
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
                InfixExpression(ctx.infix_operation().text)
            }
            else -> throw IllegalStateException("I don't know how to handle it!")
        }
        expressionQueue.add(expression)
    }

}

sealed class Expression
object ParenthesisExpression : Expression() {
    override fun toString(): String = "ParenthesisExpression"
}

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
data class InfixExpression(val operation: String) : Expression()
object ReturnExpression : Expression() {
    override fun toString(): String = "ReturnExpression"
}