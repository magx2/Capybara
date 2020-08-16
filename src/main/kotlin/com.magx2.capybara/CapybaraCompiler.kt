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

data class Function(val name: String, val returnType: String?, val parameters: List<Parameter>)

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
                listOf()
        ))
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
}
