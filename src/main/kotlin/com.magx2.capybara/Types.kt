package com.magx2.capybara

import java.util.*
import java.util.regex.Pattern
import java.util.stream.Stream

const val typePackageName = "/capybara/type"

val basicTypesExport = Export(
        typePackageName,
        setOf(
                Struct(typePackageName, BasicTypes.intType.name, LinkedList()),
                Struct(typePackageName, BasicTypes.booleanType.name, LinkedList()),
                Struct(typePackageName, BasicTypes.stringType.name, LinkedList()),
                Struct(typePackageName, BasicTypes.listType.name, LinkedList()),
                Struct(typePackageName, BasicTypes.anyType.name, LinkedList()),
                Struct(typePackageName, BasicTypes.nothingType.name, LinkedList()),
        ),
        setOf())

object BasicTypes {
    val intType = Type(typePackageName, "Int")
    val booleanType = Type(typePackageName, "Boolean")
    val stringType = Type(typePackageName, "String")
    val listType = Type(typePackageName, "List")
    val anyType = Type(typePackageName, "Any")
    val nothingType = Type(typePackageName, "Nothing")
}

fun typeToString(type: Type): String =
        if (type.genericType != null) {
            "${type.packageName}/${type.name}[${typeToString(type.genericType)}]"
        } else {
            "${type.packageName}/${type.name}"
        }

private val rawTypeRegex = Pattern.compile("(.+?)\\[(.+)]")

fun parseType(
        codeMetainfo: CodeMetainfo,
        type: String,
        localStructs: Set<BaseStruct>,
        importedStructs: List<BaseStruct>, ): Type {
    val matcher = rawTypeRegex.matcher(type)
    return if (matcher.find()) {
        val genericType = parseType(codeMetainfo, matcher.group(2), localStructs, importedStructs)
        addGenericType(parseType(codeMetainfo, matcher.group(1), localStructs, importedStructs), genericType)
    } else {
        if (type.contains("/")) {
            Type(type.substringBeforeLast("/"), type.substringAfterLast("/"))
        } else {
            concat(localStructs.stream(),
                    importedStructs.stream())
                    .filter { it.name == type }
                    .map { Type(it.packageName, it.name) }
                    .findFirst()
                    .orElseThrow { CompilationException(codeMetainfo, "Cannot find type `$type` in compilation unit nor in imports") }
        }
    }
}

private fun <T> concat(vararg streams: Stream<T>): Stream<T> {
    var stream = Stream.empty<T>()
    for (s in streams) {
        stream = Stream.concat(stream, s)
    }
    return stream
}

fun addGenericType(type: Type, genericType: Type) = type.copy(genericType = genericType)

data class TypedField(val name: String, val type: Type)
data class Type(val packageName: String, val name: String, val genericType: Type? = null)