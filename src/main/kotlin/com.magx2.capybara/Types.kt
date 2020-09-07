package com.magx2.capybara

import java.util.*
import java.util.regex.Pattern
import java.util.stream.Stream

const val typePackageName = "/capybara/type"

val basicTypesExport = Export(
        typePackageName,
        setOf(
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.intType, LinkedList()),
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.floatType, LinkedList()),
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.booleanType, LinkedList()),
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.stringType, LinkedList()),
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.listType, LinkedList()),
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.anyType, LinkedList()),
                Struct(CodeMetainfo("<native>", -1, -1), BasicTypes.nothingType, LinkedList()),
        ),
        setOf(),
        setOf())

object BasicTypes {
    val intType = Type(typePackageName, "Int")
    val floatType = Type(typePackageName, "Float")
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

fun typeToString(type: TypeWithoutPackage): String =
        if (type.genericType != null) {
            "${type.packageName}/${type.name}[${typeToString(type.genericType)}]"
        } else {
            "${type.packageName}/${type.name}"
        }

private val rawTypeRegex = Pattern.compile("(.+?)\\[(.+)]")


fun parseType(
        codeMetainfo: CodeMetainfo,
        type: TypeWithoutPackage,
        types: Set<Type>,
): Type =
        if (type.packageName != null) {
            parseType(codeMetainfo, typeToString(type), types)
        } else {
            parseType(codeMetainfo, type.name, types)
        }

fun parseType(
        codeMetainfo: CodeMetainfo,
        type: String,
        types: Set<Type>,
): Type {
    val matcher = rawTypeRegex.matcher(type)
    return if (matcher.find()) {
        val genericType = parseType(codeMetainfo, matcher.group(2), types)
        addGenericType(parseType(codeMetainfo, matcher.group(1), types), genericType)
    } else {
        if (type.contains("/")) {
            Type(type.substringBeforeLast("/"), type.substringAfterLast("/"))
        } else {
            types.stream()
                    .filter { it.name == type }
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
data class TypeWithoutPackage(val packageName: String?, val name: String, val genericType: TypeWithoutPackage? = null)
