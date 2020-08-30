package com.magx2.capybara

import java.util.regex.Pattern

object BasicTypes {
    val intType = Type("/capybara/type", "Int")
    val booleanType = Type("/capybara/type", "Boolean")
    val stringType = Type("/capybara/type", "String")
    val listType = Type("/capybara/type", "List")
    val anyType = Type("/capybara/type", "Any")
    val nothingType = Type("/capybara/type", "Nothing")
}

fun typeToString(type: Type): String =
        if (type.genericType != null) {
            "${type.packageName}/${type.name}[${typeToString(type.genericType)}]"
        } else {
            "${type.packageName}/${type.name}"
        }

private val rawTypeRegex = Pattern.compile("(.+?)\\[(.+)]")

fun parseType(codeMetainfo: CodeMetainfo, type: String, defaultPackage: String? = null): Type {
    val matcher = rawTypeRegex.matcher(type)
    return if (matcher.find()) {
        val genericType = parseType(codeMetainfo, matcher.group(2), defaultPackage)
        addGenericType(parseType(codeMetainfo, matcher.group(1), defaultPackage), genericType)
    } else {
        if (type.contains("/")) {
            Type(type.substringBeforeLast("/"), type.substringAfterLast("/"))
        } else {
            if (defaultPackage != null) {
                Type(defaultPackage, type)
            } else {
                throw CompilationException(codeMetainfo, "You need to pass default package for type `$type`")
            }
        }
    }
}

fun addGenericType(type: Type, genericType: Type) = type.copy(genericType = genericType)

data class TypedField(val name: String, val type: Type)
data class Type(val packageName: String, val name: String, val genericType: Type? = null)