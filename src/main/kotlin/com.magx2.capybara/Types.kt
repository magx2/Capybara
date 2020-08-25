package com.magx2.capybara

object BasicTypes {
    val intType = Type("/capybara/type", "Int")
    val booleanType = Type("/capybara/type", "Boolean")
    val stringType = Type("/capybara/type", "String")
}

fun typeToString(type: Type) = "${type.packageName}/${type.name}"

fun parseType(type: String, defaultPackage: String? = null): Type =
        if (type.contains("/")) {
            Type(type.substringBeforeLast("/"), type.substringAfterLast("/"))
        } else {
            Type(defaultPackage ?: error("You need to pass default package for type `$type`"), type)
        }

data class TypedField(val name: String, val type: Type)
data class Type(val packageName: String, val name: String)