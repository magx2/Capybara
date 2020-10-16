package com.magx2.capybara

import com.magx2.capybara.BasicTypes.lambdaType
import java.util.regex.Pattern
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

const val typePackageName = "/capybara/type"

object BasicTypes {
    val intType = Type(typePackageName, "Int")
    val floatType = Type(typePackageName, "Float")
    val booleanType = Type(typePackageName, "Boolean")
    val stringType = Type(typePackageName, "String")
    val listType = Type(typePackageName, "List")
    val lambdaType = Type(typePackageName, "Lambda")
    val anyType = Type(typePackageName, "Any")
    val nothingType = Type(typePackageName, "Nothing")
}


fun funToString(function: FunctionToExport): String =
        methodToString(function.packageName, function.name, function.parameters)

fun defToString(def: DefToExport): String =
        methodToString(def.packageName, def.name, def.parameters)

fun methodToString(packageName: String, name: String, parameters: List<ParameterToExport>): String {
    val p = parameters
            .stream()
            .map { it.type }
            .map { typeToString(it) }
            .collect(Collectors.joining(", "))
    return "${packageName}:${name}($p)"
}

fun typeToString(type: Type): String =
        if (type.genericTypes.isNotEmpty()) {
            val genericTypes = type.genericTypes
                    .stream()
                    .map { typeToString(it) }
                    .collect(Collectors.joining(", "))
            "${type.packageName}/${type.name}[$genericTypes]"
        } else {
            "${type.packageName}/${type.name}"
        }

fun typeToString(type: TypeWithoutPackage): String =
        if (type.genericTypes.isNotEmpty()) {
            val genericTypes = type.genericTypes
                    .stream()
                    .map { typeToString(it) }
                    .collect(Collectors.joining(", "))
            "${type.packageName}/${type.name}[$genericTypes]"
        } else {
            "${type.packageName}/${type.name}"
        }

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
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
): Type {
    return parseType(
            codeMetainfo,
            type,
            localStructs(compilationContext, compileUnit) + importsToTypes(compileUnit))
}

private fun localStructs(compilationContext: CompilationContext, compileUnit: CompileUnitWithFlatStructs): Set<Type> =
        Stream.concat(
                compilationContext.structs.stream().map { it.type },
                compilationContext.unions.stream().map { it.type })
                .filter { type -> type.packageName == compileUnit.packageName }
                .toList()
                .toSet()

private val rawTypeRegex = Pattern.compile("(.+?)\\[(.+)]")

fun parseType(
        codeMetainfo: CodeMetainfo,
        type: String,
        types: Set<Type>,
): Type {
    val matcher = rawTypeRegex.matcher(type)
    return if (matcher.find()) {
        val genericTypes = matcher.group(2)
                .split(",")
                .stream()
                .map { it.trim() }
                .map { parseType(codeMetainfo, it, types) }
                .toList()
        addGenericType(parseType(codeMetainfo, matcher.group(1), types), genericTypes)
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

fun finUnionSubTypes(unionType: Type,
                     compilationContext: CompilationContext,
                     compileUnit: CompileUnitWithFlatStructs): Set<Type> =
        (compileUnit.unions + compilationContext.unions).stream()
                .filter { it.type == unionType }
                .map { it.types }
                .findAny()
                .orElse(emptySet())

fun isTypePartOfUnion(type: Type,
                      unionType: Type,
                      compilationContext: CompilationContext,
                      compileUnit: CompileUnitWithFlatStructs): Boolean =
        finUnionSubTypes(unionType, compilationContext, compileUnit).contains(type)

fun areTypesEqual(type1: Type,
                  type2: Type,
                  compilationContext: CompilationContext,
                  compileUnit: CompileUnitWithFlatStructs): Boolean =
        type1 == type2 || isTypePartOfUnion(type1, type2, compilationContext, compileUnit) || isTypePartOfUnion(type2, type1, compilationContext, compileUnit)

fun addGenericType(type: Type, genericTypes: List<Type>): Type {
    val newGenerics = ArrayList(type.genericTypes)
    newGenerics.addAll(genericTypes)
    return type.copy(genericTypes = newGenerics.toList())
}

fun addGenericTypes(type: Type, genericTypes: List<TypedParameter>): Type =
        addGenericType(
                type,
                genericTypes.stream()
                        .map { it.type }
                        .toList())

fun addGenericType(type: Type, genericType: Type): Type = addGenericType(type, listOf(genericType))

fun isLambda(type: Type) =
        type.packageName == lambdaType.packageName && type.name == lambdaType.name

fun findSingleGenericType(type: Type, codeMetainfo: CodeMetainfo): Type {
    if (type.genericTypes.isEmpty()) {
        throw CompilationException(codeMetainfo, "Type `${typeToString(type)}` is not generic.")
    }
    if (type.genericTypes.size > 1) {
        throw CompilationException(codeMetainfo, "Type `${typeToString(type)}` is multi generic type.")
    }
    return type.genericTypes[0];
}

fun findLastGenericType(type: Type, codeMetainfo: CodeMetainfo): Type {
    if (type.genericTypes.isEmpty()) {
        throw CompilationException(codeMetainfo, "Type `${typeToString(type)}` is not generic.")
    }
    return type.genericTypes.last();
}

data class TypedField(val name: String, val type: Type)
data class Type(val packageName: String, val name: String, val genericTypes: List<Type> = emptyList())
data class TypeWithoutPackage(val packageName: String?, val name: String, val genericTypes: List<TypeWithoutPackage> = emptyList())
