package com.magx2.capybara

import java.util.*
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

sealed class BaseStruct(open val type: Type)
data class Struct(val codeMetainfo: CodeMetainfo, override val type: Type, val fields: LinkedList<Field>) : BaseStruct(type)
data class FlatStruct(override val type: Type, val fields: List<TypedField>) : BaseStruct(type)

data class Union(val codeMetainfo: CodeMetainfo, val type: Type, val types: Set<TypeWithoutPackage>)
data class UnionWithType(val type: Type, val types: Set<Type>)

fun addUnrollSpreadFieldsInStruct(types: Set<Type>,
                                  struct: Struct,
                                  fullyQualifiedStructNames: Map<Type, Struct>): FlatStruct {
    val newFields = struct.fields
            .stream()
            .flatMap { x(it, struct, fullyQualifiedStructNames) }
            .map { TypedField(it.name, parseType(it.codeMetainfo, it.type, types)) }
            .toList()
    return FlatStruct(struct.type, newFields)
}

fun x(field: Field, struct: Struct, fullyQualifiedStructNames: Map<Type, Struct>): Stream<BasicField> =
        when (field) {
            is BasicField -> Stream.of(field)
            is SpreadField -> {
                mapSpreadFieldToBasicField(field, struct, fullyQualifiedStructNames)
            }
        }

fun mapSpreadFieldToBasicField(spreadField: SpreadField, struct: Struct, fullyQualifiedStructNames: Map<Type, Struct>): Stream<BasicField> {
    val spreadType = if (spreadField.spreadType.startsWith("/")) {
        parseType(struct.codeMetainfo, spreadField.spreadType, setOf())
    } else {
        Type(struct.type.packageName, spreadField.spreadType)
    }
    val spreadStruct = fullyQualifiedStructNames[spreadType]
            ?: throw IllegalStateException("Cannot find struct with name `${spreadField.spreadType}`")
    return spreadStruct.fields.stream().flatMap { x(it, struct, fullyQualifiedStructNames) }
}

fun findFullyQualifiedStructNames(compileUnits: List<CompileUnit>): Map<Type, Struct> {
    return compileUnits.stream()
            .flatMap { it.structs.stream() }
            .map { struct -> Pair(struct.type, struct) }
            .collect(Collectors.toMap(
                    (java.util.function.Function<Pair<Type, Struct>, Type> { it.first }),
                    (java.util.function.Function<Pair<Type, Struct>, Struct> { it.second })
            ))
}