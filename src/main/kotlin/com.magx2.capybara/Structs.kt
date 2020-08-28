package com.magx2.capybara

import java.util.*
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

data class Struct(val packageName: String, val name: String, val fields: LinkedList<Field>)
data class FlatStruct(val packageName: String, val name: String, val fields: List<TypedField>)

fun addUnrollSpreadFieldsInStruct(defaultPackage: String, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): FlatStruct {
    val newFields = struct.fields
            .stream()
            .flatMap { x(it, struct, fullyQualifiedStructNames) }
            .map { TypedField(it.name, parseType(it.line, it.type, defaultPackage)) }
            .toList()
    return FlatStruct(
            struct.packageName,
            struct.name,
            newFields)
}

fun x(field: Field, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): Stream<BasicField> =
        when (field) {
            is BasicField -> Stream.of(field)
            is SpreadField -> {
                mapSpreadFieldToBasicField(field, struct, fullyQualifiedStructNames)
            }
        }

fun mapSpreadFieldToBasicField(spreadField: SpreadField, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): Stream<BasicField> {
    val spreadType = if (spreadField.spreadType.startsWith("/")) {
        spreadField.spreadType
    } else {
        "${struct.packageName}/${spreadField.spreadType}"
    }
    val spreadStruct = fullyQualifiedStructNames[spreadType]
            ?: throw IllegalStateException("Cannot find struct with name `${spreadField.spreadType}`")
    return spreadStruct.fields.stream().flatMap { x(it, struct, fullyQualifiedStructNames) }
}

fun findFullyQualifiedStructNames(compileUnits: List<CompileUnit>): Map<String, Struct> {
    return compileUnits.stream()
            .flatMap { it.structs.stream() }
            .map { struct -> Pair("${struct.packageName}/${struct.name}", struct) }
            .collect(Collectors.toMap(
                    (java.util.function.Function<Pair<String, Struct>, String> { it.first }),
                    (java.util.function.Function<Pair<String, Struct>, Struct> { it.second })
            ))
}