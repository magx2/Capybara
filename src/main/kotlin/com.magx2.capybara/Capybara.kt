package com.magx2.capybara

import com.google.gson.GsonBuilder
import java.util.function.Function
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

fun main(args: Array<String>) {
    val compiler = CapybaraCompiler.instance()
    val compileUnits = args.asSequence()
            .map { compiler.compile(it) }
            .toList()

    val structs = compileUnits.stream()
            .flatMap { it.structs.stream() }
            .toList()

    // roll out spread filed in structs
    val fullyQualifiedStructNames: Map<String, Struct> = compileUnits.stream()
            .flatMap { it.structs.stream() }
            .map { struct -> Pair("${struct.packageName}/${struct.name}", struct) }
            .collect(Collectors.toMap(
                    (Function<Pair<String, Struct>, String> { it -> it.first }),
                    (Function<Pair<String, Struct>, Struct> { it -> it.second })
            ))

    val flatStructs = structs.stream()
            .map { struct ->
                addUnrollSpreadFieldsInStruct(struct, fullyQualifiedStructNames)
            }
            .toList()

    printlnAny("Unrolled structs:", flatStructs)
}

private fun printlnAny(header: String?, any: Any) {
    val gson = GsonBuilder().setPrettyPrinting().create()
    val json = gson.toJson(any)
    if (header != null) {
        println(header)
    }
    println(json)
}

private fun addUnrollSpreadFieldsInStruct(struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): FlatStruct {
    val newFields = struct.fields
            .stream()
            .flatMap { x(it, struct, fullyQualifiedStructNames) }
            .toList()
    return FlatStruct(
            struct.packageName,
            struct.name,
            newFields)
}

private fun x(field: Field, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): Stream<BasicField> =
        when (field) {
            is BasicField -> Stream.of(field)
            is SpreadField -> {
                mapSpreadFieldToBasicField(field, struct, fullyQualifiedStructNames)
            }
        }

private fun mapSpreadFieldToBasicField(spreadField: SpreadField, struct: Struct, fullyQualifiedStructNames: Map<String, Struct>): Stream<BasicField> {
    val spreadType = if (spreadField.spreadType.startsWith("/")) {
        spreadField.spreadType
    } else {
        "${struct.packageName}/${spreadField.spreadType}"
    }
    val spreadStruct = fullyQualifiedStructNames[spreadType]
            ?: throw IllegalStateException("Cannot find struct with name `${spreadField.spreadType}`")
    return spreadStruct.fields.stream().flatMap { x(it, struct, fullyQualifiedStructNames) }
}