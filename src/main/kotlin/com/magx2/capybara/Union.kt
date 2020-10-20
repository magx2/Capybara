package com.magx2.capybara

import kotlin.streams.toList

fun parseUnion(union: Union, types: Set<Type>): UnionWithType =
        UnionWithType(
                union.type,
                union.types
                        .stream()
                        .map { parseType(union.codeMetainfo, it, types) }
                        .toList()
                        .toSet()
        )


fun parseUnion(union: Union, unit: CompileUnitWithFlatStructs): UnionWithType {
    val types = concat(importsToTypes(unit).stream(),
            unit.structs.stream().map { it.type },
            unit.unions.stream().map { it.type })
            .toList()
            .toSet()
    return parseUnion(union, types)
}