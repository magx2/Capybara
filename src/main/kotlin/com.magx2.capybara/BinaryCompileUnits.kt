package com.magx2.capybara

import com.magx2.capybara.BasicTypes.anyType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.listType
import com.magx2.capybara.BasicTypes.stringType

fun getBinaryCompileUnits() = listOf(stringCompileUnit(), listCompileUnit())

private fun stringCompileUnit(): CompileUnit {
    val originFile = "/binary/string_compile_unit.cb"
    val packageName = "/capybara/type/string"
    val codeMetainfo = CodeMetainfo(originFile, -1, -1)
    return CompileUnit(
            originFile,
            packageName,
            emptyList(),
            emptySet(),
            emptySet(),
            setOf(
                    Function(
                            codeMetainfo,
                            packageName,
                            "to_string",
                            typeToString(stringType),
                            listOf(Parameter(codeMetainfo, "any", typeToString(anyType))),
                            listOf(),
                            NativeExpression(
                                    codeMetainfo,
                                    "str(any)",
                                    stringType))),
            emptySet()
    )
}

private fun listCompileUnit(): CompileUnit {
    val originFile = "/binary/list_compile_unit.cb"
    val packageName = "/capybara/type/list"
    val codeMetainfo = CodeMetainfo(originFile, -1, -1)
    return CompileUnit(
            originFile,
            packageName,
            emptyList(),
            emptySet(),
            emptySet(),
            setOf(
                    Function(
                            codeMetainfo,
                            packageName,
                            "length",
                            typeToString(intType),
                            listOf(Parameter(codeMetainfo, "list", typeToString(addGenericType(listType, anyType)))),
                            listOf(),
                            NativeExpression(
                                    codeMetainfo,
                                    "len(list)",
                                    intType))),
            emptySet()
    )
}