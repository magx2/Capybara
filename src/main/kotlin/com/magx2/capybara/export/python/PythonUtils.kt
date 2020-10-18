package com.magx2.capybara.export.python

import com.magx2.capybara.Type

fun buildIndent(indent: Int) = "\t".repeat(indent)

fun packageToPythonPackage(packageName: String) = packageName.substring(1).replace("/", ".")

fun generateDocForDefWithTypedParameters(
        defName: String,
        parameters: List<ParameterToExport>,
        returnType: Type?,
        indent: Int = 1): String =
        generateDocForDef(
                defName,
                parameters.map { Pair(it.name, it.type) },
                returnType,
                indent)