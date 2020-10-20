package com.magx2.capybara

import com.magx2.capybara.export.python.CompilationContextToExport
import com.magx2.capybara.export.python.CompileUnitToExport
import java.util.stream.Stream

// https://stackoverflow.com/a/60010299
val camelRegex = "(?<=[a-zA-Z])[A-Z]".toRegex()
val snakeRegex = "_[a-zA-Z]".toRegex()

// String extensions
fun String.camelToSnakeCase(): String {
    return camelRegex.replace(this) {
        "_${it.value}"
    }.toLowerCase()
}

fun String.snakeToLowerCamelCase(): String {
    return snakeRegex.replace(this) {
        it.value.replace("_", "")
                .toUpperCase()
    }
}

fun String.snakeToUpperCamelCase(): String {
    return this.snakeToLowerCamelCase().capitalize()
}

fun <T> concat(vararg streams: Stream<T>): Stream<T> {
    var stream = Stream.empty<T>()
    for (s in streams) {
        stream = Stream.concat(stream, s)
    }
    return stream
}

private fun areParametersEquals(parameters1: List<Type>,
                                parameters2: List<Type>,
                                assertEquals: (t1: Type, t2: Type) -> Boolean): Boolean =
        if (parameters1.size == parameters2.size) {
            var i = 0
            var equals = true
            while (i < parameters1.size && equals) {
                val t1 = parameters1[i]
                val t2 = parameters2[i]
                equals = assertEquals(t1, t2)
                i++
            }
            equals
        } else {
            false
        }

fun areParametersEquals(parameters1: List<Type>,
                        parameters2: List<Type>,
                        compilationContext: CompilationContextToExport,
                        compileUnit: CompileUnitToExport): Boolean =
        areParametersEquals(parameters1, parameters2)
        { t1, t2 -> areTypesEqual(t1, t2, compilationContext, compileUnit) }

fun areParametersEquals(parameters1: List<Type>,
                        parameters2: List<Type>,
                        compilationContext: CompilationContext,
                        compileUnit: CompileUnitWithFlatStructs): Boolean =
        areParametersEquals(parameters1, parameters2)
        { t1, t2 -> areTypesEqual(t1, t2, compilationContext, compileUnit) }