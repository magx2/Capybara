package com.magx2.capybara

sealed class AbstractDefWithTypes(
        open val packageName: String,
        open val name: String,
        open val parameters: List<TypedParameter>,
        open val returnType: Type?)

data class DefWithTypes(
        override val packageName: String,
        override val name: String,
        override val parameters: List<TypedParameter>,
        val statements: List<StatementWithType>,
        val returnExpression: ExpressionWithReturnType?) : AbstractDefWithTypes(packageName, name, parameters, returnExpression?.returnType)

data class NativeDefWithTypes(override val packageName: String,
                              override val name: String,
                              override val parameters: List<TypedParameter>,
                              override val returnType: Type?,
                              val nativeStatements: List<String>) : AbstractDefWithTypes(packageName, name, parameters, returnType)
