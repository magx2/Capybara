package com.magx2.capybara

sealed class AbstractDefWithTypes(
        open val codeMetainfo: CodeMetainfo,
        open val packageName: String,
        open val name: String,
        open val parameters: List<TypedParameter>,
        open val returnType: Type?)

data class DefWithTypes(
        override val codeMetainfo: CodeMetainfo,
        override val packageName: String,
        override val name: String,
        override val parameters: List<TypedParameter>,
        val statements: List<StatementWithType>,
        val returnExpression: ExpressionWithReturnType?) : AbstractDefWithTypes(codeMetainfo, packageName, name, parameters, returnExpression?.returnType)

data class NativeDefWithTypes(
        override val codeMetainfo: CodeMetainfo,
        override val packageName: String,
        override val name: String,
        override val parameters: List<TypedParameter>,
        override val returnType: Type?,
        val nativeStatements: List<String>) : AbstractDefWithTypes(codeMetainfo, packageName, name, parameters, returnType)
