package com.magx2.capybara

data class DefWithTypes(val packageName: String,
                        val name: String,
                        val parameters: List<TypedParameter>,
                        val statements: List<StatementWithType>,
                        val returnExpression: ExpressionWithReturnType?)