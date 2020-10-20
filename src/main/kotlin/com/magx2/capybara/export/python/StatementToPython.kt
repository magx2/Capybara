package com.magx2.capybara.export.python

import com.magx2.capybara.*

fun statementToPython(statement: StatementWithType,
                      assertions: Boolean,
                      unions: Set<UnionWithType>,
                      packageName: String,
                      indent: Int,
                      depth: Int): String =
        when (statement) {
            is AssigmentStatementWithType ->
                assignmentToPython(statement, assertions, unions, packageName, indent, depth)
            is WhileStatementWithType ->
                whileToPython(statement, assertions, unions, packageName, indent, depth)
            is AssertStatementWithType -> assertStatementWithType(statement, assertions, unions, packageName, indent, depth)
            is DefCallStatementWithType -> defCallToPython(statement, assertions, unions, packageName, indent, depth)
        }