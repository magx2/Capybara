package com.magx2.capybara

data class Function(val codeMetainfo: CodeMetainfo,
                    val packageName: String,
                    val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val assignments: List<AssigmentStatement>,
                    val returnExpression: Expression)

data class FunctionWithReturnType(val packageName: String,
                                  val name: String,
                                  val returnExpression: ExpressionWithReturnType,
                                  val parameters: List<TypedParameter>,
                                  val assignments: List<AssigmentStatementWithReturnType>)



class FunctionCompiler(private val compilationContext: CompilationContext,
                       private val compileUnit: CompileUnitWithFlatStructs,
                       private val fullyQualifiedStructNames: Map<Type, Struct>) {

    fun findReturnTypeForAssignments(assignments: List<AssigmentStatement>): List<AssigmentStatementWithReturnType> {
        val assignmentsWithReturnType = ArrayList<AssigmentStatementWithReturnType>(assignments.size)
        for (assigment in assignments) {
            assignmentsWithReturnType.add(
                    findReturnTypeForAssignment(assigment, assignmentsWithReturnType)
            )
        }
        return assignmentsWithReturnType
    }

    private fun findReturnTypeForAssignment(
            assignment: AssigmentStatement,
            assignmentsWithReturnType: List<AssigmentStatementWithReturnType>) =
            AssigmentStatementWithReturnType(
                    assignment.name,
                    ExpressionCompiler(assignmentsWithReturnType, compilationContext, compileUnit, fullyQualifiedStructNames)
                            .findReturnType(assignment.expression))
}
