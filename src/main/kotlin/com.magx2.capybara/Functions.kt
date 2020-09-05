package com.magx2.capybara

import com.magx2.capybara.BasicTypes.anyType
import com.magx2.capybara.BasicTypes.booleanType
import com.magx2.capybara.BasicTypes.floatType
import com.magx2.capybara.BasicTypes.intType
import com.magx2.capybara.BasicTypes.listType
import com.magx2.capybara.BasicTypes.nothingType
import com.magx2.capybara.BasicTypes.stringType
import java.util.*
import java.util.stream.Collectors
import kotlin.collections.ArrayList
import kotlin.streams.toList

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

fun findReturnTypeForAssignments(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: List<AssigmentStatement>,
        fullyQualifiedStructNames: Map<String, Struct>): List<AssigmentStatementWithReturnType> {
    val assignmentsWithReturnType = ArrayList<AssigmentStatementWithReturnType>(assignments.size)
    for (assigment in assignments) {
        assignmentsWithReturnType.add(
                findReturnTypeForAssignment(
                        compilationContext,
                        compileUnit,
                        assigment,
                        fullyQualifiedStructNames,
                        assignmentsWithReturnType)
        )
    }
    return assignmentsWithReturnType
}

private fun findReturnTypeForAssignment(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignment: AssigmentStatement,
        fullyQualifiedStructNames: Map<String, Struct>,
        assignmentsWithReturnType: List<AssigmentStatementWithReturnType> = listOf()) =
        AssigmentStatementWithReturnType(
                assignment.name,
                findReturnType(
                        compilationContext,
                        compileUnit,
                        assignmentsWithReturnType,
                        assignment.expression,
                        fullyQualifiedStructNames))

fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: List<AssigmentStatementWithReturnType>,
        expression: Expression,
        fullyQualifiedStructNames: Map<String, Struct>,
        callStack: List<FunctionInvocationExpression> = listOf()): ExpressionWithReturnType =
        when (expression) {
            is ParenthesisExpression -> findReturnType(compilationContext, compileUnit, assignments, expression.expression, fullyQualifiedStructNames) // TODO should there be ParenthesisExpressionWithReturnTypeUsed?
            is ParameterExpression -> ParameterExpressionWithReturnType(parseType(expression.codeMetainfo, expression.type, compileUnit.structs, compileUnit.importStructs), expression.valueName)
            is IntegerExpression -> IntegerExpressionWithReturnType(expression.value)
            is FloatExpression -> FloatExpressionWithReturnType(expression.value)
            is BooleanExpression -> BooleanExpressionWithReturnType(expression.value)
            is StringExpression -> StringExpressionWithReturnType(expression.value)
            is FunctionInvocationExpression -> {
                val function = findFunctionForGivenFunctionInvocation(compilationContext, compileUnit, assignments, expression, fullyQualifiedStructNames)
                if (function.isPresent) {
                    val f = function.get()
                    val returnType = if (f.returnType != null) {
                        parseType(expression.codeMetainfo, f.returnType, compileUnit.structs, compileUnit.importStructs)
                    } else {
                        if (callStack.contains(f.returnExpression)) {
                            throw CompilationException(expression.codeMetainfo, "There is recursive invocation of functions. Please specify return type explicitly.")
                        }
                        findReturnType(compilationContext,
                                compileUnit,
                                assignments,
                                f.returnExpression,
                                fullyQualifiedStructNames,
                                callStack + expression).returnType
                    }
                    FunctionInvocationExpressionWithReturnType(
                            returnType,
                            f.packageName,
                            f.name,
                            expression.parameters
                                    .stream()
                                    .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
                                    .toList()
                    )
                } else {
                    val parameters = findFunctionParametersValues(compilationContext, compileUnit, assignments, expression, fullyQualifiedStructNames)
                            .stream()
                            .map { it.returnType }
                            .map { typeToString(it) }
                            .collect(Collectors.joining(", "))
                    throw CompilationException(expression.codeMetainfo, "Cant find method with signature: " +
                            "`${expression.packageName ?: compileUnit.packageName}:${expression.functionName}($parameters)`")
                }
            }
            is InfixExpression -> findReturnType(compilationContext, compileUnit, assignments, expression, fullyQualifiedStructNames)
            is IfExpression -> {
                val ifReturnType = findReturnType(compilationContext, compileUnit, assignments, expression.condition, fullyQualifiedStructNames)
                if (ifReturnType.returnType != booleanType) {
                    throw CompilationException(expression.codeMetainfo, "Expression in `if` should return `$booleanType` not `$ifReturnType`")
                }
                val trueBranchExpression = findReturnType(compilationContext, compileUnit, assignments, expression.trueBranch, fullyQualifiedStructNames)
                val falseBranchExpression = findReturnType(compilationContext, compileUnit, assignments, expression.falseBranch, fullyQualifiedStructNames)
                val type = findReturnTypeFromBranchExpression(
                        expression,
                        trueBranchExpression.returnType,
                        falseBranchExpression.returnType,
                        expression.trueBranch.codeMetainfo,
                        expression.falseBranch.codeMetainfo)
                IfExpressionWithReturnType(
                        type,
                        ifReturnType,
                        trueBranchExpression,
                        falseBranchExpression
                )
            }
            is NegateExpression -> {
                val expressionWithReturnType = findReturnType(compilationContext, compileUnit, assignments, expression.negateExpression, fullyQualifiedStructNames)
                if (expressionWithReturnType.returnType != booleanType) {
                    throw CompilationException(expression.codeMetainfo, "You can only negate boolean expressions. Type `$expressionWithReturnType` cannot be negated.")
                }
                NegateExpressionWithReturnType(expressionWithReturnType)
            }
            is ValueExpression -> {
                assignments.stream()
                        .filter { it.name == expression.valueName }
                        .map { it.expression }
                        .findAny()
                        .orElseThrow { CompilationException(expression.codeMetainfo, "There is no value with name `${expression.valueName}`") }
            }
            is NewStruct -> {
                val structPackageName = expression.packageName ?: compileUnit.packageName
                val structs = if (expression.packageName == null) {
                    compileUnit.structs + importStructsToFlatStructs(compileUnit, fullyQualifiedStructNames)
                } else {
                    compilationContext.structs
                }

                val optional = structs.stream()
                        .filter { it.name == expression.structName }
                        .filter { it.packageName == structPackageName }
                        .findFirst()

                if (optional.isPresent) {
                    val struct = optional.get()
                    val fieldsInExpression = expression.fields
                            .stream()
                            .map { it.name }
                            .toList()
                            .toSet()
                    val missingFields = struct.fields
                            .stream()
                            .filter { fieldsInExpression.contains(it.name).not() }
                            .toList()
                    if (missingFields.isNotEmpty()) {
                        val fields = missingFields.stream()
                                .map { it.name }
                                .collect(Collectors.joining(", "))
                        throw CompilationException(expression.codeMetainfo, "You are missing [$fields] fields when creating new " +
                                "`${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                    }
                    val nameStructFields = struct.fields
                            .stream()
                            .map { it.name }
                            .toList()
                            .toSet()
                    val additionalFields = fieldsInExpression.stream()
                            .filter { nameStructFields.contains(it).not() }
                            .toList()
                    if (additionalFields.isNotEmpty()) {
                        val notRecognizedFields = additionalFields.joinToString(", ")
                        throw CompilationException(expression.codeMetainfo, "I do not recognize those fields [$notRecognizedFields] in struct " +
                                "`${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                    }
                    val fields = expression.fields
                            .stream()
                            .map { StructFieldExpressionWithReturnType(it.name, findReturnType(compilationContext, compileUnit, assignments, it.value, fullyQualifiedStructNames)) }
                            .toList()
                    val declaredTypes = fields.stream()
                            .collect(Collectors.toMap(
                                    (java.util.function.Function<StructFieldExpressionWithReturnType, String> { (k, _) -> k }),
                                    (java.util.function.Function<StructFieldExpressionWithReturnType, Type> { (_, v) -> v.returnType })
                            ))
                    val wrongTypes = struct.fields
                            .stream()
                            .filter { declaredTypes[it.name]!! != it.type }
                            .map { Triple(it.name, it.type, declaredTypes[it.name]!!) }
                            .map {
                                "field name: `${it.first}`, " +
                                        "expected type: `${typeToString(it.second)}`, " +
                                        "actual type: `${typeToString(it.third)}`"
                            }
                            .collect(Collectors.joining(", "))
                    if (wrongTypes.isNotBlank()) {
                        throw CompilationException(expression.codeMetainfo, "Cannot create struct. Given arguments has bad types: [$wrongTypes]")
                    }
                    NewStructExpressionWithReturnType(
                            Type(struct.packageName, struct.name),
                            fields
                    )

                } else {
                    throw CompilationException(expression.codeMetainfo, "Cannot find struct `${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                }
            }
            is NewListExpression -> {
                val elements = expression.elements
                        .stream()
                        .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
                        .toList()
                val genericType = findCommonType(elements.map { it.returnType })
                val type = addGenericType(listType, genericType)
                NewListExpressionWithReturnType(type, elements)
            }
            is StructureAccessExpression -> {
                @Suppress("ThrowableNotThrown")
                val elementType = if (expression.structureType != null) {
                    parseType(expression.codeMetainfo, expression.structureType, compileUnit.structs, compileUnit.importStructs)
                } else {
                    assignments.stream()
                            .filter { it.name == expression.structureName }
                            .map { it.expression }
                            .map { it.returnType }
                            .findAny()
                            .orElseThrow { CompilationException(expression.codeMetainfo, "Cannot find value with name `${expression.structureName}`") }
                }

                val x = when (elementType.name) {
                    listType.name -> {
                        val indexCallType = findReturnType(compilationContext, compileUnit, assignments, expression.structureIndex, fullyQualifiedStructNames)
                        if (indexCallType.returnType != intType) {
                            throw CompilationException(
                                    expression.structureIndexCodeMetainfo,
                                    "List are indexed by `${typeToString(intType)}` not `${typeToString(indexCallType.returnType)}`")
                        }
                        indexCallType
                    }
                    else -> throw CompilationException(expression.structureIndex.codeMetainfo, "Do not know this type of structure access")
                }

                val returnType = (elementType.genericType
                        ?: throw CompilationException(expression.codeMetainfo, "Type `${typeToString(elementType)}` is not generic."))
                StructureAccessExpressionWithReturnType(
                        returnType,
                        elementType,
                        expression.structureName,
                        x)
            }
        }

private fun findCommonType(types: Collection<Type>): Type =
        when {
            types.isEmpty() -> nothingType
            types.toSet().size == 1 -> types.first()
            else -> anyType
        }

private fun findFunctionForGivenFunctionInvocation(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: List<AssigmentStatementWithReturnType>,
        function: FunctionInvocationExpression,
        fullyQualifiedStructNames: Map<String, Struct>): Optional<Function> {
    val functions = compileUnit.functions + compileUnit.importFunctions
    val parameters = findFunctionParametersValues(compilationContext, compileUnit, assignments, function, fullyQualifiedStructNames)
    return functions.stream()
            .filter { it.name == function.functionName }
            .filter { function.packageName == null }
            .filter { it.parameters.size == parameters.size }
            .filter { f ->
                var i = 0
                var equals = true
                while (i < parameters.size && equals) {
                    equals = parseType(f.codeMetainfo, f.parameters[i].type, compileUnit.structs, compileUnit.importStructs) == parameters[i].returnType
                    i++
                }
                equals
            }
            .findFirst()
            .or {
                compilationContext.functions
                        .stream()
                        .filter { it.name == function.functionName }
                        .filter { it.packageName == function.packageName }
                        .filter { it.parameters.size == parameters.size }
                        .filter { f ->
                            var i = 0
                            var equals = true
                            while (i < parameters.size && equals) {
                                equals = parseType(f.codeMetainfo, f.parameters[i].type, compileUnit.structs, compileUnit.importStructs) == parameters[i].returnType
                                i++
                            }
                            equals
                        }
                        .findFirst()
            }
}

private fun importStructsToFlatStructs(compileUnit: CompileUnitWithFlatStructs,
                                       fullyQualifiedStructNames: Map<String, Struct>): List<FlatStruct> =
        compileUnit.importStructs
                .stream()
                .map { struct ->
                    addUnrollSpreadFieldsInStruct(compileUnit.structs, compileUnit.importStructs, struct, fullyQualifiedStructNames)
                }
                .toList()

private fun findFunctionParametersValues(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: List<AssigmentStatementWithReturnType>,
        function: FunctionInvocationExpression,
        fullyQualifiedStructNames: Map<String, Struct>): List<ExpressionWithReturnType> =
        function.parameters
                .stream()
                .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
                .toList()

private fun findReturnTypeFromBranchExpression(
        expression: Expression,
        leftType: Type,
        rightType: Type,
        leftCodeMetainfo: CodeMetainfo,
        rightCodeMetainfo: CodeMetainfo): Type {
    return if (leftType == rightType) {
        leftType
    } else {
        if (leftType == stringType || rightType == stringType) {
            val notStringType = if (leftType == stringType) Pair(rightType, rightCodeMetainfo) else Pair(leftType, leftCodeMetainfo)
            if (notStringType.first == intType || notStringType.first == booleanType) {
                stringType
            } else if (expression is InfixExpression && notStringType.first == floatType) {
                if (expression.operation != "*") {
                    stringType
                } else {
                    throw CompilationException(expression.codeMetainfo,
                            "Cannot apply infix operation `${expression.operation}` to types " +
                                    "`${typeToString(leftType)}` and `${typeToString(rightType)}`")
                }
            } else {
                throw CompilationException(notStringType.second,
                        "Cannot convert type `${notStringType.first}` to `$stringType` automatically.")
            }
        } else if (isOneOfGivenType(intType, leftType, rightType) && isOneOfGivenType(floatType, leftType, rightType)) {
            floatType
        } else {
            throw CompilationException(expression.codeMetainfo, "You need to return same types from infix expression. " +
                    "Left expression returns `${typeToString(leftType)}`, " +
                    "right expression returns `${typeToString(rightType)}`")
        }
    }
}

private fun findReturnTypeForNumericOperations(
        expression: InfixExpression,
        leftType: Type,
        rightType: Type,
        leftCodeMetainfo: CodeMetainfo,
        rightCodeMetainfo: CodeMetainfo): Type {
    if (isOneOfGivenType(leftType, intType, floatType).not()) {
        throw CompilationException(leftCodeMetainfo, "Type `${typeToString(leftType)}` needs to be either `${typeToString(intType)}` or  `${typeToString(floatType)}`")
    }
    if (isOneOfGivenType(rightType, intType, floatType).not()) {
        throw CompilationException(rightCodeMetainfo, "Type `${typeToString(rightType)}` needs to be either `${typeToString(intType)}` or  `${typeToString(floatType)}`")
    }
    return when {
        isOneOfGivenType(floatType, leftType, rightType) -> {
            floatType
        }
        isOneOfGivenType(intType, leftType, rightType) -> {
            intType
        }
        else -> {
            throw CompilationException(expression.codeMetainfo, "Do not know what to return from infix expression: " +
                    "${typeToString(leftType)} ${expression.operation} ${typeToString(rightType)}")
        }
    }
}

private fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: List<AssigmentStatementWithReturnType>,
        expression: InfixExpression,
        fullyQualifiedStructNames: Map<String, Struct>): ExpressionWithReturnType {
    val leftType = findReturnType(compilationContext, compileUnit, assignments, expression.left, fullyQualifiedStructNames)
    val rightType = findReturnType(compilationContext, compileUnit, assignments, expression.right, fullyQualifiedStructNames)
    when (expression.operation) {
        "^", "-", "~/" ->
            if (isOneOfGivenType(stringType, leftType.returnType, rightType.returnType)) {
                throw CompilationException(expression.codeMetainfo,
                        "String type cannot be applied to `${expression.operation}` infix expression")
            } else if (isOneOfGivenType(booleanType, leftType.returnType, rightType.returnType)) {
                throw CompilationException(expression.codeMetainfo,
                        "Boolean type cannot be applied to `${expression.operation}` infix expression")
            }
        "&&", "||" ->
            if (isOneOfGivenType(stringType, leftType.returnType, rightType.returnType)) {
                throw CompilationException(expression.codeMetainfo,
                        "String type cannot be applied to `${expression.operation}` infix expression")
            }
        "+", ">", "<", ">=", "<=", "*" ->
            if (isOneOfGivenType(booleanType, leftType.returnType, rightType.returnType)) {
                throw CompilationException(expression.codeMetainfo,
                        "Boolean type cannot be applied to `${expression.operation}` infix expression")
            }
    }
    val type = when (expression.operation) {
        ">", "<", ">=", "<=", "!=", "==", "&&", "||" -> booleanType // FIXME check if left and right are correct types!
        // FIXME only + can be applied like this ; rest needs to have strict types
        // FIXME and you cannot add booleans...
        "+", "*" -> findReturnTypeFromBranchExpression(expression, leftType.returnType, rightType.returnType, expression.left.codeMetainfo, expression.right.codeMetainfo)
        "^", "-", "~/" -> findReturnTypeForNumericOperations(expression, leftType.returnType, rightType.returnType, expression.left.codeMetainfo, expression.right.codeMetainfo)
        else -> throw CompilationException(expression.codeMetainfo,
                "Do not know this `${expression.operation}` infix expression!")
    }
    return InfixExpressionWithReturnType(type, expression.operation, leftType, rightType)
}

private fun isOneOfGivenType(givenType: Type, vararg types: Type) =
        types.toList().stream().anyMatch { it == givenType }
