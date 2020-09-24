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
import java.util.stream.Stream
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
            assignmentsWithReturnType: List<AssigmentStatementWithReturnType> = listOf()) =
            AssigmentStatementWithReturnType(
                    assignment.name,
                    findReturnType(
                            assignmentsWithReturnType,
                            assignment.expression,
                            emptySet()))

    fun findReturnType(
            assignments: List<AssigmentStatementWithReturnType>,
            expression: Expression,
            casts: Set<Cast>,
            callStack: List<FunctionInvocationExpression> = listOf()): ExpressionWithReturnType =
            when (expression) {
                is ParenthesisExpression -> findReturnType(assignments, expression.expression, casts) // TODO should there be ParenthesisExpressionWithReturnTypeUsed?
                is ParameterExpression -> {
                    val type = casts.stream()
                            .filter { cast -> cast.name == expression.valueName }
                            .map { cast -> cast.type }
                            .findAny()
                            .orElseGet { parseType(expression.codeMetainfo, expression.type, localStructs() + importsToTypes(compileUnit)) }
                    ParameterExpressionWithReturnType(type, expression.valueName)
                }
                is IntegerExpression -> IntegerExpressionWithReturnType(expression.value)
                is FloatExpression -> FloatExpressionWithReturnType(expression.value)
                is BooleanExpression -> BooleanExpressionWithReturnType(expression.value)
                is StringExpression -> StringExpressionWithReturnType(expression.value)
                is NothingExpression -> NothingExpressionWithReturnType
                is FunctionInvocationExpression -> {
                    val function = findFunctionForGivenFunctionInvocation(assignments, expression, casts)
                    if (function.isPresent) {
                        val f = function.get()
                        val returnType = if (f.returnType != null) {
                            parseType(expression.codeMetainfo, f.returnType, localStructs() + importsToTypes(compileUnit))
                        } else {
                            if (callStack.contains(f.returnExpression)) {
                                throw CompilationException(expression.codeMetainfo, "There is recursive invocation of functions. Please specify return type explicitly.")
                            }
                            findReturnType(
                                    assignments,
                                    f.returnExpression,
                                    casts,
                                    callStack + expression).returnType
                        }
                        FunctionInvocationExpressionWithReturnType(
                                returnType,
                                f.packageName,
                                f.name,
                                expression.parameters
                                        .stream()
                                        .map { findReturnType(assignments, it, casts) }
                                        .toList()
                        )
                    } else {
                        val parameters = findFunctionParametersValues(assignments, expression, casts)
                                .stream()
                                .map { it.returnType }
                                .map { typeToString(it) }
                                .collect(Collectors.joining(", "))
                        throw CompilationException(expression.codeMetainfo, "Cant find method with signature: " +
                                "`${expression.packageName ?: compileUnit.packageName}:${expression.functionName}($parameters)`")
                    }
                }
                is InfixExpression -> findReturnType(assignments, expression, casts)
                is IfExpression -> {
                    val ifReturnType = findReturnType(assignments, expression.condition, casts)
                    if (ifReturnType.returnType != booleanType) {
                        throw CompilationException(expression.codeMetainfo, "Expression in `if` should return " +
                                "`${typeToString(booleanType)}` not `${typeToString(ifReturnType.returnType)}`")
                    }
                    val castingInIf = isCastingInIf(ifReturnType, emptySet(), assignments)
                    val trueBranchExpression = findReturnType(assignments, expression.trueBranch, castingInIf.left + casts)
                    val falseBranchExpression = findReturnType(assignments, expression.falseBranch, castingInIf.right + casts)
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
                    val expressionWithReturnType = findReturnType(assignments, expression.negateExpression, casts)
                    if (expressionWithReturnType.returnType != booleanType) {
                        throw CompilationException(expression.codeMetainfo, "You can only negate boolean expressions. Type `$expressionWithReturnType` cannot be negated.")
                    }
                    NegateExpressionWithReturnType(expressionWithReturnType)
                }
                is ValueExpression -> {
                    assignments.stream()
                            .filter { it.name == expression.valueName }
                            .map { it.expression }
                            .map { ex ->
                                val type = casts.stream()
                                        .filter { cast -> cast.name == expression.valueName }
                                        .map { cast -> cast.type }
                                        .findAny()
                                        .orElse(ex.returnType)
                                ValueExpressionWithReturnType(type, expression.valueName)
                            }
                            .findAny()
                            .orElseThrow { CompilationException(expression.codeMetainfo, "There is no value with name `${expression.valueName}`") }
                }
                is NewStruct -> {
                    val structPackageName = expression.packageName ?: compileUnit.packageName
                    val structs = if (expression.packageName == null) {
                        findPackageStructs() + importStructsToFlatStructs()
                    } else {
                        compilationContext.structs
                    }

                    val optional = structs.stream()
                            .filter { it.type.name == expression.structName }
                            .filter { it.type.packageName == structPackageName }
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
                                .map { StructFieldExpressionWithReturnType(it.name, findReturnType(assignments, it.value, casts)) }
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
                                struct.type,
                                fields
                        )

                    } else {
                        throw CompilationException(expression.codeMetainfo, "Cannot find struct `${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                    }
                }
                is NewListExpression -> {
                    val elements = expression.elements
                            .stream()
                            .map { findReturnType(assignments, it, casts) }
                            .toList()
                    val genericType = findCommonType(elements.map { it.returnType })
                    val type = addGenericType(listType, genericType)
                    NewListExpressionWithReturnType(type, elements)
                }
                is StructureAccessExpression -> {
                    @Suppress("ThrowableNotThrown")
                    val elementType = if (expression.structureType != null) {
                        parseType(expression.codeMetainfo, expression.structureType, localStructs() + importsToTypes(compileUnit))
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
                            val indexCallType = findReturnType(assignments, expression.structureIndex, casts)
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
                is IsExpression -> {
                    IsExpressionWithReturnType(
                            expression.value,
                            parseType(expression.typeCodeMetainfo, expression.type, localStructs() + importsToTypes(compileUnit)))
                }
            }

    data class SmartCasting(val left: Set<Cast>, val right: Set<Cast>)

    data class Cast(val name: String, val type: Type)

    private fun isCastingInIf(expression: ExpressionWithReturnType, actualCasts: Set<Cast>, assignments: List<AssigmentStatementWithReturnType>): SmartCasting =
            when (expression) {
                is NegateExpressionWithReturnType -> {
                    val (left, right) = isCastingInIf(expression.negateExpression, actualCasts, assignments)
                    SmartCasting(right, left)
                }
                is IsExpressionWithReturnType -> SmartCasting(setOf(Cast(expression.value, expression.type)), emptySet())
                is InfixExpressionWithReturnType -> {
                    val left = isCastingInIf(expression.left, actualCasts, assignments)
                    val right = isCastingInIf(expression.right, actualCasts + left.left + left.right, assignments)
                    SmartCasting(left.left + left.right, right.left + right.right)
                }
                is ValueExpressionWithReturnType ->
                    assignments.stream()
                            .filter { it.name == expression.valueName }
                            .map { it.expression }
                            .map { isCastingInIf(it, actualCasts, assignments) }
                            .findFirst()
                            .orElseGet { SmartCasting(setOf(), setOf()) }
                else -> SmartCasting(setOf(), setOf())
            }

    private fun parseType(typeCodeMetainfo: CodeMetainfo, type: String): Type =
            parseType(typeCodeMetainfo, type, localStructs() + importsToTypes(compileUnit))

    private fun findPackageStructs(): Set<FlatStruct> =
            compilationContext.structs
                    .stream()
                    .filter { it.type.packageName == compileUnit.packageName }
                    .toList()
                    .toSet()

    private fun findCommonType(types: Collection<Type>): Type =
            when {
                types.isEmpty() -> nothingType
                types.toSet().size == 1 -> types.first()
                else -> anyType
            }

    private fun findFunctionForGivenFunctionInvocation(
            assignments: List<AssigmentStatementWithReturnType>,
            function: FunctionInvocationExpression,
            casts: Set<Cast>): Optional<Function> {
        val functions = compileUnit.functions + compileUnit.importFunctions
        val parameters = findFunctionParametersValues(assignments, function, casts)
        return functions.stream()
                .filter { it.name == function.functionName }
                .filter { function.packageName == null }
                .filter { it.parameters.size == parameters.size }
                .filter { f ->
                    var i = 0
                    var equals = true
                    while (i < parameters.size && equals) {
                        val parameters = findFunctionParametersValues(assignments, function, casts)
                        equals = parseType(f.codeMetainfo, f.parameters[i].type, localStructs() + importsToTypes(compileUnit)) == parameters[i].returnType
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
                                    equals = parseType(f.codeMetainfo, f.parameters[i].type, localStructs() + importsToTypes(compileUnit)) == parameters[i].returnType
                                    i++
                                }
                                equals
                            }
                            .findFirst()
                }
    }

    private fun importStructsToFlatStructs(): List<FlatStruct> =
            compileUnit.importStructs
                    .stream()
                    .map { struct ->
                        addUnrollSpreadFieldsInStruct(localStructs() + importsToTypes(compileUnit), struct, fullyQualifiedStructNames)
                    }
                    .toList()

    private fun localStructs(): Set<Type> =
            Stream.concat(
                    compilationContext.structs.stream().map { it.type },
                    compilationContext.unions.stream().map { it.type })
                    .filter { type -> type.packageName == compileUnit.packageName }
                    .toList()
                    .toSet()

    private fun findFunctionParametersValues(
            assignments: List<AssigmentStatementWithReturnType>,
            function: FunctionInvocationExpression,
            casts: Set<Cast>): List<ExpressionWithReturnType> =
            function.parameters
                    .stream()
                    .map { findReturnType(assignments, it, casts) }
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
            if (findUnionType(leftType, rightType).isPresent) {
                findUnionType(leftType, rightType).get()
            } else if (leftType == stringType || rightType == stringType) {
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
                            "Cannot convert type `${typeToString(notStringType.first)}` to `${typeToString(stringType)}` automatically.")
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

    private fun findUnionType(leftType: Type, rightType: Type): Optional<Type> =
            compilationContext.unions
                    .stream()
                    .filter { it.type.packageName == compileUnit.packageName || compileUnit.importUnions.stream().anyMatch { x -> x.type == it.type } }
                    .filter { it.types.contains(leftType) }
                    .filter { it.types.contains(rightType) }
                    .map { it.type }
                    .findAny()

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
            assignments: List<AssigmentStatementWithReturnType>,
            expression: InfixExpression,
            casts: Set<Cast>): ExpressionWithReturnType {
        val leftType = findReturnType(assignments, expression.left, casts)
        val rightType = findReturnType(assignments, expression.right, casts)
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
}

private fun isOneOfGivenType(givenType: Type, vararg types: Type) =
        types.toList().stream().anyMatch { it == givenType }
