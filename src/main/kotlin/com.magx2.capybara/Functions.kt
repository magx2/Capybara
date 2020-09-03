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
import kotlin.streams.toList

data class Function(val codeMetainfo: CodeMetainfo,
                    val packageName: String,
                    val name: String,
                    val returnType: String?,
                    val parameters: List<Parameter>,
                    val assignments: Set<AssigmentStatement>,
                    val returnExpression: Expression)

data class FunctionWithReturnType(val codeMetainfo: CodeMetainfo,
                                  val packageName: String,
                                  val name: String,
                                  val returnType: Type,
                                  val parameters: List<TypedParameter>,
                                  val assignments: Set<AssigmentStatement>,
                                  val returnExpression: Expression)

fun findReturnType(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: Set<AssigmentStatement>,
        expression: Expression,
        fullyQualifiedStructNames: Map<String, Struct>,
        callStack: List<FunctionInvocationExpression> = listOf()): Type =
        when (expression) {
            is ParenthesisExpression -> findReturnType(compilationContext, compileUnit, assignments, expression.expression, fullyQualifiedStructNames)
            is ParameterExpression -> parseType(expression.codeMetainfo, expression.type, compileUnit.structs, compileUnit.importStructs)
            is IntegerExpression -> intType
            is FloatExpression -> floatType
            is BooleanExpression -> booleanType
            is StringExpression -> stringType
            is FunctionInvocationExpression -> {
                val function = findFunctionForGivenFunctionInvocation(compilationContext, compileUnit, assignments, expression, fullyQualifiedStructNames)
                if (function.isPresent) {
                    val f = function.get()
                    if (f.returnType != null) {
                        parseType(expression.codeMetainfo, f.returnType, compileUnit.structs, compileUnit.importStructs)
                    } else {
                        if (callStack.contains(f.returnExpression)) {
                            throw CompilationException(expression.codeMetainfo, "There is recursive invocation of functions. Please specify return type explicity.")
                        }
                        findReturnType(compilationContext,
                                compileUnit,
                                assignments,
                                f.returnExpression,
                                fullyQualifiedStructNames,
                                callStack + expression)
                    }
                } else {
                    val parameters = findFunctionParametersValues(compilationContext, compileUnit, assignments, expression, fullyQualifiedStructNames)
                            .stream()
                            .map { "${it.packageName}/${it.name}" }
                            .collect(Collectors.joining(", "))
                    throw CompilationException(expression.codeMetainfo, "Cant find method with signature: " +
                            "`${expression.packageName ?: compileUnit.packageName}:${expression.functionName}($parameters)`")
                }
            }
            is InfixExpression -> findReturnType(compilationContext, compileUnit, assignments, expression, fullyQualifiedStructNames)
            is IfExpression -> {
                val ifReturnType = findReturnType(compilationContext, compileUnit, assignments, expression.condition, fullyQualifiedStructNames)
                if (ifReturnType != booleanType) {
                    throw CompilationException(expression.codeMetainfo, "Expression in `if` should return `$booleanType` not `$ifReturnType`")
                }
                findReturnTypeFromBranchExpression(compilationContext, compileUnit, assignments, expression, expression.trueBranch, expression.falseBranch, fullyQualifiedStructNames)
            }
            is NegateExpression -> {
                val returnType = findReturnType(compilationContext, compileUnit, assignments, expression.negateExpression, fullyQualifiedStructNames)
                if (returnType != booleanType) {
                    throw CompilationException(expression.codeMetainfo, "You can only negate boolean expressions. Type `$returnType` cannot be negated.")
                }
                returnType
            }
            is ValueExpression -> {
                assignments.stream()
                        .filter { it.name == expression.valueName }
                        .map { it.expression }
                        .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
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
                    val declaredTypes = expression.fields
                            .stream()
                            .map { Pair(it.name, findReturnType(compilationContext, compileUnit, assignments, it.value, fullyQualifiedStructNames)) }
                            .collect(Collectors.toMap(
                                    (java.util.function.Function<Pair<String, Type>, String> { it.first }),
                                    (java.util.function.Function<Pair<String, Type>, Type> { it.second })
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
                    Type(struct.packageName, struct.name)
                } else {
                    throw CompilationException(expression.codeMetainfo, "Cannot find struct `${expression.packageName ?: compileUnit.packageName}/${expression.structName}`")
                }
            }
            is NewListExpression -> {
                val elementTypes = expression.elements
                        .stream()
                        .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
                        .toList()
                val genericType = findCommonType(elementTypes)
                addGenericType(listType, genericType)
            }
            is StructureAccessExpression -> {
                @Suppress("ThrowableNotThrown")
                val elementType = if (expression.structureType != null) {
                    parseType(expression.codeMetainfo, expression.structureType, compileUnit.structs, compileUnit.importStructs)
                } else {
                    assignments.stream()
                            .filter { it.name == expression.structureName }
                            .map { it.expression }
                            .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
                            .findAny()
                            .orElseThrow { CompilationException(expression.codeMetainfo, "Cannot find value with name `${expression.structureName}`") }
                }

                when (elementType.name) {
                    listType.name -> {
                        val indexCallType = findReturnType(compilationContext, compileUnit, assignments, expression.structureIndex, fullyQualifiedStructNames)
                        if (indexCallType != intType) {
                            throw CompilationException(
                                    expression.structureIndexCodeMetainfo,
                                    "List are indexed by `${typeToString(intType)}` not `${typeToString(indexCallType)}`")
                        }
                    }
                }

                elementType.genericType
                        ?: throw CompilationException(expression.codeMetainfo, "Type `${typeToString(elementType)}` is not generic.")
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
        assignments: Set<AssigmentStatement>,
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
                    equals = parseType(f.codeMetainfo, f.parameters[i].type, compileUnit.structs, compileUnit.importStructs) == parameters[i]
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
                                equals = parseType(f.codeMetainfo, f.parameters[i].type, compileUnit.structs, compileUnit.importStructs) == parameters[i]
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
        assignments: Set<AssigmentStatement>,
        function: FunctionInvocationExpression,
        fullyQualifiedStructNames: Map<String, Struct>): List<Type> =
        function.parameters
                .stream()
                .map { findReturnType(compilationContext, compileUnit, assignments, it, fullyQualifiedStructNames) }
                .toList<Type>()

private fun findReturnTypeFromBranchExpression(
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: Set<AssigmentStatement>,
        expression: Expression,
        left: Expression,
        right: Expression,
        fullyQualifiedStructNames: Map<String, Struct>): Type {
    val leftType = findReturnType(compilationContext, compileUnit, assignments, left, fullyQualifiedStructNames)
    val rightType = findReturnType(compilationContext, compileUnit, assignments, right, fullyQualifiedStructNames)
    return if (leftType == rightType) {
        leftType
    } else {
        if (leftType == stringType || rightType == stringType) {
            val notStringType = if (leftType == stringType) Pair(rightType, right) else Pair(leftType, left)
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
                throw CompilationException(notStringType.second.codeMetainfo,
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
        compilationContext: CompilationContext,
        compileUnit: CompileUnitWithFlatStructs,
        assignments: Set<AssigmentStatement>,
        expression: InfixExpression,
        left: Expression,
        right: Expression,
        fullyQualifiedStructNames: Map<String, Struct>): Type {
    val leftType = findReturnType(compilationContext, compileUnit, assignments, left, fullyQualifiedStructNames)
    if (isOneOfGivenType(leftType, intType, floatType).not()) {
        throw CompilationException(left.codeMetainfo, "Type `${typeToString(leftType)}` needs to be either `${typeToString(intType)}` or  `${typeToString(floatType)}`")
    }
    val rightType = findReturnType(compilationContext, compileUnit, assignments, right, fullyQualifiedStructNames)
    if (isOneOfGivenType(rightType, intType, floatType).not()) {
        throw CompilationException(right.codeMetainfo, "Type `${typeToString(rightType)}` needs to be either `${typeToString(intType)}` or  `${typeToString(floatType)}`")
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
        assignments: Set<AssigmentStatement>,
        expression: InfixExpression,
        fullyQualifiedStructNames: Map<String, Struct>): Type {
    val leftType = findReturnType(compilationContext, compileUnit, assignments, expression.left, fullyQualifiedStructNames)
    val rightType = findReturnType(compilationContext, compileUnit, assignments, expression.right, fullyQualifiedStructNames)
    when (expression.operation) {
        "^", "-", "~/" ->
            if (isOneOfGivenType(stringType, leftType, rightType)) {
                throw CompilationException(expression.codeMetainfo,
                        "String type cannot be applied to `${expression.operation}` infix expression")
            } else if (isOneOfGivenType(booleanType, leftType, rightType)) {
                throw CompilationException(expression.codeMetainfo,
                        "Boolean type cannot be applied to `${expression.operation}` infix expression")
            }
        "&&", "||" ->
            if (isOneOfGivenType(stringType, leftType, rightType)) {
                throw CompilationException(expression.codeMetainfo,
                        "String type cannot be applied to `${expression.operation}` infix expression")
            }
        "+", ">", "<", ">=", "<=", "*" ->
            if (isOneOfGivenType(booleanType, leftType, rightType)) {
                throw CompilationException(expression.codeMetainfo,
                        "Boolean type cannot be applied to `${expression.operation}` infix expression")
            }
    }
    return when (expression.operation) {
        ">", "<", ">=", "<=", "!=", "==", "&&", "||" -> booleanType // FIXME check if left and right are correct types!
        // FIXME only + can be applied like this ; rest needs to have strict types
        // FIXME and you cannot add booleans...
        "+", "*" -> findReturnTypeFromBranchExpression(compilationContext, compileUnit, assignments, expression, expression.left, expression.right, fullyQualifiedStructNames)
        "^", "-", "~/" -> findReturnTypeForNumericOperations(compilationContext, compileUnit, assignments, expression, expression.left, expression.right, fullyQualifiedStructNames)
        else -> throw CompilationException(expression.codeMetainfo,
                "Do not know this `${expression.operation}` infix expression!")
    }
}

private fun isOneOfGivenType(givenType: Type, vararg types: Type) =
        types.toList().stream().anyMatch { it == givenType }
