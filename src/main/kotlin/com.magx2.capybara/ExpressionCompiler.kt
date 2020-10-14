package com.magx2.capybara

import java.util.*
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.toList

class ExpressionCompiler(private val compilationContext: CompilationContext,
                         private val compileUnit: CompileUnitWithFlatStructs,
                         private val fullyQualifiedStructNames: Map<Type, Struct>,
                         private val pure: Boolean) {
    fun findReturnType(expression: Expression, assignments: List<AssigmentStatementWithType>): ExpressionWithReturnType =
            findReturnType(expression, assignments, emptySet(), emptySet(), listOf())

    private fun findReturnType(
            expression: Expression,
            assignments: List<AssigmentStatementWithType>,
            casts: Set<Cast>,
            notCasts: Set<NotCast>,
            callStack: List<FunctionInvocationExpression> = listOf()): ExpressionWithReturnType =
            when (expression) {
                is ParenthesisExpression -> findReturnType(expression.expression, assignments, casts, notCasts)
                is ParameterExpression -> {
                    val type = casts.stream()
                            .filter { cast -> cast.name == expression.valueName }
                            .map { cast -> cast.type }
                            .findAny()
                            .orElseGet {
                                val type = parseType(expression.codeMetainfo, expression.type, localStructs() + importsToTypes(compileUnit))

                                if (notCasts.isEmpty()) {
                                    type
                                } else {
                                    val notCast = notCasts.stream()
                                            .filter { it.name == expression.valueName }
                                            .findAny()
                                    if (notCast.isPresent.not()) {
                                        type
                                    } else {
                                        val subTypes = finUnionSubTypes(type)
                                        if (subTypes.isEmpty()) {
                                            type
                                        } else {
                                            val availableTypes = subTypes - notCast.get().notTypes
                                            if (availableTypes.size == 1) {
                                                availableTypes.first()
                                            } else {
                                                type
                                            }
                                        }
                                    }
                                }
                            }
                    ParameterExpressionWithReturnType(type, expression.valueName)
                }
                is IntegerExpression -> IntegerExpressionWithReturnType(expression.value)
                is FloatExpression -> FloatExpressionWithReturnType(expression.value)
                is BooleanExpression -> BooleanExpressionWithReturnType(expression.value)
                is StringExpression -> StringExpressionWithReturnType(expression.value)
                is NothingExpression -> NothingExpressionWithReturnType
                is FunctionInvocationExpression -> {
                    // HERE
                    when (expression.functionInvocation) {
                        is FunctionInvocationByExpression -> {
                            val functionInvocationExpression = expression.functionInvocation.expression
                            if (functionInvocationExpression is ValueExpression) {
                                val valueName = functionInvocationExpression.valueName
                                assignments.stream()
                                        .filter { it.name == valueName }
                                        .filter { isLambda(it.expression.returnType) }
//                                        .map {
//                                            ValueExpressionWithReturnType(
//                                                    findGenericType(it.expression.returnType, functionInvocationExpression.valueName, functionInvocationExpression.codeMetainfo, false),
//                                                    it.name)
//                                        }
                                        .map { it.expression }
                                        .map { exp ->
                                            @Suppress("USELESS_CAST")
                                            FunctionInvocationExpressionWithReturnType(
                                                    findGenericType(exp.returnType, functionInvocationExpression.valueName, functionInvocationExpression.codeMetainfo, false),
                                                    FunctionInvocationByExpressionWithReturnType(exp),
                                                    expression.parameters
                                                            .stream()
                                                            .map { findReturnType(it, assignments, casts, notCasts) }
                                                            .toList()) as ExpressionWithReturnType
                                        }
                                        .findAny()
                                        .or {
                                            localFunctions().stream()
                                                    .filter { it.name == valueName }
                                                    .filter { it.parameters.size == expression.parameters.size }
                                                    .filter { f ->
                                                        var i = 0
                                                        var equals = true
                                                        while (i < expression.parameters.size && equals) {
                                                            val p = findFunctionParametersValues(expression.parameters, assignments, casts, notCasts)
                                                            equals = parseType(f.codeMetainfo, f.parameters[i].type, localStructs() + importsToTypes(compileUnit)) == p[i].returnType
                                                            i++
                                                        }
                                                        equals
                                                    }
                                                    .map { f ->
                                                        FunctionInvocationExpression(
                                                                expression.codeMetainfo,
                                                                FunctionInvocationByName(f.packageName, f.name),
                                                                expression.parameters)
                                                    }
                                                    .map { f -> findReturnType(f, assignments, casts, notCasts) }
                                                    .findAny()
                                        }
                                        .or {
                                            if (pure.not()) {
                                                localDefs().stream()
                                                        .filter { it.name == valueName }
                                                        .filter { it.parameters.size == expression.parameters.size }
                                                        .filter { d ->
                                                            var i = 0
                                                            var equals = true
                                                            while (i < expression.parameters.size && equals) {
                                                                val p = findFunctionParametersValues(expression.parameters, assignments, casts, notCasts)
                                                                equals = parseType(d.codeMetainfo, d.parameters[i].type, localStructs() + importsToTypes(compileUnit)) == p[i].returnType
                                                                i++
                                                            }
                                                            equals
                                                        }
                                                        .map { d ->
                                                            FunctionInvocationExpression(
                                                                    expression.codeMetainfo,
                                                                    FunctionInvocationByName(d.packageName, d.name),
                                                                    expression.parameters)
                                                        }
                                                        .map { f -> findReturnType(f, assignments, casts, notCasts) }
                                                        .findAny()
                                            } else {
                                                Optional.empty()
                                            }
                                        }
                                        .orElseThrow {
                                            throw CompilationException(functionInvocationExpression.codeMetainfo,
                                                    "Could not find value or fun/def name `$valueName`.")
                                        }
                            } else if (functionInvocationExpression is ParameterExpression) {
                                val parameter = findReturnType(functionInvocationExpression, assignments)
                                if (!isLambda(parameter.returnType)) {
                                    throw CompilationException(functionInvocationExpression.codeMetainfo, "Parameter `${functionInvocationExpression.valueName}` is not a lambda. " +
                                            "Actual type is `${typeToString(parameter.returnType)}`.")
                                }
                                val genericType = findGenericType(parameter.returnType, functionInvocationExpression.valueName, functionInvocationExpression.codeMetainfo, true)
                                FunctionInvocationExpressionWithReturnType(
                                        genericType,
                                        FunctionInvocationByExpressionWithReturnType(parameter),
                                        expression.parameters
                                                .stream()
                                                .map { findReturnType(it, assignments, casts, notCasts) }
                                                .toList())
                            } else {
                                val returnExpression = findReturnType(
                                        functionInvocationExpression,
                                        assignments,
                                        casts,
                                        notCasts)
                                if (!isLambda(returnExpression.returnType)) {
                                    throw CompilationException(functionInvocationExpression.codeMetainfo, "This expression is not a lambda. " +
                                            "Actual return type is `${typeToString(returnExpression.returnType)}`.")
                                }
                                FunctionInvocationExpressionWithReturnType(
                                        findGenericType(returnExpression.returnType, functionInvocationExpression.codeMetainfo),
                                        FunctionInvocationByExpressionWithReturnType(returnExpression),
                                        expression.parameters
                                                .stream()
                                                .map { findReturnType(it, assignments, casts, notCasts) }
                                                .toList())

                            }
                        }
                        is FunctionInvocationByName -> {
                            val function = findFunctionForGivenFunctionInvocation(
                                    expression,
                                    assignments,
                                    expression.functionInvocation,
                                    casts,
                                    notCasts)
                            if (function.isPresent) {
                                val f = function.get()
                                val returnType = if (f.returnType != null) {
                                    parseType(expression.codeMetainfo, f.returnType, compilationContext, compileUnit)
                                } else {
                                    if (callStack.contains(f.returnExpression)) {
                                        throw CompilationException(expression.codeMetainfo, "There is recursive invocation of functions. Please specify return type explicitly.")
                                    }
                                    findReturnType(
                                            f.returnExpression,
                                            assignments,
                                            casts,
                                            notCasts,
                                            callStack + expression).returnType
                                }
                                FunctionInvocationExpressionWithReturnType(
                                        returnType,
                                        FunctionInvocationByNameWithReturnType(
                                                returnType,
                                                f.packageName,
                                                f.name),
                                        expression.parameters
                                                .stream()
                                                .map { findReturnType(it, assignments, casts, notCasts) }
                                                .toList())
                            } else if (pure.not()) {
                                // HERE
                                val def = findDefForGivenFunctionInvocation(
                                        expression,
                                        assignments,
                                        expression.functionInvocation,
                                        casts,
                                        notCasts)
                                if (def.isPresent) {
                                    val d = def.get()
                                    val defReturnType = d.returnType
                                    val returnType = if (defReturnType != null) {
                                        parseType(expression.codeMetainfo, defReturnType, compilationContext, compileUnit)
                                    } else {
                                        when (d) {
                                            is Def -> {
                                                if (callStack.contains(d.returnExpression)) {
                                                    throw CompilationException(expression.codeMetainfo, "There is recursive invocation of functions. Please specify return type explicitly.")
                                                }
                                                if (d.returnExpression == null) {
                                                    val parameters = findFunctionParametersValues(expression, assignments, casts, notCasts)
                                                            .stream()
                                                            .map { it.returnType }
                                                            .map { typeToString(it) }
                                                            .collect(Collectors.joining(", "))
                                                    throw CompilationException(expression.codeMetainfo, "Def `${d.packageName}:${d.name}($parameters)` do not return anything, so it can't be used as expression.")
                                                }
                                                findReturnType(
                                                        d.returnExpression,
                                                        assignments,
                                                        casts,
                                                        notCasts,
                                                        callStack + expression).returnType
                                            }
                                            is NativeDef -> {
                                                val parameters = findFunctionParametersValues(expression, assignments, casts, notCasts)
                                                        .stream()
                                                        .map { it.returnType }
                                                        .map { typeToString(it) }
                                                        .collect(Collectors.joining(", "))
                                                throw CompilationException(expression.codeMetainfo, "Native def `${d.packageName}:${d.name}($parameters)` do not return anything, so it can't be used as expression.")
                                            }
                                        }
                                    }
                                    DefInvocationExpressionWithReturnType(
                                            returnType,
                                            d.packageName,
                                            d.name,
                                            expression.parameters
                                                    .stream()
                                                    .map { findReturnType(it, assignments, casts, notCasts) }
                                                    .toList())
                                } else {
                                    val parameters = findFunctionParametersValues(expression, assignments, casts, notCasts)
                                            .stream()
                                            .map { it.returnType }
                                            .map { typeToString(it) }
                                            .collect(Collectors.joining(", "))
                                    throw CompilationException(expression.codeMetainfo, "Can't find fun/def with signature: " +
                                            "`${expression.functionInvocation.packageName ?: compileUnit.packageName}:${expression.functionInvocation.functionName}($parameters)`")
                                }
                            } else {
                                val parameters = findFunctionParametersValues(expression, assignments, casts, notCasts)
                                        .stream()
                                        .map { it.returnType }
                                        .map { typeToString(it) }
                                        .collect(Collectors.joining(", "))
                                throw CompilationException(expression.codeMetainfo, "Can't find fun with signature: " +
                                        "`${expression.functionInvocation.packageName ?: compileUnit.packageName}:${expression.functionInvocation.functionName}($parameters)`")
                            }
                        }
                    }
                }
                is InfixExpression -> findReturnType(expression, assignments, casts, notCasts)
                is IfExpression -> {
                    val ifReturnType = findReturnType(expression.condition, assignments, casts, notCasts)
                    if (ifReturnType.returnType != BasicTypes.booleanType) {
                        throw CompilationException(expression.codeMetainfo, "Expression in `if` should return " +
                                "`${typeToString(BasicTypes.booleanType)}` not `${typeToString(ifReturnType.returnType)}`")
                    }
                    val castingInIf = isCastingInIf(ifReturnType, emptySet(), assignments)
                    val trueBranchExpression = findReturnType(expression.trueBranch, assignments, castingInIf.leftCasts + casts, sumNotCasts(castingInIf.leftNotCasts, notCasts))
                    val falseBranchExpression = findReturnType(expression.falseBranch, assignments, castingInIf.rightCasts + casts, sumNotCasts(castingInIf.rightNotCasts, notCasts))
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
                    val expressionWithReturnType = findReturnType(expression.negateExpression, assignments, casts, notCasts)
                    if (expressionWithReturnType.returnType != BasicTypes.booleanType) {
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

                                val type2 = if (notCasts.isEmpty()) {
                                    type
                                } else {
                                    val notCast = notCasts.stream()
                                            .filter { it.name == expression.valueName }
                                            .findAny()
                                    if (notCast.isPresent.not()) {
                                        type
                                    } else {
                                        val subTypes = finUnionSubTypes(type)
                                        if (subTypes.isEmpty()) {
                                            type
                                        } else {
                                            val availableTypes = subTypes - notCast.get().notTypes
                                            if (availableTypes.size == 1) {
                                                availableTypes.first()
                                            } else {
                                                type
                                            }
                                        }
                                    }
                                }

                                ValueExpressionWithReturnType(type2, expression.valueName)
                            }
                            .findAny()
                            .orElseThrow { CompilationException(expression.codeMetainfo, "There is no value with name `${expression.valueName}`") }
                }
                is LambdaExpression -> {
                    val lambdaAssignments = FunctionCompiler(compilationContext, compileUnit, fullyQualifiedStructNames)
                            .findReturnTypeForAssignments(expression.assignments)
                            .stream()
//                            .map { AssigmentStatementWithType("_" + it.name, it.expression, it.type) }
                            .toList()
                    val returnExpression = findReturnType(expression.expression, lambdaAssignments + assignments, casts, notCasts, callStack)
                    val returnType = addGenericType(BasicTypes.lambdaType, returnExpression.returnType)
                    LambdaExpressionWithReturnType(returnType, lambdaAssignments, returnExpression)
                }
                is StructFieldAccessExpression -> {
                    val structExpression = findReturnType(expression.structureExpression, assignments, casts, notCasts, callStack)
                    val structType = structExpression.returnType
                    val structs = findPackageStructs() + importStructsToFlatStructs() + compilationContext.structs
                    val struct = structs.stream()
                            .filter { it.type == structType }
                            .findAny()
                            .orElseThrow { CompilationException(expression.codeMetainfo, "There is no struct with type `${typeToString(structType)}`") }
                    val type = struct.fields
                            .stream()
                            .filter { it.name == expression.fieldName }
                            .map { it.type }
                            .findAny()
                            .orElseThrow { CompilationException(expression.codeMetainfo, "There is no filed called `${expression.fieldName}` in type `${typeToString(structType)}`") }
                    StructFieldAccessExpressionWithReturnType(
                            structExpression,
                            expression.fieldName,
                            type
                    )
                }
                is NewStruct -> {
                    val structs = if (expression.packageName == null) {
                        findPackageStructs() + importStructsToFlatStructs()
                    } else {
                        compilationContext.structs
                    }

                    val optional = structs.stream()
                            .filter { it.type.name == expression.structName }
                            .filter {
                                if (expression.packageName == null) {
                                    true
                                } else {
                                    it.type.packageName == expression.packageName
                                }
                            }
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
                                .map { StructFieldExpressionWithReturnType(it.name, findReturnType(it.value, assignments, casts, notCasts)) }
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
                            .map { findReturnType(it, assignments, casts, notCasts) }
                            .toList()
                    val genericType = findCommonType(elements.map { it.returnType })
                    val type = addGenericType(BasicTypes.listType, genericType)
                    NewListExpressionWithReturnType(type, elements)
                }
                is StructureAccessExpression -> {
                    @Suppress("ThrowableNotThrown")
                    val elementType = if (expression.structureType != null) {
                        parseType(expression.codeMetainfo, expression.structureType, compilationContext, compileUnit)
                    } else {
                        assignments.stream()
                                .filter { it.name == expression.structureName }
                                .map { it.expression }
                                .map { it.returnType }
                                .findAny()
                                .orElseThrow { CompilationException(expression.codeMetainfo, "Cannot find value with name `${expression.structureName}`") }
                    }

                    val x = when (elementType.name) {
                        BasicTypes.listType.name -> {
                            val indexCallType = findReturnType(expression.structureIndex, assignments, casts, notCasts)
                            if (indexCallType.returnType != BasicTypes.intType) {
                                throw CompilationException(
                                        expression.structureIndexCodeMetainfo,
                                        "List are indexed by `${typeToString(BasicTypes.intType)}` not `${typeToString(indexCallType.returnType)}`")
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
                            parseType(expression.typeCodeMetainfo, expression.type, compilationContext, compileUnit))
                }
                is AssertExpression -> {
                    val checkExpression = findReturnType(expression.checkExpression, assignments, casts, notCasts, callStack)
                    if (checkExpression.returnType != BasicTypes.booleanType) {
                        throw CompilationException(expression.codeMetainfo, "First expression in `assert` should return " +
                                "`${typeToString(BasicTypes.booleanType)}` not `${typeToString(checkExpression.returnType)}`")
                    }
                    val returnExpression = findReturnType(expression.returnExpression, assignments, casts, notCasts, callStack)
                    val messageExpression = if (expression.messageExpression != null) {
                        val x = findReturnType(expression.messageExpression, assignments, casts, notCasts, callStack)
                        if (x.returnType != BasicTypes.stringType) {
                            throw CompilationException(expression.codeMetainfo, "First expression in `assert` should return " +
                                    "`${typeToString(BasicTypes.booleanType)}` not `${typeToString(x.returnType)}`")
                        }
                        x
                    } else {
                        null
                    }

                    AssertExpressionWithReturnType(checkExpression, returnExpression, messageExpression)
                }
            }

    private fun findGenericType(type: Type, name: String, codeMetainfo: CodeMetainfo, parameter: Boolean): Type {
        val x = if (parameter) "Parameter" else "Value";
        return (type.genericType
                ?: throw CompilationException(codeMetainfo, "$x `$name` should have generic type!"))
    }

    private fun findGenericType(type: Type, codeMetainfo: CodeMetainfo): Type {
        return (type.genericType
                ?: throw CompilationException(codeMetainfo, "Expression should have generic type!"))
    }

    private fun finUnionSubTypes(unionType: Type): Set<Type> = finUnionSubTypes(unionType, compilationContext, compileUnit)

    private data class SmartCasting(val leftCasts: Set<Cast>,
                                    val leftNotCasts: Set<NotCast>,
                                    val rightCasts: Set<Cast>,
                                    val rightNotCasts: Set<NotCast>)

    private data class Cast(val name: String, val type: Type)

    private data class NotCast(val name: String, val notTypes: Set<Type>)

    private fun isCastingInIf(expression: ExpressionWithReturnType, actualCasts: Set<Cast>, assignments: List<AssigmentStatementWithType>): SmartCasting =
            when (expression) {
                is NegateExpressionWithReturnType -> {
                    val smart = isCastingInIf(expression.negateExpression, actualCasts, assignments)
                    SmartCasting(smart.rightCasts,
                            smart.rightNotCasts,
                            smart.leftCasts,
                            smart.leftNotCasts)
                }
                is IsExpressionWithReturnType -> {
                    SmartCasting(
                            setOf(Cast(expression.value, expression.type)),
                            emptySet(),
                            emptySet(),
                            setOf(NotCast(expression.value, setOf(expression.type))),
                    )
                }
                is InfixExpressionWithReturnType -> {
                    val left = isCastingInIf(expression.left, actualCasts, assignments)
                    val right = isCastingInIf(expression.right, actualCasts + left.leftCasts + left.rightCasts, assignments)
                    SmartCasting(
                            left.leftCasts + left.rightCasts,
                            sumNotCasts(left.leftNotCasts, left.rightNotCasts),
                            right.leftCasts + right.rightCasts,
                            sumNotCasts(right.leftNotCasts, right.rightNotCasts))
                }
                is ValueExpressionWithReturnType ->
                    assignments.stream()
                            .filter { it.name == expression.valueName }
                            .map { it.expression }
                            .map { isCastingInIf(it, actualCasts, assignments) }
                            .findFirst()
                            .orElseGet { SmartCasting(setOf(), setOf(), setOf(), setOf()) }
                else -> SmartCasting(setOf(), setOf(), setOf(), setOf())
            }

    private fun sumNotCasts(first: Set<NotCast>, second: Set<NotCast>): Set<NotCast> {
        return (first + second).stream()
                .collect(Collectors.groupingBy(
                        (java.util.function.Function<NotCast, String> { it.name })
                ))
                .entries
                .stream()
                .map { it.value }
                .filter { it.isNotEmpty() }
                .map { list ->
                    NotCast(
                            list[0].name,
                            list.stream().flatMap { it.notTypes.stream() }.toList().toSet()
                    )
                }
                .toList()
                .toSet()
    }

    private fun findPackageStructs(): Set<FlatStruct> =
            compilationContext.structs
                    .stream()
                    .filter { it.type.packageName == compileUnit.packageName }
                    .toList()
                    .toSet()

    private fun findCommonType(types: Collection<Type>): Type =
            when {
                types.isEmpty() -> BasicTypes.nothingType
                types.toSet().size == 1 -> types.first()
                else -> BasicTypes.anyType
            }

    private fun findFunctionForGivenFunctionInvocation(
            function: FunctionInvocationExpression,
            assignments: List<AssigmentStatementWithType>,
            invocation: FunctionInvocationByName,
            casts: Set<Cast>,
            notCasts: Set<NotCast>,
    ): Optional<Function> {
        val parameters = findFunctionParametersValues(function, assignments, casts, notCasts)
        val functions = findFunctionsStream(invocation)
        return functions
                .filter { it.name == invocation.functionName }
                .filter { it.parameters.size == parameters.size }
                .filter { f ->
                    var i = 0
                    var equals = true
                    while (i < parameters.size && equals) {
                        val p = findFunctionParametersValues(function, assignments, casts, notCasts)
                        equals = parseType(f.codeMetainfo, f.parameters[i].type, localStructs() + importsToTypes(compileUnit)) == p[i].returnType
                        i++
                    }
                    equals
                }
                .findFirst()
    }

    private fun findFunctionsStream(invocation: FunctionInvocationByName): Stream<Function> =
            if (invocation.packageName == null) {
                localFunctions().stream()
            } else {
                compilationContext.functions
                        .stream()
                        .filter { it.packageName == invocation.packageName }
            }

    private fun localFunctions() = (compileUnit.functions + compileUnit.importFunctions)

    private fun findDefForGivenFunctionInvocation(
            function: FunctionInvocationExpression,
            assignments: List<AssigmentStatementWithType>,
            invocation: FunctionInvocationByName,
            casts: Set<Cast>,
            notCasts: Set<NotCast>,
    ): Optional<AbstractDef> {
        val parameters = findFunctionParametersValues(function, assignments, casts, notCasts)
        val defs = findDefsStream(invocation)
        return defs
                .filter { it.name == invocation.functionName }
                .filter { it.parameters.size == parameters.size }
                .filter { f ->
                    var i = 0
                    var equals = true
                    while (i < parameters.size && equals) {
                        val p = findFunctionParametersValues(function, assignments, casts, notCasts)
                        equals = parseType(f.codeMetainfo, f.parameters[i].type, localStructs() + importsToTypes(compileUnit)) == p[i].returnType
                        i++
                    }
                    equals
                }
                .findFirst()
    }

    private fun findDefsStream(invocation: FunctionInvocationByName): Stream<AbstractDef> =
            if (invocation.packageName == null) {
                localDefs().stream()
            } else {
                compilationContext.defs
                        .stream()
                        .filter { it.packageName == invocation.packageName }
            }

    private fun localDefs() = (compileUnit.defs + compileUnit.defs)

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
            function: FunctionInvocationExpression,
            assignments: List<AssigmentStatementWithType>,
            casts: Set<Cast>,
            notCasts: Set<NotCast>): List<ExpressionWithReturnType> = findFunctionParametersValues(function.parameters, assignments, casts, notCasts)

    private fun findFunctionParametersValues(
            parameters: List<Expression>,
            assignments: List<AssigmentStatementWithType>,
            casts: Set<Cast>,
            notCasts: Set<NotCast>): List<ExpressionWithReturnType> =
            parameters.stream()
                    .map { findReturnType(it, assignments, casts, notCasts) }
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
            } else if (leftType == BasicTypes.stringType || rightType == BasicTypes.stringType) {
                val notStringType = if (leftType == BasicTypes.stringType) Pair(rightType, rightCodeMetainfo) else Pair(leftType, leftCodeMetainfo)
                if (notStringType.first == BasicTypes.intType || notStringType.first == BasicTypes.booleanType) {
                    BasicTypes.stringType
                } else if (expression is InfixExpression && notStringType.first == BasicTypes.floatType) {
                    if (expression.operation != "*") {
                        BasicTypes.stringType
                    } else {
                        throw CompilationException(expression.codeMetainfo,
                                "Cannot apply infix operation `${expression.operation}` to types " +
                                        "`${typeToString(leftType)}` and `${typeToString(rightType)}`")
                    }
                } else {
                    throw CompilationException(notStringType.second,
                            "Cannot convert type `${typeToString(notStringType.first)}` to `${typeToString(BasicTypes.stringType)}` automatically.")
                }
            } else if (isOneOfGivenType(BasicTypes.intType, leftType, rightType) && isOneOfGivenType(BasicTypes.floatType, leftType, rightType)) {
                BasicTypes.floatType
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
        if (isOneOfGivenType(leftType, BasicTypes.intType, BasicTypes.floatType).not()) {
            throw CompilationException(leftCodeMetainfo, "Type `${typeToString(leftType)}` needs to be either `${typeToString(BasicTypes.intType)}` or  `${typeToString(BasicTypes.floatType)}`")
        }
        if (isOneOfGivenType(rightType, BasicTypes.intType, BasicTypes.floatType).not()) {
            throw CompilationException(rightCodeMetainfo, "Type `${typeToString(rightType)}` needs to be either `${typeToString(BasicTypes.intType)}` or  `${typeToString(BasicTypes.floatType)}`")
        }
        return when {
            isOneOfGivenType(BasicTypes.floatType, leftType, rightType) -> {
                BasicTypes.floatType
            }
            isOneOfGivenType(BasicTypes.intType, leftType, rightType) -> {
                BasicTypes.intType
            }
            else -> {
                throw CompilationException(expression.codeMetainfo, "Do not know what to return from infix expression: " +
                        "${typeToString(leftType)} ${expression.operation} ${typeToString(rightType)}")
            }
        }
    }

    private fun findReturnType(
            expression: InfixExpression,
            assignments: List<AssigmentStatementWithType>,
            casts: Set<Cast>,
            notCasts: Set<NotCast>,
    ): ExpressionWithReturnType {
        val leftType = findReturnType(expression.left, assignments, casts, notCasts)
        val rightType = findReturnType(expression.right, assignments, casts, notCasts)
        when (expression.operation) {
            "^", "-", "~/" ->
                if (isOneOfGivenType(BasicTypes.stringType, leftType.returnType, rightType.returnType)) {
                    throw CompilationException(expression.codeMetainfo,
                            "String type cannot be applied to `${expression.operation}` infix expression")
                } else if (isOneOfGivenType(BasicTypes.booleanType, leftType.returnType, rightType.returnType)) {
                    throw CompilationException(expression.codeMetainfo,
                            "Boolean type cannot be applied to `${expression.operation}` infix expression")
                }
            "&&", "||" ->
                if (isOneOfGivenType(BasicTypes.stringType, leftType.returnType, rightType.returnType)) {
                    throw CompilationException(expression.codeMetainfo,
                            "String type cannot be applied to `${expression.operation}` infix expression")
                }
            "+", ">", "<", ">=", "<=", "*" ->
                if (isOneOfGivenType(BasicTypes.booleanType, leftType.returnType, rightType.returnType)) {
                    throw CompilationException(expression.codeMetainfo,
                            "Boolean type cannot be applied to `${expression.operation}` infix expression")
                }
        }
        val type = when (expression.operation) {
            ">", "<", ">=", "<=", "!=", "==", "&&", "||" -> BasicTypes.booleanType // FIXME check if left and right are correct types!
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
