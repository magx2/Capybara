package dev.capylang.compiler.parser.facade;

import dev.capylang.compiler.ImportDeclaration;
import dev.capylang.compiler.Visibility;
import dev.capylang.compiler.parser.*;
import dev.capylang.compiler.parser.facade.ParserSchema.*;

import java.util.List;
import java.util.Optional;

public final class ParserDtoMapper {
    private ParserDtoMapper() {
    }

    public static SourceModuleDto sourceModule(RawModule module) {
        return new SourceModuleDto(module.name(), module.path(), module.input(), sourceKind(module.sourceKind()));
    }

    public static SourceKindDto sourceKind(SourceKind sourceKind) {
        return switch (sourceKind) {
            case FUNCTIONAL -> SourceKindDto.FUNCTIONAL;
            case OBJECT_ORIENTED -> SourceKindDto.OBJECT_ORIENTED;
        };
    }

    public static ImportDto importDeclaration(ImportDeclaration importDeclaration) {
        return new ImportDto(
                importDeclaration.moduleName(),
                importDeclaration.symbols(),
                importDeclaration.excludedSymbols(),
                importDeclaration.qualifiedOnly()
        );
    }

    public static FunctionalProgramDto functionalProgram(Program program) {
        return new FunctionalProgramDto(program.modules().stream()
                .map(ParserDtoMapper::functionalModule)
                .toList());
    }

    public static FunctionalModuleDto functionalModule(dev.capylang.compiler.parser.Module module) {
        return new FunctionalModuleDto(
                module.name(),
                module.path(),
                module.functional().definitions().stream()
                        .map(ParserDtoMapper::definition)
                        .toList(),
                module.imports().stream().map(ParserDtoMapper::importDeclaration).toList(),
                sourceKind(module.sourceKind())
        );
    }

    public static FunctionalDefinitionDto definition(Definition definition) {
        return switch (definition) {
            case Function function -> new FunctionDefinitionDto(
                    function.name(),
                    function.parameters().stream().map(ParserDtoMapper::parameter).toList(),
                    function.returnType().map(ParserDtoMapper::type),
                    expression(function.expression()),
                    function.comments(),
                    visibility(function.visibility()),
                    position(function.position()),
                    function.tailRecursive(),
                    function.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case PrimitiveBackedTypeDeclaration declaration -> new PrimitiveBackedTypeDefinitionDto(
                    declaration.name(),
                    primitiveName(declaration.backingType()),
                    declaration.constructor().map(ParserDtoMapper::expression),
                    declaration.comments(),
                    visibility(declaration.visibility()),
                    position(declaration.position()),
                    declaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case TypeDeclaration declaration -> new UnionDefinitionDto(
                    declaration.name(),
                    declaration.subTypes(),
                    declaration.fields().stream().map(ParserDtoMapper::dataField).toList(),
                    declaration.typeParameters(),
                    declaration.constructor().map(ParserDtoMapper::expression),
                    declaration.derives().stream().map(ParserDtoMapper::deriveDirective).toList(),
                    declaration.comments(),
                    visibility(declaration.visibility()),
                    position(declaration.position()),
                    declaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case DataDeclaration declaration -> new DataDefinitionDto(
                    declaration.name(),
                    declaration.fields().stream().map(ParserDtoMapper::dataField).toList(),
                    declaration.extendsTypes(),
                    declaration.typeParameters(),
                    declaration.constructor().map(ParserDtoMapper::expression),
                    declaration.derives().stream().map(ParserDtoMapper::deriveDirective).toList(),
                    declaration.comments(),
                    visibility(declaration.visibility()),
                    declaration.nativeType(),
                    position(declaration.position()),
                    declaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case EnumDeclaration declaration -> new EnumDefinitionDto(
                    declaration.name(),
                    declaration.values(),
                    declaration.comments(),
                    position(declaration.position()),
                    declaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case DeriverDeclaration declaration -> new DeriverDefinitionDto(
                    declaration.name(),
                    declaration.methods().stream().map(ParserDtoMapper::deriverMethod).toList(),
                    declaration.comments(),
                    visibility(declaration.visibility()),
                    position(declaration.position()),
                    declaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case AnnotationDeclaration declaration -> new AnnotationDefinitionDto(
                    declaration.name(),
                    declaration.targets().stream().map(ParserDtoMapper::annotationTarget).toList(),
                    declaration.fields().stream().map(ParserDtoMapper::annotationField).toList(),
                    declaration.multiple(),
                    declaration.comments(),
                    visibility(declaration.visibility()),
                    position(declaration.position()),
                    declaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
        };
    }

    private static DeriverMethodDto deriverMethod(DeriverDeclaration.DeriverMethod method) {
        return new DeriverMethodDto(
                method.name(),
                method.parameters().stream().map(ParserDtoMapper::parameter).toList(),
                type(method.returnType()),
                expression(method.expression()),
                method.comments(),
                position(method.position()),
                method.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
        );
    }

    public static ObjectOrientedModuleDto objectOrientedModule(ObjectOrientedModule module) {
        return new ObjectOrientedModuleDto(
                module.name(),
                module.path(),
                module.objectOriented().definitions().stream().map(ParserDtoMapper::ooTypeDeclaration).toList(),
                module.objectOriented().nativeProviders().stream().map(ParserDtoMapper::ooNativeProvider).toList(),
                module.imports().stream().map(ParserDtoMapper::importDeclaration).toList(),
                sourceKind(module.sourceKind())
        );
    }

    private static OoNativeProviderDeclarationDto ooNativeProvider(ObjectOriented.NativeProviderDeclaration declaration) {
        return new OoNativeProviderDeclarationDto(
                declaration.name(),
                declaration.targetType(),
                declaration.qualifier(),
                declaration.comments()
        );
    }

    private static OoTypeDeclarationDto ooTypeDeclaration(ObjectOriented.TypeDeclaration declaration) {
        return switch (declaration) {
            case ObjectOriented.ClassDeclaration classDeclaration -> new OoClassDefinitionDto(
                    classDeclaration.name(),
                    classDeclaration.constructorParameters().stream().map(ParserDtoMapper::ooParameter).toList(),
                    classDeclaration.parents().stream().map(ParserDtoMapper::ooTypeReference).toList(),
                    classDeclaration.members().stream().map(ParserDtoMapper::ooMember).toList(),
                    classDeclaration.modifiers(),
                    classDeclaration.comments(),
                    classDeclaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case ObjectOriented.TraitDeclaration traitDeclaration -> new OoTraitDefinitionDto(
                    traitDeclaration.name(),
                    traitDeclaration.parents().stream().map(ParserDtoMapper::ooTypeReference).toList(),
                    traitDeclaration.members().stream().map(ParserDtoMapper::ooMember).toList(),
                    traitDeclaration.comments(),
                    traitDeclaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case ObjectOriented.InterfaceDeclaration interfaceDeclaration -> new OoInterfaceDefinitionDto(
                    interfaceDeclaration.name(),
                    interfaceDeclaration.parents().stream().map(ParserDtoMapper::ooTypeReference).toList(),
                    interfaceDeclaration.members().stream().map(ParserDtoMapper::ooMember).toList(),
                    interfaceDeclaration.comments(),
                    interfaceDeclaration.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
        };
    }

    private static OoMemberDeclarationDto ooMember(ObjectOriented.MemberDeclaration member) {
        return switch (member) {
            case ObjectOriented.FieldDeclaration field -> new OoFieldDefinitionDto(
                    field.name(),
                    field.type(),
                    field.visibility(),
                    field.initializer(),
                    field.comments(),
                    field.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case ObjectOriented.MethodDeclaration method -> new OoMethodDefinitionDto(
                    method.name(),
                    method.parameters().stream().map(ParserDtoMapper::ooParameter).toList(),
                    method.returnType(),
                    method.visibility(),
                    method.modifiers(),
                    method.body().map(ParserDtoMapper::ooMethodBody),
                    method.comments(),
                    method.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
            case ObjectOriented.InitBlock init -> new OoInitBlockDto(
                    ooStatementBlock(init.body()),
                    init.comments(),
                    init.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
            );
        };
    }

    private static OoMethodBodyDto ooMethodBody(ObjectOriented.MethodBody body) {
        return switch (body) {
            case ObjectOriented.ExpressionBody expressionBody -> new OoExpressionBodyDto(expressionBody.expression());
            case ObjectOriented.StatementBlock statementBlock -> ooStatementBlock(statementBlock);
        };
    }

    private static OoStatementBlockDto ooStatementBlock(ObjectOriented.StatementBlock block) {
        return new OoStatementBlockDto(block.statements().stream()
                .map(ParserDtoMapper::ooStatement)
                .toList());
    }

    private static OoStatementDto ooStatement(ObjectOriented.Statement statement) {
        return switch (statement) {
            case ObjectOriented.LetStatement let -> new OoLetStatementDto(let.name(), let.type(), let.expression());
            case ObjectOriented.LocalMethodStatement localMethod -> new OoLocalMethodStatementDto(
                    localMethod.name(),
                    localMethod.parameters().stream().map(ParserDtoMapper::ooParameter).toList(),
                    localMethod.returnType(),
                    ooMethodBody(localMethod.body()),
                    localMethod.comments()
            );
            case ObjectOriented.MutableVariableStatement variable -> new OoMutableVariableStatementDto(
                    variable.name(),
                    variable.type(),
                    variable.expression()
            );
            case ObjectOriented.AssignmentStatement assignment -> new OoAssignmentStatementDto(assignment.name(), assignment.expression());
            case ObjectOriented.ExpressionStatement expression -> new OoExpressionStatementDto(expression.expression());
            case ObjectOriented.ThrowStatement throwStatement -> new OoThrowStatementDto(throwStatement.expression());
            case ObjectOriented.ReturnStatement returnStatement -> new OoReturnStatementDto(returnStatement.expression());
            case ObjectOriented.IfStatement ifStatement -> new OoIfStatementDto(
                    ifStatement.condition(),
                    ooStatementBlock(ifStatement.thenBranch()),
                    ifStatement.elseBranch().map(ParserDtoMapper::ooStatement)
            );
            case ObjectOriented.TryCatchStatement tryCatch -> new OoTryCatchStatementDto(
                    ooStatementBlock(tryCatch.tryBlock()),
                    tryCatch.catches().stream()
                            .map(catchClause -> new OoCatchClauseDto(catchClause.name(), ooStatementBlock(catchClause.body())))
                            .toList()
            );
            case ObjectOriented.WhileStatement whileStatement -> new OoWhileStatementDto(
                    whileStatement.condition(),
                    ooStatementBlock(whileStatement.body())
            );
            case ObjectOriented.DoWhileStatement doWhileStatement -> new OoDoWhileStatementDto(
                    ooStatementBlock(doWhileStatement.body()),
                    doWhileStatement.condition()
            );
            case ObjectOriented.ForEachStatement forEachStatement -> new OoForEachStatementDto(
                    forEachStatement.name(),
                    forEachStatement.type(),
                    forEachStatement.iterable(),
                    ooStatementBlock(forEachStatement.body())
            );
            case ObjectOriented.StatementBlock statementBlock -> ooStatementBlock(statementBlock);
        };
    }

    private static OoParameterDto ooParameter(ObjectOriented.Parameter parameter) {
        return new OoParameterDto(parameter.name(), parameter.type());
    }

    private static OoTypeReferenceDto ooTypeReference(ObjectOriented.TypeReference typeReference) {
        return new OoTypeReferenceDto(typeReference.name());
    }

    private static ParameterDto parameter(Parameter parameter) {
        return new ParameterDto(type(parameter.type()), parameter.name(), position(parameter.position()));
    }

    private static DataFieldDto dataField(DataDeclaration.DataField field) {
        return new DataFieldDto(
                field.name(),
                type(field.type()),
                field.annotations().stream().map(ParserDtoMapper::annotationUsage).toList()
        );
    }

    private static DeriveDirectiveDto deriveDirective(DeriveDirective directive) {
        return new DeriveDirectiveDto(directive.name(), position(directive.position()));
    }

    public static TypeDto type(Type type) {
        return switch (type) {
            case PrimitiveType primitiveType -> new PrimitiveTypeDto(primitiveName(primitiveType));
            case DataType dataType -> new DataTypeDto(dataType.name());
            case CollectionType.ListType listType -> new ListTypeDto(type(listType.elementType()));
            case CollectionType.SetType setType -> new SetTypeDto(type(setType.elementType()));
            case CollectionType.DictType dictType -> new DictTypeDto(type(dictType.valueType()));
            case FunctionType functionType -> new FunctionTypeDto(type(functionType.argumentType()), type(functionType.returnType()));
            case TupleType tupleType -> new TupleTypeDto(tupleType.elementTypes().stream().map(ParserDtoMapper::type).toList());
        };
    }

    public static ExpressionDto expression(Expression expression) {
        return switch (expression) {
            case BooleanValue value -> new BoolExpressionDto(value.value(), position(value.position()));
            case ByteValue value -> new ByteExpressionDto(value.byteValue(), position(value.position()));
            case IntValue value -> new IntExpressionDto(value.intValue(), position(value.position()));
            case LongValue value -> new LongExpressionDto(value.longValue(), position(value.position()));
            case FloatValue value -> new FloatExpressionDto(value.floatValue(), position(value.position()));
            case DoubleValue value -> new DoubleExpressionDto(value.doubleValue(), position(value.position()));
            case StringValue value -> new StringExpressionDto(value.stringValue(), position(value.position()));
            case NothingValue value -> new NothingExpressionDto(value.literal(), position(value.position()));
            case Value value -> new ValueExpressionDto(value.name(), position(value.position()));
            case FunctionCall call -> new FunctionCallExpressionDto(
                    call.moduleName(),
                    call.name(),
                    call.arguments().stream().map(ParserDtoMapper::expression).toList(),
                    position(call.position())
            );
            case FunctionInvoke invoke -> new FunctionInvokeExpressionDto(
                    expression(invoke.function()),
                    invoke.arguments().stream().map(ParserDtoMapper::expression).toList(),
                    position(invoke.position())
            );
            case FunctionReference reference -> new FunctionReferenceExpressionDto(reference.name(), position(reference.position()));
            case IfExpression ifExpression -> new IfExpressionDto(
                    expression(ifExpression.condition()),
                    expression(ifExpression.thenBranch()),
                    expression(ifExpression.elseBranch()),
                    position(ifExpression.position())
            );
            case InfixExpression infix -> new InfixExpressionDto(
                    expression(infix.left()),
                    infix.operator().symbol(),
                    expression(infix.right()),
                    position(infix.position())
            );
            case FieldAccess access -> new FieldAccessExpressionDto(
                    expression(access.source()),
                    access.field(),
                    position(access.position())
            );
            case LambdaExpression lambda -> new LambdaExpressionDto(
                    lambda.argumentNames(),
                    expression(lambda.expression()),
                    position(lambda.position())
            );
            case LetExpression let -> new LetExpressionDto(
                    let.name(),
                    let.declaredType().map(ParserDtoMapper::type),
                    let.kind().name(),
                    expression(let.value()),
                    expression(let.rest()),
                    position(let.position())
            );
            case ReduceExpression reduce -> new ReduceExpressionDto(
                    expression(reduce.initialValue()),
                    reduce.accumulatorName(),
                    reduce.keyName(),
                    reduce.valueName(),
                    expression(reduce.reducerExpression()),
                    position(reduce.position())
            );
            case IndexExpression index -> new IndexExpressionDto(
                    expression(index.source()),
                    index.arguments().stream().map(ParserDtoMapper::expression).toList(),
                    position(index.position())
            );
            case SliceExpression slice -> new SliceExpressionDto(
                    expression(slice.source()),
                    slice.start().map(ParserDtoMapper::expression),
                    slice.end().map(ParserDtoMapper::expression),
                    position(slice.position())
            );
            case MatchExpression match -> new MatchExpressionDto(
                    expression(match.matchWith()),
                    match.cases().stream().map(ParserDtoMapper::matchCase).toList(),
                    position(match.position())
            );
            case NewData newData -> new NewDataExpressionDto(
                    type(newData.type()),
                    newData.bypassConstructor(),
                    newData.assignments().stream().map(ParserDtoMapper::fieldAssignment).toList(),
                    newData.positionalArguments().stream().map(ParserDtoMapper::expression).toList(),
                    newData.spreads().stream().map(ParserDtoMapper::expression).toList(),
                    position(newData.position())
            );
            case ConstructorData constructorData -> new ConstructorDataExpressionDto(
                    constructorData.assignments().stream().map(ParserDtoMapper::fieldAssignment).toList(),
                    constructorData.positionalArguments().stream().map(ParserDtoMapper::expression).toList(),
                    constructorData.spreads().stream().map(ParserDtoMapper::expression).toList(),
                    position(constructorData.position())
            );
            case WithExpression with -> new WithExpressionDto(
                    expression(with.source()),
                    with.assignments().stream().map(ParserDtoMapper::fieldAssignment).toList(),
                    position(with.position())
            );
            case NewListExpression list -> new NewListExpressionDto(
                    list.values().stream().map(ParserDtoMapper::expression).toList(),
                    position(list.position())
            );
            case NewSetExpression set -> new NewSetExpressionDto(
                    set.values().stream().map(ParserDtoMapper::expression).toList(),
                    position(set.position())
            );
            case NewDictExpression dict -> new NewDictExpressionDto(
                    dict.entries().stream().map(entry -> new DictEntryDto(expression(entry.key()), expression(entry.value()))).toList(),
                    position(dict.position())
            );
            case TupleExpression tuple -> new TupleExpressionDto(
                    tuple.values().stream().map(ParserDtoMapper::expression).toList(),
                    position(tuple.position())
            );
            case UnwrapExpression unwrap -> new UnwrapExpressionDto(expression(unwrap.expression()), position(unwrap.position()));
            case PlaceholderExpression placeholder -> new PlaceholderExpressionDto(position(placeholder.position()));
        };
    }

    private static MatchCaseDto matchCase(MatchExpression.MatchCase matchCase) {
        return new MatchCaseDto(
                pattern(matchCase.pattern()),
                matchCase.guard().map(ParserDtoMapper::expression),
                expression(matchCase.expression())
        );
    }

    private static PatternDto pattern(MatchExpression.Pattern pattern) {
        return switch (pattern) {
            case MatchExpression.IntPattern intPattern -> new IntPatternDto(intPattern.value());
            case MatchExpression.LongPattern longPattern -> new LongPatternDto(longPattern.value());
            case MatchExpression.StringPattern stringPattern -> new StringPatternDto(stringPattern.value());
            case MatchExpression.BoolPattern boolPattern -> new BoolPatternDto(boolPattern.value());
            case MatchExpression.FloatPattern floatPattern -> new FloatPatternDto(floatPattern.value());
            case MatchExpression.TypedPattern typedPattern -> new TypedPatternDto(type(typedPattern.type()), typedPattern.name());
            case MatchExpression.VariablePattern variablePattern -> new VariablePatternDto(variablePattern.name());
            case MatchExpression.WildcardPattern ignored -> WildcardPatternDto.WILDCARD;
            case MatchExpression.WildcardBindingPattern wildcardBindingPattern -> new WildcardBindingPatternDto(wildcardBindingPattern.name());
            case MatchExpression.ConstructorPattern constructorPattern -> new ConstructorPatternDto(
                    constructorPattern.constructorName(),
                    constructorPattern.fieldPatterns().stream().map(ParserDtoMapper::pattern).toList()
            );
        };
    }

    private static FieldAssignmentDto fieldAssignment(NewData.FieldAssignment assignment) {
        return new FieldAssignmentDto(assignment.name(), expression(assignment.value()));
    }

    private static AnnotationUsageDto annotationUsage(AnnotationUsage usage) {
        return new AnnotationUsageDto(
                usage.name(),
                usage.arguments().stream().map(ParserDtoMapper::annotationArgument).toList(),
                position(usage.position())
        );
    }

    private static AnnotationArgumentDto annotationArgument(AnnotationArgument argument) {
        return new AnnotationArgumentDto(
                argument.name(),
                annotationValue(argument.value()),
                position(argument.position())
        );
    }

    private static AnnotationTargetDto annotationTarget(AnnotationTarget target) {
        return new AnnotationTargetDto(target.name(), position(target.position()));
    }

    private static AnnotationFieldDefinitionDto annotationField(AnnotationFieldDeclaration field) {
        return new AnnotationFieldDefinitionDto(
                field.name(),
                field.type(),
                field.defaultValue().map(ParserDtoMapper::annotationValue),
                position(field.position())
        );
    }

    private static AnnotationValueDto annotationValue(AnnotationValue value) {
        return switch (value) {
            case AnnotationValue.StringValue stringValue -> new StringAnnotationValueDto(stringValue.value(), position(stringValue.position()));
            case AnnotationValue.IntValue intValue -> new IntAnnotationValueDto(intValue.value(), position(intValue.position()));
            case AnnotationValue.LongValue longValue -> new LongAnnotationValueDto(longValue.value(), position(longValue.position()));
            case AnnotationValue.FloatValue floatValue -> new FloatAnnotationValueDto(floatValue.value(), position(floatValue.position()));
            case AnnotationValue.DoubleValue doubleValue -> new DoubleAnnotationValueDto(doubleValue.value(), position(doubleValue.position()));
            case AnnotationValue.BoolValue boolValue -> new BoolAnnotationValueDto(boolValue.value(), position(boolValue.position()));
            case AnnotationValue.NothingValue nothingValue -> new NothingAnnotationValueDto(position(nothingValue.position()));
            case AnnotationValue.TypeNameValue typeNameValue -> new TypeNameAnnotationValueDto(typeNameValue.name(), position(typeNameValue.position()));
        };
    }

    private static Optional<String> visibility(Visibility visibility) {
        return Optional.ofNullable(visibility).map(value -> value.name().toLowerCase(java.util.Locale.ROOT));
    }

    private static String primitiveName(PrimitiveType primitiveType) {
        return switch (primitiveType) {
            case BYTE -> "byte";
            case INT -> "int";
            case LONG -> "long";
            case DOUBLE -> "double";
            case STRING -> "String";
            case BOOL -> "bool";
            case FLOAT -> "float";
            case ANY -> "any";
            case DATA -> "data";
            case ENUM -> "enum";
            case NOTHING -> "nothing";
        };
    }

    private static Optional<SourcePositionDto> position(Optional<SourcePosition> position) {
        return position.map(ParserDtoMapper::position);
    }

    private static SourcePositionDto position(SourcePosition position) {
        return new SourcePositionDto(position.line(), position.column(), position.length());
    }
}
