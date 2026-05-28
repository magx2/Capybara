package dev.capylang.compiler.parser.facade;

import dev.capylang.compiler.ImportDeclaration;
import dev.capylang.compiler.Visibility;
import dev.capylang.compiler.parser.*;
import dev.capylang.compiler.parser.facade.ParserSchema.*;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;

public final class ParserIrAdapters {
    private ParserIrAdapters() {
    }

    public static RawModule rawModule(SourceModuleDto module) {
        return new RawModule(module.name(), module.path(), module.input(), sourceKind(module.sourceKind()));
    }

    public static SourceKind sourceKind(SourceKindDto sourceKind) {
        return switch (sourceKind) {
            case FUNCTIONAL -> SourceKind.FUNCTIONAL;
            case OBJECT_ORIENTED -> SourceKind.OBJECT_ORIENTED;
        };
    }

    public static ImportDeclaration importDeclaration(ImportDto importDto) {
        return new ImportDeclaration(
                importDto.moduleName(),
                importDto.symbols(),
                importDto.excludedSymbols(),
                importDto.qualifiedOnly()
        );
    }

    public static Program functionalProgram(FunctionalProgramDto program) {
        return new Program(program.modules().stream()
                .map(ParserIrAdapters::functionalModule)
                .toList());
    }

    public static dev.capylang.compiler.parser.Module functionalModule(FunctionalModuleDto module) {
        var definitions = module.definitions().stream()
                .map(ParserIrAdapters::definition)
                .collect(java.util.stream.Collectors.toCollection(LinkedHashSet::new));
        return new dev.capylang.compiler.parser.Module(
                module.name(),
                module.path(),
                new Functional(definitions),
                module.imports().stream().map(ParserIrAdapters::importDeclaration).toList(),
                sourceKind(module.sourceKind())
        );
    }

    public static Definition definition(FunctionalDefinitionDto definition) {
        if (definition instanceof FunctionDefinitionDto function) {
            return new Function(
                    function.name(),
                    function.parameters().stream().map(ParserIrAdapters::parameter).toList(),
                    function.returnType().map(ParserIrAdapters::type),
                    expression(function.expression()),
                    function.comments(),
                    visibility(function.visibility()),
                    position(function.position()),
                    function.tailRecursive(),
                    function.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof PrimitiveBackedTypeDefinitionDto primitiveBackedType) {
            return new PrimitiveBackedTypeDeclaration(
                    primitiveBackedType.name(),
                    PrimitiveType.find(primitiveBackedType.backingType())
                            .orElseThrow(() -> new IllegalArgumentException("Unknown primitive type: " + primitiveBackedType.backingType())),
                    primitiveBackedType.constructor().map(ParserIrAdapters::expression),
                    primitiveBackedType.comments(),
                    visibility(primitiveBackedType.visibility()),
                    position(primitiveBackedType.position()),
                    primitiveBackedType.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof UnionDefinitionDto union) {
            return new TypeDeclaration(
                    union.name(),
                    union.subTypes(),
                    union.fields().stream().map(ParserIrAdapters::dataField).toList(),
                    union.typeParameters(),
                    union.constructor().map(ParserIrAdapters::expression),
                    union.derives().stream().map(ParserIrAdapters::deriveDirective).toList(),
                    union.comments(),
                    visibility(union.visibility()),
                    position(union.position()),
                    union.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof DataDefinitionDto data) {
            return new DataDeclaration(
                    data.name(),
                    data.fields().stream().map(ParserIrAdapters::dataField).toList(),
                    data.extendsTypes(),
                    data.typeParameters(),
                    data.constructor().map(ParserIrAdapters::expression),
                    data.derives().stream().map(ParserIrAdapters::deriveDirective).toList(),
                    data.comments(),
                    visibility(data.visibility()),
                    data.nativeType(),
                    position(data.position()),
                    data.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof EnumDefinitionDto enumDefinition) {
            return new EnumDeclaration(
                    enumDefinition.name(),
                    enumDefinition.values(),
                    enumDefinition.comments(),
                    position(enumDefinition.position()),
                    enumDefinition.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof DeriverDefinitionDto deriver) {
            return new DeriverDeclaration(
                    deriver.name(),
                    deriver.methods().stream().map(ParserIrAdapters::deriverMethod).toList(),
                    deriver.comments(),
                    visibility(deriver.visibility()),
                    position(deriver.position()),
                    deriver.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof AnnotationDefinitionDto annotation) {
            return new AnnotationDeclaration(
                    annotation.name(),
                    annotation.targets().stream().map(ParserIrAdapters::annotationTarget).toList(),
                    annotation.fields().stream().map(ParserIrAdapters::annotationField).toList(),
                    annotation.multiple(),
                    annotation.comments(),
                    visibility(annotation.visibility()),
                    position(annotation.position()),
                    annotation.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        throw new IllegalArgumentException("Unknown functional definition DTO: " + definition);
    }

    private static DeriverDeclaration.DeriverMethod deriverMethod(DeriverMethodDto method) {
        return new DeriverDeclaration.DeriverMethod(
                method.name(),
                method.parameters().stream().map(ParserIrAdapters::parameter).toList(),
                type(method.returnType()),
                expression(method.expression()),
                method.comments(),
                position(method.position()),
                method.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
        );
    }

    public static ObjectOrientedModule objectOrientedModule(ObjectOrientedModuleDto module) {
        return new ObjectOrientedModule(
                module.name(),
                module.path(),
                new ObjectOriented(
                        module.definitions().stream().map(ParserIrAdapters::ooTypeDeclaration).toList(),
                        module.nativeProviders().stream().map(ParserIrAdapters::ooNativeProvider).toList()
                ),
                module.imports().stream().map(ParserIrAdapters::importDeclaration).toList(),
                sourceKind(module.sourceKind())
        );
    }

    private static ObjectOriented.NativeProviderDeclaration ooNativeProvider(OoNativeProviderDeclarationDto declaration) {
        return new ObjectOriented.NativeProviderDeclaration(
                declaration.name(),
                declaration.targetType(),
                declaration.qualifier(),
                declaration.comments()
        );
    }

    private static ObjectOriented.TypeDeclaration ooTypeDeclaration(OoTypeDeclarationDto declaration) {
        if (declaration instanceof OoClassDefinitionDto classDefinition) {
            return new ObjectOriented.ClassDeclaration(
                    classDefinition.name(),
                    classDefinition.constructorParameters().stream().map(ParserIrAdapters::ooParameter).toList(),
                    classDefinition.parents().stream().map(ParserIrAdapters::ooTypeReference).toList(),
                    classDefinition.members().stream().map(ParserIrAdapters::ooMember).toList(),
                    classDefinition.modifiers(),
                    classDefinition.comments(),
                    classDefinition.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (declaration instanceof OoTraitDefinitionDto traitDefinition) {
            return new ObjectOriented.TraitDeclaration(
                    traitDefinition.name(),
                    traitDefinition.parents().stream().map(ParserIrAdapters::ooTypeReference).toList(),
                    traitDefinition.members().stream().map(ParserIrAdapters::ooMember).toList(),
                    traitDefinition.comments(),
                    traitDefinition.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (declaration instanceof OoInterfaceDefinitionDto interfaceDefinition) {
            return new ObjectOriented.InterfaceDeclaration(
                    interfaceDefinition.name(),
                    interfaceDefinition.parents().stream().map(ParserIrAdapters::ooTypeReference).toList(),
                    interfaceDefinition.members().stream().map(ParserIrAdapters::ooMember).toList(),
                    interfaceDefinition.comments(),
                    interfaceDefinition.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        throw new IllegalArgumentException("Unknown object-oriented type DTO: " + declaration);
    }

    private static ObjectOriented.MemberDeclaration ooMember(OoMemberDeclarationDto member) {
        if (member instanceof OoFieldDefinitionDto field) {
            return new ObjectOriented.FieldDeclaration(
                    field.name(),
                    field.type(),
                    field.visibility(),
                    field.initializer(),
                    field.comments(),
                    field.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (member instanceof OoMethodDefinitionDto method) {
            return new ObjectOriented.MethodDeclaration(
                    method.name(),
                    method.parameters().stream().map(ParserIrAdapters::ooParameter).toList(),
                    method.returnType(),
                    method.visibility(),
                    method.modifiers(),
                    method.body().map(ParserIrAdapters::ooMethodBody),
                    method.comments(),
                    method.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        if (member instanceof OoInitBlockDto init) {
            return new ObjectOriented.InitBlock(
                    ooStatementBlock(init.body()),
                    init.comments(),
                    init.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
            );
        }
        throw new IllegalArgumentException("Unknown object-oriented member DTO: " + member);
    }

    private static ObjectOriented.MethodBody ooMethodBody(OoMethodBodyDto body) {
        if (body instanceof OoExpressionBodyDto expressionBody) {
            return new ObjectOriented.ExpressionBody(expressionBody.rawExpression());
        }
        if (body instanceof OoStatementBlockDto statementBlock) {
            return ooStatementBlock(statementBlock);
        }
        throw new IllegalArgumentException("Unknown object-oriented method body DTO: " + body);
    }

    private static ObjectOriented.StatementBlock ooStatementBlock(OoStatementBlockDto block) {
        return new ObjectOriented.StatementBlock(block.statements().stream()
                .map(ParserIrAdapters::ooStatement)
                .toList());
    }

    private static ObjectOriented.Statement ooStatement(OoStatementDto statement) {
        if (statement instanceof OoLetStatementDto let) {
            return new ObjectOriented.LetStatement(let.name(), let.type(), let.rawExpression());
        }
        if (statement instanceof OoLocalMethodStatementDto localMethod) {
            return new ObjectOriented.LocalMethodStatement(
                    localMethod.name(),
                    localMethod.parameters().stream().map(ParserIrAdapters::ooParameter).toList(),
                    localMethod.returnType(),
                    ooMethodBody(localMethod.body()),
                    localMethod.comments()
            );
        }
        if (statement instanceof OoMutableVariableStatementDto variable) {
            return new ObjectOriented.MutableVariableStatement(variable.name(), variable.type(), variable.rawExpression());
        }
        if (statement instanceof OoAssignmentStatementDto assignment) {
            return new ObjectOriented.AssignmentStatement(assignment.name(), assignment.rawExpression());
        }
        if (statement instanceof OoExpressionStatementDto expression) {
            return new ObjectOriented.ExpressionStatement(expression.rawExpression());
        }
        if (statement instanceof OoThrowStatementDto throwStatement) {
            return new ObjectOriented.ThrowStatement(throwStatement.rawExpression());
        }
        if (statement instanceof OoReturnStatementDto returnStatement) {
            return new ObjectOriented.ReturnStatement(returnStatement.rawExpression());
        }
        if (statement instanceof OoIfStatementDto ifStatement) {
            return new ObjectOriented.IfStatement(
                    ifStatement.rawCondition(),
                    ooStatementBlock(ifStatement.thenBranch()),
                    ifStatement.elseBranch().map(ParserIrAdapters::ooStatement)
            );
        }
        if (statement instanceof OoTryCatchStatementDto tryCatch) {
            return new ObjectOriented.TryCatchStatement(
                    ooStatementBlock(tryCatch.tryBlock()),
                    tryCatch.catches().stream()
                            .map(catchClause -> new ObjectOriented.CatchClause(catchClause.name(), ooStatementBlock(catchClause.body())))
                            .toList()
            );
        }
        if (statement instanceof OoWhileStatementDto whileStatement) {
            return new ObjectOriented.WhileStatement(whileStatement.rawCondition(), ooStatementBlock(whileStatement.body()));
        }
        if (statement instanceof OoDoWhileStatementDto doWhileStatement) {
            return new ObjectOriented.DoWhileStatement(ooStatementBlock(doWhileStatement.body()), doWhileStatement.rawCondition());
        }
        if (statement instanceof OoForEachStatementDto forEachStatement) {
            return new ObjectOriented.ForEachStatement(
                    forEachStatement.name(),
                    forEachStatement.type(),
                    forEachStatement.rawIterable(),
                    ooStatementBlock(forEachStatement.body())
            );
        }
        if (statement instanceof OoStatementBlockDto statementBlock) {
            return ooStatementBlock(statementBlock);
        }
        throw new IllegalArgumentException("Unknown object-oriented statement DTO: " + statement);
    }

    private static ObjectOriented.Parameter ooParameter(OoParameterDto parameter) {
        return new ObjectOriented.Parameter(parameter.name(), parameter.type());
    }

    private static ObjectOriented.TypeReference ooTypeReference(OoTypeReferenceDto typeReference) {
        return new ObjectOriented.TypeReference(typeReference.name());
    }

    private static Parameter parameter(ParameterDto parameter) {
        return new Parameter(type(parameter.type()), parameter.name(), position(parameter.position()));
    }

    private static DataDeclaration.DataField dataField(DataFieldDto field) {
        return new DataDeclaration.DataField(
                field.name(),
                type(field.type()),
                field.annotations().stream().map(ParserIrAdapters::annotationUsage).toList()
        );
    }

    private static DeriveDirective deriveDirective(DeriveDirectiveDto directive) {
        return new DeriveDirective(directive.name(), position(directive.position()));
    }

    public static Type type(TypeDto type) {
        if (type instanceof PrimitiveTypeDto primitiveType) {
            return PrimitiveType.find(primitiveType.name())
                    .orElseThrow(() -> new IllegalArgumentException("Unknown primitive type: " + primitiveType.name()));
        }
        if (type instanceof DataTypeDto dataType) {
            return new DataType(dataType.name());
        }
        if (type instanceof ListTypeDto listType) {
            return new CollectionType.ListType(type(listType.elementType()));
        }
        if (type instanceof SetTypeDto setType) {
            return new CollectionType.SetType(type(setType.elementType()));
        }
        if (type instanceof DictTypeDto dictType) {
            return new CollectionType.DictType(type(dictType.valueType()));
        }
        if (type instanceof FunctionTypeDto functionType) {
            return new FunctionType(type(functionType.argumentType()), type(functionType.returnType()));
        }
        if (type instanceof TupleTypeDto tupleType) {
            return new TupleType(tupleType.elementTypes().stream().map(ParserIrAdapters::type).toList());
        }
        throw new IllegalArgumentException("Unknown type DTO: " + type);
    }

    public static Expression expression(ExpressionDto expression) {
        if (expression instanceof BoolExpressionDto value) {
            return new BooleanValue(value.value(), position(value.position()));
        }
        if (expression instanceof ByteExpressionDto value) {
            return new ByteValue(value.value(), position(value.position()));
        }
        if (expression instanceof IntExpressionDto value) {
            return new IntValue(value.value(), position(value.position()));
        }
        if (expression instanceof LongExpressionDto value) {
            return new LongValue(value.value(), position(value.position()));
        }
        if (expression instanceof FloatExpressionDto value) {
            return new FloatValue(value.value(), position(value.position()));
        }
        if (expression instanceof DoubleExpressionDto value) {
            return new DoubleValue(value.value(), position(value.position()));
        }
        if (expression instanceof StringExpressionDto value) {
            return new StringValue(value.value(), position(value.position()));
        }
        if (expression instanceof NothingExpressionDto value) {
            return new NothingValue(position(value.position()), value.literal());
        }
        if (expression instanceof ValueExpressionDto value) {
            return new Value(value.name(), position(value.position()));
        }
        if (expression instanceof FunctionCallExpressionDto call) {
            return new FunctionCall(
                    call.moduleName(),
                    call.name(),
                    call.arguments().stream().map(ParserIrAdapters::expression).toList(),
                    position(call.position())
            );
        }
        if (expression instanceof FunctionInvokeExpressionDto invoke) {
            return new FunctionInvoke(
                    expression(invoke.function()),
                    invoke.arguments().stream().map(ParserIrAdapters::expression).toList(),
                    position(invoke.position())
            );
        }
        if (expression instanceof FunctionReferenceExpressionDto reference) {
            return new FunctionReference(reference.name(), position(reference.position()));
        }
        if (expression instanceof IfExpressionDto ifExpression) {
            return new IfExpression(
                    expression(ifExpression.condition()),
                    expression(ifExpression.thenBranch()),
                    expression(ifExpression.elseBranch()),
                    position(ifExpression.position())
            );
        }
        if (expression instanceof InfixExpressionDto infix) {
            return new InfixExpression(
                    expression(infix.left()),
                    InfixOperator.fromSymbol(infix.operator()),
                    expression(infix.right()),
                    position(infix.position())
            );
        }
        if (expression instanceof FieldAccessExpressionDto access) {
            return new FieldAccess(expression(access.source()), access.field(), position(access.position()));
        }
        if (expression instanceof LambdaExpressionDto lambda) {
            return new LambdaExpression(lambda.argumentNames(), expression(lambda.expression()), position(lambda.position()));
        }
        if (expression instanceof LetExpressionDto let) {
            return new LetExpression(
                    let.name(),
                    let.declaredType().map(ParserIrAdapters::type),
                    LetExpression.Kind.valueOf(let.kind()),
                    expression(let.value()),
                    expression(let.rest()),
                    position(let.position())
            );
        }
        if (expression instanceof ReduceExpressionDto reduce) {
            return new ReduceExpression(
                    expression(reduce.initialValue()),
                    reduce.accumulatorName(),
                    reduce.keyName(),
                    reduce.valueName(),
                    expression(reduce.reducerExpression()),
                    position(reduce.position())
            );
        }
        if (expression instanceof IndexExpressionDto index) {
            return new IndexExpression(
                    expression(index.source()),
                    index.arguments().stream().map(ParserIrAdapters::expression).toList(),
                    position(index.position())
            );
        }
        if (expression instanceof SliceExpressionDto slice) {
            return new SliceExpression(
                    expression(slice.source()),
                    slice.start().map(ParserIrAdapters::expression),
                    slice.end().map(ParserIrAdapters::expression),
                    position(slice.position())
            );
        }
        if (expression instanceof MatchExpressionDto match) {
            return new MatchExpression(
                    expression(match.matchWith()),
                    match.cases().stream().map(ParserIrAdapters::matchCase).toList(),
                    position(match.position())
            );
        }
        if (expression instanceof NewDataExpressionDto newData) {
            return new NewData(
                    type(newData.type()),
                    newData.bypassConstructor(),
                    newData.assignments().stream().map(ParserIrAdapters::fieldAssignment).toList(),
                    newData.positionalArguments().stream().map(ParserIrAdapters::expression).toList(),
                    newData.spreads().stream().map(ParserIrAdapters::expression).toList(),
                    position(newData.position())
            );
        }
        if (expression instanceof ConstructorDataExpressionDto constructorData) {
            return new ConstructorData(
                    constructorData.assignments().stream().map(ParserIrAdapters::fieldAssignment).toList(),
                    constructorData.positionalArguments().stream().map(ParserIrAdapters::expression).toList(),
                    constructorData.spreads().stream().map(ParserIrAdapters::expression).toList(),
                    position(constructorData.position())
            );
        }
        if (expression instanceof WithExpressionDto with) {
            return new WithExpression(
                    expression(with.source()),
                    with.assignments().stream().map(ParserIrAdapters::fieldAssignment).toList(),
                    position(with.position())
            );
        }
        if (expression instanceof NewListExpressionDto list) {
            return new NewListExpression(list.values().stream().map(ParserIrAdapters::expression).toList(), position(list.position()));
        }
        if (expression instanceof NewSetExpressionDto set) {
            return new NewSetExpression(set.values().stream().map(ParserIrAdapters::expression).toList(), position(set.position()));
        }
        if (expression instanceof NewDictExpressionDto dict) {
            return new NewDictExpression(
                    dict.entries().stream()
                            .map(entry -> new NewDictExpression.Entry(expression(entry.key()), expression(entry.value())))
                            .toList(),
                    position(dict.position())
            );
        }
        if (expression instanceof TupleExpressionDto tuple) {
            return new TupleExpression(tuple.values().stream().map(ParserIrAdapters::expression).toList(), position(tuple.position()));
        }
        if (expression instanceof UnwrapExpressionDto unwrap) {
            return new UnwrapExpression(expression(unwrap.expression()), position(unwrap.position()));
        }
        if (expression instanceof PlaceholderExpressionDto placeholder) {
            return new PlaceholderExpression(position(placeholder.position()));
        }
        throw new IllegalArgumentException("Unknown expression DTO: " + expression);
    }

    private static MatchExpression.MatchCase matchCase(MatchCaseDto matchCase) {
        return new MatchExpression.MatchCase(
                pattern(matchCase.pattern()),
                matchCase.guard().map(ParserIrAdapters::expression),
                expression(matchCase.expression())
        );
    }

    private static MatchExpression.Pattern pattern(PatternDto pattern) {
        if (pattern instanceof IntPatternDto intPattern) {
            return new MatchExpression.IntPattern(intPattern.value());
        }
        if (pattern instanceof LongPatternDto longPattern) {
            return new MatchExpression.LongPattern(longPattern.value());
        }
        if (pattern instanceof StringPatternDto stringPattern) {
            return new MatchExpression.StringPattern(stringPattern.value());
        }
        if (pattern instanceof BoolPatternDto boolPattern) {
            return new MatchExpression.BoolPattern(boolPattern.value());
        }
        if (pattern instanceof FloatPatternDto floatPattern) {
            return new MatchExpression.FloatPattern(floatPattern.value());
        }
        if (pattern instanceof TypedPatternDto typedPattern) {
            return new MatchExpression.TypedPattern(type(typedPattern.type()), typedPattern.name());
        }
        if (pattern instanceof VariablePatternDto variablePattern) {
            return new MatchExpression.VariablePattern(variablePattern.name());
        }
        if (pattern instanceof WildcardPatternDto) {
            return MatchExpression.WildcardPattern.WILDCARD;
        }
        if (pattern instanceof WildcardBindingPatternDto wildcardBindingPattern) {
            return new MatchExpression.WildcardBindingPattern(wildcardBindingPattern.name());
        }
        if (pattern instanceof ConstructorPatternDto constructorPattern) {
            return new MatchExpression.ConstructorPattern(
                    constructorPattern.constructorName(),
                    constructorPattern.fieldPatterns().stream().map(ParserIrAdapters::pattern).toList()
            );
        }
        throw new IllegalArgumentException("Unknown pattern DTO: " + pattern);
    }

    private static NewData.FieldAssignment fieldAssignment(FieldAssignmentDto assignment) {
        return new NewData.FieldAssignment(assignment.name(), expression(assignment.value()));
    }

    private static AnnotationUsage annotationUsage(AnnotationUsageDto usage) {
        return new AnnotationUsage(
                usage.name(),
                usage.arguments().stream().map(ParserIrAdapters::annotationArgument).toList(),
                position(usage.position())
        );
    }

    private static AnnotationArgument annotationArgument(AnnotationArgumentDto argument) {
        return new AnnotationArgument(
                argument.name(),
                annotationValue(argument.value()),
                position(argument.position())
        );
    }

    private static AnnotationTarget annotationTarget(AnnotationTargetDto target) {
        return new AnnotationTarget(target.name(), position(target.position()));
    }

    private static AnnotationFieldDeclaration annotationField(AnnotationFieldDefinitionDto field) {
        return new AnnotationFieldDeclaration(
                field.name(),
                field.type(),
                field.defaultValue().map(ParserIrAdapters::annotationValue),
                position(field.position())
        );
    }

    private static AnnotationValue annotationValue(AnnotationValueDto value) {
        if (value instanceof StringAnnotationValueDto stringValue) {
            return new AnnotationValue.StringValue(stringValue.value(), position(stringValue.position()));
        }
        if (value instanceof IntAnnotationValueDto intValue) {
            return new AnnotationValue.IntValue(intValue.value(), position(intValue.position()));
        }
        if (value instanceof LongAnnotationValueDto longValue) {
            return new AnnotationValue.LongValue(longValue.value(), position(longValue.position()));
        }
        if (value instanceof FloatAnnotationValueDto floatValue) {
            return new AnnotationValue.FloatValue(floatValue.value(), position(floatValue.position()));
        }
        if (value instanceof DoubleAnnotationValueDto doubleValue) {
            return new AnnotationValue.DoubleValue(doubleValue.value(), position(doubleValue.position()));
        }
        if (value instanceof BoolAnnotationValueDto boolValue) {
            return new AnnotationValue.BoolValue(boolValue.value(), position(boolValue.position()));
        }
        if (value instanceof NothingAnnotationValueDto nothingValue) {
            return new AnnotationValue.NothingValue(position(nothingValue.position()));
        }
        if (value instanceof TypeNameAnnotationValueDto typeNameValue) {
            return new AnnotationValue.TypeNameValue(typeNameValue.name(), position(typeNameValue.position()));
        }
        throw new IllegalArgumentException("Unknown annotation value DTO: " + value);
    }

    private static Visibility visibility(Optional<String> visibility) {
        return visibility.map(value -> Visibility.valueOf(value.toUpperCase(java.util.Locale.ROOT))).orElse(null);
    }

    private static Optional<SourcePosition> position(Optional<SourcePositionDto> position) {
        return position.map(value -> new SourcePosition(value.line(), value.column(), value.length()));
    }
}
