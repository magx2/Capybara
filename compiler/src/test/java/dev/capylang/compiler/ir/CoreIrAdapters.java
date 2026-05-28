package dev.capylang.compiler.ir;

import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledAnnotation;
import dev.capylang.compiler.CompiledAnnotationArgument;
import dev.capylang.compiler.CompiledAnnotationValue;
import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunction;
import dev.capylang.compiler.CompiledFunctionType;
import dev.capylang.compiler.CompiledGenericTypeParameter;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledNativeProviderBinding;
import dev.capylang.compiler.CompiledNativeProviderDeclaration;
import dev.capylang.compiler.CompiledObjectKind;
import dev.capylang.compiler.CompiledObjectMethod;
import dev.capylang.compiler.CompiledObjectMethodParameter;
import dev.capylang.compiler.CompiledObjectType;
import dev.capylang.compiler.CompiledPrimitiveBackedType;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.CompiledTupleType;
import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.GenericDataType;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderBinding;
import dev.capylang.compiler.NativeProviderCatalog;
import dev.capylang.compiler.NativeProviderManifest;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.Visibility;
import dev.capylang.compiler.expression.CompiledBooleanValue;
import dev.capylang.compiler.expression.CompiledExpression;
import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledInfixExpression;
import dev.capylang.compiler.expression.CompiledIntValue;
import dev.capylang.compiler.expression.CompiledNewData;
import dev.capylang.compiler.expression.CompiledNothingValue;
import dev.capylang.compiler.expression.CompiledStringValue;
import dev.capylang.compiler.expression.CompiledVariable;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.compiler.parser.Program;
import dev.capylang.compiler.parser.facade.ParserDtoMapper;
import dev.capylang.compiler.parser.facade.ParserIrAdapters;
import dev.capylang.compiler.parser.facade.ParserSchema;
import dev.capylang.generator.GeneratedModule;
import dev.capylang.generator.GeneratedProgram;

import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Collectors;

final class CoreIrAdapters {
    private CoreIrAdapters() {
    }

    static CoreIr.FunctionalProgram functionalProgram(Program program) {
        return functionalProgram(ParserDtoMapper.functionalProgram(program));
    }

    static CoreIr.FunctionalProgram functionalProgram(ParserSchema.FunctionalProgramDto program) {
        return new CoreIr.FunctionalProgram(program.modules().stream()
                .map(CoreIrAdapters::functionalModule)
                .toList());
    }

    static ParserSchema.FunctionalProgramDto functionalProgramDto(CoreIr.FunctionalProgram program) {
        return new ParserSchema.FunctionalProgramDto(program.modules().stream()
                .map(CoreIrAdapters::functionalModuleDto)
                .toList());
    }

    static CoreIr.ObjectOrientedModule objectOrientedModule(ObjectOrientedModule module) {
        return objectOrientedModule(ParserDtoMapper.objectOrientedModule(module));
    }

    static ParserSchema.ObjectOrientedModuleDto objectOrientedModuleDto(CoreIr.ObjectOrientedModule module) {
        return new ParserSchema.ObjectOrientedModuleDto(
                module.module().name(),
                module.module().path(),
                module.definitions().stream().map(CoreIrAdapters::ooDeclarationDto).toList(),
                module.native_providers().stream().map(CoreIrAdapters::ooNativeProviderDto).toList(),
                module.imports().stream().map(CoreIrAdapters::importDto).toList(),
                sourceKindDto(module.source_kind())
        );
    }

    static CoreIr.CompiledProgram compiledProgram(CompiledProgram program) {
        return new CoreIr.CompiledProgram(
                program.modules().stream().map(CoreIrAdapters::compiledModule).toList(),
                program.objectOrientedModules().stream().map(CoreIrAdapters::objectOrientedModule).toList(),
                nativeProviderManifest(program.nativeProviders()),
                nativeProviderCatalog(program.nativeProviderCatalog())
        );
    }

    static CoreIrSchema.CompiledProgramDto compiledProgramDto(CoreIr.CompiledProgram program) {
        return new CoreIrSchema.CompiledProgramDto(
                program.modules().stream().map(CoreIrAdapters::compiledModuleDto).toList(),
                program.object_oriented_modules().stream().map(CoreIrAdapters::objectOrientedModuleDto).toList(),
                nativeProviderManifestDto(program.native_providers()),
                nativeProviderCatalogDto(program.native_provider_catalog())
        );
    }

    static CompiledProgram compiledProgram(CoreIrSchema.CompiledProgramDto program) {
        return new CompiledProgram(
                program.modules().stream().map(CoreIrAdapters::compiledModule).collect(Collectors.toCollection(TreeSet::new)),
                program.objectOrientedModules().stream().map(ParserIrAdapters::objectOrientedModule).toList(),
                nativeProviderManifest(program.nativeProviders()),
                nativeProviderCatalog(program.nativeProviderCatalog())
        );
    }

    static CoreIr.GeneratedProgram generatedProgram(GeneratedProgram program) {
        return new CoreIr.GeneratedProgram(program.modules().stream()
                .map(module -> new CoreIr.GeneratedModule(module.relativePath().toString().replace('\\', '/'), module.code()))
                .toList());
    }

    static CoreIrSchema.GeneratedProgramDto generatedProgramDto(CoreIr.GeneratedProgram program) {
        return new CoreIrSchema.GeneratedProgramDto(program.modules().stream()
                .map(module -> new CoreIrSchema.GeneratedModuleDto(module.relative_path(), module.code()))
                .toList());
    }

    static GeneratedProgram generatedProgram(CoreIrSchema.GeneratedProgramDto program) {
        return new GeneratedProgram(program.modules().stream()
                .map(module -> new GeneratedModule(Path.of(module.relativePath()), module.code()))
                .toList());
    }

    private static CoreIr.FunctionalModule functionalModule(ParserSchema.FunctionalModuleDto module) {
        return new CoreIr.FunctionalModule(
                new CoreIr.ModuleId(module.name(), module.path()),
                module.definitions().stream().map(CoreIrAdapters::functionalDeclaration).toList(),
                module.imports().stream().map(CoreIrAdapters::importDeclaration).toList(),
                sourceKind(module.sourceKind())
        );
    }

    private static ParserSchema.FunctionalModuleDto functionalModuleDto(CoreIr.FunctionalModule module) {
        return new ParserSchema.FunctionalModuleDto(
                module.module().name(),
                module.module().path(),
                module.definitions().stream().map(CoreIrAdapters::functionalDeclarationDto).toList(),
                module.imports().stream().map(CoreIrAdapters::importDto).toList(),
                sourceKindDto(module.source_kind())
        );
    }

    private static CoreIr.FunctionalDeclaration functionalDeclaration(ParserSchema.FunctionalDefinitionDto definition) {
        if (definition instanceof ParserSchema.FunctionDefinitionDto function) {
            return new CoreIr.FunctionDeclaration(
                    function.name(),
                    function.parameters().stream().map(CoreIrAdapters::parameter).toList(),
                    mapOptional(function.returnType(), CoreIrAdapters::parsedType),
                    parsedExpression(function.expression()),
                    function.comments(),
                    visibility(function.visibility()),
                    mapOptional(function.position(), CoreIrAdapters::sourcePosition),
                    function.tailRecursive(),
                    function.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof ParserSchema.PrimitiveBackedTypeDefinitionDto primitive) {
            return new CoreIr.PrimitiveBackedTypeDeclaration(
                    primitive.name(),
                    primitiveType(primitive.backingType()),
                    mapOptional(primitive.constructor(), CoreIrAdapters::parsedExpression),
                    primitive.comments(),
                    visibility(primitive.visibility()),
                    mapOptional(primitive.position(), CoreIrAdapters::sourcePosition),
                    primitive.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof ParserSchema.UnionDefinitionDto union) {
            return new CoreIr.UnionDeclaration(
                    union.name(),
                    union.subTypes(),
                    union.fields().stream().map(CoreIrAdapters::dataField).toList(),
                    union.typeParameters(),
                    mapOptional(union.constructor(), CoreIrAdapters::parsedExpression),
                    union.derives().stream().map(CoreIrAdapters::deriveDirective).toList(),
                    union.comments(),
                    visibility(union.visibility()),
                    mapOptional(union.position(), CoreIrAdapters::sourcePosition),
                    union.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof ParserSchema.DataDefinitionDto data) {
            return new CoreIr.DataDeclaration(
                    data.name(),
                    data.fields().stream().map(CoreIrAdapters::dataField).toList(),
                    data.extendsTypes(),
                    data.typeParameters(),
                    mapOptional(data.constructor(), CoreIrAdapters::parsedExpression),
                    data.derives().stream().map(CoreIrAdapters::deriveDirective).toList(),
                    data.comments(),
                    visibility(data.visibility()),
                    data.nativeType(),
                    mapOptional(data.position(), CoreIrAdapters::sourcePosition),
                    data.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof ParserSchema.EnumDefinitionDto enumDefinition) {
            return new CoreIr.EnumDeclaration(
                    enumDefinition.name(),
                    enumDefinition.values(),
                    enumDefinition.comments(),
                    mapOptional(enumDefinition.position(), CoreIrAdapters::sourcePosition),
                    enumDefinition.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof ParserSchema.DeriverDefinitionDto deriver) {
            return new CoreIr.DeriverDeclaration(
                    deriver.name(),
                    deriver.methods().stream().map(CoreIrAdapters::deriverMethod).toList(),
                    deriver.comments(),
                    visibility(deriver.visibility()),
                    mapOptional(deriver.position(), CoreIrAdapters::sourcePosition),
                    deriver.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        if (definition instanceof ParserSchema.AnnotationDefinitionDto annotation) {
            return new CoreIr.AnnotationDeclaration(
                    annotation.name(),
                    annotation.targets().stream().map(CoreIrAdapters::annotationTarget).toList(),
                    annotation.fields().stream().map(CoreIrAdapters::annotationField).toList(),
                    annotation.multiple(),
                    annotation.comments(),
                    visibility(annotation.visibility()),
                    mapOptional(annotation.position(), CoreIrAdapters::sourcePosition),
                    annotation.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
            );
        }
        throw unsupported("functional definition", definition);
    }

    private static ParserSchema.FunctionalDefinitionDto functionalDeclarationDto(CoreIr.FunctionalDeclaration declaration) {
        if (declaration instanceof CoreIr.FunctionDeclaration function) {
            return new ParserSchema.FunctionDefinitionDto(
                    function.name(),
                    function.parameters().stream().map(CoreIrAdapters::parameterDto).toList(),
                    optional(function.return_type()).map(CoreIrAdapters::parsedTypeDto),
                    parsedExpressionDto(function.expression()),
                    function.comments(),
                    visibilityDto(optional(function.visibility())),
                    optional(function.position()).map(CoreIrAdapters::sourcePositionDto),
                    function.tail_recursive(),
                    function.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.PrimitiveBackedTypeDeclaration primitive) {
            return new ParserSchema.PrimitiveBackedTypeDefinitionDto(
                    primitive.name(),
                    primitiveName(primitive.backing_type()),
                    optional(primitive.constructor_expression()).map(CoreIrAdapters::parsedExpressionDto),
                    primitive.comments(),
                    visibilityDto(optional(primitive.visibility())),
                    optional(primitive.position()).map(CoreIrAdapters::sourcePositionDto),
                    primitive.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.UnionDeclaration union) {
            return new ParserSchema.UnionDefinitionDto(
                    union.name(),
                    union.sub_types(),
                    union.fields().stream().map(CoreIrAdapters::dataFieldDto).toList(),
                    union.type_parameters(),
                    optional(union.constructor_expression()).map(CoreIrAdapters::parsedExpressionDto),
                    union.derives().stream().map(CoreIrAdapters::deriveDirectiveDto).toList(),
                    union.comments(),
                    visibilityDto(optional(union.visibility())),
                    optional(union.position()).map(CoreIrAdapters::sourcePositionDto),
                    union.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.DataDeclaration data) {
            return new ParserSchema.DataDefinitionDto(
                    data.name(),
                    data.fields().stream().map(CoreIrAdapters::dataFieldDto).toList(),
                    data.extends_types(),
                    data.type_parameters(),
                    optional(data.constructor_expression()).map(CoreIrAdapters::parsedExpressionDto),
                    data.derives().stream().map(CoreIrAdapters::deriveDirectiveDto).toList(),
                    data.comments(),
                    visibilityDto(optional(data.visibility())),
                    data.native_type(),
                    optional(data.position()).map(CoreIrAdapters::sourcePositionDto),
                    data.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.EnumDeclaration enumDeclaration) {
            return new ParserSchema.EnumDefinitionDto(
                    enumDeclaration.name(),
                    enumDeclaration.values(),
                    enumDeclaration.comments(),
                    optional(enumDeclaration.position()).map(CoreIrAdapters::sourcePositionDto),
                    enumDeclaration.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.DeriverDeclaration deriver) {
            return new ParserSchema.DeriverDefinitionDto(
                    deriver.name(),
                    deriver.methods().stream().map(CoreIrAdapters::deriverMethodDto).toList(),
                    deriver.comments(),
                    visibilityDto(optional(deriver.visibility())),
                    optional(deriver.position()).map(CoreIrAdapters::sourcePositionDto),
                    deriver.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.AnnotationDeclaration annotation) {
            return new ParserSchema.AnnotationDefinitionDto(
                    annotation.name(),
                    annotation.targets().stream().map(CoreIrAdapters::annotationTargetDto).toList(),
                    annotation.fields().stream().map(CoreIrAdapters::annotationFieldDto).toList(),
                    annotation.multiple(),
                    annotation.comments(),
                    visibilityDto(optional(annotation.visibility())),
                    optional(annotation.position()).map(CoreIrAdapters::sourcePositionDto),
                    annotation.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        throw unsupported("functional declaration", declaration);
    }

    private static CoreIr.ObjectOrientedModule objectOrientedModule(ParserSchema.ObjectOrientedModuleDto module) {
        return new CoreIr.ObjectOrientedModule(
                new CoreIr.ModuleId(module.name(), module.path()),
                module.definitions().stream().map(CoreIrAdapters::ooDeclaration).toList(),
                module.nativeProviders().stream().map(CoreIrAdapters::ooNativeProvider).toList(),
                module.imports().stream().map(CoreIrAdapters::importDeclaration).toList(),
                sourceKind(module.sourceKind())
        );
    }

    private static CoreIr.OoDeclaration ooDeclaration(ParserSchema.OoTypeDeclarationDto declaration) {
        if (declaration instanceof ParserSchema.OoClassDefinitionDto classDefinition) {
            return new CoreIr.OoClassDeclaration(
                    classDefinition.name(),
                    classDefinition.constructorParameters().stream().map(CoreIrAdapters::ooParameter).toList(),
                    classDefinition.parents().stream().map(CoreIrAdapters::ooTypeReference).toList(),
                    classDefinition.members().stream().map(CoreIrAdapters::ooMember).toList(),
                    classDefinition.modifiers(),
                    classDefinition.comments(),
                    classDefinition.annotations().stream().map(CoreIrAdapters::annotationUsage).toList(),
                    List.of()
            );
        }
        if (declaration instanceof ParserSchema.OoTraitDefinitionDto traitDefinition) {
            return new CoreIr.OoTraitDeclaration(
                    traitDefinition.name(),
                    traitDefinition.parents().stream().map(CoreIrAdapters::ooTypeReference).toList(),
                    traitDefinition.members().stream().map(CoreIrAdapters::ooMember).toList(),
                    traitDefinition.comments(),
                    traitDefinition.annotations().stream().map(CoreIrAdapters::annotationUsage).toList(),
                    List.of()
            );
        }
        if (declaration instanceof ParserSchema.OoInterfaceDefinitionDto interfaceDefinition) {
            return new CoreIr.OoInterfaceDeclaration(
                    interfaceDefinition.name(),
                    interfaceDefinition.parents().stream().map(CoreIrAdapters::ooTypeReference).toList(),
                    interfaceDefinition.members().stream().map(CoreIrAdapters::ooMember).toList(),
                    interfaceDefinition.comments(),
                    interfaceDefinition.annotations().stream().map(CoreIrAdapters::annotationUsage).toList(),
                    List.of()
            );
        }
        throw unsupported("OO declaration", declaration);
    }

    private static ParserSchema.OoTypeDeclarationDto ooDeclarationDto(CoreIr.OoDeclaration declaration) {
        if (declaration instanceof CoreIr.OoClassDeclaration classDeclaration) {
            return new ParserSchema.OoClassDefinitionDto(
                    classDeclaration.name(),
                    classDeclaration.constructor_parameters().stream().map(CoreIrAdapters::ooParameterDto).toList(),
                    classDeclaration.parents().stream().map(CoreIrAdapters::ooTypeReferenceDto).toList(),
                    classDeclaration.members().stream().map(CoreIrAdapters::ooMemberDto).toList(),
                    classDeclaration.modifiers(),
                    classDeclaration.comments(),
                    classDeclaration.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.OoTraitDeclaration traitDeclaration) {
            return new ParserSchema.OoTraitDefinitionDto(
                    traitDeclaration.name(),
                    traitDeclaration.parents().stream().map(CoreIrAdapters::ooTypeReferenceDto).toList(),
                    traitDeclaration.members().stream().map(CoreIrAdapters::ooMemberDto).toList(),
                    traitDeclaration.comments(),
                    traitDeclaration.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (declaration instanceof CoreIr.OoInterfaceDeclaration interfaceDeclaration) {
            return new ParserSchema.OoInterfaceDefinitionDto(
                    interfaceDeclaration.name(),
                    interfaceDeclaration.parents().stream().map(CoreIrAdapters::ooTypeReferenceDto).toList(),
                    interfaceDeclaration.members().stream().map(CoreIrAdapters::ooMemberDto).toList(),
                    interfaceDeclaration.comments(),
                    interfaceDeclaration.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        throw unsupported("OO declaration", declaration);
    }

    private static CoreIr.OoMemberDeclaration ooMember(ParserSchema.OoMemberDeclarationDto member) {
        if (member instanceof ParserSchema.OoFieldDefinitionDto field) {
            return new CoreIr.OoFieldDeclaration(
                    field.name(),
                    field.type(),
                    field.visibility(),
                    field.initializer(),
                    field.comments(),
                    field.annotations().stream().map(CoreIrAdapters::annotationUsage).toList(),
                    List.of()
            );
        }
        if (member instanceof ParserSchema.OoMethodDefinitionDto method) {
            return new CoreIr.OoMethodDeclaration(
                    method.name(),
                    method.parameters().stream().map(CoreIrAdapters::ooParameter).toList(),
                    method.returnType(),
                    method.visibility(),
                    method.modifiers(),
                    mapOptional(method.body(), CoreIrAdapters::ooMethodBody),
                    method.comments(),
                    method.annotations().stream().map(CoreIrAdapters::annotationUsage).toList(),
                    List.of()
            );
        }
        if (member instanceof ParserSchema.OoInitBlockDto init) {
            return new CoreIr.OoInitBlock(
                    ooStatementBlock(init.body()),
                    init.comments(),
                    init.annotations().stream().map(CoreIrAdapters::annotationUsage).toList(),
                    List.of()
            );
        }
        throw unsupported("OO member", member);
    }

    private static ParserSchema.OoMemberDeclarationDto ooMemberDto(CoreIr.OoMemberDeclaration member) {
        if (member instanceof CoreIr.OoFieldDeclaration field) {
            return new ParserSchema.OoFieldDefinitionDto(
                    field.name(),
                    field.type(),
                    field.visibility(),
                    optional(field.initializer()),
                    field.comments(),
                    field.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (member instanceof CoreIr.OoMethodDeclaration method) {
            return new ParserSchema.OoMethodDefinitionDto(
                    method.name(),
                    method.parameters().stream().map(CoreIrAdapters::ooParameterDto).toList(),
                    method.return_type(),
                    method.visibility(),
                    method.modifiers(),
                    optional(method.body()).map(CoreIrAdapters::ooMethodBodyDto),
                    method.comments(),
                    method.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        if (member instanceof CoreIr.OoInitBlock init) {
            return new ParserSchema.OoInitBlockDto(
                    ooStatementBlockDto(init.body()),
                    init.comments(),
                    init.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
            );
        }
        throw unsupported("OO member", member);
    }

    private static CoreIr.OoMethodBody ooMethodBody(ParserSchema.OoMethodBodyDto body) {
        if (body instanceof ParserSchema.OoExpressionBodyDto expressionBody) {
            return new CoreIr.OoExpressionBody(expressionBody.rawExpression());
        }
        if (body instanceof ParserSchema.OoStatementBlockDto statementBlock) {
            return ooStatementBlock(statementBlock);
        }
        throw unsupported("OO method body", body);
    }

    private static ParserSchema.OoMethodBodyDto ooMethodBodyDto(CoreIr.OoMethodBody body) {
        if (body instanceof CoreIr.OoExpressionBody expressionBody) {
            return new ParserSchema.OoExpressionBodyDto(expressionBody.raw_expression());
        }
        if (body instanceof CoreIr.OoStatementBlock statementBlock) {
            return ooStatementBlockDto(statementBlock);
        }
        throw unsupported("OO method body", body);
    }

    private static ParserSchema.OoMethodBodyDto ooMethodBodyDto(Object body) {
        return ooMethodBodyDto((CoreIr.OoMethodBody) body);
    }

    private static CoreIr.OoStatementBlock ooStatementBlock(ParserSchema.OoStatementBlockDto block) {
        return new CoreIr.OoStatementBlock(block.statements().stream().map(CoreIrAdapters::ooStatement).toList());
    }

    private static ParserSchema.OoStatementBlockDto ooStatementBlockDto(CoreIr.OoStatementBlock block) {
        return new ParserSchema.OoStatementBlockDto(block.statements().stream().map(CoreIrAdapters::ooStatementDto).toList());
    }

    private static CoreIr.OoStatement ooStatement(ParserSchema.OoStatementDto statement) {
        if (statement instanceof ParserSchema.OoLetStatementDto let) {
            return new CoreIr.OoLetStatement(let.name(), let.type(), let.rawExpression());
        }
        if (statement instanceof ParserSchema.OoLocalMethodStatementDto localMethod) {
            return new CoreIr.OoLocalMethodStatement(
                    localMethod.name(),
                    localMethod.parameters().stream().map(CoreIrAdapters::ooParameter).toList(),
                    localMethod.returnType(),
                    ooMethodBody(localMethod.body()),
                    localMethod.comments()
            );
        }
        if (statement instanceof ParserSchema.OoMutableVariableStatementDto variable) {
            return new CoreIr.OoMutableVariableStatement(variable.name(), variable.type(), variable.rawExpression());
        }
        if (statement instanceof ParserSchema.OoAssignmentStatementDto assignment) {
            return new CoreIr.OoAssignmentStatement(assignment.name(), assignment.rawExpression());
        }
        if (statement instanceof ParserSchema.OoExpressionStatementDto expression) {
            return new CoreIr.OoExpressionStatement(expression.rawExpression());
        }
        if (statement instanceof ParserSchema.OoThrowStatementDto throwStatement) {
            return new CoreIr.OoThrowStatement(throwStatement.rawExpression());
        }
        if (statement instanceof ParserSchema.OoReturnStatementDto returnStatement) {
            return new CoreIr.OoReturnStatement(returnStatement.rawExpression());
        }
        if (statement instanceof ParserSchema.OoIfStatementDto ifStatement) {
            return new CoreIr.OoIfStatement(
                    ifStatement.rawCondition(),
                    ooStatementBlock(ifStatement.thenBranch()),
                    mapOptional(ifStatement.elseBranch(), CoreIrAdapters::ooStatement)
            );
        }
        if (statement instanceof ParserSchema.OoTryCatchStatementDto tryCatch) {
            return new CoreIr.OoTryCatchStatement(
                    ooStatementBlock(tryCatch.tryBlock()),
                    tryCatch.catches().stream()
                            .map(catchClause -> new CoreIr.OoCatchClause(catchClause.name(), ooStatementBlock(catchClause.body())))
                            .toList()
            );
        }
        if (statement instanceof ParserSchema.OoWhileStatementDto whileStatement) {
            return new CoreIr.OoWhileStatement(whileStatement.rawCondition(), ooStatementBlock(whileStatement.body()));
        }
        if (statement instanceof ParserSchema.OoDoWhileStatementDto doWhileStatement) {
            return new CoreIr.OoDoWhileStatement(ooStatementBlock(doWhileStatement.body()), doWhileStatement.rawCondition());
        }
        if (statement instanceof ParserSchema.OoForEachStatementDto forEachStatement) {
            return new CoreIr.OoForEachStatement(
                    forEachStatement.name(),
                    forEachStatement.type(),
                    forEachStatement.rawIterable(),
                    ooStatementBlock(forEachStatement.body())
            );
        }
        if (statement instanceof ParserSchema.OoStatementBlockDto block) {
            return ooStatementBlock(block);
        }
        throw unsupported("OO statement", statement);
    }

    private static ParserSchema.OoStatementDto ooStatementDto(CoreIr.OoStatement statement) {
        if (statement instanceof CoreIr.OoLetStatement let) {
            return new ParserSchema.OoLetStatementDto(let.name(), optional(let.type()), let.raw_expression());
        }
        if (statement instanceof CoreIr.OoLocalMethodStatement localMethod) {
            return new ParserSchema.OoLocalMethodStatementDto(
                    localMethod.name(),
                    localMethod.parameters().stream().map(CoreIrAdapters::ooParameterDto).toList(),
                    localMethod.return_type(),
                    ooMethodBodyDto(localMethod.body()),
                    localMethod.comments()
            );
        }
        if (statement instanceof CoreIr.OoMutableVariableStatement variable) {
            return new ParserSchema.OoMutableVariableStatementDto(variable.name(), optional(variable.type()), variable.raw_expression());
        }
        if (statement instanceof CoreIr.OoAssignmentStatement assignment) {
            return new ParserSchema.OoAssignmentStatementDto(assignment.name(), assignment.raw_expression());
        }
        if (statement instanceof CoreIr.OoExpressionStatement expression) {
            return new ParserSchema.OoExpressionStatementDto(expression.raw_expression());
        }
        if (statement instanceof CoreIr.OoThrowStatement throwStatement) {
            return new ParserSchema.OoThrowStatementDto(throwStatement.raw_expression());
        }
        if (statement instanceof CoreIr.OoReturnStatement returnStatement) {
            return new ParserSchema.OoReturnStatementDto(returnStatement.raw_expression());
        }
        if (statement instanceof CoreIr.OoIfStatement ifStatement) {
            return new ParserSchema.OoIfStatementDto(
                    ifStatement.raw_condition(),
                    ooStatementBlockDto(ifStatement.then_branch()),
                    optional(ifStatement.else_branch()).map(CoreIrAdapters::ooStatementDto)
            );
        }
        if (statement instanceof CoreIr.OoTryCatchStatement tryCatch) {
            return new ParserSchema.OoTryCatchStatementDto(
                    ooStatementBlockDto(tryCatch.try_block()),
                    tryCatch.catches().stream()
                            .map(catchClause -> new ParserSchema.OoCatchClauseDto(catchClause.name(), ooStatementBlockDto(catchClause.body())))
                            .toList()
            );
        }
        if (statement instanceof CoreIr.OoWhileStatement whileStatement) {
            return new ParserSchema.OoWhileStatementDto(whileStatement.raw_condition(), ooStatementBlockDto(whileStatement.body()));
        }
        if (statement instanceof CoreIr.OoDoWhileStatement doWhileStatement) {
            return new ParserSchema.OoDoWhileStatementDto(ooStatementBlockDto(doWhileStatement.body()), doWhileStatement.raw_condition());
        }
        if (statement instanceof CoreIr.OoForEachStatement forEachStatement) {
            return new ParserSchema.OoForEachStatementDto(
                    forEachStatement.name(),
                    optional(forEachStatement.type()),
                    forEachStatement.iterable(),
                    ooStatementBlockDto(forEachStatement.body())
            );
        }
        if (statement instanceof CoreIr.OoStatementBlock block) {
            return ooStatementBlockDto(block);
        }
        throw unsupported("OO statement", statement);
    }

    private static ParserSchema.OoStatementDto ooStatementDto(Object statement) {
        return ooStatementDto((CoreIr.OoStatement) statement);
    }

    private static CoreIr.ParsedType parsedType(ParserSchema.TypeDto type) {
        if (type instanceof ParserSchema.PrimitiveTypeDto primitive) {
            return new CoreIr.ParsedPrimitiveType(primitiveType(primitive.name()));
        }
        if (type instanceof ParserSchema.DataTypeDto dataType) {
            return new CoreIr.ParsedDataType(dataType.name());
        }
        if (type instanceof ParserSchema.ListTypeDto listType) {
            return new CoreIr.ParsedListType(parsedType(listType.elementType()));
        }
        if (type instanceof ParserSchema.SetTypeDto setType) {
            return new CoreIr.ParsedSetType(parsedType(setType.elementType()));
        }
        if (type instanceof ParserSchema.DictTypeDto dictType) {
            return new CoreIr.ParsedDictType(parsedType(dictType.valueType()));
        }
        if (type instanceof ParserSchema.FunctionTypeDto functionType) {
            return new CoreIr.ParsedFunctionType(parsedType(functionType.argumentType()), parsedType(functionType.returnType()));
        }
        if (type instanceof ParserSchema.TupleTypeDto tupleType) {
            return new CoreIr.ParsedTupleType(tupleType.elementTypes().stream().map(CoreIrAdapters::parsedType).toList());
        }
        throw unsupported("parsed type", type);
    }

    private static ParserSchema.TypeDto parsedTypeDto(CoreIr.ParsedType type) {
        if (type instanceof CoreIr.ParsedPrimitiveType primitive) {
            return new ParserSchema.PrimitiveTypeDto(primitiveName(primitive.value()));
        }
        if (type instanceof CoreIr.ParsedDataType dataType) {
            return new ParserSchema.DataTypeDto(dataType.name());
        }
        if (type instanceof CoreIr.ParsedListType listType) {
            return new ParserSchema.ListTypeDto(parsedTypeDto(listType.element_type()));
        }
        if (type instanceof CoreIr.ParsedSetType setType) {
            return new ParserSchema.SetTypeDto(parsedTypeDto(setType.element_type()));
        }
        if (type instanceof CoreIr.ParsedDictType dictType) {
            return new ParserSchema.DictTypeDto(parsedTypeDto(dictType.value_type()));
        }
        if (type instanceof CoreIr.ParsedFunctionType functionType) {
            return new ParserSchema.FunctionTypeDto(parsedTypeDto(functionType.argument_type()), parsedTypeDto(functionType.return_type()));
        }
        if (type instanceof CoreIr.ParsedTupleType tupleType) {
            return new ParserSchema.TupleTypeDto(tupleType.element_types().stream().map(CoreIrAdapters::parsedTypeDto).toList());
        }
        throw unsupported("parsed type", type);
    }

    private static ParserSchema.TypeDto parsedTypeDto(Object type) {
        return parsedTypeDto((CoreIr.ParsedType) type);
    }

    private static CoreIr.ParsedExpression parsedExpression(ParserSchema.ExpressionDto expression) {
        if (expression instanceof ParserSchema.BoolExpressionDto value) {
            return new CoreIr.ParsedBoolExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.ByteExpressionDto value) {
            return new CoreIr.ParsedByteExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.IntExpressionDto value) {
            return new CoreIr.ParsedIntExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.LongExpressionDto value) {
            return new CoreIr.ParsedLongExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.FloatExpressionDto value) {
            return new CoreIr.ParsedFloatExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.DoubleExpressionDto value) {
            return new CoreIr.ParsedDoubleExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.StringExpressionDto value) {
            return new CoreIr.ParsedStringExpression(value.value(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.NothingExpressionDto value) {
            return new CoreIr.ParsedNothingExpression(value.literal(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.ValueExpressionDto value) {
            return new CoreIr.ParsedValueExpression(value.name(), mapOptional(value.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.FunctionCallExpressionDto call) {
            return new CoreIr.ParsedFunctionCallExpression(
                    call.moduleName(),
                    call.name(),
                    call.arguments().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(call.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.FunctionInvokeExpressionDto invoke) {
            return new CoreIr.ParsedFunctionInvokeExpression(
                    parsedExpression(invoke.function()),
                    invoke.arguments().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(invoke.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.FunctionReferenceExpressionDto reference) {
            return new CoreIr.ParsedFunctionReferenceExpression(reference.name(), mapOptional(reference.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.IfExpressionDto ifExpression) {
            return new CoreIr.ParsedIfExpression(
                    parsedExpression(ifExpression.condition()),
                    parsedExpression(ifExpression.thenBranch()),
                    parsedExpression(ifExpression.elseBranch()),
                    mapOptional(ifExpression.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.InfixExpressionDto infix) {
            return new CoreIr.ParsedInfixExpression(
                    parsedExpression(infix.left()),
                    infixOperator(infix.operator()),
                    parsedExpression(infix.right()),
                    mapOptional(infix.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.FieldAccessExpressionDto access) {
            return new CoreIr.ParsedFieldAccessExpression(
                    parsedExpression(access.source()),
                    access.field(),
                    mapOptional(access.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.LambdaExpressionDto lambda) {
            return new CoreIr.ParsedLambdaExpression(
                    lambda.argumentNames(),
                    parsedExpression(lambda.expression()),
                    mapOptional(lambda.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.LetExpressionDto let) {
            return new CoreIr.ParsedLetExpression(
                    let.name(),
                    mapOptional(let.declaredType(), CoreIrAdapters::parsedType),
                    CoreIr.LetKind.valueOf(let.kind()),
                    parsedExpression(let.value()),
                    parsedExpression(let.rest()),
                    mapOptional(let.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.ReduceExpressionDto reduce) {
            return new CoreIr.ParsedReduceExpression(
                    parsedExpression(reduce.initialValue()),
                    reduce.accumulatorName(),
                    reduce.keyName(),
                    reduce.valueName(),
                    parsedExpression(reduce.reducerExpression()),
                    mapOptional(reduce.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.IndexExpressionDto index) {
            return new CoreIr.ParsedIndexExpression(
                    parsedExpression(index.source()),
                    index.arguments().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(index.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.SliceExpressionDto slice) {
            return new CoreIr.ParsedSliceExpression(
                    parsedExpression(slice.source()),
                    mapOptional(slice.start(), CoreIrAdapters::parsedExpression),
                    mapOptional(slice.end(), CoreIrAdapters::parsedExpression),
                    mapOptional(slice.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.MatchExpressionDto match) {
            return new CoreIr.ParsedMatchExpression(
                    parsedExpression(match.matchWith()),
                    match.cases().stream().map(CoreIrAdapters::matchCase).toList(),
                    mapOptional(match.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.NewDataExpressionDto newData) {
            return new CoreIr.ParsedNewDataExpression(
                    parsedType(newData.type()),
                    newData.bypassConstructor(),
                    newData.assignments().stream().map(CoreIrAdapters::fieldAssignment).toList(),
                    newData.positionalArguments().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    newData.spreads().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(newData.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.ConstructorDataExpressionDto constructorData) {
            return new CoreIr.ParsedConstructorDataExpression(
                    constructorData.assignments().stream().map(CoreIrAdapters::fieldAssignment).toList(),
                    constructorData.positionalArguments().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    constructorData.spreads().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(constructorData.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.WithExpressionDto with) {
            return new CoreIr.ParsedWithExpression(
                    parsedExpression(with.source()),
                    with.assignments().stream().map(CoreIrAdapters::fieldAssignment).toList(),
                    mapOptional(with.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.NewListExpressionDto list) {
            return new CoreIr.ParsedNewListExpression(
                    list.values().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(list.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.NewSetExpressionDto set) {
            return new CoreIr.ParsedNewSetExpression(
                    set.values().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(set.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.NewDictExpressionDto dict) {
            return new CoreIr.ParsedNewDictExpression(
                    dict.entries().stream()
                            .map(entry -> new CoreIr.ParsedDictEntry(parsedExpression(entry.key()), parsedExpression(entry.value())))
                            .toList(),
                    mapOptional(dict.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.TupleExpressionDto tuple) {
            return new CoreIr.ParsedTupleExpression(
                    tuple.values().stream().map(CoreIrAdapters::parsedExpression).toList(),
                    mapOptional(tuple.position(), CoreIrAdapters::sourcePosition)
            );
        }
        if (expression instanceof ParserSchema.UnwrapExpressionDto unwrap) {
            return new CoreIr.ParsedUnwrapExpression(parsedExpression(unwrap.expression()), mapOptional(unwrap.position(), CoreIrAdapters::sourcePosition));
        }
        if (expression instanceof ParserSchema.PlaceholderExpressionDto placeholder) {
            return new CoreIr.ParsedPlaceholderExpression(mapOptional(placeholder.position(), CoreIrAdapters::sourcePosition));
        }
        throw unsupported("parsed expression", expression);
    }

    private static ParserSchema.ExpressionDto parsedExpressionDto(CoreIr.ParsedExpression expression) {
        if (expression instanceof CoreIr.ParsedBoolExpression value) {
            return new ParserSchema.BoolExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedByteExpression value) {
            return new ParserSchema.ByteExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedIntExpression value) {
            return new ParserSchema.IntExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedLongExpression value) {
            return new ParserSchema.LongExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedFloatExpression value) {
            return new ParserSchema.FloatExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedDoubleExpression value) {
            return new ParserSchema.DoubleExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedStringExpression value) {
            return new ParserSchema.StringExpressionDto(value.value(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedNothingExpression value) {
            return new ParserSchema.NothingExpressionDto(value.literal(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedValueExpression value) {
            return new ParserSchema.ValueExpressionDto(value.name(), optional(value.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedFunctionCallExpression call) {
            return new ParserSchema.FunctionCallExpressionDto(
                    optional(call.module_name()),
                    call.name(),
                    call.arguments().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(call.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedFunctionInvokeExpression invoke) {
            return new ParserSchema.FunctionInvokeExpressionDto(
                    parsedExpressionDto(invoke.function()),
                    invoke.arguments().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(invoke.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedFunctionReferenceExpression reference) {
            return new ParserSchema.FunctionReferenceExpressionDto(reference.name(), optional(reference.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedIfExpression ifExpression) {
            return new ParserSchema.IfExpressionDto(
                    parsedExpressionDto(ifExpression.condition()),
                    parsedExpressionDto(ifExpression.then_branch()),
                    parsedExpressionDto(ifExpression.else_branch()),
                    optional(ifExpression.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedInfixExpression infix) {
            return new ParserSchema.InfixExpressionDto(
                    parsedExpressionDto(infix.left()),
                    infixSymbol(infix.operator()),
                    parsedExpressionDto(infix.right()),
                    optional(infix.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedFieldAccessExpression access) {
            return new ParserSchema.FieldAccessExpressionDto(
                    parsedExpressionDto(access.source()),
                    access.field(),
                    optional(access.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedLambdaExpression lambda) {
            return new ParserSchema.LambdaExpressionDto(
                    lambda.argument_names(),
                    parsedExpressionDto(lambda.expression()),
                    optional(lambda.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedLetExpression let) {
            return new ParserSchema.LetExpressionDto(
                    let.name(),
                    optional(let.declared_type()).map(CoreIrAdapters::parsedTypeDto),
                    let.kind().name(),
                    parsedExpressionDto(let.value()),
                    parsedExpressionDto(let.rest()),
                    optional(let.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedReduceExpression reduce) {
            return new ParserSchema.ReduceExpressionDto(
                    parsedExpressionDto(reduce.initial_value()),
                    reduce.accumulator_name(),
                    optional(reduce.key_name()),
                    reduce.value_name(),
                    parsedExpressionDto(reduce.reducer_expression()),
                    optional(reduce.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedIndexExpression index) {
            return new ParserSchema.IndexExpressionDto(
                    parsedExpressionDto(index.source()),
                    index.arguments().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(index.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedSliceExpression slice) {
            return new ParserSchema.SliceExpressionDto(
                    parsedExpressionDto(slice.source()),
                    optional(slice.start()).map(CoreIrAdapters::parsedExpressionDto),
                    optional(slice.end()).map(CoreIrAdapters::parsedExpressionDto),
                    optional(slice.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedMatchExpression match) {
            return new ParserSchema.MatchExpressionDto(
                    parsedExpressionDto(match.match_with()),
                    match.cases().stream().map(CoreIrAdapters::matchCaseDto).toList(),
                    optional(match.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedNewDataExpression newData) {
            return new ParserSchema.NewDataExpressionDto(
                    parsedTypeDto(newData.type()),
                    newData.bypass_constructor(),
                    newData.assignments().stream().map(CoreIrAdapters::fieldAssignmentDto).toList(),
                    newData.positional_arguments().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    newData.spreads().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(newData.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedConstructorDataExpression constructorData) {
            return new ParserSchema.ConstructorDataExpressionDto(
                    constructorData.assignments().stream().map(CoreIrAdapters::fieldAssignmentDto).toList(),
                    constructorData.positional_arguments().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    constructorData.spreads().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(constructorData.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedWithExpression with) {
            return new ParserSchema.WithExpressionDto(
                    parsedExpressionDto(with.source()),
                    with.assignments().stream().map(CoreIrAdapters::fieldAssignmentDto).toList(),
                    optional(with.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedNewListExpression list) {
            return new ParserSchema.NewListExpressionDto(
                    list.values().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(list.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedNewSetExpression set) {
            return new ParserSchema.NewSetExpressionDto(
                    set.values().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(set.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedNewDictExpression dict) {
            return new ParserSchema.NewDictExpressionDto(
                    dict.entries().stream()
                            .map(entry -> new ParserSchema.DictEntryDto(parsedExpressionDto(entry.key()), parsedExpressionDto(entry.value())))
                            .toList(),
                    optional(dict.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedTupleExpression tuple) {
            return new ParserSchema.TupleExpressionDto(
                    tuple.values().stream().map(CoreIrAdapters::parsedExpressionDto).toList(),
                    optional(tuple.position()).map(CoreIrAdapters::sourcePositionDto)
            );
        }
        if (expression instanceof CoreIr.ParsedUnwrapExpression unwrap) {
            return new ParserSchema.UnwrapExpressionDto(parsedExpressionDto(unwrap.expression()), optional(unwrap.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (expression instanceof CoreIr.ParsedPlaceholderExpression placeholder) {
            return new ParserSchema.PlaceholderExpressionDto(optional(placeholder.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        throw unsupported("parsed expression", expression);
    }

    private static ParserSchema.ExpressionDto parsedExpressionDto(Object expression) {
        return parsedExpressionDto((CoreIr.ParsedExpression) expression);
    }

    private static CoreIr.CompiledModule compiledModule(CompiledModule module) {
        return new CoreIr.CompiledModule(
                new CoreIr.ModuleId(module.name(), module.path()),
                module.types().entrySet().stream()
                        .map(entry -> new CoreIr.CompiledTypeEntry(entry.getKey(), genericCompiledType(entry.getValue())))
                        .toList(),
                module.visiblePrimitiveBackedTypes().values().stream().map(CoreIrAdapters::compiledPrimitiveBackedType).toList(),
                module.functions().stream().map(CoreIrAdapters::compiledFunction).toList(),
                module.staticImports().stream()
                        .map(staticImport -> new CoreIr.StaticImport(staticImport.className(), staticImport.memberName(), staticImport.enumValue()))
                        .toList()
        );
    }

    private static CoreIrSchema.CompiledModuleDto compiledModuleDto(CoreIr.CompiledModule module) {
        return new CoreIrSchema.CompiledModuleDto(
                new CoreIrSchema.ModuleIdDto(module.module().name(), module.module().path()),
                module.types().stream().map(entry -> new CoreIrSchema.CompiledTypeEntryDto(entry.name(), genericCompiledTypeDto(entry.type()))).toList(),
                module.visible_primitive_backed_types().stream().map(CoreIrAdapters::compiledPrimitiveBackedTypeDto).toList(),
                module.functions().stream().map(CoreIrAdapters::compiledFunctionDto).toList(),
                module.static_imports().stream()
                        .map(staticImport -> new CoreIrSchema.StaticImportDto(staticImport.class_name(), staticImport.member_name(), staticImport.enum_value()))
                        .toList()
        );
    }

    private static CompiledModule compiledModule(CoreIrSchema.CompiledModuleDto module) {
        SortedMap<String, GenericDataType> types = module.types().stream().collect(Collectors.toMap(
                CoreIrSchema.CompiledTypeEntryDto::name,
                entry -> genericCompiledType(entry.type()),
                (left, right) -> right,
                TreeMap::new
        ));
        var visiblePrimitiveBackedTypes = module.visiblePrimitiveBackedTypes().stream().collect(Collectors.toMap(
                CoreIrSchema.CompiledPrimitiveBackedTypeDto::name,
                CoreIrAdapters::compiledPrimitiveBackedType,
                (left, right) -> right,
                TreeMap::new
        ));
        return new CompiledModule(
                module.module().name(),
                module.module().path(),
                types,
                module.functions().stream().map(CoreIrAdapters::compiledFunction).toList(),
                Map.of(),
                visiblePrimitiveBackedTypes,
                module.staticImports().stream()
                        .map(staticImport -> new CompiledModule.StaticImport(staticImport.className(), staticImport.memberName(), staticImport.enumValue()))
                        .toList()
        );
    }

    private static CoreIr.CompiledFunction compiledFunction(CompiledFunction function) {
        return new CoreIr.CompiledFunction(
                function.name(),
                compiledType(function.returnType()),
                function.parameters().stream()
                        .map(parameter -> new CoreIr.CompiledFunctionParameter(parameter.name(), compiledType(parameter.type())))
                        .toList(),
                compiledExpression(function.expression()),
                function.comments(),
                visibility(function.visibility()),
                function.programMain(),
                function.recursive(),
                function.tailRecursive(),
                function.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
        );
    }

    private static CoreIrSchema.CompiledFunctionDto compiledFunctionDto(CoreIr.CompiledFunction function) {
        return new CoreIrSchema.CompiledFunctionDto(
                function.name(),
                compiledTypeDto(function.return_type()),
                function.parameters().stream()
                        .map(parameter -> new CoreIrSchema.CompiledFunctionParameterDto(parameter.name(), compiledTypeDto(parameter.type())))
                        .toList(),
                compiledExpressionDto(function.expression()),
                function.comments(),
                optional(function.visibility()).map(CoreIrAdapters::visibilityDto),
                function.program_main(),
                function.recursive(),
                function.tail_recursive(),
                function.annotations().stream().map(CoreIrAdapters::compiledAnnotationDto).toList()
        );
    }

    private static CompiledFunction compiledFunction(CoreIrSchema.CompiledFunctionDto function) {
        return new CompiledFunction(
                function.name(),
                compiledType(function.returnType()),
                function.parameters().stream()
                        .map(parameter -> new CompiledFunction.CompiledFunctionParameter(parameter.name(), compiledType(parameter.type())))
                        .toList(),
                compiledExpression(function.expression()),
                function.comments(),
                function.visibility().map(CoreIrAdapters::visibility).orElse(null),
                function.programMain(),
                function.recursive(),
                function.tailRecursive(),
                function.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
        );
    }

    private static CoreIr.CompiledType compiledType(CompiledType type) {
        if (type instanceof PrimitiveLinkedType primitive) {
            return new CoreIr.PrimitiveCompiledType(CoreIr.PrimitiveType.valueOf(primitive.name()));
        }
        if (type instanceof CollectionLinkedType.CompiledList list) {
            return new CoreIr.CompiledListType(compiledType(list.elementType()));
        }
        if (type instanceof CollectionLinkedType.CompiledSet set) {
            return new CoreIr.CompiledSetType(compiledType(set.elementType()));
        }
        if (type instanceof CollectionLinkedType.CompiledDict dict) {
            return new CoreIr.CompiledDictType(compiledType(dict.valueType()));
        }
        if (type instanceof CompiledGenericTypeParameter genericTypeParameter) {
            return new CoreIr.CompiledGenericTypeParameter(genericTypeParameter.name());
        }
        if (type instanceof CompiledFunctionType functionType) {
            return new CoreIr.CompiledFunctionType(compiledType(functionType.argumentType()), compiledType(functionType.returnType()));
        }
        if (type instanceof CompiledTupleType tupleType) {
            return new CoreIr.CompiledTupleType(tupleType.elementTypes().stream().map(CoreIrAdapters::compiledType).toList());
        }
        if (type instanceof GenericDataType genericType) {
            return (CoreIr.CompiledType) genericCompiledType(genericType);
        }
        throw unsupported("compiled type", type);
    }

    private static CoreIr.GenericCompiledType genericCompiledType(GenericDataType type) {
        if (type instanceof CompiledDataType dataType) {
            return new CoreIr.CompiledDataType(
                    dataType.name(),
                    dataType.fields().stream().map(CoreIrAdapters::compiledField).toList(),
                    dataType.typeParameters(),
                    dataType.extendedTypes(),
                    dataType.comments(),
                    visibility(dataType.visibility()),
                    dataType.singleton(),
                    dataType.nativeType(),
                    dataType.enumValue(),
                    dataType.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
            );
        }
        if (type instanceof CompiledDataParentType parentType) {
            return new CoreIr.CompiledDataParentType(
                    parentType.name(),
                    parentType.fields().stream().map(CoreIrAdapters::compiledField).toList(),
                    parentType.subTypes().stream().map(dataType -> (CoreIr.CompiledDataType) genericCompiledType(dataType)).toList(),
                    parentType.typeParameters(),
                    parentType.comments(),
                    visibility(parentType.visibility()),
                    parentType.enumType(),
                    parentType.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
            );
        }
        if (type instanceof CompiledObjectType objectType) {
            return new CoreIr.CompiledObjectType(
                    objectType.name(),
                    objectType.backendClassName(),
                    objectType.parents(),
                    visibility(objectType.visibility()),
                    objectType.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList(),
                    CoreIr.CompiledObjectKind.valueOf(objectType.kind().name()),
                    objectType.methods().stream().map(CoreIrAdapters::compiledObjectMethod).toList()
            );
        }
        if (type instanceof CompiledPrimitiveBackedType primitiveBackedType) {
            return compiledPrimitiveBackedType(primitiveBackedType);
        }
        throw unsupported("generic compiled type", type);
    }

    private static CoreIr.CompiledPrimitiveBackedType compiledPrimitiveBackedType(CompiledPrimitiveBackedType type) {
        return new CoreIr.CompiledPrimitiveBackedType(
                type.name(),
                CoreIr.PrimitiveType.valueOf(type.backingType().name()),
                type.cfunType(),
                type.comments(),
                visibility(type.visibility()),
                type.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
        );
    }

    private static CoreIrSchema.CompiledTypeDto compiledTypeDto(CoreIr.CompiledType type) {
        if (type instanceof CoreIr.PrimitiveCompiledType primitive) {
            return new CoreIrSchema.PrimitiveCompiledTypeDto(CoreIrSchema.PrimitiveTypeDto.valueOf(primitive.value().name()));
        }
        if (type instanceof CoreIr.CompiledListType listType) {
            return new CoreIrSchema.CompiledListTypeDto(compiledTypeDto(listType.element_type()));
        }
        if (type instanceof CoreIr.CompiledSetType setType) {
            return new CoreIrSchema.CompiledSetTypeDto(compiledTypeDto(setType.element_type()));
        }
        if (type instanceof CoreIr.CompiledDictType dictType) {
            return new CoreIrSchema.CompiledDictTypeDto(compiledTypeDto(dictType.value_type()));
        }
        if (type instanceof CoreIr.CompiledGenericTypeParameter genericTypeParameter) {
            return new CoreIrSchema.CompiledGenericTypeParameterDto(genericTypeParameter.name());
        }
        if (type instanceof CoreIr.CompiledFunctionType functionType) {
            return new CoreIrSchema.CompiledFunctionTypeDto(compiledTypeDto(functionType.argument_type()), compiledTypeDto(functionType.return_type()));
        }
        if (type instanceof CoreIr.CompiledTupleType tupleType) {
            return new CoreIrSchema.CompiledTupleTypeDto(tupleType.element_types().stream().map(CoreIrAdapters::compiledTypeDto).toList());
        }
        if (type instanceof CoreIr.GenericCompiledType genericType) {
            return genericCompiledTypeDto(genericType);
        }
        throw unsupported("compiled type", type);
    }

    private static CoreIrSchema.GenericCompiledTypeDto genericCompiledTypeDto(CoreIr.GenericCompiledType type) {
        if (type instanceof CoreIr.CompiledDataType dataType) {
            return new CoreIrSchema.CompiledDataTypeDto(
                    dataType.name(),
                    dataType.fields().stream().map(CoreIrAdapters::compiledFieldDto).toList(),
                    dataType.type_parameters(),
                    dataType.extended_types(),
                    dataType.comments(),
                    optional(dataType.visibility()).map(CoreIrAdapters::visibilityDto),
                    dataType.singleton(),
                    dataType.native_type(),
                    dataType.enum_value(),
                    dataType.annotations().stream().map(CoreIrAdapters::compiledAnnotationDto).toList()
            );
        }
        if (type instanceof CoreIr.CompiledDataParentType parentType) {
            return new CoreIrSchema.CompiledDataParentTypeDto(
                    parentType.name(),
                    parentType.fields().stream().map(CoreIrAdapters::compiledFieldDto).toList(),
                    parentType.sub_types().stream().map(dataType -> (CoreIrSchema.CompiledDataTypeDto) genericCompiledTypeDto(dataType)).toList(),
                    parentType.type_parameters(),
                    parentType.comments(),
                    optional(parentType.visibility()).map(CoreIrAdapters::visibilityDto),
                    parentType.enum_type(),
                    parentType.annotations().stream().map(CoreIrAdapters::compiledAnnotationDto).toList()
            );
        }
        if (type instanceof CoreIr.CompiledObjectType objectType) {
            return new CoreIrSchema.CompiledObjectTypeDto(
                    objectType.name(),
                    objectType.backend_class_name(),
                    objectType.parents(),
                    optional(objectType.visibility()).map(CoreIrAdapters::visibilityDto),
                    objectType.annotations().stream().map(CoreIrAdapters::compiledAnnotationDto).toList(),
                    CoreIrSchema.CompiledObjectKindDto.valueOf(objectType.kind().name()),
                    objectType.methods().stream().map(CoreIrAdapters::compiledObjectMethodDto).toList()
            );
        }
        if (type instanceof CoreIr.CompiledPrimitiveBackedType primitiveBackedType) {
            return compiledPrimitiveBackedTypeDto(primitiveBackedType);
        }
        throw unsupported("generic compiled type", type);
    }

    private static CompiledType compiledType(CoreIrSchema.CompiledTypeDto type) {
        if (type instanceof CoreIrSchema.PrimitiveCompiledTypeDto primitive) {
            return PrimitiveLinkedType.valueOf(primitive.value().name());
        }
        if (type instanceof CoreIrSchema.CompiledListTypeDto listType) {
            return new CollectionLinkedType.CompiledList(compiledType(listType.elementType()));
        }
        if (type instanceof CoreIrSchema.CompiledSetTypeDto setType) {
            return new CollectionLinkedType.CompiledSet(compiledType(setType.elementType()));
        }
        if (type instanceof CoreIrSchema.CompiledDictTypeDto dictType) {
            return new CollectionLinkedType.CompiledDict(compiledType(dictType.valueType()));
        }
        if (type instanceof CoreIrSchema.CompiledGenericTypeParameterDto genericTypeParameter) {
            return new CompiledGenericTypeParameter(genericTypeParameter.name());
        }
        if (type instanceof CoreIrSchema.CompiledFunctionTypeDto functionType) {
            return new CompiledFunctionType(compiledType(functionType.argumentType()), compiledType(functionType.returnType()));
        }
        if (type instanceof CoreIrSchema.CompiledTupleTypeDto tupleType) {
            return new CompiledTupleType(tupleType.elementTypes().stream().map(CoreIrAdapters::compiledType).toList());
        }
        if (type instanceof CoreIrSchema.GenericCompiledTypeDto genericType) {
            return genericCompiledType(genericType);
        }
        throw unsupported("compiled type DTO", type);
    }

    private static GenericDataType genericCompiledType(CoreIrSchema.GenericCompiledTypeDto type) {
        if (type instanceof CoreIrSchema.CompiledDataTypeDto dataType) {
            return new CompiledDataType(
                    dataType.name(),
                    dataType.fields().stream().map(CoreIrAdapters::compiledField).toList(),
                    dataType.typeParameters(),
                    dataType.extendedTypes(),
                    dataType.comments(),
                    dataType.visibility().map(CoreIrAdapters::visibility).orElse(null),
                    dataType.singleton(),
                    dataType.nativeType(),
                    dataType.enumValue(),
                    dataType.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
            );
        }
        if (type instanceof CoreIrSchema.CompiledDataParentTypeDto parentType) {
            return new CompiledDataParentType(
                    parentType.name(),
                    parentType.fields().stream().map(CoreIrAdapters::compiledField).toList(),
                    parentType.subTypes().stream().map(dataType -> (CompiledDataType) genericCompiledType(dataType)).toList(),
                    parentType.typeParameters(),
                    parentType.comments(),
                    parentType.visibility().map(CoreIrAdapters::visibility).orElse(null),
                    parentType.enumType(),
                    parentType.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
            );
        }
        if (type instanceof CoreIrSchema.CompiledObjectTypeDto objectType) {
            return new CompiledObjectType(
                    objectType.name(),
                    objectType.backendClassName(),
                    objectType.parents(),
                    objectType.visibility().map(CoreIrAdapters::visibility).orElse(null),
                    objectType.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList(),
                    CompiledObjectKind.valueOf(objectType.kind().name()),
                    objectType.methods().stream().map(CoreIrAdapters::compiledObjectMethod).toList()
            );
        }
        if (type instanceof CoreIrSchema.CompiledPrimitiveBackedTypeDto primitiveBackedType) {
            return compiledPrimitiveBackedType(primitiveBackedType);
        }
        throw unsupported("generic compiled type DTO", type);
    }

    private static CoreIr.CompiledExpression compiledExpression(CompiledExpression expression) {
        if (expression instanceof CompiledBooleanValue booleanValue) {
            return new CoreIr.CompiledBoolExpression(booleanValue == CompiledBooleanValue.TRUE);
        }
        if (expression instanceof CompiledIntValue intValue) {
            return new CoreIr.CompiledIntExpression(intValue.intValue());
        }
        if (expression instanceof CompiledStringValue stringValue) {
            return new CoreIr.CompiledStringExpression(stringValue.stringValue());
        }
        if (expression instanceof CompiledVariable variable) {
            return new CoreIr.CompiledVariableExpression(variable.name(), compiledType(variable.type()), variable.emptyShapeCompatible());
        }
        if (expression instanceof CompiledFunctionCall functionCall) {
            return new CoreIr.CompiledFunctionCallExpression(
                    functionCall.name(),
                    functionCall.arguments().stream().map(CoreIrAdapters::compiledExpression).toList(),
                    compiledType(functionCall.returnType())
            );
        }
        if (expression instanceof CompiledInfixExpression infix) {
            return new CoreIr.CompiledInfixExpression(
                    compiledExpression(infix.left()),
                    infixOperator(infix.operator().symbol()),
                    compiledExpression(infix.right()),
                    compiledType(infix.type())
            );
        }
        if (expression instanceof CompiledNewData newData) {
            return new CoreIr.CompiledNewDataExpression(
                    compiledType(newData.type()),
                    newData.assignments().stream()
                            .map(assignment -> new CoreIr.CompiledNewDataFieldAssignment(assignment.name(), compiledExpression(assignment.value())))
                            .toList()
            );
        }
        if (expression instanceof CompiledNothingValue nothingValue) {
            return new CoreIr.CompiledNothingExpression(mapOptional(nothingValue.position(), CoreIrAdapters::sourcePosition), nothingValue.message());
        }
        throw unsupported("compiled expression", expression);
    }

    private static CoreIrSchema.CompiledExpressionDto compiledExpressionDto(CoreIr.CompiledExpression expression) {
        if (expression instanceof CoreIr.CompiledBoolExpression boolExpression) {
            return new CoreIrSchema.CompiledBoolExpressionDto(boolExpression.value());
        }
        if (expression instanceof CoreIr.CompiledIntExpression intExpression) {
            return new CoreIrSchema.CompiledIntExpressionDto(intExpression.value());
        }
        if (expression instanceof CoreIr.CompiledStringExpression stringExpression) {
            return new CoreIrSchema.CompiledStringExpressionDto(stringExpression.value());
        }
        if (expression instanceof CoreIr.CompiledVariableExpression variable) {
            return new CoreIrSchema.CompiledVariableExpressionDto(
                    variable.name(),
                    compiledTypeDto(variable.type()),
                    variable.empty_shape_compatible()
            );
        }
        if (expression instanceof CoreIr.CompiledFunctionCallExpression functionCall) {
            return new CoreIrSchema.CompiledFunctionCallExpressionDto(
                    functionCall.name(),
                    functionCall.arguments().stream().map(CoreIrAdapters::compiledExpressionDto).toList(),
                    compiledTypeDto(functionCall.return_type())
            );
        }
        if (expression instanceof CoreIr.CompiledInfixExpression infix) {
            return new CoreIrSchema.CompiledInfixExpressionDto(
                    compiledExpressionDto(infix.left()),
                    infixSymbol(infix.operator()),
                    compiledExpressionDto(infix.right()),
                    compiledTypeDto(infix.type())
            );
        }
        if (expression instanceof CoreIr.CompiledNewDataExpression newData) {
            return new CoreIrSchema.CompiledNewDataExpressionDto(
                    compiledTypeDto(newData.type()),
                    newData.assignments().stream()
                            .map(assignment -> new CoreIrSchema.CompiledNewDataFieldAssignmentDto(
                                    assignment.name(),
                                    compiledExpressionDto(assignment.value())))
                            .toList()
            );
        }
        if (expression instanceof CoreIr.CompiledNothingExpression nothingExpression) {
            return new CoreIrSchema.CompiledNothingExpressionDto(
                    optional(nothingExpression.position()).map(CoreIrAdapters::coreSourcePositionDto),
                    nothingExpression.message()
            );
        }
        throw unsupported("compiled expression", expression);
    }

    private static CompiledExpression compiledExpression(CoreIrSchema.CompiledExpressionDto expression) {
        if (expression instanceof CoreIrSchema.CompiledBoolExpressionDto boolExpression) {
            return boolExpression.value() ? CompiledBooleanValue.TRUE : CompiledBooleanValue.FALSE;
        }
        if (expression instanceof CoreIrSchema.CompiledIntExpressionDto intExpression) {
            return new CompiledIntValue(intExpression.value());
        }
        if (expression instanceof CoreIrSchema.CompiledStringExpressionDto stringExpression) {
            return new CompiledStringValue(stringExpression.value());
        }
        if (expression instanceof CoreIrSchema.CompiledVariableExpressionDto variable) {
            return new CompiledVariable(variable.name(), compiledType(variable.type()), variable.emptyShapeCompatible());
        }
        if (expression instanceof CoreIrSchema.CompiledFunctionCallExpressionDto functionCall) {
            return new CompiledFunctionCall(
                    functionCall.name(),
                    functionCall.arguments().stream().map(CoreIrAdapters::compiledExpression).toList(),
                    compiledType(functionCall.returnType())
            );
        }
        if (expression instanceof CoreIrSchema.CompiledInfixExpressionDto infix) {
            return new CompiledInfixExpression(
                    compiledExpression(infix.left()),
                    dev.capylang.compiler.parser.InfixOperator.fromSymbol(infix.operator()),
                    compiledExpression(infix.right()),
                    compiledType(infix.type())
            );
        }
        if (expression instanceof CoreIrSchema.CompiledNewDataExpressionDto newData) {
            return new CompiledNewData(
                    compiledType(newData.type()),
                    newData.assignments().stream()
                            .map(assignment -> new CompiledNewData.FieldAssignment(assignment.name(), compiledExpression(assignment.value())))
                            .toList()
            );
        }
        if (expression instanceof CoreIrSchema.CompiledNothingExpressionDto nothingExpression) {
            return new CompiledNothingValue(nothingExpression.position().map(CoreIrAdapters::sourcePosition), nothingExpression.message());
        }
        throw unsupported("compiled expression DTO", expression);
    }

    private static CoreIr.NativeProviderManifest nativeProviderManifest(NativeProviderManifest manifest) {
        return new CoreIr.NativeProviderManifest(
                manifest.providers().stream().map(CoreIrAdapters::nativeProviderBinding).toList(),
                Optional.ofNullable(manifest.sourceFile())
        );
    }

    private static CoreIrSchema.NativeProviderManifestDto nativeProviderManifestDto(CoreIr.NativeProviderManifest manifest) {
        return new CoreIrSchema.NativeProviderManifestDto(
                manifest.providers().stream().map(CoreIrAdapters::nativeProviderBindingDto).toList(),
                optional(manifest.source_file())
        );
    }

    private static NativeProviderManifest nativeProviderManifest(CoreIrSchema.NativeProviderManifestDto manifest) {
        return new NativeProviderManifest(
                manifest.providers().stream().map(CoreIrAdapters::nativeProviderBinding).toList(),
                manifest.sourceFile().orElse(null)
        );
    }

    private static CoreIr.NativeProviderCatalog nativeProviderCatalog(NativeProviderCatalog catalog) {
        return new CoreIr.NativeProviderCatalog(
                catalog.declarations().stream().map(CoreIrAdapters::compiledNativeProviderDeclaration).toList(),
                catalog.bindings().stream().map(CoreIrAdapters::compiledNativeProviderBinding).toList()
        );
    }

    private static CoreIrSchema.NativeProviderCatalogDto nativeProviderCatalogDto(CoreIr.NativeProviderCatalog catalog) {
        return new CoreIrSchema.NativeProviderCatalogDto(
                catalog.declarations().stream().map(CoreIrAdapters::compiledNativeProviderDeclarationDto).toList(),
                catalog.bindings().stream().map(CoreIrAdapters::compiledNativeProviderBindingDto).toList()
        );
    }

    private static NativeProviderCatalog nativeProviderCatalog(CoreIrSchema.NativeProviderCatalogDto catalog) {
        return new NativeProviderCatalog(
                catalog.declarations().stream().map(CoreIrAdapters::compiledNativeProviderDeclaration).toList(),
                catalog.bindings().stream().map(CoreIrAdapters::compiledNativeProviderBinding).toList()
        );
    }

    private static CoreIr.NativeProviderBinding nativeProviderBinding(NativeProviderBinding binding) {
        return new CoreIr.NativeProviderBinding(
                binding.interfaceId(),
                binding.qualifier(),
                backendBindings(binding.javaBinding(), binding.javascriptBinding(), binding.pythonBinding())
        );
    }

    private static CoreIrSchema.NativeProviderBindingDto nativeProviderBindingDto(CoreIr.NativeProviderBinding binding) {
        return new CoreIrSchema.NativeProviderBindingDto(
                binding.interface_id(),
                binding.qualifier(),
                binding.bindings().stream().map(CoreIrAdapters::backendBindingDto).toList()
        );
    }

    private static NativeProviderBinding nativeProviderBinding(CoreIrSchema.NativeProviderBindingDto binding) {
        var byBackend = backendBindings(binding.bindings());
        return new NativeProviderBinding(
                binding.interfaceId(),
                binding.qualifier(),
                byBackend.javaBinding(),
                byBackend.javascriptBinding(),
                byBackend.pythonBinding()
        );
    }

    private static CoreIr.CompiledNativeProviderDeclaration compiledNativeProviderDeclaration(CompiledNativeProviderDeclaration declaration) {
        return new CoreIr.CompiledNativeProviderDeclaration(
                declaration.providerName(),
                declaration.sourceModulePath(),
                declaration.sourceModuleName(),
                declaration.targetTypeName(),
                declaration.interfaceId(),
                declaration.qualifier(),
                declaration.sourceFile()
        );
    }

    private static CoreIrSchema.CompiledNativeProviderDeclarationDto compiledNativeProviderDeclarationDto(CoreIr.CompiledNativeProviderDeclaration declaration) {
        return new CoreIrSchema.CompiledNativeProviderDeclarationDto(
                declaration.provider_name(),
                declaration.source_module_path(),
                declaration.source_module_name(),
                declaration.target_type_name(),
                declaration.interface_id(),
                declaration.qualifier(),
                declaration.source_file()
        );
    }

    private static CompiledNativeProviderDeclaration compiledNativeProviderDeclaration(CoreIrSchema.CompiledNativeProviderDeclarationDto declaration) {
        return new CompiledNativeProviderDeclaration(
                declaration.providerName(),
                declaration.sourceModulePath(),
                declaration.sourceModuleName(),
                declaration.targetTypeName(),
                declaration.interfaceId(),
                declaration.qualifier(),
                declaration.sourceFile()
        );
    }

    private static CoreIr.CompiledNativeProviderBinding compiledNativeProviderBinding(CompiledNativeProviderBinding binding) {
        return new CoreIr.CompiledNativeProviderBinding(
                binding.interfaceId(),
                binding.qualifier(),
                backendBindings(binding.javaBinding(), binding.javascriptBinding(), binding.pythonBinding())
        );
    }

    private static CoreIrSchema.CompiledNativeProviderBindingDto compiledNativeProviderBindingDto(CoreIr.CompiledNativeProviderBinding binding) {
        return new CoreIrSchema.CompiledNativeProviderBindingDto(
                binding.interface_id(),
                binding.qualifier(),
                binding.bindings().stream().map(CoreIrAdapters::backendBindingDto).toList()
        );
    }

    private static CompiledNativeProviderBinding compiledNativeProviderBinding(CoreIrSchema.CompiledNativeProviderBindingDto binding) {
        var byBackend = backendBindings(binding.bindings());
        return new CompiledNativeProviderBinding(
                binding.interfaceId(),
                binding.qualifier(),
                byBackend.javaBinding(),
                byBackend.javascriptBinding(),
                byBackend.pythonBinding()
        );
    }

    private static List<CoreIr.NativeProviderBackendBinding> backendBindings(
            NativeProviderBackendBinding javaBinding,
            NativeProviderBackendBinding javascriptBinding,
            NativeProviderBackendBinding pythonBinding
    ) {
        var bindings = new java.util.ArrayList<CoreIr.NativeProviderBackendBinding>();
        if (javaBinding != null) {
            bindings.add(new CoreIr.JavaNativeProviderBinding(javaBinding.className(), Optional.ofNullable(javaBinding.factory())));
        }
        if (javascriptBinding != null) {
            bindings.add(new CoreIr.JavascriptNativeProviderBinding(
                    javascriptBinding.moduleName(),
                    javascriptBinding.exportName(),
                    Optional.ofNullable(javascriptBinding.factory())
            ));
        }
        if (pythonBinding != null) {
            bindings.add(new CoreIr.PythonNativeProviderBinding(
                    pythonBinding.moduleName(),
                    pythonBinding.className(),
                    Optional.ofNullable(pythonBinding.factory())
            ));
        }
        return List.copyOf(bindings);
    }

    private static CoreIrSchema.NativeProviderBackendBindingDto backendBindingDto(CoreIr.NativeProviderBackendBinding binding) {
        if (binding instanceof CoreIr.JavaNativeProviderBinding javaBinding) {
            return new CoreIrSchema.JavaNativeProviderBindingDto(javaBinding.class_name(), optional(javaBinding.factory()));
        }
        if (binding instanceof CoreIr.JavascriptNativeProviderBinding javascriptBinding) {
            return new CoreIrSchema.JavascriptNativeProviderBindingDto(
                    javascriptBinding.module_name(),
                    javascriptBinding.export_name(),
                    optional(javascriptBinding.factory())
            );
        }
        if (binding instanceof CoreIr.PythonNativeProviderBinding pythonBinding) {
            return new CoreIrSchema.PythonNativeProviderBindingDto(
                    pythonBinding.module_name(),
                    pythonBinding.class_name(),
                    optional(pythonBinding.factory())
            );
        }
        throw unsupported("native provider backend binding", binding);
    }

    private static NativeBackendBindings backendBindings(List<CoreIrSchema.NativeProviderBackendBindingDto> bindings) {
        NativeProviderBackendBinding javaBinding = null;
        NativeProviderBackendBinding javascriptBinding = null;
        NativeProviderBackendBinding pythonBinding = null;
        for (var binding : bindings) {
            if (binding instanceof CoreIrSchema.JavaNativeProviderBindingDto javaDto) {
                javaBinding = new NativeProviderBackendBinding(javaDto.className(), null, null, javaDto.factory().orElse(null));
            } else if (binding instanceof CoreIrSchema.JavascriptNativeProviderBindingDto javascriptDto) {
                javascriptBinding = new NativeProviderBackendBinding(null, javascriptDto.moduleName(), javascriptDto.exportName(), javascriptDto.factory().orElse(null));
            } else if (binding instanceof CoreIrSchema.PythonNativeProviderBindingDto pythonDto) {
                pythonBinding = new NativeProviderBackendBinding(pythonDto.className(), pythonDto.moduleName(), null, pythonDto.factory().orElse(null));
            } else {
                throw unsupported("native provider backend binding DTO", binding);
            }
        }
        return new NativeBackendBindings(javaBinding, javascriptBinding, pythonBinding);
    }

    private record NativeBackendBindings(
            NativeProviderBackendBinding javaBinding,
            NativeProviderBackendBinding javascriptBinding,
            NativeProviderBackendBinding pythonBinding
    ) {
    }

    private static CoreIr.CompiledAnnotation compiledAnnotation(CompiledAnnotation annotation) {
        return new CoreIr.CompiledAnnotation(
                annotation.name(),
                annotation.packageName(),
                annotation.packagePath(),
                annotation.arguments().stream().map(CoreIrAdapters::compiledAnnotationArgument).toList()
        );
    }

    private static CoreIrSchema.CompiledAnnotationDto compiledAnnotationDto(CoreIr.CompiledAnnotation annotation) {
        return new CoreIrSchema.CompiledAnnotationDto(
                annotation.name(),
                annotation.package_name(),
                annotation.package_path(),
                annotation.arguments().stream().map(CoreIrAdapters::compiledAnnotationArgumentDto).toList()
        );
    }

    private static CompiledAnnotation compiledAnnotation(CoreIrSchema.CompiledAnnotationDto annotation) {
        return new CompiledAnnotation(
                annotation.name(),
                annotation.packageName(),
                annotation.packagePath(),
                annotation.arguments().stream().map(CoreIrAdapters::compiledAnnotationArgument).toList()
        );
    }

    private static CoreIr.CompiledAnnotationArgument compiledAnnotationArgument(CompiledAnnotationArgument argument) {
        return new CoreIr.CompiledAnnotationArgument(argument.name(), compiledAnnotationValue(argument.value()));
    }

    private static CoreIrSchema.CompiledAnnotationArgumentDto compiledAnnotationArgumentDto(CoreIr.CompiledAnnotationArgument argument) {
        return new CoreIrSchema.CompiledAnnotationArgumentDto(argument.name(), compiledAnnotationValueDto(argument.value()));
    }

    private static CompiledAnnotationArgument compiledAnnotationArgument(CoreIrSchema.CompiledAnnotationArgumentDto argument) {
        return new CompiledAnnotationArgument(argument.name(), compiledAnnotationValue(argument.value()));
    }

    private static CoreIr.CompiledAnnotationValue compiledAnnotationValue(CompiledAnnotationValue value) {
        if (value instanceof CompiledAnnotationValue.StringValue stringValue) {
            return new CoreIr.CompiledStringAnnotationValue(stringValue.value());
        }
        if (value instanceof CompiledAnnotationValue.IntValue intValue) {
            return new CoreIr.CompiledIntAnnotationValue(intValue.value());
        }
        if (value instanceof CompiledAnnotationValue.LongValue longValue) {
            return new CoreIr.CompiledLongAnnotationValue(longValue.value());
        }
        if (value instanceof CompiledAnnotationValue.FloatValue floatValue) {
            return new CoreIr.CompiledFloatAnnotationValue(floatValue.value());
        }
        if (value instanceof CompiledAnnotationValue.DoubleValue doubleValue) {
            return new CoreIr.CompiledDoubleAnnotationValue(doubleValue.value());
        }
        if (value instanceof CompiledAnnotationValue.BoolValue boolValue) {
            return new CoreIr.CompiledBoolAnnotationValue(boolValue.value());
        }
        if (value instanceof CompiledAnnotationValue.NothingValue) {
            return CoreIr.CompiledNothingAnnotationValue.INSTANCE;
        }
        if (value instanceof CompiledAnnotationValue.TypeNameValue typeNameValue) {
            return new CoreIr.CompiledTypeNameAnnotationValue(typeNameValue.name());
        }
        throw unsupported("compiled annotation value", value);
    }

    private static CoreIrSchema.CompiledAnnotationValueDto compiledAnnotationValueDto(CoreIr.CompiledAnnotationValue value) {
        if (value instanceof CoreIr.CompiledStringAnnotationValue stringValue) {
            return new CoreIrSchema.CompiledStringAnnotationValueDto(stringValue.value());
        }
        if (value instanceof CoreIr.CompiledIntAnnotationValue intValue) {
            return new CoreIrSchema.CompiledIntAnnotationValueDto(intValue.value());
        }
        if (value instanceof CoreIr.CompiledLongAnnotationValue longValue) {
            return new CoreIrSchema.CompiledLongAnnotationValueDto(longValue.value());
        }
        if (value instanceof CoreIr.CompiledFloatAnnotationValue floatValue) {
            return new CoreIrSchema.CompiledFloatAnnotationValueDto(floatValue.value());
        }
        if (value instanceof CoreIr.CompiledDoubleAnnotationValue doubleValue) {
            return new CoreIrSchema.CompiledDoubleAnnotationValueDto(doubleValue.value());
        }
        if (value instanceof CoreIr.CompiledBoolAnnotationValue boolValue) {
            return new CoreIrSchema.CompiledBoolAnnotationValueDto(boolValue.value());
        }
        if (value instanceof CoreIr.CompiledNothingAnnotationValue) {
            return new CoreIrSchema.CompiledNothingAnnotationValueDto();
        }
        if (value instanceof CoreIr.CompiledTypeNameAnnotationValue typeNameValue) {
            return new CoreIrSchema.CompiledTypeNameAnnotationValueDto(typeNameValue.name());
        }
        throw unsupported("compiled annotation value", value);
    }

    private static CompiledAnnotationValue compiledAnnotationValue(CoreIrSchema.CompiledAnnotationValueDto value) {
        if (value instanceof CoreIrSchema.CompiledStringAnnotationValueDto stringValue) {
            return new CompiledAnnotationValue.StringValue(stringValue.value());
        }
        if (value instanceof CoreIrSchema.CompiledIntAnnotationValueDto intValue) {
            return new CompiledAnnotationValue.IntValue(intValue.value());
        }
        if (value instanceof CoreIrSchema.CompiledLongAnnotationValueDto longValue) {
            return new CompiledAnnotationValue.LongValue(longValue.value());
        }
        if (value instanceof CoreIrSchema.CompiledFloatAnnotationValueDto floatValue) {
            return new CompiledAnnotationValue.FloatValue(floatValue.value());
        }
        if (value instanceof CoreIrSchema.CompiledDoubleAnnotationValueDto doubleValue) {
            return new CompiledAnnotationValue.DoubleValue(doubleValue.value());
        }
        if (value instanceof CoreIrSchema.CompiledBoolAnnotationValueDto boolValue) {
            return new CompiledAnnotationValue.BoolValue(boolValue.value());
        }
        if (value instanceof CoreIrSchema.CompiledNothingAnnotationValueDto) {
            return new CompiledAnnotationValue.NothingValue();
        }
        if (value instanceof CoreIrSchema.CompiledTypeNameAnnotationValueDto typeNameValue) {
            return new CompiledAnnotationValue.TypeNameValue(typeNameValue.name());
        }
        throw unsupported("compiled annotation value DTO", value);
    }

    private static CoreIr.CompiledField compiledField(CompiledDataType.CompiledField field) {
        return new CoreIr.CompiledField(
                field.name(),
                compiledType(field.type()),
                field.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
        );
    }

    private static CoreIrSchema.CompiledFieldDto compiledFieldDto(CoreIr.CompiledField field) {
        return new CoreIrSchema.CompiledFieldDto(
                field.name(),
                compiledTypeDto(field.type()),
                field.annotations().stream().map(CoreIrAdapters::compiledAnnotationDto).toList()
        );
    }

    private static CompiledDataType.CompiledField compiledField(CoreIrSchema.CompiledFieldDto field) {
        return new CompiledDataType.CompiledField(
                field.name(),
                compiledType(field.type()),
                field.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
        );
    }

    private static CoreIr.CompiledObjectMethod compiledObjectMethod(CompiledObjectMethod method) {
        return new CoreIr.CompiledObjectMethod(
                method.name(),
                method.parameters().stream()
                        .map(parameter -> new CoreIr.CompiledObjectMethodParameter(parameter.name(), parameter.type()))
                        .toList(),
                method.returnType(),
                method.backendMethodNames().entrySet().stream()
                        .sorted(Map.Entry.comparingByKey())
                        .map(entry -> new CoreIr.BackendMethodName(nativeProviderBackend(entry.getKey()), entry.getValue()))
                        .toList()
        );
    }

    private static CoreIrSchema.CompiledObjectMethodDto compiledObjectMethodDto(CoreIr.CompiledObjectMethod method) {
        return new CoreIrSchema.CompiledObjectMethodDto(
                method.name(),
                method.parameters().stream()
                        .map(parameter -> new CoreIrSchema.CompiledObjectMethodParameterDto(parameter.name(), parameter.type()))
                        .toList(),
                method.return_type(),
                method.backend_method_names().stream()
                        .map(entry -> new CoreIrSchema.BackendMethodNameDto(
                                CoreIrSchema.NativeProviderBackendDto.valueOf(entry.backend().name()),
                                entry.name()))
                        .toList()
        );
    }

    private static CompiledObjectMethod compiledObjectMethod(CoreIrSchema.CompiledObjectMethodDto method) {
        return new CompiledObjectMethod(
                method.name(),
                method.parameters().stream()
                        .map(parameter -> new CompiledObjectMethodParameter(parameter.name(), parameter.type()))
                        .toList(),
                method.returnType(),
                method.backendMethodNames().stream().collect(Collectors.toMap(
                        entry -> entry.backend().name().toLowerCase(Locale.ROOT),
                        CoreIrSchema.BackendMethodNameDto::name,
                        (left, right) -> right,
                        TreeMap::new
                ))
        );
    }

    private static CoreIrSchema.CompiledPrimitiveBackedTypeDto compiledPrimitiveBackedTypeDto(CoreIr.CompiledPrimitiveBackedType type) {
        return new CoreIrSchema.CompiledPrimitiveBackedTypeDto(
                type.name(),
                CoreIrSchema.PrimitiveTypeDto.valueOf(type.backing_type().name()),
                type.cfun_type(),
                type.comments(),
                optional(type.visibility()).map(CoreIrAdapters::visibilityDto),
                type.annotations().stream().map(CoreIrAdapters::compiledAnnotationDto).toList()
        );
    }

    private static CompiledPrimitiveBackedType compiledPrimitiveBackedType(CoreIrSchema.CompiledPrimitiveBackedTypeDto type) {
        return new CompiledPrimitiveBackedType(
                type.name(),
                PrimitiveLinkedType.valueOf(type.backingType().name()),
                type.cfunType(),
                type.comments(),
                type.visibility().map(CoreIrAdapters::visibility).orElse(null),
                type.annotations().stream().map(CoreIrAdapters::compiledAnnotation).toList()
        );
    }

    private static CoreIr.Parameter parameter(ParserSchema.ParameterDto parameter) {
        return new CoreIr.Parameter(
                parsedType(parameter.type()),
                parameter.name(),
                mapOptional(parameter.position(), CoreIrAdapters::sourcePosition)
        );
    }

    private static ParserSchema.ParameterDto parameterDto(CoreIr.Parameter parameter) {
        return new ParserSchema.ParameterDto(
                parsedTypeDto(parameter.type()),
                parameter.name(),
                optional(parameter.position()).map(CoreIrAdapters::sourcePositionDto)
        );
    }

    private static CoreIr.DataField dataField(ParserSchema.DataFieldDto field) {
        return new CoreIr.DataField(field.name(), parsedType(field.type()), field.annotations().stream().map(CoreIrAdapters::annotationUsage).toList());
    }

    private static ParserSchema.DataFieldDto dataFieldDto(CoreIr.DataField field) {
        return new ParserSchema.DataFieldDto(field.name(), parsedTypeDto(field.type()), field.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList());
    }

    private static CoreIr.DeriveDirective deriveDirective(ParserSchema.DeriveDirectiveDto directive) {
        return new CoreIr.DeriveDirective(directive.name(), mapOptional(directive.position(), CoreIrAdapters::sourcePosition));
    }

    private static ParserSchema.DeriveDirectiveDto deriveDirectiveDto(CoreIr.DeriveDirective directive) {
        return new ParserSchema.DeriveDirectiveDto(directive.name(), optional(directive.position()).map(CoreIrAdapters::sourcePositionDto));
    }

    private static CoreIr.DeriverMethod deriverMethod(ParserSchema.DeriverMethodDto method) {
        return new CoreIr.DeriverMethod(
                method.name(),
                method.parameters().stream().map(CoreIrAdapters::parameter).toList(),
                parsedType(method.returnType()),
                parsedExpression(method.expression()),
                method.comments(),
                mapOptional(method.position(), CoreIrAdapters::sourcePosition),
                method.annotations().stream().map(CoreIrAdapters::annotationUsage).toList()
        );
    }

    private static ParserSchema.DeriverMethodDto deriverMethodDto(CoreIr.DeriverMethod method) {
        return new ParserSchema.DeriverMethodDto(
                method.name(),
                method.parameters().stream().map(CoreIrAdapters::parameterDto).toList(),
                parsedTypeDto(method.return_type()),
                parsedExpressionDto(method.expression()),
                method.comments(),
                optional(method.position()).map(CoreIrAdapters::sourcePositionDto),
                method.annotations().stream().map(CoreIrAdapters::annotationUsageDto).toList()
        );
    }

    private static CoreIr.AnnotationUsage annotationUsage(ParserSchema.AnnotationUsageDto usage) {
        return new CoreIr.AnnotationUsage(
                usage.name(),
                usage.arguments().stream().map(CoreIrAdapters::annotationArgument).toList(),
                mapOptional(usage.position(), CoreIrAdapters::sourcePosition)
        );
    }

    private static ParserSchema.AnnotationUsageDto annotationUsageDto(CoreIr.AnnotationUsage usage) {
        return new ParserSchema.AnnotationUsageDto(
                usage.name(),
                usage.arguments().stream().map(CoreIrAdapters::annotationArgumentDto).toList(),
                optional(usage.position()).map(CoreIrAdapters::sourcePositionDto)
        );
    }

    private static CoreIr.AnnotationArgument annotationArgument(ParserSchema.AnnotationArgumentDto argument) {
        return new CoreIr.AnnotationArgument(
                argument.name(),
                annotationValue(argument.value()),
                mapOptional(argument.position(), CoreIrAdapters::sourcePosition)
        );
    }

    private static ParserSchema.AnnotationArgumentDto annotationArgumentDto(CoreIr.AnnotationArgument argument) {
        return new ParserSchema.AnnotationArgumentDto(
                argument.name(),
                annotationValueDto(argument.value()),
                optional(argument.position()).map(CoreIrAdapters::sourcePositionDto)
        );
    }

    private static CoreIr.AnnotationValue annotationValue(ParserSchema.AnnotationValueDto value) {
        if (value instanceof ParserSchema.StringAnnotationValueDto stringValue) {
            return new CoreIr.StringAnnotationValue(stringValue.value(), mapOptional(stringValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.IntAnnotationValueDto intValue) {
            return new CoreIr.IntAnnotationValue(intValue.value(), mapOptional(intValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.LongAnnotationValueDto longValue) {
            return new CoreIr.LongAnnotationValue(longValue.value(), mapOptional(longValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.FloatAnnotationValueDto floatValue) {
            return new CoreIr.FloatAnnotationValue(floatValue.value(), mapOptional(floatValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.DoubleAnnotationValueDto doubleValue) {
            return new CoreIr.DoubleAnnotationValue(doubleValue.value(), mapOptional(doubleValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.BoolAnnotationValueDto boolValue) {
            return new CoreIr.BoolAnnotationValue(boolValue.value(), mapOptional(boolValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.NothingAnnotationValueDto nothingValue) {
            return new CoreIr.NothingAnnotationValue(mapOptional(nothingValue.position(), CoreIrAdapters::sourcePosition));
        }
        if (value instanceof ParserSchema.TypeNameAnnotationValueDto typeNameValue) {
            return new CoreIr.TypeNameAnnotationValue(typeNameValue.name(), mapOptional(typeNameValue.position(), CoreIrAdapters::sourcePosition));
        }
        throw unsupported("annotation value", value);
    }

    private static ParserSchema.AnnotationValueDto annotationValueDto(CoreIr.AnnotationValue value) {
        if (value instanceof CoreIr.StringAnnotationValue stringValue) {
            return new ParserSchema.StringAnnotationValueDto(stringValue.value(), optional(stringValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.IntAnnotationValue intValue) {
            return new ParserSchema.IntAnnotationValueDto(intValue.value(), optional(intValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.LongAnnotationValue longValue) {
            return new ParserSchema.LongAnnotationValueDto(longValue.value(), optional(longValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.FloatAnnotationValue floatValue) {
            return new ParserSchema.FloatAnnotationValueDto(floatValue.value(), optional(floatValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.DoubleAnnotationValue doubleValue) {
            return new ParserSchema.DoubleAnnotationValueDto(doubleValue.value(), optional(doubleValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.BoolAnnotationValue boolValue) {
            return new ParserSchema.BoolAnnotationValueDto(boolValue.value(), optional(boolValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.NothingAnnotationValue nothingValue) {
            return new ParserSchema.NothingAnnotationValueDto(optional(nothingValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        if (value instanceof CoreIr.TypeNameAnnotationValue typeNameValue) {
            return new ParserSchema.TypeNameAnnotationValueDto(typeNameValue.name(), optional(typeNameValue.position()).map(CoreIrAdapters::sourcePositionDto));
        }
        throw unsupported("annotation value", value);
    }

    private static ParserSchema.AnnotationValueDto annotationValueDto(Object value) {
        return annotationValueDto((CoreIr.AnnotationValue) value);
    }

    private static CoreIr.AnnotationTarget annotationTarget(ParserSchema.AnnotationTargetDto target) {
        return new CoreIr.AnnotationTarget(target.name(), mapOptional(target.position(), CoreIrAdapters::sourcePosition));
    }

    private static ParserSchema.AnnotationTargetDto annotationTargetDto(CoreIr.AnnotationTarget target) {
        return new ParserSchema.AnnotationTargetDto(target.name(), optional(target.position()).map(CoreIrAdapters::sourcePositionDto));
    }

    private static CoreIr.AnnotationFieldDeclaration annotationField(ParserSchema.AnnotationFieldDefinitionDto field) {
        return new CoreIr.AnnotationFieldDeclaration(
                field.name(),
                field.type(),
                mapOptional(field.defaultValue(), CoreIrAdapters::annotationValue),
                mapOptional(field.position(), CoreIrAdapters::sourcePosition)
        );
    }

    private static ParserSchema.AnnotationFieldDefinitionDto annotationFieldDto(CoreIr.AnnotationFieldDeclaration field) {
        return new ParserSchema.AnnotationFieldDefinitionDto(
                field.name(),
                field.type(),
                optional(field.default_value()).map(CoreIrAdapters::annotationValueDto),
                optional(field.position()).map(CoreIrAdapters::sourcePositionDto)
        );
    }

    private static CoreIr.ParsedMatchCase matchCase(ParserSchema.MatchCaseDto matchCase) {
        return new CoreIr.ParsedMatchCase(
                parsedPattern(matchCase.pattern()),
                mapOptional(matchCase.guard(), CoreIrAdapters::parsedExpression),
                parsedExpression(matchCase.expression())
        );
    }

    private static ParserSchema.MatchCaseDto matchCaseDto(CoreIr.ParsedMatchCase matchCase) {
        return new ParserSchema.MatchCaseDto(
                parsedPatternDto(matchCase.pattern()),
                optional(matchCase.guard()).map(CoreIrAdapters::parsedExpressionDto),
                parsedExpressionDto(matchCase.expression())
        );
    }

    private static CoreIr.ParsedPattern parsedPattern(ParserSchema.PatternDto pattern) {
        if (pattern instanceof ParserSchema.IntPatternDto intPattern) {
            return new CoreIr.ParsedIntPattern(intPattern.value());
        }
        if (pattern instanceof ParserSchema.LongPatternDto longPattern) {
            return new CoreIr.ParsedLongPattern(longPattern.value());
        }
        if (pattern instanceof ParserSchema.StringPatternDto stringPattern) {
            return new CoreIr.ParsedStringPattern(stringPattern.value());
        }
        if (pattern instanceof ParserSchema.BoolPatternDto boolPattern) {
            return new CoreIr.ParsedBoolPattern(boolPattern.value());
        }
        if (pattern instanceof ParserSchema.FloatPatternDto floatPattern) {
            return new CoreIr.ParsedFloatPattern(floatPattern.value());
        }
        if (pattern instanceof ParserSchema.TypedPatternDto typedPattern) {
            return new CoreIr.ParsedTypedPattern(parsedType(typedPattern.type()), typedPattern.name());
        }
        if (pattern instanceof ParserSchema.VariablePatternDto variablePattern) {
            return new CoreIr.ParsedVariablePattern(variablePattern.name());
        }
        if (pattern instanceof ParserSchema.WildcardPatternDto) {
            return CoreIr.ParsedWildcardPattern.INSTANCE;
        }
        if (pattern instanceof ParserSchema.WildcardBindingPatternDto wildcardBindingPattern) {
            return new CoreIr.ParsedWildcardBindingPattern(wildcardBindingPattern.name());
        }
        if (pattern instanceof ParserSchema.ConstructorPatternDto constructorPattern) {
            return new CoreIr.ParsedConstructorPattern(
                    constructorPattern.constructorName(),
                    constructorPattern.fieldPatterns().stream().map(CoreIrAdapters::parsedPattern).toList()
            );
        }
        throw unsupported("parsed pattern", pattern);
    }

    private static ParserSchema.PatternDto parsedPatternDto(CoreIr.ParsedPattern pattern) {
        if (pattern instanceof CoreIr.ParsedIntPattern intPattern) {
            return new ParserSchema.IntPatternDto(intPattern.value());
        }
        if (pattern instanceof CoreIr.ParsedLongPattern longPattern) {
            return new ParserSchema.LongPatternDto(longPattern.value());
        }
        if (pattern instanceof CoreIr.ParsedStringPattern stringPattern) {
            return new ParserSchema.StringPatternDto(stringPattern.value());
        }
        if (pattern instanceof CoreIr.ParsedBoolPattern boolPattern) {
            return new ParserSchema.BoolPatternDto(boolPattern.value());
        }
        if (pattern instanceof CoreIr.ParsedFloatPattern floatPattern) {
            return new ParserSchema.FloatPatternDto(floatPattern.value());
        }
        if (pattern instanceof CoreIr.ParsedTypedPattern typedPattern) {
            return new ParserSchema.TypedPatternDto(parsedTypeDto(typedPattern.type()), typedPattern.name());
        }
        if (pattern instanceof CoreIr.ParsedVariablePattern variablePattern) {
            return new ParserSchema.VariablePatternDto(variablePattern.name());
        }
        if (pattern instanceof CoreIr.ParsedWildcardPattern) {
            return ParserSchema.WildcardPatternDto.WILDCARD;
        }
        if (pattern instanceof CoreIr.ParsedWildcardBindingPattern wildcardBindingPattern) {
            return new ParserSchema.WildcardBindingPatternDto(wildcardBindingPattern.name());
        }
        if (pattern instanceof CoreIr.ParsedConstructorPattern constructorPattern) {
            return new ParserSchema.ConstructorPatternDto(
                    constructorPattern.constructor_name(),
                    constructorPattern.field_patterns().stream().map(CoreIrAdapters::parsedPatternDto).toList()
            );
        }
        throw unsupported("parsed pattern", pattern);
    }

    private static CoreIr.ParsedFieldAssignment fieldAssignment(ParserSchema.FieldAssignmentDto assignment) {
        return new CoreIr.ParsedFieldAssignment(assignment.name(), parsedExpression(assignment.value()));
    }

    private static ParserSchema.FieldAssignmentDto fieldAssignmentDto(CoreIr.ParsedFieldAssignment assignment) {
        return new ParserSchema.FieldAssignmentDto(assignment.name(), parsedExpressionDto(assignment.value()));
    }

    private static CoreIr.Import importDeclaration(ParserSchema.ImportDto importDto) {
        return new CoreIr.Import(
                importDto.moduleName(),
                importDto.symbols(),
                importDto.excludedSymbols(),
                importDto.qualifiedOnly()
        );
    }

    private static ParserSchema.ImportDto importDto(CoreIr.Import importDeclaration) {
        return new ParserSchema.ImportDto(
                importDeclaration.module_name(),
                importDeclaration.symbols(),
                importDeclaration.excluded_symbols(),
                importDeclaration.qualified_only()
        );
    }

    private static CoreIr.OoNativeProviderDeclaration ooNativeProvider(ParserSchema.OoNativeProviderDeclarationDto declaration) {
        return new CoreIr.OoNativeProviderDeclaration(declaration.name(), declaration.targetType(), declaration.qualifier(), declaration.comments());
    }

    private static ParserSchema.OoNativeProviderDeclarationDto ooNativeProviderDto(CoreIr.OoNativeProviderDeclaration declaration) {
        return new ParserSchema.OoNativeProviderDeclarationDto(declaration.name(), declaration.target_type(), declaration.qualifier(), declaration.comments());
    }

    private static CoreIr.OoParameter ooParameter(ParserSchema.OoParameterDto parameter) {
        return new CoreIr.OoParameter(parameter.name(), parameter.type());
    }

    private static ParserSchema.OoParameterDto ooParameterDto(CoreIr.OoParameter parameter) {
        return new ParserSchema.OoParameterDto(parameter.name(), parameter.type());
    }

    private static CoreIr.OoTypeReference ooTypeReference(ParserSchema.OoTypeReferenceDto reference) {
        return new CoreIr.OoTypeReference(reference.name());
    }

    private static ParserSchema.OoTypeReferenceDto ooTypeReferenceDto(CoreIr.OoTypeReference reference) {
        return new ParserSchema.OoTypeReferenceDto(reference.name());
    }

    private static CoreIr.SourcePosition sourcePosition(ParserSchema.SourcePositionDto position) {
        return new CoreIr.SourcePosition(position.line(), position.column(), position.length());
    }

    private static ParserSchema.SourcePositionDto sourcePositionDto(CoreIr.SourcePosition position) {
        return new ParserSchema.SourcePositionDto(position.line(), position.column(), optional(position.length()));
    }

    private static ParserSchema.SourcePositionDto sourcePositionDto(Object position) {
        return sourcePositionDto((CoreIr.SourcePosition) position);
    }

    private static dev.capylang.compiler.parser.SourcePosition sourcePosition(CoreIrSchema.SourcePositionDto position) {
        return new dev.capylang.compiler.parser.SourcePosition(position.line(), position.column(), position.length());
    }

    private static CoreIrSchema.SourcePositionDto coreSourcePositionDto(CoreIr.SourcePosition position) {
        return new CoreIrSchema.SourcePositionDto(position.line(), position.column(), optional(position.length()));
    }

    private static CoreIrSchema.SourcePositionDto coreSourcePositionDto(Object position) {
        return coreSourcePositionDto((CoreIr.SourcePosition) position);
    }

    private static CoreIr.SourcePosition sourcePosition(dev.capylang.compiler.parser.SourcePosition position) {
        return new CoreIr.SourcePosition(position.line(), position.column(), position.length());
    }

    private static CoreIr.SourceKind sourceKind(ParserSchema.SourceKindDto sourceKind) {
        return switch (sourceKind) {
            case FUNCTIONAL -> CoreIr.SourceKind.FUNCTIONAL;
            case OBJECT_ORIENTED -> CoreIr.SourceKind.OBJECT_ORIENTED;
        };
    }

    private static ParserSchema.SourceKindDto sourceKindDto(CoreIr.SourceKind sourceKind) {
        return switch (sourceKind) {
            case FUNCTIONAL -> ParserSchema.SourceKindDto.FUNCTIONAL;
            case OBJECT_ORIENTED -> ParserSchema.SourceKindDto.OBJECT_ORIENTED;
        };
    }

    private static Optional<CoreIr.Visibility> visibility(Optional<String> visibility) {
        return visibility.map(value -> CoreIr.Visibility.valueOf(value.toUpperCase(Locale.ROOT)));
    }

    private static Optional<CoreIr.Visibility> visibility(Visibility visibility) {
        return Optional.ofNullable(visibility).map(value -> CoreIr.Visibility.valueOf(value.name()));
    }

    private static Optional<String> visibilityDto(Optional<CoreIr.Visibility> visibility) {
        return visibility.map(value -> value.name().toLowerCase(Locale.ROOT));
    }

    private static CoreIrSchema.VisibilityDto visibilityDto(CoreIr.Visibility visibility) {
        return CoreIrSchema.VisibilityDto.valueOf(visibility.name());
    }

    private static CoreIrSchema.VisibilityDto visibilityDto(Object visibility) {
        return visibilityDto((CoreIr.Visibility) visibility);
    }

    private static Visibility visibility(CoreIrSchema.VisibilityDto visibility) {
        return Visibility.valueOf(visibility.name());
    }

    private static CoreIr.PrimitiveType primitiveType(String name) {
        return switch (name) {
            case "byte" -> CoreIr.PrimitiveType.BYTE;
            case "int" -> CoreIr.PrimitiveType.INT;
            case "long" -> CoreIr.PrimitiveType.LONG;
            case "double" -> CoreIr.PrimitiveType.DOUBLE;
            case "String" -> CoreIr.PrimitiveType.STRING;
            case "bool" -> CoreIr.PrimitiveType.BOOL;
            case "float" -> CoreIr.PrimitiveType.FLOAT;
            case "any" -> CoreIr.PrimitiveType.ANY;
            case "data" -> CoreIr.PrimitiveType.DATA;
            case "enum" -> CoreIr.PrimitiveType.ENUM;
            case "nothing" -> CoreIr.PrimitiveType.NOTHING;
            default -> throw new IllegalArgumentException("Unknown primitive type: " + name);
        };
    }

    private static String primitiveName(CoreIr.PrimitiveType primitiveType) {
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

    private static CoreIr.InfixOperator infixOperator(String symbol) {
        return switch (symbol) {
            case "+" -> CoreIr.InfixOperator.PLUS;
            case "-" -> CoreIr.InfixOperator.MINUS;
            case "*" -> CoreIr.InfixOperator.MUL;
            case "/" -> CoreIr.InfixOperator.DIV;
            case "%" -> CoreIr.InfixOperator.MOD;
            case "^" -> CoreIr.InfixOperator.POWER;
            case ".and." -> CoreIr.InfixOperator.BITWISE_AND;
            case ".nand." -> CoreIr.InfixOperator.BITWISE_NAND;
            case ".or." -> CoreIr.InfixOperator.BITWISE_OR;
            case ".xor." -> CoreIr.InfixOperator.BITWISE_XOR;
            case ".not." -> CoreIr.InfixOperator.BITWISE_NOT;
            case ">" -> CoreIr.InfixOperator.GT;
            case "<" -> CoreIr.InfixOperator.LT;
            case "==" -> CoreIr.InfixOperator.EQUAL;
            case "!=" -> CoreIr.InfixOperator.NOTEQUAL;
            case "<=" -> CoreIr.InfixOperator.LE;
            case ">=" -> CoreIr.InfixOperator.GE;
            case "&" -> CoreIr.InfixOperator.AND;
            case "?" -> CoreIr.InfixOperator.QUESTION;
            case "~" -> CoreIr.InfixOperator.TILDE;
            case "~~" -> CoreIr.InfixOperator.TILDE_TILDE;
            case "~>" -> CoreIr.InfixOperator.TILDE_GT;
            case "/>" -> CoreIr.InfixOperator.DIV_GT;
            case "|" -> CoreIr.InfixOperator.PIPE;
            case "|-" -> CoreIr.InfixOperator.PIPE_MINUS;
            case "|*" -> CoreIr.InfixOperator.PIPE_FLATMAP;
            case "|>" -> CoreIr.InfixOperator.PIPE_REDUCE;
            default -> throw new IllegalArgumentException("Unknown infix operator: " + symbol);
        };
    }

    private static String infixSymbol(CoreIr.InfixOperator operator) {
        return switch (operator) {
            case PLUS -> "+";
            case MINUS -> "-";
            case MUL -> "*";
            case DIV -> "/";
            case MOD -> "%";
            case POWER -> "^";
            case BITWISE_AND -> ".and.";
            case BITWISE_NAND -> ".nand.";
            case BITWISE_OR -> ".or.";
            case BITWISE_XOR -> ".xor.";
            case BITWISE_NOT -> ".not.";
            case GT -> ">";
            case LT -> "<";
            case EQUAL -> "==";
            case NOTEQUAL -> "!=";
            case LE -> "<=";
            case GE -> ">=";
            case AND -> "&";
            case QUESTION -> "?";
            case TILDE -> "~";
            case TILDE_TILDE -> "~~";
            case TILDE_GT -> "~>";
            case DIV_GT -> "/>";
            case PIPE -> "|";
            case PIPE_MINUS -> "|-";
            case PIPE_FLATMAP -> "|*";
            case PIPE_REDUCE -> "|>";
        };
    }

    private static CoreIr.NativeProviderBackend nativeProviderBackend(String backend) {
        return CoreIr.NativeProviderBackend.valueOf(backend.toUpperCase(Locale.ROOT));
    }

    private static <T, R> Optional<R> mapOptional(Optional<T> value, Function<T, R> mapper) {
        return value.map(mapper);
    }

    @SuppressWarnings("unchecked")
    private static <T> Optional<T> optional(Object value) {
        if (value == null) {
            return Optional.empty();
        }
        return (Optional<T>) value;
    }

    private static IllegalArgumentException unsupported(String label, Object value) {
        return new IllegalArgumentException("Unsupported " + label + ": " + value);
    }
}
