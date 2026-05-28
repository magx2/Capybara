package dev.capylang.compiler.parser.facade;

import java.util.List;
import java.util.Optional;

public final class ParserSchema {
    private ParserSchema() {
    }

    public enum SourceKindDto {
        FUNCTIONAL,
        OBJECT_ORIENTED
    }

    public record SourceModuleDto(String name, String path, String input, SourceKindDto sourceKind) {
        public String file() {
            var normalizedPath = path.replace('\\', '/');
            if (!normalizedPath.startsWith("/")) {
                normalizedPath = "/" + normalizedPath;
            }
            var extension = sourceKind == SourceKindDto.OBJECT_ORIENTED ? ".coo" : ".cfun";
            return normalizedPath + "/" + name + extension;
        }
    }

    public record ImportDto(
            String moduleName,
            List<String> symbols,
            List<String> excludedSymbols,
            boolean qualifiedOnly
    ) {
    }

    public record ImportLineDto(int line, ImportDto importDeclaration) {
    }

    public record ImportExtractionDto(
            SourceModuleDto module,
            String sourceWithoutImports,
            List<ImportDto> imports,
            List<ImportLineDto> importLines
    ) {
    }

    public record SyntaxDiagnosticDto(String file, int line, int column, String message) {
    }

    public record ParseResultDto<T>(Optional<T> value, List<SyntaxDiagnosticDto> diagnostics) {
        public ParseResultDto {
            value = value == null ? Optional.empty() : value;
            diagnostics = diagnostics == null ? List.of() : List.copyOf(diagnostics);
        }

        public static <T> ParseResultDto<T> success(T value) {
            return new ParseResultDto<>(Optional.of(value), List.of());
        }

        public static <T> ParseResultDto<T> error(List<SyntaxDiagnosticDto> diagnostics) {
            return new ParseResultDto<>(Optional.empty(), diagnostics);
        }

        public boolean success() {
            return value.isPresent() && diagnostics.isEmpty();
        }
    }

    public record SourcePositionDto(int line, int column, Optional<Integer> length) {
        public SourcePositionDto {
            length = length == null ? Optional.empty() : length;
        }
    }

    public record FunctionalProgramDto(List<FunctionalModuleDto> modules) {
        public FunctionalProgramDto {
            modules = List.copyOf(modules);
        }
    }

    public record FunctionalModuleDto(
            String name,
            String path,
            List<FunctionalDefinitionDto> definitions,
            List<ImportDto> imports,
            SourceKindDto sourceKind
    ) {
        public FunctionalModuleDto {
            definitions = List.copyOf(definitions);
            imports = List.copyOf(imports);
        }
    }

    public interface FunctionalDefinitionDto {
    }

    public record FunctionDefinitionDto(
            String name,
            List<ParameterDto> parameters,
            Optional<TypeDto> returnType,
            ExpressionDto expression,
            List<String> comments,
            Optional<String> visibility,
            Optional<SourcePositionDto> position,
            boolean tailRecursive,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public FunctionDefinitionDto {
            parameters = List.copyOf(parameters);
            returnType = returnType == null ? Optional.empty() : returnType;
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record PrimitiveBackedTypeDefinitionDto(
            String name,
            String backingType,
            Optional<ExpressionDto> constructor,
            List<String> comments,
            Optional<String> visibility,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public PrimitiveBackedTypeDefinitionDto {
            constructor = constructor == null ? Optional.empty() : constructor;
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record UnionDefinitionDto(
            String name,
            List<String> subTypes,
            List<DataFieldDto> fields,
            List<String> typeParameters,
            Optional<ExpressionDto> constructor,
            List<DeriveDirectiveDto> derives,
            List<String> comments,
            Optional<String> visibility,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public UnionDefinitionDto {
            subTypes = List.copyOf(subTypes);
            fields = List.copyOf(fields);
            typeParameters = List.copyOf(typeParameters);
            constructor = constructor == null ? Optional.empty() : constructor;
            derives = List.copyOf(derives);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record DataDefinitionDto(
            String name,
            List<DataFieldDto> fields,
            List<String> extendsTypes,
            List<String> typeParameters,
            Optional<ExpressionDto> constructor,
            List<DeriveDirectiveDto> derives,
            List<String> comments,
            Optional<String> visibility,
            boolean nativeType,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public DataDefinitionDto {
            fields = List.copyOf(fields);
            extendsTypes = List.copyOf(extendsTypes);
            typeParameters = List.copyOf(typeParameters);
            constructor = constructor == null ? Optional.empty() : constructor;
            derives = List.copyOf(derives);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record EnumDefinitionDto(
            String name,
            List<String> values,
            List<String> comments,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public EnumDefinitionDto {
            values = List.copyOf(values);
            comments = List.copyOf(comments);
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record DeriverDefinitionDto(
            String name,
            List<DeriverMethodDto> methods,
            List<String> comments,
            Optional<String> visibility,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public DeriverDefinitionDto {
            methods = List.copyOf(methods);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record DeriverMethodDto(
            String name,
            List<ParameterDto> parameters,
            TypeDto returnType,
            ExpressionDto expression,
            List<String> comments,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) {
        public DeriverMethodDto {
            parameters = List.copyOf(parameters);
            comments = List.copyOf(comments);
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record AnnotationDefinitionDto(
            String name,
            List<AnnotationTargetDto> targets,
            List<AnnotationFieldDefinitionDto> fields,
            boolean multiple,
            List<String> comments,
            Optional<String> visibility,
            Optional<SourcePositionDto> position,
            List<AnnotationUsageDto> annotations
    ) implements FunctionalDefinitionDto {
        public AnnotationDefinitionDto {
            targets = List.copyOf(targets);
            fields = List.copyOf(fields);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            position = position == null ? Optional.empty() : position;
            annotations = List.copyOf(annotations);
        }
    }

    public record AnnotationTargetDto(String name, Optional<SourcePositionDto> position) {
        public AnnotationTargetDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record AnnotationFieldDefinitionDto(
            String name,
            String type,
            Optional<AnnotationValueDto> defaultValue,
            Optional<SourcePositionDto> position
    ) {
        public AnnotationFieldDefinitionDto {
            defaultValue = defaultValue == null ? Optional.empty() : defaultValue;
            position = position == null ? Optional.empty() : position;
        }
    }

    public record DeriveDirectiveDto(String name, Optional<SourcePositionDto> position) {
        public DeriveDirectiveDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record DataFieldDto(String name, TypeDto type, List<AnnotationUsageDto> annotations) {
        public DataFieldDto {
            annotations = List.copyOf(annotations);
        }
    }

    public record ParameterDto(TypeDto type, String name, Optional<SourcePositionDto> position) {
        public ParameterDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public interface TypeDto {
    }

    public record PrimitiveTypeDto(String name) implements TypeDto {
    }

    public record DataTypeDto(String name) implements TypeDto {
    }

    public record ListTypeDto(TypeDto elementType) implements TypeDto {
    }

    public record SetTypeDto(TypeDto elementType) implements TypeDto {
    }

    public record DictTypeDto(TypeDto valueType) implements TypeDto {
    }

    public record FunctionTypeDto(TypeDto argumentType, TypeDto returnType) implements TypeDto {
    }

    public record TupleTypeDto(List<TypeDto> elementTypes) implements TypeDto {
        public TupleTypeDto {
            elementTypes = List.copyOf(elementTypes);
        }
    }

    public record AnnotationUsageDto(
            String name,
            List<AnnotationArgumentDto> arguments,
            Optional<SourcePositionDto> position
    ) {
        public AnnotationUsageDto {
            arguments = List.copyOf(arguments);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record AnnotationArgumentDto(
            String name,
            AnnotationValueDto value,
            Optional<SourcePositionDto> position
    ) {
        public AnnotationArgumentDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public interface AnnotationValueDto {
        Optional<SourcePositionDto> position();
    }

    public record StringAnnotationValueDto(String value, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public StringAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record IntAnnotationValueDto(String value, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public IntAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record LongAnnotationValueDto(String value, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public LongAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FloatAnnotationValueDto(String value, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public FloatAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record DoubleAnnotationValueDto(String value, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public DoubleAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record BoolAnnotationValueDto(boolean value, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public BoolAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record NothingAnnotationValueDto(Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public NothingAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record TypeNameAnnotationValueDto(String name, Optional<SourcePositionDto> position) implements AnnotationValueDto {
        public TypeNameAnnotationValueDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public interface ExpressionDto {
        Optional<SourcePositionDto> position();
    }

    public record BoolExpressionDto(boolean value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public BoolExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record ByteExpressionDto(String value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public ByteExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record IntExpressionDto(String value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public IntExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record LongExpressionDto(String value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public LongExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FloatExpressionDto(String value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public FloatExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record DoubleExpressionDto(String value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public DoubleExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record StringExpressionDto(String value, Optional<SourcePositionDto> position) implements ExpressionDto {
        public StringExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record NothingExpressionDto(String literal, Optional<SourcePositionDto> position) implements ExpressionDto {
        public NothingExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record ValueExpressionDto(String name, Optional<SourcePositionDto> position) implements ExpressionDto {
        public ValueExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FunctionCallExpressionDto(
            Optional<String> moduleName,
            String name,
            List<ExpressionDto> arguments,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public FunctionCallExpressionDto {
            moduleName = moduleName == null ? Optional.empty() : moduleName;
            arguments = List.copyOf(arguments);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FunctionInvokeExpressionDto(
            ExpressionDto function,
            List<ExpressionDto> arguments,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public FunctionInvokeExpressionDto {
            arguments = List.copyOf(arguments);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FunctionReferenceExpressionDto(String name, Optional<SourcePositionDto> position) implements ExpressionDto {
        public FunctionReferenceExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record IfExpressionDto(
            ExpressionDto condition,
            ExpressionDto thenBranch,
            ExpressionDto elseBranch,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public IfExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record InfixExpressionDto(
            ExpressionDto left,
            String operator,
            ExpressionDto right,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public InfixExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FieldAccessExpressionDto(
            ExpressionDto source,
            String field,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public FieldAccessExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record LambdaExpressionDto(
            List<String> argumentNames,
            ExpressionDto expression,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public LambdaExpressionDto {
            argumentNames = List.copyOf(argumentNames);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record LetExpressionDto(
            String name,
            Optional<TypeDto> declaredType,
            String kind,
            ExpressionDto value,
            ExpressionDto rest,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public LetExpressionDto {
            declaredType = declaredType == null ? Optional.empty() : declaredType;
            position = position == null ? Optional.empty() : position;
        }
    }

    public record ReduceExpressionDto(
            ExpressionDto initialValue,
            String accumulatorName,
            Optional<String> keyName,
            String valueName,
            ExpressionDto reducerExpression,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public ReduceExpressionDto {
            keyName = keyName == null ? Optional.empty() : keyName;
            position = position == null ? Optional.empty() : position;
        }
    }

    public record IndexExpressionDto(
            ExpressionDto source,
            List<ExpressionDto> arguments,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public IndexExpressionDto {
            arguments = List.copyOf(arguments);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record SliceExpressionDto(
            ExpressionDto source,
            Optional<ExpressionDto> start,
            Optional<ExpressionDto> end,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public SliceExpressionDto {
            start = start == null ? Optional.empty() : start;
            end = end == null ? Optional.empty() : end;
            position = position == null ? Optional.empty() : position;
        }
    }

    public record MatchExpressionDto(
            ExpressionDto matchWith,
            List<MatchCaseDto> cases,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public MatchExpressionDto {
            cases = List.copyOf(cases);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record MatchCaseDto(PatternDto pattern, Optional<ExpressionDto> guard, ExpressionDto expression) {
        public MatchCaseDto {
            guard = guard == null ? Optional.empty() : guard;
        }
    }

    public interface PatternDto {
    }

    public record IntPatternDto(String value) implements PatternDto {
    }

    public record LongPatternDto(String value) implements PatternDto {
    }

    public record StringPatternDto(String value) implements PatternDto {
    }

    public record BoolPatternDto(String value) implements PatternDto {
    }

    public record FloatPatternDto(String value) implements PatternDto {
    }

    public record TypedPatternDto(TypeDto type, String name) implements PatternDto {
    }

    public record VariablePatternDto(String name) implements PatternDto {
    }

    public enum WildcardPatternDto implements PatternDto {
        WILDCARD
    }

    public record WildcardBindingPatternDto(String name) implements PatternDto {
    }

    public record ConstructorPatternDto(String constructorName, List<PatternDto> fieldPatterns) implements PatternDto {
        public ConstructorPatternDto {
            fieldPatterns = List.copyOf(fieldPatterns);
        }
    }

    public record NewDataExpressionDto(
            TypeDto type,
            boolean bypassConstructor,
            List<FieldAssignmentDto> assignments,
            List<ExpressionDto> positionalArguments,
            List<ExpressionDto> spreads,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public NewDataExpressionDto {
            assignments = List.copyOf(assignments);
            positionalArguments = List.copyOf(positionalArguments);
            spreads = List.copyOf(spreads);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record ConstructorDataExpressionDto(
            List<FieldAssignmentDto> assignments,
            List<ExpressionDto> positionalArguments,
            List<ExpressionDto> spreads,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public ConstructorDataExpressionDto {
            assignments = List.copyOf(assignments);
            positionalArguments = List.copyOf(positionalArguments);
            spreads = List.copyOf(spreads);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record FieldAssignmentDto(String name, ExpressionDto value) {
    }

    public record WithExpressionDto(
            ExpressionDto source,
            List<FieldAssignmentDto> assignments,
            Optional<SourcePositionDto> position
    ) implements ExpressionDto {
        public WithExpressionDto {
            assignments = List.copyOf(assignments);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record NewListExpressionDto(List<ExpressionDto> values, Optional<SourcePositionDto> position) implements ExpressionDto {
        public NewListExpressionDto {
            values = List.copyOf(values);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record NewSetExpressionDto(List<ExpressionDto> values, Optional<SourcePositionDto> position) implements ExpressionDto {
        public NewSetExpressionDto {
            values = List.copyOf(values);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record NewDictExpressionDto(List<DictEntryDto> entries, Optional<SourcePositionDto> position) implements ExpressionDto {
        public NewDictExpressionDto {
            entries = List.copyOf(entries);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record DictEntryDto(ExpressionDto key, ExpressionDto value) {
    }

    public record TupleExpressionDto(List<ExpressionDto> values, Optional<SourcePositionDto> position) implements ExpressionDto {
        public TupleExpressionDto {
            values = List.copyOf(values);
            position = position == null ? Optional.empty() : position;
        }
    }

    public record UnwrapExpressionDto(ExpressionDto expression, Optional<SourcePositionDto> position) implements ExpressionDto {
        public UnwrapExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record PlaceholderExpressionDto(Optional<SourcePositionDto> position) implements ExpressionDto {
        public PlaceholderExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record ObjectOrientedModuleDto(
            String name,
            String path,
            List<OoTypeDeclarationDto> definitions,
            List<OoNativeProviderDeclarationDto> nativeProviders,
            List<ImportDto> imports,
            SourceKindDto sourceKind
    ) {
        public ObjectOrientedModuleDto {
            definitions = List.copyOf(definitions);
            nativeProviders = List.copyOf(nativeProviders);
            imports = List.copyOf(imports);
        }
    }

    public record ParsedModuleSetDto(
            FunctionalProgramDto functionalProgram,
            List<ObjectOrientedModuleDto> objectOrientedModules
    ) {
        public ParsedModuleSetDto {
            objectOrientedModules = List.copyOf(objectOrientedModules);
        }
    }

    public interface OoTypeDeclarationDto {
        String name();
    }

    public record OoClassDefinitionDto(
            String name,
            List<OoParameterDto> constructorParameters,
            List<OoTypeReferenceDto> parents,
            List<OoMemberDeclarationDto> members,
            List<String> modifiers,
            List<String> comments,
            List<AnnotationUsageDto> annotations
    ) implements OoTypeDeclarationDto {
        public OoClassDefinitionDto {
            constructorParameters = List.copyOf(constructorParameters);
            parents = List.copyOf(parents);
            members = List.copyOf(members);
            modifiers = List.copyOf(modifiers);
            comments = List.copyOf(comments);
            annotations = List.copyOf(annotations);
        }
    }

    public record OoTraitDefinitionDto(
            String name,
            List<OoTypeReferenceDto> parents,
            List<OoMemberDeclarationDto> members,
            List<String> comments,
            List<AnnotationUsageDto> annotations
    ) implements OoTypeDeclarationDto {
        public OoTraitDefinitionDto {
            parents = List.copyOf(parents);
            members = List.copyOf(members);
            comments = List.copyOf(comments);
            annotations = List.copyOf(annotations);
        }
    }

    public record OoInterfaceDefinitionDto(
            String name,
            List<OoTypeReferenceDto> parents,
            List<OoMemberDeclarationDto> members,
            List<String> comments,
            List<AnnotationUsageDto> annotations
    ) implements OoTypeDeclarationDto {
        public OoInterfaceDefinitionDto {
            parents = List.copyOf(parents);
            members = List.copyOf(members);
            comments = List.copyOf(comments);
            annotations = List.copyOf(annotations);
        }
    }

    public record OoNativeProviderDeclarationDto(String name, String targetType, String qualifier, List<String> comments) {
        public OoNativeProviderDeclarationDto {
            comments = List.copyOf(comments);
        }
    }

    public interface OoMemberDeclarationDto {
    }

    public record OoFieldDefinitionDto(
            String name,
            String type,
            String visibility,
            Optional<String> initializer,
            List<String> comments,
            List<AnnotationUsageDto> annotations
    ) implements OoMemberDeclarationDto {
        public OoFieldDefinitionDto {
            initializer = initializer == null ? Optional.empty() : initializer;
            comments = List.copyOf(comments);
            annotations = List.copyOf(annotations);
        }
    }

    public record OoMethodDefinitionDto(
            String name,
            List<OoParameterDto> parameters,
            String returnType,
            String visibility,
            List<String> modifiers,
            Optional<OoMethodBodyDto> body,
            List<String> comments,
            List<AnnotationUsageDto> annotations
    ) implements OoMemberDeclarationDto {
        public OoMethodDefinitionDto {
            parameters = List.copyOf(parameters);
            modifiers = List.copyOf(modifiers);
            body = body == null ? Optional.empty() : body;
            comments = List.copyOf(comments);
            annotations = List.copyOf(annotations);
        }
    }

    public record OoInitBlockDto(
            OoStatementBlockDto body,
            List<String> comments,
            List<AnnotationUsageDto> annotations
    ) implements OoMemberDeclarationDto {
        public OoInitBlockDto {
            comments = List.copyOf(comments);
            annotations = List.copyOf(annotations);
        }
    }

    public interface OoMethodBodyDto {
    }

    public interface OoStatementDto {
    }

    public record OoExpressionBodyDto(String rawExpression) implements OoMethodBodyDto {
    }

    public record OoStatementBlockDto(List<OoStatementDto> statements) implements OoMethodBodyDto, OoStatementDto {
        public OoStatementBlockDto {
            statements = List.copyOf(statements);
        }
    }

    public record OoLetStatementDto(String name, Optional<String> type, String rawExpression) implements OoStatementDto {
        public OoLetStatementDto {
            type = type == null ? Optional.empty() : type;
        }
    }

    public record OoLocalMethodStatementDto(
            String name,
            List<OoParameterDto> parameters,
            String returnType,
            OoMethodBodyDto body,
            List<String> comments
    ) implements OoStatementDto {
        public OoLocalMethodStatementDto {
            parameters = List.copyOf(parameters);
            comments = List.copyOf(comments);
        }
    }

    public record OoMutableVariableStatementDto(String name, Optional<String> type, String rawExpression) implements OoStatementDto {
        public OoMutableVariableStatementDto {
            type = type == null ? Optional.empty() : type;
        }
    }

    public record OoAssignmentStatementDto(String name, String rawExpression) implements OoStatementDto {
    }

    public record OoExpressionStatementDto(String rawExpression) implements OoStatementDto {
    }

    public record OoThrowStatementDto(String rawExpression) implements OoStatementDto {
    }

    public record OoReturnStatementDto(String rawExpression) implements OoStatementDto {
    }

    public record OoIfStatementDto(
            String rawCondition,
            OoStatementBlockDto thenBranch,
            Optional<OoStatementDto> elseBranch
    ) implements OoStatementDto {
        public OoIfStatementDto {
            elseBranch = elseBranch == null ? Optional.empty() : elseBranch;
        }
    }

    public record OoTryCatchStatementDto(OoStatementBlockDto tryBlock, List<OoCatchClauseDto> catches) implements OoStatementDto {
        public OoTryCatchStatementDto {
            catches = List.copyOf(catches);
        }
    }

    public record OoCatchClauseDto(String name, OoStatementBlockDto body) {
    }

    public record OoWhileStatementDto(String rawCondition, OoStatementBlockDto body) implements OoStatementDto {
    }

    public record OoDoWhileStatementDto(OoStatementBlockDto body, String rawCondition) implements OoStatementDto {
    }

    public record OoForEachStatementDto(
            String name,
            Optional<String> type,
            String rawIterable,
            OoStatementBlockDto body
    ) implements OoStatementDto {
        public OoForEachStatementDto {
            type = type == null ? Optional.empty() : type;
        }
    }

    public record OoParameterDto(String name, String type) {
    }

    public record OoTypeReferenceDto(String name) {
    }
}
