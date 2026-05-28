package dev.capylang.compiler.ir;

import dev.capylang.compiler.parser.facade.ParserSchema;

import java.util.List;
import java.util.Optional;

public final class CoreIrSchema {
    private CoreIrSchema() {
    }

    public enum PrimitiveTypeDto {
        BYTE, INT, LONG, DOUBLE, STRING, BOOL, FLOAT, ANY, DATA, ENUM, NOTHING
    }

    public enum VisibilityDto {
        LOCAL, PRIVATE
    }

    public enum NativeProviderBackendDto {
        JAVA, JAVASCRIPT, PYTHON
    }

    public enum CompiledObjectKindDto {
        CLASS, ABSTRACT_CLASS, TRAIT, INTERFACE
    }

    public record ModuleIdDto(String name, String path) {
    }

    public record SourcePositionDto(int line, int column, Optional<Integer> length) {
        public SourcePositionDto {
            length = length == null ? Optional.empty() : length;
        }
    }

    public record CompiledProgramDto(
            List<CompiledModuleDto> modules,
            List<ParserSchema.ObjectOrientedModuleDto> objectOrientedModules,
            NativeProviderManifestDto nativeProviders,
            NativeProviderCatalogDto nativeProviderCatalog
    ) {
        public CompiledProgramDto {
            modules = List.copyOf(modules);
            objectOrientedModules = List.copyOf(objectOrientedModules);
        }
    }

    public record CompiledModuleDto(
            ModuleIdDto module,
            List<CompiledTypeEntryDto> types,
            List<CompiledPrimitiveBackedTypeDto> visiblePrimitiveBackedTypes,
            List<CompiledFunctionDto> functions,
            List<StaticImportDto> staticImports
    ) {
        public CompiledModuleDto {
            types = List.copyOf(types);
            visiblePrimitiveBackedTypes = List.copyOf(visiblePrimitiveBackedTypes);
            functions = List.copyOf(functions);
            staticImports = List.copyOf(staticImports);
        }
    }

    public record StaticImportDto(String className, String memberName, boolean enumValue) {
    }

    public record CompiledTypeEntryDto(String name, GenericCompiledTypeDto type) {
    }

    public sealed interface CompiledTypeDto permits
            PrimitiveCompiledTypeDto,
            CompiledListTypeDto,
            CompiledSetTypeDto,
            CompiledDictTypeDto,
            CompiledGenericTypeParameterDto,
            CompiledFunctionTypeDto,
            CompiledTupleTypeDto,
            GenericCompiledTypeDto {
    }

    public sealed interface GenericCompiledTypeDto extends CompiledTypeDto permits
            CompiledDataTypeDto,
            CompiledDataParentTypeDto,
            CompiledObjectTypeDto,
            CompiledPrimitiveBackedTypeDto {
    }

    public record PrimitiveCompiledTypeDto(PrimitiveTypeDto value) implements CompiledTypeDto {
    }

    public record CompiledListTypeDto(CompiledTypeDto elementType) implements CompiledTypeDto {
    }

    public record CompiledSetTypeDto(CompiledTypeDto elementType) implements CompiledTypeDto {
    }

    public record CompiledDictTypeDto(CompiledTypeDto valueType) implements CompiledTypeDto {
    }

    public record CompiledGenericTypeParameterDto(String name) implements CompiledTypeDto {
    }

    public record CompiledFunctionTypeDto(CompiledTypeDto argumentType, CompiledTypeDto returnType) implements CompiledTypeDto {
    }

    public record CompiledTupleTypeDto(List<CompiledTypeDto> elementTypes) implements CompiledTypeDto {
        public CompiledTupleTypeDto {
            elementTypes = List.copyOf(elementTypes);
        }
    }

    public record CompiledFieldDto(String name, CompiledTypeDto type, List<CompiledAnnotationDto> annotations) {
        public CompiledFieldDto {
            annotations = List.copyOf(annotations);
        }
    }

    public record CompiledDataTypeDto(
            String name,
            List<CompiledFieldDto> fields,
            List<String> typeParameters,
            List<String> extendedTypes,
            List<String> comments,
            Optional<VisibilityDto> visibility,
            boolean singleton,
            boolean nativeType,
            boolean enumValue,
            List<CompiledAnnotationDto> annotations
    ) implements GenericCompiledTypeDto {
        public CompiledDataTypeDto {
            fields = List.copyOf(fields);
            typeParameters = List.copyOf(typeParameters);
            extendedTypes = List.copyOf(extendedTypes);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            annotations = List.copyOf(annotations);
        }
    }

    public record CompiledDataParentTypeDto(
            String name,
            List<CompiledFieldDto> fields,
            List<CompiledDataTypeDto> subTypes,
            List<String> typeParameters,
            List<String> comments,
            Optional<VisibilityDto> visibility,
            boolean enumType,
            List<CompiledAnnotationDto> annotations
    ) implements GenericCompiledTypeDto {
        public CompiledDataParentTypeDto {
            fields = List.copyOf(fields);
            subTypes = List.copyOf(subTypes);
            typeParameters = List.copyOf(typeParameters);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            annotations = List.copyOf(annotations);
        }
    }

    public record BackendMethodNameDto(NativeProviderBackendDto backend, String name) {
    }

    public record CompiledObjectMethodParameterDto(String name, String type) {
    }

    public record CompiledObjectMethodDto(
            String name,
            List<CompiledObjectMethodParameterDto> parameters,
            String returnType,
            List<BackendMethodNameDto> backendMethodNames
    ) {
        public CompiledObjectMethodDto {
            parameters = List.copyOf(parameters);
            backendMethodNames = List.copyOf(backendMethodNames);
        }
    }

    public record CompiledObjectTypeDto(
            String name,
            String backendClassName,
            List<String> parents,
            Optional<VisibilityDto> visibility,
            List<CompiledAnnotationDto> annotations,
            CompiledObjectKindDto kind,
            List<CompiledObjectMethodDto> methods
    ) implements GenericCompiledTypeDto {
        public CompiledObjectTypeDto {
            parents = List.copyOf(parents);
            visibility = visibility == null ? Optional.empty() : visibility;
            annotations = List.copyOf(annotations);
            methods = List.copyOf(methods);
        }
    }

    public record CompiledPrimitiveBackedTypeDto(
            String name,
            PrimitiveTypeDto backingType,
            String cfunType,
            List<String> comments,
            Optional<VisibilityDto> visibility,
            List<CompiledAnnotationDto> annotations
    ) implements GenericCompiledTypeDto {
        public CompiledPrimitiveBackedTypeDto {
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            annotations = List.copyOf(annotations);
        }
    }

    public record CompiledFunctionParameterDto(String name, CompiledTypeDto type) {
    }

    public record CompiledFunctionDto(
            String name,
            CompiledTypeDto returnType,
            List<CompiledFunctionParameterDto> parameters,
            CompiledExpressionDto expression,
            List<String> comments,
            Optional<VisibilityDto> visibility,
            boolean programMain,
            boolean recursive,
            boolean tailRecursive,
            List<CompiledAnnotationDto> annotations
    ) {
        public CompiledFunctionDto {
            parameters = List.copyOf(parameters);
            comments = List.copyOf(comments);
            visibility = visibility == null ? Optional.empty() : visibility;
            annotations = List.copyOf(annotations);
        }
    }

    public sealed interface CompiledExpressionDto permits
            CompiledBoolExpressionDto,
            CompiledIntExpressionDto,
            CompiledStringExpressionDto,
            CompiledVariableExpressionDto,
            CompiledFunctionCallExpressionDto,
            CompiledInfixExpressionDto,
            CompiledNewDataExpressionDto,
            CompiledNothingExpressionDto {
    }

    public record CompiledBoolExpressionDto(boolean value) implements CompiledExpressionDto {
    }

    public record CompiledIntExpressionDto(String value) implements CompiledExpressionDto {
    }

    public record CompiledStringExpressionDto(String value) implements CompiledExpressionDto {
    }

    public record CompiledVariableExpressionDto(String name, CompiledTypeDto type, boolean emptyShapeCompatible) implements CompiledExpressionDto {
    }

    public record CompiledFunctionCallExpressionDto(String name, List<CompiledExpressionDto> arguments, CompiledTypeDto returnType) implements CompiledExpressionDto {
        public CompiledFunctionCallExpressionDto {
            arguments = List.copyOf(arguments);
        }
    }

    public record CompiledInfixExpressionDto(CompiledExpressionDto left, String operator, CompiledExpressionDto right, CompiledTypeDto type) implements CompiledExpressionDto {
    }

    public record CompiledNewDataFieldAssignmentDto(String name, CompiledExpressionDto value) {
    }

    public record CompiledNewDataExpressionDto(CompiledTypeDto type, List<CompiledNewDataFieldAssignmentDto> assignments) implements CompiledExpressionDto {
        public CompiledNewDataExpressionDto {
            assignments = List.copyOf(assignments);
        }
    }

    public record CompiledNothingExpressionDto(Optional<SourcePositionDto> position, String message) implements CompiledExpressionDto {
        public CompiledNothingExpressionDto {
            position = position == null ? Optional.empty() : position;
        }
    }

    public record CompiledAnnotationDto(
            String name,
            String packageName,
            String packagePath,
            List<CompiledAnnotationArgumentDto> arguments
    ) {
        public CompiledAnnotationDto {
            arguments = List.copyOf(arguments);
        }
    }

    public record CompiledAnnotationArgumentDto(String name, CompiledAnnotationValueDto value) {
    }

    public sealed interface CompiledAnnotationValueDto permits
            CompiledStringAnnotationValueDto,
            CompiledIntAnnotationValueDto,
            CompiledLongAnnotationValueDto,
            CompiledFloatAnnotationValueDto,
            CompiledDoubleAnnotationValueDto,
            CompiledBoolAnnotationValueDto,
            CompiledNothingAnnotationValueDto,
            CompiledTypeNameAnnotationValueDto {
    }

    public record CompiledStringAnnotationValueDto(String value) implements CompiledAnnotationValueDto {
    }

    public record CompiledIntAnnotationValueDto(String value) implements CompiledAnnotationValueDto {
    }

    public record CompiledLongAnnotationValueDto(String value) implements CompiledAnnotationValueDto {
    }

    public record CompiledFloatAnnotationValueDto(String value) implements CompiledAnnotationValueDto {
    }

    public record CompiledDoubleAnnotationValueDto(String value) implements CompiledAnnotationValueDto {
    }

    public record CompiledBoolAnnotationValueDto(boolean value) implements CompiledAnnotationValueDto {
    }

    public record CompiledNothingAnnotationValueDto() implements CompiledAnnotationValueDto {
    }

    public record CompiledTypeNameAnnotationValueDto(String name) implements CompiledAnnotationValueDto {
    }

    public record NativeProviderManifestDto(List<NativeProviderBindingDto> providers, Optional<String> sourceFile) {
        public NativeProviderManifestDto {
            providers = List.copyOf(providers);
            sourceFile = sourceFile == null ? Optional.empty() : sourceFile;
        }
    }

    public record NativeProviderCatalogDto(
            List<CompiledNativeProviderDeclarationDto> declarations,
            List<CompiledNativeProviderBindingDto> bindings
    ) {
        public NativeProviderCatalogDto {
            declarations = List.copyOf(declarations);
            bindings = List.copyOf(bindings);
        }
    }

    public record NativeProviderBindingDto(String interfaceId, String qualifier, List<NativeProviderBackendBindingDto> bindings) {
        public NativeProviderBindingDto {
            bindings = List.copyOf(bindings);
        }
    }

    public record CompiledNativeProviderBindingDto(String interfaceId, String qualifier, List<NativeProviderBackendBindingDto> bindings) {
        public CompiledNativeProviderBindingDto {
            bindings = List.copyOf(bindings);
        }
    }

    public record CompiledNativeProviderDeclarationDto(
            String providerName,
            String sourceModulePath,
            String sourceModuleName,
            String targetTypeName,
            String interfaceId,
            String qualifier,
            String lifetime,
            String sourceFile
    ) {
    }

    public sealed interface NativeProviderBackendBindingDto permits
            JavaNativeProviderBindingDto,
            JavascriptNativeProviderBindingDto,
            PythonNativeProviderBindingDto {
    }

    public record JavaNativeProviderBindingDto(String className, Optional<String> factory) implements NativeProviderBackendBindingDto {
        public JavaNativeProviderBindingDto {
            factory = factory == null ? Optional.empty() : factory;
        }
    }

    public record JavascriptNativeProviderBindingDto(String moduleName, String exportName, Optional<String> factory) implements NativeProviderBackendBindingDto {
        public JavascriptNativeProviderBindingDto {
            factory = factory == null ? Optional.empty() : factory;
        }
    }

    public record PythonNativeProviderBindingDto(String moduleName, String className, Optional<String> factory) implements NativeProviderBackendBindingDto {
        public PythonNativeProviderBindingDto {
            factory = factory == null ? Optional.empty() : factory;
        }
    }

    public record GeneratedProgramDto(List<GeneratedModuleDto> modules) {
        public GeneratedProgramDto {
            modules = List.copyOf(modules);
        }
    }

    public record GeneratedModuleDto(String relativePath, String code) {
    }
}
