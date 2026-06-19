package dev.capylang.compiler;

import dev.capylang.compiler.parser.Definition;
import dev.capylang.compiler.parser.Definition.AnnotationDeclaration;
import dev.capylang.compiler.parser.Definition.AnnotationFieldDeclaration;
import dev.capylang.compiler.parser.Definition.ConstantDefinition;
import dev.capylang.compiler.parser.Definition.DataDeclaration;
import dev.capylang.compiler.parser.Definition.DataFieldDeclaration;
import dev.capylang.compiler.parser.Definition.DataParentDeclaration;
import dev.capylang.compiler.parser.Definition.DeriverDeclaration;
import dev.capylang.compiler.parser.Definition.EnumDeclaration;
import dev.capylang.compiler.parser.Definition.FunctionDefinition;
import dev.capylang.compiler.parser.Definition.PrimitiveBackedTypeDeclaration;
import dev.capylang.compiler.parser.Definition.TypeDeclaration;
import dev.capylang.compiler.parser.Expression;
import dev.capylang.compiler.parser.Expression.BinaryExpression;
import dev.capylang.compiler.parser.Expression.BlockExpression;
import dev.capylang.compiler.parser.Expression.DataField;
import dev.capylang.compiler.parser.Expression.DataLiteral;
import dev.capylang.compiler.parser.Expression.DictEntry;
import dev.capylang.compiler.parser.Expression.DictLiteral;
import dev.capylang.compiler.parser.Expression.FieldAccessExpression;
import dev.capylang.compiler.parser.Expression.FunctionCallExpression;
import dev.capylang.compiler.parser.Expression.IfExpression;
import dev.capylang.compiler.parser.Expression.IndexExpression;
import dev.capylang.compiler.parser.Expression.LambdaExpression;
import dev.capylang.compiler.parser.Expression.LetBinding;
import dev.capylang.compiler.parser.Expression.ListLiteral;
import dev.capylang.compiler.parser.Expression.MatchCase;
import dev.capylang.compiler.parser.Expression.MatchExpression;
import dev.capylang.compiler.parser.Expression.MethodCallExpression;
import dev.capylang.compiler.parser.Expression.ReduceExpression;
import dev.capylang.compiler.parser.Expression.SetLiteral;
import dev.capylang.compiler.parser.Expression.TupleLiteral;
import dev.capylang.compiler.parser.Expression.ThrowExpression;
import dev.capylang.compiler.parser.Expression.TryCatchExpression;
import dev.capylang.compiler.parser.Expression.UnaryExpression;
import dev.capylang.compiler.parser.Expression.UnsupportedExpression;
import dev.capylang.compiler.parser.Expression.WithExpression;
import dev.capylang.compiler.parser.FunctionAnnotationApplication;
import dev.capylang.compiler.parser.FunctionAnnotationArgument;
import dev.capylang.compiler.parser.FunctionAnnotationValue.FunctionAnnotationStringValue;
import dev.capylang.compiler.parser.FunctionDeclaration;
import dev.capylang.compiler.parser.FunctionParameter;
import dev.capylang.compiler.parser.ImportDeclaration;
import dev.capylang.compiler.parser.ObjectOrientedClass;
import dev.capylang.compiler.parser.ObjectOrientedField;
import dev.capylang.compiler.parser.ObjectOrientedInitBlock;
import dev.capylang.compiler.parser.ObjectOrientedInterface;
import dev.capylang.compiler.parser.ObjectOrientedMethod;
import dev.capylang.compiler.parser.ParsedModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.parser.SourceLocation;
import dev.capylang.compiler.parser.TypeReference;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class NativeCompilerValidator {
    private static final Set<String> BUILTIN_TYPES = Set.of(
            "byte", "char", "int", "long", "double", "bool", "float", "void", "any", "data",
            "nothing", "String", "List", "Set", "Dict", "Tuple", "Option", "Result", "Either",
            "Effect", "Program", "Assert", "TestFile", "TestCase", "Seq", "Regex", "Match",
            "Path", "Ordering", "size", "index"
    );

    public List<CompilerError> validate(
            List<ParsedModule> modules,
            List<String> libraryModules,
            NativeProviderManifest nativeProviders
    ) {
        var context = new Context(modules, libraryModules);
        var errors = new ArrayList<CompilerError>();
        validateImports(context, errors);
        validateDefinitions(context, errors);
        validateObjectOriented(context, errors);
        validateNativeProviderManifest(nativeProviders, errors);
        return List.copyOf(errors);
    }

    private void validateImports(Context context, List<CompilerError> errors) {
        for (var module : context.modules) {
            for (var declaration : module.imports()) {
                if (!context.moduleExists(declaration.modulePath())) {
                    errors.add(error(module, declaration.location(), "Module `" + module.name() + "` imports unknown module `" + declaration.modulePath() + "`."));
                    continue;
                }
                if (declaration.wildcard()) {
                    for (var excluded : declaration.excludedNames()) {
                        if (!context.symbolExists(declaration.modulePath(), excluded)) {
                            errors.add(error(module, declaration.location(), "Module `" + declaration.modulePath() + "` does not export `" + excluded + "`."));
                        }
                    }
                    continue;
                }
                if (declaration.qualified()) {
                    continue;
                }
                for (var name : declaration.importedNames()) {
                    if (!context.symbolExists(declaration.modulePath(), name)) {
                        errors.add(error(module, declaration.location(), "Module `" + declaration.modulePath() + "` does not export `" + name + "`."));
                    }
                }
            }
        }
    }

    private void validateDefinitions(Context context, List<CompilerError> errors) {
        var nativeProviderKeys = new LinkedHashSet<String>();
        for (var module : context.modules) {
            var symbols = new HashSet<String>();
            for (var definition : module.definitions()) {
                var key = symbolKey(definition);
                if (!key.isBlank() && !symbols.add(key)) {
                    errors.add(error(module, location(definition), "Duplicate declaration `" + displaySymbolKey(key) + "`."));
                }
                validateDefinition(context, module, definition, errors, nativeProviderKeys);
            }
        }
    }

    private void validateDefinition(
            Context context,
            ParsedModule module,
            Definition definition,
            List<CompilerError> errors,
            Set<String> nativeProviderKeys
    ) {
        switch (definition) {
            case AnnotationDeclaration annotation -> {
                validateDuplicateAnnotationFields(module, annotation, errors);
                for (var field : annotation.fields()) {
                    validateTypeReference(context, module, field.typeReference(), List.of(), errors, field.location());
                }
            }
            case ConstantDefinition constant -> {
                validateTypeReference(context, module, constant.constant().typeReference(), List.of(), errors, constant.constant().location());
                validateExpression(context, module, constant.constant().expression(), errors);
            }
            case DataDeclaration data -> {
                validateDuplicateDataFields(module, data.fields(), errors);
                for (var field : data.fields()) {
                    validateTypeReference(context, module, field.typeReference(), data.parameters(), errors, field.location());
                }
                for (var parent : data.parents()) {
                    validateTypeReference(context, module, parent.typeReference(), data.parameters(), errors, parent.location());
                }
            }
            case DeriverDeclaration deriver -> {
                for (var method : deriver.methods()) {
                    validateFunction(context, module, method, errors, nativeProviderKeys);
                }
            }
            case EnumDeclaration ignored -> {
            }
            case FunctionDefinition function -> validateFunction(context, module, function.function(), errors, nativeProviderKeys);
            case PrimitiveBackedTypeDeclaration primitive -> validateTypeReference(context, module, primitive.backingType(), List.of(), errors, primitive.location());
            case TypeDeclaration type -> {
                validateDuplicateDataFields(module, type.fields(), errors);
                for (var field : type.fields()) {
                    validateTypeReference(context, module, field.typeReference(), type.parameters(), errors, field.location());
                }
                for (var variant : type.variants()) {
                    validateTypeReference(context, module, variant, type.parameters(), errors, type.location());
                }
            }
            default -> {
            }
        }
    }

    private void validateFunction(
            Context context,
            ParsedModule module,
            FunctionDeclaration function,
            List<CompilerError> errors,
            Set<String> nativeProviderKeys
    ) {
        validateTypeReference(context, module, function.returnType(), List.of(), errors, function.location());
        for (var parameter : function.parameters()) {
            validateTypeReference(context, module, parameter.typeReference(), List.of(), errors, parameter.location());
        }
        validateFunctionAnnotations(context, module, function, errors, nativeProviderKeys);
        validateExpression(context, module, function.body(), errors);
    }

    private void validateFunctionAnnotations(
            Context context,
            ParsedModule module,
            FunctionDeclaration function,
            List<CompilerError> errors,
            Set<String> nativeProviderKeys
    ) {
        var availableAnnotations = context.availableAnnotations(module);
        for (var annotation : function.annotations()) {
            if (isStandardNativeProvider(annotation, module)) {
                validateNativeProvider(module, function, annotation, errors, nativeProviderKeys);
                continue;
            }
            var declaration = availableAnnotations.get(unqualified(annotation.name()));
            if (declaration == null) {
                if ("NativeProvider".equals(unqualified(annotation.name())) && !standardNativeProviderImported(module)) {
                    errors.add(error(module, annotation.location(), "Unknown annotation " + annotation.name() + "."));
                } else {
                    errors.add(error(module, annotation.location(), "Unknown annotation " + annotation.name() + "."));
                }
                continue;
            }
            if (!declaration.targets().contains("fun")) {
                errors.add(error(module, annotation.location(), "Annotation " + annotation.name() + " cannot target functions."));
            }
            validateAnnotationArguments(module, annotation, declaration, errors);
        }
    }

    private void validateAnnotationArguments(
            ParsedModule module,
            FunctionAnnotationApplication usage,
            AnnotationDeclaration declaration,
            List<CompilerError> errors
    ) {
        var fields = new HashMap<String, AnnotationFieldDeclaration>();
        for (var field : declaration.fields()) {
            fields.put(field.name(), field);
        }
        var seen = new HashSet<String>();
        for (var argument : usage.arguments()) {
            if (!seen.add(argument.name())) {
                errors.add(error(module, argument.location(), "Duplicate annotation argument " + argument.name() + "."));
            }
            if (!fields.containsKey(argument.name())) {
                errors.add(error(module, argument.location(), "Unknown annotation argument " + argument.name() + "."));
            }
        }
        for (var field : declaration.fields()) {
            if (!field.hasDefault() && !seen.contains(field.name())) {
                errors.add(error(module, usage.location(), "Missing required annotation argument " + field.name() + "."));
            }
        }
    }

    private void validateNativeProvider(
            ParsedModule module,
            FunctionDeclaration function,
            FunctionAnnotationApplication annotation,
            List<CompilerError> errors,
            Set<String> nativeProviderKeys
    ) {
        if (!"Effect".equals(unqualified(function.returnType().name())) || function.returnType().arguments().size() != 1) {
            errors.add(error(module, function.location(), "NativeProvider function must return Effect[Interface]."));
        } else {
            var target = function.returnType().arguments().get(0);
            if (isBuiltinOrCompositeType(target)) {
                errors.add(error(module, function.location(), "NativeProvider target must be an object-oriented interface type."));
            }
            var key = nativeProviderInterfaceId(target) + "|" + qualifier(annotation);
            if (!nativeProviderKeys.add(key)) {
                errors.add(error(module, function.location(), "Duplicate native provider declaration for `" + nativeProviderInterfaceId(target) + "` qualifier `" + qualifier(annotation) + "`."));
            }
        }
        if (!(function.body() instanceof UnsupportedExpression unsupported) || !"<native>".equals(unsupported.source())) {
            errors.add(error(module, function.location(), "NativeProvider function body must be <native>."));
        }
        for (var argument : annotation.arguments()) {
            if ("qualifier".equals(argument.name()) && !(argument.value() instanceof FunctionAnnotationStringValue)) {
                errors.add(error(module, argument.location(), "NativeProvider qualifier must be a String."));
            }
        }
    }

    private void validateNativeProviderManifest(NativeProviderManifest manifest, List<CompilerError> errors) {
        var seen = new HashSet<String>();
        for (var binding : manifest.providers()) {
            var key = binding.interfaceId() + "|" + binding.qualifier();
            if (!seen.add(key)) {
                errors.add(globalError("Duplicate native provider binding for `" + binding.interfaceId() + "` qualifier `" + binding.qualifier() + "`."));
            }
            if (binding.interfaceId().isBlank()) {
                errors.add(globalError("Native provider binding interfaceId must not be empty."));
            }
            if (binding.javaBinding().isEmpty() && binding.javascriptBinding().isEmpty() && binding.pythonBinding().isEmpty()) {
                errors.add(globalError("Native provider binding `" + binding.interfaceId() + "` must define at least one backend binding."));
            }
        }
    }

    private void validateTypeReference(
            Context context,
            ParsedModule module,
            TypeReference type,
            List<String> typeParameters,
            List<CompilerError> errors,
            SourceLocation location
    ) {
        var name = unqualified(type.name());
        if (!knownType(context, module, name, typeParameters)) {
            errors.add(error(module, location, "Data type `" + type.name() + "` not found."));
        }
        for (var argument : type.arguments()) {
            validateTypeReference(context, module, argument, typeParameters, errors, location);
        }
    }

    private boolean knownType(Context context, ParsedModule module, String name, List<String> typeParameters) {
        return name.isBlank()
                || functionTypeName(name)
                || BUILTIN_TYPES.contains(name)
                || typeParameters.contains(name)
                || isSingleLetterGeneric(name)
                || context.typeExists(name)
                || context.moduleHasType(module, name)
                || context.importedTypeExists(module, name);
    }

    private boolean functionTypeName(String name) {
        return name.contains("=>") || name.startsWith("(");
    }

    private void validateExpression(Context context, ParsedModule module, Expression expression, List<CompilerError> errors) {
        switch (expression) {
            case BinaryExpression binary -> {
                validateExpression(context, module, binary.left(), errors);
                validateExpression(context, module, binary.right(), errors);
            }
            case BlockExpression block -> {
                for (var binding : block.bindings()) {
                    validateExpression(context, module, binding.value(), errors);
                }
                validateExpression(context, module, block.result(), errors);
            }
            case DataLiteral literal -> {
                validateDuplicateDataLiteralFields(module, literal, errors);
                for (var field : literal.fields()) {
                    validateExpression(context, module, field.value(), errors);
                }
            }
            case DictLiteral literal -> {
                for (var entry : literal.entries()) {
                    validateExpression(context, module, entry.key(), errors);
                    validateExpression(context, module, entry.value(), errors);
                }
            }
            case FieldAccessExpression access -> validateExpression(context, module, access.receiver(), errors);
            case FunctionCallExpression call -> {
                for (var argument : call.arguments()) {
                    validateExpression(context, module, argument, errors);
                }
            }
            case IfExpression ifExpression -> {
                validateExpression(context, module, ifExpression.condition(), errors);
                validateExpression(context, module, ifExpression.thenBranch(), errors);
                validateExpression(context, module, ifExpression.elseBranch(), errors);
            }
            case IndexExpression index -> {
                validateExpression(context, module, index.receiver(), errors);
                validateExpression(context, module, index.index(), errors);
                if (index.hasEndIndex()) {
                    validateExpression(context, module, index.endIndex(), errors);
                }
            }
            case LambdaExpression lambda -> validateExpression(context, module, lambda.body(), errors);
            case ListLiteral literal -> literal.values().forEach(value -> validateExpression(context, module, value, errors));
            case MatchExpression match -> {
                validateExpression(context, module, match.value(), errors);
                for (var matchCase : match.cases()) {
                    if (matchCase.hasLiteral()) {
                        validateExpression(context, module, matchCase.literal(), errors);
                    }
                    if (matchCase.hasGuard()) {
                        validateExpression(context, module, matchCase.guard(), errors);
                    }
                    validateExpression(context, module, matchCase.body(), errors);
                }
            }
            case MethodCallExpression call -> {
                validateExpression(context, module, call.receiver(), errors);
                call.arguments().forEach(argument -> validateExpression(context, module, argument, errors));
            }
            case ReduceExpression reduce -> {
                validateExpression(context, module, reduce.receiver(), errors);
                validateExpression(context, module, reduce.initial(), errors);
                validateExpression(context, module, reduce.body(), errors);
            }
            case SetLiteral literal -> literal.values().forEach(value -> validateExpression(context, module, value, errors));
            case TupleLiteral literal -> literal.values().forEach(value -> validateExpression(context, module, value, errors));
            case UnaryExpression unary -> validateExpression(context, module, unary.expression(), errors);
            case WithExpression with -> {
                validateExpression(context, module, with.receiver(), errors);
                with.fields().forEach(field -> validateExpression(context, module, field.value(), errors));
            }
            default -> {
            }
        }
    }

    private boolean knownFunction(Context context, ParsedModule module, String name) {
        if (context.moduleHasFunctionOrConstant(module, name)) {
            return true;
        }
        for (var declaration : module.imports()) {
            if (declaration.qualified()) {
                continue;
            }
            if (declaration.wildcard() && context.moduleExists(declaration.modulePath()) && !declaration.excludedNames().contains(name)) {
                return true;
            }
            if (declaration.importedNames().contains(name) && context.symbolExists(declaration.modulePath(), name)) {
                return true;
            }
        }
        return false;
    }

    private void validateObjectOriented(Context context, List<CompilerError> errors) {
        for (var module : context.modules) {
            for (var objectInterface : module.objectOriented().interfaces()) {
                for (var method : objectInterface.methods()) {
                    validateObjectExpression(module, method.body(), errors);
                }
            }
            for (var objectClass : module.objectOriented().classes()) {
                for (var field : objectClass.fields()) {
                    if (field.hasValue()) {
                        validateObjectExpression(module, field.value(), errors);
                    }
                }
                for (var initBlock : objectClass.initBlocks()) {
                    validateObjectExpression(module, initBlock.body(), errors);
                }
                for (var method : objectClass.methods()) {
                    validateObjectExpression(module, method.body(), errors);
                }
            }
        }
    }

    private void validateObjectExpression(ParsedModule module, Expression expression, List<CompilerError> errors) {
        switch (expression) {
            case UnsupportedExpression unsupported -> {
                if (unsupported.location().line() != 0 || unsupported.location().column() != 0) {
                    errors.add(error(module, unsupported.location(), "Unsupported object-oriented construct: `" + unsupported.source() + "`."));
                }
            }
            case BinaryExpression binary -> {
                validateObjectExpression(module, binary.left(), errors);
                validateObjectExpression(module, binary.right(), errors);
            }
            case BlockExpression block -> {
                for (var binding : block.bindings()) {
                    validateObjectExpression(module, binding.value(), errors);
                }
                validateObjectExpression(module, block.result(), errors);
            }
            case DataLiteral literal -> literal.fields().forEach(field -> validateObjectExpression(module, field.value(), errors));
            case DictLiteral literal -> literal.entries().forEach(entry -> {
                validateObjectExpression(module, entry.key(), errors);
                validateObjectExpression(module, entry.value(), errors);
            });
            case FieldAccessExpression access -> validateObjectExpression(module, access.receiver(), errors);
            case FunctionCallExpression call -> call.arguments().forEach(argument -> validateObjectExpression(module, argument, errors));
            case IfExpression ifExpression -> {
                validateObjectExpression(module, ifExpression.condition(), errors);
                validateObjectExpression(module, ifExpression.thenBranch(), errors);
                validateObjectExpression(module, ifExpression.elseBranch(), errors);
            }
            case IndexExpression index -> {
                validateObjectExpression(module, index.receiver(), errors);
                validateObjectExpression(module, index.index(), errors);
                if (index.hasEndIndex()) {
                    validateObjectExpression(module, index.endIndex(), errors);
                }
            }
            case LambdaExpression lambda -> validateObjectExpression(module, lambda.body(), errors);
            case ListLiteral literal -> literal.values().forEach(value -> validateObjectExpression(module, value, errors));
            case MatchExpression match -> {
                validateObjectExpression(module, match.value(), errors);
                for (var matchCase : match.cases()) {
                    if (matchCase.hasLiteral()) {
                        validateObjectExpression(module, matchCase.literal(), errors);
                    }
                    if (matchCase.hasGuard()) {
                        validateObjectExpression(module, matchCase.guard(), errors);
                    }
                    validateObjectExpression(module, matchCase.body(), errors);
                }
            }
            case ThrowExpression throwExpression -> validateObjectExpression(module, throwExpression.value(), errors);
            case TryCatchExpression tryCatch -> {
                validateObjectExpression(module, tryCatch.body(), errors);
                validateObjectExpression(module, tryCatch.catchBody(), errors);
            }
            case MethodCallExpression call -> {
                validateObjectExpression(module, call.receiver(), errors);
                call.arguments().forEach(argument -> validateObjectExpression(module, argument, errors));
            }
            case ReduceExpression reduce -> {
                validateObjectExpression(module, reduce.receiver(), errors);
                validateObjectExpression(module, reduce.initial(), errors);
                validateObjectExpression(module, reduce.body(), errors);
            }
            case SetLiteral literal -> literal.values().forEach(value -> validateObjectExpression(module, value, errors));
            case TupleLiteral literal -> literal.values().forEach(value -> validateObjectExpression(module, value, errors));
            case UnaryExpression unary -> validateObjectExpression(module, unary.expression(), errors);
            case WithExpression with -> {
                validateObjectExpression(module, with.receiver(), errors);
                with.fields().forEach(field -> validateObjectExpression(module, field.value(), errors));
            }
            default -> {
            }
        }
    }

    private void validateDuplicateAnnotationFields(ParsedModule module, AnnotationDeclaration annotation, List<CompilerError> errors) {
        var seen = new HashSet<String>();
        for (var field : annotation.fields()) {
            if (!seen.add(field.name())) {
                errors.add(error(module, field.location(), "Duplicate annotation field `" + field.name() + "`."));
            }
        }
    }

    private void validateDuplicateDataFields(ParsedModule module, List<DataFieldDeclaration> fields, List<CompilerError> errors) {
        var seen = new HashSet<String>();
        for (var field : fields) {
            if (!seen.add(field.name())) {
                errors.add(error(module, field.location(), "Duplicate field `" + field.name() + "`."));
            }
        }
    }

    private void validateDuplicateDataLiteralFields(ParsedModule module, DataLiteral literal, List<CompilerError> errors) {
        var seen = new HashSet<String>();
        for (var field : literal.fields()) {
            if (!field.spread() && !seen.add(field.name())) {
                errors.add(error(module, field.location(), "Field `" + field.name() + "` is assigned more than once."));
            }
        }
    }

    private boolean isStandardNativeProvider(FunctionAnnotationApplication annotation, ParsedModule module) {
        return "NativeProvider".equals(unqualified(annotation.name())) && standardNativeProviderImported(module);
    }

    private boolean standardNativeProviderImported(ParsedModule module) {
        return importsName(module.imports(), "/capy/meta_prog/NativeProvider", "NativeProvider")
                || importsName(module.imports(), "capy/meta_prog/NativeProvider", "NativeProvider")
                || importsName(module.imports(), "NativeProvider", "NativeProvider");
    }

    private boolean importsName(List<ImportDeclaration> imports, String modulePath, String name) {
        var expected = normalizeImportModulePath(modulePath);
        for (var declaration : imports) {
            if (normalizeImportModulePath(declaration.modulePath()).equals(expected)) {
                return declaration.wildcard() || declaration.importedNames().contains(name);
            }
        }
        return false;
    }

    private String qualifier(FunctionAnnotationApplication annotation) {
        for (var argument : annotation.arguments()) {
            if ("qualifier".equals(argument.name()) && argument.value() instanceof FunctionAnnotationStringValue value) {
                return value.value();
            }
        }
        return "";
    }

    private boolean isBuiltinOrCompositeType(TypeReference type) {
        return BUILTIN_TYPES.contains(type.name())
                || "List".equals(type.name())
                || "Set".equals(type.name())
                || "Dict".equals(type.name())
                || "Tuple".equals(type.name())
                || "Effect".equals(type.name());
    }

    private String nativeProviderInterfaceId(TypeReference type) {
        if (type.name().startsWith("__capy_oo|")) {
            return type.name().substring("__capy_oo|".length());
        }
        return type.name();
    }

    private String symbolKey(Definition definition) {
        return switch (definition) {
            case AnnotationDeclaration annotation -> annotation.name();
            case ConstantDefinition constant -> constant.constant().name();
            case DataDeclaration data -> data.name();
            case DeriverDeclaration deriver -> deriver.name();
            case EnumDeclaration enumDeclaration -> enumDeclaration.name();
            case FunctionDefinition ignored -> "";
            case PrimitiveBackedTypeDeclaration primitive -> primitive.name();
            case TypeDeclaration type -> type.name();
            default -> "";
        };
    }

    private String displaySymbolKey(String key) {
        var slash = key.lastIndexOf('/');
        return slash < 0 ? key : key.substring(0, slash);
    }

    private SourceLocation location(Definition definition) {
        return switch (definition) {
            case AnnotationDeclaration annotation -> annotation.location();
            case ConstantDefinition constant -> constant.constant().location();
            case DataDeclaration data -> data.location();
            case DeriverDeclaration deriver -> deriver.location();
            case EnumDeclaration enumDeclaration -> enumDeclaration.location();
            case FunctionDefinition function -> function.function().location();
            case PrimitiveBackedTypeDeclaration primitive -> primitive.location();
            case TypeDeclaration type -> type.location();
            default -> new SourceLocation(0, 0);
        };
    }

    private CompilerError error(ParsedModule module, SourceLocation location, String message) {
        return new CompilerError(message, moduleFileName(module), location.line(), location.column());
    }

    private CompilerError globalError(String message) {
        return new CompilerError(message, "", 0, 0);
    }

    private String moduleFileName(ParsedModule module) {
        var path = normalizeModulePath(module.path());
        var file = module.name() + (module.sourceKind() == SourceKind.OBJECT_ORIENTED ? ".coo" : ".cfun");
        return path.isBlank() ? file : "/" + path + "/" + file;
    }

    private boolean isQualifiedName(String name) {
        return name.contains(".");
    }

    private boolean isConstructorLike(String name) {
        var member = unqualified(name);
        return !member.isEmpty() && Character.isUpperCase(member.charAt(0));
    }

    private boolean isSingleLetterGeneric(String name) {
        return name.length() == 1 && Character.isUpperCase(name.charAt(0));
    }

    private String unqualified(String name) {
        var dot = name.lastIndexOf('.');
        var slash = name.lastIndexOf('/');
        var index = Math.max(dot, slash);
        return index < 0 ? name : name.substring(index + 1);
    }

    private String normalizeModulePath(String path) {
        var normalized = path.replace('\\', '/');
        while (normalized.startsWith("/")) {
            normalized = normalized.substring(1);
        }
        return normalized;
    }

    private String normalizeImportModulePath(String path) {
        var normalized = normalizeModulePath(path);
        if (normalized.startsWith("dev/capylang/capybara/")) {
            return normalizeCapybaraImportSuffix(normalized.substring("dev/capylang/capybara/".length()), "dev/capylang/");
        }
        if (normalized.startsWith("capybara/")) {
            return normalizeCapybaraImportSuffix(normalized.substring("capybara/".length()), "");
        }
        return normalized;
    }

    private String normalizeCapybaraImportSuffix(String suffix, String prefix) {
        return suffix.startsWith("test/") ? prefix + suffix : prefix + "test/" + suffix;
    }

    private boolean stdlibImport(String modulePath) {
        var normalized = normalizeModulePath(modulePath);
        return normalized.startsWith("capy/");
    }

    private final class Context {
        private final List<ParsedModule> modules;
        private final Set<String> libraryModules;
        private final Map<ParsedModule, Set<String>> symbolsByModule = new LinkedHashMap<>();
        private final Map<ParsedModule, Set<String>> typesByModule = new LinkedHashMap<>();
        private final Map<ParsedModule, Map<String, AnnotationDeclaration>> annotationsByModule = new LinkedHashMap<>();

        private Context(List<ParsedModule> modules, List<String> libraryModules) {
            this.modules = modules;
            this.libraryModules = new LinkedHashSet<>(libraryModules);
            for (var module : modules) {
                symbolsByModule.put(module, parsedSymbols(module));
                typesByModule.put(module, parsedTypes(module));
                annotationsByModule.put(module, parsedAnnotations(module));
            }
        }

        private boolean moduleExists(String path) {
            if (stdlibImport(path)) {
                return true;
            }
            return parsedModule(path) != null || libraryModule(path);
        }

        private boolean symbolExists(String path, String name) {
            if (stdlibImport(path)) {
                return true;
            }
            var module = parsedModule(path);
            if (module != null) {
                return symbolsByModule.get(module).contains(name);
            }
            return libraryModule(path);
        }

        private boolean typeExists(String name) {
            for (var types : typesByModule.values()) {
                if (types.contains(name)) {
                    return true;
                }
            }
            return false;
        }

        private boolean moduleHasType(ParsedModule module, String name) {
            return typesByModule.get(module).contains(name);
        }

        private boolean importedTypeExists(ParsedModule module, String name) {
            for (var declaration : module.imports()) {
                if (declaration.qualified()) {
                    continue;
                }
                if (declaration.wildcard()) {
                    if (declaration.excludedNames().contains(name)) {
                        continue;
                    }
                    if (stdlibImport(declaration.modulePath()) || libraryModule(declaration.modulePath())) {
                        return true;
                    }
                    var imported = parsedModule(declaration.modulePath());
                    if (imported != null && typesByModule.get(imported).contains(name)) {
                        return true;
                    }
                    continue;
                }
                if (declaration.importedNames().contains(name) && symbolExists(declaration.modulePath(), name)) {
                    return true;
                }
            }
            return false;
        }

        private boolean moduleHasFunctionOrConstant(ParsedModule module, String name) {
            for (var definition : module.definitions()) {
                if (definition instanceof FunctionDefinition function && function.function().name().equals(name)) {
                    return true;
                }
                if (definition instanceof ConstantDefinition constant && constant.constant().name().equals(name)) {
                    return true;
                }
            }
            return false;
        }

        private Map<String, AnnotationDeclaration> availableAnnotations(ParsedModule module) {
            var annotations = new LinkedHashMap<>(annotationsByModule.get(module));
            for (var declaration : module.imports()) {
                if (declaration.qualified()) {
                    continue;
                }
                var imported = parsedModule(declaration.modulePath());
                if (imported == null) {
                    continue;
                }
                var importedAnnotations = annotationsByModule.get(imported);
                if (declaration.wildcard()) {
                    importedAnnotations.forEach((name, annotation) -> {
                        if (!declaration.excludedNames().contains(name)) {
                            annotations.put(name, annotation);
                        }
                    });
                } else {
                    for (var name : declaration.importedNames()) {
                        var annotation = importedAnnotations.get(name);
                        if (annotation != null) {
                            annotations.put(name, annotation);
                        }
                    }
                }
            }
            return annotations;
        }

        private ParsedModule parsedModule(String path) {
            for (var module : modules) {
                if (parsedModuleMatches(path, module)) {
                    return module;
                }
            }
            return null;
        }

        private boolean libraryModule(String path) {
            for (var library : libraryModules) {
                if (libraryMatches(path, library)) {
                    return true;
                }
            }
            return false;
        }

        private boolean parsedModuleMatches(String path, ParsedModule module) {
            var modulePath = parsedModulePath(module);
            return path.equals(module.name())
                    || normalizeModulePath(path).equals(modulePath)
                    || normalizeImportModulePath(path).equals(modulePath);
        }

        private String parsedModulePath(ParsedModule module) {
            var path = normalizeModulePath(module.path());
            return path.isBlank() ? module.name() : path + "/" + module.name();
        }

        private boolean libraryMatches(String path, String libraryPath) {
            return path.equals(libraryPath)
                    || normalizeModulePath(path).equals(libraryPath)
                    || normalizeImportModulePath(path).equals(libraryPath)
                    || path.equals(libraryPath.replace("/", "."));
        }

        private Set<String> parsedSymbols(ParsedModule module) {
            var symbols = new LinkedHashSet<String>();
            for (var definition : module.definitions()) {
                var name = switch (definition) {
                    case AnnotationDeclaration annotation -> annotation.name();
                    case ConstantDefinition constant -> constant.constant().name();
                    case DataDeclaration data -> data.name();
                    case DeriverDeclaration deriver -> deriver.name();
                    case EnumDeclaration enumDeclaration -> enumDeclaration.name();
                    case FunctionDefinition function -> function.function().name();
                    case PrimitiveBackedTypeDeclaration primitive -> primitive.name();
                    case TypeDeclaration type -> type.name();
                    default -> "";
                };
                if (!name.isBlank()) {
                    symbols.add(name);
                }
            }
            module.objectOriented().interfaces().forEach(type -> symbols.add(type.name()));
            module.objectOriented().classes().forEach(type -> symbols.add(type.name()));
            return symbols;
        }

        private Set<String> parsedTypes(ParsedModule module) {
            var types = new LinkedHashSet<String>();
            for (var definition : module.definitions()) {
                switch (definition) {
                    case AnnotationDeclaration annotation -> types.add(annotation.name());
                    case DataDeclaration data -> types.add(data.name());
                    case EnumDeclaration enumDeclaration -> types.add(enumDeclaration.name());
                    case PrimitiveBackedTypeDeclaration primitive -> types.add(primitive.name());
                    case TypeDeclaration type -> types.add(type.name());
                    default -> {
                    }
                }
            }
            module.objectOriented().interfaces().forEach(type -> types.add(type.name()));
            module.objectOriented().classes().forEach(type -> types.add(type.name()));
            return types;
        }

        private Map<String, AnnotationDeclaration> parsedAnnotations(ParsedModule module) {
            var annotations = new LinkedHashMap<String, AnnotationDeclaration>();
            for (var definition : module.definitions()) {
                if (definition instanceof AnnotationDeclaration annotation) {
                    annotations.put(annotation.name(), annotation);
                }
            }
            return annotations;
        }

    }
}
