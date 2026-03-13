package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.linker.LinkedFunction.LinkedFunctionParameter;
import pl.grzeslowski.capybara.linker.expression.CapybaraExpressionLinker;
import pl.grzeslowski.capybara.parser.*;

import java.util.*;
import java.util.stream.Stream;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;
import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class CapybaraLinker {
    public static final CapybaraLinker INSTANCE = new CapybaraLinker();
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final java.util.regex.Pattern IDENTIFIER_PATTERN = java.util.regex.Pattern.compile("[A-Za-z_][A-Za-z0-9_]*");

    public ValueOrError<LinkedProgram> link(Program program) {
        var modulesByName = program.modules().stream().collect(toMap(Module::name, identity(), (first, second) -> first));
        var moduleClassNameByModuleName = program.modules().stream()
                .collect(toMap(
                        Module::name,
                        module -> module.path().replace('/', '.').replace('\\', '.') + "." + module.name(),
                        (first, second) -> first
                ));

        var linkedTypesByModule = new HashMap<String, Map<String, GenericDataType>>();
        for (var module : program.modules()) {
            var sourceFile = moduleSourceFile(module);
            var linkedTypes = withFile(types(module), sourceFile);
            if (linkedTypes instanceof ValueOrError.Error<Map<String, GenericDataType>> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            linkedTypesByModule.put(module.name(), ((ValueOrError.Value<Map<String, GenericDataType>>) linkedTypes).value());
        }

        var visibleTypesByModule = new HashMap<String, Map<String, GenericDataType>>();
        for (var module : program.modules()) {
            var sourceFile = moduleSourceFile(module);
            var visibleTypes = withFile(availableTypes(module, modulesByName, linkedTypesByModule, program.modules()), sourceFile);
            if (visibleTypes instanceof ValueOrError.Error<Map<String, GenericDataType>> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            visibleTypesByModule.put(module.name(), ((ValueOrError.Value<Map<String, GenericDataType>>) visibleTypes).value());
        }

        var signaturesByModule = new HashMap<String, List<CapybaraExpressionLinker.FunctionSignature>>();
        for (var module : program.modules()) {
            var functions = findFunctions(module.functional().definitions());
            var sourceFile = moduleSourceFile(module);
            var signatures = withFile(linkFunctionSignatures(functions, visibleTypesByModule.get(module.name())), sourceFile);
            if (signatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            signaturesByModule.put(module.name(), ((ValueOrError.Value<List<CapybaraExpressionLinker.FunctionSignature>>) signatures).value());
        }

        var refinedSignaturesByModule = new HashMap<>(signaturesByModule);
        for (var module : program.modules()) {
            var firstPassFunctions = firstPassLinkedFunctions(
                    module,
                    modulesByName,
                    linkedTypesByModule,
                    visibleTypesByModule,
                    signaturesByModule,
                    moduleClassNameByModuleName
            );
            firstPassFunctions = withFile(firstPassFunctions, moduleSourceFile(module));
            if (firstPassFunctions instanceof ValueOrError.Error<List<LinkedFunction>> error) {
                return new ValueOrError.Error<>(error.errors());
            }
            var refined = mergeSignatures(
                    signaturesByModule.get(module.name()),
                    signaturesFromLinkedFunctions(((ValueOrError.Value<List<LinkedFunction>>) firstPassFunctions).value())
            );
            refinedSignaturesByModule.put(module.name(), refined);
        }

        return program.modules().stream()
                .map(module -> linkModule(
                        module,
                        modulesByName,
                        linkedTypesByModule,
                        visibleTypesByModule,
                        refinedSignaturesByModule,
                        moduleClassNameByModuleName
                ))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(LinkedProgram::new);
    }

    private ValueOrError<List<LinkedFunction>> firstPassLinkedFunctions(
            Module module,
            Map<String, Module> modulesByName,
            Map<String, Map<String, GenericDataType>> linkedTypesByModule,
            Map<String, Map<String, GenericDataType>> visibleTypesByModule,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName
    ) {
        var dataTypes = visibleTypesByModule.get(module.name());
        var localTypeNames = linkedTypesByModule.get(module.name()).keySet();
        var functions = findFunctions(module.functional().definitions());
        var moduleSourceFile = moduleSourceFile(module);
        var availableSignatures = availableSignatures(module, modulesByName, linkedTypesByModule, signaturesByModule);
        if (availableSignatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
            return withFile(new ValueOrError.Error<>(error.errors()), moduleSourceFile);
        }
        var initialSignatures = ((ValueOrError.Value<List<CapybaraExpressionLinker.FunctionSignature>>) availableSignatures).value();
        return withFile(linkFunctions(functions, dataTypes, localTypeNames, initialSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile), moduleSourceFile);
    }

    private ValueOrError<LinkedModule> linkModule(
            Module module,
            Map<String, Module> modulesByName,
            Map<String, Map<String, GenericDataType>> linkedTypesByModule,
            Map<String, Map<String, GenericDataType>> visibleTypesByModule,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName
    ) {
        var localTypes = linkedTypesByModule.get(module.name());
        var visibleTypes = visibleTypesByModule.get(module.name());
        var functions = findFunctions(module.functional().definitions());
        var moduleSourceFile = moduleSourceFile(module);
        var availableSignatures = availableSignatures(module, modulesByName, linkedTypesByModule, signaturesByModule);
        if (availableSignatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
            return withFile(new ValueOrError.Error<>(error.errors()), moduleSourceFile);
        }
        var initialSignatures = ((ValueOrError.Value<List<CapybaraExpressionLinker.FunctionSignature>>) availableSignatures).value();
        return withFile(linkFunctions(functions, visibleTypes, localTypes.keySet(), initialSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile)
                .flatMap(firstPassFunctions -> {
                    var refinedSignatures = mergeSignatures(
                            signaturesByModule.get(module.name()),
                            signaturesFromLinkedFunctions(firstPassFunctions)
                    );
                    var refinedAvailableSignatures = mergeSignatures(initialSignatures, refinedSignatures);
                    return linkFunctions(functions, visibleTypes, localTypes.keySet(), refinedAvailableSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile)
                            .map(linkedFunctions -> new LinkedModule(
                                    module.name(),
                                    module.path(),
                                    localTypes,
                                    deduplicateFunctions(linkedFunctions),
                                    staticImports(module, modulesByName, linkedTypesByModule, signaturesByModule)
                            ));
                }), moduleSourceFile);
    }

    private ValueOrError<List<CapybaraExpressionLinker.FunctionSignature>> availableSignatures(
            Module module,
            Map<String, Module> modulesByName,
            Map<String, Map<String, GenericDataType>> linkedTypesByModule,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule
    ) {
        var all = new ArrayList<CapybaraExpressionLinker.FunctionSignature>(signaturesByModule.get(module.name()));
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), modulesByName);
            if (importedModule == null) {
                return ValueOrError.error("Module `" + module.name() + "` imports unknown module `" + importDeclaration.moduleName() + "`");
            }
            var importedSignatures = signaturesByModule.get(importedModule.name());
            var availableFunctionMembers = importedSignatures.stream()
                    .map(CapybaraExpressionLinker.FunctionSignature::name)
                    .collect(java.util.stream.Collectors.toSet());
            var availableTypeMembers = linkedTypesByModule.get(importedModule.name()).keySet();
            var availableMembers = new HashSet<String>(availableFunctionMembers);
            availableMembers.addAll(availableTypeMembers);
            for (var excludedSymbol : importDeclaration.excludedSymbols()) {
                if (!availableMembers.contains(excludedSymbol)) {
                    return ValueOrError.error(
                            "Module `" + module.name() + "` excludes unknown symbol `" + excludedSymbol
                            + "` from module `" + importDeclaration.moduleName() + "`"
                    );
                }
            }
            if (!importDeclaration.isStarImport()) {
                for (var symbol : importDeclaration.symbols()) {
                    if (!availableMembers.contains(symbol)) {
                        return ValueOrError.error(
                                "Module `" + module.name() + "` imports unknown symbol `" + symbol
                                + "` from module `" + importDeclaration.moduleName() + "`"
                        );
                    }
                }
            }
            for (var symbol : importDeclaration.selectedSymbols(availableMembers)) {
                var matched = importedSignatures.stream().filter(signature -> signature.name().equals(symbol)).toList();
                all.addAll(matched);
            }
        }
        return ValueOrError.success(List.copyOf(all));
    }

    private ValueOrError<Map<String, GenericDataType>> availableTypes(
            Module module,
            Map<String, Module> modulesByName,
            Map<String, Map<String, GenericDataType>> linkedTypesByModule,
            List<Module> allModules
    ) {
        var localTypes = linkedTypesByModule.get(module.name());
        var all = new LinkedHashMap<String, GenericDataType>(localTypes);
        addQualifiedTypeAliases(all, module, localTypes);
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), modulesByName);
            if (importedModule == null) {
                return ValueOrError.error("Module `" + module.name() + "` imports unknown module `" + importDeclaration.moduleName() + "`");
            }
            var importedTypes = linkedTypesByModule.get(importedModule.name());
            addQualifiedTypeAliases(all, importedModule, importedTypes);
            if (importDeclaration.isStarImport() && importDeclaration.excludedSymbols().isEmpty()) {
                importedTypes.forEach(all::put);
                continue;
            }

            var selected = importDeclaration.selectedSymbols(importedTypes.keySet());
            for (var symbol : selected) {
                var imported = importedTypes.get(symbol);
                if (imported != null) {
                    all.put(symbol, imported);
                }
            }
        }
        allModules.forEach(knownModule -> addQualifiedTypeAliases(all, knownModule, linkedTypesByModule.get(knownModule.name())));
        resolveQualifiedExternalFieldTypes(all);
        return ValueOrError.success(Map.copyOf(all));
    }

    private void addQualifiedTypeAliases(Map<String, GenericDataType> all, Module module, Map<String, GenericDataType> importedTypes) {
        importedTypes.forEach((typeName, type) -> all.put(module.name() + "." + typeName, type));
        addPathQualifiedTypeAliases(all, module, importedTypes);
    }

    private void addPathQualifiedTypeAliases(Map<String, GenericDataType> all, Module module, Map<String, GenericDataType> importedTypes) {
        var modulePath = module.path().replace('\\', '/') + "/" + module.name();
        importedTypes.forEach((typeName, type) -> {
            all.put(modulePath + "." + typeName, type);
            all.put("/" + modulePath + "." + typeName, type);
        });
    }

    private void resolveQualifiedExternalFieldTypes(Map<String, GenericDataType> all) {
        var resolved = new HashMap<String, GenericDataType>();
        for (var entry : all.entrySet()) {
            resolved.put(entry.getKey(), resolveGenericDataType(entry.getValue(), all));
        }
        all.putAll(resolved);
    }

    private GenericDataType resolveGenericDataType(GenericDataType type, Map<String, GenericDataType> all) {
        return switch (type) {
            case LinkedDataType linkedDataType -> new LinkedDataType(
                    linkedDataType.name(),
                    linkedDataType.fields().stream()
                            .map(field -> new LinkedDataType.LinkedField(field.name(), resolveLinkedType(field.type(), all)))
                            .toList(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.singleton()
            );
            case LinkedDataParentType linkedDataParentType -> new LinkedDataParentType(
                    linkedDataParentType.name(),
                    linkedDataParentType.fields().stream()
                            .map(field -> new LinkedDataType.LinkedField(field.name(), resolveLinkedType(field.type(), all)))
                            .toList(),
                    linkedDataParentType.subTypes().stream()
                            .map(subType -> (LinkedDataType) resolveGenericDataType(subType, all))
                            .toList(),
                    linkedDataParentType.typeParameters()
            );
        };
    }

    private LinkedType resolveLinkedType(LinkedType type, Map<String, GenericDataType> all) {
        return switch (type) {
            case LinkedDataType linkedDataType -> resolveGenericDataType(linkedDataType, all);
            case LinkedDataParentType linkedDataParentType -> {
                if (isQualifiedExternalPlaceholder(linkedDataParentType)) {
                    var resolved = resolveQualifiedExternalType(all, linkedDataParentType.name());
                    if (resolved != null) {
                        yield withRequestedName(resolveGenericDataType(resolved, all), linkedDataParentType.name());
                    }
                }
                yield resolveGenericDataType(linkedDataParentType, all);
            }
            case CollectionLinkedType.LinkedList linkedList -> new CollectionLinkedType.LinkedList(resolveLinkedType(linkedList.elementType(), all));
            case CollectionLinkedType.LinkedSet linkedSet -> new CollectionLinkedType.LinkedSet(resolveLinkedType(linkedSet.elementType(), all));
            case CollectionLinkedType.LinkedDict linkedDict -> new CollectionLinkedType.LinkedDict(resolveLinkedType(linkedDict.valueType(), all));
            case LinkedTupleType linkedTupleType -> new LinkedTupleType(
                    linkedTupleType.elementTypes().stream().map(element -> resolveLinkedType(element, all)).toList()
            );
            case LinkedFunctionType linkedFunctionType -> new LinkedFunctionType(
                    resolveLinkedType(linkedFunctionType.argumentType(), all),
                    resolveLinkedType(linkedFunctionType.returnType(), all)
            );
            default -> type;
        };
    }

    private boolean isQualifiedExternalPlaceholder(LinkedDataParentType type) {
        return type.name().startsWith("/")
               && type.name().contains(".")
               && type.fields().isEmpty()
               && type.subTypes().isEmpty()
               && type.typeParameters().isEmpty();
    }

    private GenericDataType resolveQualifiedExternalType(Map<String, GenericDataType> all, String qualifiedTypeName) {
        var direct = all.get(qualifiedTypeName);
        if (direct != null) {
            return direct;
        }
        var genericStart = qualifiedTypeName.indexOf('[');
        if (genericStart > 0) {
            return all.get(qualifiedTypeName.substring(0, genericStart));
        }
        return null;
    }

    private GenericDataType withRequestedName(GenericDataType type, String requestedName) {
        return switch (type) {
            case LinkedDataType linkedDataType -> new LinkedDataType(
                    requestedName,
                    linkedDataType.fields(),
                    linkedDataType.typeParameters(),
                    linkedDataType.extendedTypes(),
                    linkedDataType.singleton()
            );
            case LinkedDataParentType linkedDataParentType -> new LinkedDataParentType(
                    requestedName,
                    linkedDataParentType.fields(),
                    linkedDataParentType.subTypes(),
                    linkedDataParentType.typeParameters()
            );
        };
    }

    private Set<LinkedModule.StaticImport> staticImports(
            Module module,
            Map<String, Module> modulesByName,
            Map<String, Map<String, GenericDataType>> linkedTypesByModule,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule
    ) {
        var imports = new HashSet<LinkedModule.StaticImport>();
        for (var importDeclaration : module.imports()) {
            var importedModule = resolveImportedModule(importDeclaration.moduleName(), modulesByName);
            if (importedModule == null) {
                continue;
            }
            var className = importedModule.path().replace('/', '.').replace('\\', '.') + "." + importedModule.name();
            if (importDeclaration.isStarImport() && importDeclaration.excludedSymbols().isEmpty()) {
                imports.add(new LinkedModule.StaticImport(className, "*"));
                continue;
            }
            var availableFunctionMembers = signaturesByModule.get(importedModule.name()).stream()
                    .map(CapybaraExpressionLinker.FunctionSignature::name)
                    .collect(java.util.stream.Collectors.toSet());
            var availableTypeMembers = new HashSet<>(linkedTypesByModule.get(importedModule.name()).keySet());
            var availableMembers = new HashSet<String>(availableFunctionMembers);
            availableMembers.addAll(availableTypeMembers);
            for (var symbol : importDeclaration.selectedSymbols(availableMembers)) {
                if (availableMembers.contains(symbol)) {
                    imports.add(new LinkedModule.StaticImport(className, symbol));
                }
            }
        }
        return Set.copyOf(imports);
    }

    private Set<LinkedFunction> deduplicateFunctions(List<LinkedFunction> linkedFunctions) {
        var byKey = new LinkedHashMap<String, LinkedFunction>();
        for (var function : linkedFunctions) {
            var parameters = function.parameters().stream().map(parameter -> parameter.type().name()).toList();
            byKey.put(function.name() + "#" + parameters, function);
        }
        return Set.copyOf(byKey.values());
    }

    private Module resolveImportedModule(String rawImportedModuleName, Map<String, Module> modulesByName) {
        var direct = modulesByName.get(rawImportedModuleName);
        if (direct != null) {
            return direct;
        }

        var normalized = rawImportedModuleName.replace('\\', '/');
        var slashIdx = normalized.lastIndexOf('/');
        if (slashIdx >= 0 && slashIdx < normalized.length() - 1) {
            var tail = normalized.substring(slashIdx + 1);
            var byTail = modulesByName.get(tail);
            if (byTail != null) {
                return byTail;
            }
        }

        var dotIdx = normalized.lastIndexOf('.');
        if (dotIdx >= 0 && dotIdx < normalized.length() - 1) {
            return modulesByName.get(normalized.substring(dotIdx + 1));
        }

        return null;
    }

    private List<CapybaraExpressionLinker.FunctionSignature> mergeSignatures(
            List<CapybaraExpressionLinker.FunctionSignature> first,
            List<CapybaraExpressionLinker.FunctionSignature> second
    ) {
        var merged = new LinkedHashMap<String, CapybaraExpressionLinker.FunctionSignature>();
        first.forEach(signature -> merged.put(signatureKey(signature), signature));
        second.forEach(signature -> merged.put(signatureKey(signature), signature));
        return List.copyOf(merged.values());
    }

    private String signatureKey(CapybaraExpressionLinker.FunctionSignature signature) {
        var parameters = signature.parameterTypes().stream().map(LinkedType::name).toList();
        return signature.name() + "#" + parameters;
    }

    private List<Function> findFunctions(Set<Definition> definitions) {
        return definitions.stream()
                .filter(Function.class::isInstance)
                .map(Function.class::cast)
                .sorted(Comparator
                        .comparingInt((Function function) -> function.position().map(pl.grzeslowski.capybara.parser.SourcePosition::line).orElse(Integer.MAX_VALUE))
                        .thenComparingInt(function -> function.position().map(pl.grzeslowski.capybara.parser.SourcePosition::column).orElse(Integer.MAX_VALUE)))
                .toList();
    }

    private ValueOrError<List<LinkedFunction>> linkFunctions(
            List<Function> functions,
            Map<String, GenericDataType> dataTypes,
            Set<String> localTypeNames,
            List<CapybaraExpressionLinker.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            String moduleSourceFile
    ) {
        return functions.stream()
                .map(f -> linkFunction(f, dataTypes, localTypeNames, signatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunction> linkFunction(
            Function function,
            Map<String, GenericDataType> dataTypes,
            Set<String> localTypeNames,
            List<CapybaraExpressionLinker.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            String moduleSourceFile
    ) {
        var privateTypeSignatureError = privateTypeEscapingFunctionSignatureError(function, moduleSourceFile);
        if (privateTypeSignatureError.isPresent()) {
            return privateTypeSignatureError.get();
        }
        var localMethodValidationError = validateTypeMethodDeclaredInLocalType(function, localTypeNames, moduleSourceFile);
        if (localMethodValidationError.isPresent()) {
            return localMethodValidationError.get();
        }
        var functionGenericTypeNames = functionGenericTypeNames(function, dataTypes);
        var linked = linkParameters(function.parameters(), dataTypes, functionGenericTypeNames)
                .flatMap(parameters -> new CapybaraExpressionLinker(
                        parameters,
                        dataTypes,
                        signatures,
                        signaturesByModule,
                        moduleClassNameByModuleName
                ).linkExpression(function.expression()).flatMap(ex -> function.returnType()
                        .map(type -> linkType(type, dataTypes, functionGenericTypeNames))
                        .orElseGet(() -> ValueOrError.success(ex.type()))
                        .flatMap(rtype -> validateFunctionReturnType(function, ex, rtype, moduleSourceFile)
                                .map(validatedExpression -> new LinkedFunction(
                                        function.name(),
                                        rtype,
                                        parameters,
                                        enrichNothing(coerceReturnExpression(validatedExpression, rtype), function.name(), moduleSourceFile),
                                        function.comments(),
                                        isProgramMain(function.name(), rtype, parameters)
                                )))));
        linked = normalizeInfixOperatorErrors(linked, function, moduleSourceFile);
        linked = normalizeMatchExhaustivenessErrors(linked, function, moduleSourceFile);
        linked = normalizeIntLiteralErrors(linked, function, moduleSourceFile);
        var normalizedFile = normalizeFile(moduleSourceFile);
        var fallbackPosition = returnExpressionPosition(function.expression()).or(() -> function.position());
        return withPosition(linked, fallbackPosition, normalizedFile);
    }

    private Optional<ValueOrError<LinkedFunction>> validateTypeMethodDeclaredInLocalType(
            Function function,
            Set<String> localTypeNames,
            String moduleSourceFile
    ) {
        var ownerType = methodOwnerType(function.name());
        if (ownerType.isEmpty() || localTypeNames.contains(ownerType.get())) {
            return Optional.empty();
        }
        var normalizedFile = normalizeFile(moduleSourceFile);
        var methodLine = methodDeclarationErrorLine(function);
        var methodColumn = methodDeclarationErrorColumn(function);
        var position = new SourcePosition(methodLine, methodColumn, Optional.empty());
        var functionPreview = formatFunctionHeader(function) + " =";
        var pointerIndent = methodDeclarationPointerIndent(function, functionPreview);
        var pointer = " ".repeat(Math.max(pointerIndent, 0))
                      + "^ Cannot declare method on external type `" + ownerType.get()
                      + "`. Type methods/infix operators must be declared in the module where the type is defined.";
        var message = "error: mismatched types\n"
                      + " --> " + normalizedFile + ":" + position.line() + ":" + position.column() + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        var error = new ValueOrError.Error.SingleError(
                position.line(),
                position.column(),
                normalizedFile,
                message
        );
        return Optional.of(new ValueOrError.Error<>(List.of(error)));
    }

    private Optional<String> methodOwnerType(String functionName) {
        if (!functionName.startsWith(METHOD_DECL_PREFIX)) {
            return Optional.empty();
        }
        var separatorIndex = functionName.indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0) {
            return Optional.empty();
        }
        return Optional.of(functionName.substring(METHOD_DECL_PREFIX.length(), separatorIndex));
    }

    private ValueOrError<LinkedFunction> normalizeInfixOperatorErrors(
            ValueOrError<LinkedFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof ValueOrError.Error<LinkedFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeInfixOperatorError(singleError, function, moduleSourceFile))
                .toList();
        return new ValueOrError.Error<>(transformed);
    }

    private ValueOrError.Error.SingleError normalizeInfixOperatorError(
            ValueOrError.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        var pattern = java.util.regex.Pattern.compile("Cannot apply `([^`]+)` to `([^`]+)` and `([^`]+)`");
        var matcher = pattern.matcher(error.message());
        if (!matcher.matches()) {
            return error;
        }
        var operator = matcher.group(1);
        var leftType = normalizeReportedTypeName(matcher.group(2));
        var rightType = normalizeReportedTypeName(matcher.group(3));
        var line = Math.max(error.line(), 1);
        var column = Math.max(error.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()));
        var pointer = " ".repeat(Math.max(column, 0))
                      + "^ `" + operator + "` operator is not defined for `" + leftType + " " + operator + " " + rightType + "`";
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new ValueOrError.Error.SingleError(line, column, file, message);
    }

    private ValueOrError<LinkedFunction> normalizeMatchExhaustivenessErrors(
            ValueOrError<LinkedFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof ValueOrError.Error<LinkedFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeMatchExhaustivenessError(singleError, function, moduleSourceFile))
                .toList();
        return new ValueOrError.Error<>(transformed);
    }

    private ValueOrError<LinkedFunction> normalizeIntLiteralErrors(
            ValueOrError<LinkedFunction> linked,
            Function function,
            String moduleSourceFile
    ) {
        if (!(linked instanceof ValueOrError.Error<LinkedFunction> error)) {
            return linked;
        }
        var transformed = error.errors().stream()
                .map(singleError -> normalizeIntLiteralError(singleError, function, moduleSourceFile))
                .toList();
        return new ValueOrError.Error<>(transformed);
    }

    private ValueOrError.Error.SingleError normalizeIntLiteralError(
            ValueOrError.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        if (!error.message().startsWith("Int literal out of range:")
            && !error.message().startsWith("Invalid int literal:")) {
            return error;
        }
        var line = Math.max(error.line(), 1);
        var messageColumn = Math.max(error.column(), 1);
        var reportedColumn = messageColumn + 4;
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = formatFunctionHeaderAndExpression(function, formatExpressionPreviewWithSpaces(function.expression()));
        var pointer = " ".repeat(Math.max(messageColumn, 0)) + "^ " + error.message();
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + messageColumn + "\n"
                      + functionPreview + "\n"
                      + pointer + "\n";
        return new ValueOrError.Error.SingleError(line, reportedColumn, file, message);
    }

    private ValueOrError.Error.SingleError normalizeMatchExhaustivenessError(
            ValueOrError.Error.SingleError error,
            Function function,
            String moduleSourceFile
    ) {
        if (!error.message().startsWith("`match` is not exhaustive.")) {
            return error;
        }
        var line = Math.max(error.line(), 1);
        var column = Math.max(error.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var header = formatFunctionHeader(function) + " =";
        var matchLine = "    " + formatMatchLine(function.expression());
        var pointer = " ".repeat(Math.max(column, 0)) + "^ " + error.message();
        var message = "error: mismatched types\n"
                      + " --> " + file + ":" + line + ":" + column + "\n"
                      + header + "\n"
                      + matchLine + "\n"
                      + pointer + "\n";
        return new ValueOrError.Error.SingleError(line, column, file, message);
    }

    private ValueOrError<pl.grzeslowski.capybara.linker.expression.LinkedExpression> validateFunctionReturnType(
            Function function,
            pl.grzeslowski.capybara.linker.expression.LinkedExpression expression,
            LinkedType declaredReturnType,
            String moduleSourceFile
    ) {
        if (isAssignableReturnType(declaredReturnType, expression.type())) {
            return ValueOrError.success(expression);
        }
        var returnExpression = terminalReturnExpression(function.expression());
        var position = returnExpressionPosition(function.expression()).or(() -> function.position()).orElse(SourcePosition.EMPTY);
        var line = Math.max(position.line(), 1);
        var column = Math.max(position.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = formatReturnTypeMismatchSourceLine(function, returnExpression, position, declaredReturnType);
        var pointer = " ".repeat(Math.max(column, 0))
                      + "^ expected `" + formatLinkedType(declaredReturnType)
                      + "`, found `" + formatLinkedType(expression.type()) + "`";
        return ValueOrError.error(
                "error: mismatched types\n"
                + " --> " + file + ":" + line + ":" + column + "\n"
                + functionPreview + "\n"
                + pointer + "\n"
        );
    }

    private String formatReturnTypeMismatchSourceLine(
            Function function,
            Expression returnExpression,
            SourcePosition position,
            LinkedType declaredReturnType
    ) {
        var expressionPosition = function.expression().position().orElse(SourcePosition.EMPTY);
        if (function.expression() instanceof LetExpression && expressionPosition.line() != position.line()) {
            return formatMultilineFunctionPreview(function, declaredReturnType);
        }
        return "fun " + function.name()
               + "(" + function.parameters().stream()
                .map(parameter -> parameter.name() + ": " + formatParserType(parameter.type()))
                .collect(java.util.stream.Collectors.joining(", "))
               + "): " + formatLinkedType(declaredReturnType)
               + " = " + formatExpressionPreview(function.expression());
    }

    private String formatFunctionHeaderAndExpression(Function function, String expressionPreview) {
        return formatFunctionHeader(function) + " = " + expressionPreview;
    }

    private String formatFunctionHeader(Function function) {
        var methodDeclaration = methodDeclarationInfo(function);
        var methodParameters = methodDeclaration
                .map(ignored -> function.parameters().stream().skip(1).toList())
                .orElse(function.parameters());
        var methodHeaderName = methodDeclaration
                .map(this::formatMethodDeclarationName)
                .orElse(function.name());
        var header = new StringBuilder("fun ")
                .append(methodHeaderName)
                .append("(")
                .append(methodParameters.stream()
                        .map(parameter -> parameter.name() + ": " + formatParserTypeInHeader(parameter.type()))
                        .collect(java.util.stream.Collectors.joining(", ")))
                .append(")");
        function.returnType().ifPresent(type -> header.append(": ").append(formatParserTypeInHeader(type)));
        return header.toString();
    }

    private int methodDeclarationErrorLine(Function function) {
        if (function.expression() instanceof MatchExpression matchExpression && !matchExpression.cases().isEmpty()) {
            return matchExpression.cases().getFirst().expression().position()
                    .map(SourcePosition::line)
                    .orElseGet(() -> function.position().map(SourcePosition::line).orElse(1));
        }
        return function.position().map(SourcePosition::line).orElse(1);
    }

    private int methodDeclarationErrorColumn(Function function) {
        var methodDeclaration = methodDeclarationInfo(function);
        if (methodDeclaration.isEmpty()) {
            return function.position().map(SourcePosition::column).orElse(1);
        }
        var header = formatFunctionHeader(function);
        var declaration = methodDeclaration.get();
        if (IDENTIFIER_PATTERN.matcher(declaration.methodName()).matches()) {
            var idx = header.indexOf("." + declaration.methodName());
            if (idx >= 0) {
                return idx + 3;
            }
        } else {
            var wrappedLiteral = "`" + declaration.methodName() + "`";
            var idx = header.indexOf(wrappedLiteral);
            if (idx >= 0) {
                return idx + 1;
            }
        }
        return function.position().map(SourcePosition::column).orElse(1);
    }

    private int methodDeclarationPointerIndent(Function function, String functionPreview) {
        var methodDeclaration = methodDeclarationInfo(function);
        if (methodDeclaration.isEmpty()) {
            return Math.max(function.position().map(SourcePosition::column).orElse(1) - 1, 0);
        }
        var methodName = methodDeclaration.get().methodName();
        var idx = functionPreview.indexOf(methodName);
        if (idx >= 0) {
            return idx;
        }
        return Math.max(function.position().map(SourcePosition::column).orElse(1) - 1, 0);
    }

    private String formatMethodDeclarationName(MethodDeclarationInfo methodDeclarationInfo) {
        var owner = methodDeclarationInfo.ownerName();
        var methodName = methodDeclarationInfo.methodName();
        var methodDisplay = IDENTIFIER_PATTERN.matcher(methodName).matches()
                ? methodName
                : "`" + methodName + "`";
        return owner + "." + methodDisplay;
    }

    private Optional<MethodDeclarationInfo> methodDeclarationInfo(Function function) {
        if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
            return Optional.empty();
        }
        var separatorIndex = function.name().indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0 || separatorIndex + 2 > function.name().length()) {
            return Optional.empty();
        }
        var ownerFromName = function.name().substring(METHOD_DECL_PREFIX.length(), separatorIndex);
        var methodName = function.name().substring(separatorIndex + 2);
        var ownerWithTypeParameters = function.parameters().stream()
                .findFirst()
                .map(Parameter::type)
                .map(this::formatParserType)
                .orElse(ownerFromName);
        return Optional.of(new MethodDeclarationInfo(ownerWithTypeParameters, methodName));
    }

    private String formatParserTypeInHeader(pl.grzeslowski.capybara.parser.Type type) {
        return switch (type) {
            case FunctionType functionType -> formatParserTypeInHeader(functionType.argumentType())
                                              + " -> "
                                              + formatParserTypeInHeader(functionType.returnType());
            default -> formatParserType(type);
        };
    }

    private record MethodDeclarationInfo(String ownerName, String methodName) {
    }

    private String formatMatchLine(pl.grzeslowski.capybara.parser.Expression expression) {
        if (expression instanceof pl.grzeslowski.capybara.parser.MatchExpression matchExpression) {
            return "match " + formatExpressionPreview(matchExpression.matchWith()) + " with";
        }
        return formatExpressionPreview(expression);
    }

    private String formatMultilineFunctionPreview(Function function, LinkedType declaredReturnType) {
        var builder = new StringBuilder();
        builder.append("  fun ")
                .append(function.name())
                .append("(")
                .append(function.parameters().stream()
                        .map(parameter -> parameter.name() + ": " + formatParserType(parameter.type()))
                        .collect(java.util.stream.Collectors.joining(", ")))
                .append("): ")
                .append(formatLinkedType(declaredReturnType))
                .append(" =\n");
        builder.append(formatMultilineExpression(function.expression(), 4));
        return builder.toString();
    }

    private String formatMultilineExpression(Expression expression, int indent) {
        return switch (expression) {
            case LetExpression letExpression -> {
                var builder = new StringBuilder();
                builder.append(" ".repeat(indent))
                        .append("let ")
                        .append(letExpression.name());
                letExpression.declaredType().ifPresent(type -> builder.append(": ").append(formatParserType(type)));
                builder.append(" = ");
                if (isMultilinePreviewExpression(letExpression.value())) {
                    builder.append(formatExpressionMultilineForLetValue(letExpression.value(), indent));
                } else {
                    builder.append(formatExpressionPreview(letExpression.value()));
                }
                builder.append('\n').append(formatMultilineExpression(letExpression.rest(), indent));
                yield builder.toString();
            }
            default -> " ".repeat(indent) + formatExpressionPreview(expression);
        };
    }

    private boolean isMultilinePreviewExpression(Expression expression) {
        return expression instanceof NewData;
    }

    private String formatExpressionMultilineForLetValue(Expression expression, int indent) {
        return switch (expression) {
            case NewData newData -> formatNewDataInlineInLet(newData, indent);
            default -> formatExpressionPreview(expression);
        };
    }

    private String formatExpressionMultilineBlock(Expression expression, int indent) {
        return switch (expression) {
            case NewData newData -> formatNewDataMultiline(newData, indent);
            default -> " ".repeat(indent) + formatExpressionPreview(expression);
        };
    }

    private String formatNewDataInlineInLet(NewData newData, int indent) {
        var typeName = formatParserType(newData.type());
        var builder = new StringBuilder();
        builder.append(typeName).append(" {");
        if (!newData.positionalArguments().isEmpty() && newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.positionalArguments().size(); i++) {
                builder.append(" ".repeat(indent + 4))
                        .append(formatExpressionPreview(newData.positionalArguments().get(i)));
                if (i < newData.positionalArguments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        if (!newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.assignments().size(); i++) {
                var assignment = newData.assignments().get(i);
                builder.append(" ".repeat(indent + 4))
                        .append(assignment.name())
                        .append(": ")
                        .append(formatExpressionPreview(assignment.value()));
                if (i < newData.assignments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        builder.append("}");
        return builder.toString();
    }

    private String formatNewDataMultiline(NewData newData, int indent) {
        var typeName = formatParserType(newData.type());
        var builder = new StringBuilder();
        builder.append(" ".repeat(indent)).append(typeName).append(" {");
        if (!newData.positionalArguments().isEmpty() && newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.positionalArguments().size(); i++) {
                builder.append(" ".repeat(indent + 4))
                        .append(formatExpressionPreview(newData.positionalArguments().get(i)));
                if (i < newData.positionalArguments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        if (!newData.assignments().isEmpty()) {
            builder.append('\n');
            for (var i = 0; i < newData.assignments().size(); i++) {
                var assignment = newData.assignments().get(i);
                builder.append(" ".repeat(indent + 4))
                        .append(assignment.name())
                        .append(": ")
                        .append(formatExpressionPreview(assignment.value()));
                if (i < newData.assignments().size() - 1) {
                    builder.append(",");
                }
                builder.append('\n');
            }
            builder.append(" ".repeat(indent)).append("}");
            return builder.toString();
        }
        builder.append("}");
        return builder.toString();
    }

    private Expression terminalReturnExpression(Expression expression) {
        return switch (expression) {
            case LetExpression letExpression -> terminalReturnExpression(letExpression.rest());
            default -> expression;
        };
    }

    private Optional<SourcePosition> returnExpressionPosition(Expression expression) {
        return switch (expression) {
            case LetExpression letExpression -> returnExpressionPosition(letExpression.rest()).or(() -> letExpression.position());
            default -> expression.position();
        };
    }

    private String formatParserType(pl.grzeslowski.capybara.parser.Type type) {
        return switch (type) {
            case PrimitiveType primitiveType -> primitiveType.name().toLowerCase(java.util.Locale.ROOT);
            case CollectionType.ListType listType -> "list[" + formatParserType(listType.elementType()) + "]";
            case CollectionType.SetType setType -> "set[" + formatParserType(setType.elementType()) + "]";
            case CollectionType.DictType dictType -> "dict[" + formatParserType(dictType.valueType()) + "]";
            case TupleType tupleType -> "tuple[" + tupleType.elementTypes().stream()
                    .map(this::formatParserType)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case FunctionType functionType -> formatParserType(functionType.argumentType()) + "->" + formatParserType(functionType.returnType());
            case DataType dataType -> dataType.name();
        };
    }

    private String formatExpressionPreview(pl.grzeslowski.capybara.parser.Expression expression) {
        return switch (expression) {
            case StringValue stringValue -> stringValue.stringValue();
            case IntValue intValue -> intValue.intValue();
            case LongValue longValue -> longValue.longValue();
            case FloatValue floatValue -> floatValue.floatValue();
            case DoubleValue doubleValue -> doubleValue.doubleValue();
            case ByteValue byteValue -> byteValue.byteValue();
            case BooleanValue booleanValue -> String.valueOf(booleanValue.value());
            case Value value -> value.name();
            case InfixExpression infixExpression -> formatExpressionPreview(infixExpression.left())
                                                   + previewOperator(infixExpression.operator().symbol())
                                                   + formatExpressionPreview(infixExpression.right());
            default -> expression.toString();
        };
    }

    private String formatExpressionPreviewWithSpaces(pl.grzeslowski.capybara.parser.Expression expression) {
        return switch (expression) {
            case InfixExpression infixExpression -> formatExpressionPreviewWithSpaces(infixExpression.left())
                                                   + " " + previewOperator(infixExpression.operator().symbol()) + " "
                                                   + formatExpressionPreviewWithSpaces(infixExpression.right());
            default -> formatExpressionPreview(expression);
        };
    }

    private String previewOperator(String operator) {
        if (operator != null && operator.length() >= 2 && operator.startsWith("\"") && operator.endsWith("\"")) {
            return operator.substring(1, operator.length() - 1);
        }
        return operator;
    }

    private String normalizeReportedTypeName(String typeName) {
        if (typeName.startsWith("LinkedList[elementType=") && typeName.endsWith("]")) {
            var inner = typeName.substring("LinkedList[elementType=".length(), typeName.length() - 1);
            return "list[" + normalizeReportedTypeName(inner) + "]";
        }
        if (typeName.startsWith("LinkedSet[elementType=") && typeName.endsWith("]")) {
            var inner = typeName.substring("LinkedSet[elementType=".length(), typeName.length() - 1);
            return "set[" + normalizeReportedTypeName(inner) + "]";
        }
        if (typeName.startsWith("LinkedDict[valueType=") && typeName.endsWith("]")) {
            var inner = typeName.substring("LinkedDict[valueType=".length(), typeName.length() - 1);
            return "dict[" + normalizeReportedTypeName(inner) + "]";
        }
        if (typeName.startsWith("LinkedGenericTypeParameter[name=") && typeName.endsWith("]")) {
            return typeName.substring("LinkedGenericTypeParameter[name=".length(), typeName.length() - 1);
        }
        if (typeName.startsWith("LinkedDataType[name=")) {
            var end = typeName.indexOf(',');
            if (end > "LinkedDataType[name=".length()) {
                return typeName.substring("LinkedDataType[name=".length(), end);
            }
        }
        if (typeName.startsWith("LinkedDataParentType[name=")) {
            var end = typeName.indexOf(',');
            if (end > "LinkedDataParentType[name=".length()) {
                return typeName.substring("LinkedDataParentType[name=".length(), end);
            }
        }
        return switch (typeName) {
            case "BOOL" -> "bool";
            case "BYTE" -> "byte";
            case "INT" -> "int";
            case "LONG" -> "long";
            case "FLOAT" -> "float";
            case "DOUBLE" -> "double";
            case "STRING" -> "string";
            case "ANY" -> "any";
            case "NOTHING" -> "nothing";
            default -> typeName;
        };
    }

    private String formatLinkedType(LinkedType type) {
        return switch (type) {
            case PrimitiveLinkedType primitiveType -> primitiveType.name().toLowerCase(java.util.Locale.ROOT);
            case CollectionLinkedType.LinkedList linkedList -> "list[" + formatLinkedType(linkedList.elementType()) + "]";
            case CollectionLinkedType.LinkedSet linkedSet -> "set[" + formatLinkedType(linkedSet.elementType()) + "]";
            case CollectionLinkedType.LinkedDict linkedDict -> "dict[" + formatLinkedType(linkedDict.valueType()) + "]";
            case LinkedTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(this::formatLinkedType)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case LinkedFunctionType linkedFunctionType -> formatLinkedType(linkedFunctionType.argumentType()) + "->" + formatLinkedType(linkedFunctionType.returnType());
            case LinkedDataType linkedDataType -> linkedDataType.name();
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.name();
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private boolean isAssignableReturnType(LinkedType expected, LinkedType actual) {
        if (expected == actual || expected.equals(actual)) {
            return true;
        }
        if (actual == PrimitiveLinkedType.NOTHING
            || actual == PrimitiveLinkedType.ANY
            || expected == PrimitiveLinkedType.ANY) {
            return true;
        }
        if (expected == PrimitiveLinkedType.DATA) {
            return actual instanceof GenericDataType || actual == PrimitiveLinkedType.DATA;
        }
        if (expected instanceof PrimitiveLinkedType expectedPrimitive
            && actual instanceof PrimitiveLinkedType actualPrimitive) {
            return isAssignablePrimitiveReturnType(expectedPrimitive, actualPrimitive);
        }
        if (expected instanceof CollectionLinkedType.LinkedList expectedList
            && actual instanceof CollectionLinkedType.LinkedList actualList) {
            return isAssignableReturnType(expectedList.elementType(), actualList.elementType());
        }
        if (expected instanceof CollectionLinkedType.LinkedSet expectedSet
            && actual instanceof CollectionLinkedType.LinkedSet actualSet) {
            return isAssignableReturnType(expectedSet.elementType(), actualSet.elementType());
        }
        if (expected instanceof CollectionLinkedType.LinkedDict expectedDict
            && actual instanceof CollectionLinkedType.LinkedDict actualDict) {
            return isAssignableReturnType(expectedDict.valueType(), actualDict.valueType());
        }
        if (expected instanceof LinkedTupleType expectedTuple
            && actual instanceof LinkedTupleType actualTuple) {
            if (expectedTuple.elementTypes().size() != actualTuple.elementTypes().size()) {
                return false;
            }
            for (int i = 0; i < expectedTuple.elementTypes().size(); i++) {
                if (!isAssignableReturnType(expectedTuple.elementTypes().get(i), actualTuple.elementTypes().get(i))) {
                    return false;
                }
            }
            return true;
        }
        if (expected instanceof LinkedFunctionType expectedFunction
            && actual instanceof LinkedFunctionType actualFunction) {
            return isAssignableReturnType(expectedFunction.argumentType(), actualFunction.argumentType())
                   && isAssignableReturnType(expectedFunction.returnType(), actualFunction.returnType());
        }
        if (expected instanceof LinkedDataParentType expectedParent) {
            if (actual instanceof LinkedDataParentType actualParent) {
                return sameTypeName(expectedParent.name(), actualParent.name());
            }
            if (actual instanceof LinkedDataType actualData) {
                if (sameTypeName(expectedParent.name(), actualData.name())) {
                    return true;
                }
                return expectedParent.subTypes().stream()
                        .anyMatch(subType -> sameTypeName(subType.name(), actualData.name()));
            }
        }
        if (expected instanceof LinkedDataType expectedData) {
            if (actual instanceof LinkedDataType actualData) {
                return sameTypeName(expectedData.name(), actualData.name());
            }
            if (actual instanceof LinkedDataParentType actualParent) {
                return sameTypeName(expectedData.name(), actualParent.name());
            }
        }
        if (expected instanceof LinkedGenericTypeParameter) {
            return true;
        }
        return false;
    }

    private boolean isAssignablePrimitiveReturnType(PrimitiveLinkedType expected, PrimitiveLinkedType actual) {
        if (expected == actual) {
            return true;
        }
        if (actual == PrimitiveLinkedType.NOTHING) {
            return true;
        }
        if (expected == PrimitiveLinkedType.ANY) {
            return true;
        }
        if (expected == PrimitiveLinkedType.DATA || actual == PrimitiveLinkedType.DATA) {
            return false;
        }
        if (expected == PrimitiveLinkedType.BOOL || actual == PrimitiveLinkedType.BOOL) {
            return false;
        }
        if (expected == PrimitiveLinkedType.STRING || actual == PrimitiveLinkedType.STRING) {
            return false;
        }
        return numericRank(actual) <= numericRank(expected);
    }

    private int numericRank(PrimitiveLinkedType type) {
        return switch (type) {
            case BYTE -> 0;
            case INT -> 1;
            case LONG -> 2;
            case FLOAT -> 3;
            case DOUBLE -> 4;
            default -> Integer.MAX_VALUE;
        };
    }

    private boolean sameTypeName(String left, String right) {
        return normalizeTypeName(left).equals(normalizeTypeName(right));
    }

    private String normalizeTypeName(String name) {
        var raw = name;
        var genericStart = raw.indexOf('[');
        if (genericStart > 0) {
            raw = raw.substring(0, genericStart);
        }
        var slashIdx = raw.lastIndexOf('/');
        if (slashIdx >= 0 && slashIdx < raw.length() - 1) {
            raw = raw.substring(slashIdx + 1);
        }
        var dotIdx = raw.lastIndexOf('.');
        if (dotIdx >= 0 && dotIdx < raw.length() - 1) {
            raw = raw.substring(dotIdx + 1);
        }
        return raw;
    }

    private pl.grzeslowski.capybara.linker.expression.LinkedExpression coerceReturnExpression(
            pl.grzeslowski.capybara.linker.expression.LinkedExpression expression,
            LinkedType returnType
    ) {
        if (returnType instanceof CollectionLinkedType.LinkedDict dictType
            && expression instanceof pl.grzeslowski.capybara.linker.expression.LinkedNewSet linkedNewSet
            && linkedNewSet.values().isEmpty()) {
            return new pl.grzeslowski.capybara.linker.expression.LinkedNewDict(
                    List.of(),
                    new CollectionLinkedType.LinkedDict(dictType.valueType())
            );
        }
        return expression;
    }

    private boolean isProgramMain(String name, LinkedType returnType, List<LinkedFunctionParameter> parameters) {
        if (!"main".equals(name)) {
            return false;
        }
        if (parameters.size() != 1) {
            return false;
        }
        if (!(parameters.getFirst().type() instanceof CollectionLinkedType.LinkedList listType)
            || listType.elementType() != PrimitiveLinkedType.STRING) {
            return false;
        }
        if (!(returnType instanceof GenericDataType genericDataType)) {
            return false;
        }
        var normalized = normalizeQualifiedTypeName(genericDataType.name());
        return "Program".equals(genericDataType.name())
               || normalized.equals("/capy/lang/Program")
               || normalized.equals("/capy/lang/Program.Program")
               || normalized.equals("/cap/lang/Program")
               || normalized.equals("/cap/lang/Program.Program");
    }

    private String normalizeQualifiedTypeName(String typeName) {
        var normalized = typeName.replace('\\', '/');
        if (!normalized.startsWith("/")) {
            normalized = "/" + normalized;
        }
        return normalized;
    }

    private String moduleSourceFile(Module module) {
        return module.path().replace('\\', '/') + "/" + module.name() + ".cfun";
    }

    private pl.grzeslowski.capybara.linker.expression.LinkedExpression enrichNothing(
            pl.grzeslowski.capybara.linker.expression.LinkedExpression expression,
            String functionName,
            String moduleSourceFile
    ) {
        return switch (expression) {
            case pl.grzeslowski.capybara.linker.expression.LinkedBooleanValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedByteValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedDoubleValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedFieldAccess value -> new pl.grzeslowski.capybara.linker.expression.LinkedFieldAccess(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.field(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedFloatValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedFunctionCall value -> new pl.grzeslowski.capybara.linker.expression.LinkedFunctionCall(
                    value.name(),
                    value.arguments().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                    value.returnType()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedFunctionInvoke value -> new pl.grzeslowski.capybara.linker.expression.LinkedFunctionInvoke(
                    enrichNothing(value.function(), functionName, moduleSourceFile),
                    value.arguments().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                    value.returnType()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedIfExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedIfExpression(
                    enrichNothing(value.condition(), functionName, moduleSourceFile),
                    enrichNothing(value.thenBranch(), functionName, moduleSourceFile),
                    enrichNothing(value.elseBranch(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedIndexExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedIndexExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    enrichNothing(value.index(), functionName, moduleSourceFile),
                    value.elementType(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedInfixExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedInfixExpression(
                    enrichNothing(value.left(), functionName, moduleSourceFile),
                    value.operator(),
                    enrichNothing(value.right(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedIntValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedLambdaExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedLambdaExpression(
                    value.argumentName(),
                    enrichNothing(value.expression(), functionName, moduleSourceFile),
                    value.functionType()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedLetExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedLetExpression(
                    value.name(),
                    enrichNothing(value.value(), functionName, moduleSourceFile),
                    enrichNothing(value.rest(), functionName, moduleSourceFile)
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedLongValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedMatchExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedMatchExpression(
                    enrichNothing(value.matchWith(), functionName, moduleSourceFile),
                    value.cases().stream().map(matchCase -> new pl.grzeslowski.capybara.linker.expression.LinkedMatchExpression.MatchCase(
                            matchCase.pattern(),
                            enrichNothing(matchCase.expression(), functionName, moduleSourceFile)
                    )).toList(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedNothingValue value -> {
                var line = value.position().map(pl.grzeslowski.capybara.parser.SourcePosition::line).orElse(-1);
                var column = value.position().map(pl.grzeslowski.capybara.parser.SourcePosition::column).orElse(-1);
                var normalizedFile = moduleSourceFile.startsWith("/") ? moduleSourceFile : "/" + moduleSourceFile;
                var message = "line " + line + ", column " + column + ", file " + normalizedFile
                              + ": the function `" + functionName + "` is not yet implemented";
                yield new pl.grzeslowski.capybara.linker.expression.LinkedNothingValue(value.position(), message);
            }
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeAllExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedPipeAllExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.argumentName(),
                    enrichNothing(value.predicate(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeAnyExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedPipeAnyExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.argumentName(),
                    enrichNothing(value.predicate(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeFlatMapExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedPipeFlatMapExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.argumentName(),
                    enrichNothing(value.mapper(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeFilterOutExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedPipeFilterOutExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.argumentName(),
                    enrichNothing(value.predicate(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedPipeExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.argumentName(),
                    enrichNothing(value.mapper(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedPipeReduceExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedPipeReduceExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    enrichNothing(value.initialValue(), functionName, moduleSourceFile),
                    value.accumulatorName(),
                    value.keyName(),
                    value.valueName(),
                    enrichNothing(value.reducerExpression(), functionName, moduleSourceFile),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedNewDict value -> new pl.grzeslowski.capybara.linker.expression.LinkedNewDict(
                    value.entries().stream().map(entry -> new pl.grzeslowski.capybara.linker.expression.LinkedNewDict.Entry(
                            enrichNothing(entry.key(), functionName, moduleSourceFile),
                            enrichNothing(entry.value(), functionName, moduleSourceFile)
                    )).toList(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedNewList value -> new pl.grzeslowski.capybara.linker.expression.LinkedNewList(
                    value.values().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedNewSet value -> new pl.grzeslowski.capybara.linker.expression.LinkedNewSet(
                    value.values().stream().map(argument -> enrichNothing(argument, functionName, moduleSourceFile)).toList(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedNewData value -> new pl.grzeslowski.capybara.linker.expression.LinkedNewData(
                    value.type(),
                    value.assignments().stream().map(assignment -> new pl.grzeslowski.capybara.linker.expression.LinkedNewData.FieldAssignment(
                            assignment.name(),
                            enrichNothing(assignment.value(), functionName, moduleSourceFile)
                    )).toList()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedSliceExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedSliceExpression(
                    enrichNothing(value.source(), functionName, moduleSourceFile),
                    value.start().map(v -> enrichNothing(v, functionName, moduleSourceFile)),
                    value.end().map(v -> enrichNothing(v, functionName, moduleSourceFile)),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedTupleExpression value -> new pl.grzeslowski.capybara.linker.expression.LinkedTupleExpression(
                    value.values().stream().map(v -> enrichNothing(v, functionName, moduleSourceFile)).toList(),
                    value.type()
            );
            case pl.grzeslowski.capybara.linker.expression.LinkedStringValue value -> value;
            case pl.grzeslowski.capybara.linker.expression.LinkedVariable value -> value;
        };
    }

    private ValueOrError<List<CapybaraExpressionLinker.FunctionSignature>> linkFunctionSignatures(
            List<Function> functions,
            Map<String, GenericDataType> dataTypes
    ) {
        return functions.stream()
                .map(function -> {
                            var functionGenericTypeNames = functionGenericTypeNames(function, dataTypes);
                            return linkParameters(function.parameters(), dataTypes, functionGenericTypeNames)
                                .flatMap(parameters -> function.returnType()
                                        .map(type -> linkType(type, dataTypes, functionGenericTypeNames))
                                        .orElseGet(() -> ValueOrError.success(PrimitiveLinkedType.ANY))
                                        .map(returnType -> new CapybaraExpressionLinker.FunctionSignature(
                                                function.name(),
                                                parameters.stream().map(LinkedFunctionParameter::type).toList(),
                                                returnType
                                        )));})
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private Optional<ValueOrError.Error<LinkedFunction>> privateTypeEscapingFunctionSignatureError(
            Function function,
            String moduleSourceFile
    ) {
        if (function.name().contains("__local_fun_")) {
            return Optional.empty();
        }

        for (int i = 0; i < function.parameters().size(); i++) {
            var parameter = function.parameters().get(i);
            var escaped = firstEscapedPrivateLocalType(parameter.type());
            if (escaped.isPresent()) {
                var line = function.position().map(SourcePosition::line).orElse(0);
                var column = signatureParameterTypeColumn(function, i);
                return Optional.of(new ValueOrError.Error<>(new ValueOrError.Error.SingleError(
                        line,
                        column,
                        "",
                        privateTypeEscapingFunctionSignatureMessage(
                                function,
                                moduleSourceFile,
                                line,
                                column,
                                escaped.get()
                        )
                )));
            }
        }

        if (function.returnType().isPresent()) {
            var escaped = firstEscapedPrivateLocalType(function.returnType().get());
            if (escaped.isPresent()) {
                var line = function.position().map(SourcePosition::line).orElse(0);
                var column = signatureReturnTypeColumn(function);
                return Optional.of(new ValueOrError.Error<>(new ValueOrError.Error.SingleError(
                        line,
                        column,
                        "",
                        privateTypeEscapingFunctionSignatureMessage(
                                function,
                                moduleSourceFile,
                                line,
                                column,
                                escaped.get()
                        )
                )));
            }
        }

        return Optional.empty();
    }

    private Optional<String> firstEscapedPrivateLocalType(pl.grzeslowski.capybara.parser.Type type) {
        return switch (type) {
            case DataType dataType -> dataType.name().contains("__local_type_")
                    ? Optional.of(dataType.name())
                    : Optional.empty();
            case CollectionType.ListType listType -> firstEscapedPrivateLocalType(listType.elementType());
            case CollectionType.SetType setType -> firstEscapedPrivateLocalType(setType.elementType());
            case CollectionType.DictType dictType -> firstEscapedPrivateLocalType(dictType.valueType());
            case TupleType tupleType -> tupleType.elementTypes().stream()
                    .map(this::firstEscapedPrivateLocalType)
                    .flatMap(Optional::stream)
                    .findFirst();
            case FunctionType functionType -> firstEscapedPrivateLocalType(functionType.argumentType())
                    .or(() -> firstEscapedPrivateLocalType(functionType.returnType()));
            default -> Optional.empty();
        };
    }

    private int signatureParameterTypeColumn(Function function, int parameterIndex) {
        var column = 4 + function.name().length() + 1;
        for (int i = 0; i < parameterIndex; i++) {
            if (i > 0) {
                column += 2;
            }
            var parameter = function.parameters().get(i);
            column += parameter.name().length() + 2 + formatParserTypeForPosition(parameter.type()).length();
        }
        if (parameterIndex > 0) {
            column += 2;
        }
        var targetParameter = function.parameters().get(parameterIndex);
        return column + targetParameter.name().length() + 2;
    }

    private int signatureReturnTypeColumn(Function function) {
        var column = 4 + function.name().length() + 1;
        for (int i = 0; i < function.parameters().size(); i++) {
            if (i > 0) {
                column += 2;
            }
            var parameter = function.parameters().get(i);
            column += parameter.name().length() + 2 + formatParserTypeForPosition(parameter.type()).length();
        }
        return column + 1 + 2;
    }

    private String formatParserTypeForPosition(pl.grzeslowski.capybara.parser.Type type) {
        return formatParserType(restorePrivateTypeNameForDisplay(type));
    }

    private pl.grzeslowski.capybara.parser.Type restorePrivateTypeNameForDisplay(pl.grzeslowski.capybara.parser.Type type) {
        return switch (type) {
            case DataType dataType -> new DataType(restorePrivateTypeNameForDisplay(dataType.name()));
            case CollectionType.ListType listType -> new CollectionType.ListType(
                    restorePrivateTypeNameForDisplay(listType.elementType())
            );
            case CollectionType.SetType setType -> new CollectionType.SetType(
                    restorePrivateTypeNameForDisplay(setType.elementType())
            );
            case CollectionType.DictType dictType -> new CollectionType.DictType(
                    restorePrivateTypeNameForDisplay(dictType.valueType())
            );
            case TupleType tupleType -> new TupleType(tupleType.elementTypes().stream()
                    .map(this::restorePrivateTypeNameForDisplay)
                    .toList());
            case FunctionType functionType -> new FunctionType(
                    restorePrivateTypeNameForDisplay(functionType.argumentType()),
                    restorePrivateTypeNameForDisplay(functionType.returnType())
            );
            default -> type;
        };
    }

    private String restorePrivateTypeNameForDisplay(String typeName) {
        var genericStart = typeName.indexOf('[');
        if (genericStart > 0 && typeName.endsWith("]")) {
            var base = typeName.substring(0, genericStart);
            var suffix = typeName.substring(genericStart);
            return toUserPrivateTypeName(base) + suffix;
        }
        return toUserPrivateTypeName(typeName);
    }

    private String toUserPrivateTypeName(String typeName) {
        var marker = "__local_type_";
        var idx = typeName.indexOf(marker);
        if (idx < 0) {
            return typeName;
        }
        var suffix = typeName.substring(idx + marker.length());
        var underscoreIdx = suffix.indexOf('_');
        if (underscoreIdx < 0 || underscoreIdx + 1 >= suffix.length()) {
            return typeName;
        }
        return "__" + suffix.substring(underscoreIdx + 1);
    }

    private String privateTypeEscapingFunctionSignatureMessage(
            Function function,
            String moduleSourceFile,
            int line,
            int column,
            String escapedType
    ) {
        var functionPreview = formatFunctionHeaderWithRestoredPrivateTypes(function) + " = ...";
        var pointer = " ".repeat(Math.max(column, 0))
                      + "^ Private type `" + toUserPrivateTypeName(escapedType) + "` cannot be used in function signature";
        return "error: mismatched types\n"
               + " --> " + normalizeFile(moduleSourceFile) + ":" + line + ":" + column + "\n"
               + functionPreview + "\n"
               + pointer + "\n";
    }

    private String formatFunctionHeaderWithRestoredPrivateTypes(Function function) {
        var header = new StringBuilder("fun ")
                .append(function.name())
                .append("(")
                .append(function.parameters().stream()
                        .map(parameter -> parameter.name() + ": " + formatParserTypeForPosition(parameter.type()))
                        .collect(java.util.stream.Collectors.joining(", ")))
                .append(")");
        function.returnType().ifPresent(type -> header.append(": ").append(formatParserTypeForPosition(type)));
        return header.toString();
    }

    private List<CapybaraExpressionLinker.FunctionSignature> signaturesFromLinkedFunctions(List<LinkedFunction> functions) {
        return functions.stream()
                .map(function -> new CapybaraExpressionLinker.FunctionSignature(
                        function.name(),
                        function.parameters().stream().map(LinkedFunctionParameter::type).toList(),
                        function.returnType()
                ))
                .toList();
    }

    private ValueOrError<List<LinkedFunctionParameter>> linkParameters(List<Parameter> parameters, Map<String, GenericDataType> dataTypes) {
        return linkParameters(parameters, dataTypes, Set.of());
    }

    private ValueOrError<List<LinkedFunctionParameter>> linkParameters(
            List<Parameter> parameters,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames
    ) {
        return parameters.stream()
                .map(p -> linkParameter(p, dataTypes, functionGenericTypeNames))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunctionParameter> linkParameter(Parameter parameter, Map<String, GenericDataType> dataTypes) {
        return linkParameter(parameter, dataTypes, Set.of());
    }

    private ValueOrError<LinkedFunctionParameter> linkParameter(
            Parameter parameter,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames
    ) {
        return withPosition(
                linkType(parameter.type(), dataTypes, functionGenericTypeNames)
                        .map(type -> new LinkedFunctionParameter(parameter.name(), type)),
                parameter.position(),
                "");
    }

    private ValueOrError<LinkedType> linkType(
            pl.grzeslowski.capybara.parser.Type type,
            Map<String, GenericDataType> dataTypes
    ) {
        return CapybaraTypeLinker.linkType(type, dataTypes);
    }

    private ValueOrError<LinkedType> linkType(
            pl.grzeslowski.capybara.parser.Type type,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames
    ) {
        if (functionGenericTypeNames.isEmpty()) {
            return linkType(type, dataTypes);
        }
        return switch (type) {
            case PrimitiveType primitiveType -> ValueOrError.success(switch (primitiveType) {
                case BYTE -> PrimitiveLinkedType.BYTE;
                case INT -> PrimitiveLinkedType.INT;
                case LONG -> PrimitiveLinkedType.LONG;
                case DOUBLE -> PrimitiveLinkedType.DOUBLE;
                case STRING -> PrimitiveLinkedType.STRING;
                case BOOL -> PrimitiveLinkedType.BOOL;
                case FLOAT -> PrimitiveLinkedType.FLOAT;
                case ANY -> PrimitiveLinkedType.ANY;
                case DATA -> PrimitiveLinkedType.DATA;
                case NOTHING -> PrimitiveLinkedType.NOTHING;
            });
            case CollectionType.ListType listType -> linkType(listType.elementType(), dataTypes, functionGenericTypeNames)
                    .map(CollectionLinkedType.LinkedList::new)
                    .map(LinkedType.class::cast);
            case CollectionType.SetType setType -> linkType(setType.elementType(), dataTypes, functionGenericTypeNames)
                    .map(CollectionLinkedType.LinkedSet::new)
                    .map(LinkedType.class::cast);
            case CollectionType.DictType dictType -> linkType(dictType.valueType(), dataTypes, functionGenericTypeNames)
                    .map(CollectionLinkedType.LinkedDict::new)
                    .map(LinkedType.class::cast);
            case FunctionType functionType -> linkType(functionType.argumentType(), dataTypes, functionGenericTypeNames)
                    .flatMap(argumentType -> linkType(functionType.returnType(), dataTypes, functionGenericTypeNames)
                            .map(returnType -> (LinkedType) new LinkedFunctionType(argumentType, returnType)));
            case TupleType tupleType -> tupleType.elementTypes().stream()
                    .map(elementType -> linkType(elementType, dataTypes, functionGenericTypeNames))
                    .collect(new ValueOrErrorCollectionCollector<>())
                    .map(linkedTypes -> (LinkedType) new LinkedTupleType(linkedTypes));
            case DataType dataType -> linkDataTypeWithFunctionGenerics(dataType.name(), dataTypes, functionGenericTypeNames);
        };
    }

    private ValueOrError<LinkedType> linkDataTypeWithFunctionGenerics(
            String rawTypeName,
            Map<String, GenericDataType> dataTypes,
            Set<String> functionGenericTypeNames
    ) {
        var parsed = parseGenericTypeName(rawTypeName);
        if (parsed.typeArguments().isEmpty() && functionGenericTypeNames.contains(parsed.baseName())) {
            return ValueOrError.success(new LinkedGenericTypeParameter(parsed.baseName()));
        }

        var linkedBase = linkType(new DataType(parsed.baseName()), dataTypes);
        if (linkedBase instanceof ValueOrError.Error<LinkedType> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var baseType = ((ValueOrError.Value<LinkedType>) linkedBase).value();
        if (parsed.typeArguments().isEmpty()) {
            return ValueOrError.success(baseType);
        }

        return parsed.typeArguments().stream()
                .map(argument -> linkType(parseTypeArgument(argument), dataTypes, functionGenericTypeNames))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(arguments -> instantiateTypeArguments(baseType, arguments));
    }

    private LinkedType instantiateTypeArguments(LinkedType linkedType, List<LinkedType> typeArguments) {
        var mappedTypeArguments = typeArguments.stream().map(this::typeDescriptor).toList();
        return switch (linkedType) {
            case LinkedDataParentType parentType -> new LinkedDataParentType(
                    parentType.name(),
                    parentType.fields(),
                    parentType.subTypes(),
                    mappedTypeArguments
            );
            case LinkedDataType dataType -> {
                if (dataType.typeParameters().isEmpty()) {
                    yield new LinkedDataType(
                            dataType.name(),
                            dataType.fields(),
                            mappedTypeArguments,
                            dataType.extendedTypes(),
                            dataType.singleton()
                    );
                }
                var substitutions = new LinkedHashMap<String, LinkedType>();
                var max = Math.min(dataType.typeParameters().size(), typeArguments.size());
                for (int i = 0; i < max; i++) {
                    substitutions.put(dataType.typeParameters().get(i), typeArguments.get(i));
                }
                var substitutedFields = dataType.fields().stream()
                        .map(field -> new LinkedDataType.LinkedField(field.name(), substituteTypeParameters(field.type(), substitutions)))
                        .toList();
                yield new LinkedDataType(
                        dataType.name(),
                        substitutedFields,
                        mappedTypeArguments,
                        dataType.extendedTypes(),
                        dataType.singleton()
                );
            }
            default -> linkedType;
        };
    }

    private LinkedType substituteTypeParameters(LinkedType type, Map<String, LinkedType> substitutions) {
        if (type instanceof LinkedGenericTypeParameter genericTypeParameter) {
            return substitutions.getOrDefault(genericTypeParameter.name(), type);
        }
        return switch (type) {
            case CollectionLinkedType.LinkedList linkedList -> new CollectionLinkedType.LinkedList(
                    substituteTypeParameters(linkedList.elementType(), substitutions));
            case CollectionLinkedType.LinkedSet linkedSet -> new CollectionLinkedType.LinkedSet(
                    substituteTypeParameters(linkedSet.elementType(), substitutions));
            case CollectionLinkedType.LinkedDict linkedDict -> new CollectionLinkedType.LinkedDict(
                    substituteTypeParameters(linkedDict.valueType(), substitutions));
            case LinkedFunctionType functionType -> new LinkedFunctionType(
                    substituteTypeParameters(functionType.argumentType(), substitutions),
                    substituteTypeParameters(functionType.returnType(), substitutions)
            );
            case LinkedTupleType linkedTupleType -> new LinkedTupleType(
                    linkedTupleType.elementTypes().stream()
                            .map(elementType -> substituteTypeParameters(elementType, substitutions))
                            .toList()
            );
            default -> type;
        };
    }

    private String typeDescriptor(LinkedType type) {
        return switch (type) {
            case PrimitiveLinkedType primitive -> primitive.name().toLowerCase();
            case CollectionLinkedType.LinkedList linkedList -> "list[" + typeDescriptor(linkedList.elementType()) + "]";
            case CollectionLinkedType.LinkedSet linkedSet -> "set[" + typeDescriptor(linkedSet.elementType()) + "]";
            case CollectionLinkedType.LinkedDict linkedDict -> "dict[" + typeDescriptor(linkedDict.valueType()) + "]";
            case LinkedTupleType linkedTupleType -> "tuple[" + linkedTupleType.elementTypes().stream()
                    .map(this::typeDescriptor)
                    .collect(java.util.stream.Collectors.joining(", ")) + "]";
            case LinkedFunctionType linkedFunctionType ->
                    "(" + typeDescriptor(linkedFunctionType.argumentType()) + " -> " + typeDescriptor(linkedFunctionType.returnType()) + ")";
            case LinkedDataType linkedDataType -> linkedDataType.typeParameters().isEmpty()
                    ? linkedDataType.name()
                    : linkedDataType.name() + "[" + String.join(", ", linkedDataType.typeParameters()) + "]";
            case LinkedDataParentType linkedDataParentType -> linkedDataParentType.typeParameters().isEmpty()
                    ? linkedDataParentType.name()
                    : linkedDataParentType.name() + "[" + String.join(", ", linkedDataParentType.typeParameters()) + "]";
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private ParsedGenericTypeName parseGenericTypeName(String rawName) {
        var idx = rawName.indexOf('[');
        if (idx <= 0 || !rawName.endsWith("]")) {
            return new ParsedGenericTypeName(rawName, List.of());
        }
        var baseName = rawName.substring(0, idx);
        var argsContent = rawName.substring(idx + 1, rawName.length() - 1);
        return new ParsedGenericTypeName(baseName, splitTopLevelTypeArguments(argsContent));
    }

    private List<String> splitTopLevelTypeArguments(String content) {
        var result = new ArrayList<String>();
        var depthSquare = 0;
        var depthParen = 0;
        var current = new StringBuilder();
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch == '[') {
                depthSquare++;
                current.append(ch);
                continue;
            }
            if (ch == ']') {
                depthSquare = Math.max(0, depthSquare - 1);
                current.append(ch);
                continue;
            }
            if (ch == '(') {
                depthParen++;
                current.append(ch);
                continue;
            }
            if (ch == ')') {
                depthParen = Math.max(0, depthParen - 1);
                current.append(ch);
                continue;
            }
            if (ch == ',' && depthSquare == 0 && depthParen == 0) {
                var token = current.toString().trim();
                if (!token.isEmpty()) {
                    result.add(token);
                }
                current.setLength(0);
                continue;
            }
            current.append(ch);
        }
        var token = current.toString().trim();
        if (!token.isEmpty()) {
            result.add(token);
        }
        return List.copyOf(result);
    }

    private pl.grzeslowski.capybara.parser.Type parseTypeArgument(String raw) {
        var trimmed = raw.trim();
        return PrimitiveType.find(trimmed)
                .map(pl.grzeslowski.capybara.parser.Type.class::cast)
                .orElseGet(() -> {
                    if (trimmed.startsWith("list[") && trimmed.endsWith("]")) {
                        return new CollectionType.ListType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1)));
                    }
                    if (trimmed.startsWith("set[") && trimmed.endsWith("]")) {
                        return new CollectionType.SetType(parseTypeArgument(trimmed.substring(4, trimmed.length() - 1)));
                    }
                    if (trimmed.startsWith("dict[") && trimmed.endsWith("]")) {
                        return new CollectionType.DictType(parseTypeArgument(trimmed.substring(5, trimmed.length() - 1)));
                    }
                    if (trimmed.startsWith("tuple[") && trimmed.endsWith("]")) {
                        var inner = trimmed.substring(6, trimmed.length() - 1);
                        var elements = splitTopLevelTypeArguments(inner).stream()
                                .map(this::parseTypeArgument)
                                .toList();
                        return new TupleType(elements);
                    }
                    var arrowIndex = indexOfTopLevelArrow(trimmed);
                    if (arrowIndex > 0) {
                        var left = trimmed.substring(0, arrowIndex).trim();
                        var right = trimmed.substring(arrowIndex + 2).trim();
                        return new FunctionType(parseTypeArgument(stripOptionalParentheses(left)), parseTypeArgument(right));
                    }
                    return new DataType(trimmed);
                });
    }

    private int indexOfTopLevelArrow(String value) {
        var square = 0;
        var paren = 0;
        for (int i = 0; i < value.length() - 1; i++) {
            var ch = value.charAt(i);
            if (ch == '[') {
                square++;
                continue;
            }
            if (ch == ']') {
                square = Math.max(0, square - 1);
                continue;
            }
            if (ch == '(') {
                paren++;
                continue;
            }
            if (ch == ')') {
                paren = Math.max(0, paren - 1);
                continue;
            }
            if (ch == '-' && value.charAt(i + 1) == '>' && square == 0 && paren == 0) {
                return i;
            }
        }
        return -1;
    }

    private String stripOptionalParentheses(String value) {
        var trimmed = value.trim();
        if (!trimmed.startsWith("(") || !trimmed.endsWith(")")) {
            return trimmed;
        }
        var depth = 0;
        for (int i = 0; i < trimmed.length(); i++) {
            var ch = trimmed.charAt(i);
            if (ch == '(') {
                depth++;
            } else if (ch == ')') {
                depth--;
            }
            if (depth == 0 && i < trimmed.length() - 1) {
                return trimmed;
            }
        }
        return trimmed.substring(1, trimmed.length() - 1).trim();
    }

    private Set<String> functionGenericTypeNames(Function function, Map<String, GenericDataType> dataTypes) {
        var names = new LinkedHashSet<String>();
        function.parameters().forEach(parameter -> collectFunctionGenericTypeNames(parameter.type(), dataTypes, names));
        function.returnType().ifPresent(type -> collectFunctionGenericTypeNames(type, dataTypes, names));
        return Set.copyOf(names);
    }

    private void collectFunctionGenericTypeNames(
            pl.grzeslowski.capybara.parser.Type type,
            Map<String, GenericDataType> dataTypes,
            Set<String> names
    ) {
        switch (type) {
            case CollectionType.ListType listType -> collectFunctionGenericTypeNames(listType.elementType(), dataTypes, names);
            case CollectionType.SetType setType -> collectFunctionGenericTypeNames(setType.elementType(), dataTypes, names);
            case CollectionType.DictType dictType -> collectFunctionGenericTypeNames(dictType.valueType(), dataTypes, names);
            case FunctionType functionType -> {
                collectFunctionGenericTypeNames(functionType.argumentType(), dataTypes, names);
                collectFunctionGenericTypeNames(functionType.returnType(), dataTypes, names);
            }
            case TupleType tupleType -> tupleType.elementTypes()
                    .forEach(elementType -> collectFunctionGenericTypeNames(elementType, dataTypes, names));
            case DataType dataType -> {
                var parsed = parseGenericTypeName(dataType.name());
                if (isFunctionGenericName(parsed.baseName()) && !isKnownTypeName(parsed.baseName(), dataTypes)) {
                    names.add(parsed.baseName());
                }
                parsed.typeArguments().stream()
                        .map(this::parseTypeArgument)
                        .forEach(argType -> collectFunctionGenericTypeNames(argType, dataTypes, names));
            }
            default -> {
            }
        }
    }

    private boolean isFunctionGenericName(String name) {
        return name.length() == 1 && Character.isUpperCase(name.charAt(0));
    }

    private boolean isKnownTypeName(String typeName, Map<String, GenericDataType> dataTypes) {
        return linkType(new DataType(typeName), dataTypes) instanceof ValueOrError.Value<LinkedType>;
    }

    private record ParsedGenericTypeName(String baseName, List<String> typeArguments) {
    }

    private ValueOrError<Map<String, GenericDataType>> types(Module module) {
        var normalizedFile = normalizeFile(moduleSourceFile(module));
        var rawTypeDeclarations = castList(module, TypeDeclaration.class);
        var rawTypeDeclarationsByName = rawTypeDeclarations.stream()
                .collect(toMap(TypeDeclaration::name, identity(), (first, second) -> first));
        var dataDeclarationsOrError = linkDataDeclarations(castList(module, DataDeclaration.class), rawTypeDeclarationsByName, normalizedFile);
        var singlesDeclarationsOrError = castList(module, SingleDeclaration.class)
                .stream()
                .map(this::linkSingleDeclaration)
                .collect(new ValueOrErrorCollectionCollector<>());

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();
        if (singlesDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var singleDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) singlesDeclarationsOrError).value();
        Map<String, GenericDataType> knownDataTypes = new HashMap<>();
        dataDeclarations.forEach(dataType -> knownDataTypes.put(dataType.name(), dataType));
        singleDeclarations.forEach(dataType -> knownDataTypes.put(dataType.name(), dataType));

        var typeDeclarationsOrError = rawTypeDeclarations
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(
                        typeDeclaration,
                        Stream.concat(dataDeclarations.stream(), singleDeclarations.stream()).toList(),
                        rawTypeDeclarationsByName,
                        knownDataTypes,
                        normalizedFile))
                .collect(new ValueOrErrorCollectionCollector<>());

        if (typeDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return new ValueOrError.Error<>(error.errors());
        }
        var typeDeclarations = ((ValueOrError.Value<List<LinkedDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(singleDeclarations);
        set.addAll(typeDeclarations);
        var map = set.stream().collect(toMap(GenericDataType::name, identity()));
        typeDeclarations.forEach(parentType -> parentType.subTypes().forEach(subType -> map.put(subType.name(), subType)));
        return ValueOrError.success(map);
    }

    private ValueOrError<List<LinkedDataType>> linkDataDeclarations(List<DataDeclaration> dataDeclarations) {
        return linkDataDeclarations(dataDeclarations, Map.of(), "");
    }

    private ValueOrError<List<LinkedDataType>> linkDataDeclarations(
            List<DataDeclaration> dataDeclarations,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            String normalizedFile
    ) {
        var declarationsByName = dataDeclarations.stream()
                .collect(toMap(DataDeclaration::name, identity(), (first, second) -> first));
        var cache = new HashMap<String, ValueOrError<LinkedDataType>>();
        return dataDeclarations.stream()
                .map(dataDeclaration -> linkDataDeclaration(
                        dataDeclaration,
                        declarationsByName,
                        rawTypeDeclarationsByName,
                        cache,
                        new HashSet<>(),
                        normalizedFile))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedDataType> linkDataDeclaration(
            DataDeclaration dataDeclaration,
            Map<String, DataDeclaration> declarationsByName,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, ValueOrError<LinkedDataType>> cache,
            Set<String> visiting,
            String normalizedFile
    ) {
        var cached = cache.get(dataDeclaration.name());
        if (cached != null) {
            return cached;
        }
        if (!visiting.add(dataDeclaration.name())) {
            return withPosition(
                    ValueOrError.error("Circular data extension detected for `" + dataDeclaration.name() + "`"),
                    dataDeclaration.position(),
                    normalizedFile);
        }

        var genericTypes = Set.copyOf(dataDeclaration.typeParameters());
        var inheritedFields = dataDeclaration.extendsTypes().stream()
                .map(parentName -> {
                    var parent = declarationsByName.get(parentName);
                    if (parent == null) {
                        return ValueOrError.<List<LinkedDataType.LinkedField>>error(
                                "Extended data type `" + parentName + "` not found"
                        );
                    }
                    return linkDataDeclaration(parent, declarationsByName, rawTypeDeclarationsByName, cache, visiting, normalizedFile)
                            .map(LinkedDataType::fields);
                })
                .collect(new ValueOrErrorCollectionCollector<>());
        var ownFields = dataDeclaration.fields().stream()
                .map(field -> linkField(
                        field,
                        genericTypes,
                        declarationsByName,
                        rawTypeDeclarationsByName,
                        cache,
                        visiting,
                        normalizedFile))
                .collect(new ValueOrErrorCollectionCollector<>());
        var linked = ValueOrError.join(
                        (List<List<LinkedDataType.LinkedField>> inherited, List<LinkedDataType.LinkedField> own) -> {
                            var fields = inherited.stream()
                                    .flatMap(Collection::stream)
                                    .collect(java.util.stream.Collectors.toCollection(ArrayList::new));
                            fields.addAll(own);
                            return new LinkedDataType(
                                    dataDeclaration.name(),
                                    List.copyOf(fields),
                                    dataDeclaration.typeParameters(),
                                    dataDeclaration.extendsTypes(),
                                    false
                            );
                        },
                        inheritedFields,
                        ownFields
                );
        visiting.remove(dataDeclaration.name());
        var withPosition = withPosition(linked, dataDeclaration.position(), normalizedFile);
        cache.put(dataDeclaration.name(), withPosition);
        return withPosition;
    }

    private ValueOrError<LinkedDataType> linkSingleDeclaration(SingleDeclaration singleDeclaration) {
        return ValueOrError.success(new LinkedDataType(singleDeclaration.name(), List.of(), List.of(), List.of(), true));
    }

    private ValueOrError<LinkedDataType.LinkedField> linkField(
            DataDeclaration.DataField type,
            Set<String> genericTypes,
            Map<String, DataDeclaration> declarationsByName,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, ValueOrError<LinkedDataType>> cache,
            Set<String> visiting,
            String normalizedFile
    ) {
        if (type.type() instanceof DataType dataType && genericTypes.contains(dataType.name())) {
            return ValueOrError.success(new LinkedDataType.LinkedField(type.name(), new LinkedGenericTypeParameter(dataType.name())));
        }
        if (type.type() instanceof DataType dataType && declarationsByName.containsKey(dataType.name())) {
            return linkDataDeclaration(
                    declarationsByName.get(dataType.name()),
                    declarationsByName,
                    rawTypeDeclarationsByName,
                    cache,
                    visiting,
                    normalizedFile)
                    .map(linkedDataType -> new LinkedDataType.LinkedField(type.name(), linkedDataType));
        }
        var knownDataTypes = new HashMap<String, GenericDataType>();
        declarationsByName.forEach((name, declaration) -> {
            var cached = cache.get(name);
            if (cached instanceof ValueOrError.Value<LinkedDataType> value) {
                knownDataTypes.put(name, value.value());
            } else {
                knownDataTypes.put(name, new LinkedDataType(
                        declaration.name(),
                        List.of(),
                        declaration.typeParameters(),
                        declaration.extendsTypes(),
                        false
                ));
            }
        });
        rawTypeDeclarationsByName.forEach((name, declaration) -> knownDataTypes.putIfAbsent(
                name,
                new LinkedDataParentType(name, List.of(), List.of(), declaration.typeParameters())
        ));
        var linkedType = linkType(type.type(), knownDataTypes);
        if (linkedType instanceof ValueOrError.Error<LinkedType>
            && type.type() instanceof DataType dataType
            && isQualifiedExternalTypeName(dataType.name())) {
            if (isOptionExternalTypeName(dataType.name())) {
                return ValueOrError.success(new LinkedDataType.LinkedField(
                        type.name(),
                        optionExternalPlaceholder(dataType.name())
                ));
            }
            return ValueOrError.success(new LinkedDataType.LinkedField(
                    type.name(),
                    new LinkedDataParentType(dataType.name(), List.of(), List.of(), List.of())
            ));
        }
        return linkedType.map(t -> new LinkedDataType.LinkedField(type.name(), t));
    }

    private boolean isQualifiedExternalTypeName(String typeName) {
        return typeName.startsWith("/") && typeName.contains(".");
    }

    private boolean isOptionExternalTypeName(String typeName) {
        var baseName = baseTypeName(typeName);
        return "/capy/lang/Option.Option".equals(baseName) || "/capy/lang/Option".equals(baseName);
    }

    private String baseTypeName(String typeName) {
        var idx = typeName.indexOf('[');
        if (idx > 0 && typeName.endsWith("]")) {
            return typeName.substring(0, idx);
        }
        return typeName;
    }

    private LinkedDataParentType optionExternalPlaceholder(String typeName) {
        var optionTypeParameter = optionExternalTypeParameter(typeName);
        var some = new LinkedDataType(
                "Some",
                List.of(new LinkedDataType.LinkedField("value", new LinkedGenericTypeParameter("T"))),
                List.of("T"),
                List.of(),
                false
        );
        var none = new LinkedDataType(
                "None",
                List.of(),
                List.of(),
                List.of(),
                true
        );
        return new LinkedDataParentType(
                baseTypeName(typeName),
                List.of(),
                List.of(some, none),
                List.of(optionTypeParameter)
        );
    }

    private String optionExternalTypeParameter(String typeName) {
        var start = typeName.indexOf('[');
        if (start > 0 && typeName.endsWith("]")) {
            var value = typeName.substring(start + 1, typeName.length() - 1).trim();
            if (!value.isEmpty()) {
                return value.toUpperCase(java.util.Locale.ROOT);
            }
        }
        return "ANY";
    }

    private ValueOrError<LinkedDataParentType> linkTypeDeclaration(
            TypeDeclaration typeDeclaration,
            List<LinkedDataType> dataDeclarations,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Map<String, GenericDataType> knownDataTypes,
            String normalizedFile
    ) {
        var linked = findSubtypes(typeDeclaration.subTypes(), dataDeclarations, rawTypeDeclarationsByName, new HashSet<>())
                .flatMap(subTypes -> linkedDataParentType(typeDeclaration, subTypes, knownDataTypes));
        return withPosition(linked, typeDeclaration.position(), normalizedFile);
    }

    private ValueOrError<LinkedDataParentType> linkedDataParentType(
            TypeDeclaration typeDeclaration,
            List<LinkedDataType> subTypes,
            Map<String, GenericDataType> knownDataTypes
    ) {
        var genericTypes = Set.copyOf(typeDeclaration.typeParameters());
        return typeDeclaration.fields()
                .stream()
                .map(field -> linkField(field, genericTypes, knownDataTypes))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(fields -> {
                    var inheritedSubtypes = subTypes.stream()
                            .map(subType -> new LinkedDataType(
                                    subType.name(),
                                    mergeParentFields(fields, subType.fields()),
                                    subType.typeParameters(),
                                    subType.extendedTypes(),
                                    subType.singleton()
                            ))
                            .toList();
                    return new LinkedDataParentType(
                            typeDeclaration.name(),
                            fields,
                            inheritedSubtypes,
                            typeDeclaration.typeParameters()
                    );
                });
    }

    private List<LinkedDataType.LinkedField> mergeParentFields(
            List<LinkedDataType.LinkedField> parentFields,
            List<LinkedDataType.LinkedField> childFields
    ) {
        var merged = new ArrayList<LinkedDataType.LinkedField>(parentFields);
        var childFieldNames = childFields.stream()
                .map(LinkedDataType.LinkedField::name)
                .collect(java.util.stream.Collectors.toSet());
        for (var parentField : parentFields) {
            if (childFieldNames.contains(parentField.name())) {
                merged.removeIf(field -> field.name().equals(parentField.name()));
            }
        }
        merged.addAll(childFields);
        return List.copyOf(merged);
    }

    private ValueOrError<LinkedDataType.LinkedField> linkField(
            DataDeclaration.DataField type,
            Set<String> genericTypes,
            Map<String, GenericDataType> knownDataTypes
    ) {
        if (type.type() instanceof DataType dataType && genericTypes.contains(dataType.name())) {
            return ValueOrError.success(new LinkedDataType.LinkedField(type.name(), new LinkedGenericTypeParameter(dataType.name())));
        }
        return linkType(type.type(), knownDataTypes)
                .map(t -> new LinkedDataType.LinkedField(type.name(), t));
    }

    private ValueOrError<List<LinkedDataType>> findSubtypes(
            List<String> rawSubTypes,
            List<LinkedDataType> dataDeclarations,
            Map<String, TypeDeclaration> rawTypeDeclarationsByName,
            Set<String> visitingTypes
    ) {
        var dataTypesMap = dataDeclarations.stream().collect(toMap(LinkedDataType::name, identity(), (first, second) -> first));
        return rawSubTypes.stream()
                .map(key -> {
                    var dataType = dataTypesMap.get(key);
                    if (dataType != null) {
                        return ValueOrError.success(List.of(dataType));
                    }
                    var typeDeclaration = rawTypeDeclarationsByName.get(key);
                    if (typeDeclaration == null) {
                        return ValueOrError.<List<LinkedDataType>>error("Type " + key + " not found");
                    }
                    if (!visitingTypes.add(key)) {
                        return ValueOrError.<List<LinkedDataType>>error("Circular type hierarchy detected for `" + key + "`");
                    }
                    var nested = findSubtypes(typeDeclaration.subTypes(), dataDeclarations, rawTypeDeclarationsByName, visitingTypes);
                    visitingTypes.remove(key);
                    return nested;
                })
                .collect(new ValueOrErrorCollectionCollector<List<LinkedDataType>>())
                .map(list -> list.stream().flatMap(Collection::stream).toList());
    }

    private static <T> ValueOrError<T> withPosition(ValueOrError<T> valueOrError, Optional<SourcePosition> position, String file) {
        if (valueOrError instanceof ValueOrError.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new ValueOrError.Error<>(error.errors()
                    .stream()
                    .map(singleError -> {
                        var hasKnownPosition = singleError.line() > 0;
                        var line = hasKnownPosition ? singleError.line() : pos.line();
                        var column = hasKnownPosition ? singleError.column() : pos.column();
                        var sourceFile = singleError.file().isBlank() ? file : singleError.file();
                        return new ValueOrError.Error.SingleError(line, column, sourceFile, singleError.message());
                    })
                    .toList());
        }
        return valueOrError;
    }

    private static <T> ValueOrError<T> withFile(ValueOrError<T> valueOrError, String moduleSourceFile) {
        if (!(valueOrError instanceof ValueOrError.Error<T> error)) {
            return valueOrError;
        }
        var normalizedFile = normalizeFile(moduleSourceFile);
        return new ValueOrError.Error<>(error.errors().stream()
                .map(singleError -> new ValueOrError.Error.SingleError(
                        singleError.line(),
                        singleError.column(),
                        singleError.file().isBlank() ? normalizedFile : singleError.file(),
                        singleError.message()))
                .toList());
    }

    private static String normalizeFile(String moduleSourceFile) {
        var normalized = moduleSourceFile.replace('\\', '/');
        return normalized.startsWith("/") ? normalized : "/" + normalized;
    }

    private static <T> List<T> castList(Module module, Class<T> clazz) {
        return module.functional().definitions()
                .stream()
                .filter(clazz::isInstance)
                .map(clazz::cast)
                .toList();
    }
}
