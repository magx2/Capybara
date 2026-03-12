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
        var functions = findFunctions(module.functional().definitions());
        var moduleSourceFile = moduleSourceFile(module);
        var availableSignatures = availableSignatures(module, modulesByName, linkedTypesByModule, signaturesByModule);
        if (availableSignatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
            return withFile(new ValueOrError.Error<>(error.errors()), moduleSourceFile);
        }
        var initialSignatures = ((ValueOrError.Value<List<CapybaraExpressionLinker.FunctionSignature>>) availableSignatures).value();
        return withFile(linkFunctions(functions, dataTypes, initialSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile), moduleSourceFile);
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
        return withFile(linkFunctions(functions, visibleTypes, initialSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile)
                .flatMap(firstPassFunctions -> {
                    var refinedSignatures = mergeSignatures(
                            signaturesByModule.get(module.name()),
                            signaturesFromLinkedFunctions(firstPassFunctions)
                    );
                    var refinedAvailableSignatures = mergeSignatures(initialSignatures, refinedSignatures);
                    return linkFunctions(functions, visibleTypes, refinedAvailableSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile)
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
                    var resolved = all.get(linkedDataParentType.name());
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
            List<CapybaraExpressionLinker.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            String moduleSourceFile
    ) {
        return functions.stream()
                .map(f -> linkFunction(f, dataTypes, signatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunction> linkFunction(
            Function function,
            Map<String, GenericDataType> dataTypes,
            List<CapybaraExpressionLinker.FunctionSignature> signatures,
            Map<String, List<CapybaraExpressionLinker.FunctionSignature>> signaturesByModule,
            Map<String, String> moduleClassNameByModuleName,
            String moduleSourceFile
    ) {
        var linked = linkParameters(function.parameters(), dataTypes)
                .flatMap(parameters -> new CapybaraExpressionLinker(
                        parameters,
                        dataTypes,
                        signatures,
                        signaturesByModule,
                        moduleClassNameByModuleName
                ).linkExpression(function.expression()).flatMap(ex -> function.returnType()
                        .map(type -> linkType(type, dataTypes))
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
        var normalizedFile = normalizeFile(moduleSourceFile);
        var fallbackPosition = function.expression().position().or(() -> function.position());
        return withPosition(linked, fallbackPosition, normalizedFile);
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
        var position = function.expression().position().or(() -> function.position()).orElse(SourcePosition.EMPTY);
        var line = Math.max(position.line(), 1);
        var column = Math.max(position.column(), 1);
        var file = normalizeFile(moduleSourceFile);
        var functionPreview = "fun " + function.name()
                              + "(" + function.parameters().stream()
                .map(parameter -> parameter.name() + ": " + formatParserType(parameter.type()))
                .collect(java.util.stream.Collectors.joining(", "))
                              + "): " + formatLinkedType(declaredReturnType)
                              + " = " + formatExpressionPreview(function.expression());
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
            default -> expression.toString();
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
                .map(function -> linkParameters(function.parameters(), dataTypes)
                        .flatMap(parameters -> function.returnType()
                                .map(type -> linkType(type, dataTypes))
                                .orElseGet(() -> ValueOrError.success(PrimitiveLinkedType.ANY))
                                .map(returnType -> new CapybaraExpressionLinker.FunctionSignature(
                                        function.name(),
                                        parameters.stream().map(LinkedFunctionParameter::type).toList(),
                                        returnType
                                ))))
                .collect(new ValueOrErrorCollectionCollector<>());
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
        return parameters.stream()
                .map(p -> linkParameter(p, dataTypes))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedFunctionParameter> linkParameter(Parameter parameter, Map<String, GenericDataType> dataTypes) {
        return withPosition(
                linkType(parameter.type(), dataTypes)
                        .map(type -> new LinkedFunctionParameter(parameter.name(), type)),
                parameter.position(),
                "");
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
