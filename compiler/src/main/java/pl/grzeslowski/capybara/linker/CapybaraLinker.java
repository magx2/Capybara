package pl.grzeslowski.capybara.linker;

import pl.grzeslowski.capybara.compiler.Module;
import pl.grzeslowski.capybara.compiler.Program;
import pl.grzeslowski.capybara.compiler.ImportDeclaration;
import pl.grzeslowski.capybara.linker.LinkedFunction.LinkedFunctionParameter;
import pl.grzeslowski.capybara.linker.expression.CapybaraExpressionLinker;
import pl.grzeslowski.capybara.parser.*;

import java.util.*;
import java.util.stream.Stream;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toMap;
import static pl.grzeslowski.capybara.linker.CapybaraTypeLinker.linkType;

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
            var linkedTypes = types(module);
            if (linkedTypes instanceof ValueOrError.Error<Map<String, GenericDataType>> error) {
                return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
            }
            linkedTypesByModule.put(module.name(), ((ValueOrError.Value<Map<String, GenericDataType>>) linkedTypes).value());
        }

        var visibleTypesByModule = new HashMap<String, Map<String, GenericDataType>>();
        for (var module : program.modules()) {
            var visibleTypes = availableTypes(module, modulesByName, linkedTypesByModule, program.modules());
            if (visibleTypes instanceof ValueOrError.Error<Map<String, GenericDataType>> error) {
                return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
            }
            visibleTypesByModule.put(module.name(), ((ValueOrError.Value<Map<String, GenericDataType>>) visibleTypes).value());
        }

        var signaturesByModule = new HashMap<String, List<CapybaraExpressionLinker.FunctionSignature>>();
        for (var module : program.modules()) {
            var functions = findFunctions(module.functional().definitions());
            var signatures = linkFunctionSignatures(functions, visibleTypesByModule.get(module.name()));
            if (signatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
                return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
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
            if (firstPassFunctions instanceof ValueOrError.Error<List<LinkedFunction>> error) {
                return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
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
        var availableSignatures = availableSignatures(module, modulesByName, linkedTypesByModule, signaturesByModule);
        if (availableSignatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var initialSignatures = ((ValueOrError.Value<List<CapybaraExpressionLinker.FunctionSignature>>) availableSignatures).value();
        return linkFunctions(functions, dataTypes, initialSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile(module));
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
        var availableSignatures = availableSignatures(module, modulesByName, linkedTypesByModule, signaturesByModule);
        if (availableSignatures instanceof ValueOrError.Error<List<CapybaraExpressionLinker.FunctionSignature>> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var initialSignatures = ((ValueOrError.Value<List<CapybaraExpressionLinker.FunctionSignature>>) availableSignatures).value();
        return linkFunctions(functions, visibleTypes, initialSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile(module))
                .flatMap(firstPassFunctions -> {
                    var refinedSignatures = mergeSignatures(
                            signaturesByModule.get(module.name()),
                            signaturesFromLinkedFunctions(firstPassFunctions)
                    );
                    var refinedAvailableSignatures = mergeSignatures(initialSignatures, refinedSignatures);
                    return linkFunctions(functions, visibleTypes, refinedAvailableSignatures, signaturesByModule, moduleClassNameByModuleName, moduleSourceFile(module))
                            .map(linkedFunctions -> new LinkedModule(
                                    module.name(),
                                    module.path(),
                                    localTypes,
                                    deduplicateFunctions(linkedFunctions),
                                    staticImports(module, modulesByName, linkedTypesByModule, signaturesByModule)
                            ));
                });
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
                .flatMap(parameters ->
                        new CapybaraExpressionLinker(
                                parameters,
                                dataTypes,
                                signatures,
                                signaturesByModule,
                                moduleClassNameByModuleName
                        ).linkExpression(function.expression())
                                .flatMap(ex ->
                                        function.returnType()
                                                .map(type -> {
                                                    // todo check if this type matches ex.type
                                                    return linkType(type, dataTypes);
                                                })
                                                .orElseGet(() -> ValueOrError.success(ex.type()))
                                                .map(rtype ->
                                                        new LinkedFunction(
                                                                function.name(),
                                                                rtype,
                                                                parameters,
                                                                enrichNothing(ex, function.name(), moduleSourceFile)))));
        return withPosition(linked, function.position());
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
                parameter.position()
        );
    }

    private ValueOrError<Map<String, GenericDataType>> types(Module module) {
        var dataDeclarationsOrError = linkDataDeclarations(castList(module, DataDeclaration.class));
        var singlesDeclarationsOrError = castList(module, SingleDeclaration.class)
                .stream()
                .map(this::linkSingleDeclaration)
                .collect(new ValueOrErrorCollectionCollector<>());

        if (dataDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var dataDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) dataDeclarationsOrError).value();
        if (singlesDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var singleDeclarations = ((ValueOrError.Value<List<LinkedDataType>>) singlesDeclarationsOrError).value();

        var typeDeclarationsOrError = castList(module, TypeDeclaration.class)
                .stream()
                .map(typeDeclaration -> linkTypeDeclaration(typeDeclaration, Stream.concat(dataDeclarations.stream(), singleDeclarations.stream()).toList()))
                .collect(new ValueOrErrorCollectionCollector<>());

        if (typeDeclarationsOrError instanceof ValueOrError.Error<?> error) {
            return ValueOrError.error(error.errors().stream().map(ValueOrError.Error.SingleError::message).toList());
        }
        var typeDeclarations = ((ValueOrError.Value<List<LinkedDataParentType>>) typeDeclarationsOrError).value();

        var set = new HashSet<GenericDataType>();
        set.addAll(dataDeclarations);
        set.addAll(singleDeclarations);
        set.addAll(typeDeclarations);
        var map = set.stream().collect(toMap(GenericDataType::name, identity()));
        return ValueOrError.success(map);
    }

    private ValueOrError<List<LinkedDataType>> linkDataDeclarations(List<DataDeclaration> dataDeclarations) {
        var declarationsByName = dataDeclarations.stream()
                .collect(toMap(DataDeclaration::name, identity(), (first, second) -> first));
        var cache = new HashMap<String, ValueOrError<LinkedDataType>>();
        return dataDeclarations.stream()
                .map(dataDeclaration -> linkDataDeclaration(dataDeclaration, declarationsByName, cache, new HashSet<>()))
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private ValueOrError<LinkedDataType> linkDataDeclaration(
            DataDeclaration dataDeclaration,
            Map<String, DataDeclaration> declarationsByName,
            Map<String, ValueOrError<LinkedDataType>> cache,
            Set<String> visiting
    ) {
        var cached = cache.get(dataDeclaration.name());
        if (cached != null) {
            return cached;
        }
        if (!visiting.add(dataDeclaration.name())) {
            return withPosition(
                    ValueOrError.error("Circular data extension detected for `" + dataDeclaration.name() + "`"),
                    dataDeclaration.position()
            );
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
                    return linkDataDeclaration(parent, declarationsByName, cache, visiting)
                            .map(LinkedDataType::fields);
                })
                .collect(new ValueOrErrorCollectionCollector<>());
        var ownFields = dataDeclaration.fields().stream()
                .map(field -> linkField(field, genericTypes))
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
        var withPosition = withPosition(linked, dataDeclaration.position());
        cache.put(dataDeclaration.name(), withPosition);
        return withPosition;
    }

    private ValueOrError<LinkedDataType> linkSingleDeclaration(SingleDeclaration singleDeclaration) {
        return ValueOrError.success(new LinkedDataType(singleDeclaration.name(), List.of(), List.of(), List.of(), true));
    }

    private ValueOrError<LinkedDataType.LinkedField> linkField(DataDeclaration.DataField type, Set<String> genericTypes) {
        if (type.type() instanceof DataType dataType && genericTypes.contains(dataType.name())) {
            return ValueOrError.success(new LinkedDataType.LinkedField(type.name(), new LinkedGenericTypeParameter(dataType.name())));
        }
        return linkType(type.type())
                .map(t -> new LinkedDataType.LinkedField(type.name(), t));
    }

    private ValueOrError<LinkedDataParentType> linkTypeDeclaration(TypeDeclaration typeDeclaration, List<LinkedDataType> dataDeclarations) {
        var linked = findSubtypes(typeDeclaration.subTypes(), dataDeclarations)
                .flatMap(subTypes -> linkedDataParentType(typeDeclaration, subTypes));
        return withPosition(linked, typeDeclaration.position());
    }

    private ValueOrError<LinkedDataParentType> linkedDataParentType(TypeDeclaration typeDeclaration, List<LinkedDataType> subTypes) {
        var genericTypes = Set.copyOf(typeDeclaration.typeParameters());
        return typeDeclaration.fields()
                .stream()
                .map(field -> linkField(field, genericTypes))
                .collect(new ValueOrErrorCollectionCollector<>())
                .map(fields -> new LinkedDataParentType(typeDeclaration.name(),
                        fields,
                        subTypes,
                        typeDeclaration.typeParameters()));
    }

    private ValueOrError<List<LinkedDataType>> findSubtypes(List<String> rawSubTypes, List<LinkedDataType> dataDeclarations) {
        var dataTypesMap = dataDeclarations.stream().collect(toMap(LinkedDataType::name, identity(), (first, second) -> first));
        return rawSubTypes.stream()
                .map(key -> {
                    var dataType = dataTypesMap.get(key);
                    if (dataType == null) {
                        return ValueOrError.<LinkedDataType>error("Type " + key + " not found");
                    }
                    return ValueOrError.success(dataType);
                })
                .collect(new ValueOrErrorCollectionCollector<>());
    }

    private static <T> ValueOrError<T> withPosition(ValueOrError<T> valueOrError, Optional<pl.grzeslowski.capybara.parser.SourcePosition> position) {
        if (valueOrError instanceof ValueOrError.Error<T> error && position.isPresent()) {
            var pos = position.get();
            return new ValueOrError.Error<>(error.errors()
                    .stream()
                    .map(ValueOrError.Error.SingleError::message)
                    .map(msg -> "line %d, column %d: %s".formatted(pos.line(), pos.column(), msg))
                    .map(ValueOrError.Error.SingleError::new)
                    .toList());
        }
        return valueOrError;
    }

    private static <T> List<T> castList(Module module, Class<T> clazz) {
        return module.functional().definitions()
                .stream()
                .filter(clazz::isInstance)
                .map(clazz::cast)
                .toList();
    }
}
