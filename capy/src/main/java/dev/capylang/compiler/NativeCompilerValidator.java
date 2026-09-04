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
import dev.capylang.compiler.parser.Expression.BoolLiteral;
import dev.capylang.compiler.parser.Expression.DataField;
import dev.capylang.compiler.parser.Expression.DataLiteral;
import dev.capylang.compiler.parser.Expression.DictEntry;
import dev.capylang.compiler.parser.Expression.DictLiteral;
import dev.capylang.compiler.parser.Expression.DoubleLiteral;
import dev.capylang.compiler.parser.Expression.FieldAccessExpression;
import dev.capylang.compiler.parser.Expression.FloatLiteral;
import dev.capylang.compiler.parser.Expression.FunctionCallExpression;
import dev.capylang.compiler.parser.Expression.FunctionReferenceExpression;
import dev.capylang.compiler.parser.Expression.IfExpression;
import dev.capylang.compiler.parser.Expression.IndexExpression;
import dev.capylang.compiler.parser.Expression.IntLiteral;
import dev.capylang.compiler.parser.Expression.LambdaExpression;
import dev.capylang.compiler.parser.Expression.LetBinding;
import dev.capylang.compiler.parser.Expression.ListLiteral;
import dev.capylang.compiler.parser.Expression.LongLiteral;
import dev.capylang.compiler.parser.Expression.MatchCase;
import dev.capylang.compiler.parser.Expression.MatchExpression;
import dev.capylang.compiler.parser.Expression.MethodCallExpression;
import dev.capylang.compiler.parser.Expression.ReduceExpression;
import dev.capylang.compiler.parser.Expression.SetLiteral;
import dev.capylang.compiler.parser.Expression.StringLiteral;
import dev.capylang.compiler.parser.Expression.TupleLiteral;
import dev.capylang.compiler.parser.Expression.ThrowExpression;
import dev.capylang.compiler.parser.Expression.TryCatchExpression;
import dev.capylang.compiler.parser.Expression.UnaryExpression;
import dev.capylang.compiler.parser.Expression.UnsupportedExpression;
import dev.capylang.compiler.parser.Expression.VariableExpression;
import dev.capylang.compiler.parser.Expression.WithExpression;
import dev.capylang.compiler.parser.FunctionAnnotationApplication;
import dev.capylang.compiler.parser.FunctionAnnotationArgument;
import dev.capylang.compiler.parser.FunctionAnnotationValue.FunctionAnnotationStringValue;
import dev.capylang.compiler.parser.FunctionDeclaration;
import dev.capylang.compiler.parser.FunctionParameter;
import dev.capylang.compiler.parser.ImportDeclaration;
import dev.capylang.compiler.parser.NativeCapybaraParser;
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
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;

public final class NativeCompilerValidator {
    private static final ThreadLocal<Map<String, ParsedModule>> VALIDATED_MODULES =
            ThreadLocal.withInitial(LinkedHashMap::new);
    private static final Map<String, Optional<CompiledModule>> BUNDLED_MODULES = new ConcurrentHashMap<>();

    private record LinkedDataField(String name, TypeReference typeReference) {
    }

    private static final Set<String> PIPE_OPERATORS = Set.of("|", "|-", "|*", "|!");
    private static final Map<String, Map<String, Set<Integer>>> STANDARD_FUNCTION_ARITIES = Map.of(
            "capy/collection/Seq", Map.of("to_seq", Set.of(1))
    );
    private static final Map<String, Set<String>> JAVA_SUPPORTED_METHODS_BY_RECEIVER = Map.ofEntries(
            Map.entry("Effect", Set.of("map", "flat_map", "start")),
            Map.entry("Result", Set.of("map", "flat_map", "reduce", "reduce_left", "recover", "or_else", "or")),
            Map.entry("Option", Set.of("map", "filter", "flat_map", "or_else", "or")),
            Map.entry("Async", Set.of("join", "map", "flat_map", "`|`", "`|*`")),
            Map.entry("List", Set.of(
                    "size", "is_empty", "plus", "minus", "any", "all", "contains", "reduce", "reduce_left",
                    "fold", "map", "filter", "reject", "flat_map", "sort", "get",
                    "`+`", "`-`", "`?`", "`|>`", "`|`", "`|-`", "`|*`"
            ))
    );
    private static final Set<String> BUILTIN_TYPES = Set.of(
            "byte", "char", "int", "long", "double", "bool", "float", "void", "any", "data",
            "nothing", "String", "List", "Set", "Dict", "Tuple", "Option", "Result", "Either",
            "Effect", "Program", "Assert", "TestFile", "TestCase", "Seq", "Regex", "Match",
            "Path", "Ordering", "size", "index"
    );
    private static final Set<String> SIZE_RECEIVER_TYPES = Set.of("List", "Set", "Dict", "String", "char");
    private static final Map<String, Map<String, Set<Integer>>> NATIVE_METHOD_ARITIES = Map.ofEntries(
            Map.entry("String", Map.of(
                    "get", Set.of(1, 2),
                    "compare", Set.of(1),
                    "to_int", Set.of(0),
                    "to_long", Set.of(0),
                    "to_double", Set.of(0),
                    "to_float", Set.of(0),
                    "to_bool", Set.of(0)
            )),
            Map.entry("List", Map.of("get", Set.of(1, 2))),
            Map.entry("Dict", Map.of(
                    "entries", Set.of(0),
                    "contains_key", Set.of(1),
                    "get", Set.of(1)
            )),
            Map.entry("Tuple", Map.of("get", Set.of(1))),
            Map.entry("Regex", Map.of(
                    "matches", Set.of(1),
                    "find", Set.of(1),
                    "find_all", Set.of(1),
                    "replace", Set.of(1),
                    "split", Set.of(1)
            )),
            Map.entry("char", Map.of("to_string", Set.of(0)))
    );
    private static final Set<String> SEQ_RECEIVER_TYPES = Set.of("Seq", "Cons", "End");
    private static final Set<String> COLLECTION_RECEIVER_TYPES = Set.of(
            "List", "Set", "String", "Seq", "Cons", "End", "Dict"
    );
    private static final Set<String> COLLECTION_ONE_ARGUMENT_METHODS = Set.of(
            "map", "filter", "`|-`", "reject", "flat_map", "any", "all"
    );
    private static final Set<String> COLLECTION_TWO_ARGUMENT_METHODS = Set.of("reduce", "reduce_left");
    private static final Set<String> SEQ_ZERO_ARGUMENT_METHODS = Set.of("as_list", "first", "rest");
    private static final Set<String> SET_ZERO_ARGUMENT_METHODS = Set.of("power_set", "℘");
    private static final Set<String> SET_ONE_ARGUMENT_METHODS = Set.of(
            "contains", "is_subset_of", "is_proper_subset_of", "is_superset_of", "is_proper_superset_of",
            "union", "intersection", "difference", "symmetric_difference", "cartesian_product"
    );
    private static final Map<String, Set<Integer>> ASSERT_METHOD_ARITIES = Map.ofEntries(
            Map.entry("is_equal_to", Set.of(1, 2, 3)),
            Map.entry("is_true", Set.of(0)),
            Map.entry("is_false", Set.of(0)),
            Map.entry("is_greater_than", Set.of(1)),
            Map.entry("is_less_than", Set.of(1)),
            Map.entry("is_greater_or_equals_than", Set.of(1)),
            Map.entry("is_less_or_equals_than", Set.of(1)),
            Map.entry("is_zero", Set.of(0)),
            Map.entry("is_one", Set.of(0)),
            Map.entry("is_between", Set.of(2)),
            Map.entry("starts_with", Set.of(1)),
            Map.entry("does_not_start_with", Set.of(1)),
            Map.entry("contains", Set.of(1)),
            Map.entry("has_size", Set.of(1)),
            Map.entry("is_empty", Set.of(0)),
            Map.entry("succeeds", Set.of(0, 1)),
            Map.entry("fails", Set.of(0, 1)),
            Map.entry("has_day", Set.of(1)),
            Map.entry("has_month", Set.of(1)),
            Map.entry("has_year", Set.of(1)),
            Map.entry("has_hour", Set.of(1)),
            Map.entry("has_minute", Set.of(1)),
            Map.entry("has_second", Set.of(1)),
            Map.entry("has_offset_minutes", Set.of(1)),
            Map.entry("has_date", Set.of(1, 3)),
            Map.entry("has_time", Set.of(1, 3)),
            Map.entry("has_years", Set.of(1)),
            Map.entry("has_months", Set.of(1)),
            Map.entry("has_days", Set.of(1)),
            Map.entry("has_hours", Set.of(1)),
            Map.entry("has_minutes", Set.of(1)),
            Map.entry("has_seconds", Set.of(1)),
            Map.entry("has_weeks", Set.of(1)),
            Map.entry("has_start", Set.of(1)),
            Map.entry("has_end", Set.of(1)),
            Map.entry("has_duration", Set.of(1)),
            Map.entry("fails_with_kind", Set.of(1, 2)),
            Map.entry("does_not_contain", Set.of(1)),
            Map.entry("contains_key", Set.of(1)),
            Map.entry("does_not_contain_key", Set.of(1)),
            Map.entry("contains_value", Set.of(1))
    );
    private static final Set<String> NUMERIC_TYPES = Set.of("byte", "int", "long", "float", "double");
    private static final Map<String, Map<String, Set<Integer>>> STANDARD_METHOD_ARITIES = Map.ofEntries(
            Map.entry("Result", Map.of(
                    "map", Set.of(1),
                    "flat_map", Set.of(1),
                    "reduce", Set.of(2),
                    "reduce_left", Set.of(2),
                    "recover", Set.of(1),
                    "or_else", Set.of(1),
                    "or", Set.of(1)
            )),
            Map.entry("Option", Map.of(
                    "map", Set.of(1),
                    "filter", Set.of(1),
                    "flat_map", Set.of(1),
                    "reduce", Set.of(2),
                    "or_else", Set.of(1),
                    "or", Set.of(1)
            )),
            Map.entry("Effect", Map.of(
                    "map", Set.of(1),
                    "flat_map", Set.of(1),
                    "start", Set.of(0)
            )),
            Map.entry("Async", Map.of(
                    "join", Set.of(0),
                    "map", Set.of(1),
                    "flat_map", Set.of(1),
                    "`|`", Set.of(1),
                    "`|*`", Set.of(1)
            ))
    );
    private static final Map<String, Set<String>> STANDARD_METHODS_BY_RECEIVER = Map.ofEntries(
            Map.entry("Result", STANDARD_METHOD_ARITIES.get("Result").keySet()),
            Map.entry("Option", STANDARD_METHOD_ARITIES.get("Option").keySet()),
            Map.entry("Effect", STANDARD_METHOD_ARITIES.get("Effect").keySet()),
            Map.entry("Async", STANDARD_METHOD_ARITIES.get("Async").keySet())
    );

    public List<CompilerError> validate(
            List<ParsedModule> modules,
            List<String> libraryModules,
            NativeProviderManifest nativeProviders
    ) {
        var context = new Context(modules, libraryModules);
        VALIDATED_MODULES.get().clear();
        var errors = new ArrayList<CompilerError>();
        validateImports(context, errors);
        validateDefinitions(context, errors);
        validateObjectOriented(context, errors);
        validateNativeProviderManifest(nativeProviders, errors);
        if (errors.isEmpty()) {
            rememberValidatedModules(modules);
        }
        return List.copyOf(errors);
    }

    private void rememberValidatedModules(List<ParsedModule> modules) {
        var validated = VALIDATED_MODULES.get();
        for (var module : modules) {
            validated.put(parsedModulePath(module), module);
        }
    }

    private String parsedModulePath(ParsedModule module) {
        var path = normalizeModulePath(module.path());
        return path.isBlank() ? module.name() : path + "/" + module.name();
    }

    private Optional<CompiledModule> bundledModule(String modulePath) {
        return BUNDLED_MODULES.computeIfAbsent(modulePath, path -> {
            try (var input = NativeCompilerValidator.class.getResourceAsStream("/" + path + ".json")) {
                if (input == null) {
                    return Optional.empty();
                }
                var json = new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
                return Optional.of(LinkedJsonCodec.read(json, CompiledModule.class));
            } catch (java.io.IOException | RuntimeException ignored) {
                return Optional.empty();
            }
        });
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
                validateNestedLambdaFunctionArguments(
                        context,
                        module,
                        constant.constant().expression(),
                        Map.of(),
                        errors
                );
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
        validatePublicFunctionSignatureVisibility(context, module, function, errors);
        validateTypeReference(context, module, function.returnType(), List.of(), errors, function.location());
        for (var parameter : function.parameters()) {
            validateTypeReference(context, module, parameter.typeReference(), List.of(), errors, parameter.location());
        }
        validateFunctionAnnotations(context, module, function, errors, nativeProviderKeys);
        validateExpression(context, module, function.body(), errors);
        validateFunctionReturnType(context, module, function, errors);
        validateNestedLambdaFunctionArguments(
                context,
                module,
                function.body(),
                parameterTypes(function),
                errors
        );
        if (javaBackendSelected()) {
            validateJavaBackendVariables(
                    context,
                    module,
                    function.body(),
                    functionVariableNames(function),
                    errors
            );
            validateJavaBackendExpression(
                    module,
                    function.body(),
                    parameterTypes(function),
                    errors
            );
        }
    }

    private void validatePublicFunctionSignatureVisibility(
            Context context,
            ParsedModule module,
            FunctionDeclaration function,
            List<CompilerError> errors
    ) {
        if (!function.visibility().equals("public")) {
            return;
        }
        for (var parameter : function.parameters()) {
            var privateType = privateSignatureType(context, module, parameter.typeReference());
            if (privateType != null) {
                errors.add(privateSignatureTypeError(
                        module,
                        function,
                        privateType,
                        "parameter `" + parameter.name() + "`"
                ));
            }
        }
        var returnType = function.returnType().name().isBlank()
                ? validationExpressionType(context, module, function.body(), parameterTypes(function))
                : function.returnType();
        var privateReturnType = privateSignatureType(context, module, returnType);
        if (privateReturnType != null) {
            errors.add(privateSignatureTypeError(module, function, privateReturnType, "return type"));
        }
    }

    private CompilerError privateSignatureTypeError(
            ParsedModule module,
            FunctionDeclaration function,
            String typeName,
            String position
    ) {
        return error(
                module,
                function.location(),
                "Public function `" + function.name() + "` exposes private type `"
                        + typeName + "` in its " + position + "."
        );
    }

    private String privateSignatureType(Context context, ParsedModule module, TypeReference type) {
        if (type == null) {
            return null;
        }
        var typeName = nominalTypeName(type.name());
        if (!typeName.isBlank() && context.privateType(module, typeName)) {
            return typeName;
        }
        for (var argument : type.arguments()) {
            var privateType = privateSignatureType(context, module, argument);
            if (privateType != null) {
                return privateType;
            }
        }
        return null;
    }

    private Set<String> functionVariableNames(FunctionDeclaration function) {
        var names = new LinkedHashSet<String>();
        function.parameters().stream().map(FunctionParameter::name).forEach(names::add);
        if (function.name().contains(".")) {
            names.add("this");
        }
        return names;
    }

    private void validateJavaBackendVariables(
            Context context,
            ParsedModule module,
            Expression expression,
            Set<String> variables,
            List<CompilerError> errors
    ) {
        switch (expression) {
            case VariableExpression variable -> {
                if (!knownJavaBackendVariable(context, module, variable.name(), variables)) {
                    errors.add(error(
                            module,
                            variable.location(),
                            "Unresolved variable `" + variable.name() + "`."
                    ));
                }
            }
            case BinaryExpression binary -> {
                validateJavaBackendVariables(context, module, binary.left(), variables, errors);
                if (!PIPE_OPERATORS.contains(binary.operator()) || scopedPipeRight(binary.right())) {
                    validateJavaBackendVariables(context, module, binary.right(), variables, errors);
                }
            }
            case BlockExpression block -> {
                var blockVariables = new LinkedHashSet<>(variables);
                for (var binding : block.bindings()) {
                    validateJavaBackendVariables(context, module, binding.value(), blockVariables, errors);
                    blockVariables.add(binding.name());
                }
                validateJavaBackendVariables(context, module, block.result(), blockVariables, errors);
            }
            case DataLiteral literal -> literal.fields().forEach(field ->
                    validateJavaBackendVariables(context, module, field.value(), variables, errors));
            case DictLiteral literal -> literal.entries().forEach(entry -> {
                validateJavaBackendVariables(context, module, entry.key(), variables, errors);
                validateJavaBackendVariables(context, module, entry.value(), variables, errors);
            });
            case FieldAccessExpression access ->
                    validateJavaBackendVariables(context, module, access.receiver(), variables, errors);
            case FunctionCallExpression call -> call.arguments().forEach(argument ->
                    validateJavaBackendVariables(context, module, argument, variables, errors));
            case IfExpression ifExpression -> {
                validateJavaBackendVariables(context, module, ifExpression.condition(), variables, errors);
                validateJavaBackendVariables(context, module, ifExpression.thenBranch(), variables, errors);
                validateJavaBackendVariables(context, module, ifExpression.elseBranch(), variables, errors);
            }
            case IndexExpression index -> {
                validateJavaBackendVariables(context, module, index.receiver(), variables, errors);
                validateJavaBackendVariables(context, module, index.index(), variables, errors);
                if (index.hasEndIndex()) {
                    validateJavaBackendVariables(context, module, index.endIndex(), variables, errors);
                }
            }
            case LambdaExpression lambda -> {
                var lambdaVariables = new LinkedHashSet<>(variables);
                lambda.parameters().stream()
                        .map(this::decodedLambdaParameterName)
                        .filter(name -> !name.equals("_"))
                        .forEach(lambdaVariables::add);
                validateJavaBackendVariables(context, module, lambda.body(), lambdaVariables, errors);
            }
            case ListLiteral literal -> literal.values().forEach(value ->
                    validateJavaBackendVariables(context, module, value, variables, errors));
            case MatchExpression match -> {
                validateJavaBackendVariables(context, module, match.value(), variables, errors);
                for (var matchCase : match.cases()) {
                    if (matchCase.hasLiteral()) {
                        validateJavaBackendVariables(context, module, matchCase.literal(), variables, errors);
                    }
                    var caseVariables = new LinkedHashSet<>(variables);
                    matchCase.bindings().stream()
                            .filter(name -> !name.equals("_"))
                            .forEach(caseVariables::add);
                    if (matchCase.hasGuard()) {
                        validateJavaBackendVariables(context, module, matchCase.guard(), caseVariables, errors);
                    }
                    validateJavaBackendVariables(context, module, matchCase.body(), caseVariables, errors);
                }
            }
            case MethodCallExpression call -> {
                validateJavaBackendVariables(context, module, call.receiver(), variables, errors);
                call.arguments().forEach(argument ->
                        validateJavaBackendVariables(context, module, argument, variables, errors));
            }
            case ReduceExpression reduce -> {
                validateJavaBackendVariables(context, module, reduce.receiver(), variables, errors);
                validateJavaBackendVariables(context, module, reduce.initial(), variables, errors);
                var reduceVariables = new LinkedHashSet<>(variables);
                addVariableName(reduceVariables, reduce.accumulatorName());
                addVariableName(reduceVariables, reduce.keyName());
                addVariableName(reduceVariables, reduce.valueName());
                validateJavaBackendVariables(context, module, reduce.body(), reduceVariables, errors);
            }
            case SetLiteral literal -> literal.values().forEach(value ->
                    validateJavaBackendVariables(context, module, value, variables, errors));
            case ThrowExpression throwExpression ->
                    validateJavaBackendVariables(context, module, throwExpression.value(), variables, errors);
            case TryCatchExpression tryCatch -> {
                validateJavaBackendVariables(context, module, tryCatch.body(), variables, errors);
                tryCatch.branches().forEach(branch -> {
                    var catchVariables = new LinkedHashSet<>(variables);
                    addVariableName(catchVariables, branch.catchName());
                    validateJavaBackendVariables(context, module, branch.catchBody(), catchVariables, errors);
                });
            }
            case TupleLiteral literal -> literal.values().forEach(value ->
                    validateJavaBackendVariables(context, module, value, variables, errors));
            case UnaryExpression unary ->
                    validateJavaBackendVariables(context, module, unary.expression(), variables, errors);
            case WithExpression with -> {
                validateJavaBackendVariables(context, module, with.receiver(), variables, errors);
                with.fields().forEach(field ->
                        validateJavaBackendVariables(context, module, field.value(), variables, errors));
            }
            default -> {
            }
        }
    }

    private boolean scopedPipeRight(Expression expression) {
        if (expression instanceof LambdaExpression) {
            return true;
        }
        if (expression instanceof MethodCallExpression call) {
            return scopedPipeRight(call.receiver());
        }
        return false;
    }

    private boolean knownJavaBackendVariable(
            Context context,
            ParsedModule module,
            String name,
            Set<String> variables
    ) {
        return variables.contains(name)
                || knownFunction(context, module, name)
                || name.contains(".")
                || name.contains("/");
    }

    private String decodedLambdaParameterName(String parameter) {
        var prefix = "__capy_typed_lambda|";
        if (!parameter.startsWith(prefix)) {
            return parameter;
        }
        var rest = parameter.substring(prefix.length());
        var separator = rest.indexOf('|');
        return separator < 0 ? parameter : rest.substring(0, separator);
    }

    private TypeReference decodedLambdaParameterType(String parameter) {
        var prefix = "__capy_typed_lambda|";
        if (!parameter.startsWith(prefix)) {
            return null;
        }
        var rest = parameter.substring(prefix.length());
        var separator = rest.indexOf('|');
        return separator < 0 ? null : NativeCapybaraParser.parseTypeReference(rest.substring(separator + 1));
    }

    private void addVariableName(Set<String> variables, String name) {
        if (!name.isBlank() && !name.equals("_")) {
            variables.add(name);
        }
    }

    private boolean javaBackendSelected() {
        return BackendCompilationContext.outputType()
                .map(outputType -> outputType.equalsIgnoreCase("java"))
                .orElse(false);
    }

    private Map<String, TypeReference> parameterTypes(FunctionDeclaration function) {
        var types = new LinkedHashMap<String, TypeReference>();
        for (var parameter : function.parameters()) {
            types.put(parameter.name(), parameter.typeReference());
        }
        return types;
    }

    private void validateNestedLambdaFunctionArguments(
            Context context,
            ParsedModule module,
            Expression expression,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        switch (expression) {
            case BinaryExpression binary -> {
                validatePipeRightOperand(context, module, binary, types, errors);
                validateSequenceConcatOperands(context, module, binary, types, errors);
                validateNestedLambdaFunctionArguments(context, module, binary.left(), types, errors);
                if (PIPE_OPERATORS.contains(binary.operator())
                        && binary.right() instanceof LambdaExpression mapper) {
                    var receiverType = validationExpressionType(context, module, binary.left(), types);
                    var valueType = validationIterableElementType(receiverType);
                    var mapperTypes = valueType == null
                            ? types
                            : validationLambdaTypes(mapper, valueType, types);
                    validateNestedLambdaFunctionArguments(context, module, mapper.body(), mapperTypes, errors);
                } else {
                    validateNestedLambdaFunctionArguments(context, module, binary.right(), types, errors);
                }
            }
            case BlockExpression block -> {
                var blockTypes = new LinkedHashMap<>(types);
                for (var binding : block.bindings()) {
                    validateDistinctCollectionBinding(context, module, binding, blockTypes, errors);
                    validateNestedLambdaFunctionArguments(context, module, binding.value(), blockTypes, errors);
                    if (!binding.typeReference().name().isBlank()) {
                        blockTypes.put(binding.name(), binding.typeReference());
                    }
                }
                validateNestedLambdaFunctionArguments(context, module, block.result(), blockTypes, errors);
            }
            case DataLiteral literal -> {
                validateDataLiteralFieldTypes(context, module, literal, types, errors);
                literal.fields().forEach(field ->
                        validateNestedLambdaFunctionArguments(context, module, field.value(), types, errors));
            }
            case DictLiteral literal -> literal.entries().forEach(entry -> {
                validateNestedLambdaFunctionArguments(context, module, entry.key(), types, errors);
                validateNestedLambdaFunctionArguments(context, module, entry.value(), types, errors);
            });
            case FieldAccessExpression access ->
                    validateNestedLambdaFunctionArguments(context, module, access.receiver(), types, errors);
            case FunctionCallExpression call -> {
                validateKnownQualifiedExtensionMethodReceiver(context, module, call, types, errors);
                validateNestedLambdaCall(context, module, call, types, errors);
                call.arguments().forEach(argument ->
                        validateNestedLambdaFunctionArguments(context, module, argument, types, errors));
            }
            case IfExpression ifExpression -> {
                validateNestedLambdaFunctionArguments(context, module, ifExpression.condition(), types, errors);
                validateNestedLambdaFunctionArguments(context, module, ifExpression.thenBranch(), types, errors);
                validateNestedLambdaFunctionArguments(context, module, ifExpression.elseBranch(), types, errors);
            }
            case IndexExpression index -> {
                validateNestedLambdaFunctionArguments(context, module, index.receiver(), types, errors);
                validateNestedLambdaFunctionArguments(context, module, index.index(), types, errors);
                if (index.hasEndIndex()) {
                    validateNestedLambdaFunctionArguments(context, module, index.endIndex(), types, errors);
                }
            }
            case LambdaExpression lambda ->
                    validateNestedLambdaFunctionArguments(
                            context,
                            module,
                            lambda.body(),
                            validationDeclaredLambdaTypes(lambda, types),
                            errors
                    );
            case ListLiteral literal -> literal.values().forEach(value ->
                    validateNestedLambdaFunctionArguments(context, module, value, types, errors));
            case MatchExpression match -> {
                validateNestedLambdaFunctionArguments(context, module, match.value(), types, errors);
                for (var matchCase : match.cases()) {
                    if (matchCase.hasLiteral()) {
                        validateNestedLambdaFunctionArguments(context, module, matchCase.literal(), types, errors);
                    }
                    if (matchCase.hasGuard()) {
                        validateNestedLambdaFunctionArguments(context, module, matchCase.guard(), types, errors);
                    }
                    validateNestedLambdaFunctionArguments(context, module, matchCase.body(), types, errors);
                }
            }
            case MethodCallExpression call -> {
                validateKnownExtensionMethodReceiver(context, module, call, types, errors);
                validateResultFlatMapMapper(context, module, call, types, errors);
                validateNestedLambdaFunctionArguments(context, module, call.receiver(), types, errors);
                var receiverType = validationExpressionType(context, module, call.receiver(), types);
                var valueType = receiverType != null && receiverType.arguments().size() == 1
                        ? receiverType.arguments().getFirst()
                        : null;
                for (var argument : call.arguments()) {
                    if (argument instanceof LambdaExpression mapper && valueType != null) {
                        validateNestedLambdaFunctionArguments(
                                context,
                                module,
                                mapper.body(),
                                validationLambdaTypes(mapper, valueType, types),
                                errors
                        );
                    } else {
                        validateNestedLambdaFunctionArguments(context, module, argument, types, errors);
                    }
                }
            }
            case ReduceExpression reduce -> {
                validateNestedLambdaFunctionArguments(context, module, reduce.receiver(), types, errors);
                validateNestedLambdaFunctionArguments(context, module, reduce.initial(), types, errors);
                var reduceTypes = new LinkedHashMap<>(types);
                var accumulatorType = validationExpressionType(context, module, reduce.initial(), types);
                if (accumulatorType != null) {
                    reduceTypes.put(reduce.accumulatorName(), accumulatorType);
                }
                var receiverType = validationExpressionType(context, module, reduce.receiver(), types);
                var valueType = validationIterableElementType(receiverType);
                if (valueType != null && !reduce.valueName().isBlank() && !reduce.valueName().equals("_")) {
                    reduceTypes.put(reduce.valueName(), valueType);
                }
                validateNestedLambdaFunctionArguments(context, module, reduce.body(), reduceTypes, errors);
            }
            case SetLiteral literal -> literal.values().forEach(value ->
                    validateNestedLambdaFunctionArguments(context, module, value, types, errors));
            case ThrowExpression throwExpression ->
                    validateNestedLambdaFunctionArguments(context, module, throwExpression.value(), types, errors);
            case TryCatchExpression tryCatch -> {
                validateNestedLambdaFunctionArguments(context, module, tryCatch.body(), types, errors);
                tryCatch.branches().forEach(branch ->
                        validateNestedLambdaFunctionArguments(context, module, branch.catchBody(), types, errors));
            }
            case TupleLiteral literal -> literal.values().forEach(value ->
                    validateNestedLambdaFunctionArguments(context, module, value, types, errors));
            case UnaryExpression unary ->
                    validateNestedLambdaFunctionArguments(context, module, unary.expression(), types, errors);
            case WithExpression with -> {
                validateNestedLambdaFunctionArguments(context, module, with.receiver(), types, errors);
                with.fields().forEach(field ->
                        validateNestedLambdaFunctionArguments(context, module, field.value(), types, errors));
            }
            default -> {
            }
        }
    }

    private void validateKnownExtensionMethodReceiver(
            Context context,
            ParsedModule module,
            MethodCallExpression call,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        var receiverType = validationExpressionType(context, module, call.receiver(), types);
        validateKnownExtensionMethodReceiver(
                context,
                module,
                call.name(),
                call.arguments(),
                receiverType,
                call.location(),
                errors
        );
    }

    private void validateKnownQualifiedExtensionMethodReceiver(
            Context context,
            ParsedModule module,
            FunctionCallExpression call,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        var separator = call.name().lastIndexOf('.');
        if (separator <= 0 || separator == call.name().length() - 1) {
            return;
        }
        var receiverName = call.name().substring(0, separator);
        var receiverType = types.get(receiverName);
        if (receiverType == null) {
            receiverType = context.constantType(module, receiverName);
        }
        validateKnownExtensionMethodReceiver(
                context,
                module,
                call.name().substring(separator + 1),
                call.arguments(),
                receiverType,
                call.location(),
                errors
        );
    }

    private void validateKnownExtensionMethodReceiver(
            Context context,
            ParsedModule module,
            String methodName,
            List<Expression> arguments,
            TypeReference receiverType,
            SourceLocation location,
            List<CompilerError> errors
    ) {
        if (receiverType == null || receiverType.name().isBlank()
                || unqualified(receiverType.name()).equals("any")) {
            return;
        }
        var arity = arguments.size();
        var receiverTypes = context.extensionMethodReceiverTypes(module, methodName, arity);
        var receiverName = unqualified(receiverType.name());
        if (receiverTypes.contains(receiverName)) {
            return;
        }
        if (context.objectMethodExists(module, receiverName, methodName, arity)) {
            return;
        }
        if (methodName.equals("size") && arity == 0 && SIZE_RECEIVER_TYPES.contains(receiverName)) {
            return;
        }
        if (NATIVE_METHOD_ARITIES
                .getOrDefault(receiverName, Map.of())
                .getOrDefault(methodName, Set.of())
                .contains(arity)) {
            return;
        }
        if (intrinsicCollectionMethod(receiverName, methodName, arguments)) {
            return;
        }
        if (receiverName.equals("Assert")
                && ASSERT_METHOD_ARITIES.getOrDefault(methodName, Set.of()).contains(arity)) {
            return;
        }
        if (arity == 0
                && context.primitiveConversionMethod(module, receiverName, methodName)
                && context.extensionMethodArities(module, receiverName, methodName).isEmpty()) {
            return;
        }
        if (arity == 0
                && ((methodName.equals("name") && context.enumType(module, receiverName))
                || (methodName.equals("to_string")
                && context.numericPrimitiveBackedType(module, receiverName)))) {
            return;
        }
        var expectedArities = new TreeSet<>(context.extensionMethodArities(module, receiverName, methodName));
        if (expectedArities.isEmpty()) {
            var backingReceiverName = context.primitiveBackingType(module, receiverName);
            if (backingReceiverName != null) {
                expectedArities.addAll(context.extensionMethodArities(module, backingReceiverName, methodName));
            }
        }
        expectedArities.addAll(STANDARD_METHOD_ARITIES
                .getOrDefault(receiverName, Map.of())
                .getOrDefault(methodName, Set.of()));
        if (expectedArities.contains(arity)) {
            return;
        }
        if (!expectedArities.isEmpty()) {
            var expected = String.join(" or ", expectedArities.stream().map(Object::toString).toList());
            var argumentLabel = expectedArities.size() == 1 && expectedArities.iterator().next() == 1
                    ? " argument"
                    : " arguments";
            errors.add(error(
                    module,
                    location,
                    "Method `" + methodName + "` on receiver type `" + displayType(receiverType)
                            + "` expects " + expected + argumentLabel + ", but received " + arity + "."
            ));
            return;
        }
        var standardMethods = STANDARD_METHODS_BY_RECEIVER.get(receiverName);
        if (standardMethods != null && standardMethods.contains(methodName)) {
            return;
        }
        var methodReceiverTypes = context.extensionMethodReceiverTypes(module, methodName);
        var wrappedReceiverType = wrappedExtensionReceiverType(receiverType, methodReceiverTypes);
        if (wrappedReceiverType.isPresent()) {
            var wrappedName = wrappedReceiverType.orElseThrow();
            var expectedTypes = receiverTypes.contains(wrappedName)
                    ? String.join("`, `", receiverTypes)
                    : wrappedName;
            var hint = "; extract a `" + wrappedName + "` value before calling the method";
            errors.add(error(
                    module,
                    location,
                    "Method `" + methodName + "` requires receiver type `" + expectedTypes
                            + "`, but the receiver has type `" + displayType(receiverType) + "`" + hint + "."
            ));
            return;
        }
        if (receiverTypes.isEmpty() && standardMethods != null) {
            errors.add(error(
                    module,
                    location,
                    "Method `" + methodName + "` is not defined for receiver type `"
                            + displayType(receiverType) + "`."
            ));
            return;
        }
        if (receiverTypes.isEmpty()) {
            return;
        }
    }

    private boolean intrinsicCollectionMethod(
            String receiverName,
            String methodName,
            List<Expression> arguments
    ) {
        if (!COLLECTION_RECEIVER_TYPES.contains(receiverName)) {
            return false;
        }
        var arity = arguments.size();
        if (COLLECTION_ONE_ARGUMENT_METHODS.contains(methodName)
                && arity == 1
                && callableCollectionArgument(arguments.getFirst())) {
            return true;
        }
        if (COLLECTION_TWO_ARGUMENT_METHODS.contains(methodName)
                && arity == 2
                && callableCollectionArgument(arguments.get(1))) {
            return true;
        }
        if (methodName.equals("is_empty") && arity == 0) {
            return true;
        }
        if ((receiverName.equals("List") || SEQ_RECEIVER_TYPES.contains(receiverName))
                && methodName.equals("fold")
                && arity == 1
                && callableCollectionArgument(arguments.getFirst())) {
            return true;
        }
        if (SEQ_RECEIVER_TYPES.contains(receiverName)
                && SEQ_ZERO_ARGUMENT_METHODS.contains(methodName)
                && arity == 0) {
            return true;
        }
        return receiverName.equals("Set")
                && ((SET_ZERO_ARGUMENT_METHODS.contains(methodName) && arity == 0)
                || (SET_ONE_ARGUMENT_METHODS.contains(methodName) && arity == 1));
    }

    private boolean callableCollectionArgument(Expression argument) {
        return argument instanceof LambdaExpression || argument instanceof FunctionReferenceExpression;
    }

    private Optional<String> wrappedExtensionReceiverType(
            TypeReference receiverType,
            Set<String> receiverTypes
    ) {
        for (var argument : receiverType.arguments()) {
            var argumentName = unqualified(argument.name());
            if (receiverTypes.contains(argumentName)) {
                return Optional.of(argumentName);
            }
            var nested = wrappedExtensionReceiverType(argument, receiverTypes);
            if (nested.isPresent()) {
                return nested;
            }
        }
        return Optional.empty();
    }

    private void validatePipeRightOperand(
            Context context,
            ParsedModule module,
            BinaryExpression binary,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        if (!PIPE_OPERATORS.contains(binary.operator()) || callablePipeRight(binary.right())) {
            return;
        }
        if (binary.operator().equals("|")) {
            var leftType = validationExpressionType(context, module, binary.left(), types);
            if (leftType == null || unqualified(leftType.name()).equals("bool")) {
                return;
            }
        }
        errors.add(error(
                module,
                binary.location(),
                "Operator `" + binary.operator()
                        + "` requires a lambda or function reference on its right-hand side; found "
                        + pipeOperandDescription(binary.right()) + "."
        ));
    }

    private boolean callablePipeRight(Expression expression) {
        if (expression instanceof LambdaExpression || expression instanceof FunctionReferenceExpression) {
            return true;
        }
        if (expression instanceof MethodCallExpression call) {
            return callablePipeRight(call.receiver());
        }
        return false;
    }

    private String pipeOperandDescription(Expression expression) {
        return switch (expression) {
            case FunctionCallExpression ignored -> "a function call";
            case MethodCallExpression ignored -> "a method call";
            case VariableExpression ignored -> "a variable";
            default -> "an expression";
        };
    }

    private void validateDataLiteralFieldTypes(
            Context context,
            ParsedModule module,
            DataLiteral literal,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        var literalTypeName = nominalTypeName(literal.typeName());
        var declarationOwner = context.dataDeclarationOwner(module, literalTypeName);
        if (declarationOwner == null) {
            validateLinkedDataLiteralFieldTypes(context, module, literal, types, errors);
            return;
        }
        var declaration = context.localDataDeclaration(declarationOwner, literalTypeName);
        for (var fieldIndex = 0; fieldIndex < literal.fields().size(); fieldIndex++) {
            var field = literal.fields().get(fieldIndex);
            if (field.spread()) {
                continue;
            }
            var declaredField = declaredDataField(declaration, field, fieldIndex);
            if (declaredField == null) {
                continue;
            }
            var expected = declaredField.typeReference();
            var actual = validationExpressionType(context, module, field.value(), types);
            var primitive = context.primitiveBackedTypeDeclaration(declarationOwner, expected.name());
            if (primitiveBackedFieldType(expected, primitive) && actual != null && !sameTypeName(expected, actual)) {
                errors.add(error(
                        module,
                        field.location(),
                        "Field `" + declaredField.name() + "` of data `" + literalTypeName
                                + "` has type `" + displayType(actual) + "`, but requires primitive-backed type `"
                                + displayType(expected) + "`; construct `" + displayType(expected)
                                + "` explicitly and unwrap its `Result` before constructing `"
                                + literalTypeName + "`."
                ));
                continue;
            }
            if (functionTypeName(expected.name())) {
                if (field.value() instanceof LambdaExpression lambda) {
                    validateLambdaCompatibility(
                            context,
                            module,
                            lambda,
                            expected,
                            types,
                            "Field `" + declaredField.name() + "` of data `" + literalTypeName + "`",
                            errors
                    );
                } else if (field.value() instanceof FunctionReferenceExpression reference) {
                    validateFunctionReferenceCompatibility(
                            context,
                            module,
                            reference,
                            expected,
                            "Field `" + declaredField.name() + "` of data `" + literalTypeName + "`",
                            errors
                    );
                } else if (actual != null && !callableExpression(field.value(), actual)) {
                    errors.add(error(
                            module,
                            field.location(),
                        "Field `" + field.name() + "` of data `" + literalTypeName
                                + "` requires callable type `" + displayType(expected) + "`."
                    ));
                }
                continue;
            }
            if (field.value() instanceof LambdaExpression || field.value() instanceof FunctionReferenceExpression) {
                errors.add(error(
                        module,
                        field.location(),
                        "Field `" + field.name() + "` of data `" + literalTypeName
                                + "` requires `" + displayType(expected) + "`, but a callable value was provided."
                ));
                continue;
            }
            if (actual != null && distinctSequenceListMismatch(expected, actual)) {
                errors.add(error(
                        module,
                        field.location(),
                        "Field `" + field.name() + "` of data `" + literalTypeName
                                + "` has type `" + displayType(actual) + "`, but requires `"
                                + displayType(expected) + "`; use an explicit `to_seq` or `as_list` conversion."
                ));
            }
        }
    }

    private void validateLinkedDataLiteralFieldTypes(
            Context context,
            ParsedModule module,
            DataLiteral literal,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        var literalTypeName = nominalTypeName(literal.typeName());
        var declaredFields = context.linkedDataFields(module, literalTypeName);
        if (declaredFields == null) {
            return;
        }
        for (var fieldIndex = 0; fieldIndex < literal.fields().size(); fieldIndex++) {
            var field = literal.fields().get(fieldIndex);
            if (field.spread()) {
                continue;
            }
            var declaredField = linkedDataField(declaredFields, field, fieldIndex);
            if (declaredField == null) {
                continue;
            }
            var expected = declaredField.typeReference();
            var actual = validationExpressionType(context, module, field.value(), types);
            if (primitiveBackedFieldType(expected, null) && actual != null && !sameTypeName(expected, actual)) {
                errors.add(error(
                        module,
                        field.location(),
                        "Field `" + declaredField.name() + "` of data `" + literalTypeName
                                + "` has type `" + displayType(actual) + "`, but requires primitive-backed type `"
                                + displayType(expected) + "`; construct `" + displayType(expected)
                                + "` explicitly and unwrap its `Result` before constructing `"
                                + literalTypeName + "`."
                ));
            }
        }
    }

    private LinkedDataField linkedDataField(List<LinkedDataField> fields, DataField field, int fieldIndex) {
        if (field.name().equals("$" + fieldIndex) && fieldIndex < fields.size()) {
            return fields.get(fieldIndex);
        }
        return fields.stream()
                .filter(candidate -> candidate.name().equals(field.name()))
                .findFirst()
                .orElse(null);
    }

    private DataFieldDeclaration declaredDataField(DataDeclaration declaration, DataField field, int fieldIndex) {
        if (field.name().equals("$" + fieldIndex) && fieldIndex < declaration.fields().size()) {
            return declaration.fields().get(fieldIndex);
        }
        return declaration.fields().stream()
                .filter(candidate -> candidate.name().equals(field.name()))
                .findFirst()
                .orElse(null);
    }

    private boolean sameTypeName(TypeReference expected, TypeReference actual) {
        return nominalTypeName(expected.name()).equals(nominalTypeName(actual.name()));
    }

    private boolean primitiveBackedFieldType(
            TypeReference expected,
            PrimitiveBackedTypeDeclaration declaration
    ) {
        if (declaration != null) {
            return true;
        }
        var typeName = nominalTypeName(expected.name());
        return !typeName.isEmpty()
                && Character.isLowerCase(typeName.charAt(0))
                && !BUILTIN_TYPES.contains(typeName);
    }

    private String nominalTypeName(String name) {
        var typeName = unqualified(name);
        var rawPrefix = "__capy_raw|";
        return typeName.startsWith(rawPrefix) ? typeName.substring(rawPrefix.length()) : typeName;
    }

    private boolean callableExpression(Expression expression, TypeReference inferredType) {
        return expression instanceof LambdaExpression
                || expression instanceof FunctionReferenceExpression
                || (inferredType != null && functionTypeName(inferredType.name()));
    }

    private void validateFunctionReturnType(
            Context context,
            ParsedModule module,
            FunctionDeclaration function,
            List<CompilerError> errors
    ) {
        if (function.returnType().name().isBlank()) {
            return;
        }
        if (function.body() instanceof LambdaExpression lambda) {
            if (functionTypeName(function.returnType().name())) {
                validateLambdaCompatibility(
                        context,
                        module,
                        lambda,
                        function.returnType(),
                        parameterTypes(function),
                        "Function `" + localFunctionDisplayName(function.name()) + "`",
                        errors
                );
            } else {
                errors.add(error(
                        module,
                        function.location(),
                        "Function `" + localFunctionDisplayName(function.name()) + "` returns a lambda, but declares `"
                                + displayType(function.returnType()) + "`."
                ));
            }
            return;
        }
        if (function.body() instanceof FunctionReferenceExpression reference) {
            if (functionTypeName(function.returnType().name())) {
                validateFunctionReferenceCompatibility(
                        context,
                        module,
                        reference,
                        function.returnType(),
                        "Function `" + localFunctionDisplayName(function.name()) + "`",
                        errors
                );
            } else {
                errors.add(error(
                        module,
                        function.location(),
                        "Function `" + localFunctionDisplayName(function.name())
                                + "` returns a function reference, but declares `" + displayType(function.returnType()) + "`."
                ));
            }
            return;
        }
        var actual = validationExpressionType(context, module, function.body(), parameterTypes(function));
        if (actual == null || returnTypeAssignable(context, module, function.returnType(), actual)) {
            return;
        }
        if (!distinctSequenceListMismatch(function.returnType(), actual)
                && !definiteNestedTypeMismatch(function.returnType(), actual)
                && !definiteEffectMismatch(function.returnType(), actual, function.body())
                && !returnTypeAssignable(context, module, actual, function.returnType())) {
            return;
        }
        if (!distinctSequenceListMismatch(function.returnType(), actual)) {
            errors.add(error(
                    module,
                    function.location(),
                    "Function `" + localFunctionDisplayName(function.name()) + "` returns `"
                            + displayType(actual) + "`, but declares `"
                            + displayType(function.returnType()) + "`."
            ));
            return;
        }
        errors.add(error(
                module,
                function.location(),
                "Function `" + localFunctionDisplayName(function.name()) + "` returns `" + displayType(actual)
                        + "`, but declares `" + displayType(function.returnType())
                        + "`; use an explicit `to_seq` or `as_list` conversion."
        ));
    }

    private boolean returnTypeAssignable(
            Context context,
            ParsedModule module,
            TypeReference expected,
            TypeReference actual
    ) {
        return returnTypeAssignable(context, module, expected, actual, new HashSet<>());
    }

    private boolean returnTypeAssignable(
            Context context,
            ParsedModule module,
            TypeReference expected,
            TypeReference actual,
            Set<String> visited
    ) {
        var expectedName = nominalTypeName(expected.name());
        var actualName = nominalTypeName(actual.name());
        if (expectedName.isBlank()
                || actualName.isBlank()
                || expectedName.equals("any")
                || actualName.equals("any")
                || isSingleLetterGeneric(expectedName)
                || isSingleLetterGeneric(actualName)) {
            return true;
        }
        if (expectedName.equals("int") && Set.of("size", "index").contains(actualName)) {
            return true;
        }
        if (expectedName.equals(actualName)) {
            if (expected.arguments().isEmpty() || actual.arguments().isEmpty()) {
                return true;
            }
            if (expected.arguments().size() != actual.arguments().size()) {
                return false;
            }
            for (var index = 0; index < expected.arguments().size(); index++) {
                if (!returnTypeAssignable(
                        context,
                        module,
                        expected.arguments().get(index),
                        actual.arguments().get(index),
                        visited
                )) {
                    return false;
                }
            }
            return true;
        }

        var key = displayType(expected) + "<-" + displayType(actual);
        if (!visited.add(key)) {
            return false;
        }
        try {
            for (var parent : context.directParentTypes(module, actual)) {
                if (returnTypeAssignable(context, module, expected, parent, visited)) {
                    return true;
                }
            }
            return false;
        } finally {
            visited.remove(key);
        }
    }

    private void validateDistinctCollectionBinding(
            Context context,
            ParsedModule module,
            LetBinding binding,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        if (binding.typeReference().name().isBlank()) {
            return;
        }
        var actual = validationExpressionType(context, module, binding.value(), types);
        if (actual == null) {
            return;
        }
        if (binding.operator().equals("<-") && actual.arguments().size() == 1) {
            actual = actual.arguments().getFirst();
        }
        if (!distinctSequenceListMismatch(binding.typeReference(), actual)) {
            if (!definiteNestedTypeMismatch(binding.typeReference(), actual)) {
                return;
            }
            errors.add(error(
                    module,
                    binding.location(),
                    "Binding `" + binding.name() + "` has type `" + displayType(actual)
                            + "`, but declares `" + displayType(binding.typeReference()) + "`."
            ));
            return;
        }
        errors.add(error(
                module,
                binding.location(),
                "Binding `" + binding.name() + "` has type `" + displayType(actual)
                        + "`, but declares `" + displayType(binding.typeReference())
                        + "`; use an explicit `to_seq` or `as_list` conversion."
        ));
    }

    private boolean distinctSequenceListMismatch(TypeReference expected, TypeReference actual) {
        var expectedName = unqualified(expected.name());
        var actualName = unqualified(actual.name());
        if ((expectedName.equals("Seq") && actualName.equals("List"))
                || (expectedName.equals("List") && actualName.equals("Seq"))) {
            return true;
        }
        if (!expectedName.equals(actualName) || expected.arguments().size() != actual.arguments().size()) {
            return false;
        }
        for (var index = 0; index < expected.arguments().size(); index++) {
            if (distinctSequenceListMismatch(expected.arguments().get(index), actual.arguments().get(index))) {
                return true;
            }
        }
        return false;
    }

    private boolean definiteNestedTypeMismatch(TypeReference expected, TypeReference actual) {
        var expectedName = unqualified(expected.name());
        var actualName = unqualified(actual.name());
        if (!expectedName.equals(actualName)
                || expected.arguments().size() != actual.arguments().size()) {
            return false;
        }
        for (var index = 0; index < expected.arguments().size(); index++) {
            if (definiteTypeArgumentMismatch(expected.arguments().get(index), actual.arguments().get(index))) {
                return true;
            }
        }
        return false;
    }

    private boolean definiteEffectMismatch(TypeReference expected, TypeReference actual, Expression body) {
        return unqualified(expected.name()).equals("Effect")
                && !unqualified(actual.name()).equals("Effect")
                && !effectfulBlock(body);
    }

    private boolean effectfulBlock(Expression expression) {
        return expression instanceof BlockExpression block
                && block.bindings().stream().anyMatch(binding -> binding.operator().equals("<-"));
    }

    private boolean definiteTypeArgumentMismatch(TypeReference expected, TypeReference actual) {
        var expectedName = unqualified(expected.name());
        var actualName = unqualified(actual.name());
        if (expectedName.isBlank() || actualName.isBlank()
                || expectedName.equals("any") || actualName.equals("any")
                || isSingleLetterGeneric(expectedName) || isSingleLetterGeneric(actualName)) {
            return false;
        }
        if (!expectedName.equals(actualName)) {
            return true;
        }
        if (expected.arguments().size() != actual.arguments().size()) {
            return false;
        }
        for (var index = 0; index < expected.arguments().size(); index++) {
            if (definiteTypeArgumentMismatch(expected.arguments().get(index), actual.arguments().get(index))) {
                return true;
            }
        }
        return false;
    }

    private void validateResultFlatMapMapper(
            Context context,
            ParsedModule module,
            MethodCallExpression call,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        if (!call.name().equals("flat_map")
                || call.arguments().size() != 1
                || !(call.arguments().getFirst() instanceof LambdaExpression mapper)) {
            return;
        }
        var receiverType = validationExpressionType(context, module, call.receiver(), types);
        if (receiverType == null || !"Result".equals(unqualified(receiverType.name()))) {
            return;
        }
        var mapperTypes = new LinkedHashMap<>(types);
        if (receiverType.arguments().size() == 1 && mapper.parameters().size() == 1) {
            mapperTypes.put(decodedLambdaParameterName(mapper.parameters().getFirst()), receiverType.arguments().getFirst());
        }
        var mapperReturnType = validationExpressionType(context, module, mapper.body(), mapperTypes);
        if (mapperReturnType == null || resultCompatibleType(mapperReturnType)) {
            return;
        }
        errors.add(error(
                module,
                call.location(),
                "Result.flat_map mapper must return `Result`, but it returns `"
                        + displayType(mapperReturnType) + "`."
        ));
    }

    private boolean resultCompatibleType(TypeReference type) {
        return switch (unqualified(type.name())) {
            case "Result", "Success", "Error" -> true;
            default -> false;
        };
    }

    private TypeReference validationExpressionType(
            Context context,
            ParsedModule module,
            Expression expression,
            Map<String, TypeReference> types
    ) {
        return switch (expression) {
            case BoolLiteral ignored -> new TypeReference("bool", List.of());
            case IntLiteral ignored -> new TypeReference("int", List.of());
            case LongLiteral ignored -> new TypeReference("long", List.of());
            case FloatLiteral ignored -> new TypeReference("float", List.of());
            case DoubleLiteral ignored -> new TypeReference("double", List.of());
            case StringLiteral ignored -> new TypeReference("String", List.of());
            case VariableExpression variable -> {
                var localType = types.get(variable.name());
                yield localType == null ? context.constantType(module, variable.name()) : localType;
            }
            case FunctionCallExpression call -> {
                var callableType = types.get(call.name());
                if (callableType != null && functionTypeName(callableType.name())) {
                    yield functionTypeReturnType(callableType);
                }
                var function = context.functionDeclaration(module, call.name(), call.arguments().size());
                if (function != null) {
                    yield function.returnType();
                }
                yield validationStandardFunctionType(context, module, call, types);
            }
            case FieldAccessExpression access -> validationFieldAccessType(context, module, access, types);
            case BinaryExpression binary -> validationBinaryType(context, module, binary, types);
            case DataLiteral literal -> validationDataLiteralType(context, module, literal, types);
            case ListLiteral literal -> validationListLiteralType(context, module, literal, types);
            case BlockExpression block -> validationBlockType(context, module, block, types);
            case MethodCallExpression call -> validationMethodCallType(context, module, call, types);
            case ReduceExpression reduce -> validationExpressionType(context, module, reduce.initial(), types);
            default -> null;
        };
    }

    private TypeReference validationFieldAccessType(
            Context context,
            ParsedModule module,
            FieldAccessExpression access,
            Map<String, TypeReference> types
    ) {
        var receiverType = validationExpressionType(context, module, access.receiver(), types);
        if (receiverType == null) {
            return null;
        }
        var receiverName = unqualified(receiverType.name());
        if (access.name().equals("value")
                && Set.of("Cons", "Some", "Success").contains(receiverName)
                && !receiverType.arguments().isEmpty()) {
            return receiverType.arguments().getFirst();
        }
        var owner = context.dataDeclarationOwner(module, receiverName);
        if (owner != null) {
            var declaration = context.localDataDeclaration(owner, receiverName);
            var field = declaration.fields().stream()
                    .filter(candidate -> candidate.name().equals(access.name()))
                    .findFirst()
                    .orElse(null);
            if (field != null) {
                var bindings = new LinkedHashMap<String, TypeReference>();
                for (var index = 0;
                     index < declaration.parameters().size() && index < receiverType.arguments().size();
                     index++) {
                    bindings.put(declaration.parameters().get(index), receiverType.arguments().get(index));
                }
                return context.substituteTypeParameters(field.typeReference(), bindings);
            }
        }
        var linkedFields = context.linkedDataFields(module, receiverName);
        if (linkedFields != null) {
            return linkedFields.stream()
                    .filter(field -> field.name().equals(access.name()))
                    .map(LinkedDataField::typeReference)
                    .findFirst()
                    .orElse(null);
        }
        return null;
    }

    private TypeReference validationStandardFunctionType(
            Context context,
            ParsedModule module,
            FunctionCallExpression call,
            Map<String, TypeReference> types
    ) {
        if (!unqualified(call.name()).equals("to_seq") || call.arguments().size() != 1) {
            return null;
        }
        var argumentType = validationExpressionType(context, module, call.arguments().getFirst(), types);
        if (argumentType == null || !unqualified(argumentType.name()).equals("List")) {
            return null;
        }
        return new TypeReference("Seq", argumentType.arguments());
    }

    private TypeReference validationDataLiteralType(
            Context context,
            ParsedModule module,
            DataLiteral literal,
            Map<String, TypeReference> types
    ) {
        if (unqualified(literal.typeName()).equals("Success") && !literal.fields().isEmpty()) {
            var payload = validationExpressionType(context, module, literal.fields().getFirst().value(), types);
            if (payload != null) {
                return new TypeReference("Result", List.of(payload));
            }
        }
        var typeName = unqualified(literal.typeName());
        if (context.hasConstructor(module, typeName)) {
            return new TypeReference("Result", List.of(new TypeReference(literal.typeName(), List.of())));
        }
        return new TypeReference(literal.typeName(), List.of());
    }

    private TypeReference validationListLiteralType(
            Context context,
            ParsedModule module,
            ListLiteral literal,
            Map<String, TypeReference> types
    ) {
        if (literal.values().isEmpty()) {
            return new TypeReference("List", List.of(new TypeReference("any", List.of())));
        }
        var valueType = validationExpressionType(context, module, literal.values().getFirst(), types);
        return new TypeReference("List", List.of(valueType == null
                ? new TypeReference("any", List.of())
                : valueType));
    }

    private TypeReference validationBinaryType(
            Context context,
            ParsedModule module,
            BinaryExpression binary,
            Map<String, TypeReference> types
    ) {
        var receiverType = validationExpressionType(context, module, binary.left(), types);
        if (binary.operator().equals("+") && receiverType != null) {
            var receiverName = unqualified(receiverType.name());
            if (receiverName.equals("Seq") || receiverName.equals("List")) {
                return receiverType;
            }
            if (receiverName.equals("String")) {
                return new TypeReference("String", List.of());
            }
        }
        if (!binary.operator().equals("|") || !(binary.right() instanceof LambdaExpression mapper)) {
            return null;
        }
        if (receiverType == null
                || validationIterableElementType(receiverType) == null) {
            return null;
        }
        var mapperTypes = validationLambdaTypes(mapper, validationIterableElementType(receiverType), types);
        var mappedType = validationExpressionType(context, module, mapper.body(), mapperTypes);
        if (!mapper.parameters().isEmpty()
                && decodedLambdaParameterType(mapper.parameters().getFirst()) == null
                && mapper.body() instanceof FieldAccessExpression) {
            // Keep untyped collection lambdas on the validator's conservative inference path. Resolving a user-data
            // field here can make legacy collection pipelines appear more specific than the linked program metadata.
            mappedType = null;
        }
        return mappedType == null ? null : new TypeReference("Seq", List.of(mappedType));
    }

    private TypeReference validationBlockType(
            Context context,
            ParsedModule module,
            BlockExpression block,
            Map<String, TypeReference> types
    ) {
        var blockTypes = new LinkedHashMap<>(types);
        for (var binding : block.bindings()) {
            var bindingType = binding.typeReference().name().isBlank()
                    ? validationExpressionType(context, module, binding.value(), blockTypes)
                    : binding.typeReference();
            if (bindingType != null) {
                blockTypes.put(binding.name(), bindingType);
            }
        }
        return validationExpressionType(context, module, block.result(), blockTypes);
    }

    private TypeReference validationMethodCallType(
            Context context,
            ParsedModule module,
            MethodCallExpression call,
            Map<String, TypeReference> types
    ) {
        var receiverType = validationExpressionType(context, module, call.receiver(), types);
        if (receiverType == null) {
            return null;
        }
        var receiverName = unqualified(receiverType.name());
        if (receiverName.equals("String") && call.name().equals("to_int") && call.arguments().isEmpty()) {
            return new TypeReference("Result", List.of(new TypeReference("int", List.of())));
        }
        if (receiverName.equals("Seq") && call.name().equals("as_list") && call.arguments().isEmpty()) {
            return new TypeReference("List", receiverType.arguments());
        }
        if (!STANDARD_METHODS_BY_RECEIVER.getOrDefault(receiverName, Set.of()).contains(call.name())) {
            var extensionReturnType = context.extensionMethodReturnType(
                    module,
                    receiverName,
                    call.name(),
                    call.arguments().size()
            );
            if (extensionReturnType != null) {
                return extensionReturnType;
            }
        }
        if (call.arguments().size() != 1
                || !(call.arguments().getFirst() instanceof LambdaExpression mapper)
                || receiverType.arguments().size() != 1) {
            return null;
        }
        var mapperTypes = validationLambdaTypes(mapper, receiverType.arguments().getFirst(), types);
        var mapperReturnType = validationExpressionType(context, module, mapper.body(), mapperTypes);
        if (mapperReturnType == null) {
            return null;
        }
        if (receiverName.equals("Result")) {
            return switch (call.name()) {
                case "map" -> new TypeReference(receiverType.name(), List.of(mapperReturnType));
                case "flat_map" -> mapperReturnType;
                default -> null;
            };
        }
        if ((receiverName.equals("List") || receiverName.equals("Seq")) && call.name().equals("map")) {
            return new TypeReference("Seq", List.of(mapperReturnType));
        }
        return null;
    }

    private TypeReference validationIterableElementType(TypeReference receiverType) {
        if (receiverType == null) {
            return null;
        }
        var receiverName = unqualified(receiverType.name());
        if ((receiverName.equals("List") || receiverName.equals("Seq"))
                && receiverType.arguments().size() == 1) {
            return receiverType.arguments().getFirst();
        }
        if (receiverName.equals("String")) {
            return new TypeReference("String", List.of());
        }
        return null;
    }

    private void validateSequenceConcatOperands(
            Context context,
            ParsedModule module,
            BinaryExpression binary,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        if (!binary.operator().equals("+")) {
            return;
        }
        var leftType = validationExpressionType(context, module, binary.left(), types);
        if (leftType == null || !unqualified(leftType.name()).equals("Seq")) {
            return;
        }
        var rightType = validationExpressionType(context, module, binary.right(), types);
        if (rightType == null || Set.of("Seq", "Cons", "End").contains(unqualified(rightType.name()))) {
            return;
        }
        if (leftType.arguments().size() == 1
                && compatibleFunctionArgument(leftType.arguments().getFirst(), rightType)) {
            return;
        }
        errors.add(error(
                module,
                binary.location(),
                "Operator `+` on `" + displayType(leftType) + "` requires another `Seq` or a compatible element, "
                        + "but the right operand has type `" + displayType(rightType) + "`."
        ));
    }

    private Map<String, TypeReference> validationLambdaTypes(
            LambdaExpression lambda,
            TypeReference valueType,
            Map<String, TypeReference> types
    ) {
        var mapperTypes = new LinkedHashMap<>(types);
        if (lambda.parameters().size() == 1) {
            var parameter = lambda.parameters().getFirst();
            var declaredType = decodedLambdaParameterType(parameter);
            mapperTypes.put(decodedLambdaParameterName(parameter), declaredType == null ? valueType : declaredType);
        }
        return mapperTypes;
    }

    private Map<String, TypeReference> validationDeclaredLambdaTypes(
            LambdaExpression lambda,
            Map<String, TypeReference> types
    ) {
        var lambdaTypes = new LinkedHashMap<>(types);
        for (var parameter : lambda.parameters()) {
            var name = decodedLambdaParameterName(parameter);
            var declaredType = decodedLambdaParameterType(parameter);
            if (!name.equals("_") && declaredType != null) {
                lambdaTypes.put(name, declaredType);
            }
        }
        return lambdaTypes;
    }

    private void validateNestedLambdaCall(
            Context context,
            ParsedModule module,
            FunctionCallExpression call,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        var standardArities = context.standardFunctionArities(module, call.name());
        if (standardArities != null && !standardArities.contains(call.arguments().size())) {
            errors.add(error(
                    module,
                    call.location(),
                    "Function `" + call.name() + "` does not accept " + call.arguments().size() + " argument(s)."
            ));
            return;
        }
        var function = context.functionDeclaration(module, call.name(), call.arguments().size());
        if (function == null) {
            var callableType = types.get(call.name());
            if (callableType != null && functionTypeName(callableType.name())) {
                validateFunctionValueCall(context, module, call, callableType, types, errors);
            }
            return;
        }
        for (var index = 0; index < call.arguments().size(); index++) {
            var argument = call.arguments().get(index);
            var expected = function.parameters().get(index).typeReference();
            if (argument instanceof LambdaExpression lambda) {
                if (!functionTypeName(expected.name())) {
                    errors.add(error(
                            module,
                            lambda.location(),
                            "Argument " + (index + 1) + " of function `" + call.name()
                                    + "` requires `" + displayType(expected) + "`, but a lambda was provided."
                    ));
                } else {
                    validateLambdaCompatibility(
                            context,
                            module,
                            lambda,
                            expected,
                            types,
                            "Argument " + (index + 1) + " of function `" + call.name() + "`",
                            errors
                    );
                }
                continue;
            }
            if (argument instanceof FunctionReferenceExpression reference) {
                if (!functionTypeName(expected.name())) {
                    errors.add(error(
                            module,
                            reference.location(),
                            "Argument " + (index + 1) + " of function `" + call.name()
                                    + "` requires `" + displayType(expected) + "`, but a function reference was provided."
                    ));
                } else {
                    validateFunctionReferenceCompatibility(
                            context,
                            module,
                            reference,
                            expected,
                            "Argument " + (index + 1) + " of function `" + call.name() + "`",
                            errors
                    );
                }
                continue;
            }
            var inferred = validationExpressionType(context, module, argument, types);
            if (inferred != null && distinctSequenceListMismatch(expected, inferred)) {
                errors.add(error(
                        module,
                        call.location(),
                        "Argument " + (index + 1) + " of function `" + call.name() + "` has type `"
                                + displayType(inferred) + "`, but `" + displayType(expected)
                                + "` is required; use an explicit `to_seq` or `as_list` conversion."
                ));
                continue;
            }
            if (inferred != null
                    && call.name().contains("__local__")
                    && !compatibleFunctionArgument(expected, inferred)) {
                errors.add(error(
                        module,
                        call.location(),
                        "Argument " + (index + 1) + " of function `" + localFunctionDisplayName(call.name())
                                + "` has type `" + displayType(inferred) + "`, but `"
                                + displayType(expected) + "` is required."
                ));
                continue;
            }
            var actual = nestedLambdaMappedType(call.arguments().get(index), types);
            if (actual == null) {
                continue;
            }
            if (sameType(actual, expected) || acceptsMappedFunction(expected)) {
                continue;
            }
            errors.add(error(
                    module,
                    call.location(),
                    "Argument " + (index + 1) + " of function `" + call.name() + "` has type `"
                            + displayType(actual) + "`, but `" + displayType(expected) + "` is required."
            ));
        }
    }

    private void validateFunctionValueCall(
            Context context,
            ParsedModule module,
            FunctionCallExpression call,
            TypeReference callableType,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        var parameters = functionTypeParameters(callableType);
        if (parameters.size() != call.arguments().size()) {
            errors.add(error(
                    module,
                    call.location(),
                    "Function value `" + call.name() + "` expects " + parameters.size()
                            + " argument(s), but " + call.arguments().size() + " were provided."
            ));
            return;
        }
        for (var index = 0; index < parameters.size(); index++) {
            var actual = validationExpressionType(context, module, call.arguments().get(index), types);
            if (actual != null && !returnTypeAssignable(context, module, parameters.get(index), actual)) {
                errors.add(error(
                        module,
                        call.location(),
                        "Argument " + (index + 1) + " of function value `" + call.name() + "` has type `"
                                + displayType(actual) + "`, but `" + displayType(parameters.get(index))
                                + "` is required."
                ));
            }
        }
    }

    private void validateLambdaCompatibility(
            Context context,
            ParsedModule module,
            LambdaExpression lambda,
            TypeReference expected,
            Map<String, TypeReference> outerTypes,
            String subject,
            List<CompilerError> errors
    ) {
        var expectedParameters = functionTypeParameters(expected);
        if (lambda.parameters().size() != expectedParameters.size()) {
            errors.add(error(
                    module,
                    lambda.location(),
                    subject + " expects callable type `" + displayType(expected) + "`, but the lambda declares "
                            + lambda.parameters().size() + " parameter(s)."
            ));
            return;
        }
        var lambdaTypes = new LinkedHashMap<>(outerTypes);
        for (var index = 0; index < lambda.parameters().size(); index++) {
            var parameter = lambda.parameters().get(index);
            var contextual = expectedParameters.get(index);
            var declared = decodedLambdaParameterType(parameter);
            if (declared != null && !returnTypeAssignable(context, module, declared, contextual)) {
                errors.add(error(
                        module,
                        lambda.location(),
                        subject + " expects parameter " + (index + 1) + " to accept `" + displayType(contextual)
                                + "`, but the lambda declares `" + displayType(declared) + "`."
                ));
            }
            var name = decodedLambdaParameterName(parameter);
            if (!name.equals("_")) {
                lambdaTypes.put(name, declared == null ? contextual : declared);
            }
        }
        var actualReturn = validationExpressionType(context, module, lambda.body(), lambdaTypes);
        var expectedReturn = functionTypeReturnType(expected);
        if (actualReturn != null && !returnTypeAssignable(context, module, expectedReturn, actualReturn)) {
            errors.add(error(
                    module,
                    lambda.location(),
                    subject + " expects the lambda to return `" + displayType(expectedReturn)
                            + "`, but it returns `" + displayType(actualReturn) + "`."
            ));
        }
    }

    private void validateFunctionReferenceCompatibility(
            Context context,
            ParsedModule module,
            FunctionReferenceExpression reference,
            TypeReference expected,
            String subject,
            List<CompilerError> errors
    ) {
        var expectedParameters = functionTypeParameters(expected);
        var function = context.functionDeclaration(module, reference.name(), expectedParameters.size());
        if (function == null) {
            return;
        }
        for (var index = 0; index < expectedParameters.size(); index++) {
            var declared = function.parameters().get(index).typeReference();
            if (!returnTypeAssignable(context, module, declared, expectedParameters.get(index))) {
                errors.add(error(
                        module,
                        reference.location(),
                        subject + " expects parameter " + (index + 1) + " to accept `"
                                + displayType(expectedParameters.get(index)) + "`, but function `" + reference.name()
                                + "` declares `" + displayType(declared) + "`."
                ));
                return;
            }
        }
        var expectedReturn = functionTypeReturnType(expected);
        if (!returnTypeAssignable(context, module, expectedReturn, function.returnType())) {
            errors.add(error(
                    module,
                    reference.location(),
                    subject + " expects return type `" + displayType(expectedReturn) + "`, but function `"
                            + reference.name() + "` returns `" + displayType(function.returnType()) + "`."
            ));
        }
    }

    private List<TypeReference> functionTypeParameters(TypeReference functionType) {
        var arrow = topLevelFunctionArrow(functionType.name());
        if (arrow < 0) {
            return List.of();
        }
        var text = functionType.name().substring(0, arrow).trim();
        if (text.equals("()")) {
            return List.of();
        }
        if (text.startsWith("(") && text.endsWith(")")) {
            text = text.substring(1, text.length() - 1);
        }
        return splitFunctionTypeParameters(text);
    }

    private TypeReference functionTypeReturnType(TypeReference functionType) {
        var arrow = topLevelFunctionArrow(functionType.name());
        return arrow < 0
                ? new TypeReference("any", List.of())
                : NativeCapybaraParser.parseTypeReference(functionType.name().substring(arrow + 2));
    }

    private int topLevelFunctionArrow(String text) {
        var brackets = 0;
        var parentheses = 0;
        for (var index = 0; index < text.length() - 1; index++) {
            var current = text.charAt(index);
            if (current == '[') {
                brackets++;
            } else if (current == ']') {
                brackets--;
            } else if (current == '(') {
                parentheses++;
            } else if (current == ')') {
                parentheses--;
            } else if (current == '=' && text.charAt(index + 1) == '>' && brackets == 0 && parentheses == 0) {
                return index;
            }
        }
        return -1;
    }

    private List<TypeReference> splitFunctionTypeParameters(String text) {
        var parameters = new ArrayList<TypeReference>();
        var brackets = 0;
        var parentheses = 0;
        var start = 0;
        for (var index = 0; index <= text.length(); index++) {
            if (index == text.length()
                    || (text.charAt(index) == ',' && brackets == 0 && parentheses == 0)) {
                parameters.add(NativeCapybaraParser.parseTypeReference(text.substring(start, index)));
                start = index + 1;
                continue;
            }
            var current = text.charAt(index);
            if (current == '[') {
                brackets++;
            } else if (current == ']') {
                brackets--;
            } else if (current == '(') {
                parentheses++;
            } else if (current == ')') {
                parentheses--;
            }
        }
        return List.copyOf(parameters);
    }

    private boolean compatibleFunctionArgument(TypeReference expected, TypeReference actual) {
        if (actual.name().isBlank()
                || expected.name().equals("any")
                || actual.name().equals("any")
                || isSingleLetterGeneric(expected.name())
                || isSingleLetterGeneric(actual.name())) {
            return true;
        }
        if (unqualified(expected.name()).equals("int")
                && Set.of("size", "index").contains(unqualified(actual.name()))) {
            return true;
        }
        if (!unqualified(expected.name()).equals(unqualified(actual.name()))) {
            return false;
        }
        if (expected.arguments().isEmpty() || actual.arguments().isEmpty()) {
            return true;
        }
        if (expected.arguments().size() != actual.arguments().size()) {
            return false;
        }
        for (var index = 0; index < expected.arguments().size(); index++) {
            if (!compatibleFunctionArgument(expected.arguments().get(index), actual.arguments().get(index))) {
                return false;
            }
        }
        return true;
    }

    private String localFunctionDisplayName(String name) {
        var marker = "__local__";
        var start = name.indexOf(marker);
        if (start < 0) {
            return name;
        }
        start += marker.length();
        var end = name.indexOf("__", start);
        return end < 0 ? name.substring(start) : name.substring(start, end);
    }

    private boolean acceptsMappedFunction(TypeReference expected) {
        if (expected.name().equals("any") || isSingleLetterGeneric(expected.name())) {
            return true;
        }
        if (expected.arguments().size() != 1) {
            return false;
        }
        var valueType = expected.arguments().getFirst().name();
        return valueType.equals("any")
                || isSingleLetterGeneric(valueType)
                || functionTypeName(valueType);
    }

    private FunctionDeclaration localFunction(ParsedModule module, String name, int arity) {
        FunctionDeclaration match = null;
        for (var definition : module.definitions()) {
            if (!(definition instanceof FunctionDefinition function)
                    || !function.function().name().equals(name)
                    || function.function().parameters().size() != arity) {
                continue;
            }
            if (match != null) {
                return null;
            }
            match = function.function();
        }
        return match;
    }

    private TypeReference nestedLambdaMappedType(Expression expression, Map<String, TypeReference> types) {
        if (!(expression instanceof MethodCallExpression call)
                || !call.name().equals("map")
                || call.arguments().size() != 1
                || !(call.arguments().getFirst() instanceof LambdaExpression mapper)
                || !(mapper.body() instanceof LambdaExpression)
                || !(call.receiver() instanceof VariableExpression receiver)) {
            return null;
        }
        var receiverType = types.get(receiver.name());
        if (receiverType == null || receiverType.arguments().isEmpty()) {
            return null;
        }
        return new TypeReference(receiverType.name(), List.of(new TypeReference("function", List.of())));
    }

    private String displayType(TypeReference type) {
        if (type.arguments().isEmpty()) {
            return type.name();
        }
        return type.name() + "[" + type.arguments().stream()
                .map(this::displayType)
                .reduce((left, right) -> left + ", " + right)
                .orElse("") + "]";
    }

    private void validateJavaBackendExpression(
            ParsedModule module,
            Expression expression,
            Map<String, TypeReference> types,
            List<CompilerError> errors
    ) {
        switch (expression) {
            case BinaryExpression binary -> {
                validateJavaBackendExpression(module, binary.left(), types, errors);
                validateJavaBackendExpression(module, binary.right(), types, errors);
            }
            case BlockExpression block -> {
                var blockTypes = new LinkedHashMap<>(types);
                for (var binding : block.bindings()) {
                    validateJavaBackendExpression(module, binding.value(), blockTypes, errors);
                    if (!binding.typeReference().name().isBlank()) {
                        blockTypes.put(binding.name(), binding.typeReference());
                    }
                }
                validateJavaBackendExpression(module, block.result(), blockTypes, errors);
            }
            case DataLiteral literal -> literal.fields().forEach(field ->
                    validateJavaBackendExpression(module, field.value(), types, errors));
            case DictLiteral literal -> literal.entries().forEach(entry -> {
                validateJavaBackendExpression(module, entry.key(), types, errors);
                validateJavaBackendExpression(module, entry.value(), types, errors);
            });
            case FieldAccessExpression access ->
                    validateJavaBackendExpression(module, access.receiver(), types, errors);
            case FunctionCallExpression call -> call.arguments().forEach(argument ->
                    validateJavaBackendExpression(module, argument, types, errors));
            case IfExpression ifExpression -> {
                validateJavaBackendExpression(module, ifExpression.condition(), types, errors);
                validateJavaBackendExpression(module, ifExpression.thenBranch(), types, errors);
                validateJavaBackendExpression(module, ifExpression.elseBranch(), types, errors);
            }
            case IndexExpression index -> {
                validateJavaBackendExpression(module, index.receiver(), types, errors);
                validateJavaBackendExpression(module, index.index(), types, errors);
                if (index.hasEndIndex()) {
                    validateJavaBackendExpression(module, index.endIndex(), types, errors);
                }
            }
            case LambdaExpression lambda ->
                    validateJavaBackendExpression(module, lambda.body(), types, errors);
            case ListLiteral literal -> literal.values().forEach(value ->
                    validateJavaBackendExpression(module, value, types, errors));
            case MatchExpression match -> {
                validateJavaBackendExpression(module, match.value(), types, errors);
                for (var matchCase : match.cases()) {
                    if (matchCase.hasLiteral()) {
                        validateJavaBackendExpression(module, matchCase.literal(), types, errors);
                    }
                    if (matchCase.hasGuard()) {
                        validateJavaBackendExpression(module, matchCase.guard(), types, errors);
                    }
                    validateJavaBackendExpression(module, matchCase.body(), types, errors);
                }
            }
            case MethodCallExpression call -> {
                javaBackendMethodError(module, call, types).ifPresent(errors::add);
                validateJavaBackendExpression(module, call.receiver(), types, errors);
                call.arguments().forEach(argument ->
                        validateJavaBackendExpression(module, argument, types, errors));
            }
            case ReduceExpression reduce -> {
                validateJavaBackendExpression(module, reduce.receiver(), types, errors);
                validateJavaBackendExpression(module, reduce.initial(), types, errors);
                var reduceTypes = new LinkedHashMap<>(types);
                inferJavaBackendExpressionType(reduce.initial(), types)
                        .ifPresent(type -> reduceTypes.put(reduce.accumulatorName(), type));
                validateJavaBackendExpression(module, reduce.body(), reduceTypes, errors);
            }
            case SetLiteral literal -> literal.values().forEach(value ->
                    validateJavaBackendExpression(module, value, types, errors));
            case TupleLiteral literal -> literal.values().forEach(value ->
                    validateJavaBackendExpression(module, value, types, errors));
            case UnaryExpression unary ->
                    validateJavaBackendExpression(module, unary.expression(), types, errors);
            case WithExpression with -> {
                validateJavaBackendExpression(module, with.receiver(), types, errors);
                with.fields().forEach(field ->
                        validateJavaBackendExpression(module, field.value(), types, errors));
            }
            default -> {
            }
        }
    }

    private java.util.Optional<CompilerError> javaBackendMethodError(
            ParsedModule module,
            MethodCallExpression call,
            Map<String, TypeReference> types
    ) {
        return inferJavaBackendExpressionType(call.receiver(), types).flatMap(type -> {
            var receiverType = unqualified(type.name());
            var standardMethods = STANDARD_METHODS_BY_RECEIVER.get(receiverType);
            if (standardMethods != null && !standardMethods.contains(call.name())) {
                return java.util.Optional.empty();
            }
            var supportedMethods = JAVA_SUPPORTED_METHODS_BY_RECEIVER.get(receiverType);
            if (supportedMethods == null || supportedMethods.contains(call.name())) {
                return java.util.Optional.empty();
            }
            return java.util.Optional.of(error(
                    module,
                    call.location(),
                    "Method `" + call.name() + "` on `" + receiverType
                            + "` is not supported by the Java backend."
            ));
        });
    }

    private java.util.Optional<TypeReference> inferJavaBackendExpressionType(
            Expression expression,
            Map<String, TypeReference> types
    ) {
        return switch (expression) {
            case VariableExpression variable -> java.util.Optional.ofNullable(types.get(variable.name()));
            case FunctionCallExpression call when Set.of("pure", "Effect.pure", "delay", "Effect.delay")
                    .contains(call.name()) -> java.util.Optional.of(new TypeReference("Effect", List.of()));
            default -> java.util.Optional.empty();
        };
    }

    private void validateFunctionAnnotations(
            Context context,
            ParsedModule module,
            FunctionDeclaration function,
            List<CompilerError> errors,
            Set<String> nativeProviderKeys
    ) {
        for (var annotation : function.annotations()) {
            if (isStandardNativeProvider(annotation, module)) {
                validateNativeProvider(module, function, annotation, errors, nativeProviderKeys);
                continue;
            }
            if (isStandardRecursive(annotation, module)) {
                validateStandardRecursive(module, annotation, errors);
                continue;
            }
            var declaration = context.annotationDeclaration(module, annotation.name());
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
            case LambdaExpression lambda -> {
                for (var parameter : lambda.parameters()) {
                    var declaredType = decodedLambdaParameterType(parameter);
                    if (declaredType != null) {
                        validateTypeReference(context, module, declaredType, List.of(), errors, lambda.location());
                    }
                }
                validateExpression(context, module, lambda.body(), errors);
            }
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
                    validateObjectExpression(module, method.body(), errors, objectMethodEnv(method));
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
                    validateObjectExpression(module, method.body(), errors, objectMethodEnv(method));
                }
            }
        }
    }

    private void validateObjectExpression(ParsedModule module, Expression expression, List<CompilerError> errors) {
        validateObjectExpression(module, expression, errors, Map.of());
    }

    private void validateObjectExpression(
            ParsedModule module,
            Expression expression,
            List<CompilerError> errors,
            Map<String, TypeReference> env
    ) {
        switch (expression) {
            case UnsupportedExpression unsupported -> {
                if (unsupported.location().line() != 0 || unsupported.location().column() != 0) {
                    errors.add(error(module, unsupported.location(), "Unsupported object-oriented construct: `" + unsupported.source() + "`."));
                }
            }
            case BinaryExpression binary -> {
                validateObjectExpression(module, binary.left(), errors, env);
                validateObjectExpression(module, binary.right(), errors, env);
            }
            case BlockExpression block -> {
                var localEnv = new HashMap<>(env);
                for (var binding : block.bindings()) {
                    validateObjectExpression(module, binding.value(), errors, localEnv);
                    var bindingType = objectBindingType(binding, localEnv);
                    if (!bindingType.name().isBlank()) {
                        localEnv.put(binding.name(), bindingType);
                    }
                }
                validateObjectExpression(module, block.result(), errors, localEnv);
            }
            case DataLiteral literal -> literal.fields().forEach(field -> validateObjectExpression(module, field.value(), errors, env));
            case DictLiteral literal -> literal.entries().forEach(entry -> {
                validateObjectExpression(module, entry.key(), errors, env);
                validateObjectExpression(module, entry.value(), errors, env);
            });
            case FieldAccessExpression access -> validateObjectExpression(module, access.receiver(), errors, env);
            case FunctionCallExpression call -> call.arguments().forEach(argument -> validateObjectExpression(module, argument, errors, env));
            case IfExpression ifExpression -> {
                validateObjectExpression(module, ifExpression.condition(), errors, env);
                validateObjectExpression(module, ifExpression.thenBranch(), errors, env);
                validateObjectExpression(module, ifExpression.elseBranch(), errors, env);
            }
            case IndexExpression index -> {
                validateObjectExpression(module, index.receiver(), errors, env);
                validateObjectExpression(module, index.index(), errors, env);
                if (index.hasEndIndex()) {
                    validateObjectExpression(module, index.endIndex(), errors, env);
                }
            }
            case LambdaExpression lambda -> validateObjectExpression(module, lambda.body(), errors, env);
            case ListLiteral literal -> literal.values().forEach(value -> validateObjectExpression(module, value, errors, env));
            case MatchExpression match -> {
                validateObjectExpression(module, match.value(), errors, env);
                for (var matchCase : match.cases()) {
                    if (matchCase.hasLiteral()) {
                        validateObjectExpression(module, matchCase.literal(), errors, env);
                    }
                    if (matchCase.hasGuard()) {
                        validateObjectExpression(module, matchCase.guard(), errors, env);
                    }
                    validateObjectExpression(module, matchCase.body(), errors, env);
                }
            }
            case ThrowExpression throwExpression -> {
                validateObjectExpression(module, throwExpression.value(), errors, env);
                var valueType = inferObjectExpressionType(throwExpression.value(), env);
                if (knownObjectExpressionType(valueType) && !resultErrorType(valueType)) {
                    errors.add(error(module, throwExpression.location(), "OO throw expression must have type `/capy/lang/Result.Error`."));
                }
            }
            case TryCatchExpression tryCatch -> {
                validateObjectExpression(module, tryCatch.body(), errors, env);
                tryCatch.branches().forEach(branch -> {
                    var catchEnv = new HashMap<>(env);
                    catchEnv.put(branch.catchName(), resultErrorType());
                    validateObjectExpression(module, branch.catchBody(), errors, catchEnv);
                });
            }
            case MethodCallExpression call -> {
                validateObjectExpression(module, call.receiver(), errors, env);
                call.arguments().forEach(argument -> validateObjectExpression(module, argument, errors, env));
            }
            case ReduceExpression reduce -> {
                validateObjectExpression(module, reduce.receiver(), errors, env);
                validateObjectExpression(module, reduce.initial(), errors, env);
                validateObjectExpression(module, reduce.body(), errors, env);
            }
            case SetLiteral literal -> literal.values().forEach(value -> validateObjectExpression(module, value, errors, env));
            case TupleLiteral literal -> literal.values().forEach(value -> validateObjectExpression(module, value, errors, env));
            case UnaryExpression unary -> validateObjectExpression(module, unary.expression(), errors, env);
            case WithExpression with -> {
                validateObjectExpression(module, with.receiver(), errors, env);
                with.fields().forEach(field -> validateObjectExpression(module, field.value(), errors, env));
            }
            default -> {
            }
        }
    }

    private Map<String, TypeReference> objectMethodEnv(ObjectOrientedMethod method) {
        var env = new HashMap<String, TypeReference>();
        for (var parameter : method.parameters()) {
            env.put(parameter.name(), parameter.typeReference());
        }
        return env;
    }

    private TypeReference objectBindingType(LetBinding binding, Map<String, TypeReference> env) {
        if (!binding.typeReference().name().isBlank()) {
            return binding.typeReference();
        }
        return inferObjectExpressionType(binding.value(), env);
    }

    private TypeReference inferObjectExpressionType(Expression expression, Map<String, TypeReference> env) {
        return switch (expression) {
            case BoolLiteral ignored -> builtinType("bool");
            case DataLiteral literal -> new TypeReference(literal.typeName(), List.of());
            case DoubleLiteral ignored -> builtinType("double");
            case FloatLiteral ignored -> builtinType("float");
            case FunctionCallExpression call -> resultErrorFunctionCall(call.name()) ? resultErrorType() : missingType();
            case IfExpression ifExpression -> preferredObjectBranchType(
                    inferObjectExpressionType(ifExpression.thenBranch(), env),
                    inferObjectExpressionType(ifExpression.elseBranch(), env)
            );
            case IntLiteral ignored -> builtinType("int");
            case LongLiteral ignored -> builtinType("long");
            case StringLiteral ignored -> builtinType("String");
            case TryCatchExpression tryCatch -> inferObjectExpressionType(tryCatch.body(), env);
            case VariableExpression variable -> env.getOrDefault(variable.name(), missingType());
            case BlockExpression block -> inferObjectBlockType(block, env);
            default -> missingType();
        };
    }

    private TypeReference inferObjectBlockType(BlockExpression block, Map<String, TypeReference> env) {
        var localEnv = new HashMap<>(env);
        for (var binding : block.bindings()) {
            var bindingType = objectBindingType(binding, localEnv);
            if (!bindingType.name().isBlank()) {
                localEnv.put(binding.name(), bindingType);
            }
        }
        return inferObjectExpressionType(block.result(), localEnv);
    }

    private TypeReference preferredObjectBranchType(TypeReference thenType, TypeReference elseType) {
        if (thenType.name().isBlank()) {
            return elseType;
        }
        if (elseType.name().isBlank() || sameType(thenType, elseType)) {
            return thenType;
        }
        return missingType();
    }

    private boolean knownObjectExpressionType(TypeReference type) {
        return !type.name().isBlank();
    }

    private boolean resultErrorType(TypeReference type) {
        var name = type.name();
        return "Error".equals(name)
                || "Result.Error".equals(name)
                || "/capy/lang/Result.Error".equals(name)
                || "capy/lang/Result.Error".equals(name);
    }

    private TypeReference resultErrorType() {
        return new TypeReference("/capy/lang/Result.Error", List.of());
    }

    private boolean resultErrorFunctionCall(String name) {
        return switch (unqualified(name)) {
            case "error", "error_kind", "error_with", "error_at", "error_full" -> true;
            default -> false;
        };
    }

    private TypeReference builtinType(String name) {
        return new TypeReference(name, List.of());
    }

    private TypeReference missingType() {
        return new TypeReference("", List.of());
    }

    private boolean sameType(TypeReference left, TypeReference right) {
        return left.name().equals(right.name()) && left.arguments().equals(right.arguments());
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
        return "NativeProvider".equals(unqualified(annotation.name()))
                && (standardNativeProviderImported(module)
                || explicitAnnotationPath(annotation.name(), "/capy/meta_prog/NativeProvider", "NativeProvider"));
    }

    private boolean standardNativeProviderImported(ParsedModule module) {
        return importsName(module.imports(), "/capy/meta_prog/NativeProvider", "NativeProvider")
                || importsName(module.imports(), "capy/meta_prog/NativeProvider", "NativeProvider")
                || importsName(module.imports(), "NativeProvider", "NativeProvider");
    }

    private boolean isStandardRecursive(FunctionAnnotationApplication annotation, ParsedModule module) {
        return "Recursive".equals(unqualified(annotation.name()))
                && (standardRecursiveImported(module)
                || explicitAnnotationPath(annotation.name(), "/capy/meta_prog/Recursive", "Recursive"));
    }

    private boolean standardRecursiveImported(ParsedModule module) {
        return importsName(module.imports(), "/capy/meta_prog/Recursive", "Recursive")
                || importsName(module.imports(), "capy/meta_prog/Recursive", "Recursive")
                || importsName(module.imports(), "Recursive", "Recursive");
    }

    private boolean explicitAnnotationPath(String annotationName, String modulePath, String name) {
        return normalizeModulePath(annotationName).equals(normalizeModulePath(modulePath) + "." + name);
    }

    private void validateStandardRecursive(
            ParsedModule module,
            FunctionAnnotationApplication annotation,
            List<CompilerError> errors
    ) {
        for (var argument : annotation.arguments()) {
            errors.add(error(module, argument.location(), "Unknown annotation argument " + argument.name() + "."));
        }
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
        private final List<CompiledModule> linkedModules;
        private final List<ParsedModule> previouslyValidatedModules;
        private final Map<ParsedModule, Set<String>> symbolsByModule = new LinkedHashMap<>();
        private final Map<ParsedModule, Set<String>> typesByModule = new LinkedHashMap<>();
        private final Map<ParsedModule, Map<String, AnnotationDeclaration>> annotationsByModule = new LinkedHashMap<>();

        private Context(List<ParsedModule> modules, List<String> libraryModules) {
            this.modules = modules;
            this.libraryModules = new LinkedHashSet<>(libraryModules);
            this.linkedModules = NativeLinkedProgramIO.linkedModules();
            this.previouslyValidatedModules = List.copyOf(VALIDATED_MODULES.get().values());
            for (var module : modules) {
                symbolsByModule.put(module, parsedSymbols(module));
                typesByModule.put(module, parsedTypes(module));
                annotationsByModule.put(module, parsedAnnotations(module));
            }
            for (var module : previouslyValidatedModules) {
                symbolsByModule.putIfAbsent(module, parsedSymbols(module));
                typesByModule.putIfAbsent(module, parsedTypes(module));
                annotationsByModule.putIfAbsent(module, parsedAnnotations(module));
            }
        }

        private boolean moduleExists(String path) {
            if (stdlibImport(path)) {
                return true;
            }
            return parsedModule(path) != null || libraryModule(path);
        }

        private boolean symbolExists(String path, String name) {
            var parsed = parsedModule(path);
            if (parsed != null) {
                return symbolsByModule.get(parsed).contains(name);
            }
            if (stdlibImport(path)) {
                var linked = linkedModule(path);
                if (linked != null) {
                    return linkedSymbolExists(linked, name);
                }
            }
            var linked = linkedModule(path);
            if (linked != null) {
                return linkedSymbolExists(linked, name);
            }
            if (stdlibImport(path)) {
                return true;
            }
            return libraryModule(path);
        }

        private boolean linkedSymbolExists(CompiledModule module, String name) {
            if (module.types().containsKey(name)
                    || module.visiblePrimitiveBackedTypes().containsKey(name)
                    || module.annotations().containsKey(name)) {
                return true;
            }
            for (var function : module.functions()) {
                if (function.name().equals(name)
                        || function.name().equals("__capy_schema_type|" + name)
                        || function.name().equals("__capy_schema_primitive|" + name)
                        || function.name().equals("__capy_constructor|" + name)) {
                    return true;
                }
            }
            return false;
        }

        private boolean typeExists(String name) {
            for (var types : typesByModule.values()) {
                if (types.contains(name)) {
                    return true;
                }
            }
            return false;
        }

        private boolean objectMethodExists(
                ParsedModule module,
                String receiverType,
                String methodName,
                int arity
        ) {
            return objectMethodExists(module, receiverType, methodName, arity, new HashSet<>());
        }

        private boolean objectMethodExists(
                ParsedModule module,
                String receiverType,
                String methodName,
                int arity,
                Set<String> visitedTypes
        ) {
            var receiverName = unqualified(receiverType);
            if (!visitedTypes.add(receiverName)) {
                return false;
            }
            if (moduleHasType(module, receiverName)) {
                return parsedObjectMethodExists(module, receiverName, methodName, arity, visitedTypes);
            }
            for (var declaration : module.imports()) {
                if (!importExposes(declaration, receiverName)) {
                    continue;
                }
                var parsedOwner = parsedModule(declaration.modulePath());
                if (parsedOwner != null && moduleHasType(parsedOwner, receiverName)
                        && parsedObjectMethodExists(
                                parsedOwner,
                                receiverName,
                                methodName,
                                arity,
                                visitedTypes
                        )) {
                    return true;
                }
                var linkedOwner = linkedModule(declaration.modulePath());
                if (linkedOwner != null && linkedObjectTypeExists(linkedOwner, receiverName)
                        && linkedObjectMethodExists(
                                linkedOwner,
                                receiverName,
                                methodName,
                                arity,
                                visitedTypes
                        )) {
                    return true;
                }
            }
            return false;
        }

        private boolean parsedObjectMethodExists(
                ParsedModule module,
                String receiverName,
                String methodName,
                int arity,
                Set<String> visitedTypes
        ) {
            for (var objectClass : module.objectOriented().classes()) {
                if (objectClass.name().equals(receiverName)
                        && (objectMethodsContain(objectClass.methods(), methodName, arity)
                        || objectParentsContainMethod(
                                module,
                                objectClass.parents(),
                                methodName,
                                arity,
                                visitedTypes
                        ))) {
                    return true;
                }
            }
            for (var objectInterface : module.objectOriented().interfaces()) {
                if (objectInterface.name().equals(receiverName)
                        && (objectMethodsContain(objectInterface.methods(), methodName, arity)
                        || objectParentsContainMethod(
                                module,
                                objectInterface.parents(),
                                methodName,
                                arity,
                                visitedTypes
                        ))) {
                    return true;
                }
            }
            return false;
        }

        private boolean importExposes(ImportDeclaration declaration, String name) {
            return !declaration.qualified()
                    && !declaration.excludedNames().contains(name)
                    && (declaration.wildcard() || declaration.importedNames().contains(name));
        }

        private boolean linkedObjectTypeExists(CompiledModule module, String name) {
            return hasLinkedSchema(module, "__capy_oo_class|" + name)
                    || hasLinkedSchema(module, "__capy_oo_interface|" + name);
        }

        private boolean linkedObjectMethodExists(
                CompiledModule module,
                String receiverName,
                String methodName,
                int arity,
                Set<String> visitedTypes
        ) {
            var classMethod = "__capy_oo_method|" + receiverName + "|" + methodName;
            var interfaceMethod = "__capy_oo_interface_method|" + receiverName + "|" + methodName;
            if (module.functions().stream().anyMatch(function ->
                    (function.name().equals(classMethod) || function.name().equals(interfaceMethod))
                            && function.parameters().size() == arity)) {
                return true;
            }
            return linkedObjectParentsContainMethod(
                    module,
                    receiverName,
                    "__capy_oo_parent|",
                    methodName,
                    arity,
                    visitedTypes
            ) || linkedObjectParentsContainMethod(
                    module,
                    receiverName,
                    "__capy_oo_interface_parent|",
                    methodName,
                    arity,
                    visitedTypes
            );
        }

        private boolean linkedObjectParentsContainMethod(
                CompiledModule module,
                String receiverName,
                String parentPrefix,
                String methodName,
                int arity,
                Set<String> visitedTypes
        ) {
            var prefix = parentPrefix + receiverName + "|";
            return module.functions().stream()
                    .filter(function -> function.name().startsWith(prefix))
                    .map(function -> function.body())
                    .filter(CompiledExpression.CompiledStringLiteral.class::isInstance)
                    .map(CompiledExpression.CompiledStringLiteral.class::cast)
                    .anyMatch(parent -> linkedObjectMethodExists(
                            schemaTypeReference(parent.value()).name(),
                            methodName,
                            arity,
                            visitedTypes
                    ));
        }

        private boolean linkedObjectMethodExists(
                String receiverType,
                String methodName,
                int arity,
                Set<String> visitedTypes
        ) {
            var receiverName = unqualified(receiverType);
            if (!visitedTypes.add(receiverName)) {
                return false;
            }
            return linkedModules.stream()
                    .filter(module -> linkedObjectTypeExists(module, receiverName))
                    .anyMatch(module -> linkedObjectMethodExists(
                            module,
                            receiverName,
                            methodName,
                            arity,
                            visitedTypes
                    ));
        }

        private boolean objectParentsContainMethod(
                ParsedModule module,
                List<TypeReference> parents,
                String methodName,
                int arity,
                Set<String> visitedTypes
        ) {
            return parents.stream().anyMatch(parent ->
                    objectMethodExists(module, parent.name(), methodName, arity, visitedTypes));
        }

        private boolean objectMethodsContain(List<ObjectOrientedMethod> methods, String methodName, int arity) {
            return methods.stream().anyMatch(method ->
                    method.name().equals(methodName) && method.parameters().size() == arity);
        }

        private boolean moduleHasType(ParsedModule module, String name) {
            return typesByModule.get(module).contains(name);
        }

        private boolean importedTypeExists(ParsedModule module, String name) {
            for (var declaration : module.imports()) {
                if (declaration.qualified()) {
                    if (unqualified(declaration.modulePath()).equals(name)
                            && symbolExists(declaration.modulePath(), name)) {
                        return true;
                    }
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

        private boolean enumType(ParsedModule module, String name) {
            var typeName = unqualified(name);
            if (localEnumType(module, typeName)) {
                return true;
            }
            for (var declaration : module.imports()) {
                if (!importExposes(declaration, typeName)) {
                    continue;
                }
                var parsed = parsedModule(declaration.modulePath());
                if (parsed != null && localEnumType(parsed, typeName)) {
                    return true;
                }
                var linked = linkedModule(declaration.modulePath());
                if (linked != null && linkedSchemaValue(linked, "__capy_schema_kind|" + typeName)
                        .map("enum"::equals)
                        .orElse(false)) {
                    return true;
                }
            }
            return false;
        }

        private boolean localEnumType(ParsedModule module, String name) {
            return module.definitions().stream().anyMatch(definition ->
                    definition instanceof EnumDeclaration enumDeclaration
                            && enumDeclaration.name().equals(name));
        }

        private boolean numericPrimitiveBackedType(ParsedModule module, String name) {
            var typeName = unqualified(name);
            var parsed = primitiveBackedTypeDeclaration(module, typeName);
            if (parsed != null && NUMERIC_TYPES.contains(unqualified(parsed.backingType().name()))) {
                return true;
            }
            for (var declaration : module.imports()) {
                if (!importExposes(declaration, typeName)) {
                    continue;
                }
                var linked = linkedModule(declaration.modulePath());
                if (linked == null) {
                    continue;
                }
                var primitive = linked.visiblePrimitiveBackedTypes().get(typeName);
                if (primitive != null && NUMERIC_TYPES.contains(unqualified(primitive.backingType().name()))) {
                    return true;
                }
            }
            return false;
        }

        private boolean primitiveConversionMethod(ParsedModule module, String receiverName, String methodName) {
            var backingType = primitiveBackingType(module, receiverName);
            if (backingType == null) {
                return false;
            }
            return switch (methodName) {
                case "to_int" -> Set.of("long", "float", "double").contains(backingType);
                case "to_long" -> Set.of("float", "double").contains(backingType);
                default -> false;
            };
        }

        private String primitiveBackingType(ParsedModule module, String name) {
            var typeName = unqualified(name);
            var parsed = primitiveBackedTypeDeclaration(module, typeName);
            if (parsed != null) {
                return unqualified(parsed.backingType().name());
            }
            String match = null;
            for (var declaration : module.imports()) {
                if (!importExposes(declaration, typeName)) {
                    continue;
                }
                var linked = linkedModule(declaration.modulePath());
                if (linked == null) {
                    continue;
                }
                var primitive = linked.visiblePrimitiveBackedTypes().get(typeName);
                if (primitive == null) {
                    continue;
                }
                if (match != null) {
                    return null;
                }
                match = unqualified(primitive.backingType().name());
            }
            return match;
        }

        private Optional<String> linkedSchemaValue(CompiledModule module, String name) {
            return module.functions().stream()
                    .filter(function -> function.name().equals(name))
                    .map(function -> function.body())
                    .filter(CompiledExpression.CompiledStringLiteral.class::isInstance)
                    .map(CompiledExpression.CompiledStringLiteral.class::cast)
                    .map(CompiledExpression.CompiledStringLiteral::value)
                    .findFirst();
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

        private TypeReference constantType(ParsedModule module, String name) {
            var local = localConstantType(module, name);
            if (local != null) {
                return local;
            }
            TypeReference match = null;
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(name))
                        || declaration.excludedNames().contains(name)) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                if (importedModule == null) {
                    continue;
                }
                var imported = localConstantType(importedModule, name);
                if (imported == null) {
                    continue;
                }
                if (match != null) {
                    return null;
                }
                match = imported;
            }
            return match;
        }

        private TypeReference localConstantType(ParsedModule module, String name) {
            for (var definition : module.definitions()) {
                if (definition instanceof ConstantDefinition constant
                        && constant.constant().name().equals(name)) {
                    return constant.constant().typeReference();
                }
            }
            return null;
        }

        private boolean hasConstructor(ParsedModule module, String typeName) {
            var constructorName = "__capy_constructor|" + typeName;
            if (hasLocalFunction(module, constructorName)) {
                return true;
            }
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(typeName))
                        || declaration.excludedNames().contains(typeName)) {
                    continue;
                }
                var parsed = parsedModule(declaration.modulePath());
                if (parsed != null && hasLocalFunction(parsed, constructorName)) {
                    return true;
                }
                var linked = linkedModule(declaration.modulePath());
                if (linked != null && linked.functions().stream()
                        .anyMatch(function -> function.name().equals(constructorName))) {
                    return true;
                }
            }
            return false;
        }

        private boolean hasLocalFunction(ParsedModule module, String name) {
            for (var definition : module.definitions()) {
                if (definition instanceof FunctionDefinition function
                        && function.function().name().equals(name)) {
                    return true;
                }
            }
            return false;
        }

        private FunctionDeclaration functionDeclaration(ParsedModule module, String name, int arity) {
            var local = localFunction(module, name, arity);
            if (local != null) {
                return local;
            }
            FunctionDeclaration match = null;
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(name))
                        || declaration.excludedNames().contains(name)) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                if (importedModule == null) {
                    continue;
                }
                var imported = localFunction(importedModule, name, arity);
                if (imported == null) {
                    continue;
                }
                if (match != null) {
                    return null;
                }
                match = imported;
            }
            return match;
        }

        private Set<String> extensionMethodReceiverTypes(
                ParsedModule module,
                String methodName,
                int arity
        ) {
            var receiverTypes = new LinkedHashSet<String>();
            collectParsedExtensionMethodReceiverTypes(module, null, methodName, arity, receiverTypes);
            for (var declaration : module.imports()) {
                if (declaration.qualified()) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                if (importedModule != null) {
                    collectParsedExtensionMethodReceiverTypes(
                            importedModule,
                            declaration,
                            methodName,
                            arity,
                            receiverTypes
                    );
                }
                var linkedModule = linkedModule(declaration.modulePath());
                if (linkedModule != null) {
                    collectLinkedExtensionMethodReceiverTypes(
                            declaration,
                            linkedModule,
                            methodName,
                            arity,
                            receiverTypes
                    );
                }
            }
            return receiverTypes;
        }

        private Set<String> extensionMethodReceiverTypes(ParsedModule module, String methodName) {
            return extensionMethodReceiverTypes(module, methodName, -1);
        }

        private Set<Integer> extensionMethodArities(
                ParsedModule module,
                String receiverType,
                String methodName
        ) {
            var arities = new LinkedHashSet<Integer>();
            collectParsedExtensionMethodArities(module, null, receiverType, methodName, arities);
            for (var declaration : module.imports()) {
                if (declaration.qualified()) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                if (importedModule != null) {
                    collectParsedExtensionMethodArities(
                            importedModule,
                            declaration,
                            receiverType,
                            methodName,
                            arities
                    );
                }
                var linkedModule = linkedModule(declaration.modulePath());
                if (linkedModule != null) {
                    collectLinkedExtensionMethodArities(
                            declaration,
                            linkedModule,
                            receiverType,
                            methodName,
                            arities
                    );
                }
            }
            return arities;
        }

        private TypeReference extensionMethodReturnType(
                ParsedModule module,
                String receiverType,
                String methodName,
                int arity
        ) {
            var local = parsedExtensionMethodReturnType(module, null, receiverType, methodName, arity);
            if (local != null) {
                return local;
            }
            TypeReference match = null;
            for (var declaration : module.imports()) {
                if (declaration.qualified()) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                var imported = importedModule == null
                        ? null
                        : parsedExtensionMethodReturnType(
                                importedModule,
                                declaration,
                                receiverType,
                                methodName,
                                arity
                        );
                if (imported == null) {
                    var linkedModule = linkedModule(declaration.modulePath());
                    imported = linkedModule == null
                            ? null
                            : linkedExtensionMethodReturnType(
                                    declaration,
                                    linkedModule,
                                    receiverType,
                                    methodName,
                                    arity
                            );
                }
                if (imported == null) {
                    continue;
                }
                if (match != null && !match.equals(imported)) {
                    return null;
                }
                match = imported;
            }
            return match;
        }

        private TypeReference parsedExtensionMethodReturnType(
                ParsedModule module,
                ImportDeclaration declaration,
                String receiverType,
                String methodName,
                int arity
        ) {
            for (var definition : module.definitions()) {
                if (!(definition instanceof FunctionDefinition function)
                        || function.function().parameters().size() != arity
                        || (declaration != null && function.function().visibility().equals("private"))) {
                    continue;
                }
                var owner = extensionMethodReceiverType(function.function().name(), methodName);
                if (receiverType.equals(owner) && (declaration == null || importedExtensionMethodVisible(
                        declaration,
                        receiverType,
                        methodName
                ))) {
                    return function.function().returnType();
                }
            }
            return null;
        }

        private TypeReference linkedExtensionMethodReturnType(
                ImportDeclaration declaration,
                CompiledModule module,
                String receiverType,
                String methodName,
                int arity
        ) {
            for (var function : module.functions()) {
                if (function.parameters().size() != arity || function.visibility().equals("private")) {
                    continue;
                }
                var owner = extensionMethodReceiverType(function.name(), methodName);
                if (receiverType.equals(owner) && importedExtensionMethodVisible(
                        declaration,
                        receiverType,
                        methodName
                )) {
                    return parsedTypeReference(function.returnType());
                }
            }
            return null;
        }

        private TypeReference parsedTypeReference(CompiledTypeReference reference) {
            return new TypeReference(
                    reference.name(),
                    reference.arguments().stream().map(this::parsedTypeReference).toList()
            );
        }

        private void collectParsedExtensionMethodReceiverTypes(
                ParsedModule module,
                ImportDeclaration declaration,
                String methodName,
                int arity,
                Set<String> receiverTypes
        ) {
            for (var definition : module.definitions()) {
                if (!(definition instanceof FunctionDefinition function)
                        || (arity >= 0 && function.function().parameters().size() != arity)
                        || (declaration != null && function.function().visibility().equals("private"))) {
                    continue;
                }
                var receiverType = extensionMethodReceiverType(function.function().name(), methodName);
                if (receiverType != null && (declaration == null || importedExtensionMethodVisible(
                        declaration,
                        receiverType,
                        methodName
                ))) {
                    receiverTypes.add(receiverType);
                }
            }
        }

        private void collectLinkedExtensionMethodReceiverTypes(
                ImportDeclaration declaration,
                CompiledModule module,
                String methodName,
                int arity,
                Set<String> receiverTypes
        ) {
            for (var function : module.functions()) {
                if ((arity >= 0 && function.parameters().size() != arity)
                        || function.visibility().equals("private")) {
                    continue;
                }
                var receiverType = extensionMethodReceiverType(function.name(), methodName);
                if (receiverType != null && importedExtensionMethodVisible(
                        declaration,
                        receiverType,
                        methodName
                )) {
                    receiverTypes.add(receiverType);
                }
            }
        }

        private void collectParsedExtensionMethodArities(
                ParsedModule module,
                ImportDeclaration declaration,
                String receiverType,
                String methodName,
                Set<Integer> arities
        ) {
            for (var definition : module.definitions()) {
                if (!(definition instanceof FunctionDefinition function)
                        || (declaration != null && function.function().visibility().equals("private"))) {
                    continue;
                }
                var owner = extensionMethodReceiverType(function.function().name(), methodName);
                if (receiverType.equals(owner) && (declaration == null || importedExtensionMethodVisible(
                        declaration,
                        receiverType,
                        methodName
                ))) {
                    arities.add(function.function().parameters().size());
                }
            }
        }

        private void collectLinkedExtensionMethodArities(
                ImportDeclaration declaration,
                CompiledModule module,
                String receiverType,
                String methodName,
                Set<Integer> arities
        ) {
            for (var function : module.functions()) {
                if (function.visibility().equals("private")) {
                    continue;
                }
                var owner = extensionMethodReceiverType(function.name(), methodName);
                if (receiverType.equals(owner) && importedExtensionMethodVisible(
                        declaration,
                        receiverType,
                        methodName
                )) {
                    arities.add(function.parameters().size());
                }
            }
        }

        private boolean importedExtensionMethodVisible(
                ImportDeclaration declaration,
                String receiverType,
                String methodName
        ) {
            return declaration.wildcard()
                    ? !declaration.excludedNames().contains(methodName)
                    : declaration.importedNames().contains(receiverType);
        }

        private String extensionMethodReceiverType(String functionName, String methodName) {
            var suffix = "." + methodName;
            var quotedSuffix = ".`" + methodName + "`";
            if (functionName.endsWith(suffix)) {
                return extensionMethodReceiverBaseType(
                        functionName.substring(0, functionName.length() - suffix.length())
                );
            }
            if (functionName.endsWith(quotedSuffix)) {
                return extensionMethodReceiverBaseType(
                        functionName.substring(0, functionName.length() - quotedSuffix.length())
                );
            }
            return null;
        }

        private String extensionMethodReceiverBaseType(String name) {
            var typeName = unqualified(name);
            var genericStart = typeName.indexOf('[');
            return genericStart < 0 ? typeName : typeName.substring(0, genericStart);
        }

        private Set<Integer> standardFunctionArities(ParsedModule module, String name) {
            if (moduleHasFunctionOrConstant(module, name)) {
                return null;
            }
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || declaration.excludedNames().contains(name)
                        || (!declaration.wildcard() && !declaration.importedNames().contains(name))) {
                    continue;
                }
                var functions = STANDARD_FUNCTION_ARITIES.get(normalizeModulePath(declaration.modulePath()));
                if (functions == null) {
                    continue;
                }
                var arities = functions.get(name);
                if (arities != null) {
                    return arities;
                }
            }
            return null;
        }

        private DataDeclaration dataDeclaration(ParsedModule module, String name) {
            var owner = dataDeclarationOwner(module, name);
            return owner == null ? null : localDataDeclaration(owner, unqualified(name));
        }

        private ParsedModule dataDeclarationOwner(ParsedModule module, String name) {
            if (localDataDeclaration(module, unqualified(name)) != null) {
                return module;
            }
            ParsedModule match = null;
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(unqualified(name)))
                        || declaration.excludedNames().contains(unqualified(name))) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                if (importedModule == null) {
                    continue;
                }
                var imported = localDataDeclaration(importedModule, unqualified(name));
                if (imported == null) {
                    continue;
                }
                if (match != null) {
                    return null;
                }
                match = importedModule;
            }
            return match;
        }

        private DataDeclaration localDataDeclaration(ParsedModule module, String name) {
            for (var definition : module.definitions()) {
                if (definition instanceof DataDeclaration data && data.name().equals(name)) {
                    return data;
                }
            }
            return null;
        }

        private List<TypeReference> directParentTypes(ParsedModule module, TypeReference actual) {
            var parents = new LinkedHashSet<TypeReference>();
            collectParsedParentTypes(module, actual, parents);
            for (var declaration : module.imports()) {
                var actualName = unqualified(actual.name());
                if (declaration.qualified()
                        || declaration.excludedNames().contains(actualName)
                        || (!declaration.wildcard() && !declaration.importedNames().contains(actualName))) {
                    continue;
                }
                var imported = parsedModule(declaration.modulePath());
                if (imported != null) {
                    collectParsedParentTypes(imported, actual, parents);
                }
                var linked = linkedModule(declaration.modulePath());
                if (linked != null) {
                    collectLinkedParentTypes(linked, actual, parents);
                }
            }
            return List.copyOf(parents);
        }

        private void collectParsedParentTypes(
                ParsedModule module,
                TypeReference actual,
                Set<TypeReference> parents
        ) {
            var actualName = unqualified(actual.name());
            for (var definition : module.definitions()) {
                if (definition instanceof TypeDeclaration type) {
                    for (var variant : type.variants()) {
                        if (!unqualified(variant.name()).equals(actualName)) {
                            continue;
                        }
                        var bindings = new LinkedHashMap<String, TypeReference>();
                        bindTypeParameters(variant, actual, bindings);
                        parents.add(new TypeReference(
                                type.name(),
                                type.parameters().stream()
                                        .map(parameter -> bindings.getOrDefault(
                                                parameter,
                                                new TypeReference(parameter, List.of())
                                        ))
                                        .toList()
                        ));
                    }
                }
                if (definition instanceof DataDeclaration data && data.name().equals(actualName)) {
                    var bindings = new LinkedHashMap<String, TypeReference>();
                    for (var index = 0;
                         index < data.parameters().size() && index < actual.arguments().size();
                         index++) {
                        bindings.put(data.parameters().get(index), actual.arguments().get(index));
                    }
                    for (var parent : data.parents()) {
                        parents.add(substituteTypeParameters(parent.typeReference(), bindings));
                    }
                }
            }
        }

        private void bindTypeParameters(
                TypeReference template,
                TypeReference actual,
                Map<String, TypeReference> bindings
        ) {
            if (isSingleLetterGeneric(template.name())) {
                bindings.put(template.name(), actual);
                return;
            }
            if (!unqualified(template.name()).equals(unqualified(actual.name()))
                    || template.arguments().size() != actual.arguments().size()) {
                return;
            }
            for (var index = 0; index < template.arguments().size(); index++) {
                bindTypeParameters(template.arguments().get(index), actual.arguments().get(index), bindings);
            }
        }

        private TypeReference substituteTypeParameters(
                TypeReference type,
                Map<String, TypeReference> bindings
        ) {
            var replacement = bindings.get(type.name());
            if (replacement != null) {
                return replacement;
            }
            return new TypeReference(
                    type.name(),
                    type.arguments().stream()
                            .map(argument -> substituteTypeParameters(argument, bindings))
                            .toList()
            );
        }

        private void collectLinkedParentTypes(
                CompiledModule module,
                TypeReference actual,
                Set<TypeReference> parents
        ) {
            var actualName = unqualified(actual.name());
            if (!hasLinkedSchema(module, "__capy_schema_type|" + actualName)) {
                return;
            }
            var prefix = "__capy_schema_parent|" + actualName + "|";
            for (var function : module.functions()) {
                if (!function.name().startsWith(prefix)
                        || !(function.body() instanceof CompiledExpression.CompiledStringLiteral schema)) {
                    continue;
                }
                var parent = schemaTypeReference(schema.value());
                if (parent.arguments().isEmpty() && !actual.arguments().isEmpty()) {
                    parent = new TypeReference(parent.name(), actual.arguments());
                }
                parents.add(parent);
            }
        }

        private TypeReference schemaTypeReference(String value) {
            var bracket = value.indexOf('[');
            if (bracket < 0 || !value.endsWith("]")) {
                return new TypeReference(value, List.of());
            }
            var arguments = new ArrayList<TypeReference>();
            var argumentsSource = value.substring(bracket + 1, value.length() - 1);
            var depth = 0;
            var start = 0;
            for (var index = 0; index <= argumentsSource.length(); index++) {
                if (index == argumentsSource.length()
                        || (argumentsSource.charAt(index) == ',' && depth == 0)) {
                    arguments.add(schemaTypeReference(argumentsSource.substring(start, index)));
                    start = index + 1;
                    continue;
                }
                if (argumentsSource.charAt(index) == '[') {
                    depth++;
                } else if (argumentsSource.charAt(index) == ']') {
                    depth--;
                }
            }
            return new TypeReference(value.substring(0, bracket), List.copyOf(arguments));
        }

        private boolean privateType(ParsedModule module, String name) {
            var typeName = unqualified(name);
            if (localTypeVisibility(module, typeName).map("private"::equals).orElse(false)) {
                return true;
            }
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(typeName))
                        || declaration.excludedNames().contains(typeName)) {
                    continue;
                }
                var parsed = parsedModule(declaration.modulePath());
                if (parsed != null
                        && localTypeVisibility(parsed, typeName).map("private"::equals).orElse(false)) {
                    return true;
                }
                var linked = linkedModule(declaration.modulePath());
                if (linked != null && linkedTypeVisibility(linked, typeName).map("private"::equals).orElse(false)) {
                    return true;
                }
            }
            return false;
        }

        private Optional<String> localTypeVisibility(ParsedModule module, String name) {
            for (var definition : module.definitions()) {
                switch (definition) {
                    case AnnotationDeclaration annotation -> {
                        if (annotation.name().equals(name)) {
                            return Optional.of(annotation.visibility());
                        }
                    }
                    case DataDeclaration data -> {
                        if (data.name().equals(name)) {
                            return Optional.of(data.visibility());
                        }
                    }
                    case PrimitiveBackedTypeDeclaration primitive -> {
                        if (primitive.name().equals(name)) {
                            return Optional.of(primitive.visibility());
                        }
                    }
                    case TypeDeclaration type -> {
                        if (type.name().equals(name)) {
                            return Optional.of(type.visibility());
                        }
                    }
                    default -> {
                    }
                }
            }
            return Optional.empty();
        }

        private Optional<String> linkedTypeVisibility(CompiledModule module, String name) {
            var primitive = module.visiblePrimitiveBackedTypes().get(name);
            if (primitive != null) {
                return Optional.of(primitive.visibility());
            }
            var schemaName = "__capy_schema_visibility|" + name;
            for (var function : module.functions()) {
                if (function.name().equals(schemaName)
                        && function.body() instanceof CompiledExpression.CompiledStringLiteral literal) {
                    return Optional.of(literal.value());
                }
            }
            return Optional.empty();
        }

        private List<LinkedDataField> linkedDataFields(ParsedModule module, String name) {
            var typeName = unqualified(name);
            CompiledModule owner = null;
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(typeName))
                        || declaration.excludedNames().contains(typeName)) {
                    continue;
                }
                var imported = linkedModule(declaration.modulePath());
                if (imported == null || !hasLinkedSchema(imported, "__capy_schema_type|" + typeName)) {
                    continue;
                }
                if (owner != null) {
                    return null;
                }
                owner = imported;
            }
            return owner == null ? null : linkedDataFields(owner, typeName);
        }

        private List<LinkedDataField> linkedDataFields(CompiledModule module, String typeName) {
            var fields = new java.util.TreeMap<Integer, LinkedDataField>();
            var prefix = "__capy_schema_field|" + typeName + "|";
            for (var function : module.functions()) {
                if (!function.name().startsWith(prefix)
                        || !(function.body() instanceof CompiledExpression.CompiledStringLiteral schema)) {
                    continue;
                }
                var separator = schema.value().indexOf('|');
                if (separator < 0) {
                    continue;
                }
                try {
                    var index = Integer.parseInt(function.name().substring(prefix.length()));
                    fields.put(index, new LinkedDataField(
                            schema.value().substring(0, separator),
                            new TypeReference(schema.value().substring(separator + 1), List.of())
                    ));
                } catch (NumberFormatException ignored) {
                    // Ignore malformed linked schema entries; linked JSON compatibility checks handle them.
                }
            }
            return fields.isEmpty() ? null : List.copyOf(fields.values());
        }

        private boolean hasLinkedSchema(CompiledModule module, String name) {
            return module.functions().stream().anyMatch(function -> function.name().equals(name));
        }

        private CompiledModule linkedModule(String path) {
            for (var module : linkedModules) {
                if (linkedModuleMatches(path, module)) {
                    return module;
                }
            }
            return stdlibImport(path)
                    ? bundledModule(normalizeImportModulePath(path)).orElse(null)
                    : null;
        }

        private boolean linkedModuleMatches(String path, CompiledModule module) {
            var modulePath = module.path().isBlank() ? module.name() : module.path() + "/" + module.name();
            return path.equals(module.name())
                    || normalizeModulePath(path).equals(modulePath)
                    || normalizeImportModulePath(path).equals(modulePath);
        }

        private PrimitiveBackedTypeDeclaration primitiveBackedTypeDeclaration(ParsedModule module, String name) {
            var local = localPrimitiveBackedTypeDeclaration(module, unqualified(name));
            if (local != null) {
                return local;
            }
            PrimitiveBackedTypeDeclaration match = null;
            for (var declaration : module.imports()) {
                if (declaration.qualified()
                        || (!declaration.wildcard() && !declaration.importedNames().contains(unqualified(name)))
                        || declaration.excludedNames().contains(unqualified(name))) {
                    continue;
                }
                var importedModule = parsedModule(declaration.modulePath());
                if (importedModule == null) {
                    continue;
                }
                var imported = localPrimitiveBackedTypeDeclaration(importedModule, unqualified(name));
                if (imported == null) {
                    continue;
                }
                if (match != null) {
                    return null;
                }
                match = imported;
            }
            return match;
        }

        private PrimitiveBackedTypeDeclaration localPrimitiveBackedTypeDeclaration(ParsedModule module, String name) {
            for (var definition : module.definitions()) {
                if (definition instanceof PrimitiveBackedTypeDeclaration primitive && primitive.name().equals(name)) {
                    return primitive;
                }
            }
            return null;
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

        private AnnotationDeclaration annotationDeclaration(ParsedModule module, String name) {
            if (!name.startsWith("/")) {
                return availableAnnotations(module).get(unqualified(name));
            }
            var separator = name.lastIndexOf('.');
            if (separator < 0) {
                return null;
            }
            var declaringModule = parsedModule(name.substring(0, separator));
            if (declaringModule == null) {
                return null;
            }
            return annotationsByModule.get(declaringModule).get(name.substring(separator + 1));
        }

        private ParsedModule parsedModule(String path) {
            var current = currentParsedModule(path);
            if (current != null) {
                return current;
            }
            if (libraryModule(path)) {
                for (var module : previouslyValidatedModules) {
                    if (parsedModuleMatches(path, module)) {
                        return module;
                    }
                }
            }
            return null;
        }

        private ParsedModule currentParsedModule(String path) {
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
                switch (definition) {
                    case AnnotationDeclaration annotation -> symbols.add(annotation.name());
                    case ConstantDefinition constant -> symbols.add(constant.constant().name());
                    case DataDeclaration data -> symbols.add(data.name());
                    case DeriverDeclaration deriver -> symbols.add(deriver.name());
                    case EnumDeclaration enumDeclaration -> {
                        symbols.add(enumDeclaration.name());
                        enumDeclaration.values().forEach(value -> symbols.add(value.name()));
                    }
                    case FunctionDefinition function -> symbols.add(function.function().name());
                    case PrimitiveBackedTypeDeclaration primitive -> symbols.add(primitive.name());
                    case TypeDeclaration type -> symbols.add(type.name());
                    default -> {
                    }
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
