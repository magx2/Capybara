package dev.capylang.compiler;

import dev.capylang.compiler.parser.Definition;
import dev.capylang.compiler.parser.Definition.ConstantDefinition;
import dev.capylang.compiler.parser.Definition.DataDeclaration;
import dev.capylang.compiler.parser.Definition.DataFieldDeclaration;
import dev.capylang.compiler.parser.Definition.EnumDeclaration;
import dev.capylang.compiler.parser.Definition.FunctionDefinition;
import dev.capylang.compiler.parser.Definition.TypeDeclaration;
import dev.capylang.compiler.parser.Expression;
import dev.capylang.compiler.parser.Expression.BinaryExpression;
import dev.capylang.compiler.parser.Expression.BlockExpression;
import dev.capylang.compiler.parser.Expression.BoolLiteral;
import dev.capylang.compiler.parser.Expression.DataLiteral;
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
import dev.capylang.compiler.parser.Expression.ListLiteral;
import dev.capylang.compiler.parser.Expression.LongLiteral;
import dev.capylang.compiler.parser.Expression.MatchExpression;
import dev.capylang.compiler.parser.Expression.MethodCallExpression;
import dev.capylang.compiler.parser.Expression.ReduceExpression;
import dev.capylang.compiler.parser.Expression.SetLiteral;
import dev.capylang.compiler.parser.Expression.StringLiteral;
import dev.capylang.compiler.parser.Expression.ThrowExpression;
import dev.capylang.compiler.parser.Expression.TryCatchExpression;
import dev.capylang.compiler.parser.Expression.TupleLiteral;
import dev.capylang.compiler.parser.Expression.UnaryExpression;
import dev.capylang.compiler.parser.Expression.UnsupportedExpression;
import dev.capylang.compiler.parser.Expression.VariableExpression;
import dev.capylang.compiler.parser.Expression.WithExpression;
import dev.capylang.compiler.parser.FunctionDeclaration;
import dev.capylang.compiler.parser.ImportDeclaration;
import dev.capylang.compiler.parser.ObjectOrientedClass;
import dev.capylang.compiler.parser.ObjectOrientedInterface;
import dev.capylang.compiler.parser.ObjectOrientedMethod;
import dev.capylang.compiler.parser.ParsedModule;
import dev.capylang.compiler.parser.SourceKind;
import dev.capylang.compiler.parser.SourceLocation;
import dev.capylang.compiler.parser.TypeReference;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Backend-independent, bidirectional type checking for parsed Capybara programs. */
final class StrictSemanticAnalyzer {
    private static final Type UNKNOWN = new Type("<unknown>", List.of(), List.of(), null);
    private static final Type ERROR = new Type("<error>", List.of(), List.of(), null);
    private static final Type ANY = simple("any");
    private static final Type BOOL = simple("bool");
    private static final Type INT = simple("int");
    private static final Type STRING = simple("String");
    private static final Type NOTHING = simple("nothing");
    private static final Set<String> NUMERIC = Set.of("byte", "int", "long", "float", "double", "size", "index");
    private static final Set<String> COMPARISON = Set.of("==", "!=", ">", "<", ">=", "<=");
    private static final Set<String> BOOLEAN = Set.of("&");
    private static final Set<String> NUMERIC_OPERATORS = Set.of("-", "*", "/", "%", "^", ".and.", ".or.", ".xor.", ".nand.");
    private static final Set<String> BUILTIN_METHODS = Set.of(
            "size", "is_empty", "get", "map", "flat_map", "filter", "reject", "fold", "reduce",
            "reduce_left", "any", "all", "contains", "plus", "minus", "sort", "first", "rest",
            "as_list", "or", "or_else", "recover", "start", "join", "trim", "replace", "compare",
            "starts_with", "end_with", "to_int", "to_long", "to_float", "to_double", "to_bool",
            "to_string", "entries", "contains_key", "find", "find_all", "matches", "split",
            "is_equal_to", "is_true", "is_false", "has_size", "succeeds", "fails", "fails_with_kind"
    );
    private static final Map<String, Integer> BUILTIN_ARITIES = Map.ofEntries(
            Map.entry("List", 1), Map.entry("Set", 1), Map.entry("Dict", 1), Map.entry("Option", 1),
            Map.entry("Result", 1), Map.entry("Effect", 1), Map.entry("Seq", 1), Map.entry("Either", 2),
            Map.entry("Async", 1), Map.entry("array", 1)
    );

    private final List<ParsedModule> modules;
    private final List<CompiledModule> linkedModules;
    private final Map<String, ParsedModule> modulesByPath = new LinkedHashMap<>();
    private final Map<String, CompiledModule> linkedByPath = new LinkedHashMap<>();
    private final List<CompilerError> errors = new ArrayList<>();

    StrictSemanticAnalyzer(List<ParsedModule> modules, List<CompiledModule> linkedModules) {
        this.modules = modules;
        this.linkedModules = linkedModules;
        modules.forEach(module -> modulesByPath.put(modulePath(module), module));
        linkedModules.forEach(module -> linkedByPath.put(modulePath(module), module));
    }

    List<CompilerError> analyze() {
        for (var module : modules) {
            // The standard library is the compiler's trusted signature source. Its implementation
            // contains primitive-backed/native representations which are intentionally opaque to
            // source-level checking; callers are still checked against the linked declarations.
            if (modulePath(module).startsWith("capy/")) continue;
            validateAmbiguousExplicitImports(module);
            validateDeclaredTypes(module);
            analyzeFunctional(module);
            analyzeObjects(module);
        }
        errors.sort(Comparator.comparing(CompilerError::moduleName)
                .thenComparingInt(CompilerError::line)
                .thenComparingInt(CompilerError::column)
                .thenComparing(CompilerError::code));
        return List.copyOf(errors);
    }

    static boolean isAssignableForTesting(TypeReference actual, TypeReference expected) {
        return new StrictSemanticAnalyzer(List.of(), List.of()).assignable(type(actual), type(expected));
    }

    private void validateDeclaredTypes(ParsedModule module) {
        for (var definition : module.definitions()) {
            switch (definition) {
                case ConstantDefinition constant -> validateTypeReference(module, constant.constant().typeReference(), constant.constant().location());
                case FunctionDefinition function -> {
                    validateTypeReference(module, function.function().returnType(), function.function().location());
                    function.function().parameters().forEach(parameter -> validateTypeReference(module, parameter.typeReference(), parameter.location()));
                }
                case DataDeclaration data -> {
                    data.fields().forEach(field -> validateTypeReference(module, field.typeReference(), field.location()));
                    data.parents().forEach(parent -> validateTypeReference(module, parent.typeReference(), parent.location()));
                }
                case TypeDeclaration union -> {
                    union.fields().forEach(field -> validateTypeReference(module, field.typeReference(), field.location()));
                    union.variants().forEach(variant -> validateTypeReference(module, variant, union.location()));
                }
                case Definition.PrimitiveBackedTypeDeclaration primitive -> validateTypeReference(module, primitive.backingType(), primitive.location());
                default -> { }
            }
        }
        module.objectOriented().classes().forEach(objectClass -> {
            objectClass.parameters().forEach(parameter -> validateTypeReference(module, parameter.typeReference(), parameter.location()));
            objectClass.parents().forEach(parent -> validateTypeReference(module, parent, objectClass.location()));
            objectClass.fields().forEach(field -> validateTypeReference(module, field.typeReference(), field.location()));
            objectClass.methods().forEach(method -> {
                validateTypeReference(module, method.returnType(), method.location());
                method.parameters().forEach(parameter -> validateTypeReference(module, parameter.typeReference(), parameter.location()));
            });
        });
        module.objectOriented().interfaces().forEach(objectInterface -> objectInterface.methods().forEach(method -> {
            validateTypeReference(module, method.returnType(), method.location());
            method.parameters().forEach(parameter -> validateTypeReference(module, parameter.typeReference(), parameter.location()));
        }));
    }

    private void validateTypeReference(ParsedModule module, TypeReference reference, SourceLocation location) {
        if (reference.name().isBlank() || reference.name().contains("=>")) return;
        var expected = declaredArity(module, unqualified(reference.name()), new HashSet<>());
        if (expected != null && reference.arguments().size() != expected) {
            report(module, location, "TYPE_ARITY", "Type `" + unqualified(reference.name()) + "` expects " + expected
                    + " type argument(s), but received " + reference.arguments().size() + ".");
        }
        reference.arguments().forEach(argument -> validateTypeReference(module, argument, location));
    }

    private Integer declaredArity(ParsedModule module, String name, Set<String> visited) {
        if (name.equals("Tuple")) return null;
        if (BUILTIN_ARITIES.containsKey(name)) return BUILTIN_ARITIES.get(name);
        // User-defined generic data may intentionally omit arguments when constructor context
        // supplies them; the linker performs the corresponding declaration validation.
        if (!Set.of("List", "Set", "Dict", "Option", "Result", "Effect", "Seq", "Either", "Async", "array").contains(name)) return null;
        if (!visited.add(modulePath(module))) return null;
        for (var definition : module.definitions()) {
            if (definition instanceof DataDeclaration data && data.name().equals(name)) return data.parameters().size();
            if (definition instanceof TypeDeclaration union && union.name().equals(name)) return union.parameters().size();
            if (definition instanceof EnumDeclaration value && value.name().equals(name)) return 0;
            if (definition instanceof Definition.PrimitiveBackedTypeDeclaration value && value.name().equals(name)) return 0;
        }
        if (module.objectOriented().classes().stream().anyMatch(value -> value.name().equals(name))
                || module.objectOriented().interfaces().stream().anyMatch(value -> value.name().equals(name))) return 0;
        for (var declaration : module.imports()) {
            var imported = resolveParsed(declaration.modulePath());
            if (imported != null) {
                var arity = declaredArity(imported, name, visited);
                if (arity != null) return arity;
            }
        }
        return null;
    }

    private void validateAmbiguousExplicitImports(ParsedModule module) {
        var owners = new LinkedHashMap<String, String>();
        for (var declaration : module.imports()) {
            if (declaration.qualified() || declaration.wildcard()) continue;
            for (var name : declaration.importedNames()) {
                var previous = owners.putIfAbsent(name, normalize(declaration.modulePath()));
                if (previous != null && !previous.equals(normalize(declaration.modulePath()))) {
                    report(module, declaration.location(), "AMBIGUOUS_SYMBOL",
                            "Symbol `" + name + "` is imported from both `/" + previous + "` and `/"
                                    + normalize(declaration.modulePath()) + "`.");
                }
            }
        }
    }

    private void analyzeFunctional(ParsedModule module) {
        for (var definition : module.definitions()) {
            switch (definition) {
                case ConstantDefinition constant -> {
                    var expected = type(constant.constant().typeReference());
                    var actual = infer(module, constant.constant().expression(), expected, new Env());
                    if (directlyCheckable(constant.constant().expression())) {
                        requireAssignable(module, constant.constant().location(), "ASSIGNMENT_TYPE", actual, expected,
                                "Constant `" + constant.constant().name() + "`");
                    }
                }
                case FunctionDefinition function -> analyzeFunction(module, function.function(), new Env());
                case DataDeclaration data -> validateTypeArity(module, data.name(), data.parameters().size(), data.location());
                default -> { }
            }
        }
    }

    private void analyzeFunction(ParsedModule module, FunctionDeclaration function, Env parent) {
        var env = parent.copy();
        function.parameters().forEach(parameter -> env.values.put(parameter.name(), type(parameter.typeReference())));
        if (function.name().contains(".")) {
            env.values.put("this", simple(function.name().substring(0, function.name().lastIndexOf('.'))));
        }
        var expected = function.returnType().name().isBlank() ? null : type(function.returnType());
        var actual = infer(module, function.body(), expected, env);
        if (expected == null && function.body() instanceof ListLiteral literal && literal.values().isEmpty()) {
            report(module, literal.location(), "CANNOT_INFER_TYPE",
                    "Cannot infer element type of empty `List`; add a type annotation.");
        }
        if (expected != null && directlyCheckable(function.body())
                && !(function.body() instanceof UnsupportedExpression unsupported && unsupported.source().equals("<native>"))) {
            requireAssignable(module, function.location(), "RETURN_TYPE", actual, expected,
                    "Function `" + displayName(function.name()) + "` return value");
        }
    }

    private void analyzeObjects(ParsedModule module) {
        for (var objectClass : module.objectOriented().classes()) {
            var owner = simple(objectClass.name());
            var base = new Env();
            base.values.put("this", owner);
            objectClass.parameters().forEach(parameter -> base.values.put(parameter.name(), type(parameter.typeReference())));
            objectClass.fields().forEach(field -> base.values.put(field.name(), type(field.typeReference())));
            objectClass.fields().stream().filter(field -> field.hasValue()).forEach(field -> {
                var actual = infer(module, field.value(), type(field.typeReference()), base.copy());
                requireAssignable(module, field.location(), "ASSIGNMENT_TYPE", actual, type(field.typeReference()),
                        "Field `" + field.name() + "`");
            });
            objectClass.initBlocks().forEach(block -> infer(module, block.body(), null, base.copy()));
            objectClass.methods().forEach(method -> analyzeObjectMethod(module, objectClass, method, base));
        }
        // Interfaces contribute signatures only; they do not have executable bodies to validate.
    }

    private void analyzeObjectMethod(ParsedModule module, ObjectOrientedClass owner, ObjectOrientedMethod method, Env base) {
        if (method.body() instanceof UnsupportedExpression unsupported && unsupported.source().isBlank()) return;
        var env = base.copy();
        method.parameters().forEach(parameter -> env.values.put(parameter.name(), type(parameter.typeReference())));
        var expected = type(method.returnType());
        var actual = infer(module, method.body(), expected, env);
        if (directlyCheckable(method.body())) {
            requireAssignable(module, method.location(), "RETURN_TYPE", actual, expected,
                    "Method `" + owner.name() + "." + method.name() + "` return value");
        }
    }

    private Type infer(ParsedModule module, Expression expression, Type expected, Env env) {
        return switch (expression) {
            case BoolLiteral ignored -> BOOL;
            case IntLiteral ignored -> INT;
            case LongLiteral ignored -> simple("long");
            case FloatLiteral ignored -> simple("float");
            case DoubleLiteral ignored -> simple("double");
            case StringLiteral ignored -> STRING;
            case VariableExpression variable -> variableType(module, variable, env);
            case FunctionCallExpression call -> functionCallType(module, call, expected, env);
            case FunctionReferenceExpression reference -> functionReferenceType(module, reference, expected);
            case MethodCallExpression call -> methodCallType(module, call, expected, env);
            case FieldAccessExpression access -> fieldAccessType(module, access, env);
            case DataLiteral literal -> dataLiteralType(module, literal, expected, env);
            case ListLiteral literal -> collectionType(module, "List", literal.values(), literal.location(), expected, env);
            case SetLiteral literal -> collectionType(module, "Set", literal.values(), literal.location(), expected, env);
            case DictLiteral literal -> dictType(module, literal, expected, env);
            case TupleLiteral literal -> new Type("Tuple", literal.values().stream()
                    .map(value -> infer(module, value, null, env)).toList(), List.of(), null);
            case IfExpression conditional -> ifType(module, conditional, expected, env);
            case BinaryExpression binary -> binaryType(module, binary, expected, env);
            case UnaryExpression unary -> unaryType(module, unary, env);
            case BlockExpression block -> blockType(module, block, expected, env);
            case LambdaExpression lambda -> lambdaType(module, lambda, expected, env);
            case IndexExpression index -> indexType(module, index, env);
            case ReduceExpression reduce -> reduceType(module, reduce, expected, env);
            case MatchExpression match -> matchType(module, match, expected, env);
            case ThrowExpression thrown -> { infer(module, thrown.value(), null, env); yield NOTHING; }
            case TryCatchExpression attempt -> tryCatchType(module, attempt, expected, env);
            case WithExpression with -> withType(module, with, expected, env);
            case UnsupportedExpression ignored -> UNKNOWN;
            default -> UNKNOWN;
        };
    }

    private Type variableType(ParsedModule module, VariableExpression variable, Env env) {
        var local = env.values.get(variable.name());
        if (local != null) return local;
        var constant = visibleConstant(module, variable.name());
        if (constant != null) return constant;
        if (enumValue(module, variable.name()) || knownFunctionName(module, variable.name())) return UNKNOWN;
        return UNKNOWN; // The established resolver owns unresolved-name diagnostics.
    }

    private Type functionCallType(ParsedModule module, FunctionCallExpression call, Type expected, Env env) {
        var callable = env.values.get(call.name());
        if (callable != null) {
            if (dynamic(callable)) {
                call.arguments().forEach(argument -> infer(module, argument, null, env));
                return ANY;
            }
            if (callable.functionResult == null) {
                report(module, call.location(), "NOT_CALLABLE",
                        "Value `" + call.name() + "` has type `" + callable + "` and cannot be invoked.");
                return ERROR;
            }
            checkArguments(module, call.name(), call.arguments(), callable.parameters, env, call.location());
            return callable.functionResult;
        }
        var constant = visibleConstant(module, call.name());
        if (constant != null) {
            report(module, call.location(), "NOT_CALLABLE",
                    "Value `" + call.name() + "` has type `" + constant + "` and cannot be invoked.");
            return ERROR;
        }
        var candidates = visibleFunctions(module, call.name());
        if (candidates.isEmpty()) return UNKNOWN; // Existing name validation owns the primary diagnostic.
        var arity = candidates.stream().filter(candidate -> candidate.parameters.size() == call.arguments().size()).toList();
        if (arity.isEmpty()) return ERROR; // Existing arity validation owns the diagnostic.
        var chosen = bestCandidate(module, arity, call.arguments(), env);
        if (chosen == null) {
            return UNKNOWN; // The established overload resolver has richer import information.
        }
        var substitutions = inferSubstitutions(module, chosen, call.arguments(), env);
        var parameters = chosen.parameters.stream().map(type -> substitute(type, substitutions)).toList();
        checkArguments(module, call.name(), call.arguments(), parameters, env, call.location());
        return substitute(chosen.result, substitutions);
    }

    private Type functionReferenceType(ParsedModule module, FunctionReferenceExpression reference, Type expected) {
        var candidates = visibleFunctions(module, reference.name());
        if (expected != null && expected.functionResult != null) {
            candidates = candidates.stream().filter(candidate -> candidate.parameters.size() == expected.parameters.size()).toList();
            if (candidates.size() == 1) {
                var candidate = candidates.getFirst();
                var actual = function(candidate.parameters, candidate.result);
                if (!assignable(module, actual, expected)) {
                    report(module, reference.location(), "ARGUMENT_TYPE",
                            "Function reference `:" + reference.name() + "` has type `" + actual + "`, but `" + expected + "` is required.");
                }
                return actual;
            }
        }
        if (candidates.size() == 1) return function(candidates.getFirst().parameters, candidates.getFirst().result);
        report(module, reference.location(), "CANNOT_INFER_TYPE",
                "Cannot infer a unique callable type for function reference `:" + reference.name() + "`.");
        return ERROR;
    }

    private Type methodCallType(ParsedModule module, MethodCallExpression call, Type expected, Env env) {
        var receiver = infer(module, call.receiver(), null, env);
        if (dynamic(receiver)) {
            call.arguments().forEach(argument -> infer(module, argument, null, env));
            return ANY;
        }
        var candidates = visibleMethods(module, receiver.name, call.name());
        var matching = candidates.stream().filter(candidate -> candidate.parameters.size() == call.arguments().size()).toList();
        if (!matching.isEmpty()) {
            var chosen = bestMethodCandidate(module, matching, receiver, call.arguments(), env);
            if (chosen == null) {
                return UNKNOWN;
            }
            var substitutions = new HashMap<String, Type>();
            bind(chosen.owner, receiver, substitutions);
            inferMethodSubstitutions(module, chosen, call.arguments(), env, substitutions);
            var parameters = chosen.parameters.stream().map(type -> substitute(type, substitutions)).toList();
            checkArguments(module, call.name(), call.arguments(), parameters, env, call.location());
            checkCallableArguments(module, call.name(), call.arguments(), parameters, env);
            return substitute(chosen.result, substitutions);
        }
        if (BUILTIN_METHODS.contains(call.name())) {
            call.arguments().forEach(argument -> infer(module, argument, null, env));
            return builtinMethodResult(receiver, call, expected, env, module);
        }
        if (candidates.isEmpty() && locallyDeclaresNominal(module, receiver.name)) {
            report(module, call.location(), "METHOD_NOT_FOUND",
                    "Method `" + call.name() + "` is not defined for receiver type `" + receiver + "`.");
        }
        return ERROR;
    }

    private Type builtinMethodResult(Type receiver, MethodCallExpression call, Type expected, Env env, ParsedModule module) {
        if (Set.of("size").contains(call.name())) return INT;
        if (Set.of("is_empty", "contains", "any", "all", "starts_with", "end_with", "contains_key", "matches").contains(call.name())) return BOOL;
        if (call.name().equals("as_list")) return new Type("List", receiver.arguments, List.of(), null);
        if (Set.of("map", "flat_map", "filter", "reject").contains(call.name()) && !receiver.arguments.isEmpty()) {
            var element = receiver.arguments.getFirst();
            if (!call.arguments().isEmpty()) {
                var mapperType = infer(module, call.arguments().getFirst(), function(List.of(element), UNKNOWN), env);
                if (mapperType.functionResult != null) {
                    if (call.name().equals("map")) {
                        return new Type(receiver.name, List.of(mapperType.functionResult), List.of(), null);
                    }
                    if (call.name().equals("flat_map")
                            && unqualified(mapperType.functionResult.name).equals(unqualified(receiver.name))) {
                        return mapperType.functionResult;
                    }
                }
            }
            return receiver;
        }
        if (call.name().startsWith("to_")) {
            var target = call.name().substring(3);
            return switch (target) {
                case "int", "long", "float", "double", "bool" -> new Type("Result", List.of(simple(target)), List.of(), null);
                case "string" -> STRING;
                default -> UNKNOWN;
            };
        }
        return expected == null ? UNKNOWN : expected;
    }

    private Type fieldAccessType(ParsedModule module, FieldAccessExpression access, Env env) {
        var receiver = infer(module, access.receiver(), null, env);
        if (dynamic(receiver)) return ANY;
        var field = dataFields(module, receiver.name).stream().filter(candidate -> candidate.name().equals(access.name())).findFirst().orElse(null);
        if (field != null) return type(field.typeReference());
        var objectField = objectClass(module, receiver.name);
        if (objectField != null) {
            var match = objectField.fields().stream().filter(candidate -> candidate.name().equals(access.name())).findFirst().orElse(null);
            if (match != null) return type(match.typeReference());
        }
        if (Set.of("Some", "Success", "Cons").contains(receiver.name) && access.name().equals("value") && !receiver.arguments.isEmpty()) {
            return receiver.arguments.getFirst();
        }
        if (locallyDeclaresNominal(module, receiver.name)) {
            report(module, access.location(), "FIELD_NOT_FOUND",
                    "Field `" + access.name() + "` is not defined on type `" + receiver + "`.");
            return ERROR;
        }
        return UNKNOWN;
    }

    private Type dataLiteralType(ParsedModule module, DataLiteral literal, Type expected, Env env) {
        return dataLiteralType(module, literal, expected, env, true);
    }

    private Type dataLiteralType(ParsedModule module, DataLiteral literal, Type expected, Env env,
                                 boolean requireAllFields) {
        var result = simple(stripRaw(literal.typeName()));
        if (expected != null && switch (unqualified(expected.name)) {
            case "Result" -> Set.of("Success", "Error").contains(unqualified(result.name));
            case "Option" -> Set.of("Some", "None").contains(unqualified(result.name));
            case "Either" -> Set.of("Left", "Right").contains(unqualified(result.name));
            case "Seq" -> Set.of("Cons", "End").contains(unqualified(result.name));
            default -> false;
        }) {
            literal.fields().forEach(field -> infer(module, field.value(), null, env));
            return expected;
        }
        var promotedResult = expected != null && subtype(module, result.name, expected.name) ? expected : result;
        var fields = dataFields(module, result.name);
        if (fields.isEmpty()) {
            literal.fields().forEach(field -> infer(module, field.value(), null, env));
            return promotedResult;
        }
        var seen = new HashSet<String>();
        var positional = 0;
        for (var value : literal.fields()) {
            if (value.spread()) {
                infer(module, value.value(), result, env);
                continue;
            }
            var name = value.name().startsWith("$") ? (positional < fields.size() ? fields.get(positional).name() : value.name()) : value.name();
            positional++;
            var declaration = fields.stream().filter(field -> field.name().equals(name)).findFirst().orElse(null);
            if (declaration == null) {
                report(module, value.location(), "CONSTRUCTOR_FIELD",
                        "Data `" + result.name + "` has no field `" + name + "`.");
                infer(module, value.value(), null, env);
                continue;
            }
            seen.add(name);
            var required = type(declaration.typeReference());
            var actual = infer(module, value.value(), required, env);
            if (!generic(required) && required.functionResult == null && !primitiveBacked(module, required.name)) {
                requireAssignable(module, value.location(), "ASSIGNMENT_TYPE", actual, required,
                        "Field `" + name + "` of data `" + result.name + "`");
            }
        }
        if (requireAllFields && literal.fields().stream().noneMatch(Expression.DataField::spread)) {
            fields.stream().filter(field -> !seen.contains(field.name())).forEach(field -> report(module, literal.location(),
                    "CONSTRUCTOR_FIELD", "Data `" + result.name + "` requires field `" + field.name() + "`."));
        }
        return promotedResult;
    }

    private Type collectionType(ParsedModule module, String name, List<Expression> values, SourceLocation location, Type expected, Env env) {
        var expectedElement = expected != null && expected.name.equals(name) && expected.arguments.size() == 1
                ? expected.arguments.getFirst() : null;
        if (values.isEmpty()) {
            if (expectedElement == null) {
                return new Type(name, List.of(UNKNOWN), List.of(), null);
            }
            return new Type(name, List.of(expectedElement), List.of(), null);
        }
        var element = expectedElement == null ? infer(module, values.getFirst(), null, env) : expectedElement;
        for (var value : values) {
            var actual = infer(module, value, element, env);
            requireAssignable(module, location(value), "ASSIGNMENT_TYPE", actual, element, name + " element");
        }
        return new Type(name, List.of(element), List.of(), null);
    }

    private Type dictType(ParsedModule module, DictLiteral literal, Type expected, Env env) {
        Type key = STRING;
        Type value = expected != null && expected.name.equals("Dict") && expected.arguments.size() == 1 ? expected.arguments.getFirst() : null;
        if (literal.entries().isEmpty() && value == null) {
            report(module, literal.location(), "CANNOT_INFER_TYPE", "Cannot infer key and value types of empty `Dict`; add a type annotation.");
            return ERROR;
        }
        for (var entry : literal.entries()) {
            var actualKey = infer(module, entry.key(), key, env);
            var actualValue = infer(module, entry.value(), value, env);
            requireAssignable(module, entry.location(), "ASSIGNMENT_TYPE", actualKey, key, "Dict key");
            if (value == null) value = actualValue; else requireAssignable(module, entry.location(), "ASSIGNMENT_TYPE", actualValue, value, "Dict value");
        }
        return new Type("Dict", List.of(value), List.of(), null);
    }

    private Type ifType(ParsedModule module, IfExpression conditional, Type expected, Env env) {
        var condition = infer(module, conditional.condition(), BOOL, env);
        requireAssignable(module, location(conditional.condition()), "CONDITION_TYPE", condition, BOOL, "If condition");
        var thenType = infer(module, conditional.thenBranch(), expected, env.copy());
        var elseType = infer(module, conditional.elseBranch(), expected, env.copy());
        if (expected != null) {
            requireAssignable(module, location(conditional.thenBranch()), "ASSIGNMENT_TYPE",
                    thenType, expected, "If then branch");
            requireAssignable(module, location(conditional.elseBranch()), "ASSIGNMENT_TYPE",
                    elseType, expected, "If else branch");
            return expected;
        }
        if (assignable(module, thenType, elseType)) return thenType;
        if (assignable(module, elseType, thenType)) return elseType;
        report(module, conditional.location(), "ASSIGNMENT_TYPE",
                "If branches have incompatible types `" + thenType + "` and `" + elseType + "`.");
        return ERROR;
    }

    private Type binaryType(ParsedModule module, BinaryExpression binary, Type expected, Env env) {
        if (Set.of("|", "|-", "|*", "|>", "|!").contains(binary.operator())) {
            return expected == null ? UNKNOWN : expected;
        }
        var left = infer(module, binary.left(), null, env);
        var right = infer(module, binary.right(), left, env);
        var effectiveLeft = primitiveBackedEffectiveType(left);
        var effectiveRight = primitiveBackedEffectiveType(right);
        if (COMPARISON.contains(binary.operator())) {
            if (!comparable(left, right)) report(module, binary.location(), "OPERATOR_TYPE",
                    "Operator `" + binary.operator() + "` cannot compare `" + left + "` with `" + right + "`.");
            return BOOL;
        }
        if (BOOLEAN.contains(binary.operator())) {
            requireAssignable(module, binary.location(), "OPERATOR_TYPE", left, BOOL, "Left operand");
            requireAssignable(module, binary.location(), "OPERATOR_TYPE", right, BOOL, "Right operand");
            return BOOL;
        }
        if (binary.operator().equals("+") && (left.name.equals("String") || left.name.equals("List")
                || left.name.equals("Set") || left.name.equals("Dict") || left.name.equals("Seq"))) {
            return left;
        }
        if (binary.operator().equals("-") && Set.of("List", "Set", "Dict").contains(left.name)) return left;
        if (binary.operator().equals("+") || NUMERIC_OPERATORS.contains(binary.operator())) {
            if (!dynamic(effectiveLeft) && !dynamic(effectiveRight)
                    && (!numeric(effectiveLeft) || !numeric(effectiveRight))
                    && scalarPrimitive(effectiveLeft) && scalarPrimitive(effectiveRight)) {
                report(module, binary.location(), "OPERATOR_TYPE",
                        "Operator `" + binary.operator() + "` requires numeric operands, but received `" + left + "` and `" + right + "`.");
                return ERROR;
            }
            return numeric(effectiveLeft) && numeric(effectiveRight)
                    ? wider(effectiveLeft, effectiveRight)
                    : expected == null ? UNKNOWN : expected;
        }
        return expected == null ? UNKNOWN : expected;
    }

    private Type unaryType(ParsedModule module, UnaryExpression unary, Env env) {
        var operand = infer(module, unary.expression(), null, env);
        if (unary.operator().equals("!")) {
            requireAssignable(module, unary.location(), "OPERATOR_TYPE", operand, BOOL, "Unary `!` operand");
            return BOOL;
        }
        var effectiveOperand = primitiveBackedEffectiveType(operand);
        if (!numeric(effectiveOperand)) report(module, unary.location(), "OPERATOR_TYPE",
                "Unary operator `" + unary.operator() + "` requires a numeric operand, but received `" + operand + "`.");
        return effectiveOperand;
    }

    private Type blockType(ParsedModule module, BlockExpression block, Type expected, Env outer) {
        var env = outer.copy();
        for (var binding : block.bindings()) {
            var declared = binding.typeReference().name().isBlank() ? null : type(binding.typeReference());
            var actual = infer(module, binding.value(), declared, env);
            if (binding.operator().equals("<-") && !dynamic(actual)) {
                if (!Set.of("Effect", "Result", "Option", "Either").contains(actual.name) || actual.arguments.isEmpty()) {
                    actual = UNKNOWN; // The established effect/result validator owns this diagnostic.
                } else {
                    actual = actual.arguments.getLast();
                }
            }
            if (declared != null) requireBindingAssignable(module, binding, actual, declared);
            env.values.put(binding.name(), declared == null ? actual : declared);
        }
        return infer(module, block.result(), expected, env);
    }

    private void requireBindingAssignable(ParsedModule module, Expression.LetBinding binding, Type actual, Type expected) {
        if (assignable(module, actual, expected)) return;
        if (distinctSequenceListMismatch(actual, expected)) {
            report(module, binding.location(), "ASSIGNMENT_TYPE",
                    "Binding `" + binding.name() + "` has type `" + actual + "`, but declares `" + expected
                            + "`; use an explicit `to_seq` or `as_list` conversion.");
            return;
        }
        requireAssignable(module, binding.location(), "ASSIGNMENT_TYPE", actual, expected,
                "Binding `" + binding.name() + "`");
    }

    private boolean distinctSequenceListMismatch(Type actual, Type expected) {
        var actualName = unqualified(actual.name);
        var expectedName = unqualified(expected.name);
        if ((actualName.equals("Seq") && expectedName.equals("List"))
                || (actualName.equals("List") && expectedName.equals("Seq"))) {
            return true;
        }
        if (!actualName.equals(expectedName) || actual.arguments.size() != expected.arguments.size()) {
            return false;
        }
        for (var index = 0; index < actual.arguments.size(); index++) {
            if (distinctSequenceListMismatch(actual.arguments.get(index), expected.arguments.get(index))) {
                return true;
            }
        }
        return false;
    }

    private Type lambdaType(ParsedModule module, LambdaExpression lambda, Type expected, Env outer) {
        var expectedParameters = expected != null && expected.functionResult != null ? expected.parameters : List.<Type>of();
        var env = outer.copy();
        var parameters = new ArrayList<Type>();
        for (var index = 0; index < lambda.parameters().size(); index++) {
            var encoded = lambda.parameters().get(index);
            var declared = decodedLambdaType(encoded);
            var contextual = index < expectedParameters.size() ? expectedParameters.get(index) : null;
            var parameter = declared != null ? declared : contextual;
            if (parameter == null) {
                parameter = ANY;
            }
            parameters.add(parameter);
            if (!decodedLambdaName(encoded).equals("_")) env.values.put(decodedLambdaName(encoded), parameter);
        }
        var expectedReturn = expected == null ? null : expected.functionResult;
        var result = infer(module, lambda.body(), expectedReturn, env);
        return function(parameters, result);
    }

    private Type indexType(ParsedModule module, IndexExpression index, Env env) {
        var receiver = infer(module, index.receiver(), null, env);
        var key = infer(module, index.index(), receiver.name.equals("Dict") ? STRING : INT, env);
        if (receiver.name.equals("Dict") && receiver.arguments.size() == 1) {
            requireAssignable(module, index.location(), "OPERATOR_TYPE", key, STRING, "Dict index");
            return receiver.arguments.getFirst();
        }
        requireAssignable(module, index.location(), "OPERATOR_TYPE", key, INT, "Index");
        if (index.hasEndIndex()) requireAssignable(module, index.location(), "OPERATOR_TYPE",
                infer(module, index.endIndex(), INT, env), INT, "Slice end");
        if (receiver.name.equals("String")) return index.hasEndIndex() ? STRING : simple("char");
        if (Set.of("List", "Seq", "Tuple").contains(receiver.name) && !receiver.arguments.isEmpty()) {
            return index.hasEndIndex() ? receiver : receiver.arguments.getFirst();
        }
        return ERROR;
    }

    private Type reduceType(ParsedModule module, ReduceExpression reduce, Type expected, Env outer) {
        var receiver = infer(module, reduce.receiver(), null, outer);
        var initialExpected = reduce.initial() instanceof ListLiteral
                ? new Type("List", List.of(ANY), List.of(), null) : expected;
        var initial = infer(module, reduce.initial(), initialExpected, outer);
        var env = outer.copy();
        env.values.put(reduce.accumulatorName(), initial);
        if (!receiver.arguments.isEmpty()) env.values.put(reduce.valueName(), receiver.arguments.getLast());
        var result = infer(module, reduce.body(), initial, env);
        requireAssignable(module, reduce.location(), "ASSIGNMENT_TYPE", result, initial, "Reduce body");
        return initial;
    }

    private Type matchType(ParsedModule module, MatchExpression match, Type expected, Env env) {
        infer(module, match.value(), null, env);
        Type result = expected;
        for (var branch : match.cases()) {
            if (!branch.bindsWholeValue() && unionType(module, branch.typeName())) {
                report(module, branch.location(), "PATTERN_TYPE", "Union type `" + unqualified(branch.typeName())
                        + "` cannot be used as a constructor pattern; match one of its variants instead.");
            }
            var branchEnv = env.copy();
            branch.bindings().stream().filter(name -> !name.equals("_")).forEach(name -> branchEnv.values.put(name, ANY));
            if (branch.hasGuard()) requireAssignable(module, branch.location(), "CONDITION_TYPE",
                    infer(module, branch.guard(), BOOL, branchEnv), BOOL, "Match guard");
            var branchType = infer(module, branch.body(), expected, branchEnv);
            if (result == null) result = branchType;
            else { /* Control-flow-aware validation owns branch compatibility. */ }
        }
        return result == null ? UNKNOWN : result;
    }

    private boolean unionType(ParsedModule module, String name) {
        if (name.isBlank()) return false;
        var typeName = unqualified(name);
        var fragments = moduleFragments(module);
        if (fragments.stream().anyMatch(fragment -> localNominalType(fragment, typeName))) {
            return fragments.stream().anyMatch(fragment -> localUnionType(fragment, typeName));
        }
        for (var declaration : fragments.stream().flatMap(fragment -> fragment.imports().stream()).toList()) {
            if (declaration.qualified() || !exposes(declaration, typeName)) continue;
            var imported = resolveParsed(declaration.modulePath());
            if (imported != null && moduleFragments(imported).stream()
                    .anyMatch(fragment -> localUnionType(fragment, typeName))) return true;
            var linked = resolveLinked(declaration.modulePath());
            if (linked == null && normalize(declaration.modulePath()).startsWith("capy/")) {
                linked = NativeCompilerValidator.bundledModule(normalize(declaration.modulePath())).orElse(null);
            }
            if (linked != null && linkedSchemaValue(linked, "__capy_schema_kind|" + typeName).equals("union")) return true;
        }
        return false;
    }

    private List<ParsedModule> moduleFragments(ParsedModule module) {
        var path = modulePath(module);
        return modules.stream().filter(candidate -> modulePath(candidate).equals(path)).toList();
    }

    private boolean localNominalType(ParsedModule module, String name) {
        return module.definitions().stream().anyMatch(definition -> switch (definition) {
            case DataDeclaration data -> data.name().equals(name);
            case TypeDeclaration union -> union.name().equals(name);
            case EnumDeclaration enumeration -> enumeration.name().equals(name);
            case Definition.PrimitiveBackedTypeDeclaration primitive -> primitive.name().equals(name);
            default -> false;
        }) || module.objectOriented().classes().stream().anyMatch(value -> value.name().equals(name))
                || module.objectOriented().interfaces().stream().anyMatch(value -> value.name().equals(name));
    }

    private boolean localUnionType(ParsedModule module, String name) {
        return module != null && module.definitions().stream()
                .filter(TypeDeclaration.class::isInstance)
                .map(TypeDeclaration.class::cast)
                .anyMatch(union -> union.name().equals(name));
    }

    private String linkedSchemaValue(CompiledModule module, String name) {
        return module.functions().stream()
                .filter(function -> function.name().equals(name))
                .map(function -> function.body())
                .filter(CompiledExpression.CompiledStringLiteral.class::isInstance)
                .map(CompiledExpression.CompiledStringLiteral.class::cast)
                .map(CompiledExpression.CompiledStringLiteral::value)
                .findFirst()
                .orElse("");
    }

    private Type tryCatchType(ParsedModule module, TryCatchExpression attempt, Type expected, Env env) {
        var result = infer(module, attempt.body(), expected, env);
        for (var branch : attempt.branches()) {
            var catchEnv = env.copy();
            catchEnv.values.put(branch.catchName(), simple("Error"));
            var branchType = infer(module, branch.catchBody(), expected, catchEnv);
        }
        return result;
    }

    private Type withType(ParsedModule module, WithExpression with, Type expected, Env env) {
        var receiver = infer(module, with.receiver(), expected, env);
        var synthetic = new DataLiteral(receiver.name, with.fields(), with.location());
        dataLiteralType(module, synthetic, receiver, env, false);
        return receiver;
    }

    private void checkArguments(ParsedModule module, String name, List<Expression> arguments, List<Type> parameters,
                                Env env, SourceLocation location) {
        if (arguments.size() != parameters.size()) return;
        for (var index = 0; index < arguments.size(); index++) {
            if (!directlyCheckable(arguments.get(index))) continue;
            var actual = infer(module, arguments.get(index), parameters.get(index), env);
            requireAssignable(module, location(arguments.get(index)), "ARGUMENT_TYPE", actual, parameters.get(index),
                    "Argument " + (index + 1) + " of `" + name + "`");
        }
    }

    private FunctionSig bestCandidate(ParsedModule module, List<FunctionSig> candidates, List<Expression> arguments, Env env) {
        var compatible = candidates.stream().filter(candidate -> {
            var substitutions = inferSubstitutions(module, candidate, arguments, env);
            for (var index = 0; index < arguments.size(); index++) {
                var actual = probe(module, arguments.get(index), env);
                if (!assignable(module, actual, substitute(candidate.parameters.get(index), substitutions))) return false;
            }
            return true;
        }).toList();
        return compatible.size() == 1 ? compatible.getFirst() : candidates.size() == 1 ? candidates.getFirst() : null;
    }

    private FunctionSig bestMethodCandidate(
            ParsedModule module,
            List<FunctionSig> candidates,
            Type receiver,
            List<Expression> arguments,
            Env env
    ) {
        var compatible = candidates.stream().filter(candidate -> {
            var substitutions = new HashMap<String, Type>();
            bind(candidate.owner, receiver, substitutions);
            inferMethodSubstitutions(module, candidate, arguments, env, substitutions);
            for (var index = 0; index < arguments.size(); index++) {
                var expected = substitute(candidate.parameters.get(index), substitutions);
                var actual = probe(module, arguments.get(index), expected, env);
                if (!assignable(module, actual, expected)) return false;
            }
            return true;
        }).toList();
        return compatible.size() == 1 ? compatible.getFirst() : candidates.size() == 1 ? candidates.getFirst() : null;
    }

    private void inferMethodSubstitutions(
            ParsedModule module,
            FunctionSig signature,
            List<Expression> arguments,
            Env env,
            Map<String, Type> substitutions
    ) {
        for (var index = 0; index < Math.min(signature.parameters.size(), arguments.size()); index++) {
            var expected = substitute(signature.parameters.get(index), substitutions);
            var actual = probe(module, arguments.get(index), expected, env);
            bind(signature.parameters.get(index), actual, substitutions);
        }
    }

    private void checkCallableArguments(
            ParsedModule module,
            String name,
            List<Expression> arguments,
            List<Type> parameters,
            Env env
    ) {
        for (var index = 0; index < Math.min(arguments.size(), parameters.size()); index++) {
            var argument = arguments.get(index);
            if (!(argument instanceof LambdaExpression) && !(argument instanceof FunctionReferenceExpression)) continue;
            var actual = infer(module, argument, parameters.get(index), env);
            requireAssignable(module, location(argument), "ARGUMENT_TYPE", actual, parameters.get(index),
                    "Argument " + (index + 1) + " of `" + name + "`");
        }
    }

    private Map<String, Type> inferSubstitutions(ParsedModule module, FunctionSig signature, List<Expression> arguments, Env env) {
        var result = new HashMap<String, Type>();
        for (var index = 0; index < Math.min(signature.parameters.size(), arguments.size()); index++) {
            bind(signature.parameters.get(index), probe(module, arguments.get(index), env), result);
        }
        return result;
    }

    private Type probe(ParsedModule module, Expression expression, Env env) {
        return probe(module, expression, null, env);
    }

    private Type probe(ParsedModule module, Expression expression, Type expected, Env env) {
        var size = errors.size();
        var result = infer(module, expression, expected, env.copy());
        while (errors.size() > size) errors.removeLast();
        return result;
    }

    private void bind(Type declared, Type actual, Map<String, Type> substitutions) {
        if (generic(declared)) {
            substitutions.putIfAbsent(declared.name, actual);
            return;
        }
        if (declared.functionResult != null && actual.functionResult != null) {
            for (var index = 0; index < Math.min(declared.parameters.size(), actual.parameters.size()); index++) {
                bind(declared.parameters.get(index), actual.parameters.get(index), substitutions);
            }
            bind(declared.functionResult, actual.functionResult, substitutions);
            return;
        }
        for (var index = 0; index < Math.min(declared.arguments.size(), actual.arguments.size()); index++) {
            bind(declared.arguments.get(index), actual.arguments.get(index), substitutions);
        }
    }

    private Type substitute(Type type, Map<String, Type> substitutions) {
        if (generic(type)) return substitutions.getOrDefault(type.name, type);
        if (type.functionResult != null) return function(type.parameters.stream().map(value -> substitute(value, substitutions)).toList(),
                substitute(type.functionResult, substitutions));
        return new Type(type.name, type.arguments.stream().map(value -> substitute(value, substitutions)).toList(), List.of(), null);
    }

    private boolean assignable(Type actual, Type expected) {
        if (actual == null || expected == null || dynamic(actual) || dynamic(expected) || actual == ERROR || expected == ERROR) return true;
        if (generic(actual) || generic(expected) || expected.name.equals("void")) return true;
        if (actual.name.equals("nothing")) return true;
        if (expected.functionResult != null || actual.functionResult != null) {
            if (expected.functionResult == null || actual.functionResult == null || expected.parameters.size() != actual.parameters.size()) return false;
            for (var index = 0; index < expected.parameters.size(); index++) {
                if (!assignable(expected.parameters.get(index), actual.parameters.get(index))) return false;
            }
            return assignable(actual.functionResult, expected.functionResult);
        }
        if (numeric(actual) && numeric(expected)) return numericRank(actual.name) <= numericRank(expected.name);
        if (!unqualified(actual.name).equals(unqualified(expected.name))) return false;
        if (actual.arguments.size() != expected.arguments.size()) return actual.arguments.isEmpty() || expected.arguments.isEmpty();
        for (var index = 0; index < actual.arguments.size(); index++) {
            if (!assignable(actual.arguments.get(index), expected.arguments.get(index))) return false;
        }
        return true;
    }

    private boolean assignable(ParsedModule module, Type actual, Type expected) {
        if (actual == null || expected == null || dynamic(actual) || dynamic(expected)
                || actual == ERROR || expected == ERROR) {
            return true;
        }
        if (expected.functionResult != null || actual.functionResult != null) {
            if (expected.functionResult == null || actual.functionResult == null
                    || expected.parameters.size() != actual.parameters.size()) {
                return false;
            }
            for (var index = 0; index < expected.parameters.size(); index++) {
                if (!assignable(module, expected.parameters.get(index), actual.parameters.get(index))) return false;
            }
            return assignable(module, actual.functionResult, expected.functionResult);
        }
        if (assignable(actual, expected)) return true;
        if (!subtype(module, actual.name, expected.name)) return false;
        if (actual.arguments.isEmpty()) return true;
        if (actual.arguments.size() != expected.arguments.size()) return false;
        for (var index = 0; index < actual.arguments.size(); index++) {
            if (!assignable(module, actual.arguments.get(index), expected.arguments.get(index))) return false;
        }
        return true;
    }

    private void requireAssignable(ParsedModule module, SourceLocation location, String code, Type actual, Type expected, String subject) {
        if (!assignable(module, actual, expected)) report(module, location, code,
                subject + " has type `" + actual + "`, but `" + expected + "` is required.");
    }

    private List<FunctionSig> visibleFunctions(ParsedModule module, String name) {
        var result = new ArrayList<FunctionSig>();
        collectFunctions(module, name, result);
        var separator = name.lastIndexOf('.');
        if (separator > 0) {
            var modulePath = qualifiedModulePath(module, name.substring(0, separator));
            if (modulePath != null) {
                collectPublicFunctions(resolveParsed(modulePath), name.substring(separator + 1), result);
                collectPublicFunctions(resolveLinked(modulePath), name.substring(separator + 1), result);
            }
            return deduplicate(result);
        }
        for (var declaration : module.imports()) {
            if (declaration.qualified() || !exposes(declaration, name)) continue;
            collectFunctions(resolveParsed(declaration.modulePath()), name, result);
            collectFunctions(resolveLinked(declaration.modulePath()), name, result);
        }
        return deduplicate(result);
    }

    private String qualifiedModulePath(ParsedModule module, String qualifier) {
        if (qualifier.startsWith("/")) return qualifier;
        return module.imports().stream()
                .filter(ImportDeclaration::qualified)
                .filter(declaration -> unqualified(declaration.modulePath()).equals(qualifier))
                .map(ImportDeclaration::modulePath)
                .findFirst()
                .orElse(null);
    }

    private List<FunctionSig> visibleMethods(ParsedModule module, String receiver, String name) {
        var result = new ArrayList<FunctionSig>();
        collectMethods(module, receiver, name, result);
        for (var declaration : module.imports()) {
            if (declaration.qualified()) continue;
            collectMethods(resolveParsed(declaration.modulePath()), receiver, name, result);
            collectMethods(resolveLinked(declaration.modulePath()), receiver, name, result);
        }
        collectObjectMethods(module, receiver, name, result, new HashSet<>());
        return deduplicate(result);
    }

    private void collectMethods(ParsedModule module, String receiver, String name, List<FunctionSig> target) {
        if (module == null) return;
        module.definitions().stream().filter(FunctionDefinition.class::isInstance).map(FunctionDefinition.class::cast)
                .map(FunctionDefinition::function)
                .filter(function -> extensionMethodMatches(function.name(), receiver, name))
                .map(this::signature).forEach(target::add);
    }

    private void collectMethods(CompiledModule module, String receiver, String name, List<FunctionSig> target) {
        if (module == null) return;
        module.functions().stream().filter(function -> extensionMethodMatches(function.name(), receiver, name))
                .map(function -> new FunctionSig(
                        function.name(), function.parameters().stream().map(parameter -> type(parameter.typeReference())).toList(),
                        type(function.returnType()), ownerType(function.name())))
                .forEach(target::add);
    }

    private boolean extensionMethodMatches(String functionName, String receiver, String methodName) {
        var separator = functionName.lastIndexOf('.');
        if (separator <= 0) return false;
        var candidateName = functionName.substring(separator + 1);
        if (candidateName.startsWith("`") && candidateName.endsWith("`")) {
            candidateName = candidateName.substring(1, candidateName.length() - 1);
        }
        if (!candidateName.equals(methodName)) return false;
        return unqualified(parseType(functionName.substring(0, separator)).name)
                .equals(unqualified(receiver));
    }

    private void collectObjectMethods(
            ParsedModule module,
            String receiver,
            String name,
            List<FunctionSig> target,
            Set<String> visited
    ) {
        var nominal = unqualified(receiver);
        if (!visited.add(nominal)) return;
        var owner = objectClass(module, nominal);
        if (owner != null) {
            owner.methods().stream().filter(method -> method.name().equals(name))
                    .map(method -> signature(nominal, method)).forEach(target::add);
            owner.parents().forEach(parent -> collectObjectMethods(
                    module, parent.name(), name, target, visited));
        }
        var objectInterface = objectInterface(module, nominal);
        if (objectInterface != null) {
            objectInterface.methods().stream().filter(method -> method.name().equals(name))
                    .map(method -> signature(nominal, method)).forEach(target::add);
            objectInterface.parents().forEach(parent -> collectObjectMethods(
                    module, parent.name(), name, target, visited));
        }
    }

    private List<FunctionSig> deduplicate(List<FunctionSig> values) {
        var keys = new LinkedHashSet<String>();
        return values.stream().filter(value -> keys.add(value.name + value.parameters + value.result)).toList();
    }

    private void collectFunctions(ParsedModule module, String name, List<FunctionSig> target) {
        if (module == null) return;
        module.definitions().stream().filter(FunctionDefinition.class::isInstance).map(FunctionDefinition.class::cast)
                .map(FunctionDefinition::function).filter(function -> function.name().equals(name))
                .map(this::signature).forEach(target::add);
    }

    private void collectFunctions(CompiledModule module, String name, List<FunctionSig> target) {
        if (module == null) return;
        module.functions().stream().filter(function -> function.name().equals(name)).map(function -> new FunctionSig(
                function.name(), function.parameters().stream().map(parameter -> type(parameter.typeReference())).toList(),
                type(function.returnType()), ownerType(function.name()))).forEach(target::add);
    }

    private void collectPublicFunctions(ParsedModule module, String name, List<FunctionSig> target) {
        if (module == null) return;
        module.definitions().stream().filter(FunctionDefinition.class::isInstance).map(FunctionDefinition.class::cast)
                .map(FunctionDefinition::function).filter(function -> function.name().equals(name))
                .filter(function -> !function.visibility().equals("private"))
                .map(this::signature).forEach(target::add);
    }

    private void collectPublicFunctions(CompiledModule module, String name, List<FunctionSig> target) {
        if (module == null) return;
        module.functions().stream().filter(function -> function.name().equals(name))
                .filter(function -> !function.visibility().equals("private"))
                .map(function -> new FunctionSig(function.name(),
                        function.parameters().stream().map(parameter -> type(parameter.typeReference())).toList(),
                        type(function.returnType()), ownerType(function.name())))
                .forEach(target::add);
    }

    private FunctionSig signature(FunctionDeclaration function) {
        return new FunctionSig(function.name(), function.parameters().stream().map(parameter -> type(parameter.typeReference())).toList(),
                type(function.returnType()), ownerType(function.name()));
    }

    private FunctionSig signature(String owner, ObjectOrientedMethod method) {
        return new FunctionSig(owner + "." + method.name(), method.parameters().stream().map(parameter -> type(parameter.typeReference())).toList(),
                type(method.returnType()), simple(owner));
    }

    private Type ownerType(String name) {
        return name.contains(".") ? parseType(name.substring(0, name.lastIndexOf('.'))) : UNKNOWN;
    }

    private Type visibleConstant(ParsedModule module, String name) {
        var local = constant(module, name);
        if (local != null) return local;
        for (var declaration : module.imports()) {
            if (!declaration.qualified() && exposes(declaration, name)) {
                var imported = constant(resolveParsed(declaration.modulePath()), name);
                if (imported != null) return imported;
            }
        }
        return null;
    }

    private Type constant(ParsedModule module, String name) {
        if (module == null) return null;
        return module.definitions().stream().filter(ConstantDefinition.class::isInstance).map(ConstantDefinition.class::cast)
                .map(ConstantDefinition::constant).filter(value -> value.name().equals(name)).findFirst()
                .map(value -> type(value.typeReference())).orElse(null);
    }

    private List<DataFieldDeclaration> dataFields(ParsedModule module, String name) {
        var declaration = dataDeclaration(module, name);
        if (declaration != null) return declaration.fields();
        for (var imported : module.imports()) {
            declaration = dataDeclaration(resolveParsed(imported.modulePath()), name);
            if (declaration != null) return declaration.fields();
        }
        return List.of();
    }

    private DataDeclaration dataDeclaration(ParsedModule module, String name) {
        if (module == null) return null;
        return module.definitions().stream().filter(DataDeclaration.class::isInstance).map(DataDeclaration.class::cast)
                .filter(value -> value.name().equals(unqualified(name))).findFirst().orElse(null);
    }

    private boolean subtype(ParsedModule module, String actual, String expected) {
        return subtype(module, actual, expected, new HashSet<>());
    }

    private boolean subtype(ParsedModule module, String actual, String expected, Set<String> visited) {
        var actualName = unqualified(actual);
        var expectedName = unqualified(expected);
        if (actualName.equals(expectedName)) return true;
        if (!visited.add(moduleFileName(module) + "\n" + actualName + "\n" + expectedName)) return false;
        for (var definition : module.definitions()) {
            if (definition instanceof TypeDeclaration union
                    && union.name().equals(expectedName)
                    && union.variants().stream().anyMatch(variant -> unqualified(variant.name()).equals(actualName))) {
                return true;
            }
            if (definition instanceof DataDeclaration data && data.name().equals(actualName)) {
                for (var parent : data.parents()) {
                    if (subtype(module, parent.typeReference().name(), expectedName, visited)) return true;
                }
            }
        }
        for (var objectClass : module.objectOriented().classes()) {
            if (!objectClass.name().equals(actualName)) continue;
            for (var parent : objectClass.parents()) {
                if (subtype(module, parent.name(), expectedName, visited)) return true;
            }
        }
        for (var objectInterface : module.objectOriented().interfaces()) {
            if (!objectInterface.name().equals(actualName)) continue;
            for (var parent : objectInterface.parents()) {
                if (subtype(module, parent.name(), expectedName, visited)) return true;
            }
        }
        for (var fragment : modules) {
            if (fragment != module && modulePath(fragment).equals(modulePath(module))
                    && subtype(fragment, actual, expected, visited)) {
                return true;
            }
        }
        for (var declaration : module.imports()) {
            var imported = resolveParsed(declaration.modulePath());
            if (imported != null && subtype(imported, actual, expected, visited)) return true;
        }
        return false;
    }

    private ObjectOrientedClass objectClass(ParsedModule module, String name) {
        var local = module.objectOriented().classes().stream().filter(value -> value.name().equals(unqualified(name))).findFirst().orElse(null);
        if (local != null) return local;
        for (var declaration : module.imports()) {
            var imported = resolveParsed(declaration.modulePath());
            if (imported == null) continue;
            local = imported.objectOriented().classes().stream().filter(value -> value.name().equals(unqualified(name))).findFirst().orElse(null);
            if (local != null) return local;
        }
        return null;
    }

    private ObjectOrientedInterface objectInterface(ParsedModule module, String name) {
        var nominal = unqualified(name);
        var local = module.objectOriented().interfaces().stream()
                .filter(value -> value.name().equals(nominal)).findFirst().orElse(null);
        if (local != null) return local;
        for (var declaration : module.imports()) {
            var imported = resolveParsed(declaration.modulePath());
            if (imported == null) continue;
            local = imported.objectOriented().interfaces().stream()
                    .filter(value -> value.name().equals(nominal)).findFirst().orElse(null);
            if (local != null) return local;
        }
        return null;
    }

    private boolean locallyDeclaresNominal(ParsedModule module, String name) {
        var nominal = unqualified(name);
        return dataDeclaration(module, nominal) != null
                || module.objectOriented().classes().stream().anyMatch(value -> value.name().equals(nominal))
                || module.objectOriented().interfaces().stream().anyMatch(value -> value.name().equals(nominal));
    }

    private boolean primitiveBacked(ParsedModule module, String name) {
        return primitiveBacked(module, unqualified(name), new HashSet<>());
    }

    private Type primitiveBackedEffectiveType(Type type) {
        if (type == null || numeric(type)) return type;
        var nominal = unqualified(type.name);
        for (var candidate : modules) {
            var backingType = candidate.definitions().stream()
                    .filter(Definition.PrimitiveBackedTypeDeclaration.class::isInstance)
                    .map(Definition.PrimitiveBackedTypeDeclaration.class::cast)
                    .filter(declaration -> declaration.name().equals(nominal))
                    .map(Definition.PrimitiveBackedTypeDeclaration::backingType)
                    .findFirst()
                    .orElse(null);
            if (backingType != null) return type(backingType);
        }
        for (var candidate : linkedModules) {
            var primitive = candidate.visiblePrimitiveBackedTypes().get(nominal);
            if (primitive != null) return type(primitive.backingType());
        }
        return type;
    }

    private boolean primitiveBacked(ParsedModule module, String nominal, Set<String> visited) {
        if (module == null || !visited.add(modulePath(module))) return false;
        if (module.definitions().stream()
                .filter(Definition.PrimitiveBackedTypeDeclaration.class::isInstance)
                .map(Definition.PrimitiveBackedTypeDeclaration.class::cast)
                .anyMatch(value -> value.name().equals(nominal))) return true;
        return module.imports().stream().map(value -> resolveParsed(value.modulePath()))
                .anyMatch(value -> primitiveBacked(value, nominal, visited));
    }

    private boolean enumValue(ParsedModule module, String name) {
        return module.definitions().stream().filter(EnumDeclaration.class::isInstance).map(EnumDeclaration.class::cast)
                .anyMatch(value -> value.values().stream().anyMatch(item -> item.name().equals(name)));
    }

    private boolean knownFunctionName(ParsedModule module, String name) {
        return !visibleFunctions(module, name).isEmpty();
    }

    private boolean exposes(ImportDeclaration declaration, String name) {
        return !declaration.excludedNames().contains(name)
                && (declaration.wildcard() || declaration.importedNames().contains(name));
    }

    private ParsedModule resolveParsed(String path) {
        var normalized = normalize(path);
        return modulesByPath.entrySet().stream().filter(entry -> entry.getKey().equals(normalized)
                || entry.getKey().endsWith("/" + normalized) || entry.getValue().name().equals(path)).map(Map.Entry::getValue).findFirst().orElse(null);
    }

    private CompiledModule resolveLinked(String path) {
        var normalized = normalize(path);
        return linkedByPath.entrySet().stream().filter(entry -> entry.getKey().equals(normalized)
                || entry.getKey().endsWith("/" + normalized) || entry.getValue().name().equals(path)).map(Map.Entry::getValue).findFirst().orElse(null);
    }

    private void validateTypeArity(ParsedModule module, String name, int arity, SourceLocation location) {
        if (arity < 0) report(module, location, "TYPE_ARITY", "Invalid generic arity for type `" + name + "`.");
    }

    private void report(ParsedModule module, SourceLocation location, String code, String message) {
        errors.add(new CompilerError(code, message, moduleFileName(module), location.line(), location.column()));
    }

    private static Type type(TypeReference reference) {
        if (reference == null || reference.name().isBlank()) return UNKNOWN;
        var name = reference.name().trim();
        var arrow = topLevelArrow(name);
        if (arrow >= 0) {
            var parameterText = name.substring(0, arrow).trim();
            var resultText = name.substring(arrow + 2).trim();
            if (parameterText.startsWith("(") && parameterText.endsWith(")")) parameterText = parameterText.substring(1, parameterText.length() - 1);
            var parameters = parameterText.isBlank() ? List.<Type>of() : splitTopLevel(parameterText).stream().map(StrictSemanticAnalyzer::parseType).toList();
            return function(parameters, parseType(resultText));
        }
        return new Type(name, reference.arguments().stream().map(StrictSemanticAnalyzer::type).toList(), List.of(), null);
    }

    private static Type type(CompiledTypeReference reference) {
        if (topLevelArrow(reference.name()) >= 0) return parseType(reference.name());
        return new Type(reference.name(), reference.arguments().stream().map(StrictSemanticAnalyzer::type).toList(), List.of(), null);
    }

    private static Type parseType(String value) {
        var arrow = topLevelArrow(value);
        if (arrow >= 0) {
            var parameterText = value.substring(0, arrow).trim();
            if (parameterText.startsWith("(") && parameterText.endsWith(")")) {
                parameterText = parameterText.substring(1, parameterText.length() - 1);
            }
            var parameters = parameterText.isBlank()
                    ? List.<Type>of()
                    : splitTopLevel(parameterText).stream().map(StrictSemanticAnalyzer::parseType).toList();
            return function(parameters, parseType(value.substring(arrow + 2).trim()));
        }
        var bracket = value.indexOf('[');
        if (bracket < 0 || !value.endsWith("]")) return simple(value.trim());
        return new Type(value.substring(0, bracket).trim(), splitTopLevel(value.substring(bracket + 1, value.length() - 1)).stream()
                .map(StrictSemanticAnalyzer::parseType).toList(), List.of(), null);
    }

    private static int topLevelArrow(String value) {
        var depth = 0;
        for (var index = 0; index < value.length() - 1; index++) {
            var c = value.charAt(index);
            if (c == '(' || c == '[') depth++;
            else if (c == ')' || c == ']') depth--;
            else if (c == '=' && value.charAt(index + 1) == '>' && depth == 0) return index;
        }
        return -1;
    }

    private static List<String> splitTopLevel(String value) {
        var parts = new ArrayList<String>();
        var depth = 0;
        var start = 0;
        for (var index = 0; index <= value.length(); index++) {
            if (index == value.length() || value.charAt(index) == ',' && depth == 0) {
                parts.add(value.substring(start, index).trim());
                start = index + 1;
            } else if (value.charAt(index) == '(' || value.charAt(index) == '[') depth++;
            else if (value.charAt(index) == ')' || value.charAt(index) == ']') depth--;
        }
        return parts;
    }

    private static Type decodedLambdaType(String value) {
        var prefix = "__capy_typed_lambda|";
        if (!value.startsWith(prefix)) return null;
        var separator = value.indexOf('|', prefix.length());
        return separator < 0 ? null : parseType(value.substring(separator + 1));
    }

    private static String decodedLambdaName(String value) {
        var prefix = "__capy_typed_lambda|";
        if (!value.startsWith(prefix)) return value;
        var separator = value.indexOf('|', prefix.length());
        return separator < 0 ? value : value.substring(prefix.length(), separator);
    }

    private static Type simple(String name) { return new Type(name, List.of(), List.of(), null); }
    private static Type function(List<Type> parameters, Type result) { return new Type("function", List.of(), List.copyOf(parameters), result); }
    private static boolean directlyCheckable(Expression expression) {
        return expression instanceof BoolLiteral || expression instanceof IntLiteral || expression instanceof LongLiteral
                || expression instanceof FloatLiteral || expression instanceof DoubleLiteral || expression instanceof StringLiteral;
    }
    private static boolean scalarPrimitive(Type type) {
        return type != null && (numeric(type) || Set.of("String", "bool", "char").contains(unqualified(type.name)));
    }
    private static boolean generic(Type type) { return type.functionResult == null && type.arguments.isEmpty() && type.name.length() == 1 && Character.isUpperCase(type.name.charAt(0)); }
    private static boolean unknown(Type type) { return type == null || type == UNKNOWN || type.name.equals("<unknown>"); }
    private static boolean dynamic(Type type) { return unknown(type) || type == ANY || type.name.equals("any"); }
    private static boolean numeric(Type type) { return type != null && NUMERIC.contains(unqualified(type.name)); }
    private static boolean comparable(Type left, Type right) { return left == ERROR || right == ERROR || dynamic(left) || dynamic(right) || assignableStatic(left, right) || assignableStatic(right, left); }
    private static boolean assignableStatic(Type actual, Type expected) {
        if (actual == null || expected == null || dynamic(actual) || dynamic(expected)) return true;
        if (numeric(actual) && numeric(expected)) return true;
        return unqualified(actual.name).equals(unqualified(expected.name));
    }
    private static Type wider(Type left, Type right) { return numericRank(left.name) >= numericRank(right.name) ? normalizeNumeric(left) : normalizeNumeric(right); }
    private static Type normalizeNumeric(Type type) { return Set.of("size", "index").contains(type.name) ? INT : type; }
    private static int numericRank(String name) { return switch (unqualified(name)) { case "byte" -> 0; case "int", "size", "index" -> 1; case "long" -> 2; case "float" -> 3; case "double" -> 4; default -> 99; }; }
    private static String stripRaw(String name) { return name.startsWith("__capy_raw|") ? name.substring("__capy_raw|".length()) : name; }
    private static String unqualified(String name) { var slash = Math.max(name.lastIndexOf('/'), name.lastIndexOf('.')); return slash < 0 ? name : name.substring(slash + 1); }
    private static String normalize(String path) { var value = path.replace('\\', '/'); while (value.startsWith("/")) value = value.substring(1); return value; }
    private static String modulePath(ParsedModule module) { var path = normalize(module.path()); return path.isBlank() ? module.name() : path + "/" + module.name(); }
    private static String modulePath(CompiledModule module) { var path = normalize(module.path()); return path.isBlank() ? module.name() : path + "/" + module.name(); }
    private static String moduleFileName(ParsedModule module) {
        var path = normalize(module.path());
        var file = module.name() + (module.sourceKind() == SourceKind.OBJECT_ORIENTED ? ".coo" : ".cfun");
        return path.isBlank() ? file : "/" + path + "/" + file;
    }
    private static String displayName(String name) { var marker = name.indexOf("__local__"); return marker < 0 ? name : name.substring(marker + "__local__".length()).split("__", 2)[0]; }

    private static SourceLocation location(Expression expression) {
        return switch (expression) {
            case BinaryExpression value -> value.location(); case BlockExpression value -> value.location();
            case BoolLiteral value -> value.location(); case DataLiteral value -> value.location(); case DictLiteral value -> value.location();
            case DoubleLiteral value -> value.location(); case FieldAccessExpression value -> value.location(); case FloatLiteral value -> value.location();
            case FunctionCallExpression value -> value.location(); case FunctionReferenceExpression value -> value.location(); case IfExpression value -> value.location();
            case IndexExpression value -> value.location(); case IntLiteral value -> value.location(); case LambdaExpression value -> value.location();
            case ListLiteral value -> value.location(); case LongLiteral value -> value.location(); case MatchExpression value -> value.location();
            case MethodCallExpression value -> value.location(); case ReduceExpression value -> value.location(); case SetLiteral value -> value.location();
            case StringLiteral value -> value.location(); case ThrowExpression value -> value.location(); case TryCatchExpression value -> value.location();
            case TupleLiteral value -> value.location(); case UnaryExpression value -> value.location(); case VariableExpression value -> value.location();
            case WithExpression value -> value.location(); case UnsupportedExpression value -> value.location(); default -> new SourceLocation(0, 0);
        };
    }

    private record FunctionSig(String name, List<Type> parameters, Type result, Type owner) { }
    private record Type(String name, List<Type> arguments, List<Type> parameters, Type functionResult) {
        @Override public String toString() {
            if (functionResult != null) return (parameters.size() == 1 ? parameters.getFirst().toString() : "(" + join(parameters) + ")") + " => " + functionResult;
            return arguments.isEmpty() ? name : name + "[" + join(arguments) + "]";
        }
        private static String join(List<Type> values) { return values.stream().map(Type::toString).reduce((a, b) -> a + ", " + b).orElse(""); }
    }

    private static final class Env {
        private final Map<String, Type> values = new LinkedHashMap<>();
        private Env copy() { var copy = new Env(); copy.values.putAll(values); return copy; }
    }
}
