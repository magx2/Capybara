package dev.capylang.generator;

import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunction;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.CompiledTupleType;
import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.GenericDataType;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.expression.*;
import dev.capylang.compiler.parser.InfixOperator;
import dev.capylang.generator.java.JavaAstBuilder;
import dev.capylang.generator.java.JavaClass;
import dev.capylang.generator.java.JavaConst;
import dev.capylang.generator.java.JavaEnum;
import dev.capylang.generator.java.JavaMethod;
import dev.capylang.generator.java.JavaRecord;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

public final class JavaScriptGenerator implements Generator {
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final Path RUNTIME_PATH = Path.of("dev", "capylang", "capybara.js");
    private static final java.util.regex.Pattern CONST_NAME_PATTERN = java.util.regex.Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");
    private static final Set<String> JS_KEYWORDS = Set.of(
            "await", "break", "case", "catch", "class", "const", "continue", "debugger", "default",
            "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for",
            "function", "if", "implements", "import", "in", "instanceof", "interface", "let",
            "new", "null", "package", "private", "protected", "public", "return", "super",
            "switch", "static", "this", "throw", "try", "true", "typeof", "var", "void",
            "while", "with", "yield"
    );

    @Override
    public GeneratedProgram generate(CompiledProgram program) {
        if (!program.objectOrientedModules().isEmpty()) {
            throw new IllegalStateException("Object-oriented `.coo` generation is only supported for JAVA");
        }
        if (program.modules().isEmpty()) {
            return new GeneratedProgram(List.of());
        }

        var functionNameOverrides = buildFunctionNameOverrides(program);
        var astBuilder = new JavaAstBuilder(functionNameOverrides);
        var modules = new ArrayList<GeneratedModule>();
        var moduleInfos = program.modules().stream()
                .map(module -> ModuleInfo.from(module, astBuilder.build(module)))
                .toList();
        var context = ProgramContext.build(moduleInfos, functionNameOverrides);

        for (var moduleInfo : moduleInfos) {
            modules.add(new GeneratedModule(moduleInfo.relativePath(), new ModuleRenderer(context, moduleInfo).render()));
        }
        modules.addAll(RuntimeModules.modules());
        return new GeneratedProgram(List.copyOf(modules));
    }

    private static final class ModuleRenderer {
        private final ProgramContext programContext;
        private final ModuleInfo moduleInfo;
        private final JavaClass javaClass;
        private final ExpressionRenderer expressions;
        private final LinkedHashMap<String, Path> requiredModules = new LinkedHashMap<>();
        private final LinkedHashSet<String> exportNames = new LinkedHashSet<>();

        private ModuleRenderer(ProgramContext programContext, ModuleInfo moduleInfo) {
            this.programContext = programContext;
            this.moduleInfo = moduleInfo;
            this.javaClass = moduleInfo.javaClass();
            this.expressions = new ExpressionRenderer(programContext, moduleInfo, requiredModules);
            for (var staticImport : javaClass.staticImports()) {
                var className = staticImportClassName(staticImport);
                if (!className.equals(moduleInfo.className())) {
                    requireClassName(className);
                }
            }
        }

        private String render() {
            var body = new StringBuilder();
            body.append("'use strict';\n\n");
            body.append("const capy = require(")
                    .append(jsString(relativeRequire(moduleInfo.relativePath(), RUNTIME_PATH)))
                    .append(");\n");
            for (var entry : requiredModules.entrySet()) {
                body.append("const ")
                        .append(moduleVar(entry.getKey()))
                        .append(" = require(")
                        .append(jsString(relativeRequire(moduleInfo.relativePath(), entry.getValue())))
                        .append(");\n");
            }
            if (!requiredModules.isEmpty()) {
                body.append('\n');
            }

            for (var record : javaClass.records()) {
                body.append(renderRecord(record)).append('\n');
            }
            for (var javaEnum : javaClass.enums()) {
                body.append(renderEnum(javaEnum)).append('\n');
            }
            for (var javaConst : javaClass.staticConsts()) {
                body.append(renderConst(javaConst)).append('\n');
            }
            for (var method : javaClass.staticMethods()) {
                body.append(renderFunction(method, true)).append('\n');
            }
            body.append(renderExports());
            body.append(renderProgramMain());
            return body.toString();
        }

        private void requireClassName(String className) {
            programContext.pathForClassName(className)
                    .ifPresent(path -> requiredModules.putIfAbsent(className, path));
        }

        private String renderRecord(JavaRecord record) {
            var code = new StringBuilder();
            var name = record.name().toString();
            code.append("class ").append(name).append(" {\n");
            code.append("    constructor(fields = {}) {\n");
            code.append("        this.__capybaraType = ").append(jsString(name)).append(";\n");
            var parentTypes = programContext.parentTypes(name);
            code.append("        this.__capybaraTypes = ")
                    .append(jsArray(Stream.concat(Stream.of(name), parentTypes.stream()).distinct().toList()))
                    .append(";\n");
            for (var field : record.fields()) {
                code.append("        this.").append(field.name()).append(" = fields.").append(field.name()).append(";\n");
            }
            code.append("    }\n");
            code.append("    with(fields = {}) {\n");
            code.append("        return new ").append(name).append("({\n");
            for (var field : record.fields()) {
                code.append("            ").append(field.name()).append(": Object.prototype.hasOwnProperty.call(fields, ")
                        .append(jsString(field.name()))
                        .append(") ? fields.").append(field.name()).append(" : this.").append(field.name()).append(",\n");
            }
            code.append("        });\n");
            code.append("    }\n");
            code.append("    toString() {\n");
            code.append("        return capy.dataToString(this);\n");
            code.append("    }\n");
            code.append("    capybaraDataValueInfo() {\n");
            code.append("        return capy.dataValueInfo(this, ")
                    .append(jsString(name))
                    .append(", ")
                    .append(jsString(moduleInfo.packageName()))
                    .append(", ")
                    .append(jsString(moduleInfo.packagePath()))
                    .append(");\n");
            code.append("    }\n");
            for (var method : record.methods()) {
                code.append(renderFunction(method, false));
            }
            code.append("}\n");
            if (!record.isPrivate()) {
                exportNames.add(name);
            }
            return code.toString();
        }

        private String renderEnum(JavaEnum javaEnum) {
            var enumName = javaEnum.name().toString();
            var values = javaEnum.values().isEmpty() ? List.of("INSTANCE") : javaEnum.values();
            var code = new StringBuilder();
            if (values.size() == 1 && "INSTANCE".equals(values.getFirst())) {
                code.append("const ").append(enumName)
                        .append(" = capy.enumValue(")
                        .append(jsString(enumName)).append(", ")
                        .append(jsString(enumName)).append(", ")
                        .append(jsArray(programContext.parentTypes(enumName)))
                        .append(");\n");
                exportNames.add(enumName);
                return code.toString();
            }
            code.append("const ").append(enumName).append(" = Object.freeze({\n");
            for (var value : values) {
                code.append("    ").append(value).append(": capy.enumValue(")
                        .append(jsString(value)).append(", ")
                        .append(jsString(enumName)).append(", ")
                        .append(jsArray(List.of(enumName))).append("),\n");
            }
            code.append("});\n");
            exportNames.add(enumName);
            for (var value : values) {
                code.append("const ").append(value).append(" = ").append(enumName).append(".").append(value).append(";\n");
                exportNames.add(value);
            }
            return code.toString();
        }

        private String renderConst(JavaConst javaConst) {
            var expression = expressions.render(javaConst.expression(), Scope.root());
            if (!javaConst.isPrivate()) {
                exportNames.add(javaConst.name());
            }
            return "const " + javaConst.name() + " = " + expression + ";\n";
        }

        private String renderFunction(JavaMethod method, boolean topLevel) {
            var name = method.name();
            var params = method.parameters().stream()
                    .map(JavaMethod.JavaFunctionParameter::generatedName)
                    .map(JavaScriptGenerator::normalizeJsIdentifier)
                    .toList();
            var scope = Scope.root();
            for (var parameter : method.parameters()) {
                scope = scope.bind(parameter.sourceName(), normalizeJsIdentifier(parameter.generatedName()));
            }
            var body = expressions.render(method.expression(), scope);
            var code = new StringBuilder();
            if (topLevel) {
                code.append("function ").append(name).append("(").append(String.join(", ", params)).append(") {\n");
            } else {
                code.append("    ").append(name).append("(").append(String.join(", ", params)).append(") {\n");
            }
            code.append(topLevel ? "    " : "        ")
                    .append("return ")
                    .append(body)
                    .append(";\n");
            code.append(topLevel ? "}\n" : "    }\n");
            if (topLevel && !method.isPrivate()) {
                exportNames.add(name);
            }
            return code.toString();
        }

        private String renderExports() {
            if (exportNames.isEmpty()) {
                return "module.exports = {};\n";
            }
            return "module.exports = {\n"
                   + exportNames.stream()
                           .map(name -> "    " + name + ",")
                           .collect(joining("\n"))
                   + "\n};\n";
        }

        private String renderProgramMain() {
            var main = javaClass.staticMethods().stream()
                    .filter(JavaMethod::programMain)
                    .findFirst();
            if (main.isEmpty()) {
                return "";
            }
            var mainName = main.orElseThrow().name();
            return "\nif (require.main === module) {\n"
                   + "    const result = " + mainName + "(process.argv.slice(2));\n"
                   + "    const value = capy.isEffect(result) ? result.unsafe_run() : result;\n"
                   + "    if (value !== undefined && value !== null) {\n"
                   + "        console.log(capy.toStringValue(value));\n"
                   + "    }\n"
                   + "}\n";
        }

        private static String staticImportClassName(String staticImport) {
            var idx = staticImport.lastIndexOf('.');
            return idx < 0 ? staticImport : staticImport.substring(0, idx);
        }
    }

    private static final class ExpressionRenderer {
        private static final AtomicLong TEMP_COUNTER = new AtomicLong();
        private final ProgramContext programContext;
        private final ModuleInfo moduleInfo;
        private final Map<String, Path> requiredModules;

        private ExpressionRenderer(ProgramContext programContext, ModuleInfo moduleInfo, Map<String, Path> requiredModules) {
            this.programContext = programContext;
            this.moduleInfo = moduleInfo;
            this.requiredModules = requiredModules;
        }

        private String render(CompiledExpression expression, Scope scope) {
            return switch (expression) {
                case CompiledBooleanValue booleanValue -> booleanValue.toString();
                case CompiledByteValue byteValue -> stripNumericSuffix(byteValue.byteValue());
                case CompiledDoubleValue doubleValue -> stripNumericSuffix(doubleValue.doubleValue());
                case CompiledFloatValue floatValue -> stripNumericSuffix(floatValue.floatValue());
                case CompiledIntValue intValue -> stripNumericSuffix(intValue.intValue());
                case CompiledLongValue longValue -> stripNumericSuffix(longValue.longValue());
                case CompiledStringValue stringValue -> stringValue.toString();
                case CompiledVariable variable -> scope.resolve(variable.name());
                case CompiledNumericWidening numericWidening -> render(numericWidening.expression(), scope);
                case CompiledNewList newList -> renderNewList(newList, scope);
                case CompiledNewSet newSet -> renderNewSet(newSet, scope);
                case CompiledNewDict newDict -> renderNewDict(newDict, scope);
                case CompiledTupleExpression tupleExpression -> renderTuple(tupleExpression, scope);
                case CompiledFieldAccess fieldAccess -> "(" + render(fieldAccess.source(), scope) + ")." + fieldAccess.field();
                case CompiledFunctionCall functionCall -> renderFunctionCall(functionCall, scope);
                case CompiledFunctionInvoke functionInvoke -> renderFunctionInvoke(functionInvoke, scope);
                case CompiledIfExpression ifExpression -> "((" + renderBoolean(ifExpression.condition(), scope) + ") ? ("
                                                          + render(ifExpression.thenBranch(), scope) + ") : ("
                                                          + render(ifExpression.elseBranch(), scope) + "))";
                case CompiledInfixExpression infixExpression -> renderInfix(infixExpression, scope);
                case CompiledLetExpression letExpression -> renderLet(letExpression, scope);
                case CompiledLambdaExpression lambdaExpression -> renderLambda(lambdaExpression, scope);
                case CompiledIndexExpression indexExpression -> renderIndex(indexExpression, scope);
                case CompiledSliceExpression sliceExpression -> renderSlice(sliceExpression, scope);
                case CompiledNewData newData -> renderNewData(newData, scope);
                case CompiledMatchExpression matchExpression -> renderMatch(matchExpression, scope);
                case CompiledPipeExpression pipeExpression -> renderPipe(pipeExpression, scope);
                case CompiledPipeFilterOutExpression pipeFilterOutExpression -> renderPipeFilterOut(pipeFilterOutExpression, scope);
                case CompiledPipeFlatMapExpression pipeFlatMapExpression -> renderPipeFlatMap(pipeFlatMapExpression, scope);
                case CompiledPipeReduceExpression pipeReduceExpression -> renderPipeReduce(pipeReduceExpression, scope);
                case CompiledEffectExpression effectExpression -> "capy.delay(() => (" + render(effectExpression.body(), scope) + "))";
                case CompiledEffectBindExpression bindExpression -> renderEffectBind(bindExpression, scope);
                case CompiledReflectionValue reflectionValue -> renderReflection(reflectionValue, scope);
                case CompiledNothingValue nothingValue -> "capy.unsupported(" + jsString(nothingValue.message()) + ")";
            };
        }

        private String renderNewList(CompiledNewList newList, Scope scope) {
            return newList.values().stream()
                    .map(value -> render(value, scope))
                    .collect(joining(", ", "[", "]"));
        }

        private String renderNewSet(CompiledNewSet newSet, Scope scope) {
            return newSet.values().stream()
                    .map(value -> render(value, scope))
                    .collect(joining(", ", "new Set([", "])"));
        }

        private String renderNewDict(CompiledNewDict newDict, Scope scope) {
            return newDict.entries().stream()
                    .map(entry -> "[" + render(entry.key(), scope) + ", " + render(entry.value(), scope) + "]")
                    .collect(joining(", ", "new Map([", "])"));
        }

        private String renderTuple(CompiledTupleExpression tupleExpression, Scope scope) {
            return tupleExpression.values().stream()
                    .map(value -> render(value, scope))
                    .collect(joining(", ", "[", "]"));
        }

        private String renderFunctionCall(CompiledFunctionCall functionCall, Scope scope) {
            var args = functionCall.arguments().stream().map(argument -> render(argument, scope)).toList();
            if (functionCall.name().startsWith(METHOD_DECL_PREFIX)) {
                return renderMethodCall(functionCall, args, scope);
            }
            if ("sqrt".equals(functionCall.name()) && args.size() == 1) {
                return "Math.sqrt(" + args.getFirst() + ")";
            }
            var target = resolveFunctionTarget(functionCall);
            if (isConstCall(functionCall)) {
                return target;
            }
            return target + "(" + String.join(", ", args) + ")";
        }

        private String renderMethodCall(CompiledFunctionCall functionCall, List<String> args, Scope scope) {
            if (args.isEmpty()) {
                throw new IllegalStateException("Method call requires receiver argument: " + functionCall.name());
            }
            var methodName = simpleMethodName(functionCall.name());
            var receiver = args.getFirst();
            var receiverType = functionCall.arguments().getFirst().type();
            var tailArgs = args.subList(1, args.size());

            if ("unsafe_run".equals(methodName)) {
                return "(" + receiver + ").unsafe_run()";
            }
            if ("length".equals(methodName) && receiverType == PrimitiveLinkedType.STRING) {
                return "(" + receiver + ").length";
            }
            if ("size".equals(methodName)) {
                if (receiverType instanceof CollectionLinkedType.CompiledDict
                    || receiverType instanceof CollectionLinkedType.CompiledSet) {
                    return "(" + receiver + ").size";
                }
                return "(" + receiver + ").length";
            }
            if ("to_list".equals(methodName) && receiverType instanceof CollectionLinkedType.CompiledSet) {
                return "Array.from(" + receiver + ")";
            }
            if ("entries".equals(methodName) && receiverType instanceof CollectionLinkedType.CompiledDict) {
                return "Array.from((" + receiver + ").entries())";
            }
            if ("replace".equals(methodName) && tailArgs.size() == 2) {
                return "String(" + receiver + ").split(" + tailArgs.get(0) + ").join(" + tailArgs.get(1) + ")";
            }
            if (("get".equals(methodName) || "_contains_native".equals(methodName) || "contains_native".equals(methodName))
                && renderNativeCollectionMethod(functionCall, args, methodName).isPresent()) {
                return renderNativeCollectionMethod(functionCall, args, methodName).orElseThrow();
            }
            if (List.of("+", "plus").contains(methodName) && tailArgs.size() == 1) {
                return renderCollectionPlus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst());
            }
            if (List.of("-", "minus").contains(methodName) && tailArgs.size() == 1) {
                return renderCollectionMinus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst());
            }
            if ("contains".equals(methodName) || "?".equals(methodName)) {
                return renderContains(receiverType, receiver, tailArgs.getFirst());
            }
            if ("is_empty".equals(methodName)) {
                return renderSize(receiverType, receiver) + " === 0";
            }
            if ("any".equals(methodName) && tailArgs.size() == 1) {
                return "capy.any(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("all".equals(methodName) && tailArgs.size() == 1) {
                return "capy.all(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("map".equals(methodName) || "|".equals(methodName)) {
                return "capy.mapCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("filter".equals(methodName) || "|-".equals(methodName)) {
                return "capy.filterCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("reject".equals(methodName)) {
                return "capy.rejectCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("flat_map".equals(methodName) || "flatMap".equals(methodName) || "|*".equals(methodName)) {
                return "capy.flatMapCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("reduce".equals(methodName) || "|>".equals(methodName)) {
                return "capy.reduceCollection(" + receiver + ", " + tailArgs.get(0) + ", " + tailArgs.get(1) + ")";
            }
            if (methodName.startsWith("to_")) {
                return renderConversion(methodName, receiver, functionCall.type());
            }
            var emittedName = emittedMethodName(functionCall);
            return "(" + receiver + ")." + emittedName + "(" + String.join(", ", tailArgs) + ")";
        }

        private Optional<String> renderNativeCollectionMethod(CompiledFunctionCall functionCall, List<String> args, String methodName) {
            var receiverType = functionCall.arguments().getFirst().type();
            var receiver = args.get(0);
            if ("get".equals(methodName)) {
                if (args.size() == 2) {
                    if (receiverType instanceof CompiledTupleType) {
                        return Optional.of("capy.rawIndex(" + receiver + ", " + args.get(1) + ")");
                    }
                    return Optional.of("capy.getIndex(" + receiver + ", " + args.get(1) + ")");
                }
                if (args.size() == 3) {
                    return Optional.of("capy.slice(" + receiver + ", " + args.get(1) + ", " + args.get(2) + ")");
                }
            }
            if (("_contains_native".equals(methodName) || "contains_native".equals(methodName))
                && receiverType instanceof CollectionLinkedType.CompiledSet
                && args.size() == 2) {
                return Optional.of("(" + receiver + ").has(" + args.get(1) + ")");
            }
            return Optional.empty();
        }

        private String renderFunctionInvoke(CompiledFunctionInvoke functionInvoke, Scope scope) {
            var call = new StringBuilder("(").append(render(functionInvoke.function(), scope)).append(")");
            if (functionInvoke.arguments().isEmpty()) {
                return call.append("()").toString();
            }
            for (var argument : functionInvoke.arguments()) {
                call.append("(").append(render(argument, scope)).append(")");
            }
            return call.toString();
        }

        private String renderInfix(CompiledInfixExpression expression, Scope scope) {
            var left = render(expression.left(), scope);
            var right = render(expression.right(), scope);
            return switch (expression.operator()) {
                case PLUS -> "(" + renderCollectionPlus(expression.left().type(), left, expression.right().type(), right) + ")";
                case MINUS -> "(" + renderCollectionMinus(expression.left().type(), left, expression.right().type(), right) + ")";
                case MUL -> "((" + left + ") * (" + right + "))";
                case DIV -> expression.type() == PrimitiveLinkedType.INT
                        ? "Math.trunc((" + left + ") / (" + right + "))"
                        : "((" + left + ") / (" + right + "))";
                case MOD -> "((" + left + ") % (" + right + "))";
                case POWER -> "Math.pow(" + left + ", " + right + ")";
                case GT, LT, LE, GE -> "((" + left + ") " + expression.operator().symbol() + " (" + right + "))";
                case EQUAL -> "capy.equals(" + left + ", " + right + ")";
                case NOTEQUAL -> "(!capy.equals(" + left + ", " + right + "))";
                case AND -> "((" + renderBoolean(expression.left(), scope) + ") && (" + renderBoolean(expression.right(), scope) + "))";
                case PIPE -> "((" + renderBoolean(expression.left(), scope) + ") || (" + renderBoolean(expression.right(), scope) + "))";
                case QUESTION -> renderContains(expression.left().type(), left, right);
                case BITWISE_AND -> "((" + left + ") & (" + right + "))";
                case BITWISE_NAND -> "(~((" + left + ") & (" + right + ")))";
                case BITWISE_OR -> "((" + left + ") | (" + right + "))";
                case BITWISE_XOR -> "((" + left + ") ^ (" + right + "))";
                case BITWISE_NOT -> "(~(" + left + "))";
                default -> throw new UnsupportedOperationException("Unsupported JS infix operator: " + expression.operator());
            };
        }

        private String renderLet(CompiledLetExpression letExpression, Scope scope) {
            var jsName = scope.reserve(letExpression.name());
            var child = scope.bind(letExpression.name(), jsName);
            return "(() => { const " + jsName + " = " + render(letExpression.value(), scope)
                   + "; return " + render(letExpression.rest(), child) + "; })()";
        }

        private String renderLambda(CompiledLambdaExpression lambdaExpression, Scope scope) {
            if (lambdaExpression.functionType().argumentType() == PrimitiveLinkedType.NOTHING) {
                return "(() => (" + render(lambdaExpression.expression(), scope) + "))";
            }
            var jsName = scope.reserve(lambdaExpression.argumentName());
            var child = scope.bind(lambdaExpression.argumentName(), jsName);
            return "((" + jsName + ") => (" + render(lambdaExpression.expression(), child) + "))";
        }

        private String renderIndex(CompiledIndexExpression indexExpression, Scope scope) {
            var source = render(indexExpression.source(), scope);
            var index = render(indexExpression.index(), scope);
            if (indexExpression.source().type() instanceof CompiledTupleType) {
                return "capy.rawIndex(" + source + ", " + index + ")";
            }
            return "capy.getIndex(" + source + ", " + index + ")";
        }

        private String renderSlice(CompiledSliceExpression sliceExpression, Scope scope) {
            var source = render(sliceExpression.source(), scope);
            var start = sliceExpression.start().map(expression -> render(expression, scope)).orElse("undefined");
            var end = sliceExpression.end().map(expression -> render(expression, scope)).orElse("undefined");
            return "capy.slice(" + source + ", " + start + ", " + end + ")";
        }

        private String renderNewData(CompiledNewData newData, Scope scope) {
            if (!(newData.type() instanceof CompiledDataType dataType)) {
                throw new UnsupportedOperationException("Cannot instantiate non-data type: " + newData.type());
            }
            var constructor = dataConstructorReference(dataType);
            if (dataType.singleton()) {
                return constructor;
            }
            var fields = newData.assignments().stream()
                    .map(assignment -> assignment.name() + ": " + render(assignment.value(), scope))
                    .collect(joining(", ", "{ ", " }"));
            return "new " + constructor + "(" + fields + ")";
        }

        private String renderMatch(CompiledMatchExpression matchExpression, Scope scope) {
            var matchName = "__match" + TEMP_COUNTER.incrementAndGet();
            var code = new StringBuilder("(() => { const ")
                    .append(matchName)
                    .append(" = ")
                    .append(render(matchExpression.matchWith(), scope))
                    .append("; ");
            for (var matchCase : matchExpression.cases()) {
                var branchScope = scope.bind(matchName, matchName);
                var pattern = renderPattern(matchName, matchCase.pattern(), branchScope);
                code.append("if (").append(pattern.condition()).append(") { ");
                for (var binding : pattern.bindings()) {
                    code.append(binding).append("; ");
                }
                if (matchCase.guard().isPresent()) {
                    code.append("if (!(").append(renderBoolean(matchCase.guard().orElseThrow(), pattern.scope())).append(")) { } else ");
                }
                code.append("{ return ")
                        .append(render(matchCase.expression(), pattern.scope()))
                        .append("; } ");
                code.append("} ");
            }
            code.append("throw new Error('Non-exhaustive match expression'); })()");
            return code.toString();
        }

        private RenderedPattern renderPattern(String value, CompiledMatchExpression.Pattern pattern, Scope scope) {
            return switch (pattern) {
                case CompiledMatchExpression.IntPattern intPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + stripNumericSuffix(intPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.LongPattern longPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + stripNumericSuffix(longPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.FloatPattern floatPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + stripNumericSuffix(floatPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.StringPattern stringPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + jsString(stringPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.BoolPattern boolPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + boolPattern.value() + ")", List.of(), scope);
                case CompiledMatchExpression.WildcardPattern ignored ->
                        new RenderedPattern("true", List.of(), scope);
                case CompiledMatchExpression.VariablePattern variablePattern -> {
                    if (isTypeLikeIdentifier(variablePattern.name())) {
                        yield new RenderedPattern(
                                "capy.isType(" + value + ", " + jsString(simpleTypeName(variablePattern.name())) + ")",
                                List.of(),
                                scope
                        );
                    }
                    yield bindPatternValue(value, variablePattern.name(), scope);
                }
                case CompiledMatchExpression.WildcardBindingPattern wildcardBindingPattern -> bindPatternValue(value, wildcardBindingPattern.name(), scope);
                case CompiledMatchExpression.TypedPattern typedPattern -> {
                    var bound = bindPatternValue(value, typedPattern.name(), scope);
                    yield new RenderedPattern(
                            "capy.isType(" + value + ", " + jsString(simpleTypeName(typedPattern.type().name())) + ")",
                            bound.bindings(),
                            bound.scope()
                    );
                }
                case CompiledMatchExpression.ConstructorPattern constructorPattern ->
                        renderConstructorPattern(value, constructorPattern, scope);
            };
        }

        private RenderedPattern renderConstructorPattern(String value, CompiledMatchExpression.ConstructorPattern pattern, Scope scope) {
            var constructorName = simpleTypeName(pattern.constructorName());
            var fields = programContext.fieldsForType(constructorName);
            var condition = new StringBuilder("capy.isType(")
                    .append(value)
                    .append(", ")
                    .append(jsString(constructorName))
                    .append(")");
            var bindings = new ArrayList<String>();
            var current = scope;
            for (int i = 0; i < pattern.fieldPatterns().size(); i++) {
                var fieldName = i < fields.size() ? fields.get(i) : "value";
                var fieldValue = value + "." + fieldName;
                var rendered = renderPattern(fieldValue, pattern.fieldPatterns().get(i), current);
                condition.append(" && (").append(rendered.condition()).append(")");
                bindings.addAll(rendered.bindings());
                current = rendered.scope();
            }
            return new RenderedPattern(condition.toString(), bindings, current);
        }

        private RenderedPattern bindPatternValue(String value, String sourceName, Scope scope) {
            var jsName = scope.reserve(sourceName);
            var child = scope.bind(sourceName, jsName);
            return new RenderedPattern("true", List.of("const " + jsName + " = " + value), child);
        }

        private String renderPipe(CompiledPipeExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var mapper = lambdaForPipe(pipeExpression.argumentName(), pipeExpression.mapper(), scope);
            if (isOptionType(pipeExpression.source().type())) {
                return "capy.optionMap(" + source + ", " + mapper + ")";
            }
            return "capy.mapCollection(" + source + ", " + mapper + ")";
        }

        private String renderPipeFilterOut(CompiledPipeFilterOutExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var predicate = lambdaForPipe(pipeExpression.argumentName(), pipeExpression.predicate(), scope);
            if (isOptionType(pipeExpression.source().type())) {
                return "capy.optionFilterOut(" + source + ", " + predicate + ")";
            }
            return "capy.rejectCollection(" + source + ", " + predicate + ")";
        }

        private String renderPipeFlatMap(CompiledPipeFlatMapExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var mapper = lambdaForPipe(pipeExpression.argumentName(), pipeExpression.mapper(), scope);
            if (isOptionType(pipeExpression.source().type())) {
                return "capy.optionFlatMap(" + source + ", " + mapper + ")";
            }
            return "capy.flatMapCollection(" + source + ", " + mapper + ")";
        }

        private String renderPipeReduce(CompiledPipeReduceExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var initial = render(pipeExpression.initialValue(), scope);
            var accumulator = normalizeJsIdentifier(pipeExpression.accumulatorName());
            var value = normalizeJsIdentifier(pipeExpression.valueName());
            var child = scope.bind(pipeExpression.accumulatorName(), accumulator).bind(pipeExpression.valueName(), value);
            var reducer = "((" + accumulator + ", " + value + ") => (" + render(pipeExpression.reducerExpression(), child) + "))";
            return "capy.reduceCollection(" + source + ", " + initial + ", " + reducer + ")";
        }

        private String lambdaForPipe(String argumentName, CompiledExpression body, Scope scope) {
            var args = argumentName.split("::|;;");
            if (args.length == 2) {
                var first = normalizeJsIdentifier(args[0]);
                var second = normalizeJsIdentifier(args[1]);
                var child = scope.bind(args[0], first).bind(args[1], second);
                return "((" + first + ", " + second + ") => (" + render(body, child) + "))";
            }
            var jsName = normalizeJsIdentifier(argumentName);
            var child = scope.bind(argumentName, jsName);
            return "((" + jsName + ") => (" + render(body, child) + "))";
        }

        private String renderEffectBind(CompiledEffectBindExpression bind, Scope scope) {
            var jsName = scope.reserve(bind.name());
            var child = scope.bind(bind.name(), jsName);
            return "capy.delay(() => { const " + jsName + " = (" + render(bind.source(), scope)
                   + ").unsafe_run(); return (" + render(bind.rest(), child) + ").unsafe_run(); })";
        }

        private String renderReflection(CompiledReflectionValue reflectionValue, Scope scope) {
            var target = render(reflectionValue.target(), scope);
            return "capy.reflection(" + target + ", "
                   + jsString(reflectionValue.name()) + ", "
                   + jsString(reflectionValue.packageName()) + ", "
                   + jsString(reflectionValue.packagePath()) + ", "
                   + jsArray(reflectionValue.fields().stream().map(CompiledReflectionValue.Field::name).toList())
                   + ")";
        }

        private String renderBoolean(CompiledExpression expression, Scope scope) {
            if (expression.type() == PrimitiveLinkedType.BOOL) {
                return render(expression, scope);
            }
            return "capy.truthy(" + render(expression, scope) + ")";
        }

        private String renderSize(CompiledType type, String receiver) {
            if (type instanceof CollectionLinkedType.CompiledDict || type instanceof CollectionLinkedType.CompiledSet) {
                return "(" + receiver + ").size";
            }
            return "(" + receiver + ").length";
        }

        private String renderContains(CompiledType leftType, String left, String right) {
            if (leftType instanceof CollectionLinkedType.CompiledDict) {
                return "(" + left + ").has(" + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledSet) {
                return "(" + left + ").has(" + right + ")";
            }
            if (leftType == PrimitiveLinkedType.STRING) {
                return "String(" + left + ").includes(" + right + ")";
            }
            return "capy.contains(" + left + ", " + right + ")";
        }

        private String renderCollectionPlus(CompiledType leftType, String left, CompiledType rightType, String right) {
            if (leftType instanceof CollectionLinkedType.CompiledList) {
                return rightType instanceof CollectionLinkedType.CompiledList
                        ? "capy.listPlus(" + left + ", " + right + ")"
                        : "capy.listAppend(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledSet) {
                return rightType instanceof CollectionLinkedType.CompiledSet
                        ? "capy.setPlus(" + left + ", " + right + ")"
                        : "capy.setAppend(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledDict) {
                return rightType instanceof CompiledTupleType
                        ? "capy.dictPut(" + left + ", " + right + ")"
                        : "capy.dictPlus(" + left + ", " + right + ")";
            }
            if (leftType == PrimitiveLinkedType.STRING || rightType == PrimitiveLinkedType.STRING) {
                return "capy.toStringValue(" + left + ") + capy.toStringValue(" + right + ")";
            }
            return "((" + left + ") + (" + right + "))";
        }

        private String renderCollectionMinus(CompiledType leftType, String left, CompiledType rightType, String right) {
            if (leftType instanceof CollectionLinkedType.CompiledList) {
                return rightType instanceof CollectionLinkedType.CompiledList
                        ? "capy.listMinus(" + left + ", " + right + ")"
                        : "capy.listRemove(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledSet) {
                return rightType instanceof CollectionLinkedType.CompiledSet
                        ? "capy.setMinus(" + left + ", " + right + ")"
                        : "capy.setRemove(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledDict) {
                return rightType instanceof CollectionLinkedType.CompiledDict
                        ? "capy.dictMinus(" + left + ", " + right + ")"
                        : "capy.dictRemove(" + left + ", " + right + ")";
            }
            return "((" + left + ") - (" + right + "))";
        }

        private String renderConversion(String methodName, String receiver, CompiledType returnType) {
            return switch (methodName) {
                case "to_int" -> returnType instanceof GenericDataType
                        ? "capy.parseIntResult(" + receiver + ")"
                        : "Math.trunc(" + receiver + ")";
                case "to_long" -> returnType instanceof GenericDataType
                        ? "capy.parseLongResult(" + receiver + ")"
                        : "Math.trunc(" + receiver + ")";
                case "to_double", "to_float" -> returnType instanceof GenericDataType
                        ? "capy.parseFloatResult(" + receiver + ")"
                        : "Number(" + receiver + ")";
                case "to_bool" -> returnType instanceof GenericDataType
                        ? "capy.parseBoolResult(" + receiver + ")"
                        : "capy.truthy(" + receiver + ")";
                default -> "(" + receiver + ")";
            };
        }

        private String resolveFunctionTarget(CompiledFunctionCall functionCall) {
            var emittedName = emittedMethodName(functionCall);
            var lastDot = functionCall.name().lastIndexOf('.');
            if (lastDot >= 0) {
                var className = functionCall.name().substring(0, lastDot);
                if (isCurrentClassReference(className)) {
                    return emittedName;
                }
                var resolvedClassName = programContext.resolveClassName(className).orElse(className);
                require(resolvedClassName);
                return moduleVar(resolvedClassName) + "." + emittedName;
            }
            var owner = programContext.importedMemberOwner(moduleInfo.className(), emittedName)
                    .or(() -> programContext.importedMemberOwner(moduleInfo.className(), simpleMethodName(functionCall.name())));
            if (owner.isPresent()) {
                var className = owner.orElseThrow();
                require(className);
                return moduleVar(className) + "." + emittedName;
            }
            return emittedName;
        }

        private void require(String className) {
            if (className.equals(moduleInfo.className())) {
                return;
            }
            programContext.pathForClassName(className)
                    .ifPresent(path -> requiredModules.putIfAbsent(className, path));
        }

        private boolean isCurrentClassReference(String className) {
            if (className.equals(moduleInfo.className())) {
                return true;
            }
            return classNameCandidates(className).contains(moduleInfo.className());
        }

        private String dataConstructorReference(CompiledDataType dataType) {
            var typeName = simpleTypeName(dataType.name());
            if (programContext.localTypeNames(moduleInfo.className()).contains(typeName)) {
                return typeName;
            }
            var owner = programContext.importedMemberOwner(moduleInfo.className(), typeName);
            if (owner.isPresent()) {
                var className = owner.orElseThrow();
                require(className);
                return moduleVar(className) + "." + typeName;
            }
            return typeName;
        }

        private String emittedMethodName(CompiledFunctionCall functionCall) {
            return programContext.emittedFunctionName(functionCall.name(), functionCall.arguments().stream().map(CompiledExpression::type).toList());
        }

        private static boolean isConstCall(CompiledFunctionCall functionCall) {
            if (!functionCall.arguments().isEmpty()) {
                return false;
            }
            var name = simpleMethodName(functionCall.name());
            return name.contains("__local_const_") || CONST_NAME_PATTERN.matcher(name).matches();
        }
    }

    private record RenderedPattern(String condition, List<String> bindings, Scope scope) {
    }

    private record Scope(Map<String, String> bindings, Set<String> usedNames) {
        static Scope root() {
            return new Scope(Map.of(), Set.of());
        }

        String resolve(String sourceName) {
            return bindings.getOrDefault(sourceName, normalizeJsIdentifier(sourceName));
        }

        Scope bind(String sourceName, String jsName) {
            var updatedBindings = new HashMap<>(bindings);
            var updatedUsedNames = new HashSet<>(usedNames);
            updatedBindings.put(sourceName, jsName);
            updatedUsedNames.add(jsName);
            return new Scope(Map.copyOf(updatedBindings), Set.copyOf(updatedUsedNames));
        }

        String reserve(String sourceName) {
            var base = normalizeJsIdentifier(sourceName);
            if (!usedNames.contains(base)) {
                return base;
            }
            var idx = 1;
            while (usedNames.contains(base + "_j" + idx)) {
                idx++;
            }
            return base + "_j" + idx;
        }
    }

    private record ModuleInfo(CompiledModule module, JavaClass javaClass, String className, Path relativePath, String packageName, String packagePath) {
        static ModuleInfo from(CompiledModule module, JavaClass javaClass) {
            var packageName = javaClass.javaPackage().toString();
            var className = packageName.isBlank() ? javaClass.name().toString() : packageName + "." + javaClass.name();
            var relativePath = packageName.isBlank()
                    ? Path.of(javaClass.name() + ".js")
                    : Path.of(packageName.replace('.', '/'), javaClass.name() + ".js");
            var packagePath = module.path().replace('\\', '/').replaceFirst("^/", "");
            if (packagePath.isBlank()) {
                packagePath = module.name();
            } else {
                packagePath = packagePath + "/" + module.name();
            }
            return new ModuleInfo(module, javaClass, className, relativePath, packageName, packagePath);
        }
    }

    private record ProgramContext(
            Map<String, Path> pathsByClassName,
            Map<String, Set<String>> exportsByClassName,
            Map<String, Set<String>> localTypesByClassName,
            Map<String, Map<String, String>> importedOwnersByClassName,
            Map<String, List<String>> fieldsByType,
            Map<String, List<String>> parentTypesByType,
            Map<String, String> functionNameOverrides
    ) {
        static ProgramContext build(List<ModuleInfo> modules, Map<String, String> functionNameOverrides) {
            var paths = new LinkedHashMap<String, Path>();
            var exports = runtimeExports();
            var localTypes = new LinkedHashMap<String, Set<String>>();
            var fields = standardFields();
            var parentTypes = new LinkedHashMap<String, List<String>>();

            for (var runtimeClassName : RuntimeModules.classNames()) {
                paths.put(runtimeClassName, classNamePath(runtimeClassName));
            }
            for (var module : modules) {
                paths.put(module.className(), module.relativePath());
                var moduleExports = new LinkedHashSet<String>();
                var moduleTypes = new LinkedHashSet<String>();
                for (var record : module.javaClass().records()) {
                    var name = record.name().toString();
                    moduleTypes.add(name);
                    fields.put(name, record.fields().stream().map(JavaRecord.JavaRecordField::name).toList());
                    if (!record.isPrivate()) {
                        moduleExports.add(name);
                    }
                    for (var iface : record.implementInterfaces()) {
                        parentTypes.computeIfAbsent(name, ignored -> new ArrayList<>()).add(iface.toString());
                    }
                }
                for (var javaEnum : module.javaClass().enums()) {
                    var name = javaEnum.name().toString();
                    moduleTypes.add(name);
                    moduleExports.add(name);
                    if (javaEnum.values().isEmpty()) {
                        fields.putIfAbsent(name, List.of());
                    }
                    for (var value : javaEnum.values()) {
                        moduleTypes.add(value);
                        moduleExports.add(value);
                        fields.putIfAbsent(value, List.of());
                        parentTypes.computeIfAbsent(value, ignored -> new ArrayList<>()).add(name);
                    }
                }
                module.javaClass().staticConsts().stream()
                        .filter(javaConst -> !javaConst.isPrivate())
                        .map(JavaConst::name)
                        .forEach(moduleExports::add);
                module.javaClass().staticMethods().stream()
                        .filter(method -> !method.isPrivate())
                        .map(JavaMethod::name)
                        .forEach(moduleExports::add);
                exports.put(module.className(), Set.copyOf(moduleExports));
                localTypes.put(module.className(), Set.copyOf(moduleTypes));
            }

            var importedOwners = new LinkedHashMap<String, Map<String, String>>();
            for (var module : modules) {
                var imported = new LinkedHashMap<String, String>();
                for (var staticImport : module.javaClass().staticImports()) {
                    var idx = staticImport.lastIndexOf('.');
                    if (idx < 0) {
                        continue;
                    }
                    var className = staticImport.substring(0, idx);
                    var memberName = staticImport.substring(idx + 1);
                    if ("*".equals(memberName)) {
                        exports.getOrDefault(className, Set.of()).forEach(member -> imported.putIfAbsent(member, className));
                    } else {
                        imported.putIfAbsent(memberName, className);
                        imported.putIfAbsent(normalizeJsIdentifier(memberName), className);
                    }
                }
                importedOwners.put(module.className(), Map.copyOf(imported));
            }

            return new ProgramContext(
                    Map.copyOf(paths),
                    Map.copyOf(exports),
                    Map.copyOf(localTypes),
                    Map.copyOf(importedOwners),
                    Map.copyOf(fields),
                    Map.copyOf(parentTypes),
                    Map.copyOf(functionNameOverrides)
            );
        }

        Optional<Path> pathForClassName(String className) {
            return resolveClassName(className).map(pathsByClassName::get);
        }

        Optional<String> resolveClassName(String className) {
            return classNameCandidates(className).stream()
                    .filter(pathsByClassName::containsKey)
                    .findFirst();
        }

        Optional<String> importedMemberOwner(String className, String memberName) {
            return Optional.ofNullable(importedOwnersByClassName.getOrDefault(className, Map.of()).get(memberName));
        }

        Set<String> localTypeNames(String className) {
            return localTypesByClassName.getOrDefault(className, Set.of());
        }

        List<String> fieldsForType(String typeName) {
            return fieldsByType.getOrDefault(simpleTypeName(typeName), List.of());
        }

        List<String> parentTypes(String typeName) {
            return parentTypesByType.getOrDefault(simpleTypeName(typeName), List.of());
        }

        String emittedFunctionName(String name, List<CompiledType> parameterTypes) {
            var key = signatureKey(name, parameterTypes);
            if (functionNameOverrides.containsKey(key)) {
                return functionNameOverrides.get(key);
            }
            var parameterSignature = parameterTypes.stream().map(String::valueOf).collect(joining(","));
            var simple = simpleMethodName(name);
            var overrideBySimpleName = findOverrideBySimpleName(functionNameOverrides, name, parameterSignature);
            return overrideBySimpleName.orElseGet(() -> normalizeJsIdentifier(simple));
        }

        private static Map<String, Set<String>> runtimeExports() {
            var exports = new LinkedHashMap<String, Set<String>>();
            exports.put("capy.lang.Option", Set.of("Some", "None"));
            exports.put("capy.lang.Result", Set.of("Success", "Error"));
            exports.put("capy.lang.Effect", Set.of("pure", "delay"));
            exports.put("capy.lang.Primitives", Set.of("to_int", "to_long", "to_double", "to_float", "to_bool"));
            exports.put("capy.lang.String", Set.of("length", "get", "replace", "is_empty", "plus", "contains", "trim"));
            exports.put("capy.collection.List", Set.of("size", "get", "is_empty", "plus", "minus", "contains", "any", "all", "map", "filter", "reject", "flat_map", "flatMap", "reduce"));
            exports.put("capy.collection.Set", Set.of("size", "to_list", "is_empty", "plus", "minus", "contains", "any", "all", "map", "filter", "reject", "flat_map", "flatMap", "reduce"));
            exports.put("capy.collection.Dict", Set.of("size", "entries", "get", "is_empty", "plus", "minus", "contains_key", "any", "all", "map", "filter", "reject", "reduce"));
            exports.put("capy.collection.Tuple", Set.of("get"));
            exports.put("capy.io.Console", Set.of("print", "println", "print_error", "println_error", "read_line"));
            return exports;
        }

        private static Map<String, List<String>> standardFields() {
            var fields = new LinkedHashMap<String, List<String>>();
            fields.put("Some", List.of("value"));
            fields.put("None", List.of());
            fields.put("Success", List.of("value"));
            fields.put("Error", List.of("message"));
            fields.put("_UnsafeEffect", List.of("unsafe_thunk"));
            fields.put("Cons", List.of("value", "rest"));
            fields.put("End", List.of());
            return fields;
        }
    }

    private static final class RuntimeModules {
        private static List<GeneratedModule> modules() {
            return List.of(
                    new GeneratedModule(RUNTIME_PATH, runtime()),
                    new GeneratedModule(Path.of("capy", "lang", "Option.js"), runtimeForwarder("../../dev/capylang/capybara.js", "Some", "None")),
                    new GeneratedModule(Path.of("capy", "lang", "Result.js"), runtimeForwarder("../../dev/capylang/capybara.js", "Success", "Error")),
                    new GeneratedModule(Path.of("capy", "lang", "Effect.js"), runtimeForwarder("../../dev/capylang/capybara.js", "pure", "delay")),
                    new GeneratedModule(Path.of("capy", "lang", "Primitives.js"), primitivesRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "String.js"), stringRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "List.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Set.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Dict.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Tuple.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "Console.js"), consoleRuntime())
            );
        }

        private static Set<String> classNames() {
            return Set.of(
                    "capy.lang.Option",
                    "capy.lang.Result",
                    "capy.lang.Effect",
                    "capy.lang.Primitives",
                    "capy.lang.String",
                    "capy.collection.List",
                    "capy.collection.Set",
                    "capy.collection.Dict",
                    "capy.collection.Tuple",
                    "capy.io.Console"
            );
        }

        private static String runtimeForwarder(String helperPath, String... names) {
            return "'use strict';\n"
                   + "const capy = require(" + jsString(helperPath) + ");\n"
                   + "module.exports = {\n"
                   + Stream.of(names).map(name -> "    " + name + ": capy." + name + ",").collect(joining("\n"))
                   + "\n};\n";
        }

        private static String primitivesRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "module.exports = {\n"
                   + "    to_int: capy.parseIntResult,\n"
                   + "    to_long: capy.parseLongResult,\n"
                   + "    to_double: capy.parseFloatResult,\n"
                   + "    to_float: capy.parseFloatResult,\n"
                   + "    to_bool: capy.parseBoolResult,\n"
                   + "};\n";
        }

        private static String stringRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "module.exports = {\n"
                   + "    length: value => String(value).length,\n"
                   + "    get: (value, start, end) => end === undefined ? capy.getIndex(value, start) : capy.slice(value, start, end),\n"
                   + "    replace: (value, oldValue, newValue) => String(value).split(oldValue).join(newValue),\n"
                   + "    is_empty: value => String(value).length === 0,\n"
                   + "    plus: (left, right) => capy.toStringValue(left) + capy.toStringValue(right),\n"
                   + "    contains: (value, part) => String(value).includes(part),\n"
                   + "    trim: value => String(value).trim(),\n"
                   + "};\n";
        }

        private static String collectionRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "module.exports = {\n"
                   + "    size: value => value instanceof Map || value instanceof Set ? value.size : value.length,\n"
                   + "    get: (value, start, end) => end === undefined ? capy.getIndex(value, start) : capy.slice(value, start, end),\n"
                   + "    entries: value => Array.from(value.entries()),\n"
                   + "    to_list: value => Array.from(value),\n"
                   + "    is_empty: value => (value instanceof Map || value instanceof Set ? value.size : value.length) === 0,\n"
                   + "    plus: (left, right) => Array.isArray(left) ? (Array.isArray(right) ? capy.listPlus(left, right) : capy.listAppend(left, right)) : left instanceof Set ? (right instanceof Set ? capy.setPlus(left, right) : capy.setAppend(left, right)) : Array.isArray(right) ? capy.dictPut(left, right) : capy.dictPlus(left, right),\n"
                   + "    minus: (left, right) => Array.isArray(left) ? (Array.isArray(right) ? capy.listMinus(left, right) : capy.listRemove(left, right)) : left instanceof Set ? (right instanceof Set ? capy.setMinus(left, right) : capy.setRemove(left, right)) : right instanceof Map ? capy.dictMinus(left, right) : capy.dictRemove(left, right),\n"
                   + "    contains: capy.contains,\n"
                   + "    contains_key: (dict, key) => dict.has(key),\n"
                   + "    any: capy.any,\n"
                   + "    all: capy.all,\n"
                   + "    map: capy.mapCollection,\n"
                   + "    filter: capy.filterCollection,\n"
                   + "    reject: capy.rejectCollection,\n"
                   + "    flat_map: capy.flatMapCollection,\n"
                   + "    flatMap: capy.flatMapCollection,\n"
                   + "    reduce: capy.reduceCollection,\n"
                   + "};\n";
        }

        private static String consoleRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "const printValue = value => { process.stdout.write(capy.toStringValue(value)); return value; };\n"
                   + "const printlnValue = value => { console.log(capy.toStringValue(value)); return value; };\n"
                   + "const printError = value => { process.stderr.write(capy.toStringValue(value)); return value; };\n"
                   + "const printlnError = value => { console.error(capy.toStringValue(value)); return value; };\n"
                   + "module.exports = {\n"
                   + "    print: value => capy.delay(() => printValue(value)),\n"
                   + "    println: value => capy.delay(() => printlnValue(value)),\n"
                   + "    print_error: value => capy.delay(() => printError(value)),\n"
                   + "    println_error: value => capy.delay(() => printlnError(value)),\n"
                   + "    read_line: () => capy.delay(() => capy.None),\n"
                   + "};\n";
        }

        private static String runtime() {
            return """
                    'use strict';

                    class Some {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Some';
                            this.__capybaraTypes = ['Some', 'Option'];
                            this.value = fields.value;
                        }
                        toString() { return dataToString(this); }
                        capybaraDataValueInfo() { return dataValueInfo(this, 'Some', 'capy.lang', 'capy/lang/Option'); }
                    }

                    const None = Object.freeze({
                        __capybaraType: 'None',
                        __capybaraTypes: ['None', 'Option'],
                        toString() { return 'None { }'; },
                        capybaraDataValueInfo() { return dataValueInfo(this, 'None', 'capy.lang', 'capy/lang/Option'); },
                    });

                    class Success {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Success';
                            this.__capybaraTypes = ['Success', 'Result'];
                            this.value = fields.value;
                        }
                        toString() { return dataToString(this); }
                        capybaraDataValueInfo() { return dataValueInfo(this, 'Success', 'capy.lang', 'capy/lang/Result'); }
                    }

                    class ErrorValue {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Error';
                            this.__capybaraTypes = ['Error', 'Result'];
                            this.message = fields.message;
                        }
                        toString() { return dataToString(this); }
                        capybaraDataValueInfo() { return dataValueInfo(this, 'Error', 'capy.lang', 'capy/lang/Result'); }
                    }

                    class Effect {
                        constructor(thunk) {
                            this.thunk = thunk;
                            this.__capybaraType = '_UnsafeEffect';
                            this.__capybaraTypes = ['_UnsafeEffect', 'Effect'];
                        }
                        unsafe_run() { return this.thunk(); }
                        map(mapper) { return delay(() => mapper(this.unsafe_run())); }
                        flat_map(mapper) { return delay(() => mapper(this.unsafe_run()).unsafe_run()); }
                    }

                    function delay(thunk) {
                        return new Effect(thunk);
                    }

                    function pure(value) {
                        return delay(() => value);
                    }

                    function isEffect(value) {
                        return value instanceof Effect || Boolean(value && typeof value.unsafe_run === 'function');
                    }

                    function invoke(fn, ...args) {
                        let result = fn;
                        for (const arg of args) {
                            result = result(arg);
                        }
                        return result;
                    }

                    function enumValue(name, owner, parents = []) {
                        return Object.freeze({
                            __capybaraType: name,
                            __capybaraTypes: [name, owner, ...parents],
                            toString() { return name; },
                            capybaraDataValueInfo() { return dataValueInfo(this, name, '', owner); },
                        });
                    }

                    function isType(value, typeName) {
                        return Boolean(value && Array.isArray(value.__capybaraTypes) && value.__capybaraTypes.includes(typeName));
                    }

                    function normalizeIndex(index, size) {
                        return index < 0 ? size + index : index;
                    }

                    function rawIndex(source, index) {
                        const normalized = normalizeIndex(index, source.length);
                        return source[normalized];
                    }

                    function getIndex(source, index) {
                        if (source instanceof Map) {
                            return source.has(index) ? new Some({ value: source.get(index) }) : None;
                        }
                        const value = String(source) === source ? String(source) : source;
                        const size = value.length;
                        const normalized = normalizeIndex(index, size);
                        if (normalized < 0 || normalized >= size) {
                            return None;
                        }
                        return new Some({ value: String(source) === source ? value.charAt(normalized) : value[normalized] });
                    }

                    function slice(source, start, end) {
                        const value = String(source) === source ? String(source) : source;
                        const size = value.length;
                        const from = start === undefined ? 0 : normalizeIndex(start, size);
                        const to = end === undefined ? size : normalizeIndex(end, size);
                        return value.slice(from, to);
                    }

                    function equals(left, right) {
                        if (Object.is(left, right)) {
                            return true;
                        }
                        if (Array.isArray(left) && Array.isArray(right)) {
                            return left.length === right.length && left.every((value, index) => equals(value, right[index]));
                        }
                        if (left instanceof Set && right instanceof Set) {
                            return left.size === right.size && Array.from(left).every(value => contains(right, value));
                        }
                        if (left instanceof Map && right instanceof Map) {
                            return left.size === right.size
                                && Array.from(left.entries()).every(([key, value]) => right.has(key) && equals(value, right.get(key)));
                        }
                        if (left && right && left.__capybaraType && right.__capybaraType && left.__capybaraType === right.__capybaraType) {
                            const keys = Object.keys(left).filter(key => !key.startsWith('__') && typeof left[key] !== 'function');
                            return keys.length === Object.keys(right).filter(key => !key.startsWith('__') && typeof right[key] !== 'function').length
                                && keys.every(key => equals(left[key], right[key]));
                        }
                        return false;
                    }

                    function contains(collection, value) {
                        if (collection instanceof Set) {
                            return Array.from(collection).some(item => equals(item, value));
                        }
                        if (collection instanceof Map) {
                            return collection.has(value);
                        }
                        if (typeof collection === 'string') {
                            return collection.includes(value);
                        }
                        return collection.some(item => equals(item, value));
                    }

                    function truthy(value) {
                        if (value === false || value === null || value === undefined) {
                            return false;
                        }
                        if (typeof value === 'number') {
                            return value !== 0;
                        }
                        if (typeof value === 'string' || Array.isArray(value)) {
                            return value.length > 0;
                        }
                        if (value instanceof Set || value instanceof Map) {
                            return value.size > 0;
                        }
                        return true;
                    }

                    function listAppend(list, value) {
                        return [...list, value];
                    }

                    function listPlus(left, right) {
                        return [...left, ...right];
                    }

                    function listRemove(list, value) {
                        return list.filter(item => !equals(item, value));
                    }

                    function listMinus(left, right) {
                        return left.filter(item => !contains(right, item));
                    }

                    function setAppend(set, value) {
                        return new Set([...set, value]);
                    }

                    function setPlus(left, right) {
                        return new Set([...left, ...right]);
                    }

                    function setRemove(set, value) {
                        return new Set(Array.from(set).filter(item => !equals(item, value)));
                    }

                    function setMinus(left, right) {
                        return new Set(Array.from(left).filter(item => !contains(right, item)));
                    }

                    function dictPut(dict, tuple) {
                        const result = new Map(dict);
                        result.set(tuple[0], tuple[1]);
                        return result;
                    }

                    function dictPlus(left, right) {
                        return new Map([...left, ...right]);
                    }

                    function dictRemove(dict, key) {
                        const result = new Map(dict);
                        result.delete(key);
                        return result;
                    }

                    function dictMinus(left, right) {
                        const result = new Map(left);
                        for (const key of right.keys()) {
                            result.delete(key);
                        }
                        return result;
                    }

                    function entries(value) {
                        if (value instanceof Map) {
                            return Array.from(value.entries());
                        }
                        if (value instanceof Set) {
                            return Array.from(value.values());
                        }
                        if (typeof value === 'string') {
                            return Array.from(value);
                        }
                        return value;
                    }

                    function mapCollection(value, mapper) {
                        if (value instanceof Map) {
                            return new Map(Array.from(value.entries()).map(([key, item]) => [key, invoke(mapper, key, item)]));
                        }
                        if (value instanceof Set) {
                            return new Set(Array.from(value).map(item => invoke(mapper, item)));
                        }
                        if (typeof value === 'string') {
                            return Array.from(value).map(item => invoke(mapper, item));
                        }
                        return value.map(item => invoke(mapper, item));
                    }

                    function filterCollection(value, predicate) {
                        if (value instanceof Map) {
                            return new Map(Array.from(value.entries()).filter(([key, item]) => invoke(predicate, key, item)));
                        }
                        if (value instanceof Set) {
                            return new Set(Array.from(value).filter(item => invoke(predicate, item)));
                        }
                        return value.filter(item => invoke(predicate, item));
                    }

                    function rejectCollection(value, predicate) {
                        if (value instanceof Map) {
                            return new Map(Array.from(value.entries()).filter(([key, item]) => !invoke(predicate, key, item)));
                        }
                        if (value instanceof Set) {
                            return new Set(Array.from(value).filter(item => !invoke(predicate, item)));
                        }
                        return value.filter(item => !invoke(predicate, item));
                    }

                    function flatMapCollection(value, mapper) {
                        const mapped = entries(value).flatMap(item => invoke(mapper, item));
                        return mapped;
                    }

                    function any(value, predicate) {
                        if (value instanceof Map) {
                            return Array.from(value.entries()).some(([key, item]) => invoke(predicate, key, item));
                        }
                        return entries(value).some(item => invoke(predicate, item));
                    }

                    function all(value, predicate) {
                        if (value instanceof Map) {
                            return Array.from(value.entries()).every(([key, item]) => invoke(predicate, key, item));
                        }
                        return entries(value).every(item => invoke(predicate, item));
                    }

                    function reduceCollection(value, initial, reducer) {
                        let acc = initial;
                        if (value instanceof Map) {
                            for (const [key, item] of value.entries()) {
                                acc = invoke(reducer, acc, key, item);
                            }
                            return acc;
                        }
                        for (const item of entries(value)) {
                            acc = invoke(reducer, acc, item);
                        }
                        return acc;
                    }

                    function optionMap(option, mapper) {
                        return isType(option, 'Some') ? mapper(option.value) : None;
                    }

                    function optionFlatMap(option, mapper) {
                        return isType(option, 'Some') ? mapper(option.value) : None;
                    }

                    function optionFilterOut(option, predicate) {
                        return isType(option, 'Some') && predicate(option.value) ? None : option;
                    }

                    function parseResult(value, parser) {
                        try {
                            const parsed = parser(value);
                            if (Number.isNaN(parsed)) {
                                return new ErrorValue({ message: `Unable to parse ${value}` });
                            }
                            return new Success({ value: parsed });
                        } catch (error) {
                            return new ErrorValue({ message: error.message });
                        }
                    }

                    function parseIntResult(value) {
                        return parseResult(value, text => Number.parseInt(text, 10));
                    }

                    function parseLongResult(value) {
                        return parseIntResult(value);
                    }

                    function parseFloatResult(value) {
                        return parseResult(value, text => Number.parseFloat(text));
                    }

                    function parseBoolResult(value) {
                        if (value === 'true') {
                            return new Success({ value: true });
                        }
                        if (value === 'false') {
                            return new Success({ value: false });
                        }
                        return new ErrorValue({ message: `Unable to parse ${value}` });
                    }

                    function toStringValue(value) {
                        if (value === null || value === undefined) {
                            return '';
                        }
                        if (typeof value === 'string') {
                            return value;
                        }
                        if (Array.isArray(value)) {
                            return '[' + value.map(toStringValue).join(', ') + ']';
                        }
                        if (value instanceof Set) {
                            return '{' + Array.from(value).map(toStringValue).join(', ') + '}';
                        }
                        if (value instanceof Map) {
                            return '{' + Array.from(value.entries()).map(([key, item]) => `${key}: ${toStringValue(item)}`).join(', ') + '}';
                        }
                        if (typeof value.toString === 'function' && value.toString !== Object.prototype.toString) {
                            return value.toString();
                        }
                        return String(value);
                    }

                    function dataToString(value) {
                        const keys = Object.keys(value).filter(key => !key.startsWith('__') && typeof value[key] !== 'function');
                        if (keys.length === 0) {
                            return `${value.__capybaraType} { }`;
                        }
                        return `${value.__capybaraType} { ` + keys.map(key => `"${key}": ${toStringValue(value[key])}`).join(', ') + ' }';
                    }

                    function dataValueInfo(value, name, packageName, packagePath) {
                        const fields = Object.keys(value)
                            .filter(key => !key.startsWith('__') && typeof value[key] !== 'function')
                            .map(key => ({ name: key, value: value[key] }));
                        return { name, packageName, packagePath, fields };
                    }

                    function reflection(target, name, packageName, packagePath, fieldNames) {
                        if (target && typeof target.capybaraDataValueInfo === 'function') {
                            return target.capybaraDataValueInfo();
                        }
                        return {
                            name,
                            packageName,
                            packagePath,
                            fields: fieldNames.map(field => ({ name: field, value: target[field] })),
                        };
                    }

                    function unsupported(message) {
                        throw new Error(message);
                    }

                    module.exports = {
                        Some,
                        None,
                        Success,
                        Error: ErrorValue,
                        Effect,
                        pure,
                        delay,
                        isEffect,
                        invoke,
                        enumValue,
                        isType,
                        rawIndex,
                        getIndex,
                        slice,
                        equals,
                        contains,
                        truthy,
                        listAppend,
                        listPlus,
                        listRemove,
                        listMinus,
                        setAppend,
                        setPlus,
                        setRemove,
                        setMinus,
                        dictPut,
                        dictPlus,
                        dictRemove,
                        dictMinus,
                        mapCollection,
                        filterCollection,
                        rejectCollection,
                        flatMapCollection,
                        any,
                        all,
                        reduceCollection,
                        optionMap,
                        optionFlatMap,
                        optionFilterOut,
                        parseIntResult,
                        parseLongResult,
                        parseFloatResult,
                        parseBoolResult,
                        toStringValue,
                        dataToString,
                        dataValueInfo,
                        reflection,
                        unsupported,
                    };
                    """;
        }
    }

    private static Map<String, String> buildFunctionNameOverrides(CompiledProgram program) {
        var overrides = new LinkedHashMap<String, String>();
        var collisions = new LinkedHashMap<String, List<CompiledFunction>>();
        var ownerModuleNames = new java.util.IdentityHashMap<CompiledFunction, String>();
        for (var module : program.modules()) {
            for (var function : module.functions()) {
                ownerModuleNames.put(function, module.name());
                var ownerKey = function.name().startsWith(METHOD_DECL_PREFIX)
                        ? function.name().substring(0, Math.max(function.name().lastIndexOf("__"), METHOD_DECL_PREFIX.length()))
                        : module.name();
                var normalizedBaseName = normalizeJsIdentifier(baseMethodName(function.name()));
                var erasedSignature = function.parameters().stream()
                        .map(parameter -> String.valueOf(parameter.type()))
                        .collect(joining(","));
                collisions.computeIfAbsent(ownerKey + "|" + normalizedBaseName + "|" + erasedSignature, ignored -> new ArrayList<>()).add(function);
            }
        }
        for (var functions : collisions.values()) {
            if (functions.size() < 2) {
                continue;
            }
            var canonicalNamedFunction = functions.stream()
                    .filter(function -> isNamedIdentifier(baseMethodName(function.name())))
                    .findFirst();
            var mixedRawNames = functions.stream().map(function -> baseMethodName(function.name())).distinct().count() > 1;
            for (var function : functions) {
                var rawBaseName = baseMethodName(function.name());
                var normalizedBaseName = normalizeJsIdentifier(rawBaseName);
                var overloadSuffix = overloadSuffix(function);
                var legacyEmittedName = normalizedBaseName + overloadSuffix;
                var namedCanonical = canonicalNamedFunction.filter(named -> named == function).isPresent();
                var emittedName = mixedRawNames
                        ? (namedCanonical
                                ? normalizedBaseName
                                : normalizedBaseName + "__" + methodVariantSuffix(rawBaseName) + overloadSuffix)
                        : legacyEmittedName;
                var parameterTypes = function.parameters().stream().map(CompiledFunction.CompiledFunctionParameter::type).toList();
                overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
                if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(ownerModuleNames.get(function) + "." + function.name(), parameterTypes), emittedName);
                }
                if (mixedRawNames || !function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(baseMethodName(function.name()), parameterTypes), emittedName);
                }
                if (!emittedName.equals(legacyEmittedName)) {
                    overrides.put(signatureKey(legacyEmittedName, parameterTypes), emittedName);
                }
            }
        }
        return Map.copyOf(overrides);
    }

    private static Optional<String> findOverrideBySimpleName(Map<String, String> functionNameOverrides, String targetName, String parameterSignature) {
        var simpleMethodName = simpleMethodName(targetName);
        var qualifier = qualifierName(targetName);
        var candidates = functionNameOverrides.entrySet().stream()
                .filter(entry -> simpleMethodName(keyName(entry.getKey())).equals(simpleMethodName))
                .filter(entry -> keyParameterSignature(entry.getKey()).equals(parameterSignature))
                .toList();
        if (qualifier != null) {
            var qualified = candidates.stream()
                    .filter(entry -> Objects.equals(qualifierName(keyName(entry.getKey())), qualifier))
                    .map(Map.Entry::getValue)
                    .findFirst();
            if (qualified.isPresent()) {
                return qualified;
            }
        }
        return candidates.stream()
                .filter(entry -> qualifierName(keyName(entry.getKey())) == null)
                .map(Map.Entry::getValue)
                .findFirst();
    }

    private static String keyName(String signatureKey) {
        var separator = signatureKey.indexOf('|');
        return separator >= 0 ? signatureKey.substring(0, separator) : signatureKey;
    }

    private static String keyParameterSignature(String signatureKey) {
        var separator = signatureKey.indexOf('|');
        return separator >= 0 ? signatureKey.substring(separator + 1) : "";
    }

    private static String qualifierName(String target) {
        var lastDot = target.lastIndexOf('.');
        return lastDot >= 0 ? target.substring(0, lastDot) : null;
    }

    private static String signatureKey(String name, List<CompiledType> parameterTypes) {
        return name + "|" + parameterTypes.stream().map(type -> String.valueOf(type)).collect(joining(","));
    }

    private static String baseMethodName(String name) {
        if (!name.startsWith(METHOD_DECL_PREFIX)) {
            return name;
        }
        var idx = name.lastIndexOf("__");
        return idx >= 0 ? name.substring(idx + 2) : name;
    }

    private static String simpleMethodName(String target) {
        var lastDot = target.lastIndexOf('.');
        var methodName = lastDot >= 0 ? target.substring(lastDot + 1) : target;
        return baseMethodName(methodName);
    }

    private static boolean isNamedIdentifier(String value) {
        return value.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_');
    }

    private static String overloadSuffix(CompiledFunction function) {
        var suffix = function.parameters().stream()
                .map(parameter -> sanitizeOverloadSuffix(String.valueOf(parameter.type())))
                .collect(joining("__"));
        return suffix.isBlank() ? "" : "__" + suffix;
    }

    private static String methodVariantSuffix(String rawBaseName) {
        var prefix = rawBaseName.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_') ? "name" : "op";
        return prefix + "_" + sanitizeMethodNameVariant(rawBaseName);
    }

    private static String sanitizeMethodNameVariant(String rawName) {
        var builder = new StringBuilder();
        for (var i = 0; i < rawName.length(); i++) {
            var ch = rawName.charAt(i);
            if (Character.isLetterOrDigit(ch)) {
                builder.append(Character.toLowerCase(ch));
            } else if (ch == '_') {
                builder.append('_');
            } else {
                if (!builder.isEmpty() && builder.charAt(builder.length() - 1) != '_') {
                    builder.append('_');
                }
                builder.append(symbolName(ch)).append('_');
            }
        }
        var sanitized = builder.toString().replaceAll("_+", "_");
        if (sanitized.startsWith("_")) {
            sanitized = sanitized.substring(1);
        }
        if (sanitized.endsWith("_")) {
            sanitized = sanitized.substring(0, sanitized.length() - 1);
        }
        return sanitized.isBlank() ? "generated" : sanitized;
    }

    private static String sanitizeOverloadSuffix(String typeName) {
        var sanitized = typeName.replaceAll("[^A-Za-z0-9]+", "_").replaceAll("_+", "_");
        if (sanitized.startsWith("_")) {
            sanitized = sanitized.substring(1);
        }
        if (sanitized.endsWith("_")) {
            sanitized = sanitized.substring(0, sanitized.length() - 1);
        }
        return sanitized.toLowerCase();
    }

    private static boolean isOptionType(CompiledType type) {
        if (!(type instanceof GenericDataType genericDataType)) {
            return false;
        }
        var name = genericDataType.name();
        return "Option".equals(name)
               || name.endsWith("/Option")
               || name.endsWith(".Option")
               || name.endsWith("/Option.Option");
    }

    private static String simpleTypeName(String typeName) {
        var stripped = typeName;
        var generic = stripped.indexOf('[');
        if (generic >= 0) {
            stripped = stripped.substring(0, generic);
        }
        var slash = stripped.lastIndexOf('/');
        if (slash >= 0) {
            stripped = stripped.substring(slash + 1);
        }
        var dot = stripped.lastIndexOf('.');
        if (dot >= 0) {
            stripped = stripped.substring(dot + 1);
        }
        return normalizeJsTypeIdentifier(stripped);
    }

    private static boolean isTypeLikeIdentifier(String value) {
        if (value == null || value.isBlank()) {
            return false;
        }
        var idx = 0;
        while (idx < value.length() && value.charAt(idx) == '_') {
            idx++;
        }
        return idx < value.length() && Character.isUpperCase(value.charAt(idx));
    }

    private static String normalizeJsTypeIdentifier(String rawName) {
        var parts = Stream.of(rawName.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();
        var base = new StringBuilder();
        if (parts.isEmpty()) {
            base.append("Generated");
        } else {
            for (var part : parts) {
                base.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    base.append(part.substring(1));
                }
            }
        }
        var identifier = base.toString();
        if (!Character.isLetter(identifier.charAt(0)) && identifier.charAt(0) != '_' && identifier.charAt(0) != '$') {
            identifier = "T" + identifier;
        }
        if (JS_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }

    private static String normalizeJsIdentifier(String rawName) {
        if ("_".equals(rawName)) {
            return "__unused";
        }
        var leadingUnderscores = 0;
        while (leadingUnderscores < rawName.length() && rawName.charAt(leadingUnderscores) == '_') {
            leadingUnderscores++;
        }
        var suffix = rawName.substring(leadingUnderscores);
        var parts = Stream.of(suffix.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();
        var base = new StringBuilder();
        if (parts.isEmpty()) {
            base.append(encodeSymbolicIdentifier(suffix));
        } else {
            for (var i = 0; i < parts.size(); i++) {
                var part = parts.get(i);
                if (i == 0) {
                    base.append(Character.toLowerCase(part.charAt(0)));
                } else {
                    base.append(Character.toUpperCase(part.charAt(0)));
                }
                if (part.length() > 1) {
                    base.append(part.substring(1));
                }
            }
        }
        var identifier = "_".repeat(leadingUnderscores) + base;
        if (identifier.isBlank()) {
            identifier = "__value";
        }
        if (!Character.isLetter(identifier.charAt(0)) && identifier.charAt(0) != '_' && identifier.charAt(0) != '$') {
            identifier = "_" + identifier;
        }
        if (JS_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }

    private static String encodeSymbolicIdentifier(String raw) {
        if (raw.isBlank()) {
            return "generated";
        }
        var parts = new ArrayList<String>();
        for (var i = 0; i < raw.length(); i++) {
            parts.add(symbolName(raw.charAt(i)));
        }
        return parts.stream().collect(joining("_"));
    }

    private static String symbolName(char symbol) {
        return switch (symbol) {
            case '+' -> "plus";
            case '-' -> "minus";
            case '*' -> "star";
            case '/' -> "slash";
            case '\\' -> "backslash";
            case '^' -> "power";
            case '%' -> "mod";
            case '$' -> "dollar";
            case '#' -> "hash";
            case '@' -> "at";
            case '~' -> "tilde";
            case '!' -> "bang";
            case ':' -> "colon";
            case '<' -> "less";
            case '>' -> "greater";
            case '|' -> "pipe";
            case '?' -> "question";
            default -> "op" + Integer.toHexString(symbol);
        };
    }

    private static String moduleVar(String className) {
        return "__module_" + className.replaceAll("[^A-Za-z0-9]+", "_");
    }

    private static Path classNamePath(String className) {
        return Path.of(className.replace('.', '/') + ".js");
    }

    private static List<String> classNameCandidates(String rawClassName) {
        var candidates = new LinkedHashSet<String>();
        candidates.add(rawClassName);
        var normalized = rawClassName.replace('\\', '.').replace('/', '.');
        while (normalized.startsWith(".")) {
            normalized = normalized.substring(1);
        }
        candidates.add(normalized);
        if (normalized.startsWith("_") && normalized.length() > 1 && Character.isLowerCase(normalized.charAt(1))) {
            candidates.add(normalized.substring(1));
        }
        return List.copyOf(candidates);
    }

    private static String relativeRequire(Path fromModule, Path targetModule) {
        var fromDir = Optional.ofNullable(fromModule.getParent()).orElse(Path.of(""));
        var relative = fromDir.relativize(targetModule).toString().replace('\\', '/');
        if (!relative.startsWith(".")) {
            relative = "./" + relative;
        }
        return relative;
    }

    private static String stripNumericSuffix(String value) {
        if (value.endsWith("L") || value.endsWith("l") || value.endsWith("f") || value.endsWith("F") || value.endsWith("d") || value.endsWith("D")) {
            return value.substring(0, value.length() - 1);
        }
        return value;
    }

    private static String jsArray(Collection<String> values) {
        return values.stream().map(JavaScriptGenerator::jsString).collect(joining(", ", "[", "]"));
    }

    private static String jsString(String value) {
        var escaped = value
                .replace("\\", "\\\\")
                .replace("'", "\\'")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
        return "'" + escaped + "'";
    }
}
