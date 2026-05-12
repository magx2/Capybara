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
    static final Path RUNTIME_PATH = Path.of("dev", "capylang", "capybara.js");
    private static final java.util.regex.Pattern CONST_NAME_PATTERN = java.util.regex.Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");
    private static final java.util.regex.Pattern MODULE_VAR_PATTERN = java.util.regex.Pattern.compile("\\b__module_[A-Za-z0-9_]+\\b");
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
        if (program.modules().isEmpty() && program.objectOrientedModules().isEmpty()) {
            return new GeneratedProgram(List.of());
        }

        var functionNameOverrides = buildFunctionNameOverrides(program);
        var astBuilder = new JavaAstBuilder(functionNameOverrides);
        var modules = new ArrayList<GeneratedModule>();
        var moduleInfos = program.modules().stream()
                .map(module -> ModuleInfo.from(module, astBuilder.build(module)))
                .toList();
        var context = ProgramContext.build(moduleInfos, program.objectOrientedModules(), functionNameOverrides);

        for (var moduleInfo : moduleInfos) {
            modules.add(new GeneratedModule(moduleInfo.relativePath(), new ModuleRenderer(context, moduleInfo).render()));
        }
        modules.addAll(new ObjectOrientedJavaScriptGenerator(context).generate(program.objectOrientedModules()));
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
            requireReferencedModules(body.toString());

            var output = new StringBuilder();
            output.append("'use strict';\n\n");
            output.append("const capy = require(")
                    .append(jsString(relativeRequire(moduleInfo.relativePath(), RUNTIME_PATH)))
                    .append(");\n");
            for (var entry : requiredModules.entrySet()) {
                output.append("const ")
                        .append(moduleVar(entry.getKey()))
                        .append(" = require(")
                        .append(jsString(relativeRequire(moduleInfo.relativePath(), entry.getValue())))
                        .append(");\n");
            }
            if (!requiredModules.isEmpty()) {
                output.append('\n');
            }
            output.append(body);
            return output.toString();
        }

        private void requireReferencedModules(String body) {
            var matcher = MODULE_VAR_PATTERN.matcher(body);
            while (matcher.find()) {
                programContext.classNameForModuleVariable(matcher.group())
                        .ifPresent(this::requireClassName);
            }
        }

        private void requireClassName(String className) {
            var resolvedClassName = programContext.resolveClassName(className).orElse(className);
            programContext.pathForClassName(resolvedClassName)
                    .ifPresent(path -> requiredModules.putIfAbsent(resolvedClassName, path));
        }

        private String renderRecord(JavaRecord record) {
            var code = new StringBuilder();
            var name = simpleTypeName(record.name().toString());
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
            code.append("    with_(...values) {\n");
            code.append("        if (values.length === 1 && values[0] && typeof values[0] === 'object' && !Array.isArray(values[0])) {\n");
            code.append("            return this.with(values[0]);\n");
            code.append("        }\n");
            code.append("        return new ").append(name).append("({\n");
            for (int i = 0; i < record.fields().size(); i++) {
                var field = record.fields().get(i);
                code.append("            ").append(field.name()).append(": values[").append(i).append("],\n");
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
            var recordMethodNames = record.methods().stream()
                    .map(JavaMethod::name)
                    .collect(java.util.stream.Collectors.toSet());
            if (!recordMethodNames.contains("pipe")) {
                code.append("    pipe(mapper) {\n");
                code.append("        return capy.resultLikePipe(this, mapper);\n");
                code.append("    }\n");
            }
            if (!recordMethodNames.contains("pipe_star")) {
                code.append("    pipe_star(mapper) {\n");
                code.append("        return capy.resultLikeFlatMap(this, mapper);\n");
                code.append("    }\n");
            }
            code.append("}\n");
            for (var method : interfaceDefaultMethods(record)) {
                if (!recordMethodNames.contains(method.name())) {
                    code.insert(code.length() - 2, renderFunction(method, false));
                }
            }
            if (!record.isPrivate()) {
                exportNames.add(name);
            }
            return code.toString();
        }

        private List<JavaMethod> interfaceDefaultMethods(JavaRecord record) {
            return record.implementInterfaces().stream()
                    .map(javaType -> simpleTypeName(javaType.toString()))
                    .flatMap(interfaceName -> javaClass.interfaces().stream()
                            .filter(javaInterface -> simpleTypeName(javaInterface.name().toString()).equals(interfaceName))
                            .flatMap(javaInterface -> javaInterface.defaultMethods().stream()))
                    .toList();
        }

        private String renderEnum(JavaEnum javaEnum) {
            var enumName = simpleTypeName(javaEnum.name().toString());
            var values = javaEnum.values().isEmpty() ? List.of("INSTANCE") : javaEnum.values();
            var code = new StringBuilder();
            if (values.size() == 1 && "INSTANCE".equals(values.getFirst())) {
                code.append("const ").append(enumName)
                        .append(" = capy.enumValue(")
                        .append(jsString(enumName)).append(", ")
                        .append(jsString(enumName)).append(", ")
                        .append(jsArray(programContext.parentTypes(enumName))).append(", ")
                        .append("0, [], ")
                        .append(jsString(moduleInfo.packageName())).append(", ")
                        .append(jsString(moduleInfo.packagePath()))
                        .append(");\n");
                exportNames.add(enumName);
                return code.toString();
            }
            code.append("const ").append(enumName).append(" = (() => {\n");
            code.append("    const values = [\n");
            for (var i = 0; i < values.size(); i++) {
                var value = values.get(i);
                var valueName = enumValueIdentifier(value);
                var aliases = valueName.equals(value)
                        ? List.<String>of()
                        : List.of(valueName);
                code.append("        capy.enumValue(")
                        .append(jsString(value)).append(", ")
                        .append(jsString(enumName)).append(", ")
                        .append(jsArray(programContext.parentTypes(valueName))).append(", ")
                        .append(i).append(", ")
                        .append(jsArray(aliases)).append(", ")
                        .append(jsString(moduleInfo.packageName())).append(", ")
                        .append(jsString(moduleInfo.packagePath())).append("),\n");
            }
            code.append("    ];\n");
            code.append("    return Object.freeze({\n");
            for (var i = 0; i < values.size(); i++) {
                var value = values.get(i);
                var valueName = enumValueIdentifier(value);
                code.append("        ").append(valueName).append(": values[").append(i).append("],\n");
            }
            code.append("        values,\n");
            code.append("        valuesSet: () => capy.set(values),\n");
            code.append("        parse: value => capy.parseEnum(value, values, ")
                    .append(jsString(enumName))
                    .append("),\n");
            code.append("    });\n");
            code.append("})();\n");
            exportNames.add(enumName);
            for (var value : values) {
                var valueName = enumValueIdentifier(value);
                code.append("const ").append(valueName).append(" = ").append(enumName).append(".").append(valueName).append(";\n");
                var typeAlias = simpleTypeName(value);
                if (!typeAlias.equals(valueName)) {
                    code.append("const ").append(typeAlias).append(" = ").append(valueName).append(";\n");
                }
                var normalizedAlias = normalizeJsIdentifier(value);
                if (!normalizedAlias.equals(valueName) && !normalizedAlias.equals(typeAlias)) {
                    code.append("const ").append(normalizedAlias).append(" = ").append(valueName).append(";\n");
                }
                exportNames.add(valueName);
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
            var name = topLevel
                    ? programContext.emittedFunctionName(method.sourceName(), method.sourceParameterTypes())
                    : method.name();
            var params = method.parameters().stream()
                    .map(JavaMethod.JavaFunctionParameter::generatedName)
                    .map(JavaScriptGenerator::normalizeJsIdentifier)
                    .toList();
            var scope = Scope.root();
            if (!topLevel) {
                scope = scope.bind("this", "this");
            }
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
                   + "        capy.writeProgramResult(value);\n"
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
                case CompiledLongValue longValue -> renderLongLiteral(longValue.longValue());
                case CompiledStringValue stringValue -> stringValue.toString();
                case CompiledVariable variable -> scope.resolve(variable.name());
                case CompiledNumericWidening numericWidening -> renderNumericWidening(numericWidening, scope);
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
                    .collect(joining(", ", "capy.set([", "])"));
        }

        private String renderNumericWidening(CompiledNumericWidening numericWidening, Scope scope) {
            var expression = render(numericWidening.expression(), scope);
            if (numericWidening.type() == PrimitiveLinkedType.LONG && numericWidening.expression().type() != PrimitiveLinkedType.LONG) {
                return "capy.toLong(" + expression + ")";
            }
            if ((numericWidening.type() == PrimitiveLinkedType.FLOAT || numericWidening.type() == PrimitiveLinkedType.DOUBLE)
                && numericWidening.expression().type() == PrimitiveLinkedType.LONG) {
                return "Number(" + expression + ")";
            }
            return expression;
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
            if (receiverType == PrimitiveLinkedType.ENUM && tailArgs.isEmpty()) {
                if ("name".equals(methodName)) {
                    return "(" + receiver + ").name";
                }
                if ("order".equals(methodName)) {
                    return "(" + receiver + ").ordinal";
                }
            }
            if ("length".equals(methodName) && receiverType == PrimitiveLinkedType.STRING) {
                return "(" + receiver + ").length";
            }
            if (("starts_with".equals(methodName) || "startsWith".equals(methodName))
                && receiverType == PrimitiveLinkedType.STRING
                && tailArgs.size() == 1) {
                return "String(" + receiver + ").startsWith(" + tailArgs.getFirst() + ")";
            }
            if (("end_with".equals(methodName) || "endWith".equals(methodName))
                && receiverType == PrimitiveLinkedType.STRING
                && tailArgs.size() == 1) {
                return "String(" + receiver + ").endsWith(" + tailArgs.getFirst() + ")";
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
            if (receiverType instanceof CollectionLinkedType.CompiledSet) {
                var nativeSetMethod = renderNativeSetMethod(methodName, receiver, tailArgs);
                if (nativeSetMethod.isPresent()) {
                    return nativeSetMethod.orElseThrow();
                }
            }
            if (List.of("+", "plus").contains(methodName) && tailArgs.size() == 1 && isNativePlusType(receiverType)) {
                return renderCollectionPlus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst());
            }
            if (List.of("-", "minus").contains(methodName) && tailArgs.size() == 1 && isNativeMinusType(receiverType)) {
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
            if (("map".equals(methodName) || "|".equals(methodName) || "pipe".equals(methodName)) && isCollectionType(receiverType)) {
                return "capy.mapCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("filter".equals(methodName) || "|-".equals(methodName) || "pipe_minus".equals(methodName)) && isCollectionType(receiverType)) {
                return "capy.filterCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("reject".equals(methodName) && isCollectionType(receiverType)) {
                return "capy.rejectCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("flat_map".equals(methodName) || "flatMap".equals(methodName) || "|*".equals(methodName) || "pipe_star".equals(methodName))
                && isCollectionType(receiverType)) {
                return "capy.flatMapCollection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("reduce".equals(methodName) || "|>".equals(methodName) || "pipe_greater".equals(methodName)) && tailArgs.size() >= 2 && isCollectionType(receiverType)) {
                return "capy.reduceCollection(" + receiver + ", " + tailArgs.get(0) + ", " + tailArgs.get(1) + ")";
            }
            if (isPrimitiveConversion(methodName)) {
                return renderConversion(methodName, receiver, receiverType, functionCall.type());
            }
            var emittedName = emittedMethodName(functionCall);
            return "(" + receiver + ")." + emittedName + "(" + String.join(", ", tailArgs) + ")";
        }

        private Optional<String> renderNativeSetMethod(String methodName, String receiver, List<String> tailArgs) {
            if (tailArgs.isEmpty() && List.of("power_set", "powerSet", "op2118", "℘").contains(methodName)) {
                return Optional.of("capy.setPowerSet(" + receiver + ")");
            }
            if (tailArgs.size() != 1) {
                return Optional.empty();
            }
            var other = tailArgs.getFirst();
            return switch (methodName) {
                case "is_subset_of", "isSubsetOf", "op2286", "⊆" ->
                        Optional.of("capy.setIsSubsetOf(" + receiver + ", " + other + ")");
                case "is_proper_subset_of", "isProperSubsetOf", "op2282", "⊂" ->
                        Optional.of("capy.setIsProperSubsetOf(" + receiver + ", " + other + ")");
                case "is_superset_of", "isSupersetOf", "op2287", "⊇" ->
                        Optional.of("capy.setIsSupersetOf(" + receiver + ", " + other + ")");
                case "is_proper_superset_of", "isProperSupersetOf", "op2283", "⊃" ->
                        Optional.of("capy.setIsProperSupersetOf(" + receiver + ", " + other + ")");
                case "union", "op222a", "∪" ->
                        Optional.of("capy.setPlus(" + receiver + ", " + other + ")");
                case "intersection", "op2229", "∩" ->
                        Optional.of("capy.setIntersection(" + receiver + ", " + other + ")");
                case "difference" ->
                        Optional.of("capy.setMinus(" + receiver + ", " + other + ")");
                case "symmetric_difference", "symmetricDifference", "op25b3", "△" ->
                        Optional.of("capy.setSymmetricDifference(" + receiver + ", " + other + ")");
                case "cartesian_product", "cartesianProduct", "opd7", "×" ->
                        Optional.of("capy.setCartesianProduct(" + receiver + ", " + other + ")");
                default -> Optional.empty();
            };
        }

        private Optional<String> renderNativeCollectionMethod(CompiledFunctionCall functionCall, List<String> args, String methodName) {
            var receiverType = functionCall.arguments().getFirst().type();
            var receiver = args.get(0);
            if ("get".equals(methodName)) {
                if (args.size() == 2) {
                    if (receiverType instanceof CompiledTupleType) {
                        return Optional.of("capy.rawIndex(" + receiver + ", " + args.get(1) + ")");
                    }
                    if (receiverType instanceof CollectionLinkedType.CompiledList
                        || receiverType instanceof CollectionLinkedType.CompiledDict
                        || receiverType == PrimitiveLinkedType.STRING) {
                        return Optional.of("capy.getIndex(" + receiver + ", " + args.get(1) + ")");
                    }
                    return Optional.empty();
                }
                if (args.size() == 3) {
                    if (receiverType instanceof CollectionLinkedType.CompiledList
                        || receiverType == PrimitiveLinkedType.STRING) {
                        return Optional.of("capy.slice(" + receiver + ", " + args.get(1) + ", " + args.get(2) + ")");
                    }
                    return Optional.empty();
                }
            }
            if (("_contains_native".equals(methodName) || "contains_native".equals(methodName))
                && receiverType instanceof CollectionLinkedType.CompiledSet
                && args.size() == 2) {
                return Optional.of("capy.contains(" + receiver + ", " + args.get(1) + ")");
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
                case PLUS -> "(" + renderCollectionPlus(expression, left, right) + ")";
                case MINUS -> "(" + renderCollectionMinus(expression, left, right) + ")";
                case MUL -> expression.type() == PrimitiveLinkedType.LONG
                        ? "capy.longMul(" + left + ", " + right + ")"
                        : "((" + left + ") * (" + right + "))";
                case DIV -> expression.type() == PrimitiveLinkedType.INT
                        ? "Math.trunc((" + left + ") / (" + right + "))"
                        : expression.type() == PrimitiveLinkedType.LONG
                                ? "capy.longDiv(" + left + ", " + right + ")"
                        : "((" + left + ") / (" + right + "))";
                case MOD -> expression.type() == PrimitiveLinkedType.LONG
                        ? "capy.longMod(" + left + ", " + right + ")"
                        : "((" + left + ") % (" + right + "))";
                case POWER -> expression.type() == PrimitiveLinkedType.LONG
                        ? "capy.longPow(" + left + ", " + right + ")"
                        : "Math.pow(" + left + ", " + right + ")";
                case GT, LT, LE, GE -> "((" + left + ") " + expression.operator().symbol() + " (" + right + "))";
                case EQUAL -> renderEquality(expression.left().type(), left, expression.right().type(), right, false);
                case NOTEQUAL -> renderEquality(expression.left().type(), left, expression.right().type(), right, true);
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

        private String renderEquality(CompiledType leftType, String left, CompiledType rightType, String right, boolean negated) {
            var equality = (leftType == PrimitiveLinkedType.BOOL && rightType != PrimitiveLinkedType.BOOL)
                    ? "(" + left + " === capy.truthy(" + right + "))"
                    : (rightType == PrimitiveLinkedType.BOOL && leftType != PrimitiveLinkedType.BOOL)
                            ? "(capy.truthy(" + left + ") === " + right + ")"
                            : "capy.equals(" + left + ", " + right + ")";
            return negated ? "(!" + equality + ")" : equality;
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
                        new RenderedPattern("capy.equals(" + value + ", " + renderLongLiteral(longPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.FloatPattern floatPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + stripNumericSuffix(floatPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.StringPattern stringPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + stringPattern.value() + ")", List.of(), scope);
                case CompiledMatchExpression.BoolPattern boolPattern ->
                        new RenderedPattern("capy.equals(" + value + ", " + boolPattern.value() + ")", List.of(), scope);
                case CompiledMatchExpression.WildcardPattern ignored ->
                        new RenderedPattern("true", List.of(), scope);
                case CompiledMatchExpression.VariablePattern variablePattern -> {
                    if (isTypeLikeIdentifier(variablePattern.name())) {
                        yield new RenderedPattern(
                                "capy.isType(" + value + ", " + jsString(typeNameReference(variablePattern.name())) + ")",
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
                            "capy.isType(" + value + ", " + jsString(typeNameReference(typedPattern.type().name())) + ")",
                            bound.bindings(),
                            bound.scope()
                    );
                }
                case CompiledMatchExpression.ConstructorPattern constructorPattern ->
                        renderConstructorPattern(value, constructorPattern, scope);
            };
        }

        private RenderedPattern renderConstructorPattern(String value, CompiledMatchExpression.ConstructorPattern pattern, Scope scope) {
            var constructorName = typeNameReference(pattern.constructorName());
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

        private String typeNameReference(String typeName) {
            return programContext.emittedTypeName(moduleInfo.className(), typeName);
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
            if (type instanceof CollectionLinkedType.CompiledSet) {
                return "capy.size(" + receiver + ")";
            }
            if (type instanceof CollectionLinkedType.CompiledDict) {
                return "(" + receiver + ").size";
            }
            return "(" + receiver + ").length";
        }

        private boolean isCollectionType(CompiledType type) {
            return type instanceof CollectionLinkedType.CompiledList
                   || type instanceof CollectionLinkedType.CompiledSet
                   || type instanceof CollectionLinkedType.CompiledDict
                   || type == PrimitiveLinkedType.STRING;
        }

        private boolean isNativePlusType(CompiledType type) {
            return isCollectionType(type)
                   || type == PrimitiveLinkedType.STRING
                   || type == PrimitiveLinkedType.INT
                   || type == PrimitiveLinkedType.LONG
                   || type == PrimitiveLinkedType.FLOAT
                   || type == PrimitiveLinkedType.DOUBLE;
        }

        private boolean isNativeMinusType(CompiledType type) {
            return isCollectionType(type)
                   || type == PrimitiveLinkedType.INT
                   || type == PrimitiveLinkedType.LONG
                   || type == PrimitiveLinkedType.FLOAT
                   || type == PrimitiveLinkedType.DOUBLE;
        }

        private boolean isPrimitiveConversion(String methodName) {
            return List.of("to_int", "to_long", "to_double", "to_float", "to_bool").contains(methodName);
        }

        private String renderContains(CompiledType leftType, String left, String right) {
            if (leftType instanceof CollectionLinkedType.CompiledDict) {
                return "(" + left + ").has(" + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledSet) {
                return "capy.contains(" + left + ", " + right + ")";
            }
            if (leftType == PrimitiveLinkedType.STRING) {
                return "String(" + left + ").includes(" + right + ")";
            }
            return "capy.contains(" + left + ", " + right + ")";
        }

        private String renderCollectionPlus(CompiledInfixExpression expression, String left, String right) {
            return renderCollectionPlus(expression.left().type(), left, expression.right().type(), right, expression.type());
        }

        private String renderCollectionPlus(CompiledType leftType, String left, CompiledType rightType, String right) {
            return renderCollectionPlus(leftType, left, rightType, right, null);
        }

        private String renderCollectionPlus(CompiledType leftType, String left, CompiledType rightType, String right, CompiledType resultType) {
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
            if (resultType == PrimitiveLinkedType.LONG) {
                return "capy.longAdd(" + left + ", " + right + ")";
            }
            return "((" + left + ") + (" + right + "))";
        }

        private String renderCollectionMinus(CompiledInfixExpression expression, String left, String right) {
            return renderCollectionMinus(expression.left().type(), left, expression.right().type(), right, expression.type());
        }

        private String renderCollectionMinus(CompiledType leftType, String left, CompiledType rightType, String right) {
            return renderCollectionMinus(leftType, left, rightType, right, null);
        }

        private String renderCollectionMinus(CompiledType leftType, String left, CompiledType rightType, String right, CompiledType resultType) {
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
            if (resultType == PrimitiveLinkedType.LONG) {
                return "capy.longSub(" + left + ", " + right + ")";
            }
            return "((" + left + ") - (" + right + "))";
        }

        private String renderConversion(String methodName, String receiver, CompiledType receiverType, CompiledType returnType) {
            return switch (methodName) {
                case "to_int" -> returnType instanceof GenericDataType
                        ? "capy.parseIntResult(" + receiver + ")"
                        : receiverType == PrimitiveLinkedType.LONG
                                ? "capy.longToInt(" + receiver + ")"
                                : "capy.floatToInt(" + receiver + ")";
                case "to_long" -> returnType instanceof GenericDataType
                        ? "capy.parseLongResult(" + receiver + ")"
                        : "capy.floatToLong(" + receiver + ")";
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
                var localTypeName = simpleTypeName(className);
                if (programContext.localTypeNames(moduleInfo.className()).contains(localTypeName)) {
                    return localTypeName + "." + emittedName;
                }
                var resolvedClassName = programContext.resolveClassName(className).orElse(className);
                require(resolvedClassName);
                return moduleVar(resolvedClassName) + "." + emittedName;
            }
            var owner = programContext.importedMemberOwner(moduleInfo.className(), emittedName)
                    .or(() -> programContext.importedMemberOwner(moduleInfo.className(), simpleMethodName(functionCall.name())));
            if (owner.isPresent()) {
                var className = programContext.resolveClassName(owner.orElseThrow()).orElse(owner.orElseThrow());
                require(className);
                return moduleVar(className) + "." + emittedName;
            }
            return emittedName;
        }

        private void require(String className) {
            var resolvedClassName = programContext.resolveClassName(className).orElse(className);
            if (resolvedClassName.equals(moduleInfo.className())) {
                return;
            }
            programContext.pathForClassName(resolvedClassName)
                    .ifPresent(path -> requiredModules.putIfAbsent(resolvedClassName, path));
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
                var className = programContext.resolveClassName(owner.orElseThrow()).orElse(owner.orElseThrow());
                require(className);
                return moduleVar(className) + "." + typeName;
            }
            var nestedOwner = ownerClassNameForNestedType(dataType.name());
            if (nestedOwner.isPresent()) {
                var className = nestedOwner.orElseThrow();
                require(className);
                return moduleVar(className) + "." + typeName;
            }
            var exportedOwner = programContext.exportedMemberOwner(typeName);
            if (exportedOwner.isPresent()) {
                var className = exportedOwner.orElseThrow();
                require(className);
                return moduleVar(className) + "." + typeName;
            }
            return typeName;
        }

        private Optional<String> ownerClassNameForNestedType(String typeName) {
            var stripped = typeName;
            var generic = stripped.indexOf('[');
            if (generic >= 0) {
                stripped = stripped.substring(0, generic);
            }
            var normalized = stripped.replace('\\', '.').replace('/', '.');
            while (normalized.startsWith(".")) {
                normalized = normalized.substring(1);
            }
            var idx = normalized.lastIndexOf('.');
            if (idx < 0) {
                return Optional.empty();
            }
            return programContext.resolveClassName(normalized.substring(0, idx));
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

    record ModuleInfo(CompiledModule module, JavaClass javaClass, String className, Path relativePath, String packageName, String packagePath) {
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

    record ProgramContext(
            Map<String, Path> pathsByClassName,
            Map<String, Set<String>> exportsByClassName,
            Map<String, Set<String>> localTypesByClassName,
            Map<String, Map<String, String>> importedOwnersByClassName,
            Map<String, List<String>> fieldsByType,
            Map<String, List<String>> parentTypesByType,
            Map<String, String> functionNameOverrides
    ) {
        static ProgramContext build(List<ModuleInfo> modules, Map<String, String> functionNameOverrides) {
            return build(modules, List.of(), functionNameOverrides);
        }

        static ProgramContext build(
                List<ModuleInfo> modules,
                List<dev.capylang.compiler.parser.ObjectOrientedModule> objectOrientedModules,
                Map<String, String> functionNameOverrides
        ) {
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
                    var name = simpleTypeName(record.name().toString());
                    moduleTypes.add(name);
                    fields.put(name, record.fields().stream().map(JavaRecord.JavaRecordField::name).toList());
                    if (!record.isPrivate()) {
                        moduleExports.add(name);
                    }
                    for (var iface : record.implementInterfaces()) {
                        parentTypes.computeIfAbsent(name, ignored -> new ArrayList<>()).add(simpleTypeName(iface.toString()));
                    }
                }
                for (var javaEnum : module.javaClass().enums()) {
                    var name = simpleTypeName(javaEnum.name().toString());
                    moduleTypes.add(name);
                    moduleExports.add(name);
                    if (javaEnum.values().isEmpty()) {
                        fields.putIfAbsent(name, List.of());
                    }
                    for (var iface : javaEnum.implementInterfaces()) {
                        parentTypes.computeIfAbsent(name, ignored -> new ArrayList<>()).add(simpleTypeName(iface.toString()));
                    }
                    for (var value : javaEnum.values()) {
                        var valueName = enumValueIdentifier(value);
                        moduleTypes.add(valueName);
                        moduleExports.add(valueName);
                        fields.putIfAbsent(valueName, List.of());
                        parentTypes.computeIfAbsent(valueName, ignored -> new ArrayList<>()).add(name);
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
            for (var module : objectOrientedModules) {
                var packageName = ObjectOrientedJavaScriptGenerator.packageName(module.path());
                var moduleTypes = module.objectOriented().definitions().stream()
                        .map(dev.capylang.compiler.parser.ObjectOriented.TypeDeclaration::name)
                        .map(JavaScriptGenerator::simpleTypeName)
                        .collect(java.util.stream.Collectors.toCollection(LinkedHashSet::new));
                for (var definition : module.objectOriented().definitions()) {
                    var typeName = simpleTypeName(definition.name());
                    var className = packageName.isBlank() ? typeName : packageName + "." + typeName;
                    paths.put(className, ObjectOrientedJavaScriptGenerator.relativePath(module, typeName));
                    exports.put(className, Set.of(typeName));
                    localTypes.put(className, Set.copyOf(moduleTypes));
                    fields.put(typeName, definition.members().stream()
                            .filter(dev.capylang.compiler.parser.ObjectOriented.FieldDeclaration.class::isInstance)
                            .map(dev.capylang.compiler.parser.ObjectOriented.FieldDeclaration.class::cast)
                            .map(dev.capylang.compiler.parser.ObjectOriented.FieldDeclaration::name)
                            .toList());
                    parentTypes.put(typeName, definition.parents().stream()
                            .map(dev.capylang.compiler.parser.ObjectOriented.TypeReference::name)
                            .map(JavaScriptGenerator::simpleTypeName)
                            .toList());
                }
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
                    var resolvedClassName = classNameCandidates(className).stream()
                            .filter(exports::containsKey)
                            .findFirst()
                            .orElse(className);
                    var memberName = staticImport.substring(idx + 1);
                    if ("*".equals(memberName)) {
                        exports.getOrDefault(resolvedClassName, Set.of()).forEach(member -> imported.putIfAbsent(member, resolvedClassName));
                    } else {
                        imported.putIfAbsent(memberName, resolvedClassName);
                        imported.putIfAbsent(normalizeJsIdentifier(memberName), resolvedClassName);
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

        Optional<String> classNameForModuleVariable(String moduleVariable) {
            return pathsByClassName.keySet().stream()
                    .filter(className -> moduleVar(className).equals(moduleVariable))
                    .findFirst();
        }

        Optional<String> exportedMemberOwner(String memberName) {
            var normalized = normalizeJsIdentifier(memberName);
            return exportsByClassName.entrySet().stream()
                    .filter(entry -> entry.getValue().contains(memberName) || entry.getValue().contains(normalized))
                    .map(Map.Entry::getKey)
                    .findFirst();
        }

        Set<String> localTypeNames(String className) {
            return localTypesByClassName.getOrDefault(className, Set.of());
        }

        List<String> fieldsForType(String typeName) {
            var direct = fieldsByType.get(typeName);
            if (direct != null) {
                return direct;
            }
            return fieldsByType.getOrDefault(simpleTypeName(typeName), List.of());
        }

        List<String> parentTypes(String typeName) {
            var direct = parentTypesByType.get(typeName);
            if (direct != null) {
                return direct;
            }
            return parentTypesByType.getOrDefault(simpleTypeName(typeName), List.of());
        }

        String emittedTypeName(String className, String typeName) {
            var raw = rawSimpleTypeName(typeName);
            var candidates = List.of(raw, enumValueIdentifier(raw), simpleTypeName(typeName));
            var localTypes = localTypeNames(className);
            for (var candidate : candidates) {
                if (localTypes.contains(candidate)) {
                    return candidate;
                }
            }
            for (var candidate : candidates) {
                if (importedMemberOwner(className, candidate).isPresent()) {
                    return candidate;
                }
            }
            for (var candidate : candidates) {
                if (exportedMemberOwner(candidate).isPresent()) {
                    return candidate;
                }
            }
            return simpleTypeName(typeName);
        }

        String emittedFunctionName(String name, List<CompiledType> parameterTypes) {
            var key = signatureKey(name, parameterTypes);
            if (functionNameOverrides.containsKey(key)) {
                return functionNameOverrides.get(key);
            }
            var parameterSignature = parameterTypes.stream().map(String::valueOf).collect(joining(","));
            var simple = simpleMethodName(name);
            if (simple.contains("__") && isValidJsIdentifier(simple)) {
                return simple;
            }
            var overrideBySimpleName = findOverrideBySimpleName(functionNameOverrides, name, parameterSignature);
            return overrideBySimpleName.orElseGet(() -> normalizeJsIdentifier(simple));
        }

        private static Map<String, Set<String>> runtimeExports() {
            var exports = new LinkedHashMap<String, Set<String>>();
            exports.put("capy.lang.Option", Set.of("Some", "None"));
            exports.put("capy.lang.Result", Set.of("Success", "Error"));
            exports.put("capy.lang.Effect", Set.of("pure", "delay"));
            exports.put("capy.lang.Program", Set.of("Success", "Failed"));
            exports.put("capy.lang.Primitives", Set.of("to_int", "to_long", "to_double", "to_float", "to_bool"));
            exports.put("capy.lang.String", Set.of("length", "get", "replace", "is_empty", "plus", "contains", "starts_with", "end_with", "trim"));
            exports.put("capy.lang.RegexModule", Set.of("fromLiteral"));
            exports.put("capy.lang.Seq", Set.of("to_seq", "toSeq"));
            exports.put("capy.lang.System", Set.of("current_millis", "currentMillis", "nano_time", "nanoTime"));
            exports.put("capy.lang.Math", Set.of("digits", "floor_div", "floorDiv", "floor_mod", "floorMod", "min", "max"));
            exports.put("capy.collection.List", Set.of("size", "get", "is_empty", "plus", "minus", "contains", "any", "all", "map", "filter", "reject", "flat_map", "flatMap", "reduce"));
            exports.put("capy.collection.Set", Set.of("size", "to_list", "is_empty", "plus", "minus", "contains", "any", "all", "map", "filter", "reject", "flat_map", "flatMap", "reduce"));
            exports.put("capy.collection.Dict", Set.of("size", "entries", "get", "is_empty", "plus", "minus", "contains_key", "any", "all", "map", "filter", "reject", "reduce"));
            exports.put("capy.collection.Tuple", Set.of("get"));
            exports.put("capy.io.Console", Set.of("print", "println", "print_error", "printError", "println_error", "printlnError", "read_line", "readLine"));
            exports.put("capy.io.Stdout", Set.of("print", "println"));
            exports.put("capy.io.IO", Set.of("read_text", "readText", "read_lines", "readLines", "read_bytes", "readBytes",
                    "write_text", "writeText", "write_lines", "writeLines", "write_bytes", "writeBytes",
                    "append_text", "appendText", "append_lines", "appendLines", "append_bytes", "appendBytes",
                    "exists", "is_file", "isFile", "is_directory", "isDirectory", "size", "create_file", "createFile",
                    "create_directory", "createDirectory", "create_directories", "createDirectories", "list_entries",
                    "listEntries", "delete", "delete_", "copy", "copy_replace", "copyReplace", "move", "move_replace", "moveReplace"));
            exports.put("capy.date_time.DateModule", Set.of("Date", "uNIXDATE", "UNIX_DATE", "fromDaysSinceUnixEpoch", "from_days_since_unix_epoch"));
            exports.put("capy.date_time.TimeModule", Set.of("Time", "mIDNIGHT", "MIDNIGHT"));
            exports.put("capy.date_time.DurationModule", Set.of("DateDuration", "WeekDuration", "zERO", "ZERO"));
            exports.put("capy.date_time.DateTimeModule", Set.of("DateTime", "uNIXEPOCH", "UNIX_EPOCH"));
            exports.put("capy.date_time.Clock", Set.of("now"));
            exports.put("capy.test.Assert", Set.of("assert_that", "assertThat"));
            exports.put("capy.test.CapyTest", Set.of("test", "test_file", "testFile"));
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
                    new GeneratedModule(Path.of("capy", "lang", "Program.js"), programRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "Primitives.js"), primitivesRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "String.js"), stringRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "RegexModule.js"), regexRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "Seq.js"), seqRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "System.js"), systemRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "Math.js"), mathRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "List.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Set.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Dict.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Tuple.js"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "Console.js"), consoleRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "Stdout.js"), stdoutRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "IO.js"), ioRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DateModule.js"), dateRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "TimeModule.js"), timeRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DurationModule.js"), durationRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DateTimeModule.js"), dateTimeRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "Clock.js"), clockRuntime()),
                    new GeneratedModule(Path.of("capy", "test", "Assert.js"), assertRuntime()),
                    new GeneratedModule(Path.of("capy", "test", "CapyTest.js"), capyTestRuntime())
            );
        }

        private static Set<String> classNames() {
            return Set.of(
                    "capy.lang.Option",
                    "capy.lang.Result",
                    "capy.lang.Effect",
                    "capy.lang.Program",
                    "capy.lang.Primitives",
                    "capy.lang.String",
                    "capy.lang.RegexModule",
                    "capy.lang.Seq",
                    "capy.lang.System",
                    "capy.lang.Math",
                    "capy.collection.List",
                    "capy.collection.Set",
                    "capy.collection.Dict",
                    "capy.collection.Tuple",
                    "capy.io.Console",
                    "capy.io.Stdout",
                    "capy.io.IO",
                    "capy.date_time.DateModule",
                    "capy.date_time.TimeModule",
                    "capy.date_time.DurationModule",
                    "capy.date_time.DateTimeModule",
                    "capy.date_time.Clock",
                    "capy.test.Assert",
                    "capy.test.CapyTest"
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
                   + "    toInt: capy.parseIntResult,\n"
                   + "    to_long: capy.parseLongResult,\n"
                   + "    toLong: capy.parseLongResult,\n"
                   + "    to_double: capy.parseFloatResult,\n"
                   + "    toDouble: capy.parseFloatResult,\n"
                   + "    to_float: capy.parseFloatResult,\n"
                   + "    toFloat: capy.parseFloatResult,\n"
                   + "    to_bool: capy.parseBoolResult,\n"
                   + "    toBool: capy.parseBoolResult,\n"
                   + "};\n";
        }

        private static String programRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    class Success {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Success';
                            this.__capybaraTypes = ['Success', 'Program'];
                            this.results = fields.results ?? [];
                        }
                        toString() { return capy.dataToString(this); }
                        capybaraDataValueInfo() { return capy.dataValueInfo(this, 'Success', 'capy.lang', 'capy/lang/Program'); }
                    }

                    class Failed {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Failed';
                            this.__capybaraTypes = ['Failed', 'Program'];
                            this.exitCode = fields.exitCode ?? 1;
                            this.errors = fields.errors ?? [];
                        }
                        toString() { return capy.dataToString(this); }
                        capybaraDataValueInfo() { return capy.dataValueInfo(this, 'Failed', 'capy.lang', 'capy/lang/Program'); }
                    }

                    module.exports = { Success, Failed };
                    """;
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
                   + "    starts_with: (value, part) => String(value).startsWith(part),\n"
                   + "    end_with: (value, part) => String(value).endsWith(part),\n"
                   + "    trim: value => String(value).trim(),\n"
                   + "};\n";
        }

        private static String collectionRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "module.exports = {\n"
                   + "    size: value => value instanceof Map || value instanceof Set ? value.size : value.length,\n"
                   + "    get: (value, start, end) => end === undefined ? capy.getIndex(value, start) : capy.slice(value, start, end),\n"
                   + "    entries: value => capy.list(value.entries()),\n"
                   + "    to_list: value => capy.list(value),\n"
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

        private static String regexRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    class CapyRegex {
                        constructor(pattern, flags = '') {
                            this.pattern = pattern;
                            this.flags = flags;
                            this.__capybaraRegex = true;
                        }
                        compile(extraFlags = '') {
                            const uniqueFlags = Array.from(new Set((this.flags + extraFlags).split(''))).join('');
                            return new RegExp(this.pattern, uniqueFlags);
                        }
                        matches(input) {
                            return this.compile().test(String(input));
                        }
                        find(input) {
                            const match = this.compile().exec(String(input));
                            return match ? new capy.Some({ value: match[0] }) : capy.None;
                        }
                        findAll(input) {
                            return capy.seq(Array.from(String(input).matchAll(this.compile('g')), match => match[0]));
                        }
                        replace(replacement) {
                            return input => String(input).replace(this.compile('g'), replacement);
                        }
                        split(input) {
                            return capy.seq(String(input).split(this.compile()));
                        }
                        tilde(input) {
                            return this.find(input);
                        }
                        tilde_tilde(input) {
                            return this.findAll(input);
                        }
                        tilde_greater(replacement) {
                            return this.replace(replacement);
                        }
                        slash_greater(input) {
                            return this.split(input);
                        }
                    }

                    function fromLiteral(pattern, flags = '') {
                        return new CapyRegex(pattern, flags);
                    }

                    module.exports = { fromLiteral };
                    """;
        }

        private static String seqRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');
                    const toSeq = values => capy.seq(values);
                    module.exports = {
                        toSeq,
                        to_seq: toSeq,
                    };
                    """;
        }

        private static String systemRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');
                    const currentMillis = () => capy.delay(() => capy.toLong(Date.now()));
                    const nanoTime = () => capy.delay(() => capy.toLong(process.hrtime.bigint()));
                    module.exports = {
                        currentMillis,
                        current_millis: currentMillis,
                        nanoTime,
                        nano_time: nanoTime,
                    };
                    """;
        }

        private static String mathRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');
                    function floorDiv(left, right) {
                        if (typeof left === 'bigint' || typeof right === 'bigint') {
                            const dividend = capy.toLong(left);
                            const divisor = capy.toLong(right);
                            const quotient = dividend / divisor;
                            const remainder = dividend % divisor;
                            return remainder !== 0n && ((dividend < 0n) !== (divisor < 0n)) ? quotient - 1n : quotient;
                        }
                        return Math.floor(left / right);
                    }
                    function floorMod(left, right) {
                        if (typeof left === 'bigint' || typeof right === 'bigint') {
                            const dividend = capy.toLong(left);
                            const divisor = capy.toLong(right);
                            return dividend - floorDiv(dividend, divisor) * divisor;
                        }
                        return left - floorDiv(left, right) * right;
                    }
                    function digits(value) {
                        if (typeof value === 'bigint') {
                            return String(value < 0n ? -value : value).length;
                        }
                        return String(Math.trunc(Math.abs(value))).length;
                    }
                    const min = (left, right) => left < right ? left : right;
                    const max = (left, right) => left > right ? left : right;
                    module.exports = {
                        digits,
                        floorDiv,
                        floor_div: floorDiv,
                        floorMod,
                        floor_mod: floorMod,
                        min,
                        max,
                    };
                    """;
        }

        private static String consoleRuntime() {
            return "'use strict';\n"
                   + "const fs = require('node:fs');\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "let stdinLines;\n"
                   + "let stdinOffset = 0;\n"
                   + "const consoleString = value => Array.isArray(value) && value.every(item => Number.isInteger(item) && item >= 0 && item <= 255) ? String.fromCharCode(...value) : capy.toStringValue(value);\n"
                   + "const printValue = value => { process.stdout.write(consoleString(value)); return value; };\n"
                   + "const printlnValue = value => { console.log(consoleString(value)); return value; };\n"
                   + "const printError = value => { process.stderr.write(consoleString(value)); return value; };\n"
                   + "const printlnError = value => { console.error(consoleString(value)); return value; };\n"
                   + "const readLineValue = () => {\n"
                   + "    if (stdinLines === undefined) {\n"
                   + "        const input = fs.readFileSync(0, 'utf8').replace(/\\r\\n/g, '\\n');\n"
                   + "        stdinLines = input.length === 0 ? [] : input.replace(/\\n$/, '').split('\\n');\n"
                   + "    }\n"
                   + "    return stdinOffset < stdinLines.length ? new capy.Some({ value: stdinLines[stdinOffset++] }) : capy.None;\n"
                   + "};\n"
                   + "module.exports = {\n"
                   + "    print: value => capy.delay(() => printValue(value)),\n"
                   + "    println: value => capy.delay(() => printlnValue(value)),\n"
                   + "    print_error: value => capy.delay(() => printError(value)),\n"
                   + "    printError: value => capy.delay(() => printError(value)),\n"
                   + "    println_error: value => capy.delay(() => printlnError(value)),\n"
                   + "    printlnError: value => capy.delay(() => printlnError(value)),\n"
                   + "    read_line: () => capy.delay(readLineValue),\n"
                   + "    readLine: () => capy.delay(readLineValue),\n"
                   + "};\n";
        }

        private static String stdoutRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "const consoleString = value => Array.isArray(value) && value.every(item => Number.isInteger(item) && item >= 0 && item <= 255) ? String.fromCharCode(...value) : capy.toStringValue(value);\n"
                   + "module.exports = {\n"
                   + "    print(value) { process.stdout.write(consoleString(value)); },\n"
                   + "    println(value) { console.log(consoleString(value)); },\n"
                   + "};\n";
        }

        private static String ioRuntime() {
            return """
                    'use strict';
                    const fs = require('node:fs');
                    const pathModule = require('node:path');
                    const capy = require('../../dev/capylang/capybara.js');

                    const pathText = path => String(path);
                    const success = value => new capy.Success({ value });
                    const failure = (operation, error) => new capy.Error({ message: `${operation} failed: ${error.message}` });
                    const effectResult = (operation, thunk) => capy.delay(() => {
                        try {
                            return success(thunk());
                        } catch (error) {
                            return failure(operation, error);
                        }
                    });
                    const writeParent = path => {
                        const parent = pathModule.dirname(path);
                        if (parent && parent !== '.') {
                            fs.mkdirSync(parent, { recursive: true });
                        }
                    };
                    const linesText = lines => lines.length === 0 ? '' : `${lines.join('\\n')}\\n`;
                    const readLinesText = text => {
                        if (text.length === 0) {
                            return [];
                        }
                        const normalized = text.replace(/\\r\\n/g, '\\n');
                        const lines = normalized.split('\\n');
                        if (lines.at(-1) === '') {
                            lines.pop();
                        }
                        return lines;
                    };

                    function readText(path) {
                        return effectResult('read_text', () => fs.readFileSync(pathText(path), 'utf8'));
                    }
                    function readLines(path) {
                        return effectResult('read_lines', () => readLinesText(fs.readFileSync(pathText(path), 'utf8')));
                    }
                    function readBytes(path) {
                        return effectResult('read_bytes', () => Array.from(fs.readFileSync(pathText(path))));
                    }
                    function writeText(path, text) {
                        return effectResult('write_text', () => {
                            const target = pathText(path);
                            writeParent(target);
                            fs.writeFileSync(target, String(text), 'utf8');
                            return text;
                        });
                    }
                    function writeLines(path, lines) {
                        return effectResult('write_lines', () => {
                            const target = pathText(path);
                            writeParent(target);
                            fs.writeFileSync(target, linesText(lines), 'utf8');
                            return lines;
                        });
                    }
                    function writeBytes(path, bytes) {
                        return effectResult('write_bytes', () => {
                            const target = pathText(path);
                            writeParent(target);
                            fs.writeFileSync(target, Buffer.from(bytes.map(value => value & 0xff)));
                            return bytes;
                        });
                    }
                    function appendText(path, text) {
                        return effectResult('append_text', () => {
                            const target = pathText(path);
                            writeParent(target);
                            fs.appendFileSync(target, String(text), 'utf8');
                            return text;
                        });
                    }
                    function appendLines(path, lines) {
                        return effectResult('append_lines', () => {
                            const target = pathText(path);
                            writeParent(target);
                            fs.appendFileSync(target, linesText(lines), 'utf8');
                            return lines;
                        });
                    }
                    function appendBytes(path, bytes) {
                        return effectResult('append_bytes', () => {
                            const target = pathText(path);
                            writeParent(target);
                            fs.appendFileSync(target, Buffer.from(bytes.map(value => value & 0xff)));
                            return bytes;
                        });
                    }
                    const exists = path => capy.delay(() => fs.existsSync(pathText(path)));
                    const isFile = path => capy.delay(() => fs.existsSync(pathText(path)) && fs.statSync(pathText(path)).isFile());
                    const isDirectory = path => capy.delay(() => fs.existsSync(pathText(path)) && fs.statSync(pathText(path)).isDirectory());
                    const size = path => effectResult('size', () => capy.toLong(fs.statSync(pathText(path)).size));
                    const createFile = path => effectResult('create_file', () => {
                        const target = pathText(path);
                        writeParent(target);
                        fs.closeSync(fs.openSync(target, 'a'));
                        return target;
                    });
                    const createDirectory = path => effectResult('create_directory', () => {
                        const target = pathText(path);
                        fs.mkdirSync(target);
                        return target;
                    });
                    const createDirectories = path => effectResult('create_directories', () => {
                        const target = pathText(path);
                        fs.mkdirSync(target, { recursive: true });
                        return target;
                    });
                    const listEntries = path => effectResult('list_entries', () => {
                        const root = pathText(path);
                        return fs.readdirSync(root).map(entry => pathModule.join(root, entry));
                    });
                    const delete_ = path => effectResult('delete', () => {
                        fs.rmSync(pathText(path), { recursive: true });
                        return true;
                    });
                    const copy = (source, target) => effectResult('copy', () => {
                        const destination = pathText(target);
                        writeParent(destination);
                        fs.copyFileSync(pathText(source), destination, fs.constants.COPYFILE_EXCL);
                        return destination;
                    });
                    const copyReplace = (source, target) => effectResult('copy_replace', () => {
                        const destination = pathText(target);
                        writeParent(destination);
                        fs.copyFileSync(pathText(source), destination);
                        return destination;
                    });
                    const move = (source, target) => effectResult('move', () => {
                        const destination = pathText(target);
                        if (fs.existsSync(destination)) {
                            throw new Error(`Target already exists: ${destination}`);
                        }
                        writeParent(destination);
                        fs.renameSync(pathText(source), destination);
                        return destination;
                    });
                    const moveReplace = (source, target) => effectResult('move_replace', () => {
                        const destination = pathText(target);
                        writeParent(destination);
                        fs.renameSync(pathText(source), destination);
                        return destination;
                    });

                    module.exports = {
                        readText,
                        read_text: readText,
                        readLines,
                        read_lines: readLines,
                        readBytes,
                        read_bytes: readBytes,
                        writeText,
                        write_text: writeText,
                        writeLines,
                        write_lines: writeLines,
                        writeBytes,
                        write_bytes: writeBytes,
                        appendText,
                        append_text: appendText,
                        appendLines,
                        append_lines: appendLines,
                        appendBytes,
                        append_bytes: appendBytes,
                        exists,
                        isFile,
                        is_file: isFile,
                        isDirectory,
                        is_directory: isDirectory,
                        size,
                        createFile,
                        create_file: createFile,
                        createDirectory,
                        create_directory: createDirectory,
                        createDirectories,
                        create_directories: createDirectories,
                        listEntries,
                        list_entries: listEntries,
                        delete_,
                        delete: delete_,
                        copy,
                        copyReplace,
                        copy_replace: copyReplace,
                        move,
                        moveReplace,
                        move_replace: moveReplace,
                    };
                    """;
        }

        private static String dateRuntime() {
            return """
                    'use strict';
                    const MILLIS_PER_DAY = 24 * 60 * 60 * 1000;

                    class CapyDate {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Date';
                            this.__capybaraTypes = ['Date'];
                            this.day = fields.day;
                            this.month = fields.month;
                            this.year = fields.year;
                        }
                        toDaysSinceUnixEpoch() {
                            return Math.trunc(Date.UTC(this.year, this.month - 1, this.day) / MILLIS_PER_DAY);
                        }
                        to_days_since_unix_epoch() {
                            return this.toDaysSinceUnixEpoch();
                        }
                        addDays(days) {
                            return fromDaysSinceUnixEpoch(this.toDaysSinceUnixEpoch() + days);
                        }
                        add_days(days) {
                            return this.addDays(days);
                        }
                        toIso8601() {
                            const year = String(this.year).padStart(4, '0');
                            return `${year}-${String(this.month).padStart(2, '0')}-${String(this.day).padStart(2, '0')}`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    function fromDaysSinceUnixEpoch(days) {
                        const date = new globalThis.Date(days * MILLIS_PER_DAY);
                        return new CapyDate({
                            day: date.getUTCDate(),
                            month: date.getUTCMonth() + 1,
                            year: date.getUTCFullYear(),
                        });
                    }

                    const uNIXDATE = new CapyDate({ day: 1, month: 1, year: 1970 });
                    const UNIX_DATE = uNIXDATE;

                    module.exports = {
                        Date: CapyDate,
                        uNIXDATE,
                        UNIX_DATE,
                        fromDaysSinceUnixEpoch,
                        from_days_since_unix_epoch: fromDaysSinceUnixEpoch,
                    };
                    """;
        }

        private static String timeRuntime() {
            return """
                    'use strict';
                    const SECONDS_IN_DAY = 24 * 60 * 60;

                    class Time {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Time';
                            this.__capybaraTypes = ['Time'];
                            this.hour = fields.hour;
                            this.minute = fields.minute;
                            this.second = fields.second;
                            this.offset_minutes = fields.offset_minutes;
                        }
                        toSeconds() {
                            return this.hour * 3600 + this.minute * 60 + this.second;
                        }
                        to_seconds() {
                            return this.toSeconds();
                        }
                        addSeconds(seconds) {
                            const total = ((this.toSeconds() + seconds) % SECONDS_IN_DAY + SECONDS_IN_DAY) % SECONDS_IN_DAY;
                            return new Time({
                                hour: Math.trunc(total / 3600),
                                minute: Math.trunc((total % 3600) / 60),
                                second: total % 60,
                                offset_minutes: this.offset_minutes,
                            });
                        }
                        add_seconds(seconds) {
                            return this.addSeconds(seconds);
                        }
                        toIso8601() {
                            const body = `${String(this.hour).padStart(2, '0')}:${String(this.minute).padStart(2, '0')}:${String(this.second).padStart(2, '0')}`;
                            if (this.offset_minutes && this.offset_minutes.__capybaraType === 'Some') {
                                const offset = this.offset_minutes.value;
                                if (offset === 0) {
                                    return `${body}Z`;
                                }
                                const sign = offset < 0 ? '-' : '+';
                                const absolute = Math.abs(offset);
                                return `${body}${sign}${String(Math.trunc(absolute / 60)).padStart(2, '0')}:${String(absolute % 60).padStart(2, '0')}`;
                            }
                            return body;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    const mIDNIGHT = new Time({ hour: 0, minute: 0, second: 0 });
                    const MIDNIGHT = mIDNIGHT;

                    module.exports = {
                        Time,
                        mIDNIGHT,
                        MIDNIGHT,
                    };
                    """;
        }

        private static String durationRuntime() {
            return """
                    'use strict';

                    class DateDuration {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateDuration';
                            this.__capybaraTypes = ['DateDuration', 'Duration'];
                            this.years = fields.years ?? 0;
                            this.months = fields.months ?? 0;
                            this.days = fields.days ?? 0;
                            this.hours = fields.hours ?? 0;
                            this.minutes = fields.minutes ?? 0;
                            this.seconds = fields.seconds ?? 0;
                        }
                        negate() {
                            return new DateDuration({
                                years: -this.years,
                                months: -this.months,
                                days: -this.days,
                                hours: -this.hours,
                                minutes: -this.minutes,
                                seconds: -this.seconds,
                            });
                        }
                        totalSeconds() {
                            return this.days * 86400 + this.hours * 3600 + this.minutes * 60 + this.seconds;
                        }
                        toIso8601() {
                            if (this.years === 0 && this.months === 0 && this.days === 0 && this.hours === 0 && this.minutes === 0 && this.seconds === 0) {
                                return 'PT0S';
                            }
                            const datePart = `${this.years !== 0 ? `${this.years}Y` : ''}${this.months !== 0 ? `${this.months}M` : ''}${this.days !== 0 ? `${this.days}D` : ''}`;
                            const timePart = `${this.hours !== 0 ? `${this.hours}H` : ''}${this.minutes !== 0 ? `${this.minutes}M` : ''}${this.seconds !== 0 ? `${this.seconds}S` : ''}`;
                            return `P${datePart}${timePart ? `T${timePart}` : ''}`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    class WeekDuration {
                        constructor(fields = {}) {
                            this.__capybaraType = 'WeekDuration';
                            this.__capybaraTypes = ['WeekDuration', 'Duration'];
                            this.weeks = fields.weeks ?? 0;
                        }
                        negate() {
                            return new WeekDuration({ weeks: -this.weeks });
                        }
                        totalSeconds() {
                            return this.weeks * 7 * 86400;
                        }
                        toIso8601() {
                            return `P${this.weeks}W`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    const zERO = new DateDuration({ years: 0, months: 0, days: 0, hours: 0, minutes: 0, seconds: 0 });
                    const ZERO = zERO;

                    module.exports = {
                        DateDuration,
                        WeekDuration,
                        zERO,
                        ZERO,
                    };
                    """;
        }

        private static String dateTimeRuntime() {
            return """
                    'use strict';
                    const DateModule = require('./DateModule.js');
                    const TimeModule = require('./TimeModule.js');
                    const DurationModule = require('./DurationModule.js');

                    class DateTime {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateTime';
                            this.__capybaraTypes = ['DateTime'];
                            this.date = fields.date;
                            this.time = fields.time;
                        }
                        timestamp() {
                            return BigInt(this.date.toDaysSinceUnixEpoch() * 86400 + this.time.toSeconds());
                        }
                        plus(duration) {
                            return DateTime.fromTimestamp(Number(this.timestamp()) + duration.totalSeconds());
                        }
                        minus(value) {
                            if (value && value.__capybaraType === 'DateTime') {
                                const delta = Number(this.timestamp() - value.timestamp());
                                const sign = delta < 0 ? -1 : 1;
                                let remaining = Math.abs(delta);
                                const days = Math.trunc(remaining / 86400);
                                remaining %= 86400;
                                const hours = Math.trunc(remaining / 3600);
                                remaining %= 3600;
                                const minutes = Math.trunc(remaining / 60);
                                const seconds = remaining % 60;
                                return new DurationModule.DateDuration({
                                    years: 0,
                                    months: 0,
                                    days: sign * days,
                                    hours: sign * hours,
                                    minutes: sign * minutes,
                                    seconds: sign * seconds,
                                });
                            }
                            return this.plus(value.negate());
                        }
                        toIso8601() {
                            return `${this.date.toIso8601()}T${this.time.toIso8601()}`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                        static fromTimestamp(timestamp) {
                            const days = Math.floor(timestamp / 86400);
                            const seconds = ((timestamp % 86400) + 86400) % 86400;
                            return new DateTime({
                                date: DateModule.fromDaysSinceUnixEpoch(days),
                                time: TimeModule.mIDNIGHT.addSeconds(seconds),
                            });
                        }
                    }

                    const uNIXEPOCH = new DateTime({ date: DateModule.uNIXDATE, time: TimeModule.mIDNIGHT });
                    const UNIX_EPOCH = uNIXEPOCH;

                    module.exports = {
                        DateTime,
                        uNIXEPOCH,
                        UNIX_EPOCH,
                    };
                    """;
        }

        private static String clockRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');
                    const DateModule = require('./DateModule.js');
                    const TimeModule = require('./TimeModule.js');
                    const DateTimeModule = require('./DateTimeModule.js');

                    function now() {
                        return capy.delay(() => {
                            const current = new Date();
                            return new DateTimeModule.DateTime({
                                date: new DateModule.Date({
                                    day: current.getUTCDate(),
                                    month: current.getUTCMonth() + 1,
                                    year: current.getUTCFullYear(),
                                }),
                                time: new TimeModule.Time({
                                    hour: current.getUTCHours(),
                                    minute: current.getUTCMinutes(),
                                    second: current.getUTCSeconds(),
                                    offset_minutes: new capy.Some({ value: 0 }),
                                }),
                            });
                        });
                    }

                    module.exports = { now };
                    """;
        }

        private static String assertRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    class AssertionResult {
                        constructor(value, message = '') {
                            this.value = value;
                            this.message = message;
                        }
                        succeeded() {
                            return this.value;
                        }
                    }

                    function assertThat(value) {
                        return {
                            isEqualTo(expected) {
                                return new AssertionResult(capy.equals(value, expected), `Expected ${capy.toStringValue(value)} to equal ${capy.toStringValue(expected)}`);
                            },
                            succeeds() {
                                return new AssertionResult(capy.isType(value, 'Success'), 'Expected Result.Success');
                            },
                            fails() {
                                return new AssertionResult(capy.isType(value, 'Error'), 'Expected Result.Error');
                            },
                        };
                    }

                    module.exports = {
                        assertThat,
                        assert_that: assertThat,
                    };
                    """;
        }

        private static String capyTestRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    function test(name, body) {
                        return { name, body };
                    }

                    function testFile(path, testCases) {
                        return capy.delay(() => ({
                            path,
                            test_cases: testCases,
                        }));
                    }

                    module.exports = {
                        test,
                        testFile,
                        test_file: testFile,
                    };
                    """;
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
                        map(mapper) { return optionMap(this, mapper); }
                        pipe(mapper) { return optionMap(this, mapper); }
                        flat_map(mapper) { return optionFlatMap(this, mapper); }
                        flatMap(mapper) { return optionFlatMap(this, mapper); }
                        pipe_star(mapper) { return optionFlatMap(this, mapper); }
                        filter(predicate) { return predicate(this.value) ? None : this; }
                        reduce(initial, reducer) { return invoke(reducer, initial, this.value); }
                        reduceLeft(initial, reducer) { return this.reduce(initial, reducer); }
                        toString() { return dataToString(this); }
                        capybaraDataValueInfo() { return dataValueInfo(this, 'Some', 'capy.lang', 'capy/lang/Option'); }
                    }

                    const None = Object.freeze({
                        __capybaraType: 'None',
                        __capybaraTypes: ['None', 'Option'],
                        map() { return this; },
                        pipe() { return this; },
                        flat_map() { return this; },
                        flatMap() { return this; },
                        pipe_star() { return this; },
                        filter() { return this; },
                        reduce(initial) { return initial; },
                        reduceLeft(initial) { return initial; },
                        toString() { return 'None { }'; },
                        capybaraDataValueInfo() { return dataValueInfo(this, 'None', 'capy.lang', 'capy/lang/Option'); },
                    });

                    class Success {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Success';
                            this.__capybaraTypes = ['Success', 'Result'];
                            this.value = fields.value;
                        }
                        map(mapper) { return invoke(mapper, this.value); }
                        pipe(mapper) { return invoke(mapper, this.value); }
                        flat_map(mapper) { const mapped = invoke(mapper, this.value); return isSuccessLike(mapped) ? mapped.value : mapped; }
                        flatMap(mapper) { return this.flat_map(mapper); }
                        pipe_star(mapper) { return this.flat_map(mapper); }
                        orElse() { return this.value; }
                        or_else() { return this.value; }
                        or() { return this; }
                        reduce(successMapper) { return invoke(successMapper, this.value); }
                        reduceLeft(successMapper) { return this.reduce(successMapper); }
                        pipe_greater(args) { return this.reduce(args[0], args[1]); }
                        toString() { return dataToString(this); }
                        capybaraDataValueInfo() { return dataValueInfo(this, 'Success', 'capy.lang', 'capy/lang/Result'); }
                    }

                    class ErrorValue {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Error';
                            this.__capybaraTypes = ['Error', 'Result'];
                            this.message = fields.message;
                        }
                        map() { return this; }
                        pipe() { return this; }
                        flat_map() { return this; }
                        flatMap() { return this; }
                        pipe_star() { return this; }
                        orElse(value) { return value; }
                        or_else(value) { return value; }
                        or(result) { return result; }
                        reduce(successMapper, errorMapper) { return invoke(errorMapper, this.message); }
                        reduceLeft(successMapper, errorMapper) { return this.reduce(successMapper, errorMapper); }
                        pipe_greater(args) { return this.reduce(args[0], args[1]); }
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
                            if (typeof result !== 'function') {
                                return result;
                            }
                            result = result(arg);
                        }
                        return result;
                    }

                    function isSuccessLike(value) {
                        const type = String(value?.__capybaraType ?? '').toLowerCase();
                        return Object.prototype.hasOwnProperty.call(value ?? {}, 'value')
                            && (type.includes('success') || type.includes('ok'));
                    }

                    function isFailureLike(value) {
                        const type = String(value?.__capybaraType ?? '').toLowerCase();
                        return type.includes('error') || type.includes('fail');
                    }

                    function resultLikePipe(value, mapper) {
                        if (isFailureLike(value)) {
                            return value;
                        }
                        if (Object.prototype.hasOwnProperty.call(value ?? {}, 'value')) {
                            return invoke(mapper, value.value);
                        }
                        return value;
                    }

                    function resultLikeFlatMap(value, mapper) {
                        const mapped = resultLikePipe(value, mapper);
                        return isSuccessLike(mapped) ? mapped.value : mapped;
                    }

                    const arrayMethods = {
                        asList: { value() { return Array.from(this); } },
                        first: { value() { return this.length === 0 ? None : new Some({ value: this[0] }); } },
                        map: { value(mapper) { return list(Array.prototype.map.call(this, mapper)); } },
                        flatMap: { value(mapper) { return flatMapCollection(this, mapper); } },
                        flat_map: { value(mapper) { return flatMapCollection(this, mapper); } },
                        filter: { value(predicate) { return list(Array.prototype.filter.call(this, predicate)); } },
                        reject: { value(predicate) { return rejectCollection(this, predicate); } },
                        pipe: { value(mapper) { return mapCollection(this, mapper); } },
                        pipe_minus: { value(predicate) { return filterCollection(this, predicate); } },
                        pipe_star: { value(mapper) { return flatMapCollection(this, mapper); } },
                        reduce: { value(first, second) { return typeof first === 'function' ? Array.prototype.reduce.apply(this, arguments) : reduceCollection(this, first, second); } },
                        reduceLeft: { value(initial, reducer) { return reduceCollection(this, initial, reducer); } },
                        pipe_greater: { value(initial, reducer) { return reduceCollection(this, initial, reducer); } },
                    };

                    const setMethods = {
                        asList: { value() { return Array.from(this); } },
                        first: { value() { const values = Array.from(this); return values.length === 0 ? None : new Some({ value: values[0] }); } },
                        pipe: { value(mapper) { return mapCollection(this, mapper); } },
                        pipe_minus: { value(predicate) { return filterCollection(this, predicate); } },
                        pipe_star: { value(mapper) { return flatMapCollection(this, mapper); } },
                        reduceLeft: { value(initial, reducer) { return reduceCollection(this, initial, reducer); } },
                        pipe_greater: { value(initial, reducer) { return reduceCollection(this, initial, reducer); } },
                        isSubsetOf: { value(other) { return setIsSubsetOf(this, other); } },
                        op2286: { value(other) { return setIsSubsetOf(this, other); } },
                        isProperSubsetOf: { value(other) { return setIsProperSubsetOf(this, other); } },
                        op2282: { value(other) { return setIsProperSubsetOf(this, other); } },
                        isSupersetOf: { value(other) { return setIsSupersetOf(this, other); } },
                        op2287: { value(other) { return setIsSupersetOf(this, other); } },
                        isProperSupersetOf: { value(other) { return setIsProperSupersetOf(this, other); } },
                        op2283: { value(other) { return setIsProperSupersetOf(this, other); } },
                        union: { value(other) { return setPlus(this, other); } },
                        op222a: { value(other) { return setPlus(this, other); } },
                        intersection: { value(other) { return setIntersection(this, other); } },
                        op2229: { value(other) { return setIntersection(this, other); } },
                        difference: { value(other) { return setMinus(this, other); } },
                        symmetricDifference: { value(other) { return setSymmetricDifference(this, other); } },
                        op25b3: { value(other) { return setSymmetricDifference(this, other); } },
                        cartesianProduct: { value(other) { return setCartesianProduct(this, other); } },
                        opd7: { value(other) { return setCartesianProduct(this, other); } },
                        powerSet: { value() { return setPowerSet(this); } },
                        op2118: { value() { return setPowerSet(this); } },
                    };

                    function defineCapyMethods(target, methods) {
                        for (const [name, descriptor] of Object.entries(methods)) {
                            if (!Object.prototype.hasOwnProperty.call(target, name)) {
                                Object.defineProperty(target, name, { ...descriptor, enumerable: false, configurable: true });
                            }
                        }
                        return target;
                    }

                    function size(value) {
                        if (value instanceof Set) {
                            return set(value).size;
                        }
                        if (value instanceof Map) {
                            return value.size;
                        }
                        return value == null ? 0 : value.length;
                    }

                    function newArray(length, defaultValue = undefined) {
                        return Array.from({ length }, () => defaultValue);
                    }

                    function exceptionClass(name) {
                        return { getSimpleName: () => name };
                    }

                    function decorateException(error, className = error && error.name ? error.name : 'Error') {
                        if (!error || typeof error !== 'object') {
                            return decorateException(new Error(String(error)), 'RuntimeException');
                        }
                        if (typeof error.getMessage !== 'function') {
                            Object.defineProperty(error, 'getMessage', {
                                value() { return this.message ?? String(this); },
                                configurable: true,
                            });
                        }
                        if (typeof error.getClass !== 'function') {
                            Object.defineProperty(error, 'getClass', {
                                value() { return exceptionClass(className); },
                                configurable: true,
                            });
                        }
                        return error;
                    }

                    function toException(value) {
                        if (value instanceof Error) {
                            return decorateException(value);
                        }
                        return decorateException(new Error(String(value)), 'RuntimeException');
                    }

                    function arrayIndexError(index) {
                        return decorateException(new Error(String(index)), 'ArrayIndexOutOfBoundsException');
                    }

                    function arrayGet(value, index) {
                        const normalized = index < 0 && value != null ? value.length + index : index;
                        if (value == null || normalized < 0 || normalized >= value.length) {
                            throw arrayIndexError(index);
                        }
                        return value[normalized];
                    }

                    function applyTrait(targetClass, traitClass) {
                        for (const name of Object.getOwnPropertyNames(traitClass.prototype)) {
                            if (name === 'constructor' || Object.prototype.hasOwnProperty.call(targetClass.prototype, name)) {
                                continue;
                            }
                            Object.defineProperty(targetClass.prototype, name, Object.getOwnPropertyDescriptor(traitClass.prototype, name));
                        }
                    }

                    function list(values = []) {
                        return defineCapyMethods(Array.from(values), arrayMethods);
                    }

                    function setAdd(valueSet, value) {
                        if (!contains(valueSet, value)) {
                            valueSet.add(value);
                        }
                        return valueSet;
                    }

                    function set(values = []) {
                        const result = defineCapyMethods(new Set(), setMethods);
                        for (const value of values ?? []) {
                            setAdd(result, value);
                        }
                        return result;
                    }

                    function seq(values, mapper) {
                        return list(mapper === undefined ? values ?? [] : Array.from(values ?? [], mapper));
                    }

                    function enumValue(name, owner, parents = [], ordinal = 0, aliases = [], packageName = '', packagePath = owner) {
                        return Object.freeze({
                            __capybaraType: name,
                            __capybaraTypes: [name, ...aliases, owner, ...parents],
                            __capybaraEnum: true,
                            name,
                            ordinal,
                            order: ordinal,
                            toString() { return name; },
                            capybaraDataValueInfo() { return { name, packageName, packagePath, fields: [] }; },
                        });
                    }

                    function isType(value, typeName) {
                        switch (typeName) {
                            case 'BYTE':
                            case 'INT':
                                return Number.isInteger(value);
                            case 'LONG':
                                return typeof value === 'bigint';
                            case 'FLOAT':
                            case 'DOUBLE':
                                return typeof value === 'number';
                            case 'STRING':
                                return typeof value === 'string';
                            case 'BOOL':
                                return typeof value === 'boolean';
                            case 'List':
                                return Array.isArray(value);
                            case 'Set':
                                return value instanceof Set;
                            case 'Dict':
                                return value instanceof Map;
                            case 'DATA':
                                return Boolean(value && Array.isArray(value.__capybaraTypes));
                            case 'ENUM':
                                return Boolean(value && value.__capybaraEnum);
                            default:
                                break;
                        }
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
                        const result = value.slice(from, to);
                        return Array.isArray(result) ? list(result) : result;
                    }

                    function equals(left, right) {
                        if (Object.is(left, right)) {
                            return true;
                        }
                        if (typeof left === 'bigint' && typeof right === 'number') {
                            return Number.isInteger(right) && left === BigInt(right);
                        }
                        if (typeof left === 'number' && typeof right === 'bigint') {
                            return Number.isInteger(left) && BigInt(left) === right;
                        }
                        if (Array.isArray(left) && Array.isArray(right)) {
                            return left.length === right.length && left.every((value, index) => equals(value, right[index]));
                        }
                        if (left instanceof Set && right instanceof Set) {
                            const normalizedLeft = set(left);
                            const normalizedRight = set(right);
                            return normalizedLeft.size === normalizedRight.size && Array.from(normalizedLeft).every(value => contains(normalizedRight, value));
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
                        if (collection && collection.__capybaraRegex && typeof collection.matches === 'function') {
                            return collection.matches(value);
                        }
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
                        if (typeof value === 'bigint') {
                            return value !== 0n;
                        }
                        if (typeof value === 'string' || Array.isArray(value)) {
                            return value.length > 0;
                        }
                        if (value instanceof Set || value instanceof Map) {
                            return value.size > 0;
                        }
                        return true;
                    }

                    function listAppend(valueList, value) {
                        return list([...valueList, value]);
                    }

                    function listPlus(left, right) {
                        return list([...left, ...right]);
                    }

                    function listRemove(valueList, value) {
                        return list(valueList.filter(item => !equals(item, value)));
                    }

                    function listMinus(left, right) {
                        return list(left.filter(item => !contains(right, item)));
                    }

                    function setAppend(valueSet, value) {
                        const result = set(valueSet);
                        return setAdd(result, value);
                    }

                    function setPlus(left, right) {
                        const result = set(left);
                        for (const value of right) {
                            setAdd(result, value);
                        }
                        return result;
                    }

                    function setRemove(valueSet, value) {
                        return set(Array.from(valueSet).filter(item => !equals(item, value)));
                    }

                    function setMinus(left, right) {
                        return set(Array.from(left).filter(item => !contains(right, item)));
                    }

                    function setIsSubsetOf(left, right) {
                        const normalizedLeft = set(left);
                        const normalizedRight = set(right);
                        return Array.from(normalizedLeft).every(item => contains(normalizedRight, item));
                    }

                    function setIsProperSubsetOf(left, right) {
                        return setIsSubsetOf(left, right) && set(left).size < set(right).size;
                    }

                    function setIsSupersetOf(left, right) {
                        return setIsSubsetOf(right, left);
                    }

                    function setIsProperSupersetOf(left, right) {
                        return setIsSubsetOf(right, left) && set(left).size > set(right).size;
                    }

                    function setIntersection(left, right) {
                        return set(Array.from(left).filter(item => contains(right, item)));
                    }

                    function setSymmetricDifference(left, right) {
                        return setPlus(setMinus(left, right), setMinus(right, left));
                    }

                    function setCartesianProduct(left, right) {
                        const normalizedLeft = set(left);
                        const normalizedRight = set(right);
                        return set(Array.from(normalizedLeft).flatMap(l => Array.from(normalizedRight).map(r => [l, r])));
                    }

                    function setPowerSet(valueSet) {
                        const values = Array.from(set(valueSet));
                        return set(values.reduce(
                            (subsets, item) => subsets.concat(subsets.map(subset => set([...subset, item]))),
                            [set()]
                        ));
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
                        if (isType(value, 'Some')) {
                            return [value.value];
                        }
                        if (isType(value, 'None') || isType(value, 'Error')) {
                            return [];
                        }
                        if (isType(value, 'Success')) {
                            return [value.value];
                        }
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
                        if (isType(value, 'Some')) {
                            return optionMap(value, mapper);
                        }
                        if (isType(value, 'None')) {
                            return value;
                        }
                        if (isType(value, 'Success')) {
                            return invoke(mapper, value.value);
                        }
                        if (isType(value, 'Error')) {
                            return value;
                        }
                        if (value instanceof Map) {
                            return new Map(Array.from(value.entries()).map(([key, item]) => [key, invoke(mapper, key, item)]));
                        }
                        if (value instanceof Set) {
                            return set(Array.from(value).map((item, index) => invoke(mapper, item, index)));
                        }
                        if (typeof value === 'string') {
                            return list(Array.from(value).map((item, index) => invoke(mapper, item, index)));
                        }
                        return list(value.map((item, index) => invoke(mapper, item, index)));
                    }

                    function filterCollection(value, predicate) {
                        if (isType(value, 'Some')) {
                            return optionFilterOut(value, predicate);
                        }
                        if (isType(value, 'None') || isType(value, 'Error')) {
                            return value;
                        }
                        if (isType(value, 'Success')) {
                            return invoke(predicate, value.value) ? value : new ErrorValue({ message: 'Filtered out' });
                        }
                        if (value instanceof Map) {
                            return new Map(Array.from(value.entries()).filter(([key, item]) => invoke(predicate, key, item)));
                        }
                        if (value instanceof Set) {
                            return set(Array.from(value).filter((item, index) => invoke(predicate, item, index)));
                        }
                        if (typeof value === 'string') {
                            return list(Array.from(value).filter((item, index) => invoke(predicate, item, index)));
                        }
                        return list(value.filter((item, index) => invoke(predicate, item, index)));
                    }

                    function rejectCollection(value, predicate) {
                        if (isType(value, 'Some')) {
                            return optionFilterOut(value, predicate);
                        }
                        if (isType(value, 'None') || isType(value, 'Error')) {
                            return value;
                        }
                        if (isType(value, 'Success')) {
                            return invoke(predicate, value.value) ? new ErrorValue({ message: 'Rejected' }) : value;
                        }
                        if (value instanceof Map) {
                            return new Map(Array.from(value.entries()).filter(([key, item]) => !invoke(predicate, key, item)));
                        }
                        if (value instanceof Set) {
                            return set(Array.from(value).filter((item, index) => !invoke(predicate, item, index)));
                        }
                        if (typeof value === 'string') {
                            return list(Array.from(value).filter((item, index) => !invoke(predicate, item, index)));
                        }
                        return list(value.filter((item, index) => !invoke(predicate, item, index)));
                    }

                    function flatMapCollection(value, mapper) {
                        if (isType(value, 'Some')) {
                            return optionFlatMap(value, mapper);
                        }
                        if (isType(value, 'None') || isType(value, 'Error')) {
                            return value;
                        }
                        if (isType(value, 'Success')) {
                            return invoke(mapper, value.value);
                        }
                        const mapped = entries(value).flatMap((item, index) => {
                            const result = invoke(mapper, item, index);
                            if (Array.isArray(result)) {
                                return result;
                            }
                            if (result instanceof Set) {
                                return Array.from(result);
                            }
                            return [result];
                        });
                        return list(mapped);
                    }

                    function any(value, predicate) {
                        if (value instanceof Map) {
                            return Array.from(value.entries()).some(([key, item]) => invoke(predicate, key, item));
                        }
                        return entries(value).some((item, index) => invoke(predicate, item, index));
                    }

                    function all(value, predicate) {
                        if (value instanceof Map) {
                            return Array.from(value.entries()).every(([key, item]) => invoke(predicate, key, item));
                        }
                        return entries(value).every((item, index) => invoke(predicate, item, index));
                    }

                    function reduceCollection(value, initial, reducer) {
                        let acc = initial;
                        if (value instanceof Map) {
                            for (const [key, item] of value.entries()) {
                                acc = invoke(reducer, acc, key, item);
                            }
                            return acc;
                        }
                        for (const [index, item] of entries(value).entries()) {
                            acc = invoke(reducer, acc, item, index);
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

                    const LONG_MIN = -9223372036854775808n;
                    const LONG_MAX = 9223372036854775807n;

                    function toLong(value) {
                        return BigInt.asIntN(64, BigInt(value));
                    }

                    function longAdd(left, right) {
                        return toLong(BigInt(left) + BigInt(right));
                    }

                    function longSub(left, right) {
                        return toLong(BigInt(left) - BigInt(right));
                    }

                    function longMul(left, right) {
                        return toLong(BigInt(left) * BigInt(right));
                    }

                    function longDiv(left, right) {
                        return toLong(BigInt(left) / BigInt(right));
                    }

                    function longMod(left, right) {
                        return toLong(BigInt(left) % BigInt(right));
                    }

                    function longPow(left, right) {
                        return floatToLong(Math.pow(Number(left), Number(right)));
                    }

                    function floatToInt(value) {
                        const number = Number(value);
                        if (Number.isNaN(number)) {
                            return 0;
                        }
                        if (number >= 2147483647) {
                            return 2147483647;
                        }
                        if (number <= -2147483648) {
                            return -2147483648;
                        }
                        return Math.trunc(number);
                    }

                    function longToInt(value) {
                        return Number(BigInt.asIntN(32, BigInt(value)));
                    }

                    function floatToLong(value) {
                        const number = Number(value);
                        if (Number.isNaN(number)) {
                            return 0n;
                        }
                        if (number >= 9223372036854775807) {
                            return LONG_MAX;
                        }
                        if (number <= -9223372036854775808) {
                            return LONG_MIN;
                        }
                        return BigInt(Math.trunc(number));
                    }

                    function parseResult(value, typeName, parser) {
                        try {
                            const text = String(value);
                            const parsed = parser(text);
                            if (Number.isNaN(parsed)) {
                                return new ErrorValue({ message: `Cannot parse string to ${typeName}: ${value}` });
                            }
                            return new Success({ value: parsed });
                        } catch (error) {
                            return new ErrorValue({ message: `Cannot parse string to ${typeName}: ${value}` });
                        }
                    }

                    function parseIntResult(value) {
                        return parseResult(value, 'int', text => /^[-+]?\\d+$/.test(text) ? Number.parseInt(text, 10) : Number.NaN);
                    }

                    function parseLongResult(value) {
                        return parseResult(value, 'long', text => {
                            if (!/^[-+]?\\d+$/.test(text)) {
                                return Number.NaN;
                            }
                            const parsed = BigInt(text);
                            return parsed < LONG_MIN || parsed > LONG_MAX ? Number.NaN : parsed;
                        });
                    }

                    function parseFloatResult(value) {
                        return parseResult(value, 'float', text => /^[-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?$/.test(text) ? Number.parseFloat(text) : Number.NaN);
                    }

                    function parseBoolResult(value) {
                        const text = String(value);
                        if (text === 'true') {
                            return new Success({ value: true });
                        }
                        if (text === 'false') {
                            return new Success({ value: false });
                        }
                        return new ErrorValue({ message: `Cannot parse string to bool: ${value}` });
                    }

                    function parseEnum(value, values, enumName) {
                        const parsed = values.find(item => item.name === value || item.ordinal === value || item.order === value);
                        if (parsed) {
                            return new Success({ value: parsed });
                        }
                        return new ErrorValue({ message: `Unable to parse ${enumName} from ${value}` });
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
                        return `${value.__capybaraType} { ` + keys.map(key => `"${key}": ${dataFieldToString(value[key])}`).join(', ') + ' }';
                    }

                    function dataFieldToString(value) {
                        return typeof value === 'string' ? `"${value}"` : toStringValue(value);
                    }

                    function dataValueInfo(value, name, packageName, packagePath) {
                        const fields = Object.keys(value)
                            .filter(key => !key.startsWith('__') && typeof value[key] !== 'function')
                            .map(key => ({ name: key, value: value[key] }));
                        return { name, packageName, packagePath, fields };
                    }

                    function reflection(target, name, packageName, packagePath, fieldNames) {
                        if (name) {
                            return {
                                name,
                                packageName,
                                packagePath,
                                fields: fieldNames.map(field => ({ name: field, value: target[field] })),
                            };
                        }
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

                    function writeProgramResult(value) {
                        if (isType(value, 'Program') && isType(value, 'Success') && Array.isArray(value.results)) {
                            for (const result of value.results) {
                                console.log(toStringValue(result));
                            }
                            return;
                        }
                        if (isType(value, 'Program') && isType(value, 'Failed')) {
                            for (const error of value.errors ?? []) {
                                console.error(toStringValue(error));
                            }
                            process.exitCode = value.exitCode ?? 1;
                            return;
                        }
                        console.log(toStringValue(value));
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
                        resultLikePipe,
                        resultLikeFlatMap,
                        list,
                        set,
                        seq,
                        size,
                        newArray,
                        toException,
                        decorateException,
                        arrayGet,
                        applyTrait,
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
                        setIsSubsetOf,
                        setIsProperSubsetOf,
                        setIsSupersetOf,
                        setIsProperSupersetOf,
                        setIntersection,
                        setSymmetricDifference,
                        setCartesianProduct,
                        setPowerSet,
                        dictPut,
                        dictPlus,
                        dictRemove,
                        dictMinus,
                        entries,
                        floatToInt,
                        longToInt,
                        floatToLong,
                        toLong,
                        longAdd,
                        longSub,
                        longMul,
                        longDiv,
                        longMod,
                        longPow,
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
                        parseEnum,
                        toStringValue,
                        dataToString,
                        dataValueInfo,
                        reflection,
                        writeProgramResult,
                        unsupported,
                    };
                    """;
        }
    }

    static Map<String, String> buildFunctionNameOverrides(CompiledProgram program) {
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
                collisions.computeIfAbsent(ownerKey + "|" + normalizedBaseName, ignored -> new ArrayList<>()).add(function);
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
            var overloadedRawNames = functions.stream()
                    .collect(java.util.stream.Collectors.groupingBy(function -> baseMethodName(function.name()), LinkedHashMap::new, java.util.stream.Collectors.counting()));
            for (var function : functions) {
                var rawBaseName = baseMethodName(function.name());
                var normalizedBaseName = normalizeJsIdentifier(rawBaseName);
                var overloadSuffix = overloadSuffix(function);
                var legacyEmittedName = normalizedBaseName + overloadSuffix;
                var namedCanonical = canonicalNamedFunction.filter(named -> named == function).isPresent();
                var emittedName = mixedRawNames
                        ? (namedCanonical && overloadedRawNames.getOrDefault(rawBaseName, 0L) == 1L
                                ? normalizedBaseName
                                : normalizedBaseName + "__" + methodVariantSuffix(rawBaseName) + overloadSuffix)
                        : normalizedBaseName + overloadSuffix;
                var parameterTypes = function.parameters().stream().map(CompiledFunction.CompiledFunctionParameter::type).toList();
                overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
                if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
                    overrides.put(signatureKey(ownerModuleNames.get(function) + "." + function.name(), parameterTypes), emittedName);
                }
                if (!function.name().startsWith(METHOD_DECL_PREFIX)) {
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

    static String simpleMethodName(String target) {
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

    static String simpleTypeName(String typeName) {
        return normalizeJsTypeIdentifier(rawSimpleTypeName(typeName));
    }

    static String rawSimpleTypeName(String typeName) {
        var stripped = typeName;
        var squareGeneric = stripped.indexOf('[');
        var javaGeneric = stripped.indexOf('<');
        var generic = squareGeneric >= 0 && javaGeneric >= 0
                ? Math.min(squareGeneric, javaGeneric)
                : Math.max(squareGeneric, javaGeneric);
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
        return stripped;
    }

    static boolean isTypeLikeIdentifier(String value) {
        if (value == null || value.isBlank()) {
            return false;
        }
        var idx = 0;
        while (idx < value.length() && value.charAt(idx) == '_') {
            idx++;
        }
        return idx < value.length() && Character.isUpperCase(value.charAt(idx));
    }

    static String normalizeJsTypeIdentifier(String rawName) {
        var preservePrivatePrefix = rawName.startsWith("_") && !rawName.startsWith("__");
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
        if (preservePrivatePrefix && !identifier.startsWith("_")) {
            identifier = "_" + identifier;
        }
        if (!Character.isLetter(identifier.charAt(0)) && identifier.charAt(0) != '_' && identifier.charAt(0) != '$') {
            identifier = "T" + identifier;
        }
        if (JS_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }

    static String enumValueIdentifier(String rawName) {
        if (isValidJsIdentifier(rawName) && !JS_KEYWORDS.contains(rawName)) {
            return rawName;
        }
        return normalizeJsIdentifier(rawName);
    }

    static boolean isValidJsIdentifier(String value) {
        if (value == null || value.isBlank()) {
            return false;
        }
        var first = value.charAt(0);
        if (!Character.isLetter(first) && first != '_' && first != '$') {
            return false;
        }
        for (var i = 1; i < value.length(); i++) {
            var ch = value.charAt(i);
            if (!Character.isLetterOrDigit(ch) && ch != '_' && ch != '$') {
                return false;
            }
        }
        return true;
    }

    static String normalizeJsIdentifier(String rawName) {
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

    static String moduleVar(String className) {
        return "__module_" + className.replaceAll("[^A-Za-z0-9]+", "_");
    }

    static Path classNamePath(String className) {
        return Path.of(className.replace('.', '/') + ".js");
    }

    static List<String> classNameCandidates(String rawClassName) {
        var candidates = new LinkedHashSet<String>();
        candidates.add(rawClassName);
        var normalized = rawClassName.replace('\\', '.').replace('/', '.');
        while (normalized.startsWith(".")) {
            normalized = normalized.substring(1);
        }
        candidates.add(normalized);
        var normalizedPackageSegments = normalizePackageSegments(normalized);
        candidates.add(normalizedPackageSegments);
        moduleClassNameCandidate(normalized).ifPresent(candidates::add);
        moduleClassNameCandidate(normalizedPackageSegments).ifPresent(candidates::add);
        if (normalized.startsWith("_") && normalized.length() > 1 && Character.isLowerCase(normalized.charAt(1))) {
            candidates.add(normalized.substring(1));
        }
        return List.copyOf(candidates);
    }

    private static Optional<String> moduleClassNameCandidate(String className) {
        var lastDot = className.lastIndexOf('.');
        if (lastDot < 0 || className.endsWith("Module")) {
            return Optional.empty();
        }
        var simpleName = className.substring(lastDot + 1);
        if (simpleName.isBlank() || !Character.isUpperCase(simpleName.charAt(0))) {
            return Optional.empty();
        }
        return Optional.of(className + "Module");
    }

    private static String normalizePackageSegments(String className) {
        var segments = className.split("\\.");
        if (segments.length < 2) {
            return className;
        }
        var normalized = new ArrayList<String>();
        for (var i = 0; i < segments.length - 1; i++) {
            normalized.add(normalizeJsIdentifier(segments[i]));
        }
        normalized.add(segments[segments.length - 1]);
        return String.join(".", normalized);
    }

    static String relativeRequire(Path fromModule, Path targetModule) {
        var fromDir = Optional.ofNullable(fromModule.getParent()).orElse(Path.of(""));
        var relative = fromDir.relativize(targetModule).toString().replace('\\', '/');
        if (!relative.startsWith(".")) {
            relative = "./" + relative;
        }
        return relative;
    }

    static String stripNumericSuffix(String value) {
        if (value.endsWith("L") || value.endsWith("l") || value.endsWith("f") || value.endsWith("F") || value.endsWith("d") || value.endsWith("D")) {
            return value.substring(0, value.length() - 1);
        }
        return value;
    }

    static String renderNumericLiteral(String value) {
        if (value.endsWith("L") || value.endsWith("l")) {
            return renderLongLiteral(value);
        }
        return stripNumericSuffix(value);
    }

    static String renderLongLiteral(String value) {
        var literal = value.endsWith("L") || value.endsWith("l")
                ? value.substring(0, value.length() - 1)
                : value;
        if (literal.startsWith("+")) {
            literal = literal.substring(1);
        }
        return literal.replace("_", "") + "n";
    }

    static String jsArray(Collection<String> values) {
        return values.stream().map(JavaScriptGenerator::jsString).collect(joining(", ", "[", "]"));
    }

    static String jsString(String value) {
        var escaped = value
                .replace("\\", "\\\\")
                .replace("'", "\\'")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
        return "'" + escaped + "'";
    }
}
