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
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;

public final class PythonGenerator implements Generator {
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final String DICT_PIPE_ARGS_SEPARATOR = "::";
    private static final String TUPLE_PIPE_ARGS_SEPARATOR = ";;";
    static final Path RUNTIME_PATH = Path.of("dev", "capylang", "capybara.py");
    private static final Pattern MODULE_VAR_PATTERN = Pattern.compile("\\bcapy_module_[A-Za-z0-9_]+\\b");
    private static final Pattern CONST_NAME_PATTERN = Pattern.compile("^_?[A-Z_][A-Z0-9_]*$");
    private static final Set<String> RUNTIME_PROVIDED_MODULE_NAMES = Set.of(
            "capy/collection/Dict",
            "capy/collection/List",
            "capy/collection/Set",
            "capy/collection/Tuple",
            "capy/date_time/Clock",
            "capy/date_time/Date",
            "capy/date_time/DateTime",
            "capy/date_time/Duration",
            "capy/date_time/Interval",
            "capy/date_time/Time",
            "capy/io/Console",
            "capy/io/IO",
            "capy/io/Stdout",
            "capy/lang/Effect",
            "capy/lang/Option",
            "capy/lang/Primitives",
            "capy/lang/Regex",
            "capy/lang/Result",
            "capy/lang/String",
            "capy/lang/System",
            "capy/test/Assert",
            "capy/test/CapyTest"
    );
    private static final Set<String> PY_KEYWORDS = Set.of(
            "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class",
            "continue", "def", "del", "elif", "else", "except", "finally", "for", "from",
            "global", "if", "import", "in", "is", "lambda", "match", "nonlocal", "not",
            "or", "pass", "raise", "return", "try", "while", "with", "yield"
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
                .filter(module -> !isRuntimeProvidedModule(module))
                .map(module -> ModuleInfo.from(module, astBuilder.build(module)))
                .toList();
        var context = ProgramContext.build(moduleInfos, program.objectOrientedModules(), functionNameOverrides);

        for (var moduleInfo : moduleInfos) {
            modules.add(new GeneratedModule(moduleInfo.relativePath(), new ModuleRenderer(context, moduleInfo).render()));
        }
        modules.addAll(new ObjectOrientedPythonGenerator(context).generate(program.objectOrientedModules()));
        modules.addAll(RuntimeModules.modules());
        return new GeneratedProgram(List.copyOf(modules));
    }

    private static boolean isRuntimeProvidedModule(CompiledModule module) {
        var path = module.path().replace('\\', '/').replaceFirst("^/+", "");
        var moduleName = path.isBlank() ? module.name() : path + "/" + module.name();
        return RUNTIME_PROVIDED_MODULE_NAMES.contains(moduleName);
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
            body.append(renderExportAliases());
            body.append(renderProgramMain());
            requireReferencedModules(body.toString());

            var output = new StringBuilder();
            output.append("# Generated by Capybara. Do not edit.\n");
            output.append(renderSysPathBootstrap(moduleInfo.relativePath()));
            output.append("import dev.capylang.capybara as capy\n");
            for (var entry : requiredModules.entrySet()) {
                output.append("import ")
                        .append(moduleImportPath(entry.getValue()))
                        .append(" as ")
                        .append(moduleVar(entry.getKey()))
                        .append("\n");
            }
            output.append('\n');
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
            var capybaraName = capybaraTypeName(record.name().toString());
            code.append("class ").append(name).append(":\n");
            code.append("    def __init__(self, fields=None):\n");
            code.append("        fields = fields or {}\n");
            code.append("        self.__capybaraType = ").append(pyString(capybaraName)).append("\n");
            var typeNames = new ArrayList<String>();
            typeNames.add(capybaraName);
            if (!capybaraName.equals(name)) {
                typeNames.add(name);
            }
            typeNames.addAll(programContext.parentTypes(name));
            code.append("        self.__capybaraTypes = ").append(pyArray(typeNames.stream().distinct().toList())).append("\n");
            for (var field : record.fields()) {
                var primaryFieldName = pyIdentifier(field.name());
                code.append("        self.")
                        .append(primaryFieldName)
                        .append(" = fields.get(")
                        .append(pyString(field.name()))
                        .append(field.name().equals(primaryFieldName) ? "" : ", fields.get(" + pyString(primaryFieldName) + ")")
                        .append(")\n");
                if (!field.name().equals(primaryFieldName)) {
                    code.append("        setattr(self, ")
                            .append(pyString(field.name()))
                            .append(", self.")
                            .append(primaryFieldName)
                            .append(")\n");
                }
            }
            code.append("\n");
            code.append("    def with_(self, *values, **fields):\n");
            code.append("        if len(values) == 1 and isinstance(values[0], dict) and not fields:\n");
            code.append("            fields = values[0]\n");
            code.append("            values = ()\n");
            code.append("        if fields:\n");
            code.append("            return ").append(name).append("({\n");
            for (var field : record.fields()) {
                var primaryFieldName = pyIdentifier(field.name());
                code.append("                ").append(pyString(field.name())).append(": fields.get(")
                        .append(pyString(field.name()))
                        .append(field.name().equals(primaryFieldName) ? "" : ", fields.get(" + pyString(primaryFieldName) + ", self." + primaryFieldName + ")")
                        .append(field.name().equals(primaryFieldName) ? ", self." + primaryFieldName : "")
                        .append("),\n");
            }
            code.append("            })\n");
            code.append("        return ").append(name).append("({\n");
            for (int i = 0; i < record.fields().size(); i++) {
                var field = record.fields().get(i);
                code.append("            ").append(pyString(field.name())).append(": values[").append(i).append("] if len(values) > ").append(i).append(" else None,\n");
            }
            code.append("        })\n\n");
            code.append("    def toString(self):\n");
            code.append("        return capy.data_to_string(self)\n\n");
            code.append("    def __str__(self):\n");
            code.append("        return self.toString()\n\n");
            code.append("    def capybaraDataValueInfo(self):\n");
            code.append("        return capy.data_value_info(self, ")
                    .append(pyString(capybaraName))
                    .append(", ")
                    .append(pyString(moduleInfo.packageName()))
                    .append(", ")
                    .append(pyString(moduleInfo.packagePath()))
                    .append(", ")
                    .append(pyArray(record.fields().stream().map(JavaRecord.JavaRecordField::name).toList()))
                    .append(")\n\n");
            for (var method : record.methods()) {
                code.append(renderFunction(method, false));
            }
            var recordMethodNames = record.methods().stream()
                    .map(JavaMethod::name)
                    .collect(java.util.stream.Collectors.toSet());
            if (!recordMethodNames.contains("pipe")) {
                code.append("    def pipe(self, mapper):\n");
                code.append("        return capy.result_like_pipe(self, mapper)\n\n");
            }
            if (!recordMethodNames.contains("pipe_star")) {
                code.append("    def pipe_star(self, mapper):\n");
                code.append("        return capy.result_like_flat_map(self, mapper)\n\n");
            }
            for (var method : interfaceDefaultMethods(record)) {
                if (!recordMethodNames.contains(method.name())) {
                    code.append(renderFunction(method, false));
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
            var capybaraName = capybaraTypeName(javaEnum.name().toString());
            var values = javaEnum.values().isEmpty() ? List.of("INSTANCE") : javaEnum.values();
            var code = new StringBuilder();
            if (values.size() == 1 && "INSTANCE".equals(values.getFirst())) {
                code.append(enumName)
                        .append(" = capy.enum_value(")
                        .append(pyString(capybaraName)).append(", ")
                        .append(pyString(capybaraName)).append(", ")
                        .append(pyArray(programContext.parentTypes(enumName))).append(", ")
                        .append("0, [], ")
                        .append(pyString(moduleInfo.packageName())).append(", ")
                        .append(pyString(moduleInfo.packagePath()))
                        .append(")\n");
                exportNames.add(enumName);
                return code.toString();
            }
            var valuesName = "_" + enumName + "_values";
            code.append(valuesName).append(" = [\n");
            for (int i = 0; i < values.size(); i++) {
                var value = values.get(i);
                var valueName = enumValueIdentifier(value);
                var capybaraValueName = capybaraTypeName(value);
                var aliases = new ArrayList<String>();
                if (!valueName.equals(capybaraValueName)) {
                    aliases.add(valueName);
                }
                code.append("    capy.enum_value(")
                        .append(pyString(capybaraValueName)).append(", ")
                        .append(pyString(capybaraName)).append(", ")
                        .append(pyArray(programContext.parentTypes(valueName))).append(", ")
                        .append(i).append(", ")
                        .append(pyArray(aliases)).append(", ")
                        .append(pyString(moduleInfo.packageName())).append(", ")
                        .append(pyString(moduleInfo.packagePath())).append("),\n");
            }
            code.append("]\n");
            code.append("class ").append(enumName).append(":\n");
            code.append("    values = ").append(valuesName).append("\n");
            for (int i = 0; i < values.size(); i++) {
                code.append("    ").append(enumValueIdentifier(values.get(i))).append(" = values[").append(i).append("]\n");
            }
            code.append("\n");
            code.append("    @staticmethod\n");
            code.append("    def valuesSet():\n");
            code.append("        return capy.set_(").append(enumName).append(".values)\n\n");
            code.append("    @staticmethod\n");
            code.append("    def parse(value):\n");
            code.append("        return capy.parse_enum(value, ").append(enumName).append(".values, ").append(pyString(capybaraName)).append(")\n\n");
            exportNames.add(enumName);
            for (var value : values) {
                var valueName = enumValueIdentifier(value);
                code.append(valueName).append(" = ").append(enumName).append(".").append(valueName).append("\n");
                var typeAlias = simpleTypeName(value);
                if (!typeAlias.equals(valueName)) {
                    code.append(typeAlias).append(" = ").append(valueName).append("\n");
                }
                var normalizedAlias = normalizeIdentifier(value);
                if (!normalizedAlias.equals(valueName) && !normalizedAlias.equals(typeAlias)) {
                    code.append(normalizedAlias).append(" = ").append(valueName).append("\n");
                }
                exportNames.add(valueName);
            }
            return code.toString();
        }

        private String renderConst(JavaConst javaConst) {
            var expression = expressions.render(javaConst.expression(), Scope.root());
            var name = pyIdentifier(javaConst.name());
            if (!javaConst.isPrivate()) {
                exportNames.add(name);
            }
            return name + " = " + expression + "\n";
        }

        private String renderFunction(JavaMethod method, boolean topLevel) {
            var name = topLevel
                    ? programContext.emittedFunctionName(method.sourceName(), method.sourceParameterTypes())
                    : pyIdentifier(method.name());
            var params = method.parameters().stream()
                    .map(JavaMethod.JavaFunctionParameter::generatedName)
                    .map(PythonGenerator::pyIdentifier)
                    .toList();
            var scope = Scope.root();
            if (!topLevel) {
                scope = scope.bind("this", "self");
            }
            for (var parameter : method.parameters()) {
                scope = scope.bind(parameter.sourceName(), pyIdentifier(parameter.generatedName()));
            }
            var body = expressions.render(method.expression(), scope);
            var code = new StringBuilder();
            if (topLevel) {
                code.append("def ").append(name).append("(").append(String.join(", ", params)).append("):\n");
                code.append("    return ").append(body).append("\n");
            } else {
                var allParams = new ArrayList<String>();
                allParams.add("self");
                allParams.addAll(params);
                code.append("    def ").append(name).append("(").append(String.join(", ", allParams)).append("):\n");
                code.append("        return ").append(body).append("\n\n");
            }
            if (topLevel && !method.isPrivate()) {
                exportNames.add(name);
                var alias = pyIdentifier(method.name());
                if (!alias.equals(name)) {
                    code.append(alias).append(" = ").append(name).append("\n");
                    exportNames.add(alias);
                }
            }
            return code.toString();
        }

        private String renderExportAliases() {
            var exportAliases = new LinkedHashMap<String, List<String>>();
            for (var exportName : exportNames) {
                var overloadSeparator = exportName.indexOf("__");
                if (overloadSeparator > 0) {
                    exportAliases.computeIfAbsent(exportName.substring(0, overloadSeparator), ignored -> new ArrayList<>())
                            .add(exportName);
                }
            }
            var aliases = new StringBuilder();
            for (var entry : exportAliases.entrySet()) {
                if (exportNames.contains(entry.getKey())) {
                    continue;
                }
                aliases.append("def ")
                        .append(entry.getKey())
                        .append("(*args):\n")
                        .append("    return capy.dispatch_overload([\n");
                for (var overload : entry.getValue()) {
                    aliases.append("        (")
                            .append(pyString(overload))
                            .append(", ")
                            .append(overload)
                            .append("),\n");
                }
                aliases.append("    ], args)\n\n");
            }
            return aliases.toString();
        }

        private String renderProgramMain() {
            var main = javaClass.staticMethods().stream()
                    .filter(JavaMethod::programMain)
                    .findFirst();
            if (main.isEmpty()) {
                return "";
            }
            var mainName = pyIdentifier(main.orElseThrow().name());
            return "\nif __name__ == '__main__':\n"
                   + "    result = " + mainName + "(__capy_sys.argv[1:])\n"
                   + "    value = result.unsafe_run() if capy.is_effect(result) else result\n"
                   + "    if value is not None:\n"
                   + "        capy.write_program_result(value)\n";
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
                case CompiledBooleanValue booleanValue -> pyBool(booleanValue == CompiledBooleanValue.TRUE);
                case CompiledByteValue byteValue -> JavaScriptGenerator.stripNumericSuffix(byteValue.byteValue());
                case CompiledDoubleValue doubleValue -> JavaScriptGenerator.stripNumericSuffix(doubleValue.doubleValue());
                case CompiledFloatValue floatValue -> JavaScriptGenerator.stripNumericSuffix(floatValue.floatValue());
                case CompiledIntValue intValue -> JavaScriptGenerator.stripNumericSuffix(intValue.intValue());
                case CompiledLongValue longValue -> renderLongLiteral(longValue.longValue());
                case CompiledStringValue stringValue -> stringValue.toString();
                case CompiledVariable variable -> scope.resolve(variable.name());
                case CompiledNumericWidening numericWidening -> renderNumericWidening(numericWidening, scope);
                case CompiledNewList newList -> renderNewList(newList, scope);
                case CompiledNewSet newSet -> renderNewSet(newSet, scope);
                case CompiledNewDict newDict -> renderNewDict(newDict, scope);
                case CompiledTupleExpression tupleExpression -> renderTuple(tupleExpression, scope);
                case CompiledFieldAccess fieldAccess -> "getattr((" + render(fieldAccess.source(), scope) + "), " + pyString(fieldAccess.field()) + ")";
                case CompiledFunctionCall functionCall -> renderFunctionCall(functionCall, scope);
                case CompiledFunctionInvoke functionInvoke -> renderFunctionInvoke(functionInvoke, scope);
                case CompiledIfExpression ifExpression -> "((" + render(ifExpression.thenBranch(), scope) + ") if ("
                                                          + renderBoolean(ifExpression.condition(), scope) + ") else ("
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
                case CompiledEffectExpression effectExpression -> "capy.delay(lambda: (" + render(effectExpression.body(), scope) + "))";
                case CompiledEffectBindExpression bindExpression -> renderEffectBind(bindExpression, scope);
                case CompiledReflectionValue reflectionValue -> renderReflection(reflectionValue, scope);
                case CompiledNothingValue nothingValue -> "capy.unsupported(" + pyString(nothingValue.message()) + ")";
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
                    .collect(joining(", ", "capy.set_([", "])"));
        }

        private String renderNumericWidening(CompiledNumericWidening numericWidening, Scope scope) {
            var expression = render(numericWidening.expression(), scope);
            if (numericWidening.type() == PrimitiveLinkedType.LONG && numericWidening.expression().type() != PrimitiveLinkedType.LONG) {
                return "capy.to_long(" + expression + ")";
            }
            return expression;
        }

        private String renderNewDict(CompiledNewDict newDict, Scope scope) {
            return newDict.entries().stream()
                    .map(entry -> "(" + render(entry.key(), scope) + ", " + render(entry.value(), scope) + ")")
                    .collect(joining(", ", "capy.dict_([", "])"));
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
                return "capy.math.sqrt(" + args.getFirst() + ")";
            }
            var nativeCall = renderRuntimeBackedFunctionCall(functionCall, args);
            if (nativeCall.isPresent()) {
                return nativeCall.orElseThrow();
            }
            var target = resolveFunctionTarget(functionCall);
            if (isConstCall(functionCall)) {
                return target;
            }
            return target + "(" + String.join(", ", args) + ")";
        }

        private Optional<String> renderRuntimeBackedFunctionCall(CompiledFunctionCall functionCall, List<String> args) {
            var owner = functionOwner(functionCall);
            var methodName = simpleMethodName(functionCall.name());
            if (owner.filter("capy.lang.Primitives"::equals).isPresent() && args.size() == 1) {
                return switch (methodName) {
                    case "to_int", "toInt" -> Optional.of("capy.parse_int_result(" + args.getFirst() + ")");
                    case "to_long", "toLong" -> Optional.of("capy.parse_long_result(" + args.getFirst() + ")");
                    case "to_double", "toDouble" -> Optional.of("capy.parse_float_result(" + args.getFirst() + ", 'double')");
                    case "to_float", "toFloat" -> Optional.of("capy.parse_float_result(" + args.getFirst() + ", 'float')");
                    case "to_bool", "toBool" -> Optional.of("capy.parse_bool_result(" + args.getFirst() + ")");
                    default -> Optional.empty();
                };
            }
            if (owner.filter("capy.lang.Random"::equals).isPresent() && "seed".equals(methodName) && args.isEmpty()) {
                require("capy.lang.Random");
                return Optional.of(moduleVar("capy.lang.Random") + ".Seed({ 'value': capy.current_millis() })");
            }
            return Optional.empty();
        }

        private Optional<String> functionOwner(CompiledFunctionCall functionCall) {
            var lastDot = functionCall.name().lastIndexOf('.');
            if (lastDot >= 0) {
                return programContext.resolveClassName(functionCall.name().substring(0, lastDot));
            }
            var emittedName = emittedMethodName(functionCall);
            return programContext.importedMemberOwner(moduleInfo.className(), emittedName)
                    .or(() -> programContext.importedMemberOwner(moduleInfo.className(), simpleMethodName(functionCall.name())))
                    .flatMap(programContext::resolveClassName);
        }

        private String renderMethodCall(CompiledFunctionCall functionCall, List<String> args, Scope scope) {
            if (args.isEmpty()) {
                throw new IllegalStateException("Method call requires receiver argument: " + functionCall.name());
            }
            var methodName = simpleMethodName(functionCall.name());
            var receiver = args.getFirst();
            var receiverType = functionCall.arguments().getFirst().type();
            var tailArgs = args.subList(1, args.size());

            if ("unsafe_run".equals(methodName) || "unsafeRun".equals(methodName)) {
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
                return "len(" + receiver + ")";
            }
            if (("starts_with".equals(methodName) || "startsWith".equals(methodName))
                && receiverType == PrimitiveLinkedType.STRING
                && tailArgs.size() == 1) {
                return "str(" + receiver + ").startswith(" + tailArgs.getFirst() + ")";
            }
            if (("end_with".equals(methodName) || "endWith".equals(methodName))
                && receiverType == PrimitiveLinkedType.STRING
                && tailArgs.size() == 1) {
                return "str(" + receiver + ").endswith(" + tailArgs.getFirst() + ")";
            }
            if ("trim".equals(methodName) && receiverType == PrimitiveLinkedType.STRING && tailArgs.isEmpty()) {
                return "str(" + receiver + ").strip()";
            }
            if ("size".equals(methodName) && isCollectionType(receiverType)) {
                return "capy.size(" + receiver + ")";
            }
            if (("to_list".equals(methodName) || "toList".equals(methodName) || "as_list".equals(methodName) || "asList".equals(methodName))
                && isCollectionType(receiverType)) {
                return "capy.list_(" + receiver + ")";
            }
            if ("entries".equals(methodName) && receiverType instanceof CollectionLinkedType.CompiledDict) {
                return "capy.entries(" + receiver + ")";
            }
            if ("replace".equals(methodName) && tailArgs.size() == 2) {
                return "str(" + receiver + ").replace(" + tailArgs.get(0) + ", " + tailArgs.get(1) + ")";
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
                return renderCollectionPlus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst(), functionCall.type());
            }
            if (List.of("-", "minus").contains(methodName) && tailArgs.size() == 1 && isNativeMinusType(receiverType)) {
                return renderCollectionMinus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst(), functionCall.type());
            }
            if (("contains".equals(methodName) || "?".equals(methodName)) && isCollectionType(receiverType)) {
                return renderContains(receiverType, receiver, tailArgs.getFirst());
            }
            if ("is_empty".equals(methodName) && isCollectionType(receiverType)) {
                return "capy.size(" + receiver + ") == 0";
            }
            if ("any".equals(methodName) && tailArgs.size() == 1) {
                return "capy.any_(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("all".equals(methodName) && tailArgs.size() == 1) {
                return "capy.all_(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("map".equals(methodName) || "|".equals(methodName) || "pipe".equals(methodName)) && isCollectionType(receiverType)) {
                return "capy.map_collection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("filter".equals(methodName) || "|-".equals(methodName) || "pipe_minus".equals(methodName)) && isCollectionType(receiverType)) {
                return "capy.filter_collection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if ("reject".equals(methodName) && isCollectionType(receiverType)) {
                return "capy.reject_collection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("flat_map".equals(methodName) || "flatMap".equals(methodName) || "|*".equals(methodName) || "pipe_star".equals(methodName))
                && isCollectionType(receiverType)) {
                return "capy.flat_map_collection(" + receiver + ", " + tailArgs.getFirst() + ")";
            }
            if (("reduce".equals(methodName) || "|>".equals(methodName) || "pipe_greater".equals(methodName)) && tailArgs.size() >= 2 && isCollectionType(receiverType)) {
                return "capy.reduce_collection(" + receiver + ", " + tailArgs.get(0) + ", " + tailArgs.get(1) + ")";
            }
            if (isPrimitiveConversion(methodName)) {
                return renderConversion(methodName, receiver, receiverType, functionCall.type());
            }
            var emittedName = pyIdentifier(emittedMethodName(functionCall));
            if ("with".equals(emittedName)) {
                emittedName = "with_";
            }
            return "(" + receiver + ")." + emittedName + "(" + String.join(", ", tailArgs) + ")";
        }

        private Optional<String> renderNativeSetMethod(String methodName, String receiver, List<String> tailArgs) {
            if (tailArgs.isEmpty() && List.of("power_set", "powerSet", "op2118", "℘").contains(methodName)) {
                return Optional.of("capy.set_power_set(" + receiver + ")");
            }
            if (tailArgs.size() != 1) {
                return Optional.empty();
            }
            var other = tailArgs.getFirst();
            return switch (methodName) {
                case "is_subset_of", "isSubsetOf", "op2286", "⊆" -> Optional.of("capy.set_is_subset_of(" + receiver + ", " + other + ")");
                case "is_proper_subset_of", "isProperSubsetOf", "op2282", "⊂" -> Optional.of("capy.set_is_proper_subset_of(" + receiver + ", " + other + ")");
                case "is_superset_of", "isSupersetOf", "op2287", "⊇" -> Optional.of("capy.set_is_superset_of(" + receiver + ", " + other + ")");
                case "is_proper_superset_of", "isProperSupersetOf", "op2283", "⊃" -> Optional.of("capy.set_is_proper_superset_of(" + receiver + ", " + other + ")");
                case "union", "op222a", "∪" -> Optional.of("capy.set_plus(" + receiver + ", " + other + ")");
                case "intersection", "op2229", "∩" -> Optional.of("capy.set_intersection(" + receiver + ", " + other + ")");
                case "difference" -> Optional.of("capy.set_minus(" + receiver + ", " + other + ")");
                case "symmetric_difference", "symmetricDifference", "op25b3", "△" -> Optional.of("capy.set_symmetric_difference(" + receiver + ", " + other + ")");
                case "cartesian_product", "cartesianProduct", "opd7", "×" -> Optional.of("capy.set_cartesian_product(" + receiver + ", " + other + ")");
                default -> Optional.empty();
            };
        }

        private Optional<String> renderNativeCollectionMethod(CompiledFunctionCall functionCall, List<String> args, String methodName) {
            var receiverType = functionCall.arguments().getFirst().type();
            var receiver = args.get(0);
            if ("get".equals(methodName)) {
                if (args.size() == 2) {
                    if (receiverType instanceof CompiledTupleType) {
                        return Optional.of("capy.raw_index(" + receiver + ", " + args.get(1) + ")");
                    }
                    if (receiverType instanceof CollectionLinkedType.CompiledList
                        || receiverType instanceof CollectionLinkedType.CompiledDict
                        || receiverType == PrimitiveLinkedType.STRING) {
                        return Optional.of("capy.get_index(" + receiver + ", " + args.get(1) + ")");
                    }
                    return Optional.empty();
                }
                if (args.size() == 3) {
                    if (receiverType instanceof CollectionLinkedType.CompiledList
                        || receiverType == PrimitiveLinkedType.STRING) {
                        return Optional.of("capy.slice_(" + receiver + ", " + args.get(1) + ", " + args.get(2) + ")");
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
                        ? "capy.long_mul(" + left + ", " + right + ")"
                        : expression.type() == PrimitiveLinkedType.INT
                                ? "capy.int_mul(" + left + ", " + right + ")"
                                : "((" + left + ") * (" + right + "))";
                case DIV -> expression.type() == PrimitiveLinkedType.INT
                        ? "capy.int_div(" + left + ", " + right + ")"
                        : expression.type() == PrimitiveLinkedType.LONG
                                ? "capy.long_div(" + left + ", " + right + ")"
                                : "((" + left + ") / (" + right + "))";
                case MOD -> expression.type() == PrimitiveLinkedType.LONG
                        ? "capy.long_mod(" + left + ", " + right + ")"
                        : expression.type() == PrimitiveLinkedType.INT
                                ? "capy.int_mod(" + left + ", " + right + ")"
                                : "((" + left + ") % (" + right + "))";
                case POWER -> expression.type() == PrimitiveLinkedType.LONG
                        ? "capy.long_pow(" + left + ", " + right + ")"
                        : "capy.math.pow(" + left + ", " + right + ")";
                case GT, LT, LE, GE -> "((" + left + ") " + expression.operator().symbol() + " (" + right + "))";
                case EQUAL -> renderEquality(expression.left().type(), left, expression.right().type(), right, false);
                case NOTEQUAL -> renderEquality(expression.left().type(), left, expression.right().type(), right, true);
                case AND -> "((" + renderBoolean(expression.left(), scope) + ") and (" + renderBoolean(expression.right(), scope) + "))";
                case PIPE -> "((" + renderBoolean(expression.left(), scope) + ") or (" + renderBoolean(expression.right(), scope) + "))";
                case QUESTION -> renderContains(expression.left().type(), left, right);
                case BITWISE_AND -> "((" + left + ") & (" + right + "))";
                case BITWISE_NAND -> "(~((" + left + ") & (" + right + ")))";
                case BITWISE_OR -> "((" + left + ") | (" + right + "))";
                case BITWISE_XOR -> "((" + left + ") ^ (" + right + "))";
                case BITWISE_NOT -> "(~(" + left + "))";
                default -> throw new UnsupportedOperationException("Unsupported Python infix operator: " + expression.operator());
            };
        }

        private String renderEquality(CompiledType leftType, String left, CompiledType rightType, String right, boolean negated) {
            var equality = (leftType == PrimitiveLinkedType.BOOL && rightType != PrimitiveLinkedType.BOOL)
                    ? "(" + left + " == capy.truthy(" + right + "))"
                    : (rightType == PrimitiveLinkedType.BOOL && leftType != PrimitiveLinkedType.BOOL)
                            ? "(capy.truthy(" + left + ") == " + right + ")"
                            : "capy.equals(" + left + ", " + right + ")";
            return negated ? "(not " + equality + ")" : equality;
        }

        private String renderLet(CompiledLetExpression letExpression, Scope scope) {
            var pyName = scope.reserve(letExpression.name());
            var child = scope.bind(letExpression.name(), pyName);
            return "capy.let_(" + render(letExpression.value(), scope) + ", lambda " + pyName + ": (" + render(letExpression.rest(), child) + "))";
        }

        private String renderLambda(CompiledLambdaExpression lambdaExpression, Scope scope) {
            if (lambdaExpression.functionType().argumentType() == PrimitiveLinkedType.NOTHING) {
                return "(lambda: (" + render(lambdaExpression.expression(), scope) + "))";
            }
            var tupleArgs = parseTuplePipeArguments(lambdaExpression.argumentName());
            if (tupleArgs.length > 0 && lambdaExpression.functionType().argumentType() instanceof CompiledTupleType tupleType) {
                var pyName = scope.reserve("__tuple_item");
                var child = scope.bind(lambdaExpression.argumentName(), pyName);
                var lambdaArgs = new ArrayList<String>();
                var callArgs = new ArrayList<String>();
                var size = Math.min(tupleType.elementTypes().size(), tupleArgs.length);
                for (int i = 0; i < size; i++) {
                    if (!"_".equals(tupleArgs[i]) && !tupleArgs[i].isBlank()) {
                        var itemName = pyIdentifier(tupleArgs[i]);
                        lambdaArgs.add(itemName);
                        callArgs.add("capy.raw_index(" + pyName + ", " + i + ")");
                        child = child.bind(tupleArgs[i], itemName);
                    }
                }
                return "(lambda " + pyName + ": (lambda " + String.join(", ", lambdaArgs) + ": ("
                       + render(lambdaExpression.expression(), child) + "))(" + String.join(", ", callArgs) + "))";
            }
            var pyName = scope.reserve(lambdaExpression.argumentName());
            var child = scope.bind(lambdaExpression.argumentName(), pyName);
            return "(lambda " + pyName + ": (" + render(lambdaExpression.expression(), child) + "))";
        }

        private String renderIndex(CompiledIndexExpression indexExpression, Scope scope) {
            var source = render(indexExpression.source(), scope);
            var index = render(indexExpression.index(), scope);
            if (indexExpression.source().type() instanceof CompiledTupleType) {
                return "capy.raw_index(" + source + ", " + index + ")";
            }
            return "capy.get_index(" + source + ", " + index + ")";
        }

        private String renderSlice(CompiledSliceExpression sliceExpression, Scope scope) {
            var source = render(sliceExpression.source(), scope);
            var start = sliceExpression.start().map(expression -> render(expression, scope)).orElse("None");
            var end = sliceExpression.end().map(expression -> render(expression, scope)).orElse("None");
            return "capy.slice_(" + source + ", " + start + ", " + end + ")";
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
                    .map(assignment -> pyString(assignment.name()) + ": " + render(assignment.value(), scope))
                    .collect(joining(", ", "{ ", " }"));
            return constructor + "(" + fields + ")";
        }

        private String renderMatch(CompiledMatchExpression matchExpression, Scope scope) {
            var value = render(matchExpression.matchWith(), scope);
            var cases = new ArrayList<String>();
            for (var matchCase : matchExpression.cases()) {
                var rendered = renderPattern(matchCase.pattern(), scope);
                var guard = matchCase.guard()
                        .map(guardExpression -> "lambda " + rendered.parameterList() + ": (" + renderBoolean(guardExpression, rendered.scope()) + ")")
                        .orElse("None");
                var body = "lambda " + rendered.parameterList() + ": (" + render(matchCase.expression(), rendered.scope()) + ")";
                cases.add("(" + rendered.pattern() + ", " + guard + ", " + body + ")");
            }
            return "capy.match_value(" + value + ", [" + String.join(", ", cases) + "])";
        }

        private RenderedPattern renderPattern(CompiledMatchExpression.Pattern pattern, Scope scope) {
            return switch (pattern) {
                case CompiledMatchExpression.IntPattern intPattern ->
                        new RenderedPattern("capy.pattern_value(" + JavaScriptGenerator.stripNumericSuffix(intPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.LongPattern longPattern ->
                        new RenderedPattern("capy.pattern_value(" + renderLongLiteral(longPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.FloatPattern floatPattern ->
                        new RenderedPattern("capy.pattern_value(" + JavaScriptGenerator.stripNumericSuffix(floatPattern.value()) + ")", List.of(), scope);
                case CompiledMatchExpression.StringPattern stringPattern ->
                        new RenderedPattern("capy.pattern_value(" + stringPattern.value() + ")", List.of(), scope);
                case CompiledMatchExpression.BoolPattern boolPattern ->
                        new RenderedPattern("capy.pattern_value(" + pyBool(Boolean.parseBoolean(boolPattern.value())) + ")", List.of(), scope);
                case CompiledMatchExpression.WildcardPattern ignored ->
                        new RenderedPattern("capy.pattern_wildcard()", List.of(), scope);
                case CompiledMatchExpression.VariablePattern variablePattern -> {
                    if (JavaScriptGenerator.isTypeLikeIdentifier(variablePattern.name())) {
                        yield new RenderedPattern("capy.pattern_type(" + pyString(typeNameReference(variablePattern.name())) + ")", List.of(), scope);
                    }
                    yield bindPatternValue(variablePattern.name(), scope);
                }
                case CompiledMatchExpression.WildcardBindingPattern wildcardBindingPattern -> bindPatternValue(wildcardBindingPattern.name(), scope);
                case CompiledMatchExpression.TypedPattern typedPattern -> {
                    var bound = bindPatternValue(typedPattern.name(), scope);
                    yield new RenderedPattern(
                            "capy.pattern_typed(" + pyString(typeNameReference(typedPattern.type().name())) + ", " + pyString(pyIdentifier(typedPattern.name())) + ")",
                            bound.bindings(),
                            bound.scope()
                    );
                }
                case CompiledMatchExpression.ConstructorPattern constructorPattern ->
                        renderConstructorPattern(constructorPattern, scope);
            };
        }

        private RenderedPattern renderConstructorPattern(CompiledMatchExpression.ConstructorPattern pattern, Scope scope) {
            var constructorName = typeNameReference(pattern.constructorName());
            var fields = programContext.fieldsForType(constructorName);
            var descriptors = new ArrayList<String>();
            var bindings = new ArrayList<String>();
            var current = scope;
            for (int i = 0; i < pattern.fieldPatterns().size(); i++) {
                var fieldName = i < fields.size() ? fields.get(i) : "value";
                var rendered = renderPattern(pattern.fieldPatterns().get(i), current);
                descriptors.add("(" + pyString(fieldName) + ", " + rendered.pattern() + ")");
                bindings.addAll(rendered.bindings());
                current = rendered.scope();
            }
            return new RenderedPattern(
                    "capy.pattern_constructor(" + pyString(constructorName) + ", [" + String.join(", ", descriptors) + "])",
                    bindings,
                    current
            );
        }

        private String typeNameReference(String typeName) {
            return programContext.emittedTypeName(moduleInfo.className(), typeName);
        }

        private RenderedPattern bindPatternValue(String sourceName, Scope scope) {
            var pyName = scope.reserve(sourceName);
            var child = scope.bind(sourceName, pyName);
            return new RenderedPattern("capy.pattern_bind(" + pyString(pyName) + ")", List.of(pyName), child);
        }

        private String renderPipe(CompiledPipeExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var mapper = lambdaForPipe(pipeExpression.argumentName(), pipeExpression.mapper(), scope);
            if (isOptionType(pipeExpression.source().type())) {
                return "capy.option_map(" + source + ", " + mapper + ")";
            }
            return "capy.map_collection(" + source + ", " + mapper + ")";
        }

        private String renderPipeFilterOut(CompiledPipeFilterOutExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var predicate = lambdaForPipe(pipeExpression.argumentName(), pipeExpression.predicate(), scope);
            if (isOptionType(pipeExpression.source().type())) {
                return "capy.option_filter_out(" + source + ", " + predicate + ")";
            }
            return "capy.reject_collection(" + source + ", " + predicate + ")";
        }

        private String renderPipeFlatMap(CompiledPipeFlatMapExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var mapper = lambdaForPipe(pipeExpression.argumentName(), pipeExpression.mapper(), scope);
            if (isOptionType(pipeExpression.source().type())) {
                return "capy.option_flat_map(" + source + ", " + mapper + ")";
            }
            return "capy.flat_map_collection(" + source + ", " + mapper + ")";
        }

        private String renderPipeReduce(CompiledPipeReduceExpression pipeExpression, Scope scope) {
            var source = render(pipeExpression.source(), scope);
            var initial = render(pipeExpression.initialValue(), scope);
            var accumulator = pyIdentifier(pipeExpression.accumulatorName());
            var value = pyIdentifier(pipeExpression.valueName());
            var child = scope.bind(pipeExpression.accumulatorName(), accumulator).bind(pipeExpression.valueName(), value);
            var reducer = "(lambda " + accumulator + ", " + value + ": (" + render(pipeExpression.reducerExpression(), child) + "))";
            return "capy.reduce_collection(" + source + ", " + initial + ", " + reducer + ")";
        }

        private String lambdaForPipe(String argumentName, CompiledExpression body, Scope scope) {
            var tupleArgs = parseTuplePipeArguments(argumentName);
            if (tupleArgs.length > 0) {
                var pyName = scope.reserve("__tuple_item");
                var child = scope.bind(argumentName, pyName);
                var lambdaArgs = new ArrayList<String>();
                var callArgs = new ArrayList<String>();
                for (int i = 0; i < tupleArgs.length; i++) {
                    if (!"_".equals(tupleArgs[i]) && !tupleArgs[i].isBlank()) {
                        var itemName = pyIdentifier(tupleArgs[i]);
                        lambdaArgs.add(itemName);
                        callArgs.add("capy.raw_index(" + pyName + ", " + i + ")");
                        child = child.bind(tupleArgs[i], itemName);
                    }
                }
                return "(lambda " + pyName + ": (lambda " + String.join(", ", lambdaArgs) + ": ("
                       + render(body, child) + "))(" + String.join(", ", callArgs) + "))";
            }
            var args = parseDictPipeArguments(argumentName);
            if (args.length == 2) {
                var first = pyIdentifier(args[0]);
                var second = pyIdentifier(args[1]);
                var child = scope.bind(args[0], first).bind(args[1], second);
                return "(lambda " + first + ", " + second + ": (" + render(body, child) + "))";
            }
            var pyName = pyIdentifier(argumentName);
            var child = scope.bind(argumentName, pyName);
            return "(lambda " + pyName + ": (" + render(body, child) + "))";
        }

        private String renderEffectBind(CompiledEffectBindExpression bind, Scope scope) {
            var pyName = scope.reserve(bind.name());
            var child = scope.bind(bind.name(), pyName);
            return "capy.effect_bind(" + render(bind.source(), scope)
                   + ", lambda " + pyName + ": (" + render(bind.rest(), child) + "))";
        }

        private String renderReflection(CompiledReflectionValue reflectionValue, Scope scope) {
            var target = render(reflectionValue.target(), scope);
            return "capy.reflection(" + target + ", "
                   + pyString(reflectionValue.name()) + ", "
                   + pyString(reflectionValue.packageName()) + ", "
                   + pyString(reflectionValue.packagePath()) + ", "
                   + pyArray(reflectionValue.fields().stream().map(CompiledReflectionValue.Field::name).toList())
                   + ")";
        }

        private String renderBoolean(CompiledExpression expression, Scope scope) {
            if (expression.type() == PrimitiveLinkedType.BOOL) {
                return render(expression, scope);
            }
            return "capy.truthy(" + render(expression, scope) + ")";
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
            if (leftType == PrimitiveLinkedType.STRING) {
                return "(" + right + " in str(" + left + "))";
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
                        ? "capy.list_plus(" + left + ", " + right + ")"
                        : "capy.list_append(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledSet) {
                return rightType instanceof CollectionLinkedType.CompiledSet
                        ? "capy.set_plus(" + left + ", " + right + ")"
                        : "capy.set_append(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledDict) {
                return rightType instanceof CompiledTupleType
                        ? "capy.dict_put(" + left + ", " + right + ")"
                        : "capy.dict_plus(" + left + ", " + right + ")";
            }
            if (leftType == PrimitiveLinkedType.STRING || rightType == PrimitiveLinkedType.STRING) {
                return "capy.to_string_value(" + left + ") + capy.to_string_value(" + right + ")";
            }
            if (resultType == PrimitiveLinkedType.LONG) {
                return "capy.long_add(" + left + ", " + right + ")";
            }
            if (resultType == PrimitiveLinkedType.INT) {
                return "capy.int_add(" + left + ", " + right + ")";
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
                        ? "capy.list_minus(" + left + ", " + right + ")"
                        : "capy.list_remove(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledSet) {
                return rightType instanceof CollectionLinkedType.CompiledSet
                        ? "capy.set_minus(" + left + ", " + right + ")"
                        : "capy.set_remove(" + left + ", " + right + ")";
            }
            if (leftType instanceof CollectionLinkedType.CompiledDict) {
                return rightType instanceof CollectionLinkedType.CompiledDict
                        ? "capy.dict_minus(" + left + ", " + right + ")"
                        : "capy.dict_remove(" + left + ", " + right + ")";
            }
            if (resultType == PrimitiveLinkedType.LONG) {
                return "capy.long_sub(" + left + ", " + right + ")";
            }
            if (resultType == PrimitiveLinkedType.INT) {
                return "capy.int_sub(" + left + ", " + right + ")";
            }
            return "((" + left + ") - (" + right + "))";
        }

        private String renderConversion(String methodName, String receiver, CompiledType receiverType, CompiledType returnType) {
            return switch (methodName) {
                case "to_int" -> returnType instanceof GenericDataType
                        ? "capy.parse_int_result(" + receiver + ")"
                        : receiverType == PrimitiveLinkedType.LONG
                                ? "capy.long_to_int(" + receiver + ")"
                                : "capy.float_to_int(" + receiver + ")";
                case "to_long" -> returnType instanceof GenericDataType
                        ? "capy.parse_long_result(" + receiver + ")"
                        : "capy.float_to_long(" + receiver + ")";
                case "to_double" -> returnType instanceof GenericDataType
                        ? "capy.parse_float_result(" + receiver + ", 'double')"
                        : "float(" + receiver + ")";
                case "to_float" -> returnType instanceof GenericDataType
                        ? "capy.parse_float_result(" + receiver + ", 'float')"
                        : "float(" + receiver + ")";
                case "to_bool" -> returnType instanceof GenericDataType
                        ? "capy.parse_bool_result(" + receiver + ")"
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

    private record RenderedPattern(String pattern, List<String> bindings, Scope scope) {
        String parameterList() {
            return String.join(", ", bindings);
        }
    }

    record Scope(Map<String, String> bindings, Set<String> usedNames) {
        static Scope root() {
            return new Scope(Map.of(), Set.of());
        }

        String resolve(String sourceName) {
            return bindings.getOrDefault(sourceName, pyIdentifier(sourceName));
        }

        Scope bind(String sourceName, String pyName) {
            var updatedBindings = new HashMap<>(bindings);
            var updatedUsedNames = new HashSet<>(usedNames);
            updatedBindings.put(sourceName, pyName);
            updatedUsedNames.add(pyName);
            return new Scope(Map.copyOf(updatedBindings), Set.copyOf(updatedUsedNames));
        }

        Scope bindExpression(String sourceName, String pyExpression) {
            var updatedBindings = new HashMap<>(bindings);
            updatedBindings.put(sourceName, pyExpression);
            return new Scope(Map.copyOf(updatedBindings), usedNames);
        }

        String reserve(String sourceName) {
            var base = pyIdentifier(sourceName);
            if (!usedNames.contains(base)) {
                return base;
            }
            var idx = 1;
            while (usedNames.contains(base + "_p" + idx)) {
                idx++;
            }
            return base + "_p" + idx;
        }
    }

    record ModuleInfo(CompiledModule module, JavaClass javaClass, String className, Path relativePath, String packageName, String packagePath) {
        static ModuleInfo from(CompiledModule module, JavaClass javaClass) {
            var packageName = javaClass.javaPackage().toString();
            var className = packageName.isBlank() ? javaClass.name().toString() : packageName + "." + javaClass.name();
            var relativePath = packageName.isBlank()
                    ? Path.of(javaClass.name() + ".py")
                    : Path.of(packageName.replace('.', '/'), javaClass.name() + ".py");
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
                        .map(javaConst -> pyIdentifier(javaConst.name()))
                        .forEach(moduleExports::add);
                module.javaClass().staticMethods().stream()
                        .filter(method -> !method.isPrivate())
                        .map(JavaMethod::name)
                        .map(PythonGenerator::pyIdentifier)
                        .forEach(moduleExports::add);
                exports.put(module.className(), Set.copyOf(moduleExports));
                localTypes.put(module.className(), Set.copyOf(moduleTypes));
                var companionClassName = module.className() + "Module";
                paths.putIfAbsent(companionClassName, module.relativePath());
                exports.putIfAbsent(companionClassName, Set.copyOf(moduleExports));
                localTypes.putIfAbsent(companionClassName, Set.copyOf(moduleTypes));
            }
            for (var module : objectOrientedModules) {
                var packageName = ObjectOrientedPythonGenerator.packageName(module.path());
                var moduleTypes = module.objectOriented().definitions().stream()
                        .map(dev.capylang.compiler.parser.ObjectOriented.TypeDeclaration::name)
                        .map(PythonGenerator::simpleTypeName)
                        .collect(java.util.stream.Collectors.toCollection(LinkedHashSet::new));
                for (var definition : module.objectOriented().definitions()) {
                    var typeName = simpleTypeName(definition.name());
                    var className = packageName.isBlank() ? typeName : packageName + "." + typeName;
                    paths.put(className, ObjectOrientedPythonGenerator.relativePath(module, typeName));
                    exports.put(className, Set.of(typeName));
                    localTypes.put(className, Set.copyOf(moduleTypes));
                    fields.put(typeName, definition.members().stream()
                            .filter(dev.capylang.compiler.parser.ObjectOriented.FieldDeclaration.class::isInstance)
                            .map(dev.capylang.compiler.parser.ObjectOriented.FieldDeclaration.class::cast)
                            .map(dev.capylang.compiler.parser.ObjectOriented.FieldDeclaration::name)
                            .toList());
                    parentTypes.put(typeName, definition.parents().stream()
                            .map(dev.capylang.compiler.parser.ObjectOriented.TypeReference::name)
                            .map(PythonGenerator::simpleTypeName)
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
                        imported.putIfAbsent(pyIdentifier(memberName), resolvedClassName);
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
            var normalized = pyIdentifier(memberName);
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
            var raw = JavaScriptGenerator.rawSimpleTypeName(typeName);
            var candidates = List.of(simpleTypeName(raw), enumValueIdentifier(raw), simpleTypeName(typeName));
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
            if (simple.contains("__local_const_")) {
                return pyIdentifier(simple);
            }
            if (simple.contains("__") && isValidPyIdentifier(simple)) {
                return pyIdentifier(simple);
            }
            var overrideBySimpleName = findOverrideBySimpleName(functionNameOverrides, name, parameterSignature);
            return overrideBySimpleName.orElseGet(() -> pyIdentifier(simple));
        }

        private static Map<String, Set<String>> runtimeExports() {
            var exports = new LinkedHashMap<String, Set<String>>();
            exports.put("capy.lang.Option", Set.of("Some", "None_"));
            exports.put("capy.lang.Result", Set.of("Success", "Error"));
            exports.put("capy.lang.Effect", Set.of("pure", "delay"));
            exports.put("capy.lang.Program", Set.of("Success", "Failed"));
            exports.put("capy.lang.Primitives", Set.of(
                    "to_int", "toInt", "to_long", "toLong", "to_double", "toDouble", "to_float", "toFloat", "to_bool", "toBool",
                    "mAXINTVALUE", "MAX_INT_VALUE", "mININTVALUE", "MIN_INT_VALUE",
                    "mAXLONGVALUE", "MAX_LONG_VALUE", "mINLONGVALUE", "MIN_LONG_VALUE",
                    "fLOATBOUND", "FLOAT_BOUND", "dOUBLEBOUND", "DOUBLE_BOUND",
                    "fLOATBOUNDASFLOAT", "FLOAT_BOUND_AS_FLOAT", "dOUBLEBOUNDASDOUBLE", "DOUBLE_BOUND_AS_DOUBLE",
                    "clamp_long_to_int", "clampLongToInt", "safe_long_to_int", "safeLongToInt"
            ));
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
            exports.put("capy.date_time.DateModule", Set.of("Date", "__constructor__data__Date", "capy__constructorDataDate", "UNIX_DATE", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.TimeModule", Set.of("Time", "__constructor__data__Time", "capy__constructorDataTime", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.DurationModule", Set.of("DateDuration", "WeekDuration", "ZERO", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.DateTimeModule", Set.of("DateTime", "UNIX_EPOCH", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.Interval", Set.of("DateTimeDurationEnd", "DateTimeStartDuration", "DateTimeStartEnd", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.Clock", Set.of("now"));
            exports.put("capy.test.Assert", Set.of("assert_all", "assertAll", "assert_that", "assertThat"));
            exports.put("capy.test.CapyTest", Set.of("test", "test_file", "testFile"));
            return exports;
        }

        private static Map<String, List<String>> standardFields() {
            var fields = new LinkedHashMap<String, List<String>>();
            fields.put("Some", List.of("value"));
            fields.put("None_", List.of());
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
                    new GeneratedModule(Path.of("capy", "lang", "Option.py"), runtimeForwarder("Some", "None_")),
                    new GeneratedModule(Path.of("capy", "lang", "Result.py"), runtimeForwarder("Success", "Error")),
                    new GeneratedModule(Path.of("capy", "lang", "Effect.py"), runtimeForwarder("pure", "delay")),
                    new GeneratedModule(Path.of("capy", "lang", "Program.py"), programRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "Primitives.py"), primitivesRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "String.py"), stringRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "RegexModule.py"), regexRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "Seq.py"), seqRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "System.py"), systemRuntime()),
                    new GeneratedModule(Path.of("capy", "lang", "Math.py"), mathRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "List.py"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Set.py"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Dict.py"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "collection", "Tuple.py"), collectionRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "Console.py"), consoleRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "Stdout.py"), stdoutRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "IO.py"), ioRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DateModule.py"), dateRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "TimeModule.py"), timeRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DurationModule.py"), durationRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DateTimeModule.py"), dateTimeRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "Interval.py"), intervalRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "Clock.py"), clockRuntime()),
                    new GeneratedModule(Path.of("capy", "test", "Assert.py"), assertRuntime()),
                    new GeneratedModule(Path.of("capy", "test", "CapyTest.py"), capyTestRuntime())
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
                    "capy.date_time.Interval",
                    "capy.date_time.Clock",
                    "capy.test.Assert",
                    "capy.test.CapyTest"
            );
        }

        private static String runtimeForwarder(String... names) {
            return "# Generated by Capybara. Do not edit.\n"
                   + "from dev.capylang.capybara import " + String.join(", ", names) + "\n";
        }

        private static String primitivesRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy

                    mAXINTVALUE = 2147483647
                    MAX_INT_VALUE = mAXINTVALUE
                    mININTVALUE = -2147483648
                    MIN_INT_VALUE = mININTVALUE
                    mAXLONGVALUE = 9223372036854775807
                    MAX_LONG_VALUE = mAXLONGVALUE
                    mINLONGVALUE = -9223372036854775808
                    MIN_LONG_VALUE = mINLONGVALUE
                    fLOATBOUND = 16777216
                    FLOAT_BOUND = fLOATBOUND
                    dOUBLEBOUND = 9007199254740992
                    DOUBLE_BOUND = dOUBLEBOUND
                    fLOATBOUNDASFLOAT = 16777216.0
                    FLOAT_BOUND_AS_FLOAT = fLOATBOUNDASFLOAT
                    dOUBLEBOUNDASDOUBLE = 9007199254740992.0
                    DOUBLE_BOUND_AS_DOUBLE = dOUBLEBOUNDASDOUBLE

                    to_int = capy.parse_int_result
                    toInt = capy.parse_int_result
                    to_long = capy.parse_long_result
                    toLong = capy.parse_long_result
                    to_double = lambda value: capy.parse_float_result(value, 'double')
                    toDouble = to_double
                    to_float = lambda value: capy.parse_float_result(value, 'float')
                    toFloat = to_float
                    to_bool = capy.parse_bool_result
                    toBool = capy.parse_bool_result
                    clamp_long_to_int = capy.clamp_long_to_int
                    clampLongToInt = capy.clamp_long_to_int
                    safe_long_to_int = capy.safe_long_to_int
                    safeLongToInt = capy.safe_long_to_int
                    """;
        }

        private static String programRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy

                    class Success:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Success'
                            self.__capybaraTypes = ['Success', 'Program']
                            self.results = fields.get('results', [])
                        def __str__(self):
                            return capy.data_to_string(self)
                        def capybaraDataValueInfo(self):
                            return capy.data_value_info(self, 'Success', 'capy.lang', 'capy/lang/Program', ['results'])

                    class Failed:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Failed'
                            self.__capybaraTypes = ['Failed', 'Program']
                            self.exitCode = fields.get('exitCode', 1)
                            self.errors = fields.get('errors', [])
                        def __str__(self):
                            return capy.data_to_string(self)
                        def capybaraDataValueInfo(self):
                            return capy.data_value_info(self, 'Failed', 'capy.lang', 'capy/lang/Program', ['exitCode', 'errors'])
                    """;
        }

        private static String stringRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy

                    length = lambda value: len(str(value))
                    get = lambda value, start, end=None: capy.get_index(value, start) if end is None else capy.slice_(value, start, end)
                    replace = lambda value, oldValue, newValue: str(value).replace(oldValue, newValue)
                    is_empty = lambda value: len(str(value)) == 0
                    plus = lambda left, right: capy.to_string_value(left) + capy.to_string_value(right)
                    contains = lambda value, part: part in str(value)
                    starts_with = lambda value, part: str(value).startswith(part)
                    end_with = lambda value, part: str(value).endswith(part)
                    trim = lambda value: str(value).strip()
                    """;
        }

        private static String collectionRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy

                    size = capy.size
                    get = lambda value, start, end=None: capy.get_index(value, start) if end is None else capy.slice_(value, start, end)
                    entries = capy.entries
                    to_list = capy.list_
                    is_empty = lambda value: capy.size(value) == 0
                    plus = capy.collection_plus
                    minus = capy.collection_minus
                    contains = capy.contains
                    contains_key = lambda dict_value, key: dict_value.has(key)
                    any = capy.any_
                    all = capy.all_
                    map = capy.map_collection
                    filter = capy.filter_collection
                    reject = capy.reject_collection
                    flat_map = capy.flat_map_collection
                    flatMap = capy.flat_map_collection
                    reduce = capy.reduce_collection
                    """;
        }

        private static String regexRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    fromLiteral = capy.regex_from_literal
                    """;
        }

        private static String seqRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    to_seq = capy.list_
                    toSeq = capy.list_
                    """;
        }

        private static String systemRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    current_millis = capy.current_millis
                    currentMillis = capy.current_millis
                    nano_time = capy.nano_time
                    nanoTime = capy.nano_time
                    """;
        }

        private static String mathRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import math as __math
                    digits = lambda value: [int(ch) for ch in str(abs(value))]
                    floor_div = lambda left, right: left // right
                    floorDiv = floor_div
                    floor_mod = lambda left, right: left % right
                    floorMod = floor_mod
                    min = lambda left, right: left if left < right else right
                    max = lambda left, right: left if left > right else right
                    """;
        }

        private static String consoleRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import sys
                    import dev.capylang.capybara as capy
                    print = lambda value: (__builtins__['print'](capy.to_string_value(value), end='') if isinstance(__builtins__, dict) else __builtins__.print(capy.to_string_value(value), end=''))
                    println = lambda value='': (__builtins__['print'](capy.to_string_value(value)) if isinstance(__builtins__, dict) else __builtins__.print(capy.to_string_value(value)))
                    print_error = lambda value: sys.stderr.write(capy.to_string_value(value))
                    printError = print_error
                    println_error = lambda value='': sys.stderr.write(capy.to_string_value(value) + '\\n')
                    printlnError = println_error
                    read_line = lambda: input()
                    readLine = read_line
                    """;
        }

        private static String stdoutRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    print = lambda value: __builtins__['print'](capy.to_string_value(value), end='') if isinstance(__builtins__, dict) else __builtins__.print(capy.to_string_value(value), end='')
                    println = lambda value='': __builtins__['print'](capy.to_string_value(value)) if isinstance(__builtins__, dict) else __builtins__.print(capy.to_string_value(value))
                    """;
        }

        private static String ioRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import shutil
                    from pathlib import Path

                    read_text = lambda path: Path(path).read_text()
                    readText = read_text
                    read_lines = lambda path: Path(path).read_text().splitlines()
                    readLines = read_lines
                    read_bytes = lambda path: list(Path(path).read_bytes())
                    readBytes = read_bytes
                    write_text = lambda path, text: Path(path).write_text(text)
                    writeText = write_text
                    write_lines = lambda path, lines: Path(path).write_text('\\n'.join(lines))
                    writeLines = write_lines
                    write_bytes = lambda path, values: Path(path).write_bytes(bytes(values))
                    writeBytes = write_bytes
                    append_text = lambda path, text: Path(path).open('a').write(text)
                    appendText = append_text
                    append_lines = lambda path, lines: Path(path).open('a').write('\\n'.join(lines))
                    appendLines = append_lines
                    append_bytes = lambda path, values: Path(path).open('ab').write(bytes(values))
                    appendBytes = append_bytes
                    exists = lambda path: Path(path).exists()
                    is_file = lambda path: Path(path).is_file()
                    isFile = is_file
                    is_directory = lambda path: Path(path).is_dir()
                    isDirectory = is_directory
                    size = lambda path: Path(path).stat().st_size
                    create_file = lambda path: Path(path).touch()
                    createFile = create_file
                    create_directory = lambda path: Path(path).mkdir(exist_ok=True)
                    createDirectory = create_directory
                    create_directories = lambda path: Path(path).mkdir(parents=True, exist_ok=True)
                    createDirectories = create_directories
                    list_entries = lambda path: [str(item) for item in Path(path).iterdir()]
                    listEntries = list_entries
                    delete = lambda path: Path(path).unlink()
                    delete_ = delete
                    copy = lambda source, target: shutil.copyfile(source, target)
                    copy_replace = copy
                    copyReplace = copy
                    move = lambda source, target: shutil.move(source, target)
                    move_replace = move
                    moveReplace = move
                    """;
        }

        private static String dateRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    Date = capy.Date
                    __constructor__data__Date = capy.make_date
                    capy__constructorDataDate = capy.make_date
                    JANUARY = jANUARY = 1
                    FEBRUARY = fEBRUARY = 2
                    MARCH = mARCH = 3
                    APRIL = aPRIL = 4
                    MAY = mAY = 5
                    JUNE = jUNE = 6
                    JULY = jULY = 7
                    AUGUST = aUGUST = 8
                    SEPTEMBER = sEPTEMBER = 9
                    OCTOBER = oCTOBER = 10
                    NOVEMBER = nOVEMBER = 11
                    DECEMBER = dECEMBER = 12
                    UNIX_DATE = uNIXDATE = Date({'day': 1, 'month': 1, 'year': 1970})
                    fromIso8601 = capy.date_from_iso
                    from_iso_8601 = fromIso8601
                    """;
        }

        private static String timeRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    Time = capy.Time
                    __constructor__data__Time = capy.make_time
                    capy__constructorDataTime = capy.make_time
                    MIDNIGHT = mIDNIGHT = Time({'hour': 0, 'minute': 0, 'second': 0, 'offset_minutes': capy.None_})
                    NOON = nOON = Time({'hour': 12, 'minute': 0, 'second': 0, 'offset_minutes': capy.None_})
                    fromIso8601 = capy.time_from_iso
                    from_iso_8601 = fromIso8601
                    """;
        }

        private static String durationRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    DateDuration = capy.DateDuration
                    WeekDuration = capy.WeekDuration
                    ZERO = zERO = capy.ZERO
                    fromIso8601 = capy.duration_from_iso
                    from_iso_8601 = fromIso8601
                    """;
        }

        private static String dateTimeRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    DateTime = capy.DateTime
                    UNIX_EPOCH = uNIXEPOCH = DateTime({'date': capy.Date({'day': 1, 'month': 1, 'year': 1970}), 'time': capy.Time({'hour': 0, 'minute': 0, 'second': 0, 'offset_minutes': capy.None_})})
                    fromIso8601 = capy.date_time_from_iso
                    from_iso_8601 = fromIso8601
                    """;
        }

        private static String intervalRuntime() {
            return "# Generated by Capybara. Do not edit.\nimport dev.capylang.capybara as capy\nDateTimeDurationEnd = capy.DateTimeDurationEnd\nDateTimeStartDuration = capy.DateTimeStartDuration\nDateTimeStartEnd = capy.DateTimeStartEnd\nfromIso8601 = capy.interval_from_iso\nfrom_iso_8601 = fromIso8601\n";
        }

        private static String clockRuntime() {
            return "# Generated by Capybara. Do not edit.\nimport dev.capylang.capybara as capy\nnow = capy.clock_now\n";
        }

        private static String assertRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    assert_that = capy.assert_that
                    assertThat = assert_that
                    assert_all = capy.assert_all
                    assertAll = assert_all
                    def __getattr__(name):
                        if name.startswith('assertThat') or name.startswith('assert_that'):
                            return assert_that
                        if name.startswith('assertAll') or name.startswith('assert_all'):
                            return assert_all
                        raise AttributeError(name)
                    """;
        }

        private static String capyTestRuntime() {
            return """
                    # Generated by Capybara. Do not edit.
                    import dev.capylang.capybara as capy
                    test = lambda name, body: capy.delay(lambda: {'name': name, 'body': body})
                    test_file = lambda path, test_cases: capy.delay(lambda: {'path': path, 'test_cases': test_cases})
                    testFile = test_file
                    """;
        }

        private static String runtime() {
            return String.join("", """
                    # Generated by Capybara. Do not edit.
                    import inspect
                    import math
                    import re
                    import sys
                    import time
                    from types import SimpleNamespace

                    LONG_MIN = -9223372036854775808
                    LONG_MAX = 9223372036854775807
                    INT_MIN = -2147483648
                    INT_MAX = 2147483647

                    def _fields(value):
                        return {k: v for k, v in vars(value).items() if not k.startswith('_')}

                    def capy_type(value):
                        if hasattr(value, '__capybaraType'):
                            return getattr(value, '__capybaraType')
                        if hasattr(value, '__dict__'):
                            for key, item in vars(value).items():
                                if key.endswith('__capybaraType'):
                                    return item
                        for key, item in vars(type(value)).items():
                            if key.endswith('__capybaraType'):
                                return item
                        return None

                    def capy_types(value):
                        if hasattr(value, '__capybaraTypes'):
                            return getattr(value, '__capybaraTypes')
                        if hasattr(value, '__dict__'):
                            for key, item in vars(value).items():
                                if key.endswith('__capybaraTypes'):
                                    return item
                        for key, item in vars(type(value)).items():
                            if key.endswith('__capybaraTypes'):
                                return item
                        return []
                    """, """
                    class Some:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Some'
                            self.__capybaraTypes = ['Some', 'Option']
                            self.value = fields.get('value')
                        def map(self, mapper): return option_map(self, mapper)
                        def pipe(self, mapper): return option_map(self, mapper)
                        def flat_map(self, mapper): return option_flat_map(self, mapper)
                        def flatMap(self, mapper): return option_flat_map(self, mapper)
                        def pipe_star(self, mapper): return option_flat_map(self, mapper)
                        def pipeStar(self, mapper): return option_flat_map(self, mapper)
                        def filter(self, predicate): return self if predicate(self.value) else None_
                        def reduce(self, initial, reducer): return invoke(reducer, initial, self.value)
                        def reduceLeft(self, initial, reducer): return self.reduce(initial, reducer)
                        def __str__(self): return data_to_string(self)
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'Some', 'capy.lang', 'capy/lang/Option', ['value'])

                    class _NoneValue:
                        __capybaraType = 'None'
                        __capybaraTypes = ['None', 'None_', 'Option']
                        def map(self, mapper=None): return self
                        def pipe(self, mapper=None): return self
                        def flat_map(self, mapper=None): return self
                        def flatMap(self, mapper=None): return self
                        def pipe_star(self, mapper=None): return self
                        def pipeStar(self, mapper=None): return self
                        def filter(self, predicate=None): return self
                        def reduce(self, initial, reducer=None): return initial
                        def reduceLeft(self, initial, reducer=None): return initial
                        def __str__(self): return 'None { }'
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'None', 'capy.lang', 'capy/lang/Option', [])

                    None_ = _NoneValue()

                    class Success:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Success'
                            self.__capybaraTypes = ['Success', 'Result']
                            self.value = fields.get('value', fields.get('results'))
                            self.results = fields.get('results', self.value)
                        def map(self, mapper): return invoke(mapper, self.value)
                        def pipe(self, mapper): return invoke(mapper, self.value)
                        def flat_map(self, mapper):
                            mapped = invoke(mapper, self.value)
                            return mapped.value if is_success_like(mapped) else mapped
                        def flatMap(self, mapper): return self.flat_map(mapper)
                        def pipe_star(self, mapper): return self.flat_map(mapper)
                        def pipeStar(self, mapper): return self.flat_map(mapper)
                        def pipeGreater(self, success_mapper, error_mapper=None):
                            if error_mapper is None and isinstance(success_mapper, (list, tuple)):
                                success_mapper, error_mapper = success_mapper
                            return self.reduce(success_mapper, error_mapper)
                        def pipe_greater(self, success_mapper, error_mapper=None): return self.pipeGreater(success_mapper, error_mapper)
                        def orElse(self, value=None): return self.value
                        def or_else(self, value=None): return self.value
                        def or_(self, result=None): return self
                        def reduce(self, success_mapper, error_mapper=None): return invoke(success_mapper, self.value)
                        def reduceLeft(self, success_mapper, error_mapper=None): return self.reduce(success_mapper, error_mapper)
                        def __str__(self): return data_to_string(self)
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'Success', 'capy.lang', 'capy/lang/Result', ['value'])

                    class Error:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Error'
                            self.__capybaraTypes = ['Error', 'Result']
                            self.message = fields.get('message', fields.get('ex'))
                            self.ex = self.message
                        def map(self, mapper=None): return self
                        def pipe(self, mapper=None): return self
                        def flat_map(self, mapper=None): return self
                        def flatMap(self, mapper=None): return self
                        def pipe_star(self, mapper=None): return self
                        def pipeStar(self, mapper=None): return self
                        def pipeGreater(self, success_mapper, error_mapper=None):
                            if error_mapper is None and isinstance(success_mapper, (list, tuple)):
                                success_mapper, error_mapper = success_mapper
                            return self.reduce(success_mapper, error_mapper)
                        def pipe_greater(self, success_mapper, error_mapper=None): return self.pipeGreater(success_mapper, error_mapper)
                        def orElse(self, value=None): return value
                        def or_else(self, value=None): return value
                        def or_(self, result=None): return result
                        def reduce(self, success_mapper, error_mapper): return invoke(error_mapper, self.message)
                        def reduceLeft(self, success_mapper, error_mapper): return self.reduce(success_mapper, error_mapper)
                        def __str__(self): return data_to_string(self)
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'Error', 'capy.lang', 'capy/lang/Result', ['message'])

                    class Effect:
                        def __init__(self, thunk):
                            self.thunk = thunk
                            self.__capybaraType = '_UnsafeEffect'
                            self.__capybaraTypes = ['_UnsafeEffect', 'Effect']
                        def unsafe_run(self): return self.thunk()
                        def unsafeRun(self): return self.unsafe_run()
                        def map(self, mapper): return delay(lambda: mapper(self.unsafe_run()))
                        def pipe(self, mapper): return self.map(mapper)
                        def flat_map(self, mapper): return delay(lambda: mapper(self.unsafe_run()).unsafe_run())
                        def flatMap(self, mapper): return self.flat_map(mapper)
                        def pipe_star(self, mapper): return self.flat_map(mapper)
                        def pipeStar(self, mapper): return self.flat_map(mapper)

                    def delay(thunk): return Effect(thunk)
                    def pure(value): return delay(lambda: value)
                    def is_effect(value): return isinstance(value, Effect) or callable(getattr(value, 'unsafe_run', None))

                    def invoke(fn, *args):
                        result = fn
                        for arg in args:
                            if not callable(result):
                                return result
                            result = result(arg)
                        return result

                    def let_(value, mapper):
                        return mapper(value)

                    def effect_bind(source, mapper):
                        return delay(lambda: mapper(source.unsafe_run()).unsafe_run())

                    class CapyList(list):
                        @property
                        def size(self): return len(self)
                        def as_list(self): return self
                        def asList(self): return self
                        def to_list(self): return self
                        def toList(self): return self
                        def pipe(self, mapper): return map_collection(self, mapper)
                        def pipeStar(self, mapper): return flat_map_collection(self, mapper)
                        def pipe_star(self, mapper): return self.pipeStar(mapper)
                        def pipeMinus(self, predicate): return filter_collection(self, predicate)
                        def pipe_minus(self, predicate): return self.pipeMinus(predicate)
                        def pipeGreater(self, initial, reducer): return reduce_collection(self, initial, reducer)
                        def pipe_greater(self, initial, reducer): return self.pipeGreater(initial, reducer)

                    class CapySet:
                        def __init__(self, values=None):
                            self.values = []
                            for value in values or []:
                                self.add(value)
                        def add(self, value):
                            if not contains(self, value):
                                self.values.append(value)
                            return self
                        def __iter__(self): return iter(self.values)
                        def __len__(self): return len(self.values)
                        @property
                        def size(self): return len(self.values)
                        def as_list(self): return CapyList(self.values)
                        def asList(self): return self.as_list()
                        def to_list(self): return self.as_list()
                        def toList(self): return self.as_list()
                        def __str__(self): return to_string_value(self)

                    class CapyDict:
                        def __init__(self, entries=None):
                            self.items = []
                            for key, value in entries or []:
                                self.set(key, value)
                        def set(self, key, value):
                            for idx, (existing, _) in enumerate(self.items):
                                if equals(existing, key):
                                    self.items[idx] = (key, value)
                                    return self
                            self.items.append((key, value))
                            return self
                        def get(self, key, default=None):
                            for existing, value in self.items:
                                if equals(existing, key):
                                    return value
                            return default
                        def has(self, key):
                            return any(equals(existing, key) for existing, _ in self.items)
                        def containsKey(self, key): return self.has(key)
                        def contains_key(self, key): return self.has(key)
                        def delete(self, key):
                            self.items = [(existing, value) for existing, value in self.items if not equals(existing, key)]
                            return self
                        def entries(self): return CapyList([CapyList([key, value]) for key, value in self.items])
                        def keys(self): return CapyList([key for key, _ in self.items])
                        def values(self): return CapyList([value for _, value in self.items])
                        def __iter__(self): return iter(self.items)
                        def __len__(self): return len(self.items)
                        @property
                        def size(self): return len(self.items)
                        def __str__(self): return to_string_value(self)

                    def list_(values=None): return CapyList(values or [])
                    def set_(values=None): return CapySet(values)
                    def dict_(entries=None): return CapyDict(entries)
                    def seq(values, mapper=None): return list(values or []) if mapper is None else [mapper(v) for v in values or []]

                    def enum_value(name, owner, parents=None, ordinal=0, aliases=None, package_name='', package_path=None):
                        parents = parents or []
                        aliases = aliases or []
                        package_path = package_path or owner
                        value = SimpleNamespace(
                            __capybaraType=name,
                            __capybaraTypes=[name] + aliases + [owner] + parents,
                            __capybaraEnum=True,
                            name=name,
                            ordinal=ordinal,
                            order=ordinal,
                        )
                        value.toString = lambda: name
                        value.capybaraDataValueInfo = lambda: data_value_info(value, name, package_name, package_path, [])
                        if owner == 'End' and 'Seq' in parents:
                            value.plus = lambda other: other
                            value.any = lambda pred: False
                            value.asList = lambda: CapyList([])
                            value.as_list = value.asList
                            value.drop = lambda n: value
                            value.dropUntil = lambda pred: value
                            value.drop_until = value.dropUntil
                            value.filter = lambda pred: value
                            value.first = lambda: None_
                            value.firstMatch = lambda pred: None_
                            value.first_match = value.firstMatch
                            value.flatMap = lambda mapper: value
                            value.flat_map = value.flatMap
                            value.map = lambda mapper: value
                            value.pipe = value.map
                            value.pipeStar = value.flatMap
                            value.pipe_star = value.flatMap
                            value.pipeMinus = value.filter
                            value.pipe_minus = value.filter
                            value.pipeGreater = lambda initial, reducer: initial
                            value.pipe_greater = value.pipeGreater
                            value.reduce = lambda initial, reducer: initial
                            value.reduceLeft = value.reduce
                            value.reduce_left = value.reduce
                            value.reject = lambda pred: value
                            value.take = lambda n: CapyList([])
                            value.takeLast = lambda n: CapyList([])
                            value.take_last = value.takeLast
                            value.until = lambda pred: value
                            value.zip = lambda other: value
                        return value

                    def is_type(value, type_name):
                        if type_name in ('BYTE', 'INT'): return isinstance(value, int) and not isinstance(value, bool)
                        if type_name == 'LONG': return isinstance(value, int) and not isinstance(value, bool)
                        if type_name in ('FLOAT', 'DOUBLE'): return isinstance(value, (int, float)) and not isinstance(value, bool)
                        if type_name == 'STRING': return isinstance(value, str)
                        if type_name == 'BOOL': return isinstance(value, bool)
                        if type_name == 'List': return isinstance(value, list)
                        if type_name == 'Set': return isinstance(value, CapySet)
                        if type_name == 'Dict': return isinstance(value, CapyDict)
                        if type_name == 'DATA': return bool(capy_types(value))
                        if type_name == 'ENUM': return bool(getattr(value, '__capybaraEnum', False))
                        types = capy_types(value)
                        return type_name in types or str(type_name).lower() in [str(item).lower() for item in types]

                    isType = is_type

                    def normalize_index(index, size):
                        return size + index if index < 0 else index

                    def raw_index(source, index):
                        return source[normalize_index(index, len(source))]

                    def get_index(source, index):
                        if isinstance(source, CapyDict):
                            return Some({'value': source.get(index)}) if source.has(index) else None_
                        value = str(source) if isinstance(source, str) else source
                        normalized = normalize_index(index, len(value))
                        if normalized < 0 or normalized >= len(value):
                            return None_
                        return Some({'value': value[normalized]})

                    def slice_(source, start=None, end=None):
                        value = str(source) if isinstance(source, str) else source
                        size_value = len(value)
                        from_index = 0 if start is None else normalize_index(start, size_value)
                        to_index = size_value if end is None else normalize_index(end, size_value)
                        result = value[from_index:to_index]
                        return list(result) if isinstance(result, list) else result
                    """, """
                    def equals(left, right):
                        if left is right: return True
                        if isinstance(left, (int, float, str, bool)) or left is None:
                            return left == right
                        if isinstance(left, (list, tuple)) and isinstance(right, (list, tuple)):
                            return len(left) == len(right) and all(equals(l, r) for l, r in zip(left, right))
                        if isinstance(left, CapySet) and isinstance(right, CapySet):
                            return len(left) == len(right) and all(contains(right, value) for value in left)
                        if isinstance(left, CapyDict) and isinstance(right, CapyDict):
                            return len(left) == len(right) and all(right.has(k) and equals(v, right.get(k)) for k, v in left.entries())
                        if capy_type(left) is not None and capy_type(left) == capy_type(right):
                            return equals(_fields(left), _fields(right))
                        if isinstance(left, dict) and isinstance(right, dict):
                            return left.keys() == right.keys() and all(equals(left[k], right[k]) for k in left)
                        return left == right

                    def contains(collection, value):
                        if hasattr(collection, '__capybaraRegex') and callable(getattr(collection, 'matches', None)):
                            return collection.matches(value)
                        if isinstance(collection, CapySet):
                            return any(equals(item, value) for item in collection)
                        if isinstance(collection, CapyDict):
                            return collection.has(value)
                        if isinstance(collection, str):
                            return value in collection
                        return any(equals(item, value) for item in collection)

                    def truthy(value):
                        if value is False or value is None: return False
                        if isinstance(value, (int, float)): return value != 0
                        if isinstance(value, (str, list, CapySet, CapyDict)): return size(value) > 0
                        return True

                    def size(value):
                        if value is None: return 0
                        if isinstance(value, (CapySet, CapyDict)): return value.size
                        return len(value)

                    def entries(value):
                        if is_type(value, 'Some'): return [value.value]
                        if is_type(value, 'None_') or is_type(value, 'Error'): return []
                        if is_type(value, 'Success'): return [value.value]
                        if isinstance(value, CapyDict): return value.entries()
                        if isinstance(value, CapySet): return list(value)
                        if isinstance(value, str): return list(value)
                        as_list = getattr(value, 'asList', None) or getattr(value, 'as_list', None)
                        if callable(as_list): return as_list()
                        return list(value)

                    def list_append(value_list, value): return CapyList(list(value_list) + [value])
                    def list_plus(left, right): return CapyList(list(left) + list(right))
                    def list_remove(value_list, value): return CapyList([item for item in value_list if not equals(item, value)])
                    def list_minus(left, right): return CapyList([item for item in left if not contains(right, item)])
                    def set_append(value_set, value): return set_(list(value_set) + [value])
                    def set_plus(left, right): return set_(list(left) + list(right))
                    def set_remove(value_set, value): return set_([item for item in value_set if not equals(item, value)])
                    def set_minus(left, right): return set_([item for item in left if not contains(right, item)])
                    def set_is_subset_of(left, right): return all(contains(right, item) for item in left)
                    def set_is_proper_subset_of(left, right): return set_is_subset_of(left, right) and len(left) < len(right)
                    def set_is_superset_of(left, right): return set_is_subset_of(right, left)
                    def set_is_proper_superset_of(left, right): return set_is_subset_of(right, left) and len(left) > len(right)
                    def set_intersection(left, right): return set_([item for item in left if contains(right, item)])
                    def set_symmetric_difference(left, right): return set_plus(set_minus(left, right), set_minus(right, left))
                    def set_cartesian_product(left, right): return set_([[l, r] for l in left for r in right])
                    def set_power_set(value_set):
                        subsets = [set_()]
                        for item in value_set:
                            subsets += [set_append(subset, item) for subset in subsets]
                        return set_(subsets)
                    def dict_put(dict_value, tuple_value): return dict_(dict_value.entries() + [(tuple_value[0], tuple_value[1])])
                    def dict_plus(left, right): return dict_(left.entries() + right.entries())
                    def dict_remove(dict_value, key): return dict_([(k, v) for k, v in dict_value.entries() if not equals(k, key)])
                    def dict_minus(left, right): return dict_([(k, v) for k, v in left.entries() if not right.has(k)])

                    def collection_plus(left, right):
                        if isinstance(left, list): return list_plus(left, right) if isinstance(right, list) else list_append(left, right)
                        if isinstance(left, CapySet): return set_plus(left, right) if isinstance(right, CapySet) else set_append(left, right)
                        if isinstance(left, CapyDict): return dict_put(left, right) if isinstance(right, list) else dict_plus(left, right)
                        return left + right

                    def collection_minus(left, right):
                        if isinstance(left, list): return list_minus(left, right) if isinstance(right, list) else list_remove(left, right)
                        if isinstance(left, CapySet): return set_minus(left, right) if isinstance(right, CapySet) else set_remove(left, right)
                        if isinstance(left, CapyDict): return dict_minus(left, right) if isinstance(right, CapyDict) else dict_remove(left, right)
                        return left - right

                    def map_collection(value, mapper):
                        if is_type(value, 'Some'): return option_map(value, mapper)
                        if is_type(value, 'None_') or is_type(value, 'Error'): return value
                        if is_type(value, 'Success'): return invoke(mapper, value.value)
                        if isinstance(value, CapyDict): return dict_([(key, invoke(mapper, key, item)) for key, item in value.entries()])
                        if isinstance(value, CapySet): return set_([invoke(mapper, item, index) for index, item in enumerate(value)])
                        return CapyList([invoke(mapper, item, index) for index, item in enumerate(entries(value))])

                    def filter_collection(value, predicate):
                        if is_type(value, 'Some'): return value if predicate(value.value) else None_
                        if is_type(value, 'None_') or is_type(value, 'Error'): return value
                        if isinstance(value, CapyDict): return dict_([(key, item) for key, item in value.entries() if invoke(predicate, key, item)])
                        if isinstance(value, CapySet): return set_([item for index, item in enumerate(value) if invoke(predicate, item, index)])
                        return CapyList([item for index, item in enumerate(entries(value)) if invoke(predicate, item, index)])

                    def reject_collection(value, predicate):
                        if is_type(value, 'Some'): return None_ if predicate(value.value) else value
                        if is_type(value, 'None_') or is_type(value, 'Error'): return value
                        if isinstance(value, CapyDict): return dict_([(key, item) for key, item in value.entries() if not invoke(predicate, key, item)])
                        if isinstance(value, CapySet): return set_([item for index, item in enumerate(value) if not invoke(predicate, item, index)])
                        return CapyList([item for index, item in enumerate(entries(value)) if not invoke(predicate, item, index)])

                    def flat_map_collection(value, mapper):
                        if is_type(value, 'Some'): return option_flat_map(value, mapper)
                        if is_type(value, 'None_') or is_type(value, 'Error'): return value
                        if is_type(value, 'Success'): return invoke(mapper, value.value)
                        mapped = []
                        for index, item in enumerate(entries(value)):
                            result = invoke(mapper, item, index)
                            if isinstance(result, CapySet):
                                mapped.extend(list(result))
                            elif isinstance(result, list):
                                mapped.extend(result)
                            else:
                                mapped.append(result)
                        return CapyList(mapped)

                    def any_(value, predicate):
                        if isinstance(value, CapyDict):
                            return any(invoke(predicate, key, item) for key, item in value.entries())
                        sequence_any = getattr(value, 'any', None)
                        if callable(sequence_any) and not isinstance(value, (list, CapySet, str)):
                            return sequence_any(lambda item: invoke(predicate, item))
                        return any(invoke(predicate, item, index) for index, item in enumerate(entries(value)))

                    def all_(value, predicate):
                        if isinstance(value, CapyDict):
                            return all(invoke(predicate, key, item) for key, item in value.entries())
                        sequence_any = getattr(value, 'any', None)
                        if callable(sequence_any) and not isinstance(value, (list, CapySet, str)):
                            return not sequence_any(lambda item: not invoke(predicate, item))
                        return all(invoke(predicate, item, index) for index, item in enumerate(entries(value)))

                    def reduce_collection(value, initial, reducer):
                        acc = initial
                        if isinstance(value, CapyDict):
                            for key, item in value.entries():
                                acc = invoke(reducer, acc, key, item)
                            return acc
                        for index, item in enumerate(entries(value)):
                            acc = invoke(reducer, acc, item, index)
                        return acc

                    def option_map(option, mapper): return mapper(option.value) if is_type(option, 'Some') else None_
                    def option_flat_map(option, mapper): return mapper(option.value) if is_type(option, 'Some') else None_
                    def option_filter_out(option, predicate): return None_ if is_type(option, 'Some') and predicate(option.value) else option

                    def to_int(value): return ((int(value) + 2**31) % 2**32) - 2**31
                    def to_long(value): return ((int(value) + 2**63) % 2**64) - 2**63
                    def int_add(left, right): return to_int(left + right)
                    def int_sub(left, right): return to_int(left - right)
                    def int_mul(left, right): return to_int(left * right)
                    def int_div(left, right):
                        if right == 0: raise ZeroDivisionError('/ by zero')
                        return to_int(int(left / right))
                    def int_mod(left, right):
                        if right == 0: raise ZeroDivisionError('/ by zero')
                        return to_int(left - int(left / right) * right)
                    def long_add(left, right): return to_long(left + right)
                    def long_sub(left, right): return to_long(left - right)
                    def long_mul(left, right): return to_long(left * right)
                    def long_div(left, right): return to_long(int(left / right))
                    def long_mod(left, right): return to_long(left - int(left / right) * right)
                    def long_pow(left, right): return float_to_long(math.pow(left, right))
                    def float_to_int(value):
                        if math.isnan(float(value)): return 0
                        return max(INT_MIN, min(INT_MAX, int(float(value))))
                    def long_to_int(value): return to_int(value)
                    def float_to_long(value):
                        if math.isnan(float(value)): return 0
                        return max(LONG_MIN, min(LONG_MAX, int(float(value))))
                    def clamp_long_to_int(value): return max(INT_MIN, min(INT_MAX, int(value)))
                    def safe_long_to_int(value):
                        if value > INT_MAX: return Error({'message': f'long value `{value}` is greater than max int value'})
                        if value < INT_MIN: return Error({'message': f'long value `{value}` is smaller than min int value'})
                        return Success({'value': long_to_int(value)})

                    def parse_result(value, type_name, parser):
                        try:
                            return Success({'value': parser(str(value))})
                        except Exception:
                            return Error({'message': f'Cannot parse string to {type_name}: {value}'})
                    def parse_int_result(value): return parse_result(value, 'int', lambda text: int(text) if re.fullmatch(r'[-+]?\\d+', text) else (_ for _ in ()).throw(ValueError()))
                    def parse_long_result(value):
                        def parser(text):
                            parsed = int(text) if re.fullmatch(r'[-+]?\\d+', text) else (_ for _ in ()).throw(ValueError())
                            if parsed < LONG_MIN or parsed > LONG_MAX: raise ValueError()
                            return parsed
                        return parse_result(value, 'long', parser)
                    def parse_float_result(value, type_name='float'): return parse_result(value, type_name, lambda text: float(text))
                    def parse_bool_result(value):
                        text = str(value)
                        if text == 'true': return Success({'value': True})
                        if text == 'false': return Success({'value': False})
                        return Error({'message': f'Cannot parse string to bool: {value}'})
                    def parse_enum(value, values, enum_name):
                        for item in values:
                            if item.name == value or item.ordinal == value or item.order == value:
                                return Success({'value': item})
                        return Error({'message': f'Unable to parse {enum_name} from {value}'})

                    def to_string_value(value):
                        if value is None: return ''
                        if value is True: return 'true'
                        if value is False: return 'false'
                        if isinstance(value, str): return value
                        if isinstance(value, list): return '[' + ', '.join(to_string_value(item) for item in value) + ']'
                        if isinstance(value, CapySet): return '{' + ', '.join(to_string_value(item) for item in value) + '}'
                        if isinstance(value, CapyDict): return '{' + ', '.join(f'{to_string_value(k)}: {to_string_value(v)}' for k, v in value.entries()) + '}'
                        if callable(getattr(value, 'toString', None)): return value.toString()
                        return str(value)

                    def data_value_info(target, name, package_name, package_path, fields=None):
                        fields = fields or []
                        return SimpleNamespace(
                            name=name,
                            packageName=package_name,
                            packagePath=package_path,
                            fields=[SimpleNamespace(name=field, value=getattr(target, field if hasattr(target, field) else field + '_', None)) for field in fields],
                        )

                    def data_to_string(value):
                        info = value.capybaraDataValueInfo() if callable(getattr(value, 'capybaraDataValueInfo', None)) else data_value_info(value, capy_type(value) or type(value).__name__, '', '', list(_fields(value).keys()))
                        fields = getattr(info, 'fields', [])
                        body = ', '.join(f'{field.name}: {to_string_value(field.value)}' for field in fields)
                        return f'{info.name} {{ {body} }}' if body else f'{info.name} {{ }}'

                    def reflection(target, name=None, package_name='', package_path='', field_names=None):
                        if name:
                            return data_value_info(target, name, package_name, package_path, field_names or [])
                        if callable(getattr(target, 'capybaraDataValueInfo', None)):
                            return target.capybaraDataValueInfo()
                        return data_value_info(target, name, package_name, package_path, field_names or [])

                    def is_success_like(value):
                        return hasattr(value, 'value') and 'success' in str(capy_type(value) or '').lower()
                    def is_failure_like(value):
                        lowered = str(capy_type(value) or '').lower()
                        return 'error' in lowered or 'fail' in lowered
                    def result_like_pipe(value, mapper):
                        if is_failure_like(value): return value
                        if hasattr(value, 'value'): return invoke(mapper, value.value)
                        return value
                    def result_like_flat_map(value, mapper):
                        if is_failure_like(value): return value
                        if hasattr(value, 'value'): return invoke(mapper, value.value)
                        return value

                    def pattern_value(expected): return ('value', expected)
                    def pattern_wildcard(): return ('wildcard',)
                    def pattern_bind(name): return ('bind', name)
                    def pattern_type(type_name): return ('type', type_name)
                    def pattern_typed(type_name, name): return ('typed', type_name, name)
                    def pattern_constructor(type_name, fields): return ('constructor', type_name, fields)

                    def _match_pattern(value, pattern):
                        kind = pattern[0]
                        if kind == 'value': return {} if equals(value, pattern[1]) else None
                        if kind == 'wildcard': return {}
                        if kind == 'bind': return {pattern[1]: value}
                        if kind == 'type': return {} if is_type(value, pattern[1]) else None
                        if kind == 'typed': return {pattern[2]: value} if is_type(value, pattern[1]) else None
                        if kind == 'constructor':
                            if not is_type(value, pattern[1]): return None
                            env = {}
                            for field_name, child_pattern in pattern[2]:
                                field_value = getattr(value, field_name if hasattr(value, field_name) else field_name + '_', None)
                                child_env = _match_pattern(field_value, child_pattern)
                                if child_env is None: return None
                                env.update(child_env)
                            return env
                        raise ValueError(f'Unsupported pattern: {pattern}')

                    def _call_case(fn, env):
                        signature = inspect.signature(fn)
                        return fn(**{name: env[name] for name in signature.parameters if name in env})

                    def match_value(value, cases):
                        for pattern, guard, body in cases:
                            env = _match_pattern(value, pattern)
                            if env is None:
                                continue
                            if guard is not None and not _call_case(guard, env):
                                continue
                            return _call_case(body, env)
                        raise RuntimeError('Non-exhaustive match expression')

                    def overload_score(name, args):
                        if not args: return 1
                        first = args[0]
                        lowered = name.lower()
                        if 'compileddict' in lowered: return 100 if isinstance(first, CapyDict) else -1
                        if 'compiledset' in lowered: return 100 if isinstance(first, CapySet) else -1
                        if 'compiledlist' in lowered: return 100 if isinstance(first, list) else -1
                        if 'string' in lowered: return 80 if isinstance(first, str) else -1
                        if any(token in lowered for token in ('long', 'int', 'float', 'double')): return 70 if isinstance(first, (int, float)) else -1
                        if capy_type(first) and capy_type(first).lower() in lowered: return 90
                        return 1
                    def dispatch_overload(overloads, args):
                        selected = max(overloads, key=lambda overload: overload_score(overload[0], args))
                        return selected[1](*args)

                    class CapyException(Exception):
                        def __init__(self, message, class_name='RuntimeException'):
                            super().__init__(message)
                            self.message = str(message)
                            self.class_name = class_name
                        def getMessage(self): return self.message
                        def getClass(self): return SimpleNamespace(getSimpleName=lambda: self.class_name)

                    def decorate_exception(error, class_name=None):
                        if isinstance(error, CapyException): return error
                        return CapyException(str(error), class_name or type(error).__name__)
                    def to_exception(value): return value if isinstance(value, BaseException) else CapyException(str(value))
                    def array_index_error(index): return CapyException(str(index), 'ArrayIndexOutOfBoundsException')
                    def array_get(value, index):
                        normalized = normalize_index(index, len(value))
                        if normalized < 0 or normalized >= len(value):
                            raise array_index_error(index)
                        return value[normalized]
                    def new_array(length, default_value=None): return [default_value for _ in range(length)]

                    def object_info(kind, name, pkg=None, open=False, fields=None, methods=None, parents=None, **kwargs):
                        data = dict(kind=kind, name=name, pkg=pkg or SimpleNamespace(name='', path=''), open=open, fields=fields or [], methods=methods or [], parents=parents or [])
                        data.update(kwargs)
                        return SimpleNamespace(**data)
                    def field_info(name, type): return SimpleNamespace(name=name, type=type)
                    def method_info(name, pkg=None, params=None, return_type=None): return SimpleNamespace(name=name, pkg=pkg or SimpleNamespace(name='', path=''), params=params or [], return_type=return_type)
                    def param_info(name, type): return SimpleNamespace(name=name, type=type)
                    def type_info(kind, name, pkg=None, **kwargs):
                        data = dict(kind=kind, name=name, pkg=pkg or SimpleNamespace(name='', path=''))
                        data.update(kwargs)
                        return SimpleNamespace(**data)

                    def regex_from_literal(pattern):
                        compiled = re.compile(pattern)
                        return SimpleNamespace(__capybaraRegex=True, matches=lambda value: compiled.search(str(value)) is not None)
                    def current_millis(): return int(time.time() * 1000)
                    def nano_time(): return time.time_ns()

                    def write_program_result(value):
                        if is_type(value, 'Program') and is_type(value, 'Success') and isinstance(getattr(value, 'results', None), list):
                            for result in value.results:
                                print(to_string_value(result))
                            return
                        if is_type(value, 'Program') and is_type(value, 'Failed'):
                            for error in getattr(value, 'errors', []) or []:
                                print(to_string_value(error), file=sys.stderr)
                            sys.exit(getattr(value, 'exitCode', 1) or 1)
                        print(to_string_value(value))

                    def unsupported(message):
                        raise RuntimeError(message)

                    class AssertionResult:
                        def __init__(self, result, message='', type='Assertion'):
                            self.result = bool(result)
                            self.message = message
                            self.type = type
                        def succeeded(self): return self.result

                    def assertion(result, message='', type='Assertion'):
                        return lambda: AssertionResult(result, message, type)

                    def assertions_of(value):
                        value = value.unsafe_run() if is_effect(value) else value
                        if value is None:
                            return []
                        if isinstance(value, list):
                            return flatten_assertions(value)
                        if isinstance(getattr(value, 'assertions', None), list):
                            return value.assertions
                        if callable(getattr(value, 'succeeded', None)) or hasattr(value, 'result'):
                            return [lambda: value]
                        return [assertion(False, f'Unsupported assertion result: {value}', 'AssertRuntime')]

                    def flatten_assertions(values):
                        flattened = []
                        for value in values:
                            flattened.extend(assertions_of(value))
                        return flattened

                    def display(value):
                        converter = getattr(value, 'toIso8601', None) or getattr(value, 'to_iso_8601', None)
                        return converter() if callable(converter) else to_string_value(value)

                    def value_at(value, name):
                        if value is None:
                            return None
                        member = getattr(value, name, None)
                        if member is None and name.endswith('_'):
                            member = getattr(value, name[:-1], None)
                        return member() if callable(member) else member

                    def first_present(*values):
                        for value in values:
                            if value is not None:
                                return value
                        return None

                    def sequence_as_list(value):
                        member = getattr(value, 'asList', None) or getattr(value, 'as_list', None)
                        if callable(member):
                            return member()
                        if isinstance(value, (list, CapySet)):
                            return list(value)
                        return None

                    def collection_size(value):
                        if is_type(value, 'None_'):
                            return 0
                        if isinstance(value, (CapySet, CapyDict)):
                            return value.size
                        sequence = sequence_as_list(value)
                        if sequence is not None:
                            return len(sequence)
                        return len(value) if value is not None else 0

                    def contains_value(container, expected):
                        if is_type(container, 'Some') or is_type(container, 'Success'):
                            return equals(container.value, expected)
                        if is_type(container, 'None_') or is_type(container, 'Error'):
                            return False
                        if isinstance(container, str):
                            return str(expected) in container
                        if isinstance(container, CapyDict):
                            return any(equals(value, expected) for value in container.values())
                        sequence = sequence_as_list(container)
                        if sequence is not None:
                            return contains(sequence, expected)
                        return contains(container, expected)
                    """, """
                    class GenericAssert:
                        def __init__(self, value, assertions=None):
                            self.__capybaraType = 'Assert'
                            self.__capybaraTypes = ['Assert']
                            self.value = value
                            self.assertions = list(assertions or [])
                        def __getattr__(self, name):
                            base = name.split('__', 1)[0]
                            if base != name and hasattr(self, base):
                                return getattr(self, base)
                            for prefix in ('isEqualTo', 'isGreaterThan', 'isGreaterOrEqualsThan', 'isLessThan', 'isLessOrEqualsThan',
                                           'hasDay', 'hasMonth', 'hasYear', 'hasHour', 'hasMinute', 'hasSecond', 'hasOffsetMinutes',
                                           'hasDate', 'hasTime', 'hasYears', 'hasMonths', 'hasDays', 'hasHours', 'hasMinutes',
                                           'hasSeconds', 'hasWeeks', 'hasStart', 'hasEnd', 'hasDuration'):
                                if name.startswith(prefix) and hasattr(self, prefix):
                                    return getattr(self, prefix)
                            raise AttributeError(name)
                        def append(self, result, message, type):
                            return GenericAssert(self.value, self.assertions + [assertion(result, message, type)])
                        def failed(self):
                            return any(not supplier().result for supplier in self.assertions)
                        def succeeded(self):
                            return not self.failed()
                        def contains(self, expected):
                            return self.append(contains_value(self.value, expected), f'Expected {display(self.value)} to contain {display(expected)}', 'contains')
                        def doesNotContain(self, expected):
                            return self.append(not contains_value(self.value, expected), f'Expected {display(self.value)} not to contain {display(expected)}', 'does_not_contain')
                        def does_not_contain(self, expected): return self.doesNotContain(expected)
                        def containsKey(self, expected):
                            return self.append(isinstance(self.value, CapyDict) and self.value.has(expected), f'Expected {display(self.value)} to contain key {display(expected)}', 'contains_key')
                        def contains_key(self, expected): return self.containsKey(expected)
                        def containsValue(self, expected):
                            return self.append(isinstance(self.value, CapyDict) and any(equals(value, expected) for value in self.value.values()), f'Expected {display(self.value)} to contain value {display(expected)}', 'contains_value')
                        def contains_value(self, expected): return self.containsValue(expected)
                        def doesNotContainKey(self, expected):
                            return self.append(not (isinstance(self.value, CapyDict) and self.value.has(expected)), f'Expected {display(self.value)} not to contain key {display(expected)}', 'does_not_contain_key')
                        def does_not_contain_key(self, expected): return self.doesNotContainKey(expected)
                        def hasSize(self, expected):
                            actual = collection_size(self.value)
                            return self.append(equals(actual, expected), f'Expected size {actual} to equal {expected}', 'has_size')
                        def has_size(self, expected): return self.hasSize(expected)
                        def isEmpty(self): return self.hasSize(0)
                        def is_empty(self): return self.isEmpty()
                        def isEqualTo(self, expected, epsilon=None):
                            result = abs(self.value - expected) <= epsilon if epsilon is not None else equals(self.value, expected)
                            return self.append(result, f'Expected {display(self.value)} to equal {display(expected)}', 'is_equal_to')
                        def is_equal_to(self, expected, epsilon=None): return self.isEqualTo(expected, epsilon)
                        def isGreaterThan(self, expected):
                            return self.append(self.value > expected, f'Expected {display(self.value)} to be greater than {display(expected)}', 'is_greater_than')
                        def is_greater_than(self, expected): return self.isGreaterThan(expected)
                        def isGreaterOrEqualsThan(self, expected):
                            return self.append(self.value >= expected, f'Expected {display(self.value)} to be greater than or equal to {display(expected)}', 'is_greater_or_equals_than')
                        def is_greater_or_equals_than(self, expected): return self.isGreaterOrEqualsThan(expected)
                        def isLessThan(self, expected):
                            return self.append(self.value < expected, f'Expected {display(self.value)} to be less than {display(expected)}', 'is_less_than')
                        def is_less_than(self, expected): return self.isLessThan(expected)
                        def isLessOrEqualsThan(self, expected):
                            return self.append(self.value <= expected, f'Expected {display(self.value)} to be less than or equal to {display(expected)}', 'is_less_or_equals_than')
                        def is_less_or_equals_than(self, expected): return self.isLessOrEqualsThan(expected)
                        def isBetween(self, start, end): return self.isGreaterOrEqualsThan(start).isLessOrEqualsThan(end)
                        def is_between(self, start, end): return self.isBetween(start, end)
                        def isZero(self): return self.isEqualTo(0)
                        def is_zero(self): return self.isZero()
                        def isOne(self): return self.isEqualTo(1)
                        def is_one(self): return self.isOne()
                        def isTrue(self): return self.isEqualTo(True)
                        def is_true(self): return self.isTrue()
                        def isFalse(self): return self.isEqualTo(False)
                        def is_false(self): return self.isFalse()
                        def startsWith(self, expected):
                            return self.append(str(self.value).startswith(expected), f'Expected {display(self.value)} to start with {display(expected)}', 'starts_with')
                        def starts_with(self, expected): return self.startsWith(expected)
                        def doesNotStartWith(self, expected):
                            return self.append(not str(self.value).startswith(expected), f'Expected {display(self.value)} not to start with {display(expected)}', 'does_not_start_with')
                        def does_not_start_with(self, expected): return self.doesNotStartWith(expected)
                        def succeeds(self, expected=None):
                            ok = is_type(self.value, 'Success')
                            base = self.append(ok, f'Expected Result.Success, got {display(self.value)}', 'succeeds')
                            if not ok or expected is None:
                                return base
                            if callable(expected):
                                return GenericAssert(self.value, base.assertions + assertions_of(expected(self.value.value)))
                            return base.append(equals(self.value.value, expected), f'Expected success value {display(self.value.value)} to equal {display(expected)}', 'succeeds')
                        def fails(self, expected_message=None):
                            ok = is_type(self.value, 'Error')
                            message_matches = expected_message is None or (ok and equals(self.value.message, expected_message))
                            message = f'Expected Result.Error, got {display(self.value)}' if expected_message is None else f'Expected error message {display(expected_message)}, got {display(getattr(self.value, "message", None))}'
                            return self.append(ok and message_matches, message, 'fails')
                        def hasDay(self, expected):
                            return self.append(equals(first_present(value_at(self.value, 'day'), value_at(value_at(self.value, 'date'), 'day')), expected), f'Expected day to equal {expected}', 'has_day')
                        def has_day(self, expected): return self.hasDay(expected)
                        def hasMonth(self, expected):
                            return self.append(equals(first_present(value_at(self.value, 'month'), value_at(value_at(self.value, 'date'), 'month')), expected), f'Expected month to equal {expected}', 'has_month')
                        def has_month(self, expected): return self.hasMonth(expected)
                        def hasYear(self, expected):
                            return self.append(equals(first_present(value_at(self.value, 'year'), value_at(value_at(self.value, 'date'), 'year')), expected), f'Expected year to equal {expected}', 'has_year')
                        def has_year(self, expected): return self.hasYear(expected)
                        def hasHour(self, expected):
                            return self.append(equals(first_present(value_at(self.value, 'hour'), value_at(value_at(self.value, 'time'), 'hour')), expected), f'Expected hour to equal {expected}', 'has_hour')
                        def has_hour(self, expected): return self.hasHour(expected)
                        def hasMinute(self, expected):
                            return self.append(equals(first_present(value_at(self.value, 'minute'), value_at(value_at(self.value, 'time'), 'minute')), expected), f'Expected minute to equal {expected}', 'has_minute')
                        def has_minute(self, expected): return self.hasMinute(expected)
                        def hasSecond(self, expected):
                            return self.append(equals(first_present(value_at(self.value, 'second'), value_at(value_at(self.value, 'time'), 'second')), expected), f'Expected second to equal {expected}', 'has_second')
                        def has_second(self, expected): return self.hasSecond(expected)
                        def hasOffsetMinutes(self, expected):
                            actual = value_at(self.value, 'offset_minutes') or value_at(value_at(self.value, 'time'), 'offset_minutes')
                            normalized = Some({'value': expected}) if isinstance(expected, int) else expected
                            return self.append(equals(actual, normalized), f'Expected offset to equal {display(normalized)}', 'has_offset_minutes')
                        def has_offset_minutes(self, expected): return self.hasOffsetMinutes(expected)
                        def hasDate(self, *args):
                            if len(args) == 3:
                                return self.hasDay(args[0]).hasMonth(args[1]).hasYear(args[2])
                            expected = args[0]
                            return self.append(equals(value_at(self.value, 'date'), expected), f'Expected date to equal {display(expected)}', 'has_date')
                        def has_date(self, *args): return self.hasDate(*args)
                        def hasTime(self, *args):
                            if len(args) == 3:
                                return self.hasHour(args[0]).hasMinute(args[1]).hasSecond(args[2])
                            expected = args[0]
                            return self.append(equals(value_at(self.value, 'time'), expected), f'Expected time to equal {display(expected)}', 'has_time')
                        def has_time(self, *args): return self.hasTime(*args)
                        def hasYears(self, expected):
                            return self.append(equals(value_at(self.value, 'years'), expected), f'Expected years to equal {expected}', 'has_years')
                        def has_years(self, expected): return self.hasYears(expected)
                        def hasMonths(self, expected):
                            return self.append(equals(value_at(self.value, 'months'), expected), f'Expected months to equal {expected}', 'has_months')
                        def has_months(self, expected): return self.hasMonths(expected)
                        def hasDays(self, expected):
                            return self.append(equals(value_at(self.value, 'days'), expected), f'Expected days to equal {expected}', 'has_days')
                        def has_days(self, expected): return self.hasDays(expected)
                        def hasHours(self, expected):
                            return self.append(equals(value_at(self.value, 'hours'), expected), f'Expected hours to equal {expected}', 'has_hours')
                        def has_hours(self, expected): return self.hasHours(expected)
                        def hasMinutes(self, expected):
                            return self.append(equals(value_at(self.value, 'minutes'), expected), f'Expected minutes to equal {expected}', 'has_minutes')
                        def has_minutes(self, expected): return self.hasMinutes(expected)
                        def hasSeconds(self, expected):
                            return self.append(equals(value_at(self.value, 'seconds'), expected), f'Expected seconds to equal {expected}', 'has_seconds')
                        def has_seconds(self, expected): return self.hasSeconds(expected)
                        def hasWeeks(self, expected):
                            return self.append(equals(value_at(self.value, 'weeks'), expected), f'Expected weeks to equal {expected}', 'has_weeks')
                        def has_weeks(self, expected): return self.hasWeeks(expected)
                        def hasStart(self, expected):
                            return self.append(equals(value_at(self.value, 'start'), expected), f'Expected start to equal {display(expected)}', 'has_start')
                        def has_start(self, expected): return self.hasStart(expected)
                        def hasEnd(self, expected):
                            return self.append(equals(value_at(self.value, 'end'), expected), f'Expected end to equal {display(expected)}', 'has_end')
                        def has_end(self, expected): return self.hasEnd(expected)
                        def hasDuration(self, expected):
                            return self.append(equals(value_at(self.value, 'duration'), expected), f'Expected duration to equal {display(expected)}', 'has_duration')
                        def has_duration(self, expected): return self.hasDuration(expected)

                    def assert_that(value, message='Assertion failed'):
                        return GenericAssert(value)
                    def assert_all(values):
                        return GenericAssert(None, flatten_assertions(values))
                    """, """
                    class CapyNumber(int):
                        def __new__(cls, value=0): return int.__new__(cls, int(value))
                        def __call__(self): return int(self)

                    def _leap_year(year):
                        return (year % 4 == 0 and year % 100 != 0) or year % 400 == 0

                    def _days_in_month(year, month):
                        if month in (4, 6, 9, 11): return 30
                        if month == 2: return 29 if _leap_year(year) else 28
                        return 31

                    def _date_valid(day, month, year):
                        return 1 <= month <= 12 and 1 <= day <= _days_in_month(year, month)

                    def _date_to_days(day, month, year):
                        y = year - 1 if month <= 2 else year
                        era = y // 400
                        yoe = y - era * 400
                        mp = month - 3 if month > 2 else month + 9
                        doy = (153 * mp + 2) // 5 + day - 1
                        doe = yoe * 365 + yoe // 4 - yoe // 100 + doy
                        return era * 146097 + doe - 719468

                    def _date_from_days(days):
                        z = days + 719468
                        era = z // 146097
                        doe = z - era * 146097
                        yoe = (doe - doe // 1460 + doe // 36524 - doe // 146096) // 365
                        y = yoe + era * 400
                        doy = doe - (365 * yoe + yoe // 4 - yoe // 100)
                        mp = (5 * doy + 2) // 153
                        day = doy - (153 * mp + 2) // 5 + 1
                        month = mp + 3 if mp < 10 else mp - 9
                        year = y + 1 if month <= 2 else y
                        return Date({'day': day, 'month': month, 'year': year})

                    class Date:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Date'
                            self.__capybaraTypes = ['Date']
                            self.day = int(fields.get('day', 1))
                            self.month = int(fields.get('month', 1))
                            self.year = int(fields.get('year', 1970))
                        def leapYear(self): return _leap_year(self.year)
                        def leap_year(self): return self.leapYear()
                        def toDaysSinceUnixEpoch(self): return _date_to_days(self.day, self.month, self.year)
                        def to_days_since_unix_epoch(self): return self.toDaysSinceUnixEpoch()
                        def addDays(self, days): return _date_from_days(self.toDaysSinceUnixEpoch() + int(days))
                        def add_days(self, days): return self.addDays(days)
                        def addYearsMonths(self, years, months):
                            total = (self.year + int(years)) * 12 + (self.month + int(months) - 1)
                            year = total // 12
                            month = total % 12 + 1
                            return Date({'day': min(self.day, _days_in_month(year, month)), 'month': month, 'year': year})
                        def add_years_months(self, years, months): return self.addYearsMonths(years, months)
                        def toIso8601(self):
                            if self.year < 0:
                                year = '-' + str(-self.year).zfill(4)
                            elif self.year > 9999:
                                year = '+' + str(self.year)
                            else:
                                year = str(self.year).zfill(4)
                            return f'{year}-{self.month:02d}-{self.day:02d}'
                        def to_iso_8601(self): return self.toIso8601()
                        def __str__(self): return self.toIso8601()
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'Date', 'capy.date_time', 'capy/date_time/DateModule', ['day', 'month', 'year'])

                    def make_date(day, month, year):
                        return Success({'value': Date({'day': day, 'month': month, 'year': year})}) if _date_valid(int(day), int(month), int(year)) else Error({'message': f'Invalid date `{day}/{month}/{year}`!'})

                    def _parse_year(value):
                        if value.startswith('+'): return int(value[1:])
                        if value.startswith('-'): return -int(value[1:])
                        return int(value)

                    def date_from_iso(value):
                        text = str(value)
                        match = re.fullmatch(r'([+-]?\\d{4,})-(\\d{2})-(\\d{2})', text)
                        if not match and re.fullmatch(r'\\d{8}', text):
                            match = re.fullmatch(r'(\\d{4})(\\d{2})(\\d{2})', text)
                        if not match:
                            return Error({'message': f'Invalid ISO 8601 date format: expected date in `YYYY-MM-DD` or `YYYYMMDD` format, got `{text}`'})
                        result = make_date(int(match.group(3)), int(match.group(2)), _parse_year(match.group(1)))
                        return result if is_type(result, 'Success') else Error({'message': 'Invalid ISO 8601 date format: ' + result.message})

                    def _option_value(option, default=None):
                        return option.value if is_type(option, 'Some') else default

                    class Time:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'Time'
                            self.__capybaraTypes = ['Time']
                            self.hour = int(fields.get('hour', 0))
                            self.minute = int(fields.get('minute', 0))
                            self.second = int(fields.get('second', 0))
                            self.offset_minutes = fields.get('offset_minutes', None_)
                        def toSeconds(self): return self.hour * 3600 + self.minute * 60 + self.second
                        def to_seconds(self): return self.toSeconds()
                        def toMinutes(self): return self.hour * 60 + self.minute
                        def to_minutes(self): return self.toMinutes()
                        def toHours(self): return self.hour
                        def to_hours(self): return self.toHours()
                        def roundToMinutes(self): return Time({'hour': self.hour, 'minute': self.minute, 'second': 0, 'offset_minutes': self.offset_minutes})
                        def round_to_minutes(self): return self.roundToMinutes()
                        def roundToHours(self): return Time({'hour': self.hour, 'minute': 0, 'second': 0, 'offset_minutes': self.offset_minutes})
                        def round_to_hours(self): return self.roundToHours()
                        def addSeconds(self, seconds):
                            total = (self.toSeconds() + int(seconds)) % 86400
                            return Time({'hour': total // 3600, 'minute': (total // 60) % 60, 'second': total % 60, 'offset_minutes': self.offset_minutes})
                        def add_seconds(self, seconds): return self.addSeconds(seconds)
                        def addMinutes(self, minutes): return self.addSeconds(int(minutes) * 60)
                        def add_minutes(self, minutes): return self.addMinutes(minutes)
                        def addHours(self, hours): return self.addSeconds(int(hours) * 3600)
                        def add_hours(self, hours): return self.addHours(hours)
                        def toIso8601(self):
                            base = f'{self.hour:02d}:{self.minute:02d}:{self.second:02d}'
                            offset = _option_value(self.offset_minutes)
                            if offset is None:
                                return base
                            if offset == 0:
                                return base + 'Z'
                            sign = '-' if offset < 0 else '+'
                            absolute = abs(int(offset))
                            return f'{base}{sign}{absolute // 60:02d}:{absolute % 60:02d}'
                        def to_iso_8601(self): return self.toIso8601()
                        def __str__(self): return self.toIso8601()
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'Time', 'capy.date_time', 'capy/date_time/TimeModule', ['hour', 'minute', 'second', 'offset_minutes'])

                    def make_time(hour, minute, second, offset_minutes=None_):
                        hour, minute, second = int(hour), int(minute), int(second)
                        offset = _option_value(offset_minutes)
                        if hour < 0 or hour > 23 or minute < 0 or minute > 59 or second < 0 or second > 59:
                            return Error({'message': f'Invalid time `{hour}:{minute}:{second}`!'})
                        if offset is not None and (offset < -1439 or offset > 1439):
                            return Error({'message': f'Invalid UTC offset `{offset}` minutes!'})
                        return Success({'value': Time({'hour': hour, 'minute': minute, 'second': second, 'offset_minutes': offset_minutes})})

                    def time_from_iso(value):
                        text = str(value)
                        body = text[1:] if text.startswith('T') else text
                        offset = None_
                        for pattern in (r'(.*)Z$', r'(.*)([+-]\\d{2}:\\d{2})$', r'(.*)([+-]\\d{4})$', r'(.*)([+-]\\d{2})$'):
                            match = re.fullmatch(pattern, body)
                            if match:
                                if pattern == r'(.*)Z$':
                                    body, offset = match.group(1), Some({'value': 0})
                                else:
                                    body, raw = match.group(1), match.group(2)
                                    sign = -1 if raw.startswith('-') else 1
                                    digits = raw[1:].replace(':', '')
                                    if sign < 0 and int(digits) == 0:
                                        return Error({'message': 'Invalid ISO 8601 time format: negative zero offset is not allowed'})
                                    hours = int(digits[:2])
                                    minutes = int(digits[2:] or '0')
                                    if hours > 23 or minutes > 59:
                                        return Error({'message': 'Invalid ISO 8601 time format: offset out of range'})
                                    offset = Some({'value': sign * (hours * 60 + minutes)})
                                break
                        if re.fullmatch(r'\\d{2}:\\d{2}:\\d{2}', body):
                            h, m, s = [int(part) for part in body.split(':')]
                        elif re.fullmatch(r'\\d{6}', body):
                            h, m, s = int(body[:2]), int(body[2:4]), int(body[4:])
                        else:
                            return Error({'message': f'Invalid ISO 8601 time format: expected time in `HH:MM:SS`, `HHMMSS`, `THH:MM:SS`, or `THHMMSS` format, got `{text}`'})
                        return make_time(h, m, s, offset)

                    class DateDuration:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'DateDuration'
                            self.__capybaraTypes = ['DateDuration', 'Duration']
                            self.years = CapyNumber(fields.get('years', 0))
                            self.months = CapyNumber(fields.get('months', 0))
                            self.days = CapyNumber(fields.get('days', 0))
                            self.hours = CapyNumber(fields.get('hours', 0))
                            self.minutes = CapyNumber(fields.get('minutes', 0))
                            self.seconds = CapyNumber(fields.get('seconds', 0))
                        def weeks(self): return 0
                        def negate(self): return DateDuration({'years': -self.years, 'months': -self.months, 'days': -self.days, 'hours': -self.hours, 'minutes': -self.minutes, 'seconds': -self.seconds})
                        def toIso8601(self):
                            if equals(self, ZERO): return 'PT0S'
                            date = ''.join([f'{self.years}Y' if self.years else '', f'{self.months}M' if self.months else '', f'{self.days}D' if self.days else ''])
                            time_part = ''.join([f'{self.hours}H' if self.hours else '', f'{self.minutes}M' if self.minutes else '', f'{self.seconds}S' if self.seconds else ''])
                            return 'P' + date + (('T' + time_part) if time_part else '')
                        def to_iso_8601(self): return self.toIso8601()
                        def capybaraDataValueInfo(self): return data_value_info(self, 'DateDuration', 'capy.date_time', 'capy/date_time/DurationModule', ['years', 'months', 'days', 'hours', 'minutes', 'seconds'])

                    class WeekDuration:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'WeekDuration'
                            self.__capybaraTypes = ['WeekDuration', 'Duration']
                            self.weeks = CapyNumber(fields.get('weeks', 0))
                            self.years = CapyNumber(0)
                            self.months = CapyNumber(0)
                            self.days = CapyNumber(self.weeks * 7)
                            self.hours = CapyNumber(0)
                            self.minutes = CapyNumber(0)
                            self.seconds = CapyNumber(0)
                        def negate(self): return WeekDuration({'weeks': -self.weeks})
                        def toIso8601(self): return f'P{self.weeks}W'
                        def to_iso_8601(self): return self.toIso8601()
                        def capybaraDataValueInfo(self): return data_value_info(self, 'WeekDuration', 'capy.date_time', 'capy/date_time/DurationModule', ['weeks'])

                    ZERO = DateDuration({'years': 0, 'months': 0, 'days': 0, 'hours': 0, 'minutes': 0, 'seconds': 0})

                    def duration_from_iso(value):
                        text = str(value)
                        if text == 'PT0S': return Success({'value': ZERO})
                        match = re.fullmatch(r'P(\\d+)W', text)
                        if match: return Success({'value': WeekDuration({'weeks': int(match.group(1))})})
                        match = re.fullmatch(r'P(?:(\\d+)Y)?(?:(\\d+)M)?(?:(\\d+)D)?(?:T(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?)?', text)
                        if match and any(match.groups()):
                            values = [int(group or 0) for group in match.groups()]
                            return Success({'value': DateDuration({'years': values[0], 'months': values[1], 'days': values[2], 'hours': values[3], 'minutes': values[4], 'seconds': values[5]})})
                        match = re.fullmatch(r'P(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})(\\d{2})', text) or re.fullmatch(r'P(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})', text)
                        if match:
                            values = [int(group) for group in match.groups()]
                            if values[1] > 12 or values[4] > 59 or values[5] > 59:
                                return Error({'message': f'Invalid ISO 8601 duration format: {text}'})
                            return Success({'value': DateDuration({'years': values[0], 'months': values[1], 'days': values[2], 'hours': values[3], 'minutes': values[4], 'seconds': values[5]})})
                        return Error({'message': f'Invalid ISO 8601 duration format: {text}'})

                    class DateTime:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'DateTime'
                            self.__capybaraTypes = ['DateTime']
                            self.date = fields.get('date', Date({'day': 1, 'month': 1, 'year': 1970}))
                            self.time = fields.get('time', Time({'hour': 0, 'minute': 0, 'second': 0, 'offset_minutes': None_}))
                        def __getattr__(self, name):
                            if name.startswith('greaterOpGreater') or name.startswith('greaterThan'): return self.greaterThan
                            if name.startswith('lessThan') or name.startswith('lessOpLess'): return self.lessThan
                            if name.startswith('minus') or name.startswith('op2d'): return self.minus
                            raise AttributeError(name)
                        def timestamp(self): return self.date.toDaysSinceUnixEpoch() * 86400 + self.time.toSeconds()
                        def plus(self, duration):
                            if is_type(duration, 'WeekDuration'):
                                duration = DateDuration({'days': duration.days})
                            date = self.date.addYearsMonths(duration.years(), duration.months())
                            seconds = self.time.toSeconds() + duration.hours() * 3600 + duration.minutes() * 60 + duration.seconds()
                            extra_days = seconds // 86400
                            return DateTime({'date': date.addDays(duration.days() + extra_days), 'time': self.time.addSeconds(duration.hours() * 3600 + duration.minutes() * 60 + duration.seconds())})
                        def minus(self, other):
                            if is_type(other, 'DateTime'):
                                delta = self.timestamp() - other.timestamp()
                                sign = -1 if delta < 0 else 1
                                delta = abs(delta)
                                days, rem = divmod(delta, 86400)
                                hours, rem = divmod(rem, 3600)
                                minutes, seconds = divmod(rem, 60)
                                return DateDuration({'days': sign * days, 'hours': sign * hours, 'minutes': sign * minutes, 'seconds': sign * seconds})
                            return self.plus(other.negate())
                        def greaterThan(self, other): return self.timestamp() > other.timestamp()
                        def greater_than(self, other): return self.greaterThan(other)
                        def greater(self, other): return self.greaterThan(other)
                        def lessThan(self, other): return self.timestamp() < other.timestamp()
                        def less_than(self, other): return self.lessThan(other)
                        def less(self, other): return self.lessThan(other)
                        def __lt__(self, other): return self.lessThan(other)
                        def __le__(self, other): return self.timestamp() <= other.timestamp()
                        def __gt__(self, other): return self.greaterThan(other)
                        def __ge__(self, other): return self.timestamp() >= other.timestamp()
                        def toIso8601(self):
                            offset = _option_value(self.time.offset_minutes)
                            instant = self if offset is None else self.plus(DateDuration({'minutes': -offset}))
                            utc = Time({'hour': instant.time.hour, 'minute': instant.time.minute, 'second': instant.time.second, 'offset_minutes': Some({'value': 0})})
                            return instant.date.toIso8601() + 'T' + utc.toIso8601()
                        def to_iso_8601(self): return self.toIso8601()
                        def __str__(self): return self.toIso8601()
                        def toString(self): return str(self)
                        def capybaraDataValueInfo(self): return data_value_info(self, 'DateTime', 'capy.date_time', 'capy/date_time/DateTimeModule', ['date', 'time'])

                    def _split_time_offset(value):
                        if value.endswith('Z'):
                            return value[:-1]
                        match = re.fullmatch(r'(.*)([+-]\\d{2}:?\\d{0,2})', value)
                        return match.group(1) if match else value

                    def _date_time_from_iso(value, allow_compact=False):
                        text = str(value)
                        if 'T' not in text or ' ' in text:
                            return Error({'message': 'Invalid ISO 8601 date-time format: expected `T` separator between date and time'})
                        left, right = text.split('T', 1)
                        if not allow_compact:
                            time_body = _split_time_offset(right)
                            extended = re.fullmatch(r'[+-]?\\d{4,}-\\d{2}-\\d{2}', left) and re.fullmatch(r'\\d{2}:\\d{2}:\\d{2}', time_body)
                            compact = re.fullmatch(r'\\d{8}', left) and re.fullmatch(r'\\d{6}', time_body)
                            if not (extended or compact):
                                return Error({'message': 'Invalid ISO 8601 date-time format: expected extended date-time format'})
                        date_result = date_from_iso(left)
                        time_result = time_from_iso(right)
                        if is_type(date_result, 'Error') or is_type(time_result, 'Error'):
                            return Error({'message': 'Invalid ISO 8601 date-time format: invalid date-time'})
                        offset = _option_value(time_result.value.offset_minutes)
                        if offset is None:
                            return Error({'message': 'Invalid ISO 8601 date-time format: expected UTC designator `Z` or numeric offset'})
                        local = DateTime({'date': date_result.value, 'time': Time({'hour': time_result.value.hour, 'minute': time_result.value.minute, 'second': time_result.value.second, 'offset_minutes': None_})})
                        return Success({'value': local.plus(DateDuration({'minutes': -offset}))})

                    def date_time_from_iso(value):
                        return _date_time_from_iso(value, False)

                    class DateTimeDurationEnd:
                        def __init__(self, fields=None):
                            fields = fields or {}
                            self.__capybaraType = 'DateTimeDurationEnd'
                            self.__capybaraTypes = ['DateTimeDurationEnd', 'DateTimeStartDuration', 'DateTimeStartEnd', 'Interval']
                            self.start_value = fields.get('start') or fields.get('start_value')
                            self.end_value = fields.get('end') or fields.get('end_value')
                            self.duration_value = fields.get('duration') or fields.get('duration_value')
                            self.format_value = fields.get('format')
                        def start(self): return self.start_value
                        def end(self): return self.end_value
                        def duration(self): return self.duration_value or self.end_value.minus(self.start_value)
                        def toIso8601(self):
                            if self.format_value == 'start_duration':
                                return self.start().toIso8601() + '/' + self.duration().toIso8601()
                            if self.format_value == 'duration_end':
                                return self.duration().toIso8601() + '/' + self.end().toIso8601()
                            return self.start().toIso8601() + '/' + self.end().toIso8601()
                        def to_iso_8601(self): return self.toIso8601()
                    class DateTimeStartDuration(DateTimeDurationEnd): pass
                    class DateTimeStartEnd(DateTimeDurationEnd): pass
                    def interval_from_iso(value):
                        text = str(value)
                        slash_count = text.count('/')
                        dash_count = text.count('--')
                        if slash_count and dash_count or slash_count > 1 or dash_count > 1:
                            return Error({'message': 'Invalid ISO 8601 interval format: expected exactly one interval separator'})
                        if slash_count == 1:
                            left, right = text.split('/', 1)
                        elif dash_count == 1:
                            left, right = text.split('--', 1)
                        else:
                            return Error({'message': 'Invalid ISO 8601 interval format: expected exactly one interval separator'})
                        left_is_duration = left.startswith('P')
                        right_is_duration = right.startswith('P')
                        if left_is_duration and right_is_duration:
                            return Error({'message': 'Invalid ISO 8601 interval format: interval cannot contain a duration on both sides'})
                        if left_is_duration:
                            duration_result = duration_from_iso(left)
                            end_result = _date_time_from_iso(right, True)
                            if is_type(duration_result, 'Error') or is_type(end_result, 'Error') or is_type(duration_result.value, 'WeekDuration'):
                                return Error({'message': 'Invalid ISO 8601 interval format: interval endpoints must be UTC date-times'})
                            end = end_result.value
                            duration = duration_result.value
                            start = end.minus(duration)
                            return Success({'value': DateTimeDurationEnd({'start': start, 'end': end, 'duration': duration, 'format': 'duration_end'})})
                        if right_is_duration:
                            start_result = _date_time_from_iso(left, True)
                            duration_result = duration_from_iso(right)
                            if is_type(start_result, 'Error') or is_type(duration_result, 'Error') or is_type(duration_result.value, 'WeekDuration'):
                                return Error({'message': 'Invalid ISO 8601 interval format: interval endpoints must be UTC date-times'})
                            start = start_result.value
                            duration = duration_result.value
                            end = start.plus(duration)
                            return Success({'value': DateTimeStartDuration({'start': start, 'end': end, 'duration': duration, 'format': 'start_duration'})})
                        start_result = _date_time_from_iso(left, True)
                        end_result = _date_time_from_iso(right, True)
                        if is_type(start_result, 'Error') or is_type(end_result, 'Error'):
                            return Error({'message': 'Invalid ISO 8601 interval format: interval endpoints must be UTC date-times'})
                        start = start_result.value
                        end = end_result.value
                        if start.timestamp() > end.timestamp():
                            return Error({'message': 'Invalid ISO 8601 interval format: interval start must not be after interval end'})
                        return Success({'value': DateTimeStartEnd({'start': start, 'end': end, 'duration': end.minus(start)})})
                    clock_now = lambda: DateTime({'date': _date_from_days(current_millis() // 86400000), 'time': Time({'hour': 0, 'minute': 0, 'second': 0, 'offset_minutes': None_})})
                    """);
        }
    }

    static String renderSysPathBootstrap(Path relativePath) {
        var parentIndex = relativePath.getNameCount() - 1;
        return "from pathlib import Path as __capy_Path\n"
               + "import sys as __capy_sys\n"
               + "__capy_root = str(__capy_Path(__file__).resolve().parents[" + parentIndex + "])\n"
               + "if __capy_root not in __capy_sys.path:\n"
               + "    __capy_sys.path.insert(0, __capy_root)\n";
    }

    static String moduleImportPath(Path relativePath) {
        var path = relativePath.toString().replace('\\', '/');
        if (path.endsWith(".py")) {
            path = path.substring(0, path.length() - 3);
        }
        return path.replace('/', '.');
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
                var normalizedBaseName = pyIdentifier(baseMethodName(function.name()));
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
                var normalizedBaseName = pyIdentifier(rawBaseName);
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
        var simpleMethodName = pyIdentifier(simpleMethodName(targetName));
        var qualifier = qualifierName(targetName);
        var candidates = functionNameOverrides.entrySet().stream()
                .filter(entry -> pyIdentifier(simpleMethodName(keyName(entry.getKey()))).equals(simpleMethodName))
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
        return name + "|" + parameterTypes.stream().map(String::valueOf).collect(joining(","));
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
        return normalizeTypeIdentifier(JavaScriptGenerator.rawSimpleTypeName(typeName));
    }

    static String capybaraTypeName(String typeName) {
        return JavaScriptGenerator.rawSimpleTypeName(typeName);
    }

    static String enumValueIdentifier(String rawName) {
        if (isValidPyIdentifier(rawName) && !PY_KEYWORDS.contains(rawName)) {
            return rawName;
        }
        return pyIdentifier(rawName);
    }

    static String normalizeTypeIdentifier(String rawName) {
        var js = JavaScriptGenerator.normalizeJsTypeIdentifier(rawName);
        if (PY_KEYWORDS.contains(js)) {
            return js + "_";
        }
        return js.replace("$", "_");
    }

    static String normalizeIdentifier(String rawName) {
        return pyIdentifier(rawName);
    }

    static String pyIdentifier(String rawName) {
        var js = JavaScriptGenerator.normalizeJsIdentifier(rawName);
        js = js.replace("$", "_");
        if (PY_KEYWORDS.contains(js)) {
            return js + "_";
        }
        if (js.startsWith("__")) {
            return "capy" + js;
        }
        return js;
    }

    static boolean isValidPyIdentifier(String value) {
        if (value == null || value.isBlank() || PY_KEYWORDS.contains(value)) {
            return false;
        }
        var first = value.charAt(0);
        if (!Character.isLetter(first) && first != '_') {
            return false;
        }
        for (var i = 1; i < value.length(); i++) {
            var ch = value.charAt(i);
            if (!Character.isLetterOrDigit(ch) && ch != '_') {
                return false;
            }
        }
        return true;
    }

    static String moduleVar(String className) {
        return "capy_module_" + className.replaceAll("[^A-Za-z0-9]+", "_");
    }

    static Path classNamePath(String className) {
        return Path.of(className.replace('.', '/') + ".py");
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
            normalized.add(pyIdentifier(segments[i]));
        }
        normalized.add(segments[segments.length - 1]);
        return String.join(".", normalized);
    }

    static String renderLongLiteral(String value) {
        var literal = value.endsWith("L") || value.endsWith("l")
                ? value.substring(0, value.length() - 1)
                : value;
        if (literal.startsWith("+")) {
            literal = literal.substring(1);
        }
        return literal.replace("_", "");
    }

    static String pyArray(Collection<String> values) {
        return values.stream().map(PythonGenerator::pyString).collect(joining(", ", "[", "]"));
    }

    static String pyString(String value) {
        var escaped = value
                .replace("\\", "\\\\")
                .replace("'", "\\'")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
        return "'" + escaped + "'";
    }

    static String pyBool(boolean value) {
        return value ? "True" : "False";
    }

    private static String[] parseDictPipeArguments(String argumentName) {
        if (!argumentName.contains(DICT_PIPE_ARGS_SEPARATOR)) {
            return new String[0];
        }
        var parts = argumentName.split(Pattern.quote(DICT_PIPE_ARGS_SEPARATOR), -1);
        if (parts.length != 2 || parts[0].isBlank() || parts[1].isBlank()) {
            return new String[0];
        }
        return parts;
    }

    private static String[] parseTuplePipeArguments(String argumentName) {
        if (!argumentName.contains(TUPLE_PIPE_ARGS_SEPARATOR)) {
            return new String[0];
        }
        return argumentName.split(Pattern.quote(TUPLE_PIPE_ARGS_SEPARATOR), -1);
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
}
