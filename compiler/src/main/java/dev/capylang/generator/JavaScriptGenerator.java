package dev.capylang.generator;

import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledAnnotation;
import dev.capylang.compiler.CompiledAnnotationValue;
import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunction;
import dev.capylang.compiler.CompiledFunctionType;
import dev.capylang.compiler.CompiledGenericTypeParameter;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledNativeProviderBinding;
import dev.capylang.compiler.CompiledNativeProviderDeclaration;
import dev.capylang.compiler.CompiledObjectMethod;
import dev.capylang.compiler.CompiledObjectKind;
import dev.capylang.compiler.CompiledObjectType;
import dev.capylang.compiler.CompiledPrimitiveBackedType;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.CompiledTupleType;
import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.GenericDataType;
import dev.capylang.compiler.NativeProviderBackendBinding;
import dev.capylang.compiler.NativeProviderCatalog;
import dev.capylang.compiler.NativeProviderLifetime;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.expression.*;
import dev.capylang.compiler.parser.InfixOperator;
import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.ObjectOrientedModule;
import dev.capylang.generator.java.JavaAstBuilder;
import dev.capylang.generator.java.JavaClass;
import dev.capylang.generator.java.JavaConst;
import dev.capylang.generator.java.JavaDataValueInfo;
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
    private static final String PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX = "__constructor__primitive__";
    private static final String DICT_PIPE_ARGS_SEPARATOR = "::";
    private static final String TUPLE_PIPE_ARGS_SEPARATOR = ";;";
    static final Path RUNTIME_PATH = Path.of("dev", "capylang", "capybara.js");
    static final Path NATIVE_PROVIDER_BOOTSTRAP_PATH = Path.of("dev", "capylang", "native_providers.js");
    private static final String NATIVE_PROVIDER_BOOTSTRAP_CLASS_NAME = "dev.capylang.native_providers";
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
                .filter(module -> !isRuntimeProvidedModule(module))
                .map(module -> ModuleInfo.from(module, astBuilder.build(module)))
                .toList();
        var context = ProgramContext.build(
                moduleInfos,
                program.modules(),
                program.objectOrientedModules(),
                program.nativeProviderCatalog(),
                functionNameOverrides
        );

        for (var moduleInfo : moduleInfos) {
            modules.add(new GeneratedModule(moduleInfo.relativePath(), new ModuleRenderer(context, moduleInfo).render()));
        }
        modules.addAll(new ObjectOrientedJavaScriptGenerator(context).generate(program.objectOrientedModules()));
        modules.addAll(nativeProviderBootstrapModules(context.nativeProviderInfos()));
        modules.addAll(RuntimeModules.modules());
        return new GeneratedProgram(List.copyOf(modules));
    }

    private List<GeneratedModule> nativeProviderBootstrapModules(List<ProgramContext.NativeProviderInfo> providers) {
        if (providers.isEmpty()) {
            return List.of();
        }
        return List.of(new GeneratedModule(NATIVE_PROVIDER_BOOTSTRAP_PATH, renderNativeProviderBootstrap(providers)));
    }

    private String renderNativeProviderBootstrap(List<ProgramContext.NativeProviderInfo> providers) {
        var code = new StringBuilder();
        code.append("'use strict';\n\n");
        code.append("const capy = require('./capybara.js');\n");
        for (var provider : providers) {
            code.append("const ")
                    .append(nativeProviderModuleVariable(provider))
                    .append(" = require(")
                    .append(jsString(provider.binding().moduleName()))
                    .append(");\n");
        }
        code.append("\n");
        code.append("const providers = capy.defineNativeProviders({\n");
        for (var provider : providers) {
            code.append(renderNativeProviderEntry(provider));
        }
        code.append("});\n\n");
        for (var provider : providers) {
            code.append(renderNativeProviderFunction(provider)).append("\n");
        }
        code.append("module.exports = {\n");
        for (var provider : providers) {
            code.append("    ").append(provider.bootstrapFunctionName()).append(",\n");
        }
        code.append("};\n");
        return code.toString();
    }

    private String renderNativeProviderEntry(ProgramContext.NativeProviderInfo provider) {
        var moduleVariable = nativeProviderModuleVariable(provider);
        var exportAccess = renderPropertyAccess(moduleVariable, provider.binding().exportName());
        return "    " + jsString(nativeProviderBootstrapKey(provider.interfaceId(), provider.qualifier())) + ": capy.nativeFactory({\n"
               + "        interfaceId: " + jsString(provider.interfaceId()) + ",\n"
               + "        qualifier: " + jsString(provider.qualifier()) + ",\n"
               + "        providerSymbol: " + jsString(provider.providerSymbolName()) + ",\n"
               + "        backend: 'javascript',\n"
               + "        sourceFile: " + jsString(provider.sourceFile()) + ",\n"
               + "        lifetime: " + jsString(provider.lifetime().jsonValue()) + ",\n"
               + "        moduleName: " + jsString(provider.binding().moduleName()) + ",\n"
               + "        exportName: " + jsString(provider.binding().exportName()) + ",\n"
               + "        exportExists: Object.prototype.hasOwnProperty.call(" + moduleVariable + ", " + jsString(provider.binding().exportName()) + "),\n"
               + "        exportValue: " + exportAccess + ",\n"
               + "        factory: " + jsString(provider.binding().factory()) + ",\n"
               + "        metadata: {\n"
               + "            methods: " + renderNativeProviderMethods(provider.methods()) + "\n"
               + "        },\n"
               + "        create: () => " + renderNativeProviderFactory(provider, exportAccess) + "\n"
               + "    }),\n";
    }

    private String renderNativeProviderFunction(ProgramContext.NativeProviderInfo provider) {
        return "function " + provider.bootstrapFunctionName() + "() {\n"
               + "    return providers.resolve(" + jsString(provider.interfaceId()) + ", " + jsString(provider.qualifier())
               + ", " + jsString(provider.providerSymbolName()) + ", 'javascript', " + jsString(provider.sourceFile()) + ");\n"
               + "}\n";
    }

    private String renderNativeProviderFactory(ProgramContext.NativeProviderInfo provider, String factory) {
        if ("new".equals(provider.binding().factory())) {
            return "new " + factory + "()";
        }
        return factory + "()";
    }

    private static String nativeProviderModuleVariable(ProgramContext.NativeProviderInfo provider) {
        return "__capy_provider_" + provider.bootstrapFunctionName() + "_module";
    }

    private static String nativeProviderBootstrapKey(String interfaceId, String qualifier) {
        return interfaceId + "#" + qualifier;
    }

    private static String renderPropertyAccess(String target, String propertyName) {
        if (isValidJsIdentifier(propertyName) && !JS_KEYWORDS.contains(propertyName)) {
            return target + "." + propertyName;
        }
        return target + "[" + jsString(propertyName) + "]";
    }

    private static String renderNativeProviderMethods(List<ProgramContext.NativeProviderMethodInfo> methods) {
        return methods.stream()
                .map(method -> "{ name: " + jsString(method.name()) + ", arity: " + method.arity() + " }")
                .collect(joining(", ", "[", "]"));
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
            for (var javaConst : ConstDependencyOrder.order(javaClass.staticConsts(), programContext::emittedFunctionName)) {
                body.append(renderConst(javaConst)).append('\n');
            }
            for (var method : javaClass.staticMethods()) {
                body.append(renderFunction(method, true)).append('\n');
            }
            body.append(renderPrimitiveTypeMetadata());
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
            code.append("        return capy.methodAliasProxy(this);\n");
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
            var dataValueInfo = record.dataValueInfo();
            code.append("        return capy.dataValueInfo(this, ")
                    .append(jsString(dataValueInfo.name()))
                    .append(", ")
                    .append(jsString(dataValueInfo.packageName()))
                    .append(", ")
                    .append(jsString(dataValueInfo.packagePath()))
                    .append(", ")
                    .append(renderDataValueFieldDescriptors(dataValueInfo.fields(), dataValueInfo.packagePath()))
                    .append(", ")
                    .append(renderAnnotations(dataValueInfo.annotations()))
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
                var dataValueInfo = javaEnum.dataValueInfos().getFirst();
                code.append("const ").append(enumName)
                        .append(" = capy.enumValue(")
                        .append(jsString(enumName)).append(", ")
                        .append(jsString(enumName)).append(", ")
                        .append(jsArray(programContext.parentTypes(enumName))).append(", ")
                        .append("0, [], ")
                        .append(jsString(dataValueInfo.packageName())).append(", ")
                        .append(jsString(dataValueInfo.packagePath())).append(", ")
                        .append(renderDataValueMetadata(dataValueInfo))
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
                var dataValueInfo = javaEnum.dataValueInfos().get(i);
                code.append("        capy.enumValue(")
                        .append(jsString(value)).append(", ")
                        .append(jsString(enumName)).append(", ")
                        .append(jsArray(programContext.parentTypes(valueName))).append(", ")
                        .append(i).append(", ")
                        .append(jsArray(aliases)).append(", ")
                        .append(jsString(dataValueInfo.packageName())).append(", ")
                        .append(jsString(dataValueInfo.packagePath())).append(", ")
                        .append(renderDataValueMetadata(dataValueInfo)).append("),\n");
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
            var name = jsConstIdentifier(javaConst.name());
            if (!javaConst.isPrivate()) {
                exportNames.add(name);
            }
            return "const " + name + " = " + expression + ";\n";
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
            var body = isCapyLangRandomSeedMethod(method)
                    ? "capy.toLong(Date.now())"
                    : expressions.render(method.expression(), scope);
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

        private boolean isCapyLangRandomSeedMethod(JavaMethod method) {
            return "capy.lang.Random".equals(moduleInfo.className())
                   && "seed".equals(method.sourceName())
                   && method.parameters().isEmpty()
                   && method.sourceReturnType() instanceof CompiledPrimitiveBackedType primitiveBackedType
                   && "seed".equals(primitiveBackedType.name())
                   && method.expression() instanceof CompiledNothingValue nothingValue
                   && (nothingValue.message().contains("`<native>`")
                       || nothingValue.message().contains("native expression in function"));
        }

        private String renderPrimitiveTypeMetadata() {
            var primitiveBackedTypes = moduleInfo.module().types().values().stream()
                    .filter(CompiledPrimitiveBackedType.class::isInstance)
                    .map(CompiledPrimitiveBackedType.class::cast)
                    .toList();
            if (primitiveBackedTypes.isEmpty()) {
                return "";
            }
            exportNames.add("__capybaraPrimitiveTypes");
            var code = new StringBuilder();
            code.append("const __capybaraPrimitiveTypes = Object.freeze({\n");
            for (var type : primitiveBackedTypes) {
                code.append("    ").append(type.name()).append(": Object.freeze({ cfunType: ")
                        .append(jsString(type.cfunType()))
                        .append(", backingType: ")
                        .append(jsString(primitiveTypeName(type.backingType())))
                        .append(" }),\n");
            }
            code.append("});\n\n");
            return code.toString();
        }

        private String primitiveTypeName(PrimitiveLinkedType type) {
            return switch (type) {
                case BYTE -> "byte";
                case INT -> "int";
                case LONG -> "long";
                case FLOAT -> "float";
                case DOUBLE -> "double";
                case STRING -> "String";
                default -> throw new IllegalArgumentException("Unsupported primitive-backed type `" + type + "`");
            };
        }

        private String renderExports() {
            if (exportNames.isEmpty()) {
                return "module.exports = {};\n";
            }
            var exportAliases = new LinkedHashMap<String, List<String>>();
            for (var exportName : exportNames) {
                if (isPrimitiveBackedMethodExportName(exportName)) {
                    continue;
                }
                var overloadSeparator = exportName.indexOf("__");
                if (overloadSeparator > 0) {
                    exportAliases.computeIfAbsent(exportName.substring(0, overloadSeparator), ignored -> new ArrayList<>())
                            .add(exportName);
                }
            }
            var aliasDeclarations = new StringBuilder();
            var allExportNames = new ArrayList<>(exportNames);
            for (var entry : exportAliases.entrySet()) {
                if (exportNames.contains(entry.getKey())) {
                    continue;
                }
                aliasDeclarations.append("function ")
                        .append(entry.getKey())
                        .append("(...args) {\n")
                        .append("    return capy.dispatchOverload([\n");
                for (var overload : entry.getValue()) {
                    aliasDeclarations.append("        [")
                            .append(jsString(overload))
                            .append(", ")
                            .append(overload)
                            .append("],\n");
                }
                aliasDeclarations.append("    ], args);\n")
                        .append("}\n\n");
                allExportNames.add(entry.getKey());
            }
            return aliasDeclarations
                   + "module.exports = {\n"
                   + allExportNames.stream()
                           .map(name -> "    " + name + ",")
                           .collect(joining("\n"))
                   + "\n};\n";
        }

        private boolean isPrimitiveBackedMethodExportName(String exportName) {
            return exportName.contains("__name_") || exportName.contains("__op_");
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
                   + "    const value = " + mainName + "(process.argv.slice(2)).unsafe_run();\n"
                   + "    capy.writeProgramResult(value);\n"
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
                case CompiledObjectConstruction objectConstruction -> renderObjectConstruction(objectConstruction, scope);
                case CompiledIfExpression ifExpression -> "((" + renderBoolean(ifExpression.condition(), scope) + ") ? ("
                                                          + render(ifExpression.thenBranch(), scope) + ") : ("
                                                          + render(ifExpression.elseBranch(), scope) + "))";
                case CompiledInfixExpression infixExpression -> renderInfix(infixExpression, scope);
                case CompiledLetExpression letExpression -> renderLet(letExpression, scope);
                case CompiledLambdaExpression lambdaExpression -> renderLambda(lambdaExpression, scope);
                case CompiledIndexExpression indexExpression -> renderIndex(indexExpression, scope);
                case CompiledSliceExpression sliceExpression -> renderSlice(sliceExpression, scope);
                case CompiledNewData newData -> renderNewData(newData, scope);
                case CompiledUnwrapExpression unwrapExpression -> render(unwrapExpression.expression(), scope);
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

        private String renderObjectConstruction(CompiledObjectConstruction objectConstruction, Scope scope) {
            var className = programContext.resolveClassName(objectConstruction.objectType().backendClassName())
                    .orElse(objectConstruction.objectType().backendClassName());
            require(className);
            var typeName = simpleTypeName(className);
            var constructorReference = isCurrentClassReference(className)
                    ? typeName
                    : moduleVar(className) + "." + typeName;
            var arguments = objectConstruction.arguments().stream()
                    .map(argument -> render(argument, scope))
                    .collect(joining(", "));
            return "capy.delay(() => new " + constructorReference + "(" + arguments + "))";
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
            var nativeCall = renderRuntimeBackedFunctionCall(functionCall, args);
            if (nativeCall.isPresent()) {
                return nativeCall.orElseThrow();
            }
            var nativeProviderCall = renderNativeProviderFunctionCall(functionCall, args);
            if (nativeProviderCall.isPresent()) {
                return nativeProviderCall.orElseThrow();
            }
            var target = resolveFunctionTarget(functionCall);
            if (ConstDependencyOrder.isConstCall(functionCall)) {
                return target;
            }
            return target + "(" + String.join(", ", args) + ")";
        }

        private Optional<String> renderNativeProviderFunctionCall(CompiledFunctionCall functionCall, List<String> args) {
            if (!args.isEmpty()) {
                return Optional.empty();
            }
            var emittedName = emittedMethodName(functionCall);
            if (hasLocalFunction(functionCall, emittedName)) {
                return Optional.empty();
            }
            var importedOwner = programContext.importedMemberOwner(moduleInfo.className(), emittedName)
                    .or(() -> programContext.importedMemberOwner(moduleInfo.className(), simpleMethodName(functionCall.name())));
            if (importedOwner.isPresent()) {
                return Optional.empty();
            }
            return programContext.nativeProviderInfo(functionCall.name(), emittedName)
                    .map(provider -> {
                        requiredModules.putIfAbsent(NATIVE_PROVIDER_BOOTSTRAP_CLASS_NAME, NATIVE_PROVIDER_BOOTSTRAP_PATH);
                        return moduleVar(NATIVE_PROVIDER_BOOTSTRAP_CLASS_NAME) + "." + provider.bootstrapFunctionName() + "()";
                    });
        }

        private boolean hasLocalFunction(CompiledFunctionCall functionCall, String emittedName) {
            var parameterTypes = functionCall.arguments().stream().map(CompiledExpression::type).toList();
            return moduleInfo.javaClass().staticMethods().stream()
                    .anyMatch(method -> method.sourceParameterTypes().equals(parameterTypes)
                                        && (method.sourceName().equals(functionCall.name())
                                            || method.name().equals(emittedName)
                                            || programContext.emittedFunctionName(method.sourceName(), parameterTypes).equals(emittedName)));
        }

        private Optional<String> renderRuntimeBackedFunctionCall(CompiledFunctionCall functionCall, List<String> args) {
            var owner = functionOwner(functionCall);
            var methodName = simpleMethodName(functionCall.name());
            if (owner.filter("capy.lang.Primitives"::equals).isPresent() && args.size() == 1) {
                return switch (methodName) {
                    case "to_int", "toInt" -> Optional.of("capy.parseIntResult(" + args.getFirst() + ")");
                    case "to_long", "toLong" -> Optional.of("capy.parseLongResult(" + args.getFirst() + ")");
                        case "to_double", "toDouble" ->
                                Optional.of("capy.parseFloatResult(" + args.getFirst() + ", 'double')");
                        case "to_float", "toFloat" ->
                                Optional.of("capy.parseFloatResult(" + args.getFirst() + ", 'float')");
                    case "to_bool", "toBool" -> Optional.of("capy.parseBoolResult(" + args.getFirst() + ")");
                    default -> Optional.empty();
                };
            }
            if (owner.filter("capy.lang.Random"::equals).isPresent() && "seed".equals(methodName) && args.isEmpty()) {
                require("capy.lang.Random");
                return Optional.of("capy.toLong(Date.now())");
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
            if ("size".equals(methodName) && isStringLike(receiverType) && tailArgs.isEmpty()) {
                return "String(" + receiver + ").length";
            }
            if ("chars".equals(methodName) && isStringLike(receiverType) && tailArgs.isEmpty()) {
                require("capy.lang.String");
                return moduleVar("capy.lang.String") + ".chars(" + receiver + ")";
            }
            if (("char_at".equals(methodName) || "charAt".equals(methodName) || "get_char".equals(methodName) || "getChar".equals(methodName))
                && isStringLike(receiverType)
                && tailArgs.size() == 1) {
                require("capy.lang.String");
                return moduleVar("capy.lang.String") + ".getChar(" + receiver + ", " + tailArgs.getFirst() + ")";
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
            if ("size".equals(methodName) && isCollectionType(receiverType)) {
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
                return renderCollectionPlus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst(), functionCall.type());
            }
            if (List.of("-", "minus").contains(methodName) && tailArgs.size() == 1 && isNativeMinusType(receiverType)) {
                return renderCollectionMinus(receiverType, receiver, functionCall.arguments().get(1).type(), tailArgs.getFirst(), functionCall.type());
            }
            if (("contains".equals(methodName) || "?".equals(methodName)) && isCollectionType(receiverType)) {
                return renderContains(receiverType, receiver, tailArgs.getFirst());
            }
            if ("is_empty".equals(methodName) && isCollectionType(receiverType)) {
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
            if (receiverType instanceof CompiledPrimitiveBackedType) {
                var target = primitiveBackedMethodTarget(functionCall)
                        .orElseGet(() -> resolveFunctionTarget(functionCall));
                return target + "(" + String.join(", ", args) + ")";
            }
            var emittedName = emittedMethodName(functionCall);
            return "(" + receiver + ")." + emittedName + "(" + String.join(", ", tailArgs) + ")";
        }

        private Optional<String> primitiveBackedMethodTarget(CompiledFunctionCall functionCall) {
            if (!(functionCall.arguments().getFirst().type() instanceof CompiledPrimitiveBackedType primitiveBackedType)) {
                return Optional.empty();
            }
            var dotIndex = primitiveBackedType.cfunType().lastIndexOf('.');
            if (dotIndex < 0) {
                return Optional.empty();
            }
            var moduleName = primitiveBackedType.cfunType().substring(0, dotIndex);
            var ownerClassName = programContext.resolveClassName(moduleName).orElse(moduleName);
            var emittedName = emittedMethodName(functionCall);
            if (isCurrentClassReference(ownerClassName)) {
                return Optional.of(emittedName);
            }
            require(ownerClassName);
            return Optional.of(moduleVar(ownerClassName) + "." + emittedName);
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
                case MINUS -> expression.type() == PrimitiveLinkedType.STRING
                        ? "(" + renderStringConcat(left, right) + ")"
                        : "(" + renderCollectionMinus(expression, left, right) + ")";
                case MUL -> {
                    if (expression.type() == PrimitiveLinkedType.STRING) {
                        yield "(" + renderStringConcat(left, right) + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.LONG) {
                        yield "capy.longMul(" + left + ", " + right + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.INT) {
                        yield "capy.intMul(" + left + ", " + right + ")";
                    }
                    yield "((" + left + ") * (" + right + "))";
                }
                case DIV -> {
                    if (expression.type() == PrimitiveLinkedType.STRING) {
                        yield "(" + renderStringConcat(left, right) + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.INT) {
                        yield "capy.intDiv(" + left + ", " + right + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.LONG) {
                        yield "capy.longDiv(" + left + ", " + right + ")";
                    }
                    yield "((" + left + ") / (" + right + "))";
                }
                case MOD -> {
                    if (expression.type() == PrimitiveLinkedType.STRING) {
                        yield "(" + renderStringConcat(left, right) + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.LONG) {
                        yield "capy.longMod(" + left + ", " + right + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.INT) {
                        yield "capy.intMod(" + left + ", " + right + ")";
                    }
                    yield "((" + left + ") % (" + right + "))";
                }
                case POWER -> {
                    if (expression.type() == PrimitiveLinkedType.STRING) {
                        yield "(" + renderStringConcat(left, right) + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.LONG) {
                        yield "capy.longPow(" + left + ", " + right + ")";
                    }
                    if (expression.type() == PrimitiveLinkedType.INT) {
                        yield "capy.intPow(" + left + ", " + right + ")";
                    }
                    yield "Math.pow(" + left + ", " + right + ")";
                }
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

        private String renderStringConcat(String left, String right) {
            return "capy.toStringValue(" + left + ") + capy.toStringValue(" + right + ")";
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
            var tupleArgs = parseTuplePipeArguments(lambdaExpression.argumentName());
            if (tupleArgs.length > 0 && lambdaExpression.functionType().argumentType() instanceof CompiledTupleType tupleType) {
                var jsName = scope.reserve("__tupleItem");
                var child = scope.bind(lambdaExpression.argumentName(), jsName);
                var size = Math.min(tupleType.elementTypes().size(), tupleArgs.length);
                for (var i = 0; i < size; i++) {
                    if (!"_".equals(tupleArgs[i]) && !tupleArgs[i].isBlank()) {
                        child = child.bindExpression(tupleArgs[i], "capy.rawIndex(" + jsName + ", " + i + ")");
                    }
                }
                return "((" + jsName + ") => (" + render(lambdaExpression.expression(), child) + "))";
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
            if (newData.type() instanceof CompiledPrimitiveBackedType) {
                return newData.assignments().stream()
                        .filter(assignment -> "value".equals(assignment.name()))
                        .findFirst()
                        .map(assignment -> render(assignment.value(), scope))
                        .orElseThrow(() -> new IllegalStateException("Primitive-backed type construction requires a `value` argument"));
            }
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
            var tupleArgs = parseTuplePipeArguments(argumentName);
            if (tupleArgs.length > 0) {
                var jsName = scope.reserve("__tupleItem");
                var child = scope.bind(argumentName, jsName);
                for (var i = 0; i < tupleArgs.length; i++) {
                    if (!"_".equals(tupleArgs[i]) && !tupleArgs[i].isBlank()) {
                        child = child.bindExpression(tupleArgs[i], "capy.rawIndex(" + jsName + ", " + i + ")");
                    }
                }
                return "((" + jsName + ") => (" + render(body, child) + "))";
            }
            var args = parseDictPipeArguments(argumentName);
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

        private static String[] parseDictPipeArguments(String argumentName) {
            if (!argumentName.contains(DICT_PIPE_ARGS_SEPARATOR)) {
                return new String[0];
            }
            var parts = argumentName.split(java.util.regex.Pattern.quote(DICT_PIPE_ARGS_SEPARATOR), -1);
            if (parts.length != 2 || parts[0].isBlank() || parts[1].isBlank()) {
                return new String[0];
            }
            return parts;
        }

        private static String[] parseTuplePipeArguments(String argumentName) {
            if (!argumentName.contains(TUPLE_PIPE_ARGS_SEPARATOR)) {
                return new String[0];
            }
            return argumentName.split(java.util.regex.Pattern.quote(TUPLE_PIPE_ARGS_SEPARATOR), -1);
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
                   + renderReflectionFieldDescriptors(reflectionValue.fields(), reflectionValue.packagePath()) + ", "
                   + renderAnnotations(reflectionValue.annotations())
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

        private boolean isStringLike(CompiledType type) {
            return type == PrimitiveLinkedType.STRING
                   || (type instanceof CompiledPrimitiveBackedType primitiveBackedType
                       && primitiveBackedType.backingType() == PrimitiveLinkedType.STRING);
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
            if (resultType == PrimitiveLinkedType.INT) {
                return "capy.intAdd(" + left + ", " + right + ")";
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
            if (resultType == PrimitiveLinkedType.INT) {
                return "capy.intSub(" + left + ", " + right + ")";
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
                    case "to_double" -> returnType instanceof GenericDataType
                            ? "capy.parseFloatResult(" + receiver + ", 'double')"
                            : "Number(" + receiver + ")";
                    case "to_float" -> returnType instanceof GenericDataType
                            ? "capy.parseFloatResult(" + receiver + ", 'float')"
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
                var importedOwner = programContext.importedMemberOwner(moduleInfo.className(), localTypeName)
                        .or(() -> programContext.importedMemberOwner(moduleInfo.className(), normalizeJsIdentifier(localTypeName)));
                if (importedOwner.isPresent()) {
                    var ownerClassName = programContext.resolveClassName(importedOwner.orElseThrow()).orElse(importedOwner.orElseThrow());
                    require(ownerClassName);
                    return moduleVar(ownerClassName) + "." + localTypeName + "." + emittedName;
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
            if (isCurrentClassReference(className) || isCurrentClassReference(resolvedClassName)) {
                return;
            }
            programContext.pathForClassName(resolvedClassName)
                    .ifPresent(path -> requiredModules.putIfAbsent(resolvedClassName, path));
        }

        private boolean isCurrentClassReference(String className) {
            if (className.equals(moduleInfo.className())) {
                return true;
            }
            var currentClassAliases = classNameCandidates(moduleInfo.className());
            if (currentClassAliases.contains(className)) {
                return true;
            }
            return classNameCandidates(className).stream()
                    .anyMatch(candidate -> candidate.equals(moduleInfo.className()) || currentClassAliases.contains(candidate));
        }

        private String dataConstructorReference(CompiledDataType dataType) {
            var qualifiedOwner = qualifiedTypeOwnerClassName(dataType.name())
                    .or(() -> dottedTypeOwnerClassName(dataType.name()));
            if (qualifiedOwner.isPresent()) {
                var ownerClassName = qualifiedOwner.orElseThrow();
                var typeName = programContext.emittedTypeName(ownerClassName, dataType.name());
                if (isCurrentClassReference(ownerClassName)) {
                    return typeName;
                }
                var className = programContext.resolveClassName(ownerClassName).orElse(ownerClassName);
                require(className);
                return moduleVar(className) + "." + typeName;
            }
            var typeName = programContext.emittedTypeName(moduleInfo.className(), dataType.name());
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

        private Optional<String> dottedTypeOwnerClassName(String typeName) {
            if (typeName.startsWith("/")) {
                return Optional.empty();
            }
            var generic = typeName.indexOf('[');
            var stripped = generic >= 0 ? typeName.substring(0, generic) : typeName;
            var dot = stripped.lastIndexOf('.');
            if (dot < 0) {
                return Optional.empty();
            }
            var ownerName = stripped.substring(0, dot).replace('\\', '.').replace('/', '.');
            while (ownerName.startsWith(".")) {
                ownerName = ownerName.substring(1);
            }
            if (ownerName.isBlank()) {
                return Optional.empty();
            }
            var resolved = programContext.resolveClassName(ownerName);
            if (resolved.isPresent()) {
                return resolved;
            }
            if (!ownerName.contains(".") && !moduleInfo.packageName().isBlank()) {
                var packageOwner = moduleInfo.packageName() + "." + ownerName;
                var packageResolved = programContext.resolveClassName(packageOwner);
                if (packageResolved.isPresent()) {
                    return packageResolved;
                }
            }
            return Optional.empty();
        }

        private static Optional<String> qualifiedTypeOwnerClassName(String typeName) {
            if (!typeName.startsWith("/")) {
                return Optional.empty();
            }
            var ownerPath = typeName.substring(1);
            var dot = ownerPath.lastIndexOf('.');
            if (dot < 0) {
                return Optional.empty();
            }
            return Optional.of(ownerPath.substring(0, dot).replace('/', '.'));
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

        Scope bindExpression(String sourceName, String jsExpression) {
            var updatedBindings = new HashMap<>(bindings);
            updatedBindings.put(sourceName, jsExpression);
            return new Scope(Map.copyOf(updatedBindings), usedNames);
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

    private static String renderDataValueMetadata(JavaDataValueInfo dataValueInfo) {
        return "{ fields: "
               + renderDataValueFieldDescriptors(dataValueInfo.fields(), dataValueInfo.packagePath())
               + ", annotations: "
               + renderAnnotations(dataValueInfo.annotations())
               + " }";
    }

    private static String renderDataValueFieldDescriptors(List<JavaDataValueInfo.Field> fields, String fallbackPackagePath) {
        if (fields.isEmpty()) {
            return "[]";
        }
        return fields.stream()
                .map(field -> "{ name: "
                              + jsString(field.name())
                              + ", type: "
                              + renderReflectionTypeInfo(field.type(), fallbackPackagePath)
                              + ", annotations: "
                              + renderAnnotations(field.annotations())
                              + " }")
                .collect(joining(", ", "[", "]"));
    }

    private static String renderReflectionFieldDescriptors(List<CompiledReflectionValue.Field> fields, String fallbackPackagePath) {
        if (fields.isEmpty()) {
            return "[]";
        }
        return fields.stream()
                .map(field -> "{ name: "
                              + jsString(field.name())
                              + ", type: "
                              + renderReflectionTypeInfo(field.type(), fallbackPackagePath)
                              + ", annotations: "
                              + renderAnnotations(field.annotations())
                              + " }")
                .collect(joining(", ", "[", "]"));
    }

    static String renderReflectionTypeInfo(CompiledType type, String fallbackPackagePath) {
        return switch (type) {
            case PrimitiveLinkedType primitive ->
                    renderDataInfo(primitiveReflectionTypeName(primitive), renderEmptyReflectionPackage(), List.of());
            case CollectionLinkedType.CompiledList listType ->
                    "{ kind: 'list', name: 'List', pkg: " + renderEmptyReflectionPackage()
                    + ", element_type: " + renderReflectionTypeInfo(listType.elementType(), fallbackPackagePath) + " }";
            case CollectionLinkedType.CompiledSet setType ->
                    "{ kind: 'set', name: 'Set', pkg: " + renderEmptyReflectionPackage()
                    + ", element_type: " + renderReflectionTypeInfo(setType.elementType(), fallbackPackagePath) + " }";
            case CollectionLinkedType.CompiledDict dictType ->
                    "{ kind: 'dict', name: 'Dict', pkg: " + renderEmptyReflectionPackage()
                    + ", value_type: " + renderReflectionTypeInfo(dictType.valueType(), fallbackPackagePath) + " }";
            case CompiledTupleType tupleType -> {
                var elements = tupleType.elementTypes().stream()
                        .map(elementType -> renderReflectionTypeInfo(elementType, fallbackPackagePath))
                        .collect(joining(", ", "[", "]"));
                yield "{ kind: 'tuple', name: 'Tuple', pkg: " + renderEmptyReflectionPackage()
                      + ", elements: " + elements + " }";
            }
            case CompiledFunctionType functionType -> {
                var shape = flattenReflectionFunctionType(functionType);
                var params = shape.parameterTypes().stream()
                        .map(parameterType -> renderReflectionTypeInfo(parameterType, fallbackPackagePath))
                        .collect(joining(", ", "[", "]"));
                yield "{ kind: 'function', name: 'function', pkg: " + renderEmptyReflectionPackage()
                      + ", params: " + params
                      + ", return_type: " + renderReflectionTypeInfo(shape.returnType(), fallbackPackagePath)
                      + " }";
            }
            case CompiledGenericTypeParameter genericTypeParameter ->
                    renderDataInfo(genericTypeParameter.name(), renderEmptyReflectionPackage(), List.of());
            case CompiledPrimitiveBackedType primitiveBackedType ->
                    renderReflectionTypeInfo(primitiveBackedType.backingType(), fallbackPackagePath);
            case CompiledDataParentType parentType ->
                    renderDataInfo(
                            simpleReflectionTypeName(parentType.name()),
                            renderReflectionPackageForType(parentType.name(), fallbackPackagePath),
                            parentType.annotations()
                    );
            case CompiledDataType dataType ->
                    renderDataInfo(
                            simpleReflectionTypeName(dataType.name()),
                            renderReflectionPackageForType(dataType.name(), fallbackPackagePath),
                            dataType.annotations()
                    );
            case CompiledObjectType objectType ->
                    renderDataInfo(
                            simpleReflectionTypeName(objectType.name()),
                            renderReflectionPackageForType(objectType.name(), fallbackPackagePath),
                            objectType.annotations()
                    );
        };
    }

    private static ReflectionFunctionShape flattenReflectionFunctionType(CompiledFunctionType functionType) {
        var parameterTypes = new ArrayList<CompiledType>();
        CompiledType current = functionType;
        while (current instanceof CompiledFunctionType currentFunctionType) {
            parameterTypes.add(currentFunctionType.argumentType());
            current = currentFunctionType.returnType();
        }
        return new ReflectionFunctionShape(List.copyOf(parameterTypes), current);
    }

    private static String renderDataInfo(String name, String pkg, List<CompiledAnnotation> annotations) {
        return "{ kind: 'data', name: "
               + jsString(name)
               + ", pkg: "
               + pkg
               + ", annotations: "
               + renderAnnotations(annotations)
               + " }";
    }

    private static String primitiveReflectionTypeName(PrimitiveLinkedType type) {
        return type == PrimitiveLinkedType.STRING
                ? "String"
                : type.name().toLowerCase(java.util.Locale.ROOT);
    }

    static String renderAnnotations(List<CompiledAnnotation> annotations) {
        if (annotations == null || annotations.isEmpty()) {
            return "[]";
        }
        return annotations.stream()
                .map(JavaScriptGenerator::renderAnnotation)
                .collect(joining(", ", "[", "]"));
    }

    private static String renderAnnotation(CompiledAnnotation annotation) {
        var arguments = annotation.arguments().stream()
                .map(argument -> "{ name: "
                                 + jsString(argument.name())
                                 + ", value: "
                                 + renderAnnotationValue(argument.value())
                                 + " }")
                .collect(joining(", ", "[", "]"));
        return "{ name: "
               + jsString(annotation.name())
               + ", pkg: "
               + renderReflectionPackage(annotation.packageName(), annotation.packagePath())
               + ", arguments: "
               + arguments
               + " }";
    }

    private static String renderAnnotationValue(CompiledAnnotationValue value) {
        return switch (value) {
            case CompiledAnnotationValue.StringValue stringValue ->
                    renderAnnotationValue("AnnotationString", "string", jsString(normalizeAnnotationStringValue(stringValue.value())));
            case CompiledAnnotationValue.IntValue intValue ->
                    renderAnnotationValue("AnnotationInt", "int", stripNumericSuffix(intValue.value()));
            case CompiledAnnotationValue.LongValue longValue ->
                    renderAnnotationValue("AnnotationLong", "long", renderLongLiteral(longValue.value()));
            case CompiledAnnotationValue.DoubleValue doubleValue ->
                    renderAnnotationValue("AnnotationDouble", "double", stripNumericSuffix(doubleValue.value()));
            case CompiledAnnotationValue.FloatValue floatValue ->
                    renderAnnotationValue("AnnotationFloat", "float", stripNumericSuffix(floatValue.value()));
            case CompiledAnnotationValue.BoolValue boolValue ->
                    renderAnnotationValue("AnnotationBool", "bool", String.valueOf(boolValue.value()));
            case CompiledAnnotationValue.TypeNameValue typeNameValue ->
                    renderAnnotationValue("AnnotationTypeName", "type_name", jsString(typeNameValue.name()));
            case CompiledAnnotationValue.NothingValue ignored ->
                    renderAnnotationValue("AnnotationNothing", "nothing", null);
        };
    }

    private static String renderAnnotationValue(String typeName, String kind, String valueExpression) {
        return "{ __capybaraType: "
               + jsString(typeName)
               + ", __capybaraTypes: ["
               + jsString(typeName)
               + ", 'AnnotationValue', 'CapybaraDataValue'], kind: "
               + jsString(kind)
               + (valueExpression == null ? "" : ", value: " + valueExpression)
               + " }";
    }

    private static String normalizeAnnotationStringValue(String raw) {
        if (raw.length() < 2) {
            return raw;
        }
        if (raw.charAt(0) == '"' && raw.charAt(raw.length() - 1) == '"') {
            return normalizeAnnotationDoubleQuotedContent(raw.substring(1, raw.length() - 1));
        }
        if (raw.charAt(0) == '\'' && raw.charAt(raw.length() - 1) == '\'') {
            return raw.substring(1, raw.length() - 1);
        }
        return raw;
    }

    private static String normalizeAnnotationDoubleQuotedContent(String content) {
        var normalized = new StringBuilder(content.length());
        for (var i = 0; i < content.length(); i++) {
            var ch = content.charAt(i);
            if (ch == '\\' && i + 1 < content.length()) {
                var next = content.charAt(i + 1);
                if (next == '"' || next == '\\') {
                    normalized.append(next);
                    i++;
                    continue;
                }
            }
            normalized.append(ch);
        }
        return normalized.toString();
    }

    private static String renderReflectionPackageForType(String symbolName, String fallbackPackagePath) {
        var path = reflectionPackagePath(symbolName, fallbackPackagePath);
        var name = path.isBlank() ? "" : simpleReflectionTypeName(path);
        return renderReflectionPackage(name, path);
    }

    private static String renderReflectionPackage(String packageName, String packagePath) {
        return "{ name: "
               + jsString(packageName == null ? "" : packageName)
               + ", path: "
               + jsString(packagePath == null ? "" : packagePath.replaceFirst("^/", ""))
               + " }";
    }

    static String renderEmptyReflectionPackage() {
        return "{ name: '', path: '' }";
    }

    private static String reflectionPackagePath(String symbolName, String fallbackPackagePath) {
        var normalized = symbolName.replace('\\', '/');
        var dot = normalized.lastIndexOf('.');
        var slash = normalized.lastIndexOf('/');
        if (slash >= 0) {
            if (dot > slash) {
                return normalized.substring(0, dot).replaceFirst("^/", "");
            }
            return normalized.substring(0, slash).replaceFirst("^/", "");
        }
        if (dot > 0) {
            return normalized.substring(0, dot);
        }
        return fallbackPackagePath == null ? "" : fallbackPackagePath;
    }

    private static String simpleReflectionTypeName(String typeName) {
        var normalized = stripGenericSuffix(typeName);
        var slash = normalized.lastIndexOf('/');
        var dot = normalized.lastIndexOf('.');
        var index = Math.max(slash, dot);
        return index >= 0 ? normalized.substring(index + 1) : normalized;
    }

    private static String stripGenericSuffix(String typeName) {
        var idx = typeName.indexOf('[');
        return idx >= 0 ? typeName.substring(0, idx) : typeName;
    }

    private record ReflectionFunctionShape(List<CompiledType> parameterTypes, CompiledType returnType) {
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
            Map<String, PrimitiveBackedTypeInfo> primitiveBackedTypesByName,
            Map<String, SingletonDataTypeInfo> singletonDataTypesByName,
            List<NativeProviderInfo> nativeProviderInfos,
            Map<String, List<NativeProviderInfo>> nativeProvidersByModule,
            Map<String, String> functionNameOverrides
    ) {
        static ProgramContext build(List<ModuleInfo> modules, Map<String, String> functionNameOverrides) {
            return build(
                    modules,
                    modules.stream().map(ModuleInfo::module).toList(),
                    List.of(),
                    NativeProviderCatalog.empty(),
                    functionNameOverrides
            );
        }

        static ProgramContext build(
                List<ModuleInfo> modules,
                Collection<CompiledModule> compiledModules,
                List<dev.capylang.compiler.parser.ObjectOrientedModule> objectOrientedModules,
                NativeProviderCatalog nativeProviderCatalog,
                Map<String, String> functionNameOverrides
        ) {
            var paths = new LinkedHashMap<String, Path>();
            var exports = runtimeExports();
            var localTypes = new LinkedHashMap<String, Set<String>>();
            var fields = standardFields();
            var parentTypes = new LinkedHashMap<String, List<String>>();
            var singletonDataTypes = new LinkedHashMap<String, SingletonDataTypeInfo>();

            for (var runtimeClassName : RuntimeModules.classNames()) {
                paths.put(runtimeClassName, classNamePath(runtimeClassName));
            }
            compiledModules.forEach(module -> module.types().values().stream()
                    .filter(CompiledDataType.class::isInstance)
                    .map(CompiledDataType.class::cast)
                    .filter(CompiledDataType::singleton)
                    .filter(type -> !type.enumValue())
                    .forEach(type -> putSingletonDataTypeAliases(
                            singletonDataTypes,
                            new SingletonDataTypeInfo(type.name(), cfunTypeName(module, type.name())),
                            module
                    )));
            putRuntimeSingletonDataTypeAliases(singletonDataTypes);
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
                        .map(JavaScriptGenerator::jsConstIdentifier)
                        .forEach(moduleExports::add);
                module.javaClass().staticMethods().stream()
                        .filter(method -> !method.isPrivate())
                        .map(JavaMethod::name)
                        .forEach(moduleExports::add);
                exports.put(module.className(), Set.copyOf(moduleExports));
                localTypes.put(module.className(), Set.copyOf(moduleTypes));
                var companionClassName = module.className() + "Module";
                paths.putIfAbsent(companionClassName, module.relativePath());
                exports.putIfAbsent(companionClassName, Set.copyOf(moduleExports));
                localTypes.putIfAbsent(companionClassName, Set.copyOf(moduleTypes));
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
                    var resolvedClassName = resolveStaticImportOwner(className, exports);
                    var memberName = staticImport.substring(idx + 1);
                    if ("*".equals(memberName)) {
                        exports.getOrDefault(resolvedClassName, Set.of()).forEach(member -> imported.putIfAbsent(member, resolvedClassName));
                    } else {
                        imported.putIfAbsent(memberName, resolvedClassName);
                        imported.putIfAbsent(simpleTypeName(memberName), resolvedClassName);
                        imported.putIfAbsent(normalizeJsIdentifier(memberName), resolvedClassName);
                    }
                }
                importedOwners.put(module.className(), Map.copyOf(imported));
            }
            var primitiveBackedTypes = primitiveBackedTypes(modules);
            var nativeProviderInfos = nativeProviderInfos(nativeProviderCatalog, compiledModules, objectOrientedModules);
            var nativeProvidersByModule = nativeProviderInfos.stream()
                    .collect(java.util.stream.Collectors.groupingBy(
                            provider -> moduleKey(provider.sourceModulePath(), provider.sourceModuleName()),
                            LinkedHashMap::new,
                            java.util.stream.Collectors.toUnmodifiableList()
                    ));
            putUniqueSimpleSingletonDataTypeAliases(singletonDataTypes);

            return new ProgramContext(
                    Map.copyOf(paths),
                    Map.copyOf(exports),
                    Map.copyOf(localTypes),
                    Map.copyOf(importedOwners),
                    Map.copyOf(fields),
                    Map.copyOf(parentTypes),
                    Map.copyOf(primitiveBackedTypes),
                    Map.copyOf(singletonDataTypes),
                    List.copyOf(nativeProviderInfos),
                    Map.copyOf(nativeProvidersByModule),
                    Map.copyOf(functionNameOverrides)
            );
        }

        private static List<NativeProviderInfo> nativeProviderInfos(
                NativeProviderCatalog catalog,
                Collection<CompiledModule> compiledModules,
                List<ObjectOrientedModule> objectOrientedModules
        ) {
            if (catalog.declarations().isEmpty()) {
                return List.of();
            }
            var bindings = nativeProviderBindings(catalog.bindings());
            var interfaces = nativeProviderInterfaceTypes(compiledModules, objectOrientedModules);
            var baseNames = new LinkedHashMap<String, Integer>();
            for (var declaration : catalog.declarations()) {
                baseNames.merge(nativeProviderIdentifier(declaration.providerName()), 1, Integer::sum);
            }

            var usedNames = new LinkedHashSet<String>();
            var infos = new ArrayList<NativeProviderInfo>();
            for (var declaration : catalog.declarations()) {
                var binding = bindings.get(nativeProviderKey(declaration.interfaceId(), declaration.qualifier()));
                var javascriptBinding = binding == null ? null : binding.javascriptBinding();
                if (javascriptBinding == null) {
                    throw new IllegalArgumentException("UnsupportedBackend: Native provider `" + declaration.providerName()
                                                       + "` for interface `" + declaration.interfaceId()
                                                       + "` with qualifier `" + declaration.qualifier()
                                                       + "` for backend `javascript` has no javascript binding in source `"
                                                       + declaration.sourceFile() + "`");
                }
                var interfaceType = interfaces.get(declaration.interfaceId());
                if (interfaceType == null) {
                    throw new IllegalArgumentException("TypeMismatch: Native provider `" + declaration.providerName()
                                                       + "` targets unknown interface `" + declaration.interfaceId()
                                                       + "` with qualifier `" + declaration.qualifier()
                                                       + "` in source `" + declaration.sourceFile() + "`");
                }
                var baseName = nativeProviderIdentifier(declaration.providerName());
                var bootstrapName = baseNames.getOrDefault(baseName, 0) == 1
                        ? baseName
                        : uniqueNativeProviderBootstrapName(baseName, declaration, usedNames);
                usedNames.add(bootstrapName);
                infos.add(new NativeProviderInfo(
                        declaration.providerName(),
                        bootstrapName,
                        interfaceType.backendClassName(),
                        declaration.interfaceId(),
                        declaration.qualifier(),
                        declaration.sourceModulePath(),
                        declaration.sourceModuleName(),
                        declaration.sourceFile(),
                        binding.lifetime(),
                        javascriptBinding,
                        interfaceType.methods()
                ));
            }
            return List.copyOf(infos);
        }

        private static Map<String, CompiledNativeProviderBinding> nativeProviderBindings(List<CompiledNativeProviderBinding> bindings) {
            var result = new LinkedHashMap<String, CompiledNativeProviderBinding>();
            for (var binding : bindings) {
                result.putIfAbsent(nativeProviderKey(binding.interfaceId(), binding.qualifier()), binding);
            }
            return Map.copyOf(result);
        }

        private static Map<String, ProviderInterfaceInfo> nativeProviderInterfaceTypes(
                Collection<CompiledModule> modules,
                List<ObjectOrientedModule> objectOrientedModules
        ) {
            var result = new LinkedHashMap<String, ProviderInterfaceInfo>();
            for (var module : modules) {
                for (var type : module.types().values()) {
                    if (type instanceof CompiledObjectType objectType && objectType.kind() == CompiledObjectKind.INTERFACE) {
                        var info = new ProviderInterfaceInfo(
                                objectType.backendClassName(),
                                objectType.methods().stream().map(ProgramContext::nativeProviderMethodInfo).toList()
                        );
                        result.put(cfunTypeName(module, objectType.name()), info);
                        if (module.name().equals(objectType.name())) {
                            result.put(cfunModuleName(module), info);
                        }
                    }
                }
            }
            for (var module : objectOrientedModules) {
                for (var definition : module.objectOriented().definitions()) {
                    if (definition instanceof ObjectOriented.InterfaceDeclaration) {
                        result.put(
                                objectInterfaceId(module, definition.name()),
                                new ProviderInterfaceInfo(
                                        objectBackendClassName(module, definition.name()),
                                        definition.members().stream()
                                                .filter(ObjectOriented.MethodDeclaration.class::isInstance)
                                                .map(ObjectOriented.MethodDeclaration.class::cast)
                                                .map(method -> new NativeProviderMethodInfo(
                                                        nativeProviderIdentifier(method.name()),
                                                        method.parameters().size()
                                                ))
                                                .toList()
                                )
                        );
                    }
                }
            }
            return Map.copyOf(result);
        }

        private static NativeProviderMethodInfo nativeProviderMethodInfo(CompiledObjectMethod method) {
            return new NativeProviderMethodInfo(
                    nativeProviderIdentifier(method.name()),
                    method.parameters().size()
            );
        }

        private static String objectInterfaceId(ObjectOrientedModule module, String typeName) {
            var moduleName = objectModuleName(module);
            var qualified = module.name().equals(typeName)
                    ? moduleName
                    : moduleName + "." + typeName;
            return qualified.startsWith("/") ? qualified : "/" + qualified;
        }

        private static String objectModuleName(ObjectOrientedModule module) {
            var path = module.path().replace('\\', '/').replaceFirst("^/+", "").replaceFirst("/+$", "");
            return path.isBlank() ? module.name() : path + "/" + module.name();
        }

        private static String objectBackendClassName(ObjectOrientedModule module, String typeName) {
            var packageName = ObjectOrientedJavaScriptGenerator.packageName(module.path());
            return packageName.isBlank() ? typeName : packageName + "." + typeName;
        }

        private static String uniqueNativeProviderBootstrapName(
                String baseName,
                CompiledNativeProviderDeclaration declaration,
                Set<String> usedNames
        ) {
            var suffix = normalizeJsIdentifier((declaration.sourceModulePath() + "_" + declaration.sourceModuleName())
                    .replaceAll("[^A-Za-z0-9_]+", "_"));
            var candidate = baseName + "__" + suffix;
            var index = 2;
            while (usedNames.contains(candidate)) {
                candidate = baseName + "__" + suffix + "_" + index;
                index++;
            }
            return candidate;
        }

        private static String nativeProviderKey(String interfaceId, String qualifier) {
            return interfaceId + "\u0000" + qualifier;
        }

        private static String nativeProviderIdentifier(String name) {
            return isValidJsIdentifier(name) && !JS_KEYWORDS.contains(name)
                    ? name
                    : normalizeJsIdentifier(name);
        }

        private static String cfunModuleName(CompiledModule module) {
            var path = module.path().replace('\\', '/').replaceFirst("^/+", "").replaceFirst("/+$", "");
            return path.isBlank() ? "/" + module.name() : "/" + path + "/" + module.name();
        }

        private static String moduleKey(String path, String name) {
            var normalizedPath = path.replace('\\', '/')
                    .replaceFirst("^/+", "")
                    .replaceFirst("/+$", "");
            if (normalizedPath.isBlank() || ".".equals(normalizedPath)) {
                return name;
            }
            return normalizedPath + "/" + name;
        }

        private static String importedModuleKey(ObjectOrientedModule module, String moduleName) {
            if (moduleName.startsWith("/")) {
                return moduleName.replace('\\', '/')
                        .replaceFirst("^/+", "")
                        .replaceFirst("/+$", "");
            }
            var basePath = module.path().replace('\\', '/')
                    .replaceFirst("^/+", "")
                    .replaceFirst("/+$", "");
            if (basePath.isBlank() || ".".equals(basePath)) {
                return moduleName;
            }
            return basePath + "/" + moduleName;
        }

        private static void putSingletonDataTypeAliases(
                Map<String, SingletonDataTypeInfo> result,
                SingletonDataTypeInfo info,
                CompiledModule module
        ) {
            putSingletonDataTypeAliases(result, info);
            result.putIfAbsent(module.name() + "." + info.name(), info);
        }

        private static void putRuntimeSingletonDataTypeAliases(Map<String, SingletonDataTypeInfo> result) {
            putSingletonDataTypeAliases(result, new SingletonDataTypeInfo("None", "/capy/lang/Option.None"));
            putSingletonDataTypeAliases(result, new SingletonDataTypeInfo("Success", "/capy/lang/Program.Success"));
        }

        private static void putSingletonDataTypeAliases(
                Map<String, SingletonDataTypeInfo> result,
                SingletonDataTypeInfo info
        ) {
            if (info.name().contains("/") || info.name().contains(".")) {
                result.putIfAbsent(info.name(), info);
            }
            result.putIfAbsent(info.cfunType(), info);
            result.putIfAbsent(withoutLeadingSlash(info.cfunType()), info);
        }

        private static void putUniqueSimpleSingletonDataTypeAliases(Map<String, SingletonDataTypeInfo> result) {
            var bySimpleName = result.values().stream()
                    .distinct()
                    .collect(java.util.stream.Collectors.groupingBy(
                            info -> simpleTypeName(info.name()),
                            LinkedHashMap::new,
                            java.util.stream.Collectors.toList()
                    ));
            bySimpleName.forEach((simpleName, infos) -> {
                if (infos.size() == 1) {
                    result.putIfAbsent(simpleName, infos.getFirst());
                }
            });
        }

        private static String cfunTypeName(ModuleInfo module, String typeName) {
            return cfunTypeName(module.module(), typeName);
        }

        private static String cfunTypeName(CompiledModule module, String typeName) {
            var path = module.path().replace('\\', '/').replaceFirst("^/+", "").replaceFirst("/+$", "");
            var owner = path.isBlank() ? "/" + module.name() : "/" + path + "/" + module.name();
            return owner + "." + typeName;
        }

        private static Map<String, PrimitiveBackedTypeInfo> primitiveBackedTypes(List<ModuleInfo> modules) {
            var result = new LinkedHashMap<String, PrimitiveBackedTypeInfo>();
            modules.forEach(module -> module.module().types().values().stream()
                    .filter(CompiledPrimitiveBackedType.class::isInstance)
                    .map(CompiledPrimitiveBackedType.class::cast)
                    .forEach(type -> {
                        var info = new PrimitiveBackedTypeInfo(
                                type.name(),
                                type.cfunType(),
                                primitiveTypeName(type.backingType()),
                                !constructorTypes(module).contains(type.name())
                        );
                        putPrimitiveBackedTypeAliases(result, info, module.module());
                    }));
            modules.forEach(module -> module.module().visiblePrimitiveBackedTypes().forEach((alias, type) -> {
                var info = result.values().stream()
                        .filter(existing -> existing.cfunType().equals(type.cfunType()))
                        .findFirst()
                        .orElseGet(() -> new PrimitiveBackedTypeInfo(
                                type.name(),
                                type.cfunType(),
                                primitiveTypeName(type.backingType()),
                                false
                        ));
                if (alias.contains("/") || alias.contains(".")) {
                    result.putIfAbsent(alias, info);
                }
                putPrimitiveBackedTypeAliases(result, info);
            }));
            putUniqueSimplePrimitiveBackedTypeAliases(result);
            return result;
        }

        private static Set<String> constructorTypes(ModuleInfo module) {
            return module.module().functions().stream()
                    .map(CompiledFunction::name)
                    .filter(name -> name.startsWith(PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX))
                    .map(name -> name.substring(PRIMITIVE_BACKED_TYPE_CONSTRUCTOR_FUNCTION_PREFIX.length()))
                    .collect(java.util.stream.Collectors.toUnmodifiableSet());
        }

        private static void putPrimitiveBackedTypeAliases(
                Map<String, PrimitiveBackedTypeInfo> result,
                PrimitiveBackedTypeInfo info,
                CompiledModule module
        ) {
            if (info.name().contains("/") || info.name().contains(".")) {
                result.putIfAbsent(info.name(), info);
            }
            result.putIfAbsent(info.cfunType(), info);
            result.putIfAbsent(withoutLeadingSlash(info.cfunType()), info);
            result.putIfAbsent(module.name() + "." + info.name(), info);
        }

        private static void putPrimitiveBackedTypeAliases(
                Map<String, PrimitiveBackedTypeInfo> result,
                PrimitiveBackedTypeInfo info
        ) {
            if (info.name().contains("/") || info.name().contains(".")) {
                result.putIfAbsent(info.name(), info);
            }
            result.putIfAbsent(info.cfunType(), info);
            result.putIfAbsent(withoutLeadingSlash(info.cfunType()), info);
        }

        private static void putUniqueSimplePrimitiveBackedTypeAliases(Map<String, PrimitiveBackedTypeInfo> result) {
            var bySimpleName = result.values().stream()
                    .distinct()
                    .collect(java.util.stream.Collectors.groupingBy(
                            info -> simpleTypeName(info.name()),
                            LinkedHashMap::new,
                            java.util.stream.Collectors.toList()
                    ));
            bySimpleName.forEach((simpleName, infos) -> {
                if (infos.size() == 1) {
                    result.putIfAbsent(simpleName, infos.getFirst());
                }
            });
        }

        private static String withoutLeadingSlash(String value) {
            return value.startsWith("/") ? value.substring(1) : value;
        }

        private static String primitiveTypeName(PrimitiveLinkedType type) {
            return switch (type) {
                case BYTE -> "byte";
                case INT -> "int";
                case LONG -> "long";
                case FLOAT -> "float";
                case DOUBLE -> "double";
                case STRING -> "String";
                default -> throw new IllegalArgumentException("Unsupported primitive-backed type `" + type + "`");
            };
        }

        Optional<PrimitiveBackedTypeInfo> primitiveBackedType(String rawType) {
            return primitiveBackedType(null, rawType);
        }

        Optional<PrimitiveBackedTypeInfo> primitiveBackedType(ObjectOrientedModule module, String rawType) {
            var normalized = rawType.trim();
            if (normalized.endsWith("!")) {
                normalized = normalized.substring(0, normalized.length() - 1).trim();
            }
            var direct = primitiveBackedTypesByName.get(normalized);
            if (direct != null) {
                return Optional.of(direct);
            }
            if (module != null) {
                var imported = importedPrimitiveBackedType(module, normalized);
                if (imported.isPresent()) {
                    return imported;
                }
            }
            return Optional.ofNullable(primitiveBackedTypesByName.get(simpleTypeName(normalized)));
        }

        Optional<SingletonDataTypeInfo> singletonDataType(ObjectOrientedModule module, String rawType) {
            var normalized = rawType.trim();
            if (normalized.endsWith("!")) {
                normalized = normalized.substring(0, normalized.length() - 1).trim();
            }
            var direct = singletonDataTypesByName.get(normalized);
            if (direct != null) {
                return Optional.of(direct);
            }
            if (module != null) {
                var imported = importedSingletonDataType(module, normalized);
                if (imported.isPresent()) {
                    return imported;
                }
            }
            return Optional.ofNullable(singletonDataTypesByName.get(simpleTypeName(normalized)));
        }

        private Optional<SingletonDataTypeInfo> importedSingletonDataType(ObjectOrientedModule module, String typeName) {
            var simpleName = simpleTypeName(typeName);
            for (var importDeclaration : module.imports()) {
                if (importDeclaration.excludedSymbols().contains(simpleName)) {
                    continue;
                }
                if (!importDeclaration.isStarImport() && !importDeclaration.symbols().contains(simpleName)) {
                    continue;
                }
                var qualifiedName = importedModuleName(module, importDeclaration.moduleName()) + "." + simpleName;
                var direct = singletonDataTypesByName.get(qualifiedName);
                if (direct != null) {
                    return Optional.of(direct);
                }
                if (importDeclaration.isStarImport()) {
                    var modulePrefix = importedModuleName(module, importDeclaration.moduleName()) + ".";
                    var imported = singletonDataTypesByName.values().stream()
                            .distinct()
                            .filter(type -> type.cfunType().startsWith(modulePrefix))
                            .filter(type -> simpleTypeName(type.name()).equals(simpleName))
                            .findFirst();
                    if (imported.isPresent()) {
                        return imported;
                    }
                }
            }
            return Optional.empty();
        }

        private Optional<PrimitiveBackedTypeInfo> importedPrimitiveBackedType(ObjectOrientedModule module, String typeName) {
            var simpleName = simpleTypeName(typeName);
            for (var importDeclaration : module.imports()) {
                if (importDeclaration.excludedSymbols().contains(simpleName)) {
                    continue;
                }
                if (!importDeclaration.isStarImport() && !importDeclaration.symbols().contains(simpleName)) {
                    continue;
                }
                var qualifiedName = importedPrimitiveBackedTypeName(module, importDeclaration.moduleName(), simpleName);
                var direct = primitiveBackedTypesByName.get(qualifiedName);
                if (direct != null) {
                    return Optional.of(direct);
                }
                if (importDeclaration.isStarImport()) {
                    var modulePrefix = importedModuleName(module, importDeclaration.moduleName()) + ".";
                    var imported = primitiveBackedTypesByName.values().stream()
                            .distinct()
                            .filter(type -> type.cfunType().startsWith(modulePrefix))
                            .filter(type -> simpleTypeName(type.name()).equals(simpleName))
                            .findFirst();
                    if (imported.isPresent()) {
                        return imported;
                    }
                }
            }
            return Optional.empty();
        }

        private String importedPrimitiveBackedTypeName(ObjectOrientedModule module, String moduleName, String typeName) {
            return importedModuleName(module, moduleName) + "." + typeName;
        }

        private String importedModuleName(ObjectOrientedModule module, String moduleName) {
            if (moduleName.startsWith("/")) {
                return moduleName;
            }
            var path = module.path().replace('\\', '/').replaceFirst("/+$", "");
            if (path.isBlank() || ".".equals(path)) {
                return "/" + moduleName;
            }
            return (path.startsWith("/") ? path : "/" + path) + "/" + moduleName;
        }

        record PrimitiveBackedTypeInfo(String name, String cfunType, String backingType, boolean directConstructionAllowed) {
        }

        record SingletonDataTypeInfo(String name, String cfunType) {
        }

        record NativeProviderInfo(
                String providerSymbolName,
                String bootstrapFunctionName,
                String targetBackendType,
                String interfaceId,
                String qualifier,
                String sourceModulePath,
                String sourceModuleName,
                String sourceFile,
                NativeProviderLifetime lifetime,
                NativeProviderBackendBinding binding,
                List<NativeProviderMethodInfo> methods
        ) {
            NativeProviderInfo {
                methods = List.copyOf(methods);
            }
        }

        record NativeProviderMethodInfo(String name, int arity) {
        }

        private record ProviderInterfaceInfo(String backendClassName, List<NativeProviderMethodInfo> methods) {
            private ProviderInterfaceInfo {
                methods = List.copyOf(methods);
            }
        }

        Map<String, NativeProviderInfo> visibleNativeProviders(ObjectOrientedModule module) {
            if (nativeProvidersByModule.isEmpty()) {
                return Map.of();
            }
            var providers = new LinkedHashMap<String, NativeProviderInfo>();
            nativeProvidersByModule.getOrDefault(moduleKey(module.path(), module.name()), List.of())
                    .forEach(provider -> providers.putIfAbsent(provider.providerSymbolName(), provider));
            for (var importDeclaration : module.imports()) {
                var importedProviders = nativeProvidersByModule.getOrDefault(importedModuleKey(module, importDeclaration.moduleName()), List.of());
                if (importedProviders.isEmpty()) {
                    continue;
                }
                for (var provider : importedProviders) {
                    if (importDeclaration.excludedSymbols().contains(provider.providerSymbolName())) {
                        continue;
                    }
                    if (importDeclaration.isStarImport() || importDeclaration.symbols().contains(provider.providerSymbolName())) {
                        providers.putIfAbsent(provider.providerSymbolName(), provider);
                    }
                }
            }
            return Map.copyOf(providers);
        }

        Optional<Path> pathForClassName(String className) {
            return resolveClassName(className).map(pathsByClassName::get);
        }

        Optional<String> resolveClassName(String className) {
            return resolveClassName(className, pathsByClassName.keySet());
        }

        Optional<String> importedMemberOwner(String className, String memberName) {
            return Optional.ofNullable(importedOwnersByClassName.getOrDefault(className, Map.of()).get(memberName));
        }

        Optional<NativeProviderInfo> nativeProviderInfo(String sourceName, String emittedName) {
            var simpleName = simpleMethodName(sourceName);
            return nativeProviderInfos.stream()
                    .filter(provider -> provider.providerSymbolName().equals(sourceName)
                                        || provider.providerSymbolName().equals(simpleName)
                                        || provider.bootstrapFunctionName().equals(sourceName)
                                        || provider.bootstrapFunctionName().equals(simpleName)
                                        || normalizeJsIdentifier(provider.providerSymbolName()).equals(emittedName)
                                        || normalizeJsIdentifier(provider.bootstrapFunctionName()).equals(emittedName))
                    .findFirst();
        }

        Optional<String> classNameForModuleVariable(String moduleVariable) {
            return pathsByClassName.keySet().stream()
                    .filter(className -> moduleVar(className).equals(moduleVariable))
                    .findFirst();
        }

        private static String resolveStaticImportOwner(String className, Map<String, Set<String>> exports) {
            return resolveClassName(className, exports.keySet()).orElse(className);
        }

        private static Optional<String> resolveClassName(String className, Set<String> knownClassNames) {
            var current = className;
            while (true) {
                var resolved = classNameCandidates(current).stream()
                        .filter(knownClassNames::contains)
                        .findFirst();
                if (resolved.isPresent()) {
                    return resolved;
                }
                var idx = current.lastIndexOf('.');
                if (idx < 0) {
                    return Optional.empty();
                }
                current = current.substring(0, idx);
            }
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
            var simple = simpleMethodName(name);
            if (parameterTypes.isEmpty() && isTopLevelConstName(simple)) {
                return jsConstIdentifier(simple);
            }
            var key = signatureKey(name, parameterTypes);
            if (functionNameOverrides.containsKey(key)) {
                return functionNameOverrides.get(key);
            }
            var parameterSignature = parameterTypes.stream().map(String::valueOf).collect(joining(","));
            if (simple.contains("__local_const_")) {
                return normalizeJsIdentifier(simple);
            }
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
            exports.put("capy.lang.Program", Set.of(
                    "Success", "Failed", "__constructor__primitive__failed_exit_code",
                    "next__name_next__failed_exit_code", "previous__name_previous__failed_exit_code",
                    "DEFAULT_FAILED_EXIT_CODE", "LAST_FAILED_EXIT_CODE",
                    "SIGHUP", "SIGINT", "SIGQUIT", "SIGABRT", "SIGFPE", "SIGKILL",
                    "SIGSEGV", "SIGPIPE", "SIGALRM", "SIGTERM", "__capybaraPrimitiveTypes"
            ));
            exports.put("capy.lang.Primitives", Set.of(
                    "to_int", "toInt", "to_long", "toLong", "to_double", "toDouble", "to_float", "toFloat", "to_bool", "toBool",
                    "mAXINTVALUE", "MAX_INT_VALUE", "mININTVALUE", "MIN_INT_VALUE",
                    "mAXLONGVALUE", "MAX_LONG_VALUE", "mINLONGVALUE", "MIN_LONG_VALUE",
                    "fLOATBOUND", "FLOAT_BOUND", "dOUBLEBOUND", "DOUBLE_BOUND",
                    "fLOATBOUNDASFLOAT", "FLOAT_BOUND_AS_FLOAT", "dOUBLEBOUNDASDOUBLE", "DOUBLE_BOUND_AS_DOUBLE",
                    "clamp_long_to_int", "clampLongToInt", "safe_long_to_int", "safeLongToInt"
            ));
            exports.put("capy.lang.String", Set.of(
                    "size", "get", "replace", "is_empty", "plus", "contains", "starts_with", "end_with", "trim",
                    "chars", "__constructor__primitive__char", "char_at", "charAt", "get_char", "getChar",
                    "to_string", "toString", "toString__name_to_string__char",
                    "op3d_op3d__op_op3d_op3d__char__char", "__capybaraPrimitiveTypes"
            ));
            exports.put("capy.lang.RegexModule", Set.of("fromLiteral"));
            exports.put("capy.lang.Seq", Set.of("to_seq", "toSeq"));
            exports.put("capy.lang.System", Set.of("current_millis", "currentMillis", "nano_time", "nanoTime"));
            exports.put("capy.lang.Math", Set.of(
                    "digits", "floor_div", "floorDiv", "floor_mod", "floorMod", "min", "max",
                    "RoundMode", "FLOOR", "CEILING", "HALF_UP", "HALF_DOWN", "HALF_EVEN", "round"
            ));
            exports.put("capy.collection.List", Set.of("size", "get", "is_empty", "plus", "minus", "contains", "any", "all", "map", "filter", "reject", "flat_map", "flatMap", "reduce"));
            exports.put("capy.collection.Set", Set.of("size", "to_list", "is_empty", "plus", "minus", "contains", "any", "all", "map", "filter", "reject", "flat_map", "flatMap", "reduce"));
            exports.put("capy.collection.Dict", Set.of("size", "entries", "get", "is_empty", "plus", "minus", "contains_key", "any", "all", "map", "filter", "reject", "reduce"));
            exports.put("capy.collection.Tuple", Set.of("get"));
            exports.put("capy.io.Console", Set.of("print", "println", "print_error", "printError", "println_error", "printlnError", "read_line", "readLine"));
            exports.put("capy.io.Stdout", Set.of("print", "println"));
            exports.put("capy.io.PathModule", Set.of("Path", "PathRoot", "RELATIVE", "ABSOLUTE", "HOME", "from_string", "fromString"));
            exports.put("capy.io.IO", Set.of("read_text", "readText", "read_lines", "readLines", "read_bytes", "readBytes",
                    "write_text", "writeText", "write_lines", "writeLines", "write_bytes", "writeBytes",
                    "append_text", "appendText", "append_lines", "appendLines", "append_bytes", "appendBytes",
                    "exists", "is_file", "isFile", "is_directory", "isDirectory", "size", "create_file", "createFile",
                    "create_directory", "createDirectory", "create_directories", "createDirectories", "list_entries",
                    "listEntries", "delete", "delete_", "copy", "copy_replace", "copyReplace", "move", "move_replace", "moveReplace"));
            exports.put("capy.date_time.DateModule", Set.of(
                    "Date", "__constructor__data__Date", "uNIXDATE", "UNIX_DATE", "fromIso8601", "from_iso_8601",
                    "fromDaysSinceUnixEpoch", "from_days_since_unix_epoch",
                    "next__name_next__month", "previous__name_previous__month",
                    "jANUARY", "JANUARY", "fEBRUARY", "FEBRUARY", "mARCH", "MARCH", "aPRIL", "APRIL",
                    "mAY", "MAY", "jUNE", "JUNE", "jULY", "JULY", "aUGUST", "AUGUST",
                    "sEPTEMBER", "SEPTEMBER", "oCTOBER", "OCTOBER", "nOVEMBER", "NOVEMBER",
                    "dECEMBER", "DECEMBER"));
            exports.put("capy.date_time.TimeModule", Set.of(
                    "Time", "__constructor__data__Time",
                    "__constructor__primitive__hour", "__constructor__primitive__minute", "__constructor__primitive__second",
                    "__constructor__primitive__offset_minutes",
                    "greater__op_greater__hour__hour", "greater__op_greater__minute__minute", "greater__op_greater__second__second",
                    "greater_op3d__op_greater_op3d__hour__hour", "greater_op3d__op_greater_op3d__minute__minute", "greater_op3d__op_greater_op3d__second__second",
                    "less__op_less__hour__hour", "less__op_less__minute__minute", "less__op_less__second__second",
                    "less_op3d__op_less_op3d__hour__hour", "less_op3d__op_less_op3d__minute__minute", "less_op3d__op_less_op3d__second__second",
                    "op3d_op3d__op_op3d_op3d__hour__hour", "op3d_op3d__op_op3d_op3d__minute__minute", "op3d_op3d__op_op3d_op3d__second__second",
                    "__capybaraPrimitiveTypes", "fromIso8601", "from_iso_8601",
                    "hOURSINDAY", "HOURS_IN_DAY", "mINUTESINHOUR", "MINUTES_IN_HOUR",
                    "sECONDSINMINUTE", "SECONDS_IN_MINUTE", "sECONDSINHOUR", "SECONDS_IN_HOUR",
                    "mINUTESINDAY", "MINUTES_IN_DAY", "sECONDSINDAY", "SECONDS_IN_DAY",
                    "mAXOFFSETMINUTES", "MAX_OFFSET_MINUTES",
                    "zEROHOUR", "ZERO_HOUR", "nOONHOUR", "NOON_HOUR", "zEROMINUTE", "ZERO_MINUTE", "zEROSECOND", "ZERO_SECOND",
                    "mIDNIGHT", "MIDNIGHT", "nOON", "NOON"));
            exports.put("capy.date_time.DurationModule", Set.of(
                    "DateDuration", "WeekDuration", "zERO", "ZERO", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.DateTimeModule", Set.of(
                    "DateTime", "uNIXEPOCH", "UNIX_EPOCH", "fromTimestamp", "from_timestamp",
                    "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.Interval", Set.of(
                    "DateTimeDurationEnd", "DateTimeStartDuration", "DateTimeStartEnd", "fromIso8601", "from_iso_8601"));
            exports.put("capy.date_time.Clock", Set.of("now"));
            exports.put("capy.test.Assert", Set.of("assert_all", "assertAll", "assert_that", "assertThat"));
            exports.put("capy.test.CapyTest", Set.of("test", "test_file", "testFile", "test_file_at", "testFileAt"));
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
                    new GeneratedModule(Path.of("capy", "io", "PathModule.js"), pathRuntime()),
                    new GeneratedModule(Path.of("capy", "io", "IO.js"), ioRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DateModule.js"), dateRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "TimeModule.js"), timeRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DurationModule.js"), durationRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "DateTimeModule.js"), dateTimeRuntime()),
                    new GeneratedModule(Path.of("capy", "date_time", "Interval.js"), intervalRuntime()),
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
                    "capy.io.PathModule",
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
                   + "const mAXINTVALUE = 2147483647;\n"
                   + "const mININTVALUE = -2147483648;\n"
                   + "const mAXLONGVALUE = 9223372036854775807n;\n"
                   + "const mINLONGVALUE = -9223372036854775808n;\n"
                   + "const fLOATBOUND = 16777216n;\n"
                   + "const dOUBLEBOUND = 9007199254740992n;\n"
                   + "const fLOATBOUNDASFLOAT = 16777216.0;\n"
                   + "const dOUBLEBOUNDASDOUBLE = 9007199254740992.0;\n"
                   + "function clampLongToInt(value) {\n"
                   + "    if (value > BigInt(mAXINTVALUE)) {\n"
                   + "        return mAXINTVALUE;\n"
                   + "    }\n"
                   + "    if (value < BigInt(mININTVALUE)) {\n"
                   + "        return mININTVALUE;\n"
                   + "    }\n"
                   + "    return capy.longToInt(value);\n"
                   + "}\n"
                   + "function safeLongToInt(value) {\n"
                   + "    if (value > BigInt(mAXINTVALUE)) {\n"
                   + "        return new capy.Error({ message: `long value \\`${value}\\` is greater than max int value` });\n"
                   + "    }\n"
                   + "    if (value < BigInt(mININTVALUE)) {\n"
                   + "        return new capy.Error({ message: `long value \\`${value}\\` is smaller than min int value` });\n"
                   + "    }\n"
                   + "    return new capy.Success({ value: capy.longToInt(value) });\n"
                   + "}\n"
                   + "module.exports = {\n"
                   + "    mAXINTVALUE,\n"
                   + "    MAX_INT_VALUE: mAXINTVALUE,\n"
                   + "    mININTVALUE,\n"
                   + "    MIN_INT_VALUE: mININTVALUE,\n"
                   + "    mAXLONGVALUE,\n"
                   + "    MAX_LONG_VALUE: mAXLONGVALUE,\n"
                   + "    mINLONGVALUE,\n"
                   + "    MIN_LONG_VALUE: mINLONGVALUE,\n"
                   + "    fLOATBOUND,\n"
                   + "    FLOAT_BOUND: fLOATBOUND,\n"
                   + "    dOUBLEBOUND,\n"
                   + "    DOUBLE_BOUND: dOUBLEBOUND,\n"
                   + "    fLOATBOUNDASFLOAT,\n"
                   + "    FLOAT_BOUND_AS_FLOAT: fLOATBOUNDASFLOAT,\n"
                   + "    dOUBLEBOUNDASDOUBLE,\n"
                   + "    DOUBLE_BOUND_AS_DOUBLE: dOUBLEBOUNDASDOUBLE,\n"
                   + "    to_int: capy.parseIntResult,\n"
                   + "    toInt: capy.parseIntResult,\n"
                   + "    to_long: capy.parseLongResult,\n"
                   + "    toLong: capy.parseLongResult,\n"
                       + "    to_double: value => capy.parseFloatResult(value, 'double'),\n"
                       + "    toDouble: value => capy.parseFloatResult(value, 'double'),\n"
                   + "    to_float: value => capy.parseFloatResult(value, 'float'),\n"
                   + "    toFloat: value => capy.parseFloatResult(value, 'float'),\n"
                   + "    to_bool: capy.parseBoolResult,\n"
                   + "    toBool: capy.parseBoolResult,\n"
                   + "    clamp_long_to_int: clampLongToInt,\n"
                   + "    clampLongToInt,\n"
                   + "    safe_long_to_int: safeLongToInt,\n"
                   + "    safeLongToInt,\n"
                   + "};\n";
        }

        private static String programRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    const MIN_FAILED_EXIT_CODE = 1;
                    const MAX_FAILED_EXIT_CODE = 255;
                    const DEFAULT_FAILED_EXIT_CODE = MIN_FAILED_EXIT_CODE;
                    const LAST_FAILED_EXIT_CODE = MAX_FAILED_EXIT_CODE;
                    const SIGHUP = 129;
                    const SIGINT = 130;
                    const SIGQUIT = 131;
                    const SIGABRT = 134;
                    const SIGFPE = 136;
                    const SIGKILL = 137;
                    const SIGSEGV = 139;
                    const SIGPIPE = 141;
                    const SIGALRM = 142;
                    const SIGTERM = 143;
                    const __capybaraPrimitiveTypes = Object.freeze({
                        failed_exit_code: Object.freeze({ cfunType: '/capy/lang/Program.failed_exit_code', backingType: 'int' })
                    });

                    const Success = Object.freeze({
                        __capybaraType: 'Success',
                        __capybaraTypes: ['Success', 'Program'],
                        toString() { return capy.dataToString(this); },
                        capybaraDataValueInfo() { return capy.dataValueInfo(this, 'Success', 'capy.lang', 'capy/lang/Program'); }
                    });

                    class Failed {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Failed';
                            this.__capybaraTypes = ['Failed', 'Program'];
                            this.exit_code = fields.exit_code ?? 1;
                        }
                        toString() { return capy.dataToString(this); }
                        capybaraDataValueInfo() { return capy.dataValueInfo(this, 'Failed', 'capy.lang', 'capy/lang/Program'); }
                    }

                    function __constructor__primitive__failed_exit_code(value) {
                        if (value < MIN_FAILED_EXIT_CODE) {
                            return new capy.Error({ message: `failed_exit_code must be greater or equals to ${MIN_FAILED_EXIT_CODE}, was \\`${value}\\`.` });
                        }
                        if (value > MAX_FAILED_EXIT_CODE) {
                            return new capy.Error({ message: `failed_exit_code must be less than or equal to ${MAX_FAILED_EXIT_CODE}, was \\`${value}\\`.` });
                        }
                        return new capy.Success({ value });
                    }

                    function next__name_next__failed_exit_code(this_) {
                        return this_ >= MAX_FAILED_EXIT_CODE ? LAST_FAILED_EXIT_CODE : this_ + 1;
                    }

                    function previous__name_previous__failed_exit_code(this_) {
                        return this_ <= MIN_FAILED_EXIT_CODE ? DEFAULT_FAILED_EXIT_CODE : this_ - 1;
                    }

                    module.exports = {
                        Success,
                        Failed,
                        __constructor__primitive__failed_exit_code,
                        next__name_next__failed_exit_code,
                        previous__name_previous__failed_exit_code,
                        DEFAULT_FAILED_EXIT_CODE,
                        LAST_FAILED_EXIT_CODE,
                        SIGHUP,
                        SIGINT,
                        SIGQUIT,
                        SIGABRT,
                        SIGFPE,
                        SIGKILL,
                        SIGSEGV,
                        SIGPIPE,
                        SIGALRM,
                        SIGTERM,
                        __capybaraPrimitiveTypes,
                    };
                    """;
        }

        private static String stringRuntime() {
            return "'use strict';\n"
                   + "const capy = require('../../dev/capylang/capybara.js');\n"
                   + "const Seq = require('./Seq.js');\n"
                   + "function __constructor__primitive__char(value) {\n"
                   + "    const text = String(value);\n"
                   + "    return text.length === 1 ? new capy.Success({ value: text }) : new capy.Error({ message: 'char must contain exactly one character' });\n"
                   + "}\n"
                   + "const charAt = (value, idx) => capy.getIndex(value, idx);\n"
                   + "const chars = value => {\n"
                   + "    const text = String(value);\n"
                   + "    const next = idx => idx >= text.length ? Seq.End : new Seq.Cons({ value: text.charAt(idx), rest: () => next(idx + 1) });\n"
                   + "    return next(0);\n"
                   + "};\n"
                   + "const toStringChar = value => String(value);\n"
                   + "const equalsChar = (left, right) => String(left) === String(right);\n"
                   + "const __capybaraPrimitiveTypes = Object.freeze({ char: Object.freeze({ cfunType: '/capy/lang/String.char', backingType: 'String' }) });\n"
                   + "module.exports = {\n"
                   + "    size: value => String(value).length,\n"
                   + "    get: (value, start, end) => end === undefined ? capy.getIndex(value, start) : capy.slice(value, start, end),\n"
                   + "    replace: (value, oldValue, newValue) => String(value).split(oldValue).join(newValue),\n"
                   + "    is_empty: value => String(value).length === 0,\n"
                   + "    plus: (left, right) => capy.toStringValue(left) + capy.toStringValue(right),\n"
                   + "    contains: (value, part) => String(value).includes(part),\n"
                   + "    starts_with: (value, part) => String(value).startsWith(part),\n"
                   + "    end_with: (value, part) => String(value).endsWith(part),\n"
                   + "    trim: value => String(value).trim(),\n"
                   + "    chars,\n"
                   + "    __constructor__primitive__char,\n"
                   + "    char_at: charAt,\n"
                   + "    charAt,\n"
                   + "    get_char: charAt,\n"
                   + "    getChar: charAt,\n"
                   + "    to_string: toStringChar,\n"
                   + "    toString: toStringChar,\n"
                   + "    toString__name_to_string__char: toStringChar,\n"
                   + "    op3d_op3d__op_op3d_op3d__char__char: equalsChar,\n"
                   + "    __capybaraPrimitiveTypes,\n"
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
                        question(input) {
                            return this.matches(input);
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
                    const RoundMode = (() => {
                        const values = [
                            capy.enumValue('FLOOR', 'RoundMode', ['RoundMode'], 0, [], 'capy.lang', 'capy/lang/Math'),
                            capy.enumValue('CEILING', 'RoundMode', ['RoundMode'], 1, [], 'capy.lang', 'capy/lang/Math'),
                            capy.enumValue('HALF_UP', 'RoundMode', ['RoundMode'], 2, [], 'capy.lang', 'capy/lang/Math'),
                            capy.enumValue('HALF_DOWN', 'RoundMode', ['RoundMode'], 3, [], 'capy.lang', 'capy/lang/Math'),
                            capy.enumValue('HALF_EVEN', 'RoundMode', ['RoundMode'], 4, [], 'capy.lang', 'capy/lang/Math'),
                        ];
                        return Object.freeze({
                            FLOOR: values[0],
                            CEILING: values[1],
                            HALF_UP: values[2],
                            HALF_DOWN: values[3],
                            HALF_EVEN: values[4],
                            values,
                            valuesSet: () => capy.set(values),
                            parse: value => capy.parseEnum(value, values, 'RoundMode'),
                        });
                    })();
                    const FLOOR = RoundMode.FLOOR;
                    const CEILING = RoundMode.CEILING;
                    const HALF_UP = RoundMode.HALF_UP;
                    const HALF_DOWN = RoundMode.HALF_DOWN;
                    const HALF_EVEN = RoundMode.HALF_EVEN;
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
                    const roundFloor = value => capy.floatToLong(Math.floor(Number(value)));
                    const roundCeiling = value => capy.floatToLong(Math.ceil(Number(value)));
                    function roundNearest(value, tieBreaker) {
                        const lower = roundFloor(value);
                        const upper = roundCeiling(value);
                        const lowerDiff = Number(value) - Number(lower);
                        const upperDiff = Number(upper) - Number(value);
                        if (lowerDiff < upperDiff) {
                            return lower;
                        }
                        if (lowerDiff > upperDiff) {
                            return upper;
                        }
                        return tieBreaker(lower, upper);
                    }
                    const halfUp = value => roundNearest(value, (lower, upper) => Number(value) < 0.0 ? lower : upper);
                    const halfDown = value => roundNearest(value, (lower, upper) => Number(value) < 0.0 ? upper : lower);
                    const halfEven = value => roundNearest(value, (lower, upper) => lower % 2n === 0n ? lower : upper);
                    function round(value, mode) {
                        if (capy.isType(mode, 'FLOOR')) {
                            return roundFloor(value);
                        }
                        if (capy.isType(mode, 'CEILING')) {
                            return roundCeiling(value);
                        }
                        if (capy.isType(mode, 'HALF_UP')) {
                            return halfUp(value);
                        }
                        if (capy.isType(mode, 'HALF_DOWN')) {
                            return halfDown(value);
                        }
                        if (capy.isType(mode, 'HALF_EVEN')) {
                            return halfEven(value);
                        }
                        throw new Error('Unexpected RoundMode: ' + capy.toStringValue(mode));
                    }
                    module.exports = {
                        RoundMode,
                        FLOOR,
                        CEILING,
                        HALF_UP,
                        HALF_DOWN,
                        HALF_EVEN,
                        digits,
                        floorDiv,
                        floor_div: floorDiv,
                        floorMod,
                        floor_mod: floorMod,
                        min,
                        max,
                        round,
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

        private static String pathRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    const PathRoot = (() => {
                        const values = [
                            capy.enumValue('RELATIVE', 'PathRoot', ['PathRoot'], 0, [], 'capy.io', 'capy/io/Path', { fields: [], annotations: [] }),
                            capy.enumValue('ABSOLUTE', 'PathRoot', ['PathRoot'], 1, [], 'capy.io', 'capy/io/Path', { fields: [], annotations: [] }),
                            capy.enumValue('HOME', 'PathRoot', ['PathRoot'], 2, [], 'capy.io', 'capy/io/Path', { fields: [], annotations: [] }),
                        ];
                        return Object.freeze({
                            RELATIVE: values[0],
                            ABSOLUTE: values[1],
                            HOME: values[2],
                            values,
                            valuesSet: () => capy.set(values),
                            parse: value => capy.parseEnum(value, values, 'PathRoot'),
                        });
                    })();
                    const RELATIVE = PathRoot.RELATIVE;
                    const ABSOLUTE = PathRoot.ABSOLUTE;
                    const HOME = PathRoot.HOME;

                    class Path {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Path';
                            this.__capybaraTypes = ['Path', 'CapybaraDataValue'];
                            this.root = fields.root;
                            this.prefix = fields.prefix ?? capy.None;
                            this.segments = fields.segments ?? [];
                            return capy.methodAliasProxy(this);
                        }
                        with(fields = {}) {
                            return new Path({
                                root: Object.prototype.hasOwnProperty.call(fields, 'root') ? fields.root : this.root,
                                prefix: Object.prototype.hasOwnProperty.call(fields, 'prefix') ? fields.prefix : this.prefix,
                                segments: Object.prototype.hasOwnProperty.call(fields, 'segments') ? fields.segments : this.segments,
                            });
                        }
                        slash(other) {
                            return other instanceof Path
                                ? new Path({ root: this.root, prefix: this.prefix, segments: this.segments.concat(other.segments) })
                                : new Path({ root: this.root, prefix: this.prefix, segments: this.segments.concat(String(other)) });
                        }
                        normalize() {
                            const normalized = [];
                            for (const segment of this.segments) {
                                if (segment === '' || segment === '.') {
                                    continue;
                                }
                                if (segment === '..') {
                                    if (normalized.length > 0) {
                                        normalized.pop();
                                    } else if (capy.isType(this.root, 'RELATIVE')) {
                                        normalized.push(segment);
                                    }
                                } else {
                                    normalized.push(segment);
                                }
                            }
                            return new Path({ root: this.root, prefix: this.prefix, segments: normalized });
                        }
                        parent() {
                            return this.segments.length === 0
                                ? capy.None
                                : new capy.Some({ value: new Path({ root: this.root, prefix: this.prefix, segments: this.segments.slice(0, -1) }) });
                        }
                        isAbsolute() {
                            return !capy.isType(this.root, 'RELATIVE');
                        }
                        isRoot() {
                            return this.segments.length === 0;
                        }
                        name() {
                            if (capy.isType(this.prefix, 'Some')) {
                                return this.prefix.value;
                            }
                            if (capy.isType(this.root, 'ABSOLUTE')) {
                                return '/';
                            }
                            if (capy.isType(this.root, 'HOME')) {
                                return '~';
                            }
                            return '.';
                        }
                        toString() {
                            const body = this.segments.join('/');
                            if (capy.isType(this.root, 'ABSOLUTE')) {
                                return '/' + body;
                            }
                            if (capy.isType(this.root, 'HOME')) {
                                return body.length === 0 ? '~' : '~/' + body;
                            }
                            return body.length === 0 ? '.' : body;
                        }
                        toString_() {
                            return this.toString();
                        }
                        capybaraDataValueInfo() {
                            return capy.dataValueInfo(this, 'Path', 'capy.io', 'capy/io/Path', [], []);
                        }
                    }

                    function fromString(pathString) {
                        const input = String(pathString);
                        const root = input.startsWith('/') ? ABSOLUTE : input.startsWith('~') ? HOME : RELATIVE;
                        const value = input.startsWith('/') ? input.slice(1) : input.startsWith('~/') ? input.slice(2) : input.startsWith('~') ? input.slice(1) : input;
                        const segments = value.split('/').filter(segment => segment.length > 0);
                        return new Path({ root, prefix: capy.None, segments }).normalize();
                    }

                    module.exports = {
                        Path,
                        PathRoot,
                        RELATIVE,
                        ABSOLUTE,
                        HOME,
                        fromString,
                        from_string: fromString,
                    };
                    """;
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
                    const capy = require('../../dev/capylang/capybara.js');

                    const jANUARY = 1;
                    const fEBRUARY = 2;
                    const mARCH = 3;
                    const aPRIL = 4;
                    const mAY = 5;
                    const jUNE = 6;
                    const jULY = 7;
                    const aUGUST = 8;
                    const sEPTEMBER = 9;
                    const oCTOBER = 10;
                    const nOVEMBER = 11;
                    const dECEMBER = 12;
                    const __DAYS_PER_400_YEARS = 146097;
                    const __DAYS_PER_YEAR = 365;
                    const __UNIX_EPOCH_CIVIL_OFFSET_DAYS = 719468;
                    const __DAYS_PER_4_YEARS_MINUS_1 = 1460;
                    const __DAYS_PER_100_YEARS_MINUS_1 = 36524;
                    const __DAYS_PER_400_YEARS_MINUS_1 = 146096;
                    const __MONTH_TO_DAY_NUMERATOR = 153;
                    const __MONTH_TO_DAY_BIAS = 2;

                    const success = value => new capy.Success({ value });
                    const failure = message => new capy.Error({ message });
                    const floorDiv = (left, right) => Math.floor(left / right);

                    function _leapYear(year) {
                        return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
                    }

                    function _days31Month(month) {
                        return month === jANUARY || month === mARCH || month === mAY || month === jULY
                            || month === aUGUST || month === oCTOBER || month === dECEMBER;
                    }

                    function _days30Month(month) {
                        return month === aPRIL || month === jUNE || month === sEPTEMBER || month === nOVEMBER;
                    }

                    function daysInMonth(year, month) {
                        if (month === fEBRUARY) {
                            return _leapYear(year) ? 29 : 28;
                        }
                        return _days31Month(month) ? 31 : 30;
                    }

                    class DateValue {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Date';
                            this.__capybaraTypes = ['Date', 'CapybaraDataValue'];
                            this.day = fields.day;
                            this.month = fields.month;
                            this.year = fields.year;
                            return capy.methodAliasProxy(this);
                        }
                        with(fields = {}) {
                            return new DateValue({
                                day: Object.prototype.hasOwnProperty.call(fields, 'day') ? fields.day : this.day,
                                month: Object.prototype.hasOwnProperty.call(fields, 'month') ? fields.month : this.month,
                                year: Object.prototype.hasOwnProperty.call(fields, 'year') ? fields.year : this.year,
                            });
                        }
                        with_(day, month, year) {
                            return arguments.length === 1 && day && typeof day === 'object'
                                ? this.with(day)
                                : new DateValue({ day, month, year });
                        }
                        toString() {
                            return capy.dataToString(this);
                        }
                        capybaraDataValueInfo() {
                            return capy.dataValueInfo(this, 'Date', 'capy.dateTime', 'capy/date_time/Date');
                        }
                        toDaysSinceUnixEpoch() {
                            const y = this.month <= 2 ? this.year - 1 : this.year;
                            const era = floorDiv(y, 400);
                            const yoe = y - era * 400;
                            const mp = this.month > fEBRUARY ? this.month - 3 : this.month + 9;
                            const doy = floorDiv(__MONTH_TO_DAY_NUMERATOR * mp + __MONTH_TO_DAY_BIAS, 5) + this.day - 1;
                            const doe = yoe * __DAYS_PER_YEAR + floorDiv(yoe, 4) - floorDiv(yoe, 100) + doy;
                            return era * __DAYS_PER_400_YEARS + doe - __UNIX_EPOCH_CIVIL_OFFSET_DAYS;
                        }
                        to_days_since_unix_epoch() {
                            return this.toDaysSinceUnixEpoch();
                        }
                        leapYear() {
                            return _leapYear(this.year);
                        }
                        leap_year() {
                            return this.leapYear();
                        }
                        firstDayOfMonth() {
                            return new DateValue({ day: 1, month: this.month, year: this.year });
                        }
                        first_day_of_month() {
                            return this.firstDayOfMonth();
                        }
                        lastDayOfMonth() {
                            return new DateValue({ day: daysInMonth(this.year, this.month), month: this.month, year: this.year });
                        }
                        last_day_of_month() {
                            return this.lastDayOfMonth();
                        }
                        addDays(days) {
                            return fromDaysSinceUnixEpoch(this.toDaysSinceUnixEpoch() + days);
                        }
                        add_days(days) {
                            return this.addDays(days);
                        }
                        addYearsMonths(years, months) {
                            const totalMonths = (this.year + years) * 12 + (this.month + months - 1);
                            const normalizedYear = floorDiv(totalMonths, 12);
                            const normalizedMonth = totalMonths - normalizedYear * 12 + 1;
                            return new DateValue({
                                day: Math.min(this.day, daysInMonth(normalizedYear, normalizedMonth)),
                                month: normalizedMonth,
                                year: normalizedYear,
                            });
                        }
                        add_years_months(years, months) {
                            return this.addYearsMonths(years, months);
                        }
                        toIso8601() {
                            const year = this.year < 0
                                ? `-${String(-this.year).padStart(4, '0')}`
                                : this.year > 9999
                                    ? `+${this.year}`
                                    : String(this.year).padStart(4, '0');
                            return `${year}-${String(this.month).padStart(2, '0')}-${String(this.day).padStart(2, '0')}`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    function fromDaysSinceUnixEpoch(days) {
                        const z = days + __UNIX_EPOCH_CIVIL_OFFSET_DAYS;
                        const era = floorDiv(z, __DAYS_PER_400_YEARS);
                        const doe = z - era * __DAYS_PER_400_YEARS;
                        const yoe = floorDiv(doe - floorDiv(doe, __DAYS_PER_4_YEARS_MINUS_1) + floorDiv(doe, __DAYS_PER_100_YEARS_MINUS_1) - floorDiv(doe, __DAYS_PER_400_YEARS_MINUS_1), __DAYS_PER_YEAR);
                        const y = yoe + era * 400;
                        const doy = doe - (__DAYS_PER_YEAR * yoe + floorDiv(yoe, 4) - floorDiv(yoe, 100));
                        const mp = floorDiv(5 * doy + __MONTH_TO_DAY_BIAS, __MONTH_TO_DAY_NUMERATOR);
                        const day = doy - floorDiv(__MONTH_TO_DAY_NUMERATOR * mp + __MONTH_TO_DAY_BIAS, 5) + 1;
                        const month = mp < 10 ? mp + 3 : mp - 9;
                        const year = month <= fEBRUARY ? y + 1 : y;
                        return new DateValue({ day, month, year });
                    }

                    function __constructor__data__Date(day, month, year) {
                        if (month < jANUARY || month > dECEMBER || day < 1 || day > daysInMonth(year, month)) {
                            return failure(`Invalid date \\`${day}/${month}/${year}\\`!`);
                        }
                        return success(new DateValue({ day, month, year }));
                    }

                    function __constructor__primitive__month(value) {
                        return value >= 1 && value <= 12
                            ? success(value)
                            : failure('month must be between 1 and 12');
                    }

                    function greater__month__month(this_, other) {
                        return this_ > other;
                    }

                    function greater_op3d__month__month(this_, other) {
                        return this_ >= other;
                    }

                    function less__month__month(this_, other) {
                        return this_ < other;
                    }

                    function less_op3d__month__month(this_, other) {
                        return this_ <= other;
                    }

                    function op3d_op3d__month__month(this_, other) {
                        return this_ === other;
                    }

                    function next__name_next__month(this_) {
                        return this_ === dECEMBER ? jANUARY : this_ + 1;
                    }

                    function previous__name_previous__month(this_) {
                        return this_ === jANUARY ? dECEMBER : this_ - 1;
                    }

                    function greater(...args) {
                        return greater__month__month(...args);
                    }

                    function greater_op3d(...args) {
                        return greater_op3d__month__month(...args);
                    }

                    function less(...args) {
                        return less__month__month(...args);
                    }

                    function less_op3d(...args) {
                        return less_op3d__month__month(...args);
                    }

                    function op3d_op3d(...args) {
                        return op3d_op3d__month__month(...args);
                    }

                    function parseInteger(text, label) {
                        if (!/^[0-9]+$/.test(text)) {
                            return failure(`Invalid ISO 8601 date format: expected digits for ${label}, got \\`${text}\\``);
                        }
                        return success(Number.parseInt(text, 10));
                    }

                    function parseYear(text) {
                        if (text.length === 0) {
                            return failure('Invalid ISO 8601 date format: expected year');
                        }
                        if (text.startsWith('+') || text.startsWith('-')) {
                            if (text.length < 5) {
                                return failure(`Invalid ISO 8601 date format: expected expanded year, got \\`${text}\\``);
                            }
                            const parsed = parseInteger(text.slice(1), 'year');
                            return capy.isType(parsed, 'Success')
                                ? success(text.startsWith('-') ? -parsed.value : parsed.value)
                                : parsed;
                        }
                        if (text.length < 4) {
                            return failure(`Invalid ISO 8601 date format: expected four-digit year, got \\`${text}\\``);
                        }
                        return parseInteger(text, 'year');
                    }

                    function parseDate(yearPart, monthPart, dayPart) {
                        const year = parseYear(yearPart);
                        if (!capy.isType(year, 'Success')) return year;
                        const monthValue = parseInteger(monthPart, 'month');
                        if (!capy.isType(monthValue, 'Success')) return monthValue;
                        const month = __constructor__primitive__month(monthValue.value);
                        if (!capy.isType(month, 'Success')) return month;
                        const day = parseInteger(dayPart, 'day');
                        if (!capy.isType(day, 'Success')) return day;
                        const constructed = __constructor__data__Date(day.value, month.value, year.value);
                        return capy.isType(constructed, 'Success')
                            ? constructed
                            : failure(`Invalid ISO 8601 date format: ${constructed.message}`);
                    }

                    function fromIso8601(iso) {
                        const value = String(iso);
                        const extendedYearEnd = value.length - 6;
                        const isExtended = value.length >= 10
                            && value.slice(extendedYearEnd, extendedYearEnd + 1) === '-'
                            && value.slice(value.length - 3, value.length - 2) === '-';
                        if (isExtended) {
                            return parseDate(value.slice(0, extendedYearEnd), value.slice(value.length - 5, value.length - 3), value.slice(value.length - 2));
                        }
                        if (value.length >= 8) {
                            return parseDate(value.slice(0, value.length - 4), value.slice(value.length - 4, value.length - 2), value.slice(value.length - 2));
                        }
                        return failure(`Invalid ISO 8601 date format: expected date in \\`YYYY-MM-DD\\` or \\`YYYYMMDD\\` format, got \\`${value}\\``);
                    }

                    const uNIXDATE = new DateValue({ day: 1, month: 1, year: 1970 });
                    const UNIX_DATE = uNIXDATE;
                    const __capybaraPrimitiveTypes = Object.freeze({
                        month: Object.freeze({ cfunType: '/capy/date_time/Date.month', backingType: 'int' }),
                    });

                    module.exports = {
                        Date: DateValue,
                        __constructor__data__Date,
                        __constructor__primitive__month,
                        _days30Month,
                        _days31Month,
                        _leapYear,
                        greater__month__month,
                        greater_op3d__month__month,
                        less__month__month,
                        less_op3d__month__month,
                        op3d_op3d__month__month,
                        next__name_next__month,
                        previous__name_previous__month,
                        greater,
                        greater_op3d,
                        less,
                        less_op3d,
                        op3d_op3d,
                        jANUARY,
                        JANUARY: jANUARY,
                        fEBRUARY,
                        FEBRUARY: fEBRUARY,
                        mARCH,
                        MARCH: mARCH,
                        aPRIL,
                        APRIL: aPRIL,
                        mAY,
                        MAY: mAY,
                        jUNE,
                        JUNE: jUNE,
                        jULY,
                        JULY: jULY,
                        aUGUST,
                        AUGUST: aUGUST,
                        sEPTEMBER,
                        SEPTEMBER: sEPTEMBER,
                        oCTOBER,
                        OCTOBER: oCTOBER,
                        nOVEMBER,
                        NOVEMBER: nOVEMBER,
                        dECEMBER,
                        DECEMBER: dECEMBER,
                        __DAYS_PER_400_YEARS,
                        __DAYS_PER_YEAR,
                        __UNIX_EPOCH_CIVIL_OFFSET_DAYS,
                        __DAYS_PER_4_YEARS_MINUS_1,
                        __DAYS_PER_100_YEARS_MINUS_1,
                        __DAYS_PER_400_YEARS_MINUS_1,
                        __MONTH_TO_DAY_NUMERATOR,
                        __MONTH_TO_DAY_BIAS,
                        uNIXDATE,
                        UNIX_DATE,
                        __capybaraPrimitiveTypes,
                        fromDaysSinceUnixEpoch,
                        from_days_since_unix_epoch: fromDaysSinceUnixEpoch,
                        fromIso8601,
                        from_iso_8601: fromIso8601,
                    };
                    """;
        }

        private static String timeRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    const hOURSINDAY = 24;
                    const mINUTESINHOUR = 60;
                    const sECONDSINMINUTE = 60;
                    const sECONDSINHOUR = mINUTESINHOUR * sECONDSINMINUTE;
                    const mINUTESINDAY = hOURSINDAY * mINUTESINHOUR;
                    const sECONDSINDAY = hOURSINDAY * sECONDSINHOUR;
                    const mAXOFFSETMINUTES = 23 * mINUTESINHOUR + 59;
                    const success = value => new capy.Success({ value });
                    const failure = message => new capy.Error({ message });
                    const floorMod = (left, right) => left - Math.floor(left / right) * right;

                    class Time {
                        constructor(fields = {}) {
                            this.__capybaraType = 'Time';
                            this.__capybaraTypes = ['Time', 'CapybaraDataValue'];
                            this.hour = fields.hour;
                            this.minute = fields.minute;
                            this.second = fields.second;
                            this.offset_minutes = fields.offset_minutes ?? capy.None;
                            return capy.methodAliasProxy(this);
                        }
                        with(fields = {}) {
                            return new Time({
                                hour: Object.prototype.hasOwnProperty.call(fields, 'hour') ? fields.hour : this.hour,
                                minute: Object.prototype.hasOwnProperty.call(fields, 'minute') ? fields.minute : this.minute,
                                second: Object.prototype.hasOwnProperty.call(fields, 'second') ? fields.second : this.second,
                                offset_minutes: Object.prototype.hasOwnProperty.call(fields, 'offset_minutes') ? fields.offset_minutes : this.offset_minutes,
                            });
                        }
                        with_(hour, minute, second, offset_minutes) {
                            return arguments.length === 1 && hour && typeof hour === 'object'
                                ? this.with(hour)
                                : new Time({ hour, minute, second, offset_minutes });
                        }
                        toString() {
                            return capy.dataToString(this);
                        }
                        capybaraDataValueInfo() {
                            return capy.dataValueInfo(this, 'Time', 'capy.dateTime', 'capy/date_time/Time');
                        }
                        toSeconds() {
                            return this.hour * sECONDSINHOUR + this.minute * sECONDSINMINUTE + this.second;
                        }
                        to_seconds() {
                            return this.toSeconds();
                        }
                        toMinutes() {
                            return this.hour * mINUTESINHOUR + this.minute;
                        }
                        to_minutes() {
                            return this.toMinutes();
                        }
                        toHours() {
                            return this.hour;
                        }
                        to_hours() {
                            return this.toHours();
                        }
                        roundToMinutes() {
                            return new Time({ hour: this.hour, minute: this.minute, second: 0, offset_minutes: this.offset_minutes });
                        }
                        round_to_minutes() {
                            return this.roundToMinutes();
                        }
                        roundToHours() {
                            return new Time({ hour: this.hour, minute: 0, second: 0, offset_minutes: this.offset_minutes });
                        }
                        round_to_hours() {
                            return this.roundToHours();
                        }
                        addSeconds(seconds) {
                            const total = floorMod(this.toSeconds() + seconds, sECONDSINDAY);
                            return new Time({
                                hour: Math.trunc(total / sECONDSINHOUR),
                                minute: Math.trunc((total % sECONDSINHOUR) / sECONDSINMINUTE),
                                second: total % sECONDSINMINUTE,
                                offset_minutes: this.offset_minutes,
                            });
                        }
                        add_seconds(seconds) {
                            return this.addSeconds(seconds);
                        }
                        addMinutes(minutes) {
                            return this.addSeconds(minutes * sECONDSINMINUTE);
                        }
                        add_minutes(minutes) {
                            return this.addMinutes(minutes);
                        }
                        addHours(hours) {
                            return this.addSeconds(hours * sECONDSINHOUR);
                        }
                        add_hours(hours) {
                            return this.addHours(hours);
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

                    function __constructor__data__Time(hour, minute, second, offset_minutes) {
                        if (hour < 0 || hour > hOURSINDAY - 1 || minute < 0 || minute > mINUTESINHOUR - 1 || second < 0 || second > sECONDSINMINUTE - 1) {
                            return failure(`Invalid time \\`${hour}:${minute}:${second}\\`!`);
                        }
                        if (offset_minutes && offset_minutes.__capybaraType === 'Some') {
                            const value = offset_minutes.value;
                            if (value < -mAXOFFSETMINUTES || value > mAXOFFSETMINUTES) {
                                return failure(`Invalid UTC offset \\`${value}\\` minutes!`);
                            }
                        }
                        return success(new Time({ hour, minute, second, offset_minutes }));
                    }

                    function __constructor__primitive__hour(value) {
                        return value >= 0 && value <= hOURSINDAY - 1
                            ? success(value)
                            : failure('hour must be between 0 and 23');
                    }

                    function __constructor__primitive__minute(value) {
                        return value >= 0 && value <= mINUTESINHOUR - 1
                            ? success(value)
                            : failure('minute must be between 0 and 59');
                    }

                    function __constructor__primitive__second(value) {
                        return value >= 0 && value <= sECONDSINMINUTE - 1
                            ? success(value)
                            : failure('second must be between 0 and 59');
                    }

                    function __constructor__primitive__offset_minutes(value) {
                        return value >= -mAXOFFSETMINUTES && value <= mAXOFFSETMINUTES
                            ? success(value)
                            : failure('offset minutes must be between -1439 and 1439');
                    }

                    const greater__op_greater__hour__hour = (this_, other) => this_ > other;
                    const greater__op_greater__minute__minute = (this_, other) => this_ > other;
                    const greater__op_greater__second__second = (this_, other) => this_ > other;
                    const greater_op3d__op_greater_op3d__hour__hour = (this_, other) => this_ >= other;
                    const greater_op3d__op_greater_op3d__minute__minute = (this_, other) => this_ >= other;
                    const greater_op3d__op_greater_op3d__second__second = (this_, other) => this_ >= other;
                    const less__op_less__hour__hour = (this_, other) => this_ < other;
                    const less__op_less__minute__minute = (this_, other) => this_ < other;
                    const less__op_less__second__second = (this_, other) => this_ < other;
                    const less_op3d__op_less_op3d__hour__hour = (this_, other) => this_ <= other;
                    const less_op3d__op_less_op3d__minute__minute = (this_, other) => this_ <= other;
                    const less_op3d__op_less_op3d__second__second = (this_, other) => this_ <= other;
                    const op3d_op3d__op_op3d_op3d__hour__hour = (this_, other) => capy.equals(this_, other);
                    const op3d_op3d__op_op3d_op3d__minute__minute = (this_, other) => capy.equals(this_, other);
                    const op3d_op3d__op_op3d_op3d__second__second = (this_, other) => capy.equals(this_, other);

                    function parseInteger(text, label) {
                        if (!/^[0-9]+$/.test(text)) {
                            return failure(`Invalid ISO 8601 time format: expected digits for ${label}, got \\`${text}\\``);
                        }
                        return success(Number.parseInt(text, 10));
                    }

                    function parseOffset(sign, hourPart, minutePart) {
                        const hours = parseInteger(hourPart, 'offset hour');
                        if (!capy.isType(hours, 'Success')) return hours;
                        const minutes = parseInteger(minutePart, 'offset minute');
                        if (!capy.isType(minutes, 'Success')) return minutes;
                        if (hours.value > 23) {
                            return failure('Invalid ISO 8601 time format: offset hour must be between 00 and 23');
                        }
                        if (minutes.value > 59) {
                            return failure('Invalid ISO 8601 time format: offset minute must be between 00 and 59');
                        }
                        if (sign === '-' && hours.value === 0 && minutes.value === 0) {
                            return failure('Invalid ISO 8601 time format: negative zero offset is not allowed');
                        }
                        const direction = sign === '-' ? -1 : 1;
                        return success(direction * (hours.value * mINUTESINHOUR + minutes.value));
                    }

                    function parseZone(value) {
                        if (value.length === 0) {
                            return failure('Invalid ISO 8601 time format: time must not be empty');
                        }
                        if (value.endsWith('Z')) {
                            return success({ buffer: value.slice(0, -1), offset_minutes: new capy.Some({ value: 0 }) });
                        }
                        const extended = value.match(/([+-])(\\d{2}):(\\d{2})$/);
                        if (extended) {
                            const offset = parseOffset(extended[1], extended[2], extended[3]);
                            return capy.isType(offset, 'Success')
                                ? success({ buffer: value.slice(0, -6), offset_minutes: new capy.Some({ value: offset.value }) })
                                : offset;
                        }
                        const basic = value.match(/([+-])(\\d{2})(\\d{2})$/);
                        if (basic) {
                            const offset = parseOffset(basic[1], basic[2], basic[3]);
                            return capy.isType(offset, 'Success')
                                ? success({ buffer: value.slice(0, -5), offset_minutes: new capy.Some({ value: offset.value }) })
                                : offset;
                        }
                        const hourOnly = value.match(/([+-])(\\d{2})$/);
                        if (hourOnly) {
                            const offset = parseOffset(hourOnly[1], hourOnly[2], '00');
                            return capy.isType(offset, 'Success')
                                ? success({ buffer: value.slice(0, -3), offset_minutes: new capy.Some({ value: offset.value }) })
                                : offset;
                        }
                        return success({ buffer: value, offset_minutes: capy.None });
                    }

                    function parseParts(hourPart, minutePart, secondPart, offset_minutes) {
                        const hourValue = parseInteger(hourPart, 'hour');
                        if (!capy.isType(hourValue, 'Success')) return hourValue;
                        const hour = __constructor__primitive__hour(hourValue.value);
                        if (!capy.isType(hour, 'Success')) return hour;
                        const minuteValue = parseInteger(minutePart, 'minute');
                        if (!capy.isType(minuteValue, 'Success')) return minuteValue;
                        const minute = __constructor__primitive__minute(minuteValue.value);
                        if (!capy.isType(minute, 'Success')) return minute;
                        const secondValue = parseInteger(secondPart, 'second');
                        if (!capy.isType(secondValue, 'Success')) return secondValue;
                        const second = __constructor__primitive__second(secondValue.value);
                        if (!capy.isType(second, 'Success')) return second;
                        const constructed = __constructor__data__Time(hour.value, minute.value, second.value, offset_minutes);
                        return capy.isType(constructed, 'Success')
                            ? constructed
                            : failure(`Invalid ISO 8601 time format: ${constructed.message}`);
                    }

                    function fromIso8601(iso) {
                        const value = String(iso).startsWith('T') ? String(iso).slice(1) : String(iso);
                        const zone = parseZone(value);
                        if (!capy.isType(zone, 'Success')) return zone;
                        const buffer = zone.value.buffer;
                        if (buffer.length === 8 && buffer[2] === ':' && buffer[5] === ':') {
                            return parseParts(buffer.slice(0, 2), buffer.slice(3, 5), buffer.slice(6, 8), zone.value.offset_minutes);
                        }
                        if (buffer.length === 6) {
                            return parseParts(buffer.slice(0, 2), buffer.slice(2, 4), buffer.slice(4, 6), zone.value.offset_minutes);
                        }
                        return failure(`Invalid ISO 8601 time format: expected time in \\`HH:MM:SS\\`, \\`HHMMSS\\`, \\`THH:MM:SS\\`, or \\`THHMMSS\\` format, got \\`${iso}\\``);
                    }

                    const zEROHOUR = 0;
                    const ZERO_HOUR = zEROHOUR;
                    const nOONHOUR = 12;
                    const NOON_HOUR = nOONHOUR;
                    const zEROMINUTE = 0;
                    const ZERO_MINUTE = zEROMINUTE;
                    const zEROSECOND = 0;
                    const ZERO_SECOND = zEROSECOND;
                    const mIDNIGHT = new Time({ hour: zEROHOUR, minute: zEROMINUTE, second: zEROSECOND, offset_minutes: capy.None });
                    const MIDNIGHT = mIDNIGHT;
                    const nOON = new Time({ hour: nOONHOUR, minute: zEROMINUTE, second: zEROSECOND, offset_minutes: capy.None });
                    const NOON = nOON;
                    const __capybaraPrimitiveTypes = Object.freeze({
                        hour: Object.freeze({ cfunType: '/capy/date_time/Time.hour', backingType: 'int' }),
                        minute: Object.freeze({ cfunType: '/capy/date_time/Time.minute', backingType: 'int' }),
                        second: Object.freeze({ cfunType: '/capy/date_time/Time.second', backingType: 'int' }),
                        offset_minutes: Object.freeze({ cfunType: '/capy/date_time/Time.offset_minutes', backingType: 'int' }),
                    });

                    module.exports = {
                        Time,
                        __constructor__data__Time,
                        __constructor__primitive__hour,
                        __constructor__primitive__minute,
                        __constructor__primitive__second,
                        __constructor__primitive__offset_minutes,
                        greater__op_greater__hour__hour,
                        greater__op_greater__minute__minute,
                        greater__op_greater__second__second,
                        greater_op3d__op_greater_op3d__hour__hour,
                        greater_op3d__op_greater_op3d__minute__minute,
                        greater_op3d__op_greater_op3d__second__second,
                        less__op_less__hour__hour,
                        less__op_less__minute__minute,
                        less__op_less__second__second,
                        less_op3d__op_less_op3d__hour__hour,
                        less_op3d__op_less_op3d__minute__minute,
                        less_op3d__op_less_op3d__second__second,
                        op3d_op3d__op_op3d_op3d__hour__hour,
                        op3d_op3d__op_op3d_op3d__minute__minute,
                        op3d_op3d__op_op3d_op3d__second__second,
                        hOURSINDAY,
                        HOURS_IN_DAY: hOURSINDAY,
                        mINUTESINHOUR,
                        MINUTES_IN_HOUR: mINUTESINHOUR,
                        sECONDSINMINUTE,
                        SECONDS_IN_MINUTE: sECONDSINMINUTE,
                        sECONDSINHOUR,
                        SECONDS_IN_HOUR: sECONDSINHOUR,
                        mINUTESINDAY,
                        MINUTES_IN_DAY: mINUTESINDAY,
                        sECONDSINDAY,
                        SECONDS_IN_DAY: sECONDSINDAY,
                        mAXOFFSETMINUTES,
                        MAX_OFFSET_MINUTES: mAXOFFSETMINUTES,
                        zEROHOUR,
                        ZERO_HOUR,
                        nOONHOUR,
                        NOON_HOUR,
                        zEROMINUTE,
                        ZERO_MINUTE,
                        zEROSECOND,
                        ZERO_SECOND,
                        mIDNIGHT,
                        MIDNIGHT,
                        nOON,
                        NOON,
                        __capybaraPrimitiveTypes,
                        fromIso8601,
                        from_iso_8601: fromIso8601,
                    };
                    """;
        }

        private static String durationRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    const success = value => new capy.Success({ value });
                    const failure = message => new capy.Error({ message });

                    class DateDuration {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateDuration';
                            this.__capybaraTypes = ['DateDuration', 'Duration', 'CapybaraDataValue'];
                            this._years = fields.years ?? fields._years ?? 0;
                            this._months = fields.months ?? fields._months ?? 0;
                            this._days = fields.days ?? fields._days ?? 0;
                            this._hours = fields.hours ?? fields._hours ?? 0;
                            this._minutes = fields.minutes ?? fields._minutes ?? 0;
                            this._seconds = fields.seconds ?? fields._seconds ?? 0;
                            return capy.methodAliasProxy(this);
                        }
                        with(fields = {}) {
                            return new DateDuration({
                                years: Object.prototype.hasOwnProperty.call(fields, 'years') ? fields.years : this._years,
                                months: Object.prototype.hasOwnProperty.call(fields, 'months') ? fields.months : this._months,
                                days: Object.prototype.hasOwnProperty.call(fields, 'days') ? fields.days : this._days,
                                hours: Object.prototype.hasOwnProperty.call(fields, 'hours') ? fields.hours : this._hours,
                                minutes: Object.prototype.hasOwnProperty.call(fields, 'minutes') ? fields.minutes : this._minutes,
                                seconds: Object.prototype.hasOwnProperty.call(fields, 'seconds') ? fields.seconds : this._seconds,
                            });
                        }
                        with_(years, months, days, hours, minutes, seconds) {
                            return arguments.length === 1 && years && typeof years === 'object'
                                ? this.with(years)
                                : new DateDuration({ years, months, days, hours, minutes, seconds });
                        }
                        toString() {
                            return capy.dataToString(this);
                        }
                        capybaraDataValueInfo() {
                            return {
                                name: 'DateDuration',
                                packageName: 'capy.dateTime',
                                packagePath: 'capy/date_time/Duration',
                                fields: [
                                    { name: 'years', value: this._years },
                                    { name: 'months', value: this._months },
                                    { name: 'days', value: this._days },
                                    { name: 'hours', value: this._hours },
                                    { name: 'minutes', value: this._minutes },
                                    { name: 'seconds', value: this._seconds },
                                ],
                            };
                        }
                        negate() {
                            return new DateDuration({
                                years: -this._years,
                                months: -this._months,
                                days: -this._days,
                                hours: -this._hours,
                                minutes: -this._minutes,
                                seconds: -this._seconds,
                            });
                        }
                        years() { return this._years; }
                        months() { return this._months; }
                        days() { return this._days; }
                        hours() { return this._hours; }
                        minutes() { return this._minutes; }
                        seconds() { return this._seconds; }
                        totalSeconds() {
                            return this._days * 86400 + this._hours * 3600 + this._minutes * 60 + this._seconds;
                        }
                        toIso8601() {
                            if (this._years === 0 && this._months === 0 && this._days === 0 && this._hours === 0 && this._minutes === 0 && this._seconds === 0) {
                                return 'PT0S';
                            }
                            const datePart = `${this._years !== 0 ? `${this._years}Y` : ''}${this._months !== 0 ? `${this._months}M` : ''}${this._days !== 0 ? `${this._days}D` : ''}`;
                            const timePart = `${this._hours !== 0 ? `${this._hours}H` : ''}${this._minutes !== 0 ? `${this._minutes}M` : ''}${this._seconds !== 0 ? `${this._seconds}S` : ''}`;
                            return `P${datePart}${timePart ? `T${timePart}` : ''}`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    class WeekDuration {
                        constructor(fields = {}) {
                            this.__capybaraType = 'WeekDuration';
                            this.__capybaraTypes = ['WeekDuration', 'Duration', 'CapybaraDataValue'];
                            this._weeks = fields.weeks ?? fields._weeks ?? 0;
                            return capy.methodAliasProxy(this);
                        }
                        with(fields = {}) {
                            return new WeekDuration({
                                weeks: Object.prototype.hasOwnProperty.call(fields, 'weeks') ? fields.weeks : this._weeks,
                            });
                        }
                        with_(weeks) {
                            return arguments.length === 1 && weeks && typeof weeks === 'object'
                                ? this.with(weeks)
                                : new WeekDuration({ weeks });
                        }
                        toString() {
                            return capy.dataToString(this);
                        }
                        capybaraDataValueInfo() {
                            return {
                                name: 'WeekDuration',
                                packageName: 'capy.dateTime',
                                packagePath: 'capy/date_time/Duration',
                                fields: [{ name: 'weeks', value: this._weeks }],
                            };
                        }
                        negate() {
                            return new WeekDuration({ weeks: -this._weeks });
                        }
                        years() { return 0; }
                        months() { return 0; }
                        days() { return this._weeks * 7; }
                        hours() { return 0; }
                        minutes() { return 0; }
                        seconds() { return 0; }
                        weeks() { return this._weeks; }
                        totalSeconds() {
                            return this._weeks * 7 * 86400;
                        }
                        toIso8601() {
                            return `P${this._weeks}W`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                    }

                    const zERO = new DateDuration({ years: 0, months: 0, days: 0, hours: 0, minutes: 0, seconds: 0 });
                    const ZERO = zERO;

                    function toDuration(parts) {
                        return parts.weeks !== 0
                            ? new WeekDuration({ weeks: parts.weeks })
                            : new DateDuration(parts);
                    }

                    function validateCombined(parts) {
                        if (parts.months < 0 || parts.months > 12) return failure('Invalid ISO 8601 duration format: combined duration month component must be between 0 and 12');
                        if (parts.days < 0 || parts.days > 31) return failure('Invalid ISO 8601 duration format: combined duration day component must be between 0 and 31');
                        if (parts.hours < 0 || parts.hours > 23) return failure('Invalid ISO 8601 duration format: combined duration hour component must be between 0 and 23');
                        if (parts.minutes < 0 || parts.minutes > 59) return failure('Invalid ISO 8601 duration format: combined duration minute component must be between 0 and 59');
                        if (parts.seconds < 0 || parts.seconds > 59) return failure('Invalid ISO 8601 duration format: combined duration second component must be between 0 and 59');
                        return success(toDuration(parts));
                    }

                    function parseCombinedBasic(value) {
                        return validateCombined({
                            years: Number.parseInt(value.slice(0, 4), 10),
                            months: Number.parseInt(value.slice(4, 6), 10),
                            weeks: 0,
                            days: Number.parseInt(value.slice(6, 8), 10),
                            hours: Number.parseInt(value.slice(9, 11), 10),
                            minutes: Number.parseInt(value.slice(11, 13), 10),
                            seconds: Number.parseInt(value.slice(13, 15), 10),
                        });
                    }

                    function parseCombinedExtended(value) {
                        return validateCombined({
                            years: Number.parseInt(value.slice(0, 4), 10),
                            months: Number.parseInt(value.slice(5, 7), 10),
                            weeks: 0,
                            days: Number.parseInt(value.slice(8, 10), 10),
                            hours: Number.parseInt(value.slice(11, 13), 10),
                            minutes: Number.parseInt(value.slice(14, 16), 10),
                            seconds: Number.parseInt(value.slice(17, 19), 10),
                        });
                    }

                    function componentOrder(inTime, designator) {
                        if (inTime) {
                            if (designator === 'H') return 5;
                            if (designator === 'M') return 6;
                            if (designator === 'S') return 7;
                            return null;
                        }
                        if (designator === 'Y') return 1;
                        if (designator === 'M') return 2;
                        if (designator === 'W') return 3;
                        if (designator === 'D') return 4;
                        return null;
                    }

                    function parseDesignatorDuration(value) {
                        let index = 0;
                        let inTime = false;
                        let previousOrder = 0;
                        let hasComponent = false;
                        let hasTimeComponent = false;
                        const parts = { years: 0, months: 0, weeks: 0, days: 0, hours: 0, minutes: 0, seconds: 0 };
                        while (index < value.length) {
                            if (value[index] === 'T') {
                                if (inTime) return failure('Invalid ISO 8601 duration format: duplicate `T` separator');
                                if (parts.weeks !== 0) return failure('Invalid ISO 8601 duration format: week durations cannot be combined with time components');
                                inTime = true;
                                previousOrder = 4;
                                index++;
                                continue;
                            }
                            const match = /^\\d+/.exec(value.slice(index));
                            if (!match) return failure(`Invalid ISO 8601 duration format: expected digit or \\`T\\`, got \\`${value[index]}\\``);
                            const amount = Number.parseInt(match[0], 10);
                            index += match[0].length;
                            if (index >= value.length) return failure(`Invalid ISO 8601 duration format: missing designator after \\`${amount}\\``);
                            const designator = value[index++];
                            const order = componentOrder(inTime, designator);
                            if (order == null) {
                                return failure(`Invalid ISO 8601 duration format: unexpected ${inTime ? 'time' : 'date'} designator \\`${designator}\\``);
                            }
                            if (order <= previousOrder) return failure('Invalid ISO 8601 duration format: components must appear in ISO 8601 order and may not repeat');
                            if (designator === 'W' && (inTime || parts.years || parts.months || parts.days || parts.hours || parts.minutes || parts.seconds)) {
                                return failure('Invalid ISO 8601 duration format: week durations cannot be combined with other components');
                            }
                            if (designator !== 'W' && parts.weeks !== 0) {
                                return failure('Invalid ISO 8601 duration format: week durations cannot be combined with other components');
                            }
                            if (designator === 'Y') parts.years = amount;
                            else if (designator === 'M' && inTime) parts.minutes = amount;
                            else if (designator === 'M') parts.months = amount;
                            else if (designator === 'W') parts.weeks = amount;
                            else if (designator === 'D') parts.days = amount;
                            else if (designator === 'H') parts.hours = amount;
                            else if (designator === 'S') parts.seconds = amount;
                            previousOrder = order;
                            hasComponent = true;
                            hasTimeComponent = hasTimeComponent || inTime;
                        }
                        if (!hasComponent) return failure('Invalid ISO 8601 duration format: missing components');
                        if (inTime && !hasTimeComponent) return failure('Invalid ISO 8601 duration format: missing time components after `T`');
                        return success(toDuration(parts));
                    }

                    function fromIso8601(iso) {
                        const value = String(iso);
                        if (!value.startsWith('P')) return failure('Invalid ISO 8601 duration format: must start with `P`');
                        const duration = value.slice(1);
                        if (duration.length === 0) return failure('Invalid ISO 8601 duration format: missing components');
                        if (/^\\d{8}T\\d{6}$/.test(duration)) return parseCombinedBasic(duration);
                        if (/^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$/.test(duration)) return parseCombinedExtended(duration);
                        return parseDesignatorDuration(duration);
                    }

                    module.exports = {
                        DateDuration,
                        WeekDuration,
                        zERO,
                        ZERO,
                        fromIso8601,
                        from_iso_8601: fromIso8601,
                    };
                    """;
        }

        private static String dateTimeRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');
                    const DateModule = require('./DateModule.js');
                    const TimeModule = require('./TimeModule.js');
                    const DurationModule = require('./DurationModule.js');
                    const success = value => new capy.Success({ value });
                    const failure = message => new capy.Error({ message });
                    const floorDiv = (left, right) => Math.floor(left / right);
                    const floorMod = (left, right) => left - floorDiv(left, right) * right;

                    class DateTime {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateTime';
                            this.__capybaraTypes = ['DateTime', 'CapybaraDataValue'];
                            this.date = fields.date;
                            this.time = fields.time;
                            return capy.methodAliasProxy(this);
                        }
                        with(fields = {}) {
                            return new DateTime({
                                date: Object.prototype.hasOwnProperty.call(fields, 'date') ? fields.date : this.date,
                                time: Object.prototype.hasOwnProperty.call(fields, 'time') ? fields.time : this.time,
                            });
                        }
                        with_(date, time) {
                            return arguments.length === 1 && date && typeof date === 'object'
                                ? this.with(date)
                                : new DateTime({ date, time });
                        }
                        toString() {
                            return capy.dataToString(this);
                        }
                        capybaraDataValueInfo() {
                            return capy.dataValueInfo(this, 'DateTime', 'capy.dateTime', 'capy/date_time/DateTime');
                        }
                        timestamp() {
                            return this.date.toDaysSinceUnixEpoch() * TimeModule.sECONDSINDAY + this.time.toSeconds();
                        }
                        plus(duration) {
                            const dateWithMonths = this.date.addYearsMonths(duration.years(), duration.months());
                            const daysFromHours = floorDiv(duration.hours(), TimeModule.hOURSINDAY);
                            const daysFromMinutes = floorDiv(duration.minutes(), TimeModule.mINUTESINDAY);
                            const daysFromSeconds = floorDiv(duration.seconds(), TimeModule.sECONDSINDAY);
                            const remainderHours = floorMod(duration.hours(), TimeModule.hOURSINDAY);
                            const remainderMinutes = floorMod(duration.minutes(), TimeModule.mINUTESINDAY);
                            const remainderSeconds = floorMod(duration.seconds(), TimeModule.sECONDSINDAY);
                            const intraDaySeconds = remainderHours * TimeModule.sECONDSINHOUR
                                + remainderMinutes * TimeModule.sECONDSINMINUTE
                                + remainderSeconds;
                            const shiftedSeconds = this.time.toSeconds() + intraDaySeconds;
                            const extraDays = floorDiv(shiftedSeconds, TimeModule.sECONDSINDAY);
                            return new DateTime({
                                date: dateWithMonths.addDays(duration.days() + daysFromHours + daysFromMinutes + daysFromSeconds + extraDays),
                                time: this.time.addSeconds(intraDaySeconds),
                            });
                        }
                        minus(value) {
                            if (value && value.__capybaraType === 'DateTime') {
                                const delta = this.timestamp() - value.timestamp();
                                const sign = delta < 0 ? -1 : 1;
                                let remaining = Math.abs(delta);
                                const days = Math.trunc(remaining / TimeModule.sECONDSINDAY);
                                remaining %= TimeModule.sECONDSINDAY;
                                const hours = Math.trunc(remaining / TimeModule.sECONDSINHOUR);
                                remaining %= TimeModule.sECONDSINHOUR;
                                const minutes = Math.trunc(remaining / TimeModule.sECONDSINMINUTE);
                                const seconds = remaining % TimeModule.sECONDSINMINUTE;
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
                        lessThan(other) {
                            return this.date.toDaysSinceUnixEpoch() < other.date.toDaysSinceUnixEpoch()
                                || (this.date.toDaysSinceUnixEpoch() === other.date.toDaysSinceUnixEpoch()
                                    && this.time.toSeconds() < other.time.toSeconds());
                        }
                        less_than(other) {
                            return this.lessThan(other);
                        }
                        less(other) {
                            return this.lessThan(other);
                        }
                        greaterThan(other) {
                            return this.date.toDaysSinceUnixEpoch() > other.date.toDaysSinceUnixEpoch()
                                || (this.date.toDaysSinceUnixEpoch() === other.date.toDaysSinceUnixEpoch()
                                    && this.time.toSeconds() > other.time.toSeconds());
                        }
                        greater_than(other) {
                            return this.greaterThan(other);
                        }
                        greater(other) {
                            return this.greaterThan(other);
                        }
                        toIso8601() {
                            const normalized = this.time.offset_minutes && this.time.offset_minutes.__capybaraType === 'Some'
                                ? new DateTime({
                                    date: this.date,
                                    time: new TimeModule.Time({
                                        hour: this.time.hour,
                                        minute: this.time.minute,
                                        second: this.time.second,
                                        offset_minutes: capy.None,
                                    }),
                                }).plus(new DurationModule.DateDuration({
                                    years: 0,
                                    months: 0,
                                    days: 0,
                                    hours: 0,
                                    minutes: -this.time.offset_minutes.value,
                                    seconds: 0,
                                }))
                                : this;
                            const utcTime = new TimeModule.Time({
                                hour: normalized.time.hour,
                                minute: normalized.time.minute,
                                second: normalized.time.second,
                                offset_minutes: new capy.Some({ value: 0 }),
                            });
                            return `${normalized.date.toIso8601()}T${utcTime.toIso8601()}`;
                        }
                        to_iso_8601() {
                            return this.toIso8601();
                        }
                        static fromTimestamp(timestamp) {
                            const normalizedTimestamp = Number(timestamp);
                            const days = Math.floor(normalizedTimestamp / TimeModule.sECONDSINDAY);
                            const seconds = ((normalizedTimestamp % TimeModule.sECONDSINDAY) + TimeModule.sECONDSINDAY) % TimeModule.sECONDSINDAY;
                            return new DateTime({
                                date: DateModule.fromDaysSinceUnixEpoch(days),
                                time: TimeModule.mIDNIGHT.addSeconds(seconds),
                            });
                        }
                    }

                    function fromTimestamp(timestamp) {
                        return DateTime.fromTimestamp(timestamp);
                    }

                    function scanSplit(value) {
                        const index = String(value).indexOf('T');
                        if (index < 0) return failure('Invalid ISO 8601 date-time format: expected `T` separator between date and time');
                        if (String(value).indexOf('T', index + 1) >= 0) return failure('Invalid ISO 8601 date-time format: expected exactly one `T` separator');
                        const left = String(value).slice(0, index);
                        const right = String(value).slice(index + 1);
                        if (left.length === 0 || right.length === 0) return failure('Invalid ISO 8601 date-time format: date-time parts must not be empty');
                        return success({ left, right });
                    }

                    function fromIso8601(iso) {
                        const split = scanSplit(iso);
                        if (!capy.isType(split, 'Success')) return split;
                        const left = split.value.left;
                        const right = split.value.right;
                        const dateIsExtended = left.length === 10 && left[4] === '-' && left[7] === '-';
                        const timeIsExtended = right.length >= 8 && right[2] === ':' && right[5] === ':';
                        if (dateIsExtended !== timeIsExtended) {
                            return failure('Invalid ISO 8601 date-time format: date and time must use the same basic or extended format');
                        }
                        const date = DateModule.fromIso8601(left);
                        if (!capy.isType(date, 'Success')) return date;
                        const timeWithOffset = TimeModule.fromIso8601(right);
                        if (!capy.isType(timeWithOffset, 'Success')) return timeWithOffset;
                        if (!timeWithOffset.value.offset_minutes || timeWithOffset.value.offset_minutes.__capybaraType !== 'Some') {
                            return failure('Invalid ISO 8601 date-time format: expected UTC designator `Z` or numeric offset');
                        }
                        const utcTime = TimeModule.__constructor__data__Time(
                            timeWithOffset.value.hour,
                            timeWithOffset.value.minute,
                            timeWithOffset.value.second,
                            capy.None
                        );
                        if (!capy.isType(utcTime, 'Success')) return utcTime;
                        return success(new DateTime({ date: date.value, time: utcTime.value }).plus(new DurationModule.DateDuration({
                            years: 0,
                            months: 0,
                            days: 0,
                            hours: 0,
                            minutes: -timeWithOffset.value.offset_minutes.value,
                            seconds: 0,
                        })));
                    }

                    const uNIXEPOCH = new DateTime({ date: DateModule.uNIXDATE, time: TimeModule.mIDNIGHT });
                    const UNIX_EPOCH = uNIXEPOCH;

                    module.exports = {
                        DateTime,
                        uNIXEPOCH,
                        UNIX_EPOCH,
                        fromTimestamp,
                        from_timestamp: fromTimestamp,
                        fromIso8601,
                        from_iso_8601: fromIso8601,
                    };
                    """;
        }

        private static String intervalRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');
                    const DateTimeModule = require('./DateTimeModule.js');
                    const DurationModule = require('./DurationModule.js');
                    const success = value => new capy.Success({ value });
                    const failure = message => new capy.Error({ message });
                    const invalid = message => failure(`Invalid ISO 8601 interval format: ${message}`);

                    class DateTimeStartEnd {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateTimeStartEnd';
                            this.__capybaraTypes = ['DateTimeStartEnd', 'Interval', 'CapybaraDataValue'];
                            this._start = fields.start ?? fields._start;
                            this._end = fields.end ?? fields._end;
                            return capy.methodAliasProxy(this);
                        }
                        start() { return this._start; }
                        end() { return this._end; }
                        duration() { return this._end.minus(this._start); }
                        toIso8601() { return `${this._start.toIso8601()}/${this._end.toIso8601()}`; }
                        to_iso_8601() { return this.toIso8601(); }
                        toString() { return capy.dataToString(this); }
                        capybaraDataValueInfo() {
                            return {
                                name: 'DateTimeStartEnd',
                                packageName: 'capy.dateTime',
                                packagePath: 'capy/date_time/Interval',
                                fields: [{ name: 'start', value: this._start }, { name: 'end', value: this._end }],
                            };
                        }
                    }

                    class DateTimeStartDuration {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateTimeStartDuration';
                            this.__capybaraTypes = ['DateTimeStartDuration', 'Interval', 'CapybaraDataValue'];
                            this._start = fields.start ?? fields._start;
                            this._duration = fields.duration ?? fields._duration;
                            return capy.methodAliasProxy(this);
                        }
                        start() { return this._start; }
                        end() { return this._start.plus(this._duration); }
                        duration() { return this._duration; }
                        toIso8601() { return `${this._start.toIso8601()}/${this._duration.toIso8601()}`; }
                        to_iso_8601() { return this.toIso8601(); }
                        toString() { return capy.dataToString(this); }
                        capybaraDataValueInfo() {
                            return {
                                name: 'DateTimeStartDuration',
                                packageName: 'capy.dateTime',
                                packagePath: 'capy/date_time/Interval',
                                fields: [{ name: 'start', value: this._start }, { name: 'duration', value: this._duration }],
                            };
                        }
                    }

                    class DateTimeDurationEnd {
                        constructor(fields = {}) {
                            this.__capybaraType = 'DateTimeDurationEnd';
                            this.__capybaraTypes = ['DateTimeDurationEnd', 'Interval', 'CapybaraDataValue'];
                            this._duration = fields.duration ?? fields._duration;
                            this._end = fields.end ?? fields._end;
                            return capy.methodAliasProxy(this);
                        }
                        start() { return this._end.minus(this._duration); }
                        end() { return this._end; }
                        duration() { return this._duration; }
                        toIso8601() { return `${this._duration.toIso8601()}/${this._end.toIso8601()}`; }
                        to_iso_8601() { return this.toIso8601(); }
                        toString() { return capy.dataToString(this); }
                        capybaraDataValueInfo() {
                            return {
                                name: 'DateTimeDurationEnd',
                                packageName: 'capy.dateTime',
                                packagePath: 'capy/date_time/Interval',
                                fields: [{ name: 'duration', value: this._duration }, { name: 'end', value: this._end }],
                            };
                        }
                    }

                    function splitInterval(value) {
                        const hasSlash = value.includes('/');
                        const hasDash = value.includes('--');
                        if (hasSlash && hasDash) return invalid('expected exactly one interval separator');
                        const separator = hasSlash ? '/' : hasDash ? '--' : null;
                        if (!separator) return invalid('expected `/` or `--` separator');
                        const first = value.indexOf(separator);
                        if (first < 0 || value.indexOf(separator, first + separator.length) >= 0) {
                            return invalid('expected exactly one interval separator');
                        }
                        const left = value.slice(0, first);
                        const right = value.slice(first + separator.length);
                        if (left.length === 0 || right.length === 0) return invalid('interval parts must not be empty');
                        return success({ left, right });
                    }

                    function parseDateTime(value) {
                        const parsed = DateTimeModule.fromIso8601(value);
                        return capy.isType(parsed, 'Success') ? parsed : invalid('interval endpoints must be UTC date-times');
                    }

                    function fromIso8601(iso) {
                        const split = splitInterval(String(iso));
                        if (!capy.isType(split, 'Success')) return split;
                        const left = split.value.left;
                        const right = split.value.right;
                        if (left.startsWith('P') && right.startsWith('P')) {
                            return invalid('interval cannot contain a duration on both sides');
                        }
                        if (left.startsWith('P')) {
                            const duration = DurationModule.fromIso8601(left);
                            if (!capy.isType(duration, 'Success')) return duration;
                            const end = parseDateTime(right);
                            return capy.isType(end, 'Success')
                                ? success(new DateTimeDurationEnd({ duration: duration.value, end: end.value }))
                                : end;
                        }
                        if (right.startsWith('P')) {
                            const duration = DurationModule.fromIso8601(right);
                            if (!capy.isType(duration, 'Success')) return duration;
                            const start = parseDateTime(left);
                            return capy.isType(start, 'Success')
                                ? success(new DateTimeStartDuration({ start: start.value, duration: duration.value }))
                                : start;
                        }
                        const start = parseDateTime(left);
                        if (!capy.isType(start, 'Success')) return start;
                        const end = parseDateTime(right);
                        if (!capy.isType(end, 'Success')) return end;
                        return start.value.greaterThan(end.value)
                            ? invalid('interval start must not be after interval end')
                            : success(new DateTimeStartEnd({ start: start.value, end: end.value }));
                    }

                    module.exports = {
                        DateTimeDurationEnd,
                        DateTimeStartDuration,
                        DateTimeStartEnd,
                        fromIso8601,
                        from_iso_8601: fromIso8601,
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
                        constructor(result, message = '', type = 'Assertion') {
                            this.result = result;
                            this.message = message;
                            this.type = type;
                        }
                        succeeded() {
                            return this.result;
                        }
                    }

                    function assertion(result, message, type) {
                        return () => new AssertionResult(Boolean(result), message, type);
                    }

                        function assertionsOf(value) {
                            if (value == null) {
                                return [];
                            }
                            if (Array.isArray(value)) {
                                return flattenAssertions(value);
                            }
                        if (Array.isArray(value.assertions)) {
                            return value.assertions;
                        }
                        if (typeof value.succeeded === 'function') {
                            return [() => value];
                        }
                        if (Object.prototype.hasOwnProperty.call(value, 'result')) {
                            return [() => value];
                        }
                            return [assertion(false, `Unsupported assertion result: ${String(value)}`, 'AssertRuntime')];
                        }

                        function flattenAssertions(values) {
                            const flattened = [];
                            for (const value of values) {
                                flattened.push(...assertionsOf(value));
                            }
                            return flattened;
                        }

                    function display(value) {
                        return value && typeof value.toIso8601 === 'function' ? value.toIso8601() : capy.toStringValue(value);
                    }

                    function valueAt(value, name) {
                        if (value == null) return undefined;
                        const member = value[name];
                        return typeof member === 'function' ? member.call(value) : member;
                    }

                    function sequenceAsList(value) {
                        if (value && typeof value.asList === 'function') {
                            return value.asList();
                        }
                        return undefined;
                    }

                    function collectionSize(value) {
                        if (value instanceof Map || value instanceof Set) return value.size;
                        const sequence = sequenceAsList(value);
                        if (sequence !== undefined) return sequence.length;
                        return value?.length ?? 0;
                    }

                    function containsValue(container, expected) {
                        if (container && container.__capybaraType === 'Some') return capy.equals(container.value, expected);
                        if (container && container.__capybaraType === 'Success') return capy.equals(container.value, expected);
                        if (typeof container === 'string') return container.includes(String(expected));
                        if (container instanceof Map) return Array.from(container.values()).some(value => capy.equals(value, expected));
                        const sequence = sequenceAsList(container);
                        if (sequence !== undefined) return capy.contains(sequence, expected);
                        return capy.contains(container, expected);
                    }

                    function isSuccess(value) {
                        return capy.isType(value, 'Success');
                    }

                        function isError(value) {
                            return capy.isType(value, 'Error');
                        }

                        function withMethodAliases(target) {
                            return new Proxy(target, {
                                get(target, property, receiver) {
                                    if (property in target) return Reflect.get(target, property, receiver);
                                    if (typeof property === 'string') {
                                        const overloadSeparator = property.indexOf('__');
                                        if (overloadSeparator > 0) {
                                            const base = property.slice(0, overloadSeparator);
                                            if (typeof target[base] === 'function') {
                                                return target[base].bind(target);
                                            }
                                        }
                                    }
                                    return undefined;
                                },
                            });
                        }

                        class GenericAssert {
                            constructor(value, assertions = []) {
                                this.__capybaraType = 'Assert';
                                this.__capybaraTypes = ['Assert'];
                                this.value = value;
                                this.assertions = assertions;
                                return withMethodAliases(this);
                            }
                        append(result, message, type) {
                            return new GenericAssert(this.value, this.assertions.concat(assertion(result, message, type)));
                        }
                        failed() {
                            return this.assertions.some(supplier => !supplier().result);
                        }
                        succeeded() {
                            return !this.failed();
                        }
                        contains(expected) {
                            return this.append(
                                containsValue(this.value, expected),
                                `Expected ${display(this.value)} to contain ${display(expected)}`,
                                'contains'
                            );
                        }
                        doesNotContain(expected) {
                            return this.append(
                                !containsValue(this.value, expected),
                                `Expected ${display(this.value)} not to contain ${display(expected)}`,
                                'does_not_contain'
                            );
                        }
                        containsKey(expected) {
                            return this.append(
                                this.value instanceof Map && this.value.has(expected),
                                `Expected ${display(this.value)} to contain key ${display(expected)}`,
                                'contains_key'
                            );
                        }
                        containsValue(expected) {
                            return this.append(
                                this.value instanceof Map && Array.from(this.value.values()).some(value => capy.equals(value, expected)),
                                `Expected ${display(this.value)} to contain value ${display(expected)}`,
                                'contains_value'
                            );
                        }
                        doesNotContainKey(expected) {
                            return this.append(
                                !(this.value instanceof Map && this.value.has(expected)),
                                `Expected ${display(this.value)} not to contain key ${display(expected)}`,
                                'does_not_contain_key'
                            );
                        }
                        hasSize(expected) {
                            return this.append(
                                capy.equals(collectionSize(this.value), expected),
                                `Expected size ${collectionSize(this.value)} to equal ${expected}`,
                                'has_size'
                            );
                        }
                        isEmpty() {
                            return this.hasSize(0);
                        }
                        isEqualTo(expected, epsilon) {
                            const result = epsilon === undefined
                                ? capy.equals(this.value, expected)
                                : Math.abs(this.value - expected) <= epsilon;
                            return this.append(
                                result,
                                `Expected ${display(this.value)} to equal ${display(expected)}`,
                                'is_equal_to'
                            );
                        }
                        isGreaterThan(expected) {
                            return this.append(this.value > expected, `Expected ${display(this.value)} to be greater than ${display(expected)}`, 'is_greater_than');
                        }
                        isGreaterOrEqualsThan(expected) {
                            return this.append(this.value >= expected, `Expected ${display(this.value)} to be greater than or equal to ${display(expected)}`, 'is_greater_or_equals_than');
                        }
                        isLessThan(expected) {
                            return this.append(this.value < expected, `Expected ${display(this.value)} to be less than ${display(expected)}`, 'is_less_than');
                        }
                        isLessOrEqualsThan(expected) {
                            return this.append(this.value <= expected, `Expected ${display(this.value)} to be less than or equal to ${display(expected)}`, 'is_less_or_equals_than');
                        }
                        isBetween(start, end) {
                            return this.isGreaterOrEqualsThan(start).isLessOrEqualsThan(end);
                        }
                        isZero() {
                            return this.isEqualTo(0);
                        }
                        isOne() {
                            return this.isEqualTo(1);
                        }
                        isTrue() {
                            return this.isEqualTo(true);
                        }
                        isFalse() {
                            return this.isEqualTo(false);
                        }
                        startsWith(expected) {
                            return this.append(String(this.value).startsWith(expected), `Expected ${display(this.value)} to start with ${display(expected)}`, 'starts_with');
                        }
                        doesNotStartWith(expected) {
                            return this.append(!String(this.value).startsWith(expected), `Expected ${display(this.value)} not to start with ${display(expected)}`, 'does_not_start_with');
                        }
                        succeeds(expectedOrAssert) {
                            const ok = isSuccess(this.value);
                            const base = this.append(ok, `Expected Result.Success, got ${display(this.value)}`, 'succeeds');
                            if (!ok) return base;
                            if (arguments.length === 0) return base;
                            if (typeof expectedOrAssert === 'function') {
                                return new GenericAssert(this.value, base.assertions.concat(assertionsOf(expectedOrAssert(this.value.value))));
                            }
                            return base.append(
                                capy.equals(this.value.value, expectedOrAssert),
                                `Expected success value ${display(this.value.value)} to equal ${display(expectedOrAssert)}`,
                                'succeeds'
                            );
                        }
                        fails(expectedMessage) {
                            const ok = isError(this.value);
                            const messageMatches = expectedMessage === undefined || (ok && capy.equals(this.value.message, expectedMessage));
                            return this.append(
                                ok && messageMatches,
                                expectedMessage === undefined
                                    ? `Expected Result.Error, got ${display(this.value)}`
                                    : `Expected error message ${display(expectedMessage)}, got ${display(this.value?.message)}`,
                                'fails'
                            );
                        }
                        hasDay(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'day') ?? valueAt(valueAt(this.value, 'date'), 'day'), expected), `Expected day to equal ${expected}`, 'has_day');
                        }
                        hasMonth(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'month') ?? valueAt(valueAt(this.value, 'date'), 'month'), expected), `Expected month to equal ${expected}`, 'has_month');
                        }
                        hasYear(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'year') ?? valueAt(valueAt(this.value, 'date'), 'year'), expected), `Expected year to equal ${expected}`, 'has_year');
                        }
                        hasHour(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'hour') ?? valueAt(valueAt(this.value, 'time'), 'hour'), expected), `Expected hour to equal ${expected}`, 'has_hour');
                        }
                        hasMinute(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'minute') ?? valueAt(valueAt(this.value, 'time'), 'minute'), expected), `Expected minute to equal ${expected}`, 'has_minute');
                        }
                        hasSecond(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'second') ?? valueAt(valueAt(this.value, 'time'), 'second'), expected), `Expected second to equal ${expected}`, 'has_second');
                        }
                        hasOffsetMinutes(expected) {
                            const actual = valueAt(this.value, 'offset_minutes') ?? valueAt(valueAt(this.value, 'time'), 'offset_minutes');
                            const normalized = typeof expected === 'number' ? new capy.Some({ value: expected }) : expected;
                            return this.append(capy.equals(actual, normalized), `Expected offset to equal ${display(normalized)}`, 'has_offset_minutes');
                        }
                        hasDate(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'date'), expected), `Expected date to equal ${display(expected)}`, 'has_date');
                        }
                        hasTime(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'time'), expected), `Expected time to equal ${display(expected)}`, 'has_time');
                        }
                        hasYears(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'years'), expected), `Expected years to equal ${expected}`, 'has_years');
                        }
                        hasMonths(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'months'), expected), `Expected months to equal ${expected}`, 'has_months');
                        }
                        hasDays(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'days'), expected), `Expected days to equal ${expected}`, 'has_days');
                        }
                        hasHours(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'hours'), expected), `Expected hours to equal ${expected}`, 'has_hours');
                        }
                        hasMinutes(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'minutes'), expected), `Expected minutes to equal ${expected}`, 'has_minutes');
                        }
                        hasSeconds(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'seconds'), expected), `Expected seconds to equal ${expected}`, 'has_seconds');
                        }
                        hasWeeks(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'weeks'), expected), `Expected weeks to equal ${expected}`, 'has_weeks');
                        }
                        hasStart(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'start'), expected), `Expected start to equal ${display(expected)}`, 'has_start');
                        }
                        hasEnd(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'end'), expected), `Expected end to equal ${display(expected)}`, 'has_end');
                        }
                        hasDuration(expected) {
                            return this.append(capy.equals(valueAt(this.value, 'duration'), expected), `Expected duration to equal ${display(expected)}`, 'has_duration');
                        }
                    }

                    function assertThat(value) {
                        return new GenericAssert(value);
                    }

                        function assertAll(asserts) {
                            return new GenericAssert(null, flattenAssertions(asserts));
                        }

                    const exportsObject = {
                        assertAll,
                        assert_all: assertAll,
                        assertThat,
                        assert_that: assertThat,
                    };

                    module.exports = new Proxy(exportsObject, {
                        get(target, property) {
                            if (property in target) return target[property];
                            if (typeof property === 'string' && (property.startsWith('assertThat') || property.startsWith('assert_that'))) {
                                return assertThat;
                            }
                            if (typeof property === 'string' && (property.startsWith('assertAll') || property.startsWith('assert_all'))) {
                                return assertAll;
                            }
                            return undefined;
                        },
                    });
                    """;
        }

        private static String capyTestRuntime() {
            return """
                    'use strict';
                    const capy = require('../../dev/capylang/capybara.js');

                    function test(name, body) {
                        return { name, body };
                    }

                    function unwrapTestCases(testCases) {
                        return testCases.map(testCase => capy.isEffect(testCase) ? testCase.unsafe_run() : testCase);
                    }

                    function testFileAt(path, timestampMillis, testCases) {
                        return {
                            path,
                            file_name: path,
                            test_cases: unwrapTestCases(testCases),
                            timestamp_millis: timestampMillis,
                        };
                    }

                    function testFile(path, testCases) {
                        return capy.delay(() => testFileAt(path, Date.now(), testCases));
                    }

                    const exportsObject = {
                        test,
                        testFileAt,
                        test_file_at: testFileAt,
                        testFile,
                        test_file: testFile,
                    };

                    module.exports = new Proxy(exportsObject, {
                        get(target, property) {
                            if (property in target) return target[property];
                            if (typeof property === 'string'
                                && (property.startsWith('testFile__string__compiledlist_elementtype_')
                                    || property.startsWith('test_file__string__compiledlist_elementtype_'))
                                && (property.includes('compileddatatype_name_testcase')
                                    || property.includes('compileddataparenttype_name_effect'))) {
                                return testFile;
                            }
                            return undefined;
                        },
                    });
                    """;
        }

        private static String runtime() {
            return """
                    'use strict';

                    class NativeProviderError extends Error {
                        constructor(message, metadata = {}) {
                            super(message);
                            this.name = 'NativeProviderError';
                            this.interfaceId = metadata.interfaceId;
                            this.qualifier = metadata.qualifier;
                            this.providerSymbol = metadata.providerSymbol;
                            this.backend = metadata.backend;
                            this.sourceFile = metadata.sourceFile;
                        }
                    }

                    function nativeProviderError(message, metadata = {}) {
                        return new NativeProviderError(message, metadata);
                    }

                    function nativeProviderKey(interfaceId, qualifier) {
                        return `${interfaceId}#${qualifier}`;
                    }

                    function nativeProviderContext(metadata) {
                        const symbol = metadata.providerSymbol ? 'provider symbol `' + metadata.providerSymbol + '`, ' : '';
                        const backend = metadata.backend ? ' for backend `' + metadata.backend + '`' : '';
                        const source = metadata.sourceFile ? ' in source `' + metadata.sourceFile + '`' : '';
                        return symbol + 'interface `' + metadata.interfaceId + '` with qualifier `' + metadata.qualifier + '`' + backend + source;
                    }

                    function requireNativeText(value, name, metadata) {
                        if (typeof value !== 'string' || value.trim() === '') {
                            throw nativeProviderError('TypeMismatch: Native provider ' + name + ' is required for ' + nativeProviderContext(metadata) + '.', metadata);
                        }
                        return value;
                    }

                    function nativeFactory(options) {
                        const metadata = {
                            interfaceId: options?.interfaceId,
                            qualifier: options?.qualifier,
                            providerSymbol: options?.providerSymbol,
                            backend: options?.backend,
                            sourceFile: options?.sourceFile,
                        };
                        const interfaceId = requireNativeText(options?.interfaceId, 'interfaceId', metadata);
                        const qualifier = requireNativeText(options?.qualifier, 'qualifier', metadata);
                        const lifetime = options?.lifetime ?? 'factory';
                        if (lifetime !== 'singleton' && lifetime !== 'factory') {
                            throw nativeProviderError('UnsupportedBackend: Native provider for ' + nativeProviderContext(metadata) + ' has unsupported lifetime `' + lifetime + '`.', metadata);
                        }
                        if (options?.factory !== 'new' && options?.factory !== 'call') {
                            throw nativeProviderError('UnsupportedBackend: Native provider for ' + nativeProviderContext(metadata) + ' has unsupported JavaScript factory `' + options?.factory + '`.', metadata);
                        }
                        if (options?.exportExists !== true || typeof options?.exportValue !== 'function') {
                            throw nativeProviderError('UnsupportedBackend: Native provider for ' + nativeProviderContext(metadata) + ' requires export `' + options?.exportName + '` from module `' + options?.moduleName + '`.', metadata);
                        }
                        if (typeof options?.create !== 'function') {
                            throw nativeProviderError('UnsupportedBackend: Native provider factory is required for ' + nativeProviderContext(metadata) + '.', metadata);
                        }
                        return Object.freeze({
                            interfaceId,
                            qualifier,
                            providerSymbol: options?.providerSymbol,
                            backend: options?.backend,
                            sourceFile: options?.sourceFile,
                            lifetime,
                            metadata: Object.freeze({
                                interfaceId,
                                qualifier,
                                providerSymbol: options?.providerSymbol,
                                backend: options?.backend,
                                sourceFile: options?.sourceFile,
                                methods: Object.freeze([...(options?.metadata?.methods ?? [])].map(method => Object.freeze({
                                    name: method.name,
                                    arity: method.arity ?? 0,
                                }))),
                            }),
                            create: options.create,
                        });
                    }

                    function defineNativeProviders(providerTable) {
                        if (providerTable === null || typeof providerTable !== 'object') {
                            throw nativeProviderError('TypeMismatch: Native provider table must be an object.');
                        }
                        const providers = new Map();
                        for (const [key, provider] of Object.entries(providerTable)) {
                            if (!provider || typeof provider !== 'object') {
                                throw nativeProviderError('TypeMismatch: Native provider table entry `' + key + '` is invalid.');
                            }
                            const expectedKey = nativeProviderKey(provider.interfaceId, provider.qualifier);
                            if (key !== expectedKey) {
                                throw nativeProviderError('TypeMismatch: Native provider table key `' + key + '` does not match ' + nativeProviderContext(provider) + '.', provider);
                            }
                            if (providers.has(key)) {
                                throw nativeProviderError('DuplicateProvider: Duplicate native provider for ' + nativeProviderContext(provider) + '.', provider);
                            }
                            providers.set(key, { provider, singletonSet: false, singletonValue: undefined });
                        }
                        const table = Object.freeze({ __capybaraNativeProviders: providers });
                        return Object.freeze({
                            resolve: resolveNativeImplementation.bind(table),
                        });
                    }

                    function resolveNativeImplementation(interfaceId, qualifier, providerSymbol, backend, sourceFile) {
                        const providers = this?.__capybaraNativeProviders;
                        const metadata = { interfaceId, qualifier, providerSymbol, backend, sourceFile };
                        if (!(providers instanceof Map)) {
                            throw nativeProviderError('NotWired: No native provider table is bound for ' + nativeProviderContext(metadata) + '.', metadata);
                        }
                        const key = nativeProviderKey(interfaceId, qualifier);
                        const entry = providers.get(key);
                        if (!entry) {
                            throw nativeProviderError('NotWired: No native provider registered for ' + nativeProviderContext(metadata) + '.', metadata);
                        }
                        if (entry.provider.lifetime === 'singleton') {
                            if (!entry.singletonSet) {
                                entry.singletonValue = createNativeImplementation(entry.provider);
                                entry.singletonSet = true;
                            }
                            return entry.singletonValue;
                        }
                        return createNativeImplementation(entry.provider);
                    }

                    function createNativeImplementation(provider) {
                        let value;
                        try {
                            value = provider.create();
                        } catch (error) {
                            if (error instanceof NativeProviderError) {
                                throw error;
                            }
                            throw nativeProviderError('InvocationFailure: Native provider for ' + nativeProviderContext(provider) + ' failed during construction: ' + (error?.message ?? String(error)), provider);
                        }
                        return validateNativeImplementation(provider.metadata, value);
                    }

                    function validateNativeImplementation(interfaceMetadata, value) {
                        const metadata = {
                            interfaceId: interfaceMetadata?.interfaceId,
                            qualifier: interfaceMetadata?.qualifier,
                            providerSymbol: interfaceMetadata?.providerSymbol,
                            backend: interfaceMetadata?.backend,
                            sourceFile: interfaceMetadata?.sourceFile,
                        };
                        if (value === null || value === undefined) {
                            throw nativeProviderError('TypeMismatch: Native provider for ' + nativeProviderContext(metadata) + ' returned null.', metadata);
                        }
                        const methods = interfaceMetadata?.methods ?? [];
                        for (const method of methods) {
                            const name = method.name;
                            const implementation = value[name];
                            if (implementation === undefined || implementation === null) {
                                throw nativeProviderError('TypeMismatch: Native provider for ' + nativeProviderContext(metadata) + ' is missing method `' + name + '`.', metadata);
                            }
                            if (typeof implementation !== 'function') {
                                throw nativeProviderError('TypeMismatch: Native provider for ' + nativeProviderContext(metadata) + ' method `' + name + '` must be a function.', metadata);
                            }
                            const arity = method.arity ?? 0;
                            if (implementation.length < arity) {
                                throw nativeProviderError('TypeMismatch: Native provider for ' + nativeProviderContext(metadata) + ' method `' + name + '` requires arity at least ' + arity + ', got ' + implementation.length + '.', metadata);
                            }
                            if (implementation.constructor?.name === 'AsyncFunction') {
                                throw nativeProviderError('TypeMismatch: Native provider for ' + nativeProviderContext(metadata) + ' method `' + name + '` is async; async host methods are unsupported in native provider wiring v1.', metadata);
                            }
                        }
                        return nativeImplementationProxy(interfaceMetadata, value);
                    }

                    function nativeImplementationProxy(interfaceMetadata, value) {
                        const methods = new Map((interfaceMetadata?.methods ?? []).map(method => [method.name, method]));
                        const interfaceName = String(interfaceMetadata?.interfaceId ?? '').split(/[/.]/).filter(Boolean).pop();
                        return new Proxy(value, {
                            get(target, property, receiver) {
                                if (property === '__capybaraType') {
                                    return target.__capybaraType ?? interfaceName;
                                }
                                if (property === '__capybaraTypes') {
                                    const existing = Array.isArray(target.__capybaraTypes) ? target.__capybaraTypes : [];
                                    return interfaceName && !existing.includes(interfaceName) ? [...existing, interfaceName] : existing;
                                }
                                if (typeof property === 'string' && methods.has(property)) {
                                    const method = target[property];
                                    return (...args) => {
                                        const result = method.apply(target, args);
                                        if (result && typeof result.then === 'function') {
                                            throw nativeProviderError('InvocationFailure: Native provider for ' + nativeProviderContext(interfaceMetadata) + ' method `' + property + '` returned a Promise; async host methods are unsupported in native provider wiring v1.', interfaceMetadata);
                                        }
                                        return result;
                                    };
                                }
                                return Reflect.get(target, property, receiver);
                            },
                        });
                    }

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
                        pipeStar(mapper) { return optionFlatMap(this, mapper); }
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
                        pipeStar() { return this; },
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
                            this.value = fields.value ?? fields.results;
                            Object.defineProperty(this, 'results', {
                                value: fields.results ?? fields.value,
                                enumerable: false,
                                configurable: true,
                            });
                            return methodAliasProxy(this);
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
                            this.message = fields.message ?? fields.ex;
                            this.ex = this.message;
                            return methodAliasProxy(this);
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
                            return methodAliasProxy(this);
                        }
                        unsafe_run() { return this.thunk(); }
                        unsafeRun() { return this.unsafe_run(); }
                        map(mapper) { return delay(() => mapper(this.unsafe_run())); }
                        pipe(mapper) { return this.map(mapper); }
                        flat_map(mapper) { return delay(() => mapper(this.unsafe_run()).unsafe_run()); }
                        flatMap(mapper) { return this.flat_map(mapper); }
                        pipe_star(mapper) { return this.flat_map(mapper); }
                        pipeStar(mapper) { return this.flat_map(mapper); }
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

                        const nativeArrayMap = Array.prototype.map;
                        const nativeArrayFlatMap = Array.prototype.flatMap;
                        const nativeArrayFilter = Array.prototype.filter;
                        const nativeArrayReduce = Array.prototype.reduce;

                        const arrayMethods = {
                            asList: { value() { return Array.from(this); } },
                            first: { value() { return this.length === 0 ? None : new Some({ value: this[0] }); } },
                            map: { value(mapper) { return list(nativeArrayMap.call(this, mapper)); } },
                            flatMap: { value(mapper) { return flatMapCollection(this, mapper); } },
                            flat_map: { value(mapper) { return flatMapCollection(this, mapper); } },
                            filter: { value(predicate) { return list(nativeArrayFilter.call(this, predicate)); } },
                            reject: { value(predicate) { return rejectCollection(this, predicate); } },
                            pipe: { value(mapper) { return mapCollection(this, mapper); } },
                            pipe_minus: { value(predicate) { return filterCollection(this, predicate); } },
                            pipe_star: { value(mapper) { return flatMapCollection(this, mapper); } },
                            reduce: { value(first, second) { return typeof first === 'function' ? nativeArrayReduce.apply(this, arguments) : reduceCollection(this, first, second); } },
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

                        const mapMethods = {
                            containsKey: { value(key) { return this.has(key); } },
                            contains_key: { value(key) { return this.has(key); } },
                            containsValue: { value(expected) { return Array.from(this.values()).some(value => equals(value, expected)); } },
                            contains_value: { value(expected) { return Array.from(this.values()).some(value => equals(value, expected)); } },
                            isEmpty: { value() { return this.size === 0; } },
                            is_empty: { value() { return this.size === 0; } },
                        };

                        function defineCapyMethods(target, methods) {
                        for (const [name, descriptor] of Object.entries(methods)) {
                            if (!Object.prototype.hasOwnProperty.call(target, name)) {
                                Object.defineProperty(target, name, { ...descriptor, enumerable: false, configurable: true });
                            }
                        }
                            return target;
                        }

                        defineCapyMethods(Map.prototype, mapMethods);

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

                    function enumValue(name, owner, parents = [], ordinal = 0, aliases = [], packageName = '', packagePath = owner, metadata = undefined) {
                        const reflectionFields = metadata && Array.isArray(metadata.fields) ? metadata.fields : [];
                        const reflectionAnnotations = metadata && Array.isArray(metadata.annotations) ? metadata.annotations : [];
                        const value = {
                            __capybaraType: name,
                            __capybaraTypes: [name, ...aliases, owner, ...parents],
                            __capybaraEnum: true,
                            name,
                            ordinal,
                            order: ordinal,
                            toString() { return name; },
                            capybaraDataValueInfo() { return dataValueInfo(this, name, packageName, packagePath, reflectionFields, reflectionAnnotations); },
                        };
                        if (parents.includes('Seq') && name === 'End') {
                            Object.assign(value, {
                                any() { return false; },
                                asList() { return []; },
                                drop() { return this; },
                                dropUntil() { return this; },
                                filter() { return this; },
                                first() { return None; },
                                firstMatch() { return None; },
                                flatMap() { return this; },
                                map() { return this; },
                                pipe() { return this; },
                                pipeStar() { return this; },
                                pipe_star() { return this; },
                                plus(other) { return other; },
                                reduce(initial) { return initial; },
                                reduceLeft(initial) { return initial; },
                                reject() { return this; },
                                take() { return []; },
                                takeLast() { return []; },
                                until() { return this; },
                                zip() { return this; },
                            });
                        }
                        return methodAliasProxy(Object.freeze(value));
                    }

                    function methodAliasProxy(target) {
                        return new Proxy(target, {
                            get(target, property, receiver) {
                                if (property in target) return Reflect.get(target, property, receiver);
                                if (typeof property !== 'string') return undefined;
                                const directCamel = property.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
                                if (directCamel !== property && typeof target[directCamel] === 'function') {
                                    return target[directCamel].bind(target);
                                }
                                const overloadSeparator = property.indexOf('__');
                                if (overloadSeparator <= 0) return undefined;
                                const base = property.slice(0, overloadSeparator);
                                const camelBase = base.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
                                const candidates = [
                                    base,
                                    camelBase,
                                    base === 'greater' ? 'greaterThan' : undefined,
                                    base === 'less' ? 'lessThan' : undefined,
                                ];
                                for (const candidate of candidates) {
                                    if (candidate && typeof target[candidate] === 'function') {
                                        return target[candidate].bind(target);
                                    }
                                }
                                return undefined;
                            },
                        });
                    }

                    function overloadScore(name, args) {
                        if (args.length === 0) return 1;
                        const first = args[0];
                        const lowered = name.toLowerCase();
                        if (lowered.includes('compileddict')) return first instanceof Map ? 100 : -1;
                        if (lowered.includes('compiledset')) return first instanceof Set ? 100 : -1;
                        if (lowered.includes('compiledlist')) return Array.isArray(first) ? 100 : -1;
                        if (lowered.includes('string')) return typeof first === 'string' ? 80 : -1;
                        if (lowered.includes('long')) return typeof first === 'bigint' ? 80 : -1;
                        if (lowered.includes('int') || lowered.includes('float') || lowered.includes('double')) {
                            return typeof first === 'number' ? 70 : -1;
                        }
                        if (first && first.__capybaraType && lowered.includes(first.__capybaraType.toLowerCase())) {
                            return 90;
                        }
                        return 1;
                    }

                    function dispatchOverload(overloads, args) {
                        let selected = overloads[0];
                        let selectedScore = -1;
                        for (const overload of overloads) {
                            const score = overloadScore(overload[0], args);
                            if (score > selectedScore) {
                                selected = overload;
                                selectedScore = score;
                            }
                        }
                        return selected[1](...args);
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
                                const keys = nativeArrayFilter.call(Object.keys(left), key => !key.startsWith('__') && typeof left[key] !== 'function');
                                return keys.length === nativeArrayFilter.call(Object.keys(right), key => !key.startsWith('__') && typeof right[key] !== 'function').length
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
                            return list(nativeArrayFilter.call(valueList, item => !equals(item, value)));
                        }

                        function listMinus(left, right) {
                            return list(nativeArrayFilter.call(left, item => !contains(right, item)));
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
                            return set(nativeArrayFilter.call(Array.from(valueSet), item => !equals(item, value)));
                        }

                        function setMinus(left, right) {
                            return set(nativeArrayFilter.call(Array.from(left), item => !contains(right, item)));
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
                            return set(nativeArrayFilter.call(Array.from(left), item => contains(right, item)));
                    }

                    function setSymmetricDifference(left, right) {
                        return setPlus(setMinus(left, right), setMinus(right, left));
                    }

                        function setCartesianProduct(left, right) {
                            const normalizedLeft = set(left);
                            const normalizedRight = set(right);
                            return set(nativeArrayFlatMap.call(Array.from(normalizedLeft), l => nativeArrayMap.call(Array.from(normalizedRight), r => [l, r])));
                        }

                        function setPowerSet(valueSet) {
                            const values = Array.from(set(valueSet));
                            return set(nativeArrayReduce.call(values,
                                (subsets, item) => subsets.concat(nativeArrayMap.call(subsets, subset => set([...subset, item]))),
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
                        if (value && typeof value.asList === 'function') {
                            return value.asList();
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
                                return new Map(nativeArrayMap.call(Array.from(value.entries()), ([key, item]) => [key, invoke(mapper, key, item)]));
                            }
                            if (value instanceof Set) {
                                return set(nativeArrayMap.call(Array.from(value), (item, index) => invoke(mapper, item, index)));
                        }
                            if (typeof value === 'string') {
                                return list(nativeArrayMap.call(Array.from(value), (item, index) => invoke(mapper, item, index)));
                            }
                            return list(nativeArrayMap.call(value, (item, index) => invoke(mapper, item, index)));
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
                                return new Map(nativeArrayFilter.call(Array.from(value.entries()), ([key, item]) => invoke(predicate, key, item)));
                            }
                            if (value instanceof Set) {
                                return set(nativeArrayFilter.call(Array.from(value), (item, index) => invoke(predicate, item, index)));
                        }
                            if (typeof value === 'string') {
                                return list(nativeArrayFilter.call(Array.from(value), (item, index) => invoke(predicate, item, index)));
                            }
                            return list(nativeArrayFilter.call(value, (item, index) => invoke(predicate, item, index)));
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
                                return new Map(nativeArrayFilter.call(Array.from(value.entries()), ([key, item]) => !invoke(predicate, key, item)));
                            }
                            if (value instanceof Set) {
                                return set(nativeArrayFilter.call(Array.from(value), (item, index) => !invoke(predicate, item, index)));
                        }
                            if (typeof value === 'string') {
                                return list(nativeArrayFilter.call(Array.from(value), (item, index) => !invoke(predicate, item, index)));
                            }
                            return list(nativeArrayFilter.call(value, (item, index) => !invoke(predicate, item, index)));
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
                            const mapped = nativeArrayFlatMap.call(entries(value), (item, index) => {
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
                        if (value && typeof value.any === 'function' && !Array.isArray(value)) {
                            return value.any(predicate);
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

                    function toInt(value) {
                        return value | 0;
                    }

                    function intAdd(left, right) {
                        return toInt(Number(left) + Number(right));
                    }

                    function intSub(left, right) {
                        return toInt(Number(left) - Number(right));
                    }

                    function intMul(left, right) {
                        return Math.imul(left, right);
                    }

                    function intDiv(left, right) {
                        if (right === 0) {
                            throw new RangeError('/ by zero');
                        }
                        return toInt(Math.trunc(left / right));
                    }

                    function intMod(left, right) {
                        if (right === 0) {
                            throw new RangeError('/ by zero');
                        }
                        return toInt(left % right);
                    }

                    function intPow(left, right) {
                        let base = toInt(left);
                        let exponent = Number(right);
                        let result = 1;
                        while (exponent > 0) {
                            if ((exponent & 1) === 1) {
                                result = intMul(result, base);
                            }
                            base = intMul(base, base);
                            exponent >>= 1;
                        }
                        return toInt(result);
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

                        function parseFloatResult(value, typeName = 'float') {
                            return parseResult(value, typeName, text => /^[-+]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][-+]?\\d+)?$/.test(text) ? Number.parseFloat(text) : Number.NaN);
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
                                return '[' + nativeArrayMap.call(value, toStringValue).join(', ') + ']';
                            }
                            if (value instanceof Set) {
                                return '{' + nativeArrayMap.call(Array.from(value), toStringValue).join(', ') + '}';
                            }
                            if (value instanceof Map) {
                                return '{' + nativeArrayMap.call(Array.from(value.entries()), ([key, item]) => `${key}: ${toStringValue(item)}`).join(', ') + '}';
                        }
                        if (typeof value.toString === 'function' && value.toString !== Object.prototype.toString) {
                            return value.toString();
                        }
                        return String(value);
                        }

                        function dataToString(value) {
                            const keys = nativeArrayFilter.call(Object.keys(value), key => !key.startsWith('__') && typeof value[key] !== 'function');
                            if (keys.length === 0) {
                                return `${value.__capybaraType} { }`;
                            }
                            return `${value.__capybaraType} { ` + nativeArrayMap.call(keys, key => `"${key}": ${dataFieldToString(value[key])}`).join(', ') + ' }';
                    }

                    function dataFieldToString(value) {
                        return typeof value === 'string' ? `"${value}"` : toStringValue(value);
                    }

                    function reflectionPackage(packageName, packagePath) {
                        return {
                            name: packageName ?? '',
                            path: String(packagePath ?? '').replace(/^\\/+/u, ''),
                        };
                    }

                    function reflectionFieldDescriptor(field) {
                        if (typeof field === 'string') {
                            return { name: field, type: undefined, annotations: [] };
                        }
                        return {
                            name: field.name,
                            type: field.type,
                            annotations: Array.isArray(field.annotations) ? field.annotations : [],
                        };
                    }

                    function dataValueInfo(value, name, packageName, packagePath, fieldInfos, annotations = []) {
                        const descriptors = Array.isArray(fieldInfos)
                            ? fieldInfos
                            : nativeArrayFilter.call(Object.keys(value), key => !key.startsWith('__') && typeof value[key] !== 'function');
                        const fields = nativeArrayMap.call(descriptors, field => {
                            const descriptor = reflectionFieldDescriptor(field);
                            return {
                                name: descriptor.name,
                                type: descriptor.type,
                                value: value[descriptor.name],
                                annotations: descriptor.annotations,
                            };
                        });
                        const pkg = reflectionPackage(packageName, packagePath);
                        return {
                            name,
                            pkg,
                            packageName: pkg.name,
                            packagePath: pkg.path,
                            fields,
                            annotations: Array.isArray(annotations) ? annotations : [],
                        };
                    }

                    function reflection(target, name, packageName, packagePath, fieldInfos, annotations = []) {
                        if (name) {
                            return dataValueInfo(target, name, packageName, packagePath, fieldInfos, annotations);
                        }
                        if (target && typeof target.capybaraDataValueInfo === 'function') {
                            return target.capybaraDataValueInfo();
                        }
                        return dataValueInfo(target, name, packageName, packagePath, fieldInfos, annotations);
                    }

                    function writeProgramResult(value) {
                        if (isType(value, 'Program') && isType(value, 'Success')) {
                            return;
                        }
                        if (isType(value, 'Program') && isType(value, 'Failed')) {
                            process.exitCode = value.exit_code ?? 1;
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
                        dispatchOverload,
                        newArray,
                        toException,
                        decorateException,
                        arrayGet,
                        applyTrait,
                        enumValue,
                        methodAliasProxy,
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
                        toInt,
                        toLong,
                        intAdd,
                        intSub,
                        intMul,
                        intDiv,
                        intMod,
                        intPow,
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
                        NativeProviderError,
                        nativeProviderError,
                        nativeFactory,
                        defineNativeProviders,
                        resolveNativeImplementation,
                        validateNativeImplementation,
                    };
                    """;
        }
    }

    static Map<String, String> buildFunctionNameOverrides(CompiledProgram program) {
        var overrides = new LinkedHashMap<String, String>();
        var collisions = new LinkedHashMap<String, List<CompiledFunction>>();
        var ownerModuleNames = new java.util.IdentityHashMap<CompiledFunction, String>();
        var primitiveBackedTypeNames = primitiveBackedTypeNames(program);
        var primitiveBackedMethods = new ArrayList<CompiledFunction>();
        for (var module : program.modules()) {
            for (var function : module.functions()) {
                ownerModuleNames.put(function, module.name());
                var primitiveBackedMethod = methodOwnerType(function.name())
                        .filter(primitiveBackedTypeNames::contains)
                        .isPresent();
                if (primitiveBackedMethod) {
                    primitiveBackedMethods.add(function);
                }
                var ownerKey = function.name().startsWith(METHOD_DECL_PREFIX) && !primitiveBackedMethod
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
        for (var function : primitiveBackedMethods) {
            var emittedName = primitiveBackedMethodName(function);
            var parameterTypes = function.parameters().stream().map(CompiledFunction.CompiledFunctionParameter::type).toList();
            overrides.put(signatureKey(function.name(), parameterTypes), emittedName);
            overrides.put(signatureKey(ownerModuleNames.get(function) + "." + function.name(), parameterTypes), emittedName);
        }
        return Map.copyOf(overrides);
    }

    private static String primitiveBackedMethodName(CompiledFunction function) {
        var rawBaseName = baseMethodName(function.name());
        return normalizeJsIdentifier(rawBaseName) + "__" + methodVariantSuffix(rawBaseName) + overloadSuffix(function);
    }

    private static Set<String> primitiveBackedTypeNames(CompiledProgram program) {
        return program.modules().stream()
                .flatMap(module -> module.types().values().stream())
                .filter(CompiledPrimitiveBackedType.class::isInstance)
                .map(CompiledPrimitiveBackedType.class::cast)
                .map(CompiledPrimitiveBackedType::name)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
    }

    private static Optional<String> methodOwnerType(String functionName) {
        if (!functionName.startsWith(METHOD_DECL_PREFIX)) {
            return Optional.empty();
        }
        var separatorIndex = functionName.indexOf("__", METHOD_DECL_PREFIX.length());
        if (separatorIndex < 0) {
            return Optional.empty();
        }
        return Optional.of(functionName.substring(METHOD_DECL_PREFIX.length(), separatorIndex));
    }

        private static Optional<String> findOverrideBySimpleName(Map<String, String> functionNameOverrides, String targetName, String parameterSignature) {
            var simpleMethodName = normalizeJsIdentifier(simpleMethodName(targetName));
            var qualifier = qualifierName(targetName);
            var candidates = functionNameOverrides.entrySet().stream()
                    .filter(entry -> normalizeJsIdentifier(simpleMethodName(keyName(entry.getKey()))).equals(simpleMethodName))
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
                .map(parameter -> sanitizeOverloadSuffix(overloadTypeName(parameter.type())))
                .collect(joining("__"));
        return suffix.isBlank() ? "" : "__" + suffix;
    }

    private static String overloadTypeName(CompiledType type) {
        if (type instanceof CompiledPrimitiveBackedType primitiveBackedType) {
            return primitiveBackedType.name();
        }
        return String.valueOf(type);
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

    static String jsConstIdentifier(String rawName) {
        if (isTopLevelConstName(rawName) && isValidJsIdentifier(rawName) && !JS_KEYWORDS.contains(rawName)) {
            return rawName;
        }
        return normalizeJsIdentifier(rawName);
    }

    private static boolean isTopLevelConstName(String name) {
        return ConstDependencyOrder.isTopLevelConstName(name);
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
