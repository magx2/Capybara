package pl.grzeslowski.capybara.generator.java;

import pl.grzeslowski.capybara.generator.java.JavaInterface.JavaInterfaceMethod;
import pl.grzeslowski.capybara.linker.*;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedDict;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedList;
import pl.grzeslowski.capybara.linker.CollectionLinkedType.LinkedSet;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.*;
import static pl.grzeslowski.capybara.generator.java.JavaAnnotation.generatedAnnotation;

public class JavaAstBuilder {
    private static final String METHOD_DECL_PREFIX = "__method__";
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class",
            "const", "continue", "default", "do", "double", "else", "enum", "extends", "final",
            "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int",
            "interface", "long", "native", "new", "package", "private", "protected", "public",
            "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
            "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false",
            "null", "record", "sealed", "permits", "var", "yield"
    );

    public JavaClass build(LinkedModule module) {
        var interfaces = buildInterfaces(module.types()
                .values()
                .stream()
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .toList());
        var subClassToInterface = findSubClassToInterface(module.types(), interfaces);
        var dataTypes = module.types()
                .values()
                .stream()
                .filter(LinkedDataType.class::isInstance)
                .map(LinkedDataType.class::cast)
                .toList();
        return new JavaClass(
                Set.of(generatedAnnotation()),
                buildClassName(module.name()),
                new JavaPackage(module.path()
                        // Linux and macOS
                        .replaceAll("/", ".")
                        // windows
                        .replaceAll("\\\\", ".")),
                module.staticImports().stream()
                        .map(staticImport -> staticImport.className() + "." + buildJavaStaticImportMember(staticImport.memberName()))
                        .collect(toSet()),
                buildStaticMethods(module.functions()),
                interfaces,
                buildRecords(dataTypes, subClassToInterface, module.functions()),
                buildEnums(dataTypes, subClassToInterface));
    }

    private Map<LinkedDataType, Set<JavaInterface>> findSubClassToInterface(Map<String, GenericDataType> types, Set<JavaInterface> interfaces) {
        var genericDataTypeToJavaInterface = interfaces.stream()
                .collect(toMap(
                        jInterface -> types.get(jInterface.name().name()),
                        identity()));

        record ClassToInterface(LinkedDataType data, LinkedDataParentType parent) {
        }
        record ClassToJavaInterface(LinkedDataType data, JavaInterface parent) {
        }

        return types.values()
                .stream()
                .filter(LinkedDataParentType.class::isInstance)
                .map(LinkedDataParentType.class::cast)
                .flatMap(parent -> parent.subTypes().stream().map(data -> new ClassToInterface(data, parent)))
                .map(pair -> new ClassToJavaInterface(pair.data, genericDataTypeToJavaInterface.get(pair.parent)))
                .collect(groupingBy(
                        ClassToJavaInterface::data,
                        mapping(ClassToJavaInterface::parent, toSet())
                ));
    }

    private static JavaType buildClassName(String name) {
        return new JavaType(normalizeJavaIdentifier(name, true));
    }

    private Set<JavaMethod> buildStaticMethods(Set<LinkedFunction> functions) {
        return functions.stream()
                .filter(function -> !function.name().startsWith(METHOD_DECL_PREFIX))
                .map(this::buildStaticMethod)
                .collect(toSet());
    }

    private JavaMethod buildStaticMethod(LinkedFunction function) {
        return new JavaMethod(
                buildMethodName(function.name()),
                buildJavaType(function.returnType()),
                buildJavaFunctionParameters(function.parameters()),
                function.expression(),
                function.comments()
        );
    }

    private String buildMethodName(String name) {
        return normalizeJavaIdentifier(name, false);
    }

    private String buildJavaStaticImportMember(String memberName) {
        if ("*".equals(memberName)) {
            return memberName;
        }
        if (!memberName.isEmpty() && Character.isUpperCase(memberName.charAt(0))) {
            return buildClassName(memberName).toString();
        }
        return buildMethodName(memberName);
    }

    private JavaType buildJavaType(LinkedType type) {
        return switch (type) {
            case GenericDataType genericDataType -> buildGenericDataType(genericDataType);
            case PrimitiveLinkedType primitiveLinkedType -> buildPrimitiveLinkedType(primitiveLinkedType);
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType);
            case LinkedFunctionType functionType -> new JavaType(
                    "java.util.function.Function<"
                    + buildJavaBoxedType(functionType.argumentType())
                    + ", "
                    + buildJavaBoxedType(functionType.returnType())
                    + ">"
            );
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> new JavaType(linkedGenericTypeParameter.name());
        };
    }

    private JavaType buildGenericDataType(GenericDataType type) {
        if (type.name().contains("/") && type.name().contains(".")) {
            var dotIndex = type.name().lastIndexOf('.');
            var slashIndex = type.name().lastIndexOf('/');
            if (slashIndex > 0 && slashIndex < dotIndex) {
                var startIdx = type.name().startsWith("/") ? 1 : 0;
                var modulePath = type.name().substring(startIdx, slashIndex).replace('/', '.');
                var moduleName = buildClassName(type.name().substring(slashIndex + 1, dotIndex));
                var nestedType = buildClassName(type.name().substring(dotIndex + 1));
                return new JavaType(modulePath + "." + moduleName + "." + nestedType);
            }
        }
        if (type.name().contains(".")) {
            var qualifiedName = Stream.of(type.name().split("\\."))
                    .map(part -> normalizeJavaIdentifier(part, true))
                    .collect(joining("."));
            return new JavaType(qualifiedName);
        }
        return buildClassName(type.name());
    }

    private JavaType buildPrimitiveLinkedType(PrimitiveLinkedType type) {
        return switch (type) {
            case BYTE -> new JavaType("byte");
            case INT -> new JavaType("int");
            case LONG -> new JavaType("long");
            case DOUBLE -> new JavaType("double");
            case STRING -> new JavaType("java.lang.String");
            case BOOL -> new JavaType("boolean");
            case FLOAT -> new JavaType("float");
            case NOTHING -> new JavaType("java.lang.Object");
            case ANY -> new JavaType("java.lang.Object");
        };
    }

    private JavaType buildCollectionLinkedType(CollectionLinkedType type) {
        return switch (type) {
            case LinkedList linkedList -> new JavaType("java.util.List<" + buildJavaBoxedType(linkedList.elementType()) + ">");
            case LinkedDict linkedDict -> new JavaType("java.util.Map<java.lang.String, " + buildJavaBoxedType(linkedDict.valueType()) + ">");
            case LinkedSet linkedSet -> new JavaType("java.util.Set<" + buildJavaBoxedType(linkedSet.elementType()) + ">");
        };
    }

    private String buildJavaBoxedType(LinkedType type) {
        return switch (type) {
            case PrimitiveLinkedType primitiveLinkedType -> switch (primitiveLinkedType) {
                case BYTE -> "java.lang.Byte";
                case INT -> "java.lang.Integer";
                case LONG -> "java.lang.Long";
                case DOUBLE -> "java.lang.Double";
                case STRING -> "java.lang.String";
                case BOOL -> "java.lang.Boolean";
                case FLOAT -> "java.lang.Float";
                case NOTHING -> "java.lang.Object";
                case ANY -> "java.lang.Object";
            };
            case GenericDataType genericDataType -> buildClassName(genericDataType.name()).toString();
            case CollectionLinkedType collectionLinkedType -> buildCollectionLinkedType(collectionLinkedType).toString();
            case LinkedFunctionType functionType -> "java.util.function.Function<"
                    + buildJavaBoxedType(functionType.argumentType())
                    + ", "
                    + buildJavaBoxedType(functionType.returnType())
                    + ">";
            case LinkedGenericTypeParameter linkedGenericTypeParameter -> linkedGenericTypeParameter.name();
        };
    }

    private List<JavaMethod.JavaFunctionParameter> buildJavaFunctionParameters(List<LinkedFunction.LinkedFunctionParameter> parameters) {
        return parameters.stream().map(this::buildJavaFunctionParameter).toList();
    }

    private JavaMethod.JavaFunctionParameter buildJavaFunctionParameter(LinkedFunction.LinkedFunctionParameter parameter) {
        return new JavaMethod.JavaFunctionParameter(
                buildJavaType(parameter.type()),
                parameter.name(),
                buildJavaFunctionParameterName(parameter.name())
        );
    }

    private String buildJavaFunctionParameterName(String name) {
        return normalizeJavaVariableName(name);
    }

    private String normalizeJavaVariableName(String name) {
        var parts = Stream.of(name.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();

        var result = new StringBuilder();
        var first = true;
        for (var part : parts) {
            if (first) {
                result.append(Character.toLowerCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
                first = false;
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        if (result.isEmpty()) {
            result.append("value");
        }
        if (!Character.isJavaIdentifierStart(result.charAt(0))) {
            result.insert(0, 'v');
        }
        var identifier = result.toString();
        if (JAVA_KEYWORDS.contains(identifier)) {
            return identifier + "_";
        }
        return identifier;
    }

    private static String normalizeJavaIdentifier(String name, boolean upperCamel) {
        var parts = Stream.of(name.split("[^A-Za-z0-9]+"))
                .filter(part -> !part.isEmpty())
                .toList();

        var base = new StringBuilder();
        if (parts.isEmpty()) {
            base.append(encodeSymbolicIdentifier(name, upperCamel));
        } else {
            for (var i = 0; i < parts.size(); i++) {
                var part = parts.get(i);
                if (i == 0 && !upperCamel) {
                    base.append(Character.toLowerCase(part.charAt(0)));
                } else {
                    base.append(Character.toUpperCase(part.charAt(0)));
                }
                if (part.length() > 1) {
                    base.append(part.substring(1));
                }
            }
        }

        var identifier = base.toString();
        if (!Character.isJavaIdentifierStart(identifier.charAt(0))) {
            identifier = (upperCamel ? "T" : "v") + identifier;
        }
        if (JAVA_KEYWORDS.contains(identifier)) {
            identifier = identifier + "_";
        }
        return identifier;
    }

    private static String encodeSymbolicIdentifier(String raw, boolean upperCamel) {
        var parts = new java.util.ArrayList<String>(raw.length());
        for (var i = 0; i < raw.length(); i++) {
            parts.add(symbolName(raw.charAt(i)));
        }
        if (parts.isEmpty()) {
            return upperCamel ? "Generated" : "generated";
        }
        var result = new StringBuilder();
        for (var i = 0; i < parts.size(); i++) {
            var part = parts.get(i);
            if (i == 0 && !upperCamel) {
                result.append(part);
            } else {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1));
                }
            }
        }
        return result.toString();
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
            default -> "op" + Integer.toHexString(symbol);
        };
    }

    private Set<JavaInterface> buildInterfaces(List<LinkedDataParentType> dataParentTypes) {
        return dataParentTypes.stream()
                .map(this::buildInterface)
                .collect(toSet());
    }

    private JavaInterface buildInterface(LinkedDataParentType type) {
        return new JavaSealedInterface(
                buildClassName(type.name()),
                buildJavaMethods(type.fields()),
                type.subTypes().stream().map(LinkedDataType::name).map(name -> buildClassName(name).toString()).toList(),
                type.typeParameters()
        );
    }

    private List<JavaInterfaceMethod> buildJavaMethods(List<LinkedDataType.LinkedField> fields) {
        return fields.stream().map(this::buildJavaMethod).toList();
    }

    private JavaInterfaceMethod buildJavaMethod(LinkedDataType.LinkedField field) {
        return new JavaInterfaceMethod(
                buildMethodName(field.name()),
                buildJavaType(field.type()));
    }

    private Set<JavaRecord> buildRecords(
            List<LinkedDataType> dataTypes,
            Map<LinkedDataType, Set<JavaInterface>> subClassToInterface,
            Set<LinkedFunction> functions
    ) {
        return dataTypes.stream()
                .filter(dt -> !dt.singleton())
                .map(dt -> buildRecord(dt, subClassToInterface, functions))
                .collect(toSet());
    }

    private JavaRecord buildRecord(
            LinkedDataType type,
            Map<LinkedDataType, Set<JavaInterface>> subClassToInterface,
            Set<LinkedFunction> functions
    ) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? Set.<JavaType>of()
                : javaInterface.stream().map(JavaInterface::name).collect(toSet());

        var interfaceFields = javaInterface == null
                ? Stream.<JavaRecord.JavaRecordField>empty()
                : javaInterface.stream()
                        .map(JavaInterface::methods)
                        .flatMap(List::stream)
                        .map(method -> new JavaRecord.JavaRecordField(
                                method.name(),
                                method.returnType()));
        var recordFields = type.fields()
                .stream()
                .map(field -> new JavaRecord.JavaRecordField(
                        field.name(),
                        buildJavaType(field.type())));
        var fields = Stream.concat(interfaceFields, recordFields).toList();
        return new JavaRecord(
                buildClassName(type.name()),
                implementInterfaces,
                fields,
                type.typeParameters(),
                Set.of(),
                buildRecordMethods(type, functions));
    }

    private Set<JavaMethod> buildRecordMethods(LinkedDataType type, Set<LinkedFunction> functions) {
        var ownerPrefix = METHOD_DECL_PREFIX + type.name() + "__";
        return functions.stream()
                .filter(function -> function.name().startsWith(ownerPrefix))
                .map(function -> buildRecordMethod(function, ownerPrefix))
                .collect(toSet());
    }

    private JavaMethod buildRecordMethod(LinkedFunction function, String ownerPrefix) {
        var methodName = function.name().substring(ownerPrefix.length());
        var parameters = function.parameters().stream().skip(1).toList();
        return new JavaMethod(
                buildMethodName(methodName),
                buildJavaType(function.returnType()),
                buildJavaFunctionParameters(parameters),
                function.expression(),
                function.comments()
        );
    }

    private Set<JavaEnum> buildEnums(List<LinkedDataType> dataTypes, Map<LinkedDataType, Set<JavaInterface>> subClassToInterface) {
        return dataTypes.stream()
                .filter(LinkedDataType::singleton)
                .map(dt -> buildEnum(dt, subClassToInterface))
                .collect(toSet());
    }

    private JavaEnum buildEnum(LinkedDataType type, Map<LinkedDataType, Set<JavaInterface>> subClassToInterface) {
        var javaInterface = subClassToInterface.get(type);
        var implementInterfaces = javaInterface == null
                ? Set.<JavaType>of()
                : javaInterface.stream().map(JavaInterface::name).collect(toSet());
        return new JavaEnum(buildClassName(type.name()), implementInterfaces);
    }
}
