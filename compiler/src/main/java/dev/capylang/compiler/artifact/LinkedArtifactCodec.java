package dev.capylang.compiler.artifact;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.jsontype.BasicPolymorphicTypeValidator;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import dev.capylang.compiler.CollectionLinkedType;
import dev.capylang.compiler.CompiledAnnotationValue;
import dev.capylang.compiler.CompiledDataParentType;
import dev.capylang.compiler.CompiledDataType;
import dev.capylang.compiler.CompiledFunctionType;
import dev.capylang.compiler.CompiledGenericTypeParameter;
import dev.capylang.compiler.CompiledModule;
import dev.capylang.compiler.CompiledObjectType;
import dev.capylang.compiler.CompiledPrimitiveBackedType;
import dev.capylang.compiler.CompiledProgram;
import dev.capylang.compiler.CompiledTupleType;
import dev.capylang.compiler.CompiledType;
import dev.capylang.compiler.GenericDataType;
import dev.capylang.compiler.PrimitiveLinkedType;
import dev.capylang.compiler.expression.CompiledBooleanValue;
import dev.capylang.compiler.expression.CompiledByteValue;
import dev.capylang.compiler.expression.CompiledDoubleValue;
import dev.capylang.compiler.expression.CompiledEffectBindExpression;
import dev.capylang.compiler.expression.CompiledEffectExpression;
import dev.capylang.compiler.expression.CompiledExpression;
import dev.capylang.compiler.expression.CompiledFieldAccess;
import dev.capylang.compiler.expression.CompiledFloatValue;
import dev.capylang.compiler.expression.CompiledFunctionCall;
import dev.capylang.compiler.expression.CompiledFunctionInvoke;
import dev.capylang.compiler.expression.CompiledIfExpression;
import dev.capylang.compiler.expression.CompiledIndexExpression;
import dev.capylang.compiler.expression.CompiledInfixExpression;
import dev.capylang.compiler.expression.CompiledIntValue;
import dev.capylang.compiler.expression.CompiledLambdaExpression;
import dev.capylang.compiler.expression.CompiledLetExpression;
import dev.capylang.compiler.expression.CompiledLongValue;
import dev.capylang.compiler.expression.CompiledMatchExpression;
import dev.capylang.compiler.expression.CompiledNewData;
import dev.capylang.compiler.expression.CompiledNewDict;
import dev.capylang.compiler.expression.CompiledNewList;
import dev.capylang.compiler.expression.CompiledNewSet;
import dev.capylang.compiler.expression.CompiledNothingValue;
import dev.capylang.compiler.expression.CompiledNumericWidening;
import dev.capylang.compiler.expression.CompiledObjectConstruction;
import dev.capylang.compiler.expression.CompiledPipeExpression;
import dev.capylang.compiler.expression.CompiledPipeFilterOutExpression;
import dev.capylang.compiler.expression.CompiledPipeFlatMapExpression;
import dev.capylang.compiler.expression.CompiledPipeReduceExpression;
import dev.capylang.compiler.expression.CompiledReflectionValue;
import dev.capylang.compiler.expression.CompiledSliceExpression;
import dev.capylang.compiler.expression.CompiledStringValue;
import dev.capylang.compiler.expression.CompiledTupleExpression;
import dev.capylang.compiler.expression.CompiledUnwrapExpression;
import dev.capylang.compiler.expression.CompiledVariable;
import dev.capylang.compiler.parser.AnnotationValue;
import dev.capylang.compiler.parser.BooleanValue;
import dev.capylang.compiler.parser.ByteValue;
import dev.capylang.compiler.parser.CollectionType;
import dev.capylang.compiler.parser.ConstructorData;
import dev.capylang.compiler.parser.DataType;
import dev.capylang.compiler.parser.DoubleValue;
import dev.capylang.compiler.parser.Expression;
import dev.capylang.compiler.parser.FieldAccess;
import dev.capylang.compiler.parser.FloatValue;
import dev.capylang.compiler.parser.FunctionCall;
import dev.capylang.compiler.parser.FunctionInvoke;
import dev.capylang.compiler.parser.FunctionReference;
import dev.capylang.compiler.parser.FunctionType;
import dev.capylang.compiler.parser.IfExpression;
import dev.capylang.compiler.parser.IndexExpression;
import dev.capylang.compiler.parser.IntValue;
import dev.capylang.compiler.parser.LambdaExpression;
import dev.capylang.compiler.parser.LetExpression;
import dev.capylang.compiler.parser.LongValue;
import dev.capylang.compiler.parser.MatchExpression;
import dev.capylang.compiler.parser.NewData;
import dev.capylang.compiler.parser.NewDictExpression;
import dev.capylang.compiler.parser.NewListExpression;
import dev.capylang.compiler.parser.NewSetExpression;
import dev.capylang.compiler.parser.NothingValue;
import dev.capylang.compiler.parser.ObjectOriented;
import dev.capylang.compiler.parser.PlaceholderExpression;
import dev.capylang.compiler.parser.PrimitiveType;
import dev.capylang.compiler.parser.ReduceExpression;
import dev.capylang.compiler.parser.SliceExpression;
import dev.capylang.compiler.parser.StringValue;
import dev.capylang.compiler.parser.TupleExpression;
import dev.capylang.compiler.parser.TupleType;
import dev.capylang.compiler.parser.Type;
import dev.capylang.compiler.parser.UnwrapExpression;
import dev.capylang.compiler.parser.Value;
import dev.capylang.compiler.parser.WithExpression;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

public final class LinkedArtifactCodec {
    public static final String PROGRAM_FORMAT = "capybara.linked.program";
    public static final String MODULE_FORMAT = "capybara.linked.module";
    public static final int SCHEMA_VERSION = 1;

    private static final ObjectMapper SCHEMA_MAPPER = createSchemaMapper();
    private static final ObjectMapper LEGACY_MAPPER = createLegacyMapper();
    private static final ObjectWriter PRETTY_SCHEMA_WRITER = SCHEMA_MAPPER.writerWithDefaultPrettyPrinter();

    private LinkedArtifactCodec() {
    }

    public static void writeProgram(OutputStream output, CompiledProgram program) throws IOException {
        PRETTY_SCHEMA_WRITER.writeValue(output, new LinkedProgramArtifact(PROGRAM_FORMAT, SCHEMA_VERSION, program));
    }

    public static void writeModule(OutputStream output, CompiledModule module) throws IOException {
        PRETTY_SCHEMA_WRITER.writeValue(output, new LinkedModuleArtifact(MODULE_FORMAT, SCHEMA_VERSION, module));
    }

    public static String writeProgramAsString(CompiledProgram program) throws IOException {
        return PRETTY_SCHEMA_WRITER.writeValueAsString(new LinkedProgramArtifact(PROGRAM_FORMAT, SCHEMA_VERSION, program));
    }

    public static String writeModuleAsString(CompiledModule module) throws IOException {
        return PRETTY_SCHEMA_WRITER.writeValueAsString(new LinkedModuleArtifact(MODULE_FORMAT, SCHEMA_VERSION, module));
    }

    public static CompiledProgram readProgram(InputStream input) throws IOException {
        var tree = SCHEMA_MAPPER.readTree(input);
        if (isSchemaArtifact(tree)) {
            validateArtifact(PROGRAM_FORMAT, tree);
            return SCHEMA_MAPPER.treeToValue(requiredPayload(tree, "program", PROGRAM_FORMAT), CompiledProgram.class);
        }
        return LEGACY_MAPPER.treeToValue(tree, CompiledProgram.class);
    }

    public static CompiledModule readModule(InputStream input) throws IOException {
        var tree = SCHEMA_MAPPER.readTree(input);
        if (isSchemaArtifact(tree)) {
            validateArtifact(MODULE_FORMAT, tree);
            return SCHEMA_MAPPER.treeToValue(requiredPayload(tree, "module", MODULE_FORMAT), CompiledModule.class);
        }
        return LEGACY_MAPPER.treeToValue(tree, CompiledModule.class);
    }

    private static boolean isSchemaArtifact(JsonNode tree) {
        return tree != null && tree.isObject() && tree.has("format");
    }

    private static void validateArtifact(String expectedFormat, JsonNode tree) {
        var format = textValue(tree, "format");
        if (!expectedFormat.equals(format)) {
            throw new LinkedArtifactSchemaException(
                    "Unsupported linked artifact format `" + format + "`. Expected `" + expectedFormat + "`."
            );
        }
        var version = tree.get("schemaVersion");
        if (version == null || !version.canConvertToInt()) {
            throw new LinkedArtifactSchemaException(
                    "Linked artifact `" + expectedFormat + "` is missing integer schemaVersion."
            );
        }
        if (version.intValue() != SCHEMA_VERSION) {
            throw new LinkedArtifactSchemaException(
                    "Unsupported linked artifact schema version `" + version.intValue()
                    + "` for `" + expectedFormat + "`. Supported versions: " + SCHEMA_VERSION + "."
            );
        }
    }

    private static JsonNode requiredPayload(JsonNode tree, String field, String format) {
        var payload = tree.get(field);
        if (payload == null || payload.isNull()) {
            throw new LinkedArtifactSchemaException(
                    "Linked artifact `" + format + "` is missing required payload field `" + field + "`."
            );
        }
        return payload;
    }

    private static String textValue(JsonNode tree, String field) {
        var value = tree.get(field);
        return value == null || value.isNull() ? "" : value.asText();
    }

    private static ObjectMapper createSchemaMapper() {
        var mapper = JsonMapper.builder()
                .addModule(new Jdk8Module())
                .configure(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS, true)
                .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                .build();
        mapper.addMixIn(CompiledType.class, CompiledTypeMixin.class);
        mapper.addMixIn(GenericDataType.class, GenericDataTypeMixin.class);
        mapper.addMixIn(CollectionLinkedType.class, CollectionLinkedTypeMixin.class);
        mapper.addMixIn(CompiledExpression.class, CompiledExpressionMixin.class);
        mapper.addMixIn(CompiledMatchExpression.Pattern.class, CompiledExpressionPatternMixin.class);
        mapper.addMixIn(CompiledAnnotationValue.class, CompiledAnnotationValueMixin.class);
        mapper.addMixIn(Type.class, ParserTypeMixin.class);
        mapper.addMixIn(CollectionType.class, ParserCollectionTypeMixin.class);
        mapper.addMixIn(Expression.class, ParserExpressionMixin.class);
        mapper.addMixIn(MatchExpression.Pattern.class, ParserExpressionPatternMixin.class);
        mapper.addMixIn(AnnotationValue.class, ParserAnnotationValueMixin.class);
        mapper.addMixIn(ObjectOriented.TypeDeclaration.class, ObjectTypeDeclarationMixin.class);
        mapper.addMixIn(ObjectOriented.MemberDeclaration.class, ObjectMemberDeclarationMixin.class);
        mapper.addMixIn(ObjectOriented.MethodBody.class, ObjectMethodBodyMixin.class);
        mapper.addMixIn(ObjectOriented.Statement.class, ObjectStatementMixin.class);
        return mapper;
    }

    private static ObjectMapper createLegacyMapper() {
        var mapper = JsonMapper.builder()
                .addModule(new Jdk8Module())
                .build();
        mapper.activateDefaultTyping(
                BasicPolymorphicTypeValidator.builder()
                        .allowIfSubType("dev.capylang")
                        .allowIfSubType("java.util.")
                        .build(),
                ObjectMapper.DefaultTyping.NON_FINAL,
                JsonTypeInfo.As.PROPERTY
        );
        return mapper;
    }

    public static final class LinkedArtifactSchemaException extends IllegalArgumentException {
        public LinkedArtifactSchemaException(String message) {
            super(message);
        }
    }

    private record LinkedProgramArtifact(String format, int schemaVersion, CompiledProgram program) {
    }

    private record LinkedModuleArtifact(String format, int schemaVersion, CompiledModule module) {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = PrimitiveLinkedType.class, name = "primitive"),
            @JsonSubTypes.Type(value = CollectionLinkedType.CompiledList.class, name = "list"),
            @JsonSubTypes.Type(value = CollectionLinkedType.CompiledSet.class, name = "set"),
            @JsonSubTypes.Type(value = CollectionLinkedType.CompiledDict.class, name = "dict"),
            @JsonSubTypes.Type(value = CompiledTupleType.class, name = "tuple"),
            @JsonSubTypes.Type(value = CompiledFunctionType.class, name = "function"),
            @JsonSubTypes.Type(value = CompiledGenericTypeParameter.class, name = "generic-parameter"),
            @JsonSubTypes.Type(value = CompiledDataType.class, name = "data"),
            @JsonSubTypes.Type(value = CompiledDataParentType.class, name = "data-parent"),
            @JsonSubTypes.Type(value = CompiledPrimitiveBackedType.class, name = "primitive-backed"),
            @JsonSubTypes.Type(value = CompiledObjectType.class, name = "object")
    })
    private interface CompiledTypeMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = CompiledDataType.class, name = "data"),
            @JsonSubTypes.Type(value = CompiledDataParentType.class, name = "data-parent"),
            @JsonSubTypes.Type(value = CompiledPrimitiveBackedType.class, name = "primitive-backed"),
            @JsonSubTypes.Type(value = CompiledObjectType.class, name = "object")
    })
    private interface GenericDataTypeMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = CollectionLinkedType.CompiledList.class, name = "list"),
            @JsonSubTypes.Type(value = CollectionLinkedType.CompiledSet.class, name = "set"),
            @JsonSubTypes.Type(value = CollectionLinkedType.CompiledDict.class, name = "dict")
    })
    private interface CollectionLinkedTypeMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = CompiledBooleanValue.class, name = "boolean"),
            @JsonSubTypes.Type(value = CompiledByteValue.class, name = "byte"),
            @JsonSubTypes.Type(value = CompiledDoubleValue.class, name = "double"),
            @JsonSubTypes.Type(value = CompiledEffectBindExpression.class, name = "effect-bind"),
            @JsonSubTypes.Type(value = CompiledEffectExpression.class, name = "effect"),
            @JsonSubTypes.Type(value = CompiledFieldAccess.class, name = "field-access"),
            @JsonSubTypes.Type(value = CompiledFloatValue.class, name = "float"),
            @JsonSubTypes.Type(value = CompiledFunctionCall.class, name = "function-call"),
            @JsonSubTypes.Type(value = CompiledFunctionInvoke.class, name = "function-invoke"),
            @JsonSubTypes.Type(value = CompiledIfExpression.class, name = "if"),
            @JsonSubTypes.Type(value = CompiledIndexExpression.class, name = "index"),
            @JsonSubTypes.Type(value = CompiledInfixExpression.class, name = "infix"),
            @JsonSubTypes.Type(value = CompiledIntValue.class, name = "int"),
            @JsonSubTypes.Type(value = CompiledLambdaExpression.class, name = "lambda"),
            @JsonSubTypes.Type(value = CompiledLetExpression.class, name = "let"),
            @JsonSubTypes.Type(value = CompiledLongValue.class, name = "long"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.class, name = "match"),
            @JsonSubTypes.Type(value = CompiledNothingValue.class, name = "nothing"),
            @JsonSubTypes.Type(value = CompiledNumericWidening.class, name = "numeric-widening"),
            @JsonSubTypes.Type(value = CompiledObjectConstruction.class, name = "object-construction"),
            @JsonSubTypes.Type(value = CompiledPipeExpression.class, name = "pipe-map"),
            @JsonSubTypes.Type(value = CompiledPipeFilterOutExpression.class, name = "pipe-filter-out"),
            @JsonSubTypes.Type(value = CompiledPipeFlatMapExpression.class, name = "pipe-flat-map"),
            @JsonSubTypes.Type(value = CompiledPipeReduceExpression.class, name = "pipe-reduce"),
            @JsonSubTypes.Type(value = CompiledReflectionValue.class, name = "reflection"),
            @JsonSubTypes.Type(value = CompiledSliceExpression.class, name = "slice"),
            @JsonSubTypes.Type(value = CompiledStringValue.class, name = "string"),
            @JsonSubTypes.Type(value = CompiledTupleExpression.class, name = "tuple"),
            @JsonSubTypes.Type(value = CompiledUnwrapExpression.class, name = "unwrap"),
            @JsonSubTypes.Type(value = CompiledNewData.class, name = "new-data"),
            @JsonSubTypes.Type(value = CompiledNewDict.class, name = "new-dict"),
            @JsonSubTypes.Type(value = CompiledNewList.class, name = "new-list"),
            @JsonSubTypes.Type(value = CompiledNewSet.class, name = "new-set"),
            @JsonSubTypes.Type(value = CompiledVariable.class, name = "variable")
    })
    private interface CompiledExpressionMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = CompiledMatchExpression.IntPattern.class, name = "int"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.LongPattern.class, name = "long"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.StringPattern.class, name = "string"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.BoolPattern.class, name = "bool"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.FloatPattern.class, name = "float"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.TypedPattern.class, name = "typed"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.VariablePattern.class, name = "variable"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.WildcardPattern.class, name = "wildcard"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.WildcardBindingPattern.class, name = "wildcard-binding"),
            @JsonSubTypes.Type(value = CompiledMatchExpression.ConstructorPattern.class, name = "constructor")
    })
    private interface CompiledExpressionPatternMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = CompiledAnnotationValue.StringValue.class, name = "string"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.IntValue.class, name = "int"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.LongValue.class, name = "long"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.FloatValue.class, name = "float"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.DoubleValue.class, name = "double"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.BoolValue.class, name = "bool"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.NothingValue.class, name = "nothing"),
            @JsonSubTypes.Type(value = CompiledAnnotationValue.TypeNameValue.class, name = "type-name")
    })
    private interface CompiledAnnotationValueMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = DataType.class, name = "data"),
            @JsonSubTypes.Type(value = PrimitiveType.class, name = "primitive"),
            @JsonSubTypes.Type(value = CollectionType.ListType.class, name = "list"),
            @JsonSubTypes.Type(value = CollectionType.SetType.class, name = "set"),
            @JsonSubTypes.Type(value = CollectionType.DictType.class, name = "dict"),
            @JsonSubTypes.Type(value = FunctionType.class, name = "function"),
            @JsonSubTypes.Type(value = TupleType.class, name = "tuple")
    })
    private interface ParserTypeMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = CollectionType.ListType.class, name = "list"),
            @JsonSubTypes.Type(value = CollectionType.SetType.class, name = "set"),
            @JsonSubTypes.Type(value = CollectionType.DictType.class, name = "dict")
    })
    private interface ParserCollectionTypeMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = BooleanValue.class, name = "boolean"),
            @JsonSubTypes.Type(value = ByteValue.class, name = "byte"),
            @JsonSubTypes.Type(value = ConstructorData.class, name = "constructor-data"),
            @JsonSubTypes.Type(value = DoubleValue.class, name = "double"),
            @JsonSubTypes.Type(value = FieldAccess.class, name = "field-access"),
            @JsonSubTypes.Type(value = FloatValue.class, name = "float"),
            @JsonSubTypes.Type(value = FunctionCall.class, name = "function-call"),
            @JsonSubTypes.Type(value = FunctionInvoke.class, name = "function-invoke"),
            @JsonSubTypes.Type(value = FunctionReference.class, name = "function-reference"),
            @JsonSubTypes.Type(value = IfExpression.class, name = "if"),
            @JsonSubTypes.Type(value = IndexExpression.class, name = "index"),
            @JsonSubTypes.Type(value = IntValue.class, name = "int"),
            @JsonSubTypes.Type(value = LambdaExpression.class, name = "lambda"),
            @JsonSubTypes.Type(value = LetExpression.class, name = "let"),
            @JsonSubTypes.Type(value = LongValue.class, name = "long"),
            @JsonSubTypes.Type(value = MatchExpression.class, name = "match"),
            @JsonSubTypes.Type(value = NewData.class, name = "new-data"),
            @JsonSubTypes.Type(value = NewDictExpression.class, name = "new-dict"),
            @JsonSubTypes.Type(value = NewListExpression.class, name = "new-list"),
            @JsonSubTypes.Type(value = NewSetExpression.class, name = "new-set"),
            @JsonSubTypes.Type(value = NothingValue.class, name = "nothing"),
            @JsonSubTypes.Type(value = PlaceholderExpression.class, name = "placeholder"),
            @JsonSubTypes.Type(value = ReduceExpression.class, name = "reduce"),
            @JsonSubTypes.Type(value = SliceExpression.class, name = "slice"),
            @JsonSubTypes.Type(value = StringValue.class, name = "string"),
            @JsonSubTypes.Type(value = TupleExpression.class, name = "tuple"),
            @JsonSubTypes.Type(value = UnwrapExpression.class, name = "unwrap"),
            @JsonSubTypes.Type(value = Value.class, name = "value"),
            @JsonSubTypes.Type(value = WithExpression.class, name = "with")
    })
    private interface ParserExpressionMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = MatchExpression.IntPattern.class, name = "int"),
            @JsonSubTypes.Type(value = MatchExpression.LongPattern.class, name = "long"),
            @JsonSubTypes.Type(value = MatchExpression.StringPattern.class, name = "string"),
            @JsonSubTypes.Type(value = MatchExpression.BoolPattern.class, name = "bool"),
            @JsonSubTypes.Type(value = MatchExpression.FloatPattern.class, name = "float"),
            @JsonSubTypes.Type(value = MatchExpression.TypedPattern.class, name = "typed"),
            @JsonSubTypes.Type(value = MatchExpression.VariablePattern.class, name = "variable"),
            @JsonSubTypes.Type(value = MatchExpression.WildcardPattern.class, name = "wildcard"),
            @JsonSubTypes.Type(value = MatchExpression.WildcardBindingPattern.class, name = "wildcard-binding"),
            @JsonSubTypes.Type(value = MatchExpression.ConstructorPattern.class, name = "constructor")
    })
    private interface ParserExpressionPatternMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = AnnotationValue.StringValue.class, name = "string"),
            @JsonSubTypes.Type(value = AnnotationValue.IntValue.class, name = "int"),
            @JsonSubTypes.Type(value = AnnotationValue.LongValue.class, name = "long"),
            @JsonSubTypes.Type(value = AnnotationValue.FloatValue.class, name = "float"),
            @JsonSubTypes.Type(value = AnnotationValue.DoubleValue.class, name = "double"),
            @JsonSubTypes.Type(value = AnnotationValue.BoolValue.class, name = "bool"),
            @JsonSubTypes.Type(value = AnnotationValue.NothingValue.class, name = "nothing"),
            @JsonSubTypes.Type(value = AnnotationValue.TypeNameValue.class, name = "type-name")
    })
    private interface ParserAnnotationValueMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = ObjectOriented.ClassDeclaration.class, name = "class"),
            @JsonSubTypes.Type(value = ObjectOriented.TraitDeclaration.class, name = "trait"),
            @JsonSubTypes.Type(value = ObjectOriented.InterfaceDeclaration.class, name = "interface")
    })
    private interface ObjectTypeDeclarationMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = ObjectOriented.FieldDeclaration.class, name = "field"),
            @JsonSubTypes.Type(value = ObjectOriented.MethodDeclaration.class, name = "method"),
            @JsonSubTypes.Type(value = ObjectOriented.InitBlock.class, name = "init")
    })
    private interface ObjectMemberDeclarationMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = ObjectOriented.ExpressionBody.class, name = "expression"),
            @JsonSubTypes.Type(value = ObjectOriented.StatementBlock.class, name = "statement-block")
    })
    private interface ObjectMethodBodyMixin {
    }

    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = ObjectOriented.LetStatement.class, name = "let"),
            @JsonSubTypes.Type(value = ObjectOriented.LocalMethodStatement.class, name = "local-method"),
            @JsonSubTypes.Type(value = ObjectOriented.MutableVariableStatement.class, name = "mutable-variable"),
            @JsonSubTypes.Type(value = ObjectOriented.AssignmentStatement.class, name = "assignment"),
            @JsonSubTypes.Type(value = ObjectOriented.ExpressionStatement.class, name = "expression"),
            @JsonSubTypes.Type(value = ObjectOriented.ThrowStatement.class, name = "throw"),
            @JsonSubTypes.Type(value = ObjectOriented.ReturnStatement.class, name = "return"),
            @JsonSubTypes.Type(value = ObjectOriented.IfStatement.class, name = "if"),
            @JsonSubTypes.Type(value = ObjectOriented.TryCatchStatement.class, name = "try-catch"),
            @JsonSubTypes.Type(value = ObjectOriented.WhileStatement.class, name = "while"),
            @JsonSubTypes.Type(value = ObjectOriented.DoWhileStatement.class, name = "do-while"),
            @JsonSubTypes.Type(value = ObjectOriented.ForEachStatement.class, name = "for-each"),
            @JsonSubTypes.Type(value = ObjectOriented.StatementBlock.class, name = "block")
    })
    private interface ObjectStatementMixin {
    }
}
