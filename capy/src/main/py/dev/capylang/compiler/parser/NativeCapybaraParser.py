import os
import re
from FunctionalLexer import FunctionalLexer
from FunctionalParser import FunctionalParser
from ObjectOrientedLexer import ObjectOrientedLexer
from ObjectOrientedParser import ObjectOrientedParser
from antlr4 import CommonTokenStream, InputStream, Token
from antlr4.error.ErrorListener import ErrorListener
from dev.capylang.capybara import NativeImplementation

MODULE_NAME_PATTERN = r"[A-Za-z_][A-Za-z0-9_]*|/[A-Za-z_][a-zA-Z0-9_]*(?:/[A-Za-z_][a-zA-Z0-9_]*)+"
FROM_IMPORT_PATTERN = re.compile(
    rf"^\s*from\s+({MODULE_NAME_PATTERN})\s+import\s*\{{\s*([^}}]*)\s*}}(?:\s+except\s*\{{\s*([^}}]*)\s*}})?\s*$"
)
QUALIFIED_IMPORT_PATTERN = re.compile(rf"^\s*import\s+({MODULE_NAME_PATTERN})\s*$")


def data(type_name, **fields):
    result = dict(fields)
    result["__type"] = type_name
    return result


def option_some(value):
    return data("Some", value=value)


def option_none():
    return data("None")


def loc(line, column):
    return data("SourceLocation", line=line, column=column)


def type_ref(name, arguments=None):
    return data("TypeReference", name=name, arguments=list(arguments or []))


def missing_type():
    return type_ref("", [])


def string_type():
    return type_ref("String", [])


def unsupported_expr(source, location):
    return data("UnsupportedExpression", source=source, location=location)


def unsupported_literal():
    return unsupported_expr("", loc(0, 0))


def text(node):
    return "" if node is None else node.getText()


def ctx_call(ctx, name, *args):
    if ctx is None:
        return None
    attr = getattr(ctx, name, None)
    if attr is None:
        attr = getattr(ctx, name + "_", None)
    if attr is None:
        return None
    return attr(*args) if callable(attr) else attr


def ctx_list(ctx, name):
    value = ctx_call(ctx, name)
    if value is None:
        return []
    if isinstance(value, list):
        return value
    return [value]


def ctx_item(ctx, name, index):
    try:
        return ctx_call(ctx, name, index)
    except TypeError:
        values = ctx_list(ctx, name)
        return values[index] if index < len(values) else None


def ctx_has(ctx, name):
    value = ctx_call(ctx, name)
    if value is None:
        return False
    if isinstance(value, list):
        return bool(value)
    return True


def source_location(ctx):
    start = ctx.start if hasattr(ctx, "start") else ctx.getStart()
    return loc(start.line, start.column)


def token_location(token):
    symbol = token.symbol if hasattr(token, "symbol") else token
    return loc(symbol.line, symbol.column)


def leading_whitespace(line):
    index = 0
    while index < len(line) and line[index].isspace():
        index += 1
    return index


def clean_number(value):
    return value.replace("_", "")


def clean_float(value):
    cleaned = clean_number(value)
    return cleaned[:-1] if cleaned[-1:] in ("f", "F") else cleaned


def clean_double(value):
    cleaned = clean_number(value)
    return cleaned[:-1] if cleaned[-1:] in ("d", "D") else cleaned


def unescape_string_content(value):
    return (
        value.replace(r"\"", '"')
        .replace(r"\'", "'")
        .replace(r"\n", "\n")
        .replace(r"\r", "\r")
        .replace(r"\t", "\t")
        .replace(r"\\", "\\")
    )


def unquote(value):
    if len(value) < 2:
        return value
    return unescape_string_content(value[1:-1])


def quote(value):
    return '"' + value.replace("\\", "\\\\").replace('"', r"\"") + '"'


def bool_value(source):
    return source == "true"


def has_child(ctx, child_text):
    return any(ctx.getChild(i).getText() == child_text for i in range(ctx.getChildCount()))


def first_child_text(ctx):
    return "" if ctx.getChildCount() == 0 else ctx.getChild(0).getText()


def is_grouped(ctx):
    if ctx.getChildCount() != 3:
        return False
    return (ctx.getChild(0).getText(), ctx.getChild(2).getText()) in (("(", ")"), ("{", "}"))


def precedence(operator):
    if operator == "^":
        return 4
    if operator in ("*", "/", "%"):
        return 3
    if operator in ("+", "-"):
        return 2
    if operator in (">", "<", ">=", "<=", "==", "!="):
        return 1
    return 0


def binary_expression(operator, left, right, location, left_grouped=False):
    if (
        not left_grouped
        and type_name(left) == "BinaryExpression"
        and precedence(operator) > precedence(left["operator"])
    ):
        return data(
            "BinaryExpression",
            operator=left["operator"],
            left=left["left"],
            right=binary_expression(operator, left["right"], right, location, False),
            location=left["location"],
        )
    return data("BinaryExpression", operator=operator, left=left, right=right, location=location)


def type_name(value):
    if isinstance(value, dict):
        return value.get("__type", "")
    return str(value)


def source_kind_name(value):
    return type_name(value)


def module_file(module):
    extension = ".coo" if source_kind_name(module["sourceKind"]) == "OBJECT_ORIENTED" else ".cfun"
    prefix = "" if not module.get("path") else module["path"] + "/"
    return prefix + module["name"] + extension


class SyntaxErrorCollector(ErrorListener):
    def __init__(self):
        super().__init__()
        self.errors = []

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.errors.append((line, column, msg))


def throw_if_invalid(module, errors):
    if not errors:
        return
    line, column, message = errors[0]
    raise ValueError(f"{module_file(module)}:{line}:{column}: ParserError: {message}")


def strip_imports(source):
    imports = []
    body = []
    for line_number, line in enumerate(re.split(r"\r\n|\r|\n", source), start=1):
        from_import = FROM_IMPORT_PATTERN.match(line)
        qualified_import = QUALIFIED_IMPORT_PATTERN.match(line)
        if from_import:
            imports.append(
                data(
                    "ImportDeclaration",
                    modulePath=from_import.group(1),
                    importedNames=import_names(from_import.group(2)),
                    excludedNames=import_names(from_import.group(3)),
                    wildcard=import_wildcard(from_import.group(2)),
                    qualified=False,
                    location=loc(line_number, leading_whitespace(line)),
                )
            )
            body.append("")
        elif qualified_import:
            imports.append(
                data(
                    "ImportDeclaration",
                    modulePath=qualified_import.group(1),
                    importedNames=[],
                    excludedNames=[],
                    wildcard=False,
                    qualified=True,
                    location=loc(line_number, leading_whitespace(line)),
                )
            )
            body.append("")
        else:
            body.append(line)
    return os.linesep.join(body), imports


def import_names(raw_names):
    if raw_names is None or import_wildcard(raw_names):
        return []
    return [name for name in (part.strip() for part in raw_names.split(",")) if name]


def import_wildcard(raw_names):
    return raw_names.strip() == "*"


@NativeImplementation(interface_id="dev.capylang.compiler.parser.CapybaraParser")
class NativeCapybaraParser:
    def parse(self, modules):
        return data("ParsedProgram", modules=[self.parse_module(module) for module in modules])

    def parse_module(self, module):
        body, imports = strip_imports(module["input"])
        if source_kind_name(module["sourceKind"]) == "OBJECT_ORIENTED":
            definitions = []
            object_oriented = self.parse_object_oriented(module, body)
        else:
            definitions = self.parse_functional(module, body)
            object_oriented = data("ObjectOriented", interfaces=[], classes=[])
        functional = data("Functional", definitions=definitions)
        return data(
            "ParsedModule",
            name=module["name"],
            path=module["path"],
            functional=functional,
            definitions=definitions,
            objectOriented=object_oriented,
            imports=imports,
            sourceKind=module["sourceKind"],
        )

    def parse_functional(self, module, source):
        lexer = FunctionalLexer(InputStream(source))
        tokens = CommonTokenStream(lexer)
        parser = FunctionalParser(tokens)
        errors = SyntaxErrorCollector()
        lexer.removeErrorListeners()
        parser.removeErrorListeners()
        lexer.addErrorListener(errors)
        parser.addErrorListener(errors)

        program = parser.program()
        throw_if_invalid(module, errors.errors)

        definitions = []
        for definition in ctx_list(program, "definition"):
            if ctx_has(definition, "dataDeclaration"):
                definitions.extend(data_declaration_definitions(ctx_call(definition, "dataDeclaration")))
            elif ctx_has(definition, "typeDeclaration"):
                definitions.extend(type_declaration_definitions(ctx_call(definition, "typeDeclaration")))
            elif ctx_has(definition, "primitiveBackedTypeDeclaration"):
                definitions.extend(
                    primitive_backed_type_declaration_definitions(
                        ctx_call(definition, "primitiveBackedTypeDeclaration")
                    )
                )
            elif ctx_has(definition, "functionDeclaration"):
                definitions.extend(function_definitions(ctx_call(definition, "functionDeclaration")))
            else:
                definitions.append(functional_definition(definition))
        return definitions

    def parse_object_oriented(self, module, source):
        lexer = ObjectOrientedLexer(InputStream(source))
        tokens = CommonTokenStream(lexer)
        parser = ObjectOrientedParser(tokens)
        errors = SyntaxErrorCollector()
        lexer.removeErrorListeners()
        parser.removeErrorListeners()
        lexer.addErrorListener(errors)
        parser.addErrorListener(errors)

        program = parser.program()
        throw_if_invalid(module, errors.errors)

        interfaces = []
        classes = []
        for definition in ctx_list(program, "definition"):
            if ctx_has(definition, "classDeclaration"):
                classes.append(object_oriented_class(ctx_call(definition, "classDeclaration")))
            elif ctx_has(definition, "interfaceDeclaration"):
                interfaces.append(object_oriented_interface(ctx_call(definition, "interfaceDeclaration")))
            elif ctx_has(definition, "traitDeclaration"):
                interfaces.append(object_oriented_trait(ctx_call(definition, "traitDeclaration")))
        return data("ObjectOriented", interfaces=interfaces, classes=classes)


def functional_definition(definition):
    if ctx_has(definition, "annotationDeclaration"):
        return annotation_declaration(ctx_call(definition, "annotationDeclaration"))
    if ctx_has(definition, "deriverDeclaration"):
        return deriver_declaration(ctx_call(definition, "deriverDeclaration"))
    if ctx_has(definition, "enumDeclaration"):
        return enum_declaration(ctx_call(definition, "enumDeclaration"))
    if ctx_has(definition, "constDeclaration"):
        return data("ConstantDefinition", constant=constant_declaration(ctx_call(definition, "constDeclaration")))
    if ctx_has(definition, "primitiveBackedTypeDeclaration"):
        return data("PrimitiveBackedTypeDeclaration")
    return data("UnsupportedDefinition")


def object_oriented_class(ctx):
    fields = []
    init_blocks = []
    methods = []
    for member in ctx_list(ctx_call(ctx, "typeBody"), "memberDeclaration"):
        if ctx_has(member, "fieldDeclaration"):
            fields.append(object_oriented_field(ctx_call(member, "fieldDeclaration")))
        elif ctx_has(member, "initBlock"):
            init_blocks.append(object_statement_block(ctx_call(ctx_call(member, "initBlock"), "statementBlock")))
        elif ctx_has(member, "methodDeclaration"):
            methods.append(object_oriented_method(ctx_call(member, "methodDeclaration")))
    return data(
        "ObjectOrientedClass",
        name=text(ctx_call(ctx, "TYPE")),
        visibility="public",
        open=object_class_open(ctx_list(ctx, "classModifier")),
        parameters=object_constructor_parameters(ctx_call(ctx, "constructorParameters")),
        parents=object_parents(ctx_call(ctx, "inheritanceClause")),
        fields=fields,
        initBlocks=init_blocks,
        methods=methods,
        annotations=object_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def object_class_open(modifiers):
    return any(text(modifier) in ("open", "abstract") for modifier in modifiers)


def object_oriented_interface(ctx):
    methods = []
    for member in ctx_list(ctx_call(ctx, "interfaceBody"), "interfaceMemberDeclaration"):
        if ctx_has(member, "interfaceMethodDeclaration"):
            methods.append(object_oriented_interface_method(ctx_call(member, "interfaceMethodDeclaration")))
    return data(
        "ObjectOrientedInterface",
        name=text(ctx_call(ctx, "TYPE")),
        visibility="public",
        kind="interface",
        parents=object_parents(ctx_call(ctx, "inheritanceClause")),
        methods=methods,
        annotations=object_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def object_oriented_trait(ctx):
    methods = []
    for member in ctx_list(ctx_call(ctx, "typeBody"), "memberDeclaration"):
        if ctx_has(member, "methodDeclaration"):
            methods.append(object_oriented_method(ctx_call(member, "methodDeclaration")))
    return data(
        "ObjectOrientedInterface",
        name=text(ctx_call(ctx, "TYPE")),
        visibility="public",
        kind="trait",
        parents=object_parents(ctx_call(ctx, "inheritanceClause")),
        methods=methods,
        annotations=object_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def object_parents(ctx):
    if ctx is None:
        return []
    return [parse_type_reference(text(parent)) for parent in ctx_list(ctx, "qualifiedType")]


def object_oriented_field(ctx):
    visibility = text(ctx_call(ctx, "visibility")) if ctx_has(ctx, "visibility") else "public"
    has_value = ctx_has(ctx, "expression")
    return data(
        "ObjectOrientedField",
        name=text(ctx_call(ctx, "identifier")),
        visibility=visibility,
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))),
        value=object_expression(ctx_call(ctx, "expression")) if has_value else unsupported(ctx),
        hasValue=has_value,
        annotations=object_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def object_constructor_parameters(ctx):
    if ctx is None or not ctx_has(ctx, "parameters"):
        return []
    return object_parameters(ctx_call(ctx, "parameters"))


def object_oriented_method(ctx):
    visibility = text(ctx_call(ctx, "visibility")) if ctx_has(ctx, "visibility") else "public"
    return data(
        "ObjectOrientedMethod",
        name=text(ctx_call(ctx, "identifier")),
        visibility=visibility,
        parameters=object_parameters(ctx_call(ctx, "parameters")) if ctx_has(ctx, "parameters") else [],
        returnType=parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type")))
        if ctx_has(ctx, "functionType")
        else missing_type(),
        body=object_method_body(ctx_call(ctx, "methodBody")),
        annotations=object_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def object_oriented_interface_method(ctx):
    visibility = text(ctx_call(ctx, "visibility")) if ctx_has(ctx, "visibility") else "public"
    return data(
        "ObjectOrientedMethod",
        name=text(ctx_call(ctx, "identifier")),
        visibility=visibility,
        parameters=object_parameters(ctx_call(ctx, "parameters")) if ctx_has(ctx, "parameters") else [],
        returnType=parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type")))
        if ctx_has(ctx, "functionType")
        else missing_type(),
        body=unsupported(ctx),
        annotations=object_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def object_parameters(ctx):
    return [
        data(
            "FunctionParameter",
            name=text(ctx_call(parameter, "identifier")),
            typeReference=parse_type_reference(text(ctx_call(parameter, "type"))),
            location=source_location(parameter),
        )
        for parameter in ctx_list(ctx, "parameter")
    ]


def object_method_body(ctx):
    if ctx is None:
        return unsupported_expr("", loc(0, 0))
    if ctx_has(ctx, "expression"):
        return object_expression(ctx_call(ctx, "expression"))
    return object_statement_block(ctx_call(ctx, "statementBlock"))


def object_statement_block(ctx):
    bindings = []
    statements = ctx_list(ctx, "statement")
    for index, statement in enumerate(statements):
        if ctx_has(statement, "letStatement"):
            bindings.append(object_let_statement_binding(ctx_call(statement, "letStatement")))
        elif index == len(statements) - 1:
            result = object_statement(statement)
            if not bindings:
                return result
            return data("BlockExpression", bindings=bindings, result=result, location=source_location(ctx))
        else:
            return unsupported(ctx)
    return unsupported(ctx)


def object_statement(ctx):
    if ctx_has(ctx, "returnStatement"):
        return object_expression(ctx_call(ctx_call(ctx, "returnStatement"), "expression"))
    if ctx_has(ctx, "ifStatement"):
        return object_if_statement(ctx_call(ctx, "ifStatement"))
    if ctx_has(ctx, "statementBlock"):
        return object_statement_block(ctx_call(ctx, "statementBlock"))
    if ctx_has(ctx, "expressionStatement"):
        return object_call_expression(ctx_call(ctx_call(ctx, "expressionStatement"), "callExpression"))
    return unsupported(ctx)


def object_let_statement_binding(ctx):
    return data(
        "LetBinding",
        name=text(ctx_call(ctx, "identifier")),
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))) if ctx_has(ctx, "type") else missing_type(),
        operator=text(ctx_call(ctx, "letBindingOperator")),
        value=object_expression(ctx_call(ctx, "expression")),
        location=source_location(ctx),
    )


def object_if_statement(ctx):
    else_branch = object_else_branch(ctx)
    if else_branch is None:
        return unsupported(ctx)
    return data(
        "IfExpression",
        condition=object_expression(ctx_call(ctx, "expression")),
        thenBranch=object_statement_block(ctx_item(ctx, "statementBlock", 0)),
        elseBranch=else_branch,
        location=source_location(ctx),
    )


def object_else_branch(ctx):
    if ctx_has(ctx, "ifStatement"):
        return object_if_statement(ctx_call(ctx, "ifStatement"))
    blocks = ctx_list(ctx, "statementBlock")
    if len(blocks) > 1:
        return object_statement_block(blocks[1])
    return None


def deriver_declaration(ctx):
    return data(
        "DeriverDeclaration",
        name=text(ctx_call(ctx, "TYPE")),
        visibility=text(ctx_call(ctx, "VISIBILITY")) if ctx_has(ctx, "VISIBILITY") else "public",
        methods=[deriver_method_declaration(method) for method in ctx_list(ctx, "deriverMethodDeclaration")],
        annotations=definition_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def deriver_method_declaration(ctx):
    return data(
        "FunctionDeclaration",
        name=text(ctx_call(ctx, "identifier")),
        visibility="public",
        parameters=[function_parameter(parameter) for parameter in ctx_list(ctx_call(ctx, "parameters"), "parameter")]
        if ctx_has(ctx, "parameters")
        else [],
        returnType=parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type"))),
        body=expression(ctx_call(ctx, "expression")),
        annotations=annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def annotation_declaration(ctx):
    return data(
        "AnnotationDeclaration",
        name=text(ctx_call(ctx, "TYPE")),
        visibility=text(ctx_call(ctx, "VISIBILITY")) if ctx_has(ctx, "VISIBILITY") else "public",
        repeatable=ctx_has(ctx, "multipleModifier"),
        targets=[text(target) for target in ctx_list(ctx_call(ctx, "annotationTargetClause"), "annotationTarget")],
        fields=annotation_fields(ctx_call(ctx, "annotationBody")),
        annotations=definition_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def annotation_fields(ctx):
    return [annotation_field(field) for field in ctx_list(ctx, "annotationFieldDeclaration")]


def annotation_field(ctx):
    has_default = ctx_has(ctx, "annotationValue")
    return data(
        "AnnotationFieldDeclaration",
        name=text(ctx_call(ctx, "identifier")),
        typeReference=type_ref(text(ctx_call(ctx_call(ctx, "annotationFieldType"), "annotationTypeReference")), []),
        defaultValue=definition_annotation_value(ctx_call(ctx, "annotationValue"))
        if has_default
        else data("AnnotationNothingValue", source="", location=source_location(ctx)),
        hasDefault=has_default,
        location=source_location(ctx),
    )


def definition_annotation_applications(blocks):
    return [definition_annotation_application(block) for block in blocks]


def definition_annotation_application(ctx):
    return data(
        "AnnotationApplication",
        name=text(ctx_call(ctx, "annotationName")),
        arguments=definition_annotation_arguments(ctx_call(ctx, "annotationArgumentList")),
        location=source_location(ctx),
    )


def definition_annotation_arguments(ctx):
    if ctx is None:
        return []
    return [
        data(
            "AnnotationArgument",
            name=text(ctx_call(argument, "identifier")),
            value=definition_annotation_value(ctx_call(argument, "annotationValue")),
            location=source_location(argument),
        )
        for argument in ctx_list(ctx, "annotationArgument")
    ]


def definition_annotation_value(ctx):
    source = text(ctx)
    location = source_location(ctx)
    if ctx_has(ctx, "STRING_LITERAL"):
        return data("AnnotationStringValue", value=unquote(source), source=source, location=location)
    if ctx_has(ctx, "INT_LITERAL"):
        return data("AnnotationIntValue", value=int(clean_number(source)), source=source, location=location)
    if ctx_has(ctx, "LONG_LITERAL"):
        return data("AnnotationLongValue", value=int(clean_number(source[:-1])), source=source, location=location)
    if ctx_has(ctx, "DOUBLE_LITERAL"):
        return data("AnnotationDoubleValue", value=float(clean_double(source)), source=source, location=location)
    if ctx_has(ctx, "FLOAT_LITERAL"):
        return data("AnnotationFloatValue", value=float(clean_float(source)), source=source, location=location)
    if ctx_has(ctx, "BOOL_LITERAL"):
        return data("AnnotationBoolValue", value=bool_value(source), source=source, location=location)
    if ctx_has(ctx, "NOTHING_LITERAL"):
        return data("AnnotationNothingValue", source=source, location=location)
    if ctx_has(ctx, "annotationTypeReference"):
        return data("AnnotationTypeNameValue", value=text(ctx_call(ctx, "annotationTypeReference")), source=source, location=location)
    return data("AnnotationNothingValue", source=source, location=location)


def annotation_applications(blocks):
    return [annotation_application(block) for block in blocks]


def annotation_application(ctx):
    return data(
        "FunctionAnnotationApplication",
        name=text(ctx_call(ctx, "annotationName")),
        arguments=annotation_arguments(ctx_call(ctx, "annotationArgumentList")),
        location=source_location(ctx),
    )


def annotation_arguments(ctx):
    if ctx is None:
        return []
    return [
        data(
            "FunctionAnnotationArgument",
            name=text(ctx_call(argument, "identifier")),
            value=annotation_value(ctx_call(argument, "annotationValue")),
            location=source_location(argument),
        )
        for argument in ctx_list(ctx, "annotationArgument")
    ]


def annotation_value(ctx):
    source = text(ctx)
    location = source_location(ctx)
    if ctx_has(ctx, "STRING_LITERAL"):
        return data("FunctionAnnotationStringValue", value=unquote(source), source=source, location=location)
    if ctx_has(ctx, "INT_LITERAL"):
        return data("FunctionAnnotationIntValue", value=int(clean_number(source)), source=source, location=location)
    if ctx_has(ctx, "LONG_LITERAL"):
        return data("FunctionAnnotationLongValue", value=int(clean_number(source[:-1])), source=source, location=location)
    if ctx_has(ctx, "DOUBLE_LITERAL"):
        return data("FunctionAnnotationDoubleValue", value=float(clean_double(source)), source=source, location=location)
    if ctx_has(ctx, "FLOAT_LITERAL"):
        return data("FunctionAnnotationFloatValue", value=float(clean_float(source)), source=source, location=location)
    if ctx_has(ctx, "BOOL_LITERAL"):
        return data("FunctionAnnotationBoolValue", value=bool_value(source), source=source, location=location)
    if ctx_has(ctx, "NOTHING_LITERAL"):
        return data("FunctionAnnotationNothingValue", source=source, location=location)
    if ctx_has(ctx, "annotationTypeReference"):
        return data(
            "FunctionAnnotationTypeNameValue",
            value=text(ctx_call(ctx, "annotationTypeReference")),
            source=source,
            location=location,
        )
    return data("FunctionAnnotationNothingValue", source=source, location=location)


def object_annotation_applications(blocks):
    return [object_annotation_application(block) for block in blocks]


def object_annotation_application(ctx):
    return data(
        "FunctionAnnotationApplication",
        name=text(ctx_call(ctx, "annotationName")),
        arguments=object_annotation_arguments(ctx_call(ctx, "annotationArgumentList")),
        location=source_location(ctx),
    )


def object_annotation_arguments(ctx):
    if ctx is None:
        return []
    return [
        data(
            "FunctionAnnotationArgument",
            name=text(ctx_call(argument, "identifier")),
            value=object_annotation_value(ctx_call(argument, "annotationValue")),
            location=source_location(argument),
        )
        for argument in ctx_list(ctx, "annotationArgument")
    ]


def object_annotation_value(ctx):
    return annotation_value(ctx)


def constant_declaration(ctx):
    return data(
        "ConstantDeclaration",
        name=text(ctx_call(ctx, "TYPE")),
        visibility=text(ctx_call(ctx, "VISIBILITY")) if ctx_has(ctx, "VISIBILITY") else "public",
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))) if ctx_has(ctx, "type") else missing_type(),
        expression=expression_no_let(ctx_call(ctx, "expressionNoLet")),
        location=source_location(ctx),
    )


def enum_declaration(ctx):
    types = ctx_list(ctx, "TYPE")
    values = [data("EnumValueDeclaration", name=text(token), location=token_location(token)) for token in types[1:]]
    return data(
        "EnumDeclaration",
        name=text(types[0]) if types else "",
        values=values,
        annotations=definition_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def function_definitions(ctx):
    definitions = []
    local_names = local_function_names(ctx)
    body = ctx_call(ctx, "functionBody")
    if body is not None:
        for definition in ctx_list(body, "localDefinition"):
            if ctx_has(definition, "localDataDeclaration"):
                definitions.extend(local_data_declaration_definitions(ctx_call(definition, "localDataDeclaration")))
            elif ctx_has(definition, "localTypeDeclaration"):
                definitions.extend(local_type_declaration_definitions(ctx_call(definition, "localTypeDeclaration")))
        for definition in ctx_list(body, "localDefinition"):
            if ctx_has(definition, "localFunctionDeclaration"):
                definitions.append(
                    data(
                        "FunctionDefinition",
                        function=local_function_declaration(ctx_call(definition, "localFunctionDeclaration"), local_names),
                    )
                )
    definitions.append(data("FunctionDefinition", function=function_declaration(ctx, local_names)))
    return definitions


def local_function_names(ctx):
    body = ctx_call(ctx, "functionBody")
    if body is None:
        return {}
    names = {}
    outer_name = text(ctx_call(ctx, "functionNameDeclaration"))
    for definition in ctx_list(body, "localDefinition"):
        if ctx_has(definition, "localFunctionDeclaration"):
            local = ctx_call(definition, "localFunctionDeclaration")
            local_name = text(ctx_call(local, "localFunctionNameDeclaration"))
            location = source_location(local)
            names[local_name] = f"{outer_name}__local__{local_name}__{location['line']}_{location['column']}"
    return names


def function_declaration(ctx, local_names=None):
    local_names = local_names or {}
    return data(
        "FunctionDeclaration",
        name=text(ctx_call(ctx, "functionNameDeclaration")),
        visibility=text(ctx_call(ctx, "VISIBILITY")) if ctx_has(ctx, "VISIBILITY") else "public",
        parameters=[function_parameter(parameter) for parameter in ctx_list(ctx_call(ctx, "parameters"), "parameter")]
        if ctx_has(ctx, "parameters")
        else [],
        returnType=parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type")))
        if ctx_has(ctx, "functionType")
        else missing_type(),
        body=function_body(ctx_call(ctx, "functionBody"), local_names),
        annotations=annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def local_function_declaration(ctx, local_names):
    name = text(ctx_call(ctx, "localFunctionNameDeclaration"))
    return data(
        "FunctionDeclaration",
        name=local_names.get(name, name),
        visibility="private",
        parameters=[function_parameter(parameter) for parameter in ctx_list(ctx_call(ctx, "parameters"), "parameter")]
        if ctx_has(ctx, "parameters")
        else [],
        returnType=parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type")))
        if ctx_has(ctx, "functionType")
        else missing_type(),
        body=rewrite_local_function_calls(expression(ctx_call(ctx, "expression")), local_names),
        annotations=annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def data_declaration_definitions(ctx):
    return data_declaration_definitions_from(
        ctx_call(ctx, "genericTypeDeclaration"),
        text(ctx_call(ctx, "VISIBILITY")) if ctx_has(ctx, "VISIBILITY") else "public",
        ctx_call(ctx, "dataBody"),
        ctx_call(ctx, "constructorClause"),
        definition_annotation_applications(ctx_list(ctx, "annotationBlock")),
        derive_applications(ctx_call(ctx, "deriveClause")),
        source_location(ctx),
    )


def local_data_declaration_definitions(ctx):
    return data_declaration_definitions_from(
        ctx_call(ctx, "genericTypeDeclaration"),
        "private",
        ctx_call(ctx, "dataBody"),
        ctx_call(ctx, "constructorClause"),
        [],
        [],
        source_location(ctx),
    )


def type_declaration_definitions(ctx):
    return type_declaration_definitions_from(
        ctx_list(ctx, "genericTypeDeclaration"),
        text(ctx_call(ctx, "VISIBILITY")) if ctx_has(ctx, "VISIBILITY") else "public",
        ctx_call(ctx, "fieldDeclarationList"),
        ctx_call(ctx, "constructorClause"),
        definition_annotation_applications(ctx_list(ctx, "annotationBlock")),
        derive_applications(ctx_call(ctx, "deriveClause")),
        source_location(ctx),
    )


def local_type_declaration_definitions(ctx):
    return type_declaration_definitions_from(
        ctx_list(ctx, "genericTypeDeclaration"),
        "private",
        ctx_call(ctx, "fieldDeclarationList"),
        ctx_call(ctx, "constructorClause"),
        [],
        [],
        source_location(ctx),
    )


def type_declaration_definitions_from(declarations, visibility, parent_fields, constructor_clause, annotations, derives, location):
    if not declarations:
        return []
    union_declaration = declarations[0]
    name = data_type_name(union_declaration)
    definitions = [
        data(
            "TypeDeclaration",
            name=name,
            visibility=visibility,
            parameters=data_type_parameters(union_declaration),
            fields=field_declaration_field_dtos(parent_fields),
            variants=type_declaration_variants(declarations),
            annotations=annotations,
            derives=derives,
            location=location,
        ),
        schema_constant_definition("__capy_schema_type|" + name, name, location),
        schema_constant_definition("__capy_schema_kind|" + name, "union", location),
    ]
    type_parameters = data_type_parameters(union_declaration)
    for index, parameter in enumerate(type_parameters):
        definitions.append(schema_constant_definition(f"__capy_schema_param|{name}|{index}", parameter, location))
    fields = field_declaration_fields(parent_fields)
    for index, field in enumerate(fields):
        definitions.append(schema_constant_definition(f"__capy_schema_field|{name}|{index}", field, location))
    for declaration_index, declaration in enumerate(declarations[1:], start=1):
        variant_name = data_type_name(declaration)
        definitions.append(
            schema_constant_definition(f"__capy_schema_parent|{variant_name}|{declaration_index - 1}", name, location)
        )
        for field_index, field in enumerate(fields):
            definitions.append(
                schema_constant_definition(f"__capy_schema_field|{variant_name}|parent|{field_index}", field, location)
            )
    if constructor_clause is not None:
        definitions.append(constructor_function_definition(name, constructor_parameters(parent_fields), constructor_clause))
    return definitions


def data_declaration_definitions_from(declaration, visibility, data_body, constructor_clause, annotations, derives, location):
    name = data_type_name(declaration)
    definitions = [
        data(
            "DataDeclaration",
            name=name,
            visibility=visibility,
            parameters=data_type_parameters(declaration),
            fields=data_declaration_own_fields(data_body),
            parents=data_declaration_parents(data_body),
            annotations=annotations,
            derives=derives,
            location=location,
        )
    ]
    if constructor_clause is not None:
        definitions.append(constructor_function_definition(name, constructor_parameters(data_body), constructor_clause))
    return definitions


def primitive_backed_type_declaration_definitions(ctx):
    name = text(ctx_call(ctx, "primitiveBackedTypeName"))
    backing_type = type_ref(text(ctx_call(ctx, "primitiveBackingType")), [])
    location = source_location(ctx)
    definitions = [
        schema_constant_definition("__capy_schema_type|" + name, name, location),
        schema_constant_definition("__capy_schema_kind|" + name, "primitive", location),
        schema_constant_definition("__capy_schema_primitive|" + name, backing_type["name"], location),
        schema_constant_definition("__capy_schema_field|" + name + "|0", "value|" + backing_type["name"], location),
    ]
    if ctx_has(ctx, "constructorClause"):
        definitions.append(
            constructor_function_definition(
                name,
                [
                    data(
                        "FunctionParameter",
                        name="value",
                        typeReference=backing_type,
                        location=source_location(ctx_call(ctx, "primitiveBackingType")),
                    )
                ],
                ctx_call(ctx, "constructorClause"),
            )
        )
    return definitions


def constructor_function_definition(name, parameters, constructor_clause):
    return data(
        "FunctionDefinition",
        function=data(
            "FunctionDeclaration",
            name="__capy_constructor|" + name,
            visibility="private",
            parameters=parameters,
            returnType=type_ref("any", []),
            body=rewrite_constructor_data(expression(ctx_call(constructor_clause, "expression")), name),
            annotations=[],
            location=source_location(constructor_clause),
        ),
    )


def constructor_parameters(ctx):
    if ctx is None:
        return []
    if ctx.__class__.__name__.endswith("DataBodyContext"):
        fields = ctx_call(ctx, "fieldDeclarationList")
    else:
        fields = ctx
    if fields is None:
        return []
    return [constructor_parameter(field) for field in ctx_list(fields, "fieldDeclaration") if not data_parent_declaration(field)]


def constructor_parameter(field):
    name = text(ctx_call(field, "identifier")) if ctx_has(field, "identifier") else unquote(text(ctx_call(field, "STRING_LITERAL")))
    return data("FunctionParameter", name=name, typeReference=parse_type_reference(text(ctx_call(field, "type"))), location=source_location(field))


def schema_constant_definition(name, value, location):
    return data(
        "ConstantDefinition",
        constant=data(
            "ConstantDeclaration",
            name=name,
            visibility="schema",
            typeReference=string_type(),
            expression=data("StringLiteral", value=value, source=quote(value), location=location),
            location=location,
        ),
    )


def data_type_name(ctx):
    return text(ctx_item(ctx, "TYPE", 0) or ctx_call(ctx, "TYPE"))


def data_type_parameters(ctx):
    return [text(token) for token in ctx_list(ctx, "TYPE")[1:]]


def data_declaration_own_fields(ctx):
    field_list = ctx_call(ctx, "fieldDeclarationList") if ctx is not None else None
    if field_list is None:
        return []
    return [data_field_declaration_dto(field) for field in ctx_list(field_list, "fieldDeclaration") if not data_parent_declaration(field)]


def field_declaration_field_dtos(ctx):
    if ctx is None:
        return []
    return [data_field_declaration_dto(field) for field in ctx_list(ctx, "fieldDeclaration") if not data_parent_declaration(field)]


def type_declaration_variants(declarations):
    return [parse_type_reference(text(declaration)) for declaration in declarations[1:]]


def derive_applications(ctx):
    if ctx is None:
        return []
    return [data("DeriveApplication", name=text(token), location=token_location(token)) for token in ctx_list(ctx, "TYPE")]


def data_field_declaration_dto(ctx):
    name = text(ctx_call(ctx, "identifier")) if ctx_has(ctx, "identifier") else unquote(text(ctx_call(ctx, "STRING_LITERAL")))
    return data(
        "DataFieldDeclaration",
        name=name,
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))),
        annotations=definition_annotation_applications(ctx_list(ctx, "annotationBlock")),
        location=source_location(ctx),
    )


def data_declaration_parents(ctx):
    field_list = ctx_call(ctx, "fieldDeclarationList") if ctx is not None else None
    if field_list is None:
        return []
    parents = []
    for field in ctx_list(field_list, "fieldDeclaration"):
        if data_parent_declaration(field):
            parents.append(
                data(
                    "DataParentDeclaration",
                    typeReference=type_ref(text(ctx_call(field, "TYPE")), []),
                    location=source_location(field),
                )
            )
    return parents


def data_parent_declaration(ctx):
    return ctx_has(ctx, "SPREAD") and ctx_has(ctx, "TYPE")


def field_declaration_fields(ctx):
    if ctx is None:
        return []
    return [data_field_declaration(field) for field in ctx_list(ctx, "fieldDeclaration")]


def data_field_declaration(ctx):
    if ctx_has(ctx, "identifier"):
        return data_field_schema(text(ctx_call(ctx, "identifier")), ctx_call(ctx, "type"))
    if ctx_has(ctx, "STRING_LITERAL"):
        return data_field_schema(unquote(text(ctx_call(ctx, "STRING_LITERAL"))), ctx_call(ctx, "type"))
    return "$unsupported|"


def data_field_schema(name, type_ctx):
    return name + "|" + text(type_ctx)


def function_parameter(ctx):
    return data(
        "FunctionParameter",
        name=text(ctx_call(ctx, "identifier")),
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))),
        location=source_location(ctx),
    )


def function_body(ctx, local_names=None):
    local_names = local_names or {}
    if not ctx_list(ctx, "localDefinition"):
        return rewrite_local_function_calls(expression(ctx_call(ctx, "expression")), local_names)
    bindings = []
    for definition in ctx_list(ctx, "localDefinition"):
        if ctx_has(definition, "localConstDeclaration"):
            bindings.append(local_const_binding(ctx_call(definition, "localConstDeclaration"), local_names))
    result = rewrite_local_function_calls(expression(ctx_call(ctx, "expression")), local_names)
    if not bindings:
        return result
    return data("BlockExpression", bindings=bindings, result=result, location=source_location(ctx))


def local_const_binding(ctx, local_names=None):
    local_names = local_names or {}
    return data(
        "LetBinding",
        name=text(ctx_call(ctx, "privateLocalConstName")),
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))) if ctx_has(ctx, "type") else missing_type(),
        operator="=",
        value=rewrite_local_function_calls(expression_no_let(ctx_call(ctx, "expressionNoLet")), local_names),
        location=source_location(ctx),
    )


def rewrite_local_function_calls(expr, local_names):
    return rewrite_expression(expr, local_names, rewrite_local_name, None)


def rewrite_constructor_data(expr, data_type_name_value):
    return rewrite_expression(expr, {}, None, data_type_name_value)


def rewrite_local_name(name, local_names):
    return local_names.get(name, name)


def rewrite_expression(expr, local_names, name_rewriter, constructor_type):
    kind = type_name(expr)
    if kind == "IfExpression":
        return data(
            "IfExpression",
            condition=rewrite_expression(expr["condition"], local_names, name_rewriter, constructor_type),
            thenBranch=rewrite_expression(expr["thenBranch"], local_names, name_rewriter, constructor_type),
            elseBranch=rewrite_expression(expr["elseBranch"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind == "BinaryExpression":
        return data(
            "BinaryExpression",
            operator=expr["operator"],
            left=rewrite_expression(expr["left"], local_names, name_rewriter, constructor_type),
            right=rewrite_expression(expr["right"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind == "UnaryExpression":
        return data(
            "UnaryExpression",
            operator=expr["operator"],
            expression=rewrite_expression(expr["expression"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind == "FunctionCallExpression":
        name = name_rewriter(expr["name"], local_names) if name_rewriter else expr["name"]
        return data(
            "FunctionCallExpression",
            name=name,
            arguments=[rewrite_expression(arg, local_names, name_rewriter, constructor_type) for arg in expr["arguments"]],
            location=expr["location"],
        )
    if kind == "FunctionReferenceExpression":
        name = name_rewriter(expr["name"], local_names) if name_rewriter else expr["name"]
        return data("FunctionReferenceExpression", name=name, location=expr["location"])
    if kind == "MethodCallExpression":
        return data(
            "MethodCallExpression",
            receiver=rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type),
            name=expr["name"],
            arguments=[rewrite_expression(arg, local_names, name_rewriter, constructor_type) for arg in expr["arguments"]],
            location=expr["location"],
        )
    if kind == "WithExpression":
        return data(
            "WithExpression",
            receiver=rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type),
            fields=rewrite_data_fields(expr["fields"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind == "LambdaExpression":
        return data(
            "LambdaExpression",
            parameters=expr["parameters"],
            body=rewrite_expression(expr["body"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind in ("ListLiteral", "SetLiteral", "TupleLiteral"):
        return data(
            kind,
            values=[rewrite_expression(value, local_names, name_rewriter, constructor_type) for value in expr["values"]],
            location=expr["location"],
        )
    if kind == "DictLiteral":
        return data(
            "DictLiteral",
            entries=[
                data(
                    "DictEntry",
                    key=rewrite_expression(entry["key"], local_names, name_rewriter, constructor_type),
                    value=rewrite_expression(entry["value"], local_names, name_rewriter, constructor_type),
                    location=entry["location"],
                )
                for entry in expr["entries"]
            ],
            location=expr["location"],
        )
    if kind == "IndexExpression":
        return data(
            "IndexExpression",
            receiver=rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type),
            index=rewrite_expression(expr["index"], local_names, name_rewriter, constructor_type),
            endIndex=rewrite_expression(expr["endIndex"], local_names, name_rewriter, constructor_type),
            hasEndIndex=expr["hasEndIndex"],
            location=expr["location"],
        )
    if kind == "DataLiteral":
        type_name_value = expr["typeName"]
        if constructor_type is not None and type_name_value == "*":
            type_name_value = "__capy_raw|" + constructor_type
        return data(
            "DataLiteral",
            typeName=type_name_value,
            fields=rewrite_data_fields(expr["fields"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind == "FieldAccessExpression":
        return data(
            "FieldAccessExpression",
            receiver=rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type),
            name=expr["name"],
            location=expr["location"],
        )
    if kind == "ReduceExpression":
        return data(
            "ReduceExpression",
            receiver=rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type),
            initial=rewrite_expression(expr["initial"], local_names, name_rewriter, constructor_type),
            accumulatorName=expr["accumulatorName"],
            keyName=expr["keyName"],
            valueName=expr["valueName"],
            body=rewrite_expression(expr["body"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    if kind == "MatchExpression":
        return data(
            "MatchExpression",
            value=rewrite_expression(expr["value"], local_names, name_rewriter, constructor_type),
            cases=[
                data(
                    "MatchCase",
                    typeName=case["typeName"],
                    bindings=case["bindings"],
                    bindsWholeValue=case["bindsWholeValue"],
                    literal=rewrite_expression(case["literal"], local_names, name_rewriter, constructor_type),
                    hasLiteral=case["hasLiteral"],
                    wildcard=case["wildcard"],
                    guard=rewrite_expression(case["guard"], local_names, name_rewriter, constructor_type),
                    hasGuard=case["hasGuard"],
                    body=rewrite_expression(case["body"], local_names, name_rewriter, constructor_type),
                    location=case["location"],
                )
                for case in expr["cases"]
            ],
            location=expr["location"],
        )
    if kind == "BlockExpression":
        return data(
            "BlockExpression",
            bindings=[
                data(
                    "LetBinding",
                    name=binding["name"],
                    typeReference=binding["typeReference"],
                    operator=binding["operator"],
                    value=rewrite_expression(binding["value"], local_names, name_rewriter, constructor_type),
                    location=binding["location"],
                )
                for binding in expr["bindings"]
            ],
            result=rewrite_expression(expr["result"], local_names, name_rewriter, constructor_type),
            location=expr["location"],
        )
    return expr


def rewrite_data_fields(fields, local_names, name_rewriter, constructor_type):
    return [
        data(
            "DataField",
            name=field["name"],
            value=rewrite_expression(field["value"], local_names, name_rewriter, constructor_type),
            spread=field["spread"],
            location=field["location"],
        )
        for field in fields
    ]


def expression(ctx):
    bindings = [let_binding(let) for let in ctx_list(ctx, "letExpression")]
    result = expression_no_let(ctx_call(ctx, "expressionNoLet"))
    if not bindings:
        return result
    return data("BlockExpression", bindings=bindings, result=result, location=source_location(ctx))


def let_binding(ctx):
    return data(
        "LetBinding",
        name=text(ctx_call(ctx, "identifier")),
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))) if ctx_has(ctx, "type") else missing_type(),
        operator=text(ctx_call(ctx, "letBindingOperator")),
        value=expression_no_let(ctx_call(ctx, "expressionNoLet")),
        location=source_location(ctx),
    )


def expression_no_let(ctx):
    if ctx_has(ctx, "ifExpression"):
        return if_expression(ctx_call(ctx, "ifExpression"))
    if ctx_has(ctx, "lambdaExpression"):
        return lambda_expression(ctx_call(ctx, "lambdaExpression"))
    if ctx_has(ctx, "functionCall"):
        return function_call(ctx_call(ctx, "functionCall"))
    if ctx_has(ctx, "functionReference"):
        reference = ctx_call(ctx, "functionReference")
        return data("FunctionReferenceExpression", name=text(ctx_call(reference, "identifier")), location=source_location(reference))
    if ctx_has(ctx, "placeholder"):
        return unsupported(ctx_call(ctx, "placeholder"))
    if ctx_has(ctx, "new_list"):
        return list_literal(ctx_call(ctx, "new_list"))
    if ctx_has(ctx, "new_dict"):
        return dict_literal(ctx_call(ctx, "new_dict"))
    if ctx_has(ctx, "tupleLiteral"):
        return tuple_literal(ctx_call(ctx, "tupleLiteral"))
    if ctx_has(ctx, "expression"):
        return expression(ctx_call(ctx, "expression"))
    if ctx_has(ctx, "new_set"):
        return set_literal(ctx_call(ctx, "new_set"))
    if ctx_has(ctx, "newData"):
        return data_literal(ctx_call(ctx, "newData"))
    if ctx_has(ctx, "constructorData"):
        return constructor_data_literal(ctx_call(ctx, "constructorData"))
    if ctx_has(ctx, "matchExpression"):
        return match_expression(ctx_call(ctx, "matchExpression"))
    if ctx_has(ctx, "value"):
        return value(ctx_call(ctx, "value"))
    children = ctx_list(ctx, "expressionNoLet")
    if ctx_has(ctx, "infixMethodLiteral") and len(children) == 2:
        return binary_expression(
            text(ctx_call(ctx, "infixMethodLiteral")),
            expression_no_let(children[0]),
            expression_no_let(children[1]),
            source_location(ctx),
            is_grouped(children[0]),
        )
    if ctx_has(ctx, "infixOperator") and len(children) == 2:
        operator = text(ctx_call(ctx, "infixOperator"))
        right_reduce = ctx_call(children[1], "reduceExpression") if children[1] is not None else None
        if operator == "|>" and right_reduce is not None:
            return reduce_expression(expression_no_let(children[0]), right_reduce, source_location(ctx))
        return binary_expression(
            operator,
            expression_no_let(children[0]),
            expression_no_let(children[1]),
            source_location(ctx),
            is_grouped(children[0]),
        )
    if ctx_has(ctx, "identifier") and len(children) == 1 and has_child(ctx, "."):
        return data(
            "FieldAccessExpression",
            receiver=expression_no_let(children[0]),
            name=text(ctx_call(ctx, "identifier")),
            location=source_location(ctx),
        )
    if ctx_has(ctx, "methodIdentifier") and len(children) == 1:
        return method_call_expression(
            expression_no_let(children[0]),
            text(ctx_call(ctx, "methodIdentifier")),
            ctx_call(ctx, "methodArgumentList"),
            source_location(ctx),
        )
    if is_invocation_expression(ctx):
        return invocation_expression(expression_no_let(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx))
    if is_slice_expression(ctx):
        return slice_expression(expression_no_let(children[0]), source_location(ctx), ctx)
    if is_index_expression(ctx):
        return index_expression(expression_no_let(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx), ctx)
    if is_unary(ctx):
        operator = first_child_text(ctx)
        return data("UnaryExpression", operator=operator, expression=unary_operand(operator, children[0]), location=source_location(ctx))
    return unsupported(ctx)


def expression_no_pipe(ctx):
    if ctx_list(ctx, "letExpressionNoPipe"):
        return unsupported(ctx)
    return expression_no_let_no_pipe(ctx_call(ctx, "expressionNoLetNoPipe"))


def expression_no_let_no_pipe(ctx):
    if ctx_has(ctx, "ifExpression"):
        return if_expression(ctx_call(ctx, "ifExpression"))
    if ctx_has(ctx, "lambdaExpression"):
        return lambda_expression(ctx_call(ctx, "lambdaExpression"))
    if ctx_has(ctx, "functionCall"):
        return function_call(ctx_call(ctx, "functionCall"))
    if ctx_has(ctx, "functionReference"):
        reference = ctx_call(ctx, "functionReference")
        return data("FunctionReferenceExpression", name=text(ctx_call(reference, "identifier")), location=source_location(reference))
    if ctx_has(ctx, "placeholder"):
        return unsupported(ctx_call(ctx, "placeholder"))
    if ctx_has(ctx, "new_list"):
        return list_literal(ctx_call(ctx, "new_list"))
    if ctx_has(ctx, "new_dict"):
        return dict_literal(ctx_call(ctx, "new_dict"))
    if ctx_has(ctx, "tupleLiteral"):
        return tuple_literal(ctx_call(ctx, "tupleLiteral"))
    if ctx_has(ctx, "expression"):
        return expression(ctx_call(ctx, "expression"))
    if ctx_has(ctx, "new_set"):
        return set_literal(ctx_call(ctx, "new_set"))
    if ctx_has(ctx, "newData"):
        return data_literal(ctx_call(ctx, "newData"))
    if ctx_has(ctx, "constructorData"):
        return constructor_data_literal(ctx_call(ctx, "constructorData"))
    if ctx_has(ctx, "matchExpressionNoPipe"):
        return match_expression_no_pipe(ctx_call(ctx, "matchExpressionNoPipe"))
    if ctx_has(ctx, "value"):
        return value(ctx_call(ctx, "value"))
    children = ctx_list(ctx, "expressionNoLetNoPipe")
    if ctx_has(ctx, "infixMethodLiteral") and len(children) == 2:
        return binary_expression(
            text(ctx_call(ctx, "infixMethodLiteral")),
            expression_no_let_no_pipe(children[0]),
            expression_no_let_no_pipe(children[1]),
            source_location(ctx),
            is_grouped(children[0]),
        )
    if ctx_has(ctx, "infixOperatorNoPipe") and len(children) == 2:
        return binary_expression(
            text(ctx_call(ctx, "infixOperatorNoPipe")),
            expression_no_let_no_pipe(children[0]),
            expression_no_let_no_pipe(children[1]),
            source_location(ctx),
            is_grouped(children[0]),
        )
    if ctx_has(ctx, "identifier") and len(children) == 1 and has_child(ctx, "."):
        return data(
            "FieldAccessExpression",
            receiver=expression_no_let_no_pipe(children[0]),
            name=text(ctx_call(ctx, "identifier")),
            location=source_location(ctx),
        )
    if ctx_has(ctx, "methodIdentifier") and len(children) == 1:
        return method_call_expression(
            expression_no_let_no_pipe(children[0]),
            text(ctx_call(ctx, "methodIdentifier")),
            ctx_call(ctx, "methodArgumentList"),
            source_location(ctx),
        )
    if is_invocation_expression_no_pipe(ctx):
        return invocation_expression(expression_no_let_no_pipe(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx))
    if is_slice_expression_no_pipe(ctx):
        return slice_expression(expression_no_let_no_pipe(children[0]), source_location(ctx), ctx)
    if ctx_has(ctx, "argumentList") and len(children) == 1 and has_child(ctx, "["):
        return index_expression(expression_no_let_no_pipe(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx), ctx)
    if is_unary_no_pipe(ctx):
        operator = first_child_text(ctx)
        return data(
            "UnaryExpression",
            operator=operator,
            expression=unary_no_pipe_operand(operator, children[0]),
            location=source_location(ctx),
        )
    return unsupported(ctx)


def unary_operand(operator, operand):
    if operator == "-":
        min_literal = negatable_min_literal(ctx_call(operand, "value"), source_location(operand))
        if min_literal is not None:
            return min_literal
    return expression_no_let(operand)


def unary_no_pipe_operand(operator, operand):
    if operator == "-":
        min_literal = negatable_min_literal(ctx_call(operand, "value"), source_location(operand))
        if min_literal is not None:
            return min_literal
    return expression_no_let_no_pipe(operand)


def negatable_min_literal(value_ctx, location):
    literal = ctx_call(value_ctx, "literal") if value_ctx is not None else None
    if literal is None:
        return None
    source = text(literal)
    if ctx_has(literal, "INT_LITERAL") and clean_number(source) == "2147483648":
        return data("IntLiteral", value=-2147483648, source=source, location=location)
    if ctx_has(literal, "LONG_LITERAL") and clean_number(source[:-1]) == "9223372036854775808":
        return data("LongLiteral", value=-9223372036854775808, source=source, location=location)
    return None


def if_expression(ctx):
    return data(
        "IfExpression",
        condition=expression(ctx_item(ctx, "expression", 0)),
        thenBranch=expression(ctx_item(ctx, "expression", 1)),
        elseBranch=expression(ctx_item(ctx, "expression", 2)),
        location=source_location(ctx),
    )


def lambda_expression(ctx):
    return data(
        "LambdaExpression",
        parameters=[text(parameter) for parameter in ctx_list(ctx, "lambdaArgument")],
        body=expression_no_pipe(ctx_call(ctx, "expressionNoPipe")),
        location=source_location(ctx),
    )


def reduce_expression(receiver, ctx, location):
    arguments = ctx_list(ctx, "lambdaArgument")
    if len(arguments) < 2:
        return unsupported_expr(text(ctx), location)
    key_name = text(arguments[1]) if len(arguments) > 2 else ""
    value_name = text(arguments[2]) if len(arguments) > 2 else text(arguments[1])
    return data(
        "ReduceExpression",
        receiver=receiver,
        initial=expression_no_let_no_pipe(ctx_call(ctx, "expressionNoLetNoPipe")),
        accumulatorName=text(arguments[0]),
        keyName=key_name,
        valueName=value_name,
        body=expression_no_pipe(ctx_call(ctx, "expressionNoPipe")),
        location=location,
    )


def function_call(ctx):
    name = (
        text(ctx_call(ctx, "TYPE")) + "." + text(ctx_call(ctx, "identifier"))
        if ctx_has(ctx, "TYPE") and ctx_has(ctx, "identifier")
        else ctx.getChild(0).getText()
    )
    return data("FunctionCallExpression", name=name, arguments=arguments(ctx_call(ctx, "argumentList")), location=source_location(ctx))


def method_call_expression(receiver, name, argument_list, location):
    if name == "with":
        return data("WithExpression", receiver=receiver, fields=with_fields(argument_list), location=location)
    reduce = method_reduce_expression(receiver, name, argument_list, location)
    if reduce is not None:
        return reduce
    return data("MethodCallExpression", receiver=receiver, name=name, arguments=method_arguments(argument_list), location=location)


def method_reduce_expression(receiver, name, argument_list, location):
    if name not in ("reduce", "reduce_left") or argument_list is None or len(ctx_list(argument_list, "methodArgument")) != 1:
        return None
    argument = ctx_list(argument_list, "methodArgument")[0]
    argument_expression = ctx_call(argument, "expression")
    if argument_expression is None:
        return None
    reduce_ctx = ctx_call(ctx_call(argument_expression, "expressionNoLet"), "reduceExpression")
    if reduce_ctx is None:
        return None
    return reduce_expression(receiver, reduce_ctx, location)


def invocation_expression(receiver, argument_list, location):
    if type_name(receiver) == "VariableExpression":
        return data("FunctionCallExpression", name=receiver["name"], arguments=arguments(argument_list), location=location)
    return data("MethodCallExpression", receiver=receiver, name="__capy_call", arguments=arguments(argument_list), location=location)


def arguments(ctx):
    if ctx is None:
        return []
    return [expression(argument) for argument in ctx_list(ctx, "expression")]


def method_arguments(ctx):
    if ctx is None:
        return []
    result = []
    for argument in ctx_list(ctx, "methodArgument"):
        if ctx_has(argument, "expression"):
            result.append(expression(ctx_call(argument, "expression")))
        else:
            result.append(unsupported(argument))
    return result


def with_fields(ctx):
    if ctx is None:
        return []
    fields = []
    for argument in ctx_list(ctx, "methodArgument"):
        if ctx_has(argument, "namedMethodArgument"):
            named = ctx_call(argument, "namedMethodArgument")
            fields.append(
                data(
                    "DataField",
                    name=text(ctx_call(named, "identifier")),
                    value=expression(ctx_call(named, "expression")),
                    spread=False,
                    location=source_location(named),
                )
            )
        else:
            fields.append(data("DataField", name="$unsupported", value=unsupported(argument), spread=False, location=source_location(argument)))
    return fields


def list_literal(ctx):
    return data("ListLiteral", values=[expression(value_ctx) for value_ctx in ctx_list(ctx, "expression")], location=source_location(ctx))


def set_literal(ctx):
    return data("SetLiteral", values=[expression(value_ctx) for value_ctx in ctx_list(ctx, "expression")], location=source_location(ctx))


def dict_literal(ctx):
    return data(
        "DictLiteral",
        entries=[
            data("DictEntry", key=expression(ctx_item(entry, "expression", 0)), value=expression(ctx_item(entry, "expression", 1)), location=source_location(entry))
            for entry in ctx_list(ctx, "dict_entry")
        ],
        location=source_location(ctx),
    )


def tuple_literal(ctx):
    return data("TupleLiteral", values=[expression(value_ctx) for value_ctx in ctx_list(ctx, "expression")], location=source_location(ctx))


def index_expression(receiver, argument_list, location, fallback):
    expressions = ctx_list(argument_list, "expression") if argument_list is not None else []
    if not expressions:
        return data("IndexExpression", receiver=receiver, index=unsupported(fallback), endIndex=unsupported(fallback), hasEndIndex=False, location=location)
    index = expression(expressions[0])
    if len(expressions) > 1:
        return data("IndexExpression", receiver=receiver, index=index, endIndex=expression(expressions[1]), hasEndIndex=True, location=location)
    return data("IndexExpression", receiver=receiver, index=index, endIndex=unsupported(fallback), hasEndIndex=False, location=location)


def slice_expression(receiver, location, ctx):
    start = data("IntLiteral", value=0, source="__capy_slice_start__", location=location)
    end = data("IntLiteral", value=0, source="__capy_slice_end__", location=location)
    before_colon = True
    for index in range(ctx.getChildCount()):
        child = ctx.getChild(index)
        child_text = child.getText()
        if child_text == ":":
            before_colon = False
        elif child.__class__.__name__ in ("SliceIndexLiteralContext", "SliceIndexNoPipeLiteralContext"):
            literal = data("IntLiteral", value=int(clean_number(child_text)), source=child_text, location=source_location(child))
            if before_colon:
                start = literal
            else:
                end = literal
    return data("IndexExpression", receiver=receiver, index=start, endIndex=end, hasEndIndex=True, location=location)


def data_literal(ctx):
    type_name_value = text(ctx_call(ctx, "type"))
    if ctx_has(ctx, "BANG"):
        type_name_value = "__capy_raw|" + type_name_value
    return data("DataLiteral", typeName=type_name_value, fields=data_fields(ctx_call(ctx, "fieldAssignmentList")), location=source_location(ctx))


def constructor_data_literal(ctx):
    return data("DataLiteral", typeName="*", fields=data_fields(ctx_call(ctx, "fieldAssignmentList")), location=source_location(ctx))


def data_fields(ctx):
    if ctx is None:
        return []
    fields = []
    positional_index = 0
    for assignment in ctx_list(ctx, "fieldAssignment"):
        if ctx_has(assignment, "namedFieldAssignment"):
            fields.append(named_data_field(ctx_call(assignment, "namedFieldAssignment")))
            positional_index += 1
        elif ctx_has(assignment, "spreadFieldAssignment"):
            fields.append(
                data(
                    "DataField",
                    name="",
                    value=expression(ctx_call(ctx_call(assignment, "spreadFieldAssignment"), "expression")),
                    spread=True,
                    location=source_location(assignment),
                )
            )
        elif ctx_has(assignment, "positionalFieldAssignment"):
            fields.append(
                data(
                    "DataField",
                    name="$" + str(positional_index),
                    value=expression(ctx_call(ctx_call(assignment, "positionalFieldAssignment"), "expression")),
                    spread=False,
                    location=source_location(assignment),
                )
            )
            positional_index += 1
        else:
            fields.append(data("DataField", name="$unsupported", value=unsupported(assignment), spread=False, location=source_location(assignment)))
            positional_index += 1
    return fields


def named_data_field(ctx):
    name = text(ctx_call(ctx, "identifier")) if ctx_has(ctx, "identifier") else unquote(text(ctx_call(ctx, "STRING_LITERAL")))
    return data("DataField", name=name, value=expression(ctx_call(ctx, "expression")), spread=False, location=source_location(ctx))


def match_expression(ctx):
    cases = []
    for case_list in ctx_list(ctx, "matchCaseList"):
        for match_case_ctx in ctx_list(case_list, "matchCase"):
            cases.extend(match_cases(match_case_ctx))
    return data("MatchExpression", value=expression(ctx_call(ctx, "expression")), cases=cases, location=source_location(ctx))


def match_expression_no_pipe(ctx):
    cases = []
    for case_list in ctx_list(ctx, "matchCaseNoPipeList"):
        for match_case_ctx in ctx_list(case_list, "matchCaseNoPipe"):
            cases.extend(match_cases_no_pipe(match_case_ctx))
    return data("MatchExpression", value=expression_no_pipe(ctx_call(ctx, "expressionNoPipe")), cases=cases, location=source_location(ctx))


def match_cases(ctx):
    patterns = ctx_list(ctx, "pattern")
    if not patterns:
        return [match_case(ctx, None)]
    return [match_case(ctx, pattern) for pattern in patterns]


def match_case(ctx, pattern):
    guard = getattr(ctx, "guard", None)
    body = getattr(ctx, "body", None)
    return data(
        "MatchCase",
        typeName=pattern_type_name(pattern),
        bindings=pattern_bindings(pattern),
        bindsWholeValue=pattern_binds_whole_value(pattern),
        literal=pattern_literal(pattern),
        hasLiteral=pattern_has_literal(pattern),
        wildcard=pattern_wildcard(pattern),
        guard=unsupported(ctx) if guard is None else expression(guard),
        hasGuard=guard is not None,
        body=expression(body),
        location=source_location(ctx),
    )


def match_cases_no_pipe(ctx):
    patterns = ctx_list(ctx, "pattern")
    if not patterns:
        return [match_case_no_pipe(ctx, None)]
    return [match_case_no_pipe(ctx, pattern) for pattern in patterns]


def match_case_no_pipe(ctx, pattern):
    guard = getattr(ctx, "guard", None)
    body = getattr(ctx, "body", None)
    return data(
        "MatchCase",
        typeName=pattern_type_name(pattern),
        bindings=pattern_bindings(pattern),
        bindsWholeValue=pattern_binds_whole_value(pattern),
        literal=pattern_literal(pattern),
        hasLiteral=pattern_has_literal(pattern),
        wildcard=pattern_wildcard(pattern),
        guard=unsupported(ctx) if guard is None else expression_no_pipe(guard),
        hasGuard=guard is not None,
        body=expression_no_pipe(body),
        location=source_location(ctx),
    )


def pattern_type_name(ctx):
    if ctx is None:
        return ""
    if ctx_has(ctx, "constructorPattern"):
        return text(ctx_call(ctx_call(ctx, "constructorPattern"), "TYPE"))
    if ctx_has(ctx, "typedPattern"):
        return text(ctx_call(ctx_call(ctx, "typedPattern"), "patternType"))
    if ctx_has(ctx, "TYPE"):
        return text(ctx_call(ctx, "TYPE"))
    return ""


def pattern_bindings(ctx, include_lower_qualified_field=False):
    if ctx is None:
        return []
    if ctx_has(ctx, "typedPattern") and ctx_has(ctx_call(ctx, "typedPattern"), "NAME"):
        return [text(ctx_call(ctx_call(ctx, "typedPattern"), "NAME"))]
    if ctx_has(ctx, "wildcardPattern") and ctx_has(ctx_call(ctx, "wildcardPattern"), "NAME"):
        return [text(ctx_call(ctx_call(ctx, "wildcardPattern"), "NAME"))]
    constructor = ctx_call(ctx, "constructorPattern")
    field_list = ctx_call(constructor, "fieldPatternList") if constructor is not None else None
    if field_list is None:
        return []
    bindings = []
    for index, field_pattern in enumerate(ctx_list(field_list, "pattern")):
        if ctx_has(field_pattern, "identifier"):
            bindings.append(text(ctx_call(field_pattern, "identifier")))
        elif (
            include_lower_qualified_field
            and ctx_has(field_pattern, "patternType")
            and ctx_has(ctx_call(field_pattern, "patternType"), "lowerQualifiedType")
        ):
            bindings.append(text(ctx_call(ctx_call(field_pattern, "patternType"), "lowerQualifiedType")))
        elif ctx_has(field_pattern, "wildcardPattern") and ctx_has(ctx_call(field_pattern, "wildcardPattern"), "NAME"):
            bindings.append(text(ctx_call(ctx_call(field_pattern, "wildcardPattern"), "NAME")))
        elif ctx_has(field_pattern, "wildcardPattern"):
            bindings.append("__capy_ignore_" + str(index))
    return bindings


def pattern_wildcard(ctx):
    return ctx is not None and ctx_has(ctx, "wildcardPattern")


def pattern_binds_whole_value(ctx):
    return ctx is not None and (ctx_has(ctx, "typedPattern") or ctx_has(ctx, "wildcardPattern"))


def pattern_has_literal(ctx):
    return ctx is not None and any(ctx_has(ctx, name) for name in ("INT_LITERAL", "LONG_LITERAL", "BOOL_LITERAL", "STRING_LITERAL", "FLOAT_LITERAL"))


def pattern_literal(ctx):
    if ctx is None:
        return unsupported_literal()
    location = source_location(ctx)
    if ctx_has(ctx, "INT_LITERAL"):
        source = text(ctx_call(ctx, "INT_LITERAL"))
        return data("IntLiteral", value=int(clean_number(source)), source=source, location=location)
    if ctx_has(ctx, "LONG_LITERAL"):
        source = text(ctx_call(ctx, "LONG_LITERAL"))
        return data("LongLiteral", value=int(clean_number(source[:-1])), source=source, location=location)
    if ctx_has(ctx, "FLOAT_LITERAL"):
        source = text(ctx_call(ctx, "FLOAT_LITERAL"))
        return data("FloatLiteral", value=float(clean_float(source)), source=source, location=location)
    if ctx_has(ctx, "BOOL_LITERAL"):
        source = text(ctx_call(ctx, "BOOL_LITERAL"))
        return data("BoolLiteral", value=bool_value(source), source=source, location=location)
    if ctx_has(ctx, "STRING_LITERAL"):
        return string_literal_expression(text(ctx_call(ctx, "STRING_LITERAL")), location)
    return unsupported_literal()


def value(ctx):
    if ctx_has(ctx, "identifier"):
        return data("VariableExpression", name=text(ctx_call(ctx, "identifier")), location=source_location(ctx))
    if ctx_has(ctx, "qualifiedType"):
        return data("VariableExpression", name=text(ctx_call(ctx, "qualifiedType")), location=source_location(ctx))
    literal = ctx_call(ctx, "literal")
    source = text(literal)
    try:
        if ctx_has(literal, "BYTE_LITERAL"):
            return data("IntLiteral", value=int(clean_number(source), 0), source=source, location=source_location(ctx))
        if ctx_has(literal, "INT_LITERAL"):
            return data("IntLiteral", value=int(clean_number(source)), source=source, location=source_location(ctx))
        if ctx_has(literal, "LONG_LITERAL"):
            return data("LongLiteral", value=int(clean_number(source[:-1])), source=source, location=source_location(ctx))
    except ValueError:
        return unsupported(ctx)
    if ctx_has(literal, "FLOAT_LITERAL"):
        return data("FloatLiteral", value=float(clean_float(source)), source=source, location=source_location(ctx))
    if ctx_has(literal, "DOUBLE_LITERAL"):
        return data("DoubleLiteral", value=float(clean_double(source)), source=source, location=source_location(ctx))
    if ctx_has(literal, "BOOL_LITERAL"):
        return data("BoolLiteral", value=bool_value(source), source=source, location=source_location(ctx))
    if ctx_has(literal, "STRING_LITERAL"):
        return string_literal_expression(source, source_location(ctx))
    if ctx_has(literal, "REGEX_LITERAL"):
        return regex_literal_expression(source, source_location(ctx))
    return unsupported(ctx)


def regex_literal_expression(source, location):
    closing_slash = regex_literal_closing_slash(source)
    if closing_slash < len("regex/"):
        return unsupported_expr(source, location)
    pattern = unescape_regex_content(source[len("regex/") : closing_slash])
    flags = source[closing_slash + 1 :]
    return data(
        "DataLiteral",
        typeName="Regex",
        fields=[
            data("DataField", name="pattern", value=data("StringLiteral", value=pattern, source=quote(pattern), location=location), spread=False, location=location),
            data("DataField", name="flags", value=data("StringLiteral", value=flags, source=quote(flags), location=location), spread=False, location=location),
        ],
        location=location,
    )


def regex_literal_closing_slash(source):
    index = len(source) - 1
    while index >= 0 and source[index] in "ims":
        index -= 1
    return index


def unescape_regex_content(value):
    result = []
    index = 0
    while index < len(value):
        current = value[index]
        if current == "\\" and index + 1 < len(value) and value[index + 1] in ("/", "\\"):
            result.append(value[index + 1])
            index += 2
        else:
            result.append(current)
            index += 1
    return "".join(result)


def string_literal_expression(source, location):
    if not source.startswith('"'):
        return data("StringLiteral", value=unquote(source), source=source, location=location)
    return interpolated_string_literal_expression(source, location)


def interpolated_string_literal_expression(source, location):
    content = source[1:-1]
    parts = []
    segment = []
    segment_start = 0
    found_interpolation = False
    index = 0
    while index < len(content):
        current = content[index]
        if current == "\\" and index + 1 < len(content):
            if content[index + 1] == "{" and index + 2 < len(content) and content[index + 2] == "{":
                segment.append("{")
                index += 2
            else:
                segment.extend([current, content[index + 1]])
                index += 2
            continue
        if current == "{":
            end = interpolation_end(content, index + 1)
            expression_source = content[index + 1 : end] if end >= 0 else ""
            if end >= 0 and expression_source.strip() == expression_source and expression_source:
                if not parts and not segment:
                    parts.append(string_segment("", location, 0))
                else:
                    add_string_segment(parts, segment, location, segment_start)
                segment = []
                parts.append(interpolation_expression(expression_source, string_content_location(location, index + 1)))
                found_interpolation = True
                index = end + 1
                segment_start = index
                continue
        segment.append(current)
        index += 1
    if not found_interpolation:
        return data("StringLiteral", value=unquote(source), source=source, location=location)
    add_string_segment(parts, segment, location, segment_start)
    return concatenated(parts, location)


def interpolation_end(content, start):
    depth = 0
    quote_char = None
    index = start
    while index < len(content):
        current = content[index]
        if current == "\\":
            index += 2
            continue
        if quote_char is not None:
            if current == quote_char:
                quote_char = None
        elif current in ('"', "'"):
            quote_char = current
        elif current == "{":
            depth += 1
        elif current == "}":
            if depth == 0:
                return index
            depth -= 1
        index += 1
    return -1


def add_string_segment(parts, segment, literal_location, segment_start):
    if segment:
        parts.append(string_segment("".join(segment), literal_location, segment_start))


def string_segment(raw_segment, literal_location, segment_start):
    value = unescape_string_content(raw_segment)
    return data("StringLiteral", value=value, source=quote(value), location=string_content_location(literal_location, segment_start))


def string_content_location(literal_location, content_offset):
    return loc(literal_location["line"], literal_location["column"] + 1 + content_offset)


def concatenated(parts, location):
    if not parts:
        return data("StringLiteral", value="", source='""', location=location)
    result = parts[0]
    for part in parts[1:]:
        result = data("BinaryExpression", operator="+", left=result, right=part, location=location)
    return result


def interpolation_expression(source, location):
    lexer = FunctionalLexer(InputStream(source))
    tokens = CommonTokenStream(lexer)
    parser = FunctionalParser(tokens)
    errors = SyntaxErrorCollector()
    lexer.removeErrorListeners()
    parser.removeErrorListeners()
    lexer.addErrorListener(errors)
    parser.addErrorListener(errors)
    parsed_expression = parser.expression()
    if tokens.LA(1) != Token.EOF:
        token_index = tokens.index if isinstance(tokens.index, int) else tokens.index()
        errors.errors.append((1, tokens.get(token_index).column, "extraneous input"))
    if errors.errors:
        return unsupported_expr(source, location)
    return offset_expression(expression(parsed_expression), location["line"] - 1, location["column"])


def offset_expression(expr, line_offset, column_offset):
    kind = type_name(expr)
    result = dict(expr)
    if "location" in result:
        result["location"] = offset_location(result["location"], line_offset, column_offset)
    if kind == "IfExpression":
        result["condition"] = offset_expression(expr["condition"], line_offset, column_offset)
        result["thenBranch"] = offset_expression(expr["thenBranch"], line_offset, column_offset)
        result["elseBranch"] = offset_expression(expr["elseBranch"], line_offset, column_offset)
    elif kind in ("BinaryExpression",):
        result["left"] = offset_expression(expr["left"], line_offset, column_offset)
        result["right"] = offset_expression(expr["right"], line_offset, column_offset)
    elif kind == "UnaryExpression":
        result["expression"] = offset_expression(expr["expression"], line_offset, column_offset)
    elif kind in ("FunctionCallExpression", "MethodCallExpression"):
        if "receiver" in result:
            result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset)
        result["arguments"] = [offset_expression(arg, line_offset, column_offset) for arg in expr["arguments"]]
    elif kind == "WithExpression":
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset)
        result["fields"] = offset_data_fields(expr["fields"], line_offset, column_offset)
    elif kind == "LambdaExpression":
        result["body"] = offset_expression(expr["body"], line_offset, column_offset)
    elif kind in ("ListLiteral", "SetLiteral", "TupleLiteral"):
        result["values"] = [offset_expression(value_ctx, line_offset, column_offset) for value_ctx in expr["values"]]
    elif kind == "DictLiteral":
        result["entries"] = [
            data(
                "DictEntry",
                key=offset_expression(entry["key"], line_offset, column_offset),
                value=offset_expression(entry["value"], line_offset, column_offset),
                location=offset_location(entry["location"], line_offset, column_offset),
            )
            for entry in expr["entries"]
        ]
    elif kind == "IndexExpression":
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset)
        result["index"] = offset_expression(expr["index"], line_offset, column_offset)
        result["endIndex"] = offset_expression(expr["endIndex"], line_offset, column_offset)
    elif kind == "DataLiteral":
        result["fields"] = offset_data_fields(expr["fields"], line_offset, column_offset)
    elif kind == "FieldAccessExpression":
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset)
    elif kind == "ReduceExpression":
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset)
        result["initial"] = offset_expression(expr["initial"], line_offset, column_offset)
        result["body"] = offset_expression(expr["body"], line_offset, column_offset)
    elif kind == "MatchExpression":
        result["value"] = offset_expression(expr["value"], line_offset, column_offset)
        result["cases"] = [
            data(
                "MatchCase",
                typeName=case["typeName"],
                bindings=case["bindings"],
                bindsWholeValue=case["bindsWholeValue"],
                literal=offset_expression(case["literal"], line_offset, column_offset),
                hasLiteral=case["hasLiteral"],
                wildcard=case["wildcard"],
                guard=offset_expression(case["guard"], line_offset, column_offset),
                hasGuard=case["hasGuard"],
                body=offset_expression(case["body"], line_offset, column_offset),
                location=offset_location(case["location"], line_offset, column_offset),
            )
            for case in expr["cases"]
        ]
    elif kind == "BlockExpression":
        result["bindings"] = [
            data(
                "LetBinding",
                name=binding["name"],
                typeReference=binding["typeReference"],
                operator=binding["operator"],
                value=offset_expression(binding["value"], line_offset, column_offset),
                location=offset_location(binding["location"], line_offset, column_offset),
            )
            for binding in expr["bindings"]
        ]
        result["result"] = offset_expression(expr["result"], line_offset, column_offset)
    return result


def offset_data_fields(fields, line_offset, column_offset):
    return [
        data(
            "DataField",
            name=field["name"],
            value=offset_expression(field["value"], line_offset, column_offset),
            spread=field["spread"],
            location=offset_location(field["location"], line_offset, column_offset),
        )
        for field in fields
    ]


def offset_location(location, line_offset, column_offset):
    line = location["line"] + line_offset
    column = location["column"] + column_offset if location["line"] == 1 else location["column"]
    return loc(line, column)


def parse_type_reference(type_text):
    if type_text is None or type_text == "":
        return missing_type()
    if has_top_level_function_arrow(type_text):
        return type_ref(type_text, [])
    if type_text.endswith("[]"):
        return type_ref("array", [parse_type_reference(type_text[:-2])])
    bracket = top_level_generic_bracket(type_text)
    if bracket < 0:
        return type_ref(type_text, [])
    return type_ref(type_text[:bracket], [parse_type_reference(arg) for arg in split_type_arguments(type_text[bracket + 1 : -1])])


def has_top_level_function_arrow(type_text):
    bracket_depth = 0
    paren_depth = 0
    for index in range(len(type_text) - 1):
        char = type_text[index]
        if char == "[":
            bracket_depth += 1
        elif char == "]":
            bracket_depth -= 1
        elif char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth -= 1
        elif char == "=" and type_text[index + 1] == ">" and bracket_depth == 0 and paren_depth == 0:
            return True
    return False


def top_level_generic_bracket(type_text):
    paren_depth = 0
    for index, char in enumerate(type_text):
        if char == "(":
            paren_depth += 1
        elif char == ")":
            paren_depth -= 1
        elif char == "[" and paren_depth == 0:
            return index
    return -1


def split_type_arguments(type_text):
    args = []
    depth = 0
    start = 0
    for index, char in enumerate(type_text):
        if char == "[":
            depth += 1
        elif char == "]":
            depth -= 1
        elif char == "," and depth == 0:
            args.append(type_text[start:index].strip())
            start = index + 1
    tail = type_text[start:].strip()
    if tail:
        args.append(tail)
    return args


def is_index_expression(ctx):
    return ctx_has(ctx, "argumentList") and len(ctx_list(ctx, "expressionNoLet")) == 1 and has_child(ctx, "[")


def is_invocation_expression(ctx):
    return len(ctx_list(ctx, "expressionNoLet")) == 1 and has_child(ctx, "(")


def is_slice_expression(ctx):
    return len(ctx_list(ctx, "expressionNoLet")) == 1 and has_child(ctx, "[") and has_child(ctx, ":")


def is_slice_expression_no_pipe(ctx):
    return len(ctx_list(ctx, "expressionNoLetNoPipe")) == 1 and has_child(ctx, "[") and has_child(ctx, ":")


def is_invocation_expression_no_pipe(ctx):
    return len(ctx_list(ctx, "expressionNoLetNoPipe")) == 1 and has_child(ctx, "(")


def is_unary(ctx):
    return len(ctx_list(ctx, "expressionNoLet")) == 1 and first_child_text(ctx) in ("!", "-", ".not.")


def is_unary_no_pipe(ctx):
    return len(ctx_list(ctx, "expressionNoLetNoPipe")) == 1 and first_child_text(ctx) in ("!", "-", ".not.")


def unsupported(ctx):
    return unsupported_expr(text(ctx), source_location(ctx))


def object_expression(ctx):
    bindings = [object_let_expression_binding(let) for let in ctx_list(ctx, "letExpression")]
    result = object_expression_no_let(ctx_call(ctx, "expressionNoLet"))
    if not bindings:
        return result
    return data("BlockExpression", bindings=bindings, result=result, location=source_location(ctx))


def object_let_expression_binding(ctx):
    return data(
        "LetBinding",
        name=text(ctx_call(ctx, "identifier")),
        typeReference=parse_type_reference(text(ctx_call(ctx, "type"))) if ctx_has(ctx, "type") else missing_type(),
        operator=text(ctx_call(ctx, "letBindingOperator")),
        value=object_expression_no_let(ctx_call(ctx, "expressionNoLet")),
        location=source_location(ctx),
    )


def object_expression_no_let(ctx):
    if ctx_has(ctx, "ifExpression"):
        if_ctx = ctx_call(ctx, "ifExpression")
        return data(
            "IfExpression",
            condition=object_expression(ctx_item(if_ctx, "expression", 0)),
            thenBranch=object_expression(ctx_item(if_ctx, "expression", 1)),
            elseBranch=object_expression(ctx_item(if_ctx, "expression", 2)),
            location=source_location(if_ctx),
        )
    if ctx_has(ctx, "newData"):
        return object_data_literal(ctx_call(ctx, "newData"))
    if ctx_has(ctx, "constructorData"):
        return object_constructor_data_literal(ctx_call(ctx, "constructorData"))
    if ctx_has(ctx, "matchExpression"):
        return object_match_expression(ctx_call(ctx, "matchExpression"))
    if ctx_has(ctx, "value"):
        return object_value(ctx_call(ctx, "value"))
    if ctx_has(ctx, "expression"):
        return object_expression(ctx_call(ctx, "expression"))
    children = ctx_list(ctx, "expressionNoLet")
    if len(children) == 2 and ctx_has(ctx, "infixOperator"):
        return data(
            "BinaryExpression",
            operator=text(ctx_call(ctx, "infixOperator")),
            left=object_expression_no_let(children[0]),
            right=object_expression_no_let(children[1]),
            location=source_location(ctx),
        )
    if len(children) == 1 and ctx_has(ctx, "methodIdentifier"):
        return data(
            "MethodCallExpression",
            receiver=object_expression_no_let(children[0]),
            name=text(ctx_call(ctx, "methodIdentifier")),
            arguments=object_method_arguments(ctx_call(ctx, "methodArgumentList")),
            location=source_location(ctx),
        )
    if len(children) == 1 and ctx_has(ctx, "identifier") and not ctx_has(ctx, "methodIdentifier"):
        return data(
            "FieldAccessExpression",
            receiver=object_expression_no_let(children[0]),
            name=text(ctx_call(ctx, "identifier")),
            location=source_location(ctx),
        )
    if ctx_has(ctx, "functionCall"):
        return object_function_call(ctx_call(ctx, "functionCall"))
    if ctx_has(ctx, "thisExpression"):
        return data("VariableExpression", name="this", location=source_location(ctx))
    return unsupported(ctx)


def object_data_literal(ctx):
    type_name_value = text(ctx_call(ctx, "type"))
    if ctx_has(ctx, "BANG"):
        type_name_value = "__capy_raw|" + type_name_value
    return data("DataLiteral", typeName=type_name_value, fields=object_data_fields(ctx_call(ctx, "fieldAssignmentList")), location=source_location(ctx))


def object_constructor_data_literal(ctx):
    return data("DataLiteral", typeName="*", fields=object_data_fields(ctx_call(ctx, "fieldAssignmentList")), location=source_location(ctx))


def object_data_fields(ctx):
    if ctx is None:
        return []
    fields = []
    positional_index = 0
    for assignment in ctx_list(ctx, "fieldAssignment"):
        if ctx_has(assignment, "namedFieldAssignment"):
            fields.append(object_named_data_field(ctx_call(assignment, "namedFieldAssignment")))
            positional_index += 1
        elif ctx_has(assignment, "spreadFieldAssignment"):
            fields.append(
                data(
                    "DataField",
                    name="",
                    value=object_expression(ctx_call(ctx_call(assignment, "spreadFieldAssignment"), "expression")),
                    spread=True,
                    location=source_location(assignment),
                )
            )
        elif ctx_has(assignment, "positionalFieldAssignment"):
            fields.append(
                data(
                    "DataField",
                    name="$" + str(positional_index),
                    value=object_expression(ctx_call(ctx_call(assignment, "positionalFieldAssignment"), "expression")),
                    spread=False,
                    location=source_location(assignment),
                )
            )
            positional_index += 1
        else:
            fields.append(data("DataField", name="$unsupported", value=unsupported(assignment), spread=False, location=source_location(assignment)))
            positional_index += 1
    return fields


def object_named_data_field(ctx):
    name = text(ctx_call(ctx, "identifier")) if ctx_has(ctx, "identifier") else unquote(text(ctx_call(ctx, "STRING_LITERAL")))
    return data("DataField", name=name, value=object_expression(ctx_call(ctx, "expression")), spread=False, location=source_location(ctx))


def object_match_expression(ctx):
    cases = []
    for case_list in ctx_list(ctx, "matchCaseList"):
        for match_case_ctx in ctx_list(case_list, "matchCase"):
            cases.extend(object_match_cases(match_case_ctx))
    return data("MatchExpression", value=object_expression(ctx_call(ctx, "expression")), cases=cases, location=source_location(ctx))


def object_match_cases(ctx):
    patterns = ctx_list(ctx, "pattern")
    if not patterns:
        return [object_match_case(ctx, None)]
    return [object_match_case(ctx, pattern) for pattern in patterns]


def object_match_case(ctx, pattern):
    guard = getattr(ctx, "guard", None)
    body = getattr(ctx, "body", None)
    return data(
        "MatchCase",
        typeName=object_pattern_type_name(pattern),
        bindings=object_pattern_bindings(pattern),
        bindsWholeValue=object_pattern_binds_whole_value(pattern),
        literal=object_pattern_literal(pattern),
        hasLiteral=object_pattern_has_literal(pattern),
        wildcard=object_pattern_wildcard(pattern),
        guard=unsupported(ctx) if guard is None else object_expression(guard),
        hasGuard=guard is not None,
        body=object_expression(body),
        location=source_location(ctx),
    )


def object_pattern_type_name(ctx):
    return pattern_type_name(ctx)


def object_pattern_bindings(ctx):
    return pattern_bindings(ctx, True)


def object_pattern_wildcard(ctx):
    return pattern_wildcard(ctx)


def object_pattern_binds_whole_value(ctx):
    return pattern_binds_whole_value(ctx)


def object_pattern_has_literal(ctx):
    return pattern_has_literal(ctx)


def object_pattern_literal(ctx):
    return pattern_literal(ctx)


def object_call_expression(ctx):
    if ctx_has(ctx, "functionCall"):
        return object_function_call(ctx_call(ctx, "functionCall"))
    if ctx_has(ctx, "methodIdentifier"):
        if ctx_has(ctx, "thisExpression"):
            receiver = data("VariableExpression", name="this", location=source_location(ctx_call(ctx, "thisExpression")))
        elif ctx_has(ctx, "value"):
            receiver = object_value(ctx_call(ctx, "value"))
        elif ctx_has(ctx, "callExpression"):
            receiver = object_call_expression(ctx_call(ctx, "callExpression"))
        else:
            receiver = unsupported(ctx)
        return data(
            "MethodCallExpression",
            receiver=receiver,
            name=text(ctx_call(ctx, "methodIdentifier")),
            arguments=object_method_arguments(ctx_call(ctx, "methodArgumentList")),
            location=source_location(ctx),
        )
    if ctx_has(ctx, "callExpression") and ctx_has(ctx, "argumentList"):
        receiver = object_call_expression(ctx_call(ctx, "callExpression"))
        if type_name(receiver) == "VariableExpression":
            return data("FunctionCallExpression", name=receiver["name"], arguments=object_arguments(ctx_call(ctx, "argumentList")), location=source_location(ctx))
        return data(
            "MethodCallExpression",
            receiver=receiver,
            name="__capy_call",
            arguments=object_arguments(ctx_call(ctx, "argumentList")),
            location=source_location(ctx),
        )
    return unsupported(ctx)


def object_function_call(ctx):
    return data("FunctionCallExpression", name=function_call_name(text(ctx)), arguments=object_arguments(ctx_call(ctx, "argumentList")), location=source_location(ctx))


def function_call_name(source):
    arguments_start = source.find("(")
    return source if arguments_start < 0 else source[:arguments_start]


def object_arguments(ctx):
    if ctx is None:
        return []
    return [object_expression(argument) for argument in ctx_list(ctx, "expression")]


def object_method_arguments(ctx):
    if ctx is None:
        return []
    result = []
    for argument in ctx_list(ctx, "methodArgument"):
        if ctx_has(argument, "namedMethodArgument"):
            result.append(object_expression(ctx_call(ctx_call(argument, "namedMethodArgument"), "expression")))
        else:
            result.append(object_expression(ctx_call(argument, "expression")))
    return result


def object_value(ctx):
    if ctx_has(ctx, "literal"):
        return object_literal(ctx_call(ctx, "literal"))
    if ctx_has(ctx, "identifier"):
        return data("VariableExpression", name=text(ctx_call(ctx, "identifier")), location=source_location(ctx))
    if ctx_has(ctx, "qualifiedType"):
        return data("VariableExpression", name=text(ctx_call(ctx, "qualifiedType")), location=source_location(ctx))
    return unsupported(ctx)


def object_literal(ctx):
    source = text(ctx)
    location = source_location(ctx)
    if ctx_has(ctx, "INT_LITERAL"):
        return data("IntLiteral", value=int(clean_number(source)), source=source, location=location)
    if ctx_has(ctx, "LONG_LITERAL"):
        return data("LongLiteral", value=int(clean_number(source[:-1])), source=source, location=location)
    if ctx_has(ctx, "FLOAT_LITERAL"):
        return data("FloatLiteral", value=float(clean_float(source)), source=source, location=location)
    if ctx_has(ctx, "DOUBLE_LITERAL"):
        return data("DoubleLiteral", value=float(clean_double(source)), source=source, location=location)
    if ctx_has(ctx, "BOOL_LITERAL"):
        return data("BoolLiteral", value=bool_value(source), source=source, location=location)
    if ctx_has(ctx, "STRING_LITERAL"):
        return data("StringLiteral", value=unquote(source), source=source, location=location)
    return unsupported(ctx)
