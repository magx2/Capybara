'use strict';

const antlr4Package = require('antlr4');
const antlr4 = antlr4Package.default || antlr4Package;
const FunctionalLexer = require('FunctionalLexer');
const FunctionalParser = require('FunctionalParser');
const ObjectOrientedLexer = require('ObjectOrientedLexer');
const ObjectOrientedParser = require('ObjectOrientedParser');
const os = require('os');

const MODULE_NAME_PATTERN = '[A-Za-z_][A-Za-z0-9_]*|\/[A-Za-z_][a-zA-Z0-9_]*(?:\/[A-Za-z_][a-zA-Z0-9_]*)+';
const FROM_IMPORT_PATTERN = new RegExp(
    '^\\s*from\\s+(' + MODULE_NAME_PATTERN + ')\\s+import\\s*\\{\\s*([^}]*)\\s*\\}(?:\\s+except\\s*\\{\\s*([^}]*)\\s*\\})?\\s*$'
);
const QUALIFIED_IMPORT_PATTERN = new RegExp('^\\s*import\\s+(' + MODULE_NAME_PATTERN + ')\\s*$');

function pySlice(value, start, end) { return value.slice(start == null ? 0 : start, end == null ? undefined : end); }
function pyEqual(left, right) { return Object.is(left, right) || JSON.stringify(left) === JSON.stringify(right); }
function pyIn(left, right) { if (typeof right === 'string') return right.includes(left); if (Array.isArray(right)) return right.some((item) => pyEqual(item, left)); return left in right; }
function pyLen(value) { return typeof value === 'string' || Array.isArray(value) ? value.length : value.size; }
function pyTruthy(value) { if (value == null || value === false) return false; if (Array.isArray(value) || typeof value === 'string') return value.length > 0; return true; }
function isPlainObject(value) { return value !== null && typeof value === 'object' && !Array.isArray(value); }
function pyRange(stop) { return Array.from({ length: Math.max(0, stop) }, (_, index) => index); }

function data(typeName, fields = {}) {
    return Object.assign({ __type: typeName }, fields);
}

function parsed_program(modules) {
    const result = data("ParsedProgram", { modules: modules });
    // Generated JavaScript compiler code maps this value inside an Effect chain.
    Object.defineProperty(result, "map", {
        enumerable: false,
        value: (mapper) => mapper(result),
    });
    return result;
}

function option_some(value) {
    return data("Some", { value: value });
}

function option_none() {
    return data("None", {  });
}

function loc(line, column) {
    return data("SourceLocation", { line: line, column: column });
}

function type_ref(name, argumentValues) {
    return data("TypeReference", { name: name, arguments: Array.from((argumentValues || [])) });
}

function missing_type() {
    return type_ref("", []);
}

function string_type() {
    return type_ref("String", []);
}

function unsupported_expr(source, location) {
    return data("UnsupportedExpression", { source: source, location: location });
}

function unsupported_literal() {
    return unsupported_expr("", loc(0, 0));
}

function text(node) {
    return ((node === null) ? "" : node.getText());
}

function ctx_call(ctx, name, ...args) {
    if (ctx == null) return null;
    var attr = ctx[name];
    if (attr == null) attr = ctx[name + "_"];
    if (attr == null) return null;
    return typeof attr === "function" ? attr.apply(ctx, args) : attr;
}

function ctx_list(ctx, name) {
    var value = ctx_call(ctx, name);
    if (pyTruthy((value === null))) {
        return [];
    }
    if (pyTruthy(Array.isArray(value))) {
        return value;
    }
    return [value];
}

function ctx_item(ctx, name, index) {
    try {
        return ctx_call(ctx, name, index);
    } catch (_) {
        var values = ctx_list(ctx, name);
        return ((index < pyLen(values)) ? values[index] : null);
    }
}

function ctx_has(ctx, name) {
    var value = ctx_call(ctx, name);
    if (pyTruthy((value === null))) {
        return false;
    }
    if (pyTruthy(Array.isArray(value))) {
        return pyTruthy(value);
    }
    return true;
}

function source_location(ctx) {
    var start = ((ctx != null && "start" in ctx) ? ctx.start : ctx.getStart());
    return loc(start.line, start.column);
}

function token_location(token) {
    var symbol = ((token != null && "symbol" in token) ? token.symbol : token);
    return loc(symbol.line, symbol.column);
}

function leading_whitespace(line) {
    var index = 0;
    while (pyTruthy(((index < pyLen(line)) && /\s/.test(line[index])))) {
        index += 1;
    }
    return index;
}

function clean_number(value) {
    return value.split("_").join("");
}

function clean_float(value) {
    var cleaned = clean_number(value);
    return ((pyIn(pySlice(cleaned, -1, null), ["f", "F"])) ? pySlice(cleaned, null, -1) : cleaned);
}

function clean_double(value) {
    var cleaned = clean_number(value);
    return ((pyIn(pySlice(cleaned, -1, null), ["d", "D"])) ? pySlice(cleaned, null, -1) : cleaned);
}

function unescape_string_content(value) {
    return value.split("\\\"").join("\"").split("\\'").join("'").split("\\n").join("\n").split("\\r").join("\r").split("\\t").join("\t").split("\\\\").join("\\");
}

function unquote(value) {
    if (pyTruthy((pyLen(value) < 2))) {
        return value;
    }
    return unescape_string_content(pySlice(value, 1, -1));
}

function quote(value) {
    return (("\"" + value.split("\\").join("\\\\").split("\"").join("\\\"")) + "\"");
}

function bool_value(source) {
    return (source === "true");
}

function has_child(ctx, child_text) {
    return Array.from(pyRange(ctx.getChildCount())).some((i) => (ctx.getChild(i).getText() === child_text));
}

function first_child_text(ctx) {
    return ((ctx.getChildCount() === 0) ? "" : ctx.getChild(0).getText());
}

function is_grouped(ctx) {
    if (pyTruthy((ctx.getChildCount() !== 3))) {
        return false;
    }
    return (pyIn([ctx.getChild(0).getText(), ctx.getChild(2).getText()], [["(", ")"], ["{", "}"]]));
}

function precedence(operator) {
    if (pyTruthy((operator === "^"))) {
        return 4;
    }
    if (pyTruthy((pyIn(operator, ["*", "/", "%"])))) {
        return 3;
    }
    if (pyTruthy((pyIn(operator, ["+", "-"])))) {
        return 2;
    }
    if (pyTruthy((pyIn(operator, [">", "<", ">=", "<=", "==", "!="])))) {
        return 1;
    }
    return 0;
}

function binary_expression(operator, left, right, location, left_grouped) {
    if (pyTruthy((!pyTruthy(left_grouped) && (type_name(left) === "BinaryExpression") && (precedence(operator) > precedence(left["operator"]))))) {
        return data("BinaryExpression", { operator: left["operator"], left: left["left"], right: binary_expression(operator, left["right"], right, location, false), location: left["location"] });
    }
    return data("BinaryExpression", { operator: operator, left: left, right: right, location: location });
}

function type_name(value) {
    if (pyTruthy(isPlainObject(value))) {
        return (value["__type"] ?? "");
    }
    return String(value);
}

function source_kind_name(value) {
    return type_name(value);
}

function module_file(module) {
    var extension = ((source_kind_name(module["sourceKind"]) === "OBJECT_ORIENTED") ? ".coo" : ".cfun");
    var prefix = (!pyTruthy((module["path"] ?? undefined)) ? "" : (module["path"] + "/"));
    return ((prefix + module["name"]) + extension);
}

class SyntaxErrorCollector extends (antlr4.error.ErrorListener || antlr4Package.ErrorListener) {
    constructor() {
        super();
        this.errors = [];
    }
    syntaxError(recognizer, offendingSymbol, line, column, msg, e) {
        this.errors.push([line, column, msg]);
    }
}

function throw_if_invalid(module, errors) {
    if (pyTruthy(!pyTruthy(errors))) {
        return;
    }
    var [line, column, message] = errors[0];
    throw Error(`${module_file(module)}:${line}:${column}: ParserError: ${message}`);
}

function strip_imports(source) {
    var imports = [];
    var body = [];
    var __index_2 = 0;
    for (const line of source.split(/\r\n|\r|\n/)) {
        const line_number = __index_2 + 1;
        __index_2++;
        var from_import = line.match(FROM_IMPORT_PATTERN);
        var qualified_import = line.match(QUALIFIED_IMPORT_PATTERN);
        if (pyTruthy(from_import)) {
            imports.push(data("ImportDeclaration", { modulePath: (from_import[1] ?? null), importedNames: import_names((from_import[2] ?? null)), excludedNames: import_names((from_import[3] ?? null)), wildcard: import_wildcard((from_import[2] ?? null)), qualified: false, location: loc(line_number, leading_whitespace(line)) }));
            body.push("");
        } else if (pyTruthy(qualified_import)) {
            imports.push(data("ImportDeclaration", { modulePath: (qualified_import[1] ?? null), importedNames: [], excludedNames: [], wildcard: false, qualified: true, location: loc(line_number, leading_whitespace(line)) }));
            body.push("");
        } else {
            body.push(line);
        }
    }
    return [body.join(os.EOL), imports];
}

function import_names(raw_names) {
    if (pyTruthy(((raw_names === null) || import_wildcard(raw_names)))) {
        return [];
    }
    return Array.from(Array.from(raw_names.split(",")).map((part) => part.trim())).filter((name) => name).map((name) => name);
}

function import_wildcard(raw_names) {
    return (raw_names.trim() === "*");
}

// @NativeImplementation(interface_id="dev.capylang.compiler.parser.CapybaraParser", class_name="NativeCapybaraParser")
class NativeCapybaraParser {
    parse(modules) {
        return parsed_program(Array.from(modules).map((module) => this.parse_module(module)));
    }
    parse_module(module) {
        var [body, imports] = strip_imports(module["input"]);
        if (pyTruthy((source_kind_name(module["sourceKind"]) === "OBJECT_ORIENTED"))) {
            var definitions = [];
            var object_oriented = this.parse_object_oriented(module, body);
        } else {
            var definitions = this.parse_functional(module, body);
            var object_oriented = data("ObjectOriented", { interfaces: [], classes: [] });
        }
        var functional = data("Functional", { definitions: definitions });
        return data("ParsedModule", { name: module["name"], path: module["path"], functional: functional, definitions: definitions, objectOriented: object_oriented, imports: imports, sourceKind: module["sourceKind"] });
    }
    parse_functional(module, source) {
        var lexer = new FunctionalLexer(new antlr4.InputStream(source));
        var tokens = new antlr4.CommonTokenStream(lexer);
        var parser = new FunctionalParser(tokens);
        var errors = new SyntaxErrorCollector();
        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(errors);
        parser.addErrorListener(errors);
        var program = parser.program();
        throw_if_invalid(module, errors.errors);
        var definitions = [];
        for (const definition of ctx_list(program, "definition")) {
            if (pyTruthy(ctx_has(definition, "dataDeclaration"))) {
                definitions.push(...data_declaration_definitions(ctx_call(definition, "dataDeclaration")));
            } else if (pyTruthy(ctx_has(definition, "typeDeclaration"))) {
                definitions.push(...type_declaration_definitions(ctx_call(definition, "typeDeclaration")));
            } else if (pyTruthy(ctx_has(definition, "primitiveBackedTypeDeclaration"))) {
                definitions.push(...primitive_backed_type_declaration_definitions(ctx_call(definition, "primitiveBackedTypeDeclaration")));
            } else if (pyTruthy(ctx_has(definition, "functionDeclaration"))) {
                definitions.push(...function_definitions(ctx_call(definition, "functionDeclaration")));
            } else {
                definitions.push(functional_definition(definition));
            }
        }
        return definitions;
    }
    parse_object_oriented(module, source) {
        var lexer = new ObjectOrientedLexer(new antlr4.InputStream(source));
        var tokens = new antlr4.CommonTokenStream(lexer);
        var parser = new ObjectOrientedParser(tokens);
        var errors = new SyntaxErrorCollector();
        lexer.removeErrorListeners();
        parser.removeErrorListeners();
        lexer.addErrorListener(errors);
        parser.addErrorListener(errors);
        var program = parser.program();
        throw_if_invalid(module, errors.errors);
        var interfaces = [];
        var classes = [];
        for (const definition of ctx_list(program, "definition")) {
            if (pyTruthy(ctx_has(definition, "classDeclaration"))) {
                classes.push(object_oriented_class(ctx_call(definition, "classDeclaration")));
            } else if (pyTruthy(ctx_has(definition, "interfaceDeclaration"))) {
                interfaces.push(object_oriented_interface(ctx_call(definition, "interfaceDeclaration")));
            } else if (pyTruthy(ctx_has(definition, "traitDeclaration"))) {
                interfaces.push(object_oriented_trait(ctx_call(definition, "traitDeclaration")));
            }
        }
        return data("ObjectOriented", { interfaces: interfaces, classes: classes });
    }
}

function functional_definition(definition) {
    if (pyTruthy(ctx_has(definition, "annotationDeclaration"))) {
        return annotation_declaration(ctx_call(definition, "annotationDeclaration"));
    }
    if (pyTruthy(ctx_has(definition, "deriverDeclaration"))) {
        return deriver_declaration(ctx_call(definition, "deriverDeclaration"));
    }
    if (pyTruthy(ctx_has(definition, "enumDeclaration"))) {
        return enum_declaration(ctx_call(definition, "enumDeclaration"));
    }
    if (pyTruthy(ctx_has(definition, "constDeclaration"))) {
        return data("ConstantDefinition", { constant: constant_declaration(ctx_call(definition, "constDeclaration")) });
    }
    if (pyTruthy(ctx_has(definition, "primitiveBackedTypeDeclaration"))) {
        return data("PrimitiveBackedTypeDeclaration", {  });
    }
    return data("UnsupportedDefinition", {  });
}

function object_oriented_class(ctx) {
    var fields = [];
    var init_blocks = [];
    var methods = [];
    for (const member of ctx_list(ctx_call(ctx, "typeBody"), "memberDeclaration")) {
        if (pyTruthy(ctx_has(member, "fieldDeclaration"))) {
            fields.push(object_oriented_field(ctx_call(member, "fieldDeclaration")));
        } else if (pyTruthy(ctx_has(member, "initBlock"))) {
            init_blocks.push(object_statement_block(ctx_call(ctx_call(member, "initBlock"), "statementBlock")));
        } else if (pyTruthy(ctx_has(member, "methodDeclaration"))) {
            methods.push(object_oriented_method(ctx_call(member, "methodDeclaration")));
        }
    }
    return data("ObjectOrientedClass", { name: text(ctx_call(ctx, "TYPE")), visibility: "public", open: object_class_open(ctx_list(ctx, "classModifier")), parameters: object_constructor_parameters(ctx_call(ctx, "constructorParameters")), parents: object_parents(ctx_call(ctx, "inheritanceClause")), fields: fields, initBlocks: init_blocks, methods: methods, annotations: object_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function object_class_open(modifiers) {
    return Array.from(modifiers).some((modifier) => (pyIn(text(modifier), ["open", "abstract"])));
}

function object_oriented_interface(ctx) {
    var methods = [];
    for (const member of ctx_list(ctx_call(ctx, "interfaceBody"), "interfaceMemberDeclaration")) {
        if (pyTruthy(ctx_has(member, "interfaceMethodDeclaration"))) {
            methods.push(object_oriented_interface_method(ctx_call(member, "interfaceMethodDeclaration")));
        }
    }
    return data("ObjectOrientedInterface", { name: text(ctx_call(ctx, "TYPE")), visibility: "public", kind: "interface", parents: object_parents(ctx_call(ctx, "inheritanceClause")), methods: methods, annotations: object_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function object_oriented_trait(ctx) {
    var methods = [];
    for (const member of ctx_list(ctx_call(ctx, "typeBody"), "memberDeclaration")) {
        if (pyTruthy(ctx_has(member, "methodDeclaration"))) {
            methods.push(object_oriented_method(ctx_call(member, "methodDeclaration")));
        }
    }
    return data("ObjectOrientedInterface", { name: text(ctx_call(ctx, "TYPE")), visibility: "public", kind: "trait", parents: object_parents(ctx_call(ctx, "inheritanceClause")), methods: methods, annotations: object_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function object_parents(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "qualifiedType")).map((parent) => parse_type_reference(text(parent)));
}

function object_oriented_field(ctx) {
    var visibility = (ctx_has(ctx, "visibility") ? text(ctx_call(ctx, "visibility")) : "public");
    var has_value = ctx_has(ctx, "expression");
    return data("ObjectOrientedField", { name: text(ctx_call(ctx, "identifier")), visibility: visibility, typeReference: parse_type_reference(text(ctx_call(ctx, "type"))), value: (has_value ? object_expression(ctx_call(ctx, "expression")) : unsupported(ctx)), hasValue: has_value, annotations: object_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function object_constructor_parameters(ctx) {
    if (pyTruthy(((ctx === null) || !pyTruthy(ctx_has(ctx, "parameters"))))) {
        return [];
    }
    return object_parameters(ctx_call(ctx, "parameters"));
}

function object_oriented_method(ctx) {
    var visibility = (ctx_has(ctx, "visibility") ? text(ctx_call(ctx, "visibility")) : "public");
    return data("ObjectOrientedMethod", { name: text(ctx_call(ctx, "identifier")), visibility: visibility, parameters: (ctx_has(ctx, "parameters") ? object_parameters(ctx_call(ctx, "parameters")) : []), returnType: (ctx_has(ctx, "functionType") ? parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type"))) : missing_type()), body: object_method_body(ctx_call(ctx, "methodBody")), annotations: object_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function object_oriented_interface_method(ctx) {
    var visibility = (ctx_has(ctx, "visibility") ? text(ctx_call(ctx, "visibility")) : "public");
    return data("ObjectOrientedMethod", { name: text(ctx_call(ctx, "identifier")), visibility: visibility, parameters: (ctx_has(ctx, "parameters") ? object_parameters(ctx_call(ctx, "parameters")) : []), returnType: (ctx_has(ctx, "functionType") ? parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type"))) : missing_type()), body: unsupported(ctx), annotations: object_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function object_parameters(ctx) {
    return Array.from(ctx_list(ctx, "parameter")).map((parameter) => data("FunctionParameter", { name: text(ctx_call(parameter, "identifier")), typeReference: parse_type_reference(text(ctx_call(parameter, "type"))), location: source_location(parameter) }));
}

function object_method_body(ctx) {
    if (pyTruthy((ctx === null))) {
        return unsupported_expr("", loc(0, 0));
    }
    if (pyTruthy(ctx_has(ctx, "expression"))) {
        return object_expression(ctx_call(ctx, "expression"));
    }
    return object_statement_block(ctx_call(ctx, "statementBlock"));
}

function object_statement_block(ctx) {
    var bindings = [];
    var statements = ctx_list(ctx, "statement");
    var __index_4 = 0;
    for (const statement of statements) {
        const index = __index_4 + 0;
        __index_4++;
        if (pyTruthy(ctx_has(statement, "letStatement"))) {
            bindings.push(object_let_statement_binding(ctx_call(statement, "letStatement")));
        } else if (pyTruthy((index === (pyLen(statements) - 1)))) {
            var result = object_statement(statement);
            if (pyTruthy(!pyTruthy(bindings))) {
                return result;
            }
            return data("BlockExpression", { bindings: bindings, result: result, location: source_location(ctx) });
        } else {
            return unsupported(ctx);
        }
    }
    return unsupported(ctx);
}

function object_statement(ctx) {
    if (pyTruthy(ctx_has(ctx, "returnStatement"))) {
        return object_expression(ctx_call(ctx_call(ctx, "returnStatement"), "expression"));
    }
    if (pyTruthy(ctx_has(ctx, "ifStatement"))) {
        return object_if_statement(ctx_call(ctx, "ifStatement"));
    }
    if (pyTruthy(ctx_has(ctx, "statementBlock"))) {
        return object_statement_block(ctx_call(ctx, "statementBlock"));
    }
    if (pyTruthy(ctx_has(ctx, "expressionStatement"))) {
        return object_call_expression(ctx_call(ctx_call(ctx, "expressionStatement"), "callExpression"));
    }
    return unsupported(ctx);
}

function object_let_statement_binding(ctx) {
    return data("LetBinding", { name: text(ctx_call(ctx, "identifier")), typeReference: (ctx_has(ctx, "type") ? parse_type_reference(text(ctx_call(ctx, "type"))) : missing_type()), operator: text(ctx_call(ctx, "letBindingOperator")), value: object_expression(ctx_call(ctx, "expression")), location: source_location(ctx) });
}

function object_if_statement(ctx) {
    var else_branch = object_else_branch(ctx);
    if (pyTruthy((else_branch === null))) {
        return unsupported(ctx);
    }
    return data("IfExpression", { condition: object_expression(ctx_call(ctx, "expression")), thenBranch: object_statement_block(ctx_item(ctx, "statementBlock", 0)), elseBranch: else_branch, location: source_location(ctx) });
}

function object_else_branch(ctx) {
    if (pyTruthy(ctx_has(ctx, "ifStatement"))) {
        return object_if_statement(ctx_call(ctx, "ifStatement"));
    }
    var blocks = ctx_list(ctx, "statementBlock");
    if (pyTruthy((pyLen(blocks) > 1))) {
        return object_statement_block(blocks[1]);
    }
    return null;
}

function deriver_declaration(ctx) {
    return data("DeriverDeclaration", { name: text(ctx_call(ctx, "TYPE")), visibility: (ctx_has(ctx, "VISIBILITY") ? text(ctx_call(ctx, "VISIBILITY")) : "public"), methods: Array.from(ctx_list(ctx, "deriverMethodDeclaration")).map((method) => deriver_method_declaration(method)), annotations: definition_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function deriver_method_declaration(ctx) {
    return data("FunctionDeclaration", { name: text(ctx_call(ctx, "identifier")), visibility: "public", parameters: (ctx_has(ctx, "parameters") ? Array.from(ctx_list(ctx_call(ctx, "parameters"), "parameter")).map((parameter) => function_parameter(parameter)) : []), returnType: parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type"))), body: expression(ctx_call(ctx, "expression")), annotations: annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function annotation_declaration(ctx) {
    return data("AnnotationDeclaration", { name: text(ctx_call(ctx, "TYPE")), visibility: (ctx_has(ctx, "VISIBILITY") ? text(ctx_call(ctx, "VISIBILITY")) : "public"), repeatable: ctx_has(ctx, "multipleModifier"), targets: Array.from(ctx_list(ctx_call(ctx, "annotationTargetClause"), "annotationTarget")).map((target) => text(target)), fields: annotation_fields(ctx_call(ctx, "annotationBody")), annotations: definition_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function annotation_fields(ctx) {
    return Array.from(ctx_list(ctx, "annotationFieldDeclaration")).map((field) => annotation_field(field));
}

function annotation_field(ctx) {
    var has_default = ctx_has(ctx, "annotationValue");
    return data("AnnotationFieldDeclaration", { name: text(ctx_call(ctx, "identifier")), typeReference: type_ref(text(ctx_call(ctx_call(ctx, "annotationFieldType"), "annotationTypeReference")), []), defaultValue: (has_default ? definition_annotation_value(ctx_call(ctx, "annotationValue")) : data("AnnotationNothingValue", { source: "", location: source_location(ctx) })), hasDefault: has_default, location: source_location(ctx) });
}

function definition_annotation_applications(blocks) {
    return Array.from(blocks).map((block) => definition_annotation_application(block));
}

function definition_annotation_application(ctx) {
    return data("AnnotationApplication", { name: text(ctx_call(ctx, "annotationName")), arguments: definition_annotation_arguments(ctx_call(ctx, "annotationArgumentList")), location: source_location(ctx) });
}

function definition_annotation_arguments(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "annotationArgument")).map((argument) => data("AnnotationArgument", { name: text(ctx_call(argument, "identifier")), value: definition_annotation_value(ctx_call(argument, "annotationValue")), location: source_location(argument) }));
}

function definition_annotation_value(ctx) {
    var source = text(ctx);
    var location = source_location(ctx);
    if (pyTruthy(ctx_has(ctx, "STRING_LITERAL"))) {
        return data("AnnotationStringValue", { value: unquote(source), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "INT_LITERAL"))) {
        return data("AnnotationIntValue", { value: Number.parseInt(clean_number(source), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "LONG_LITERAL"))) {
        return data("AnnotationLongValue", { value: Number.parseInt(clean_number(pySlice(source, null, -1)), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "DOUBLE_LITERAL"))) {
        return data("AnnotationDoubleValue", { value: Number.parseFloat(clean_double(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "FLOAT_LITERAL"))) {
        return data("AnnotationFloatValue", { value: Number.parseFloat(clean_float(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "BOOL_LITERAL"))) {
        return data("AnnotationBoolValue", { value: bool_value(source), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "NOTHING_LITERAL"))) {
        return data("AnnotationNothingValue", { source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "annotationTypeReference"))) {
        return data("AnnotationTypeNameValue", { value: text(ctx_call(ctx, "annotationTypeReference")), source: source, location: location });
    }
    return data("AnnotationNothingValue", { source: source, location: location });
}

function annotation_applications(blocks) {
    return Array.from(blocks).map((block) => annotation_application(block));
}

function annotation_application(ctx) {
    return data("FunctionAnnotationApplication", { name: text(ctx_call(ctx, "annotationName")), arguments: annotation_arguments(ctx_call(ctx, "annotationArgumentList")), location: source_location(ctx) });
}

function annotation_arguments(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "annotationArgument")).map((argument) => data("FunctionAnnotationArgument", { name: text(ctx_call(argument, "identifier")), value: annotation_value(ctx_call(argument, "annotationValue")), location: source_location(argument) }));
}

function annotation_value(ctx) {
    var source = text(ctx);
    var location = source_location(ctx);
    if (pyTruthy(ctx_has(ctx, "STRING_LITERAL"))) {
        return data("FunctionAnnotationStringValue", { value: unquote(source), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "INT_LITERAL"))) {
        return data("FunctionAnnotationIntValue", { value: Number.parseInt(clean_number(source), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "LONG_LITERAL"))) {
        return data("FunctionAnnotationLongValue", { value: Number.parseInt(clean_number(pySlice(source, null, -1)), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "DOUBLE_LITERAL"))) {
        return data("FunctionAnnotationDoubleValue", { value: Number.parseFloat(clean_double(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "FLOAT_LITERAL"))) {
        return data("FunctionAnnotationFloatValue", { value: Number.parseFloat(clean_float(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "BOOL_LITERAL"))) {
        return data("FunctionAnnotationBoolValue", { value: bool_value(source), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "NOTHING_LITERAL"))) {
        return data("FunctionAnnotationNothingValue", { source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "annotationTypeReference"))) {
        return data("FunctionAnnotationTypeNameValue", { value: text(ctx_call(ctx, "annotationTypeReference")), source: source, location: location });
    }
    return data("FunctionAnnotationNothingValue", { source: source, location: location });
}

function object_annotation_applications(blocks) {
    return Array.from(blocks).map((block) => object_annotation_application(block));
}

function object_annotation_application(ctx) {
    return data("FunctionAnnotationApplication", { name: text(ctx_call(ctx, "annotationName")), arguments: object_annotation_arguments(ctx_call(ctx, "annotationArgumentList")), location: source_location(ctx) });
}

function object_annotation_arguments(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "annotationArgument")).map((argument) => data("FunctionAnnotationArgument", { name: text(ctx_call(argument, "identifier")), value: object_annotation_value(ctx_call(argument, "annotationValue")), location: source_location(argument) }));
}

function object_annotation_value(ctx) {
    return annotation_value(ctx);
}

function constant_declaration(ctx) {
    return data("ConstantDeclaration", { name: text(ctx_call(ctx, "TYPE")), visibility: (ctx_has(ctx, "VISIBILITY") ? text(ctx_call(ctx, "VISIBILITY")) : "public"), typeReference: (ctx_has(ctx, "type") ? parse_type_reference(text(ctx_call(ctx, "type"))) : missing_type()), expression: expression_no_let(ctx_call(ctx, "expressionNoLet")), location: source_location(ctx) });
}

function enum_declaration(ctx) {
    var types = ctx_list(ctx, "TYPE");
    var values = Array.from(pySlice(types, 1, null)).map((token) => data("EnumValueDeclaration", { name: text(token), location: token_location(token) }));
    return data("EnumDeclaration", { name: (types ? text(types[0]) : ""), values: values, annotations: definition_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function function_definitions(ctx) {
    var definitions = [];
    var local_names = local_function_names(ctx);
    var body = ctx_call(ctx, "functionBody");
    if (pyTruthy((body !== null))) {
        for (const definition of ctx_list(body, "localDefinition")) {
            if (pyTruthy(ctx_has(definition, "localDataDeclaration"))) {
                definitions.push(...local_data_declaration_definitions(ctx_call(definition, "localDataDeclaration")));
            } else if (pyTruthy(ctx_has(definition, "localTypeDeclaration"))) {
                definitions.push(...local_type_declaration_definitions(ctx_call(definition, "localTypeDeclaration")));
            }
        }
        for (const definition of ctx_list(body, "localDefinition")) {
            if (pyTruthy(ctx_has(definition, "localFunctionDeclaration"))) {
                definitions.push(data("FunctionDefinition", { function: local_function_declaration(ctx_call(definition, "localFunctionDeclaration"), local_names) }));
            }
        }
    }
    definitions.push(data("FunctionDefinition", { function: function_declaration(ctx, local_names) }));
    return definitions;
}

function local_function_names(ctx) {
    var body = ctx_call(ctx, "functionBody");
    if (pyTruthy((body === null))) {
        return {  };
    }
    var names = {  };
    var outer_name = text(ctx_call(ctx, "functionNameDeclaration"));
    for (const definition of ctx_list(body, "localDefinition")) {
        if (pyTruthy(ctx_has(definition, "localFunctionDeclaration"))) {
            var local = ctx_call(definition, "localFunctionDeclaration");
            var local_name = text(ctx_call(local, "localFunctionNameDeclaration"));
            var location = source_location(local);
            names[local_name] = `${outer_name}__local__${local_name}__${location["line"]}_${location["column"]}`;
        }
    }
    return names;
}

function function_declaration(ctx, local_names) {
    var local_names = (local_names || {  });
    return data("FunctionDeclaration", { name: text(ctx_call(ctx, "functionNameDeclaration")), visibility: (ctx_has(ctx, "VISIBILITY") ? text(ctx_call(ctx, "VISIBILITY")) : "public"), parameters: (ctx_has(ctx, "parameters") ? Array.from(ctx_list(ctx_call(ctx, "parameters"), "parameter")).map((parameter) => function_parameter(parameter)) : []), returnType: (ctx_has(ctx, "functionType") ? parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type"))) : missing_type()), body: function_body(ctx_call(ctx, "functionBody"), local_names), annotations: annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function local_function_declaration(ctx, local_names) {
    var name = text(ctx_call(ctx, "localFunctionNameDeclaration"));
    return data("FunctionDeclaration", { name: (local_names[name] ?? name), visibility: "private", parameters: (ctx_has(ctx, "parameters") ? Array.from(ctx_list(ctx_call(ctx, "parameters"), "parameter")).map((parameter) => function_parameter(parameter)) : []), returnType: (ctx_has(ctx, "functionType") ? parse_type_reference(text(ctx_call(ctx_call(ctx, "functionType"), "type"))) : missing_type()), body: rewrite_local_function_calls(expression(ctx_call(ctx, "expression")), local_names), annotations: annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function data_declaration_definitions(ctx) {
    return data_declaration_definitions_from(ctx_call(ctx, "genericTypeDeclaration"), (ctx_has(ctx, "VISIBILITY") ? text(ctx_call(ctx, "VISIBILITY")) : "public"), ctx_call(ctx, "dataBody"), ctx_call(ctx, "constructorClause"), definition_annotation_applications(ctx_list(ctx, "annotationBlock")), derive_applications(ctx_call(ctx, "deriveClause")), source_location(ctx));
}

function local_data_declaration_definitions(ctx) {
    return data_declaration_definitions_from(ctx_call(ctx, "genericTypeDeclaration"), "private", ctx_call(ctx, "dataBody"), ctx_call(ctx, "constructorClause"), [], [], source_location(ctx));
}

function type_declaration_definitions(ctx) {
    return type_declaration_definitions_from(ctx_list(ctx, "genericTypeDeclaration"), (ctx_has(ctx, "VISIBILITY") ? text(ctx_call(ctx, "VISIBILITY")) : "public"), ctx_call(ctx, "fieldDeclarationList"), ctx_call(ctx, "constructorClause"), definition_annotation_applications(ctx_list(ctx, "annotationBlock")), derive_applications(ctx_call(ctx, "deriveClause")), source_location(ctx));
}

function local_type_declaration_definitions(ctx) {
    return type_declaration_definitions_from(ctx_list(ctx, "genericTypeDeclaration"), "private", ctx_call(ctx, "fieldDeclarationList"), ctx_call(ctx, "constructorClause"), [], [], source_location(ctx));
}

function type_declaration_definitions_from(declarations, visibility, parent_fields, constructor_clause, annotations, derives, location) {
    if (pyTruthy(!pyTruthy(declarations))) {
        return [];
    }
    var union_declaration = declarations[0];
    var name = data_type_name(union_declaration);
    var definitions = [data("TypeDeclaration", { name: name, visibility: visibility, parameters: data_type_parameters(union_declaration), fields: field_declaration_field_dtos(parent_fields), variants: type_declaration_variants(declarations), annotations: annotations, derives: derives, location: location }), schema_constant_definition(("__capy_schema_type|" + name), name, location), schema_constant_definition(("__capy_schema_kind|" + name), "union", location)];
    var type_parameters = data_type_parameters(union_declaration);
    var __index_6 = 0;
    for (const parameter of type_parameters) {
        const index = __index_6 + 0;
        __index_6++;
        definitions.push(schema_constant_definition(`__capy_schema_param|${name}|${index}`, parameter, location));
    }
    var fields = field_declaration_fields(parent_fields);
    var __index_8 = 0;
    for (const field of fields) {
        const index = __index_8 + 0;
        __index_8++;
        definitions.push(schema_constant_definition(`__capy_schema_field|${name}|${index}`, field, location));
    }
    var __index_10 = 0;
    for (const declaration of pySlice(declarations, 1, null)) {
        const declaration_index = __index_10 + 1;
        __index_10++;
        var variant_name = data_type_name(declaration);
        definitions.push(schema_constant_definition(`__capy_schema_parent|${variant_name}|${(declaration_index - 1)}`, name, location));
        var __index_12 = 0;
        for (const field of fields) {
            const field_index = __index_12 + 0;
            __index_12++;
            definitions.push(schema_constant_definition(`__capy_schema_field|${variant_name}|parent|${field_index}`, field, location));
        }
    }
    if (pyTruthy((constructor_clause !== null))) {
        definitions.push(constructor_function_definition(name, constructor_parameters(parent_fields), constructor_clause));
    }
    return definitions;
}

function data_declaration_definitions_from(declaration, visibility, data_body, constructor_clause, annotations, derives, location) {
    var name = data_type_name(declaration);
    var definitions = [data("DataDeclaration", { name: name, visibility: visibility, parameters: data_type_parameters(declaration), fields: data_declaration_own_fields(data_body), parents: data_declaration_parents(data_body), annotations: annotations, derives: derives, location: location })];
    if (pyTruthy((constructor_clause !== null))) {
        definitions.push(constructor_function_definition(name, constructor_parameters(data_body), constructor_clause));
    }
    return definitions;
}

function primitive_backed_type_declaration_definitions(ctx) {
    var name = text(ctx_call(ctx, "primitiveBackedTypeName"));
    var backing_type = type_ref(text(ctx_call(ctx, "primitiveBackingType")), []);
    var location = source_location(ctx);
    var definitions = [schema_constant_definition(("__capy_schema_type|" + name), name, location), schema_constant_definition(("__capy_schema_kind|" + name), "primitive", location), schema_constant_definition(("__capy_schema_primitive|" + name), backing_type["name"], location), schema_constant_definition((("__capy_schema_field|" + name) + "|0"), ("value|" + backing_type["name"]), location)];
    if (pyTruthy(ctx_has(ctx, "constructorClause"))) {
        definitions.push(constructor_function_definition(name, [data("FunctionParameter", { name: "value", typeReference: backing_type, location: source_location(ctx_call(ctx, "primitiveBackingType")) })], ctx_call(ctx, "constructorClause")));
    }
    return definitions;
}

function constructor_function_definition(name, parameters, constructor_clause) {
    return data("FunctionDefinition", { function: data("FunctionDeclaration", { name: ("__capy_constructor|" + name), visibility: "private", parameters: parameters, returnType: type_ref("any", []), body: rewrite_constructor_data(expression(ctx_call(constructor_clause, "expression")), name), annotations: [], location: source_location(constructor_clause) }) });
}

function constructor_parameters(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    if (pyTruthy(ctx.constructor.name.endsWith("DataBodyContext"))) {
        var fields = ctx_call(ctx, "fieldDeclarationList");
    } else {
        var fields = ctx;
    }
    if (pyTruthy((fields === null))) {
        return [];
    }
    return Array.from(ctx_list(fields, "fieldDeclaration")).filter((field) => !pyTruthy(data_parent_declaration(field))).map((field) => constructor_parameter(field));
}

function constructor_parameter(field) {
    var name = (ctx_has(field, "identifier") ? text(ctx_call(field, "identifier")) : unquote(text(ctx_call(field, "STRING_LITERAL"))));
    return data("FunctionParameter", { name: name, typeReference: parse_type_reference(text(ctx_call(field, "type"))), location: source_location(field) });
}

function schema_constant_definition(name, value, location) {
    return data("ConstantDefinition", { constant: data("ConstantDeclaration", { name: name, visibility: "schema", typeReference: string_type(), expression: data("StringLiteral", { value: value, source: quote(value), location: location }), location: location }) });
}

function data_type_name(ctx) {
    return text((ctx_item(ctx, "TYPE", 0) || ctx_call(ctx, "TYPE")));
}

function data_type_parameters(ctx) {
    return Array.from(pySlice(ctx_list(ctx, "TYPE"), 1, null)).map((token) => text(token));
}

function data_declaration_own_fields(ctx) {
    var field_list = ((ctx !== null) ? ctx_call(ctx, "fieldDeclarationList") : null);
    if (pyTruthy((field_list === null))) {
        return [];
    }
    return Array.from(ctx_list(field_list, "fieldDeclaration")).filter((field) => !pyTruthy(data_parent_declaration(field))).map((field) => data_field_declaration_dto(field));
}

function field_declaration_field_dtos(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "fieldDeclaration")).filter((field) => !pyTruthy(data_parent_declaration(field))).map((field) => data_field_declaration_dto(field));
}

function type_declaration_variants(declarations) {
    return Array.from(pySlice(declarations, 1, null)).map((declaration) => parse_type_reference(text(declaration)));
}

function derive_applications(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "TYPE")).map((token) => data("DeriveApplication", { name: text(token), location: token_location(token) }));
}

function data_field_declaration_dto(ctx) {
    var name = (ctx_has(ctx, "identifier") ? text(ctx_call(ctx, "identifier")) : unquote(text(ctx_call(ctx, "STRING_LITERAL"))));
    return data("DataFieldDeclaration", { name: name, typeReference: parse_type_reference(text(ctx_call(ctx, "type"))), annotations: definition_annotation_applications(ctx_list(ctx, "annotationBlock")), location: source_location(ctx) });
}

function data_declaration_parents(ctx) {
    var field_list = ((ctx !== null) ? ctx_call(ctx, "fieldDeclarationList") : null);
    if (pyTruthy((field_list === null))) {
        return [];
    }
    var parents = [];
    for (const field of ctx_list(field_list, "fieldDeclaration")) {
        if (pyTruthy(data_parent_declaration(field))) {
            parents.push(data("DataParentDeclaration", { typeReference: type_ref(text(ctx_call(field, "TYPE")), []), location: source_location(field) }));
        }
    }
    return parents;
}

function data_parent_declaration(ctx) {
    return (ctx_has(ctx, "SPREAD") && ctx_has(ctx, "TYPE"));
}

function field_declaration_fields(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "fieldDeclaration")).map((field) => data_field_declaration(field));
}

function data_field_declaration(ctx) {
    if (pyTruthy(ctx_has(ctx, "identifier"))) {
        return data_field_schema(text(ctx_call(ctx, "identifier")), ctx_call(ctx, "type"));
    }
    if (pyTruthy(ctx_has(ctx, "STRING_LITERAL"))) {
        return data_field_schema(unquote(text(ctx_call(ctx, "STRING_LITERAL"))), ctx_call(ctx, "type"));
    }
    return "$unsupported|";
}

function data_field_schema(name, type_ctx) {
    return ((name + "|") + text(type_ctx));
}

function function_parameter(ctx) {
    return data("FunctionParameter", { name: text(ctx_call(ctx, "identifier")), typeReference: parse_type_reference(text(ctx_call(ctx, "type"))), location: source_location(ctx) });
}

function function_body(ctx, local_names) {
    var local_names = (local_names || {  });
    if (pyTruthy(!pyTruthy(ctx_list(ctx, "localDefinition")))) {
        return rewrite_local_function_calls(expression(ctx_call(ctx, "expression")), local_names);
    }
    var bindings = [];
    for (const definition of ctx_list(ctx, "localDefinition")) {
        if (pyTruthy(ctx_has(definition, "localConstDeclaration"))) {
            bindings.push(local_const_binding(ctx_call(definition, "localConstDeclaration"), local_names));
        }
    }
    var result = rewrite_local_function_calls(expression(ctx_call(ctx, "expression")), local_names);
    if (pyTruthy(!pyTruthy(bindings))) {
        return result;
    }
    return data("BlockExpression", { bindings: bindings, result: result, location: source_location(ctx) });
}

function local_const_binding(ctx, local_names) {
    var local_names = (local_names || {  });
    return data("LetBinding", { name: text(ctx_call(ctx, "privateLocalConstName")), typeReference: (ctx_has(ctx, "type") ? parse_type_reference(text(ctx_call(ctx, "type"))) : missing_type()), operator: "=", value: rewrite_local_function_calls(expression_no_let(ctx_call(ctx, "expressionNoLet")), local_names), location: source_location(ctx) });
}

function rewrite_local_function_calls(expr, local_names) {
    return rewrite_expression(expr, local_names, rewrite_local_name, null);
}

function rewrite_constructor_data(expr, data_type_name_value) {
    return rewrite_expression(expr, {  }, null, data_type_name_value);
}

function rewrite_local_name(name, local_names) {
    return (local_names[name] ?? name);
}

function rewrite_expression(expr, local_names, name_rewriter, constructor_type) {
    var kind = type_name(expr);
    if (pyTruthy((kind === "IfExpression"))) {
        return data("IfExpression", { condition: rewrite_expression(expr["condition"], local_names, name_rewriter, constructor_type), thenBranch: rewrite_expression(expr["thenBranch"], local_names, name_rewriter, constructor_type), elseBranch: rewrite_expression(expr["elseBranch"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((kind === "BinaryExpression"))) {
        return data("BinaryExpression", { operator: expr["operator"], left: rewrite_expression(expr["left"], local_names, name_rewriter, constructor_type), right: rewrite_expression(expr["right"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((kind === "UnaryExpression"))) {
        return data("UnaryExpression", { operator: expr["operator"], expression: rewrite_expression(expr["expression"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((kind === "FunctionCallExpression"))) {
        var name = (name_rewriter ? name_rewriter(expr["name"], local_names) : expr["name"]);
        return data("FunctionCallExpression", { name: name, arguments: Array.from(expr["arguments"]).map((arg) => rewrite_expression(arg, local_names, name_rewriter, constructor_type)), location: expr["location"] });
    }
    if (pyTruthy((kind === "FunctionReferenceExpression"))) {
        var name = (name_rewriter ? name_rewriter(expr["name"], local_names) : expr["name"]);
        return data("FunctionReferenceExpression", { name: name, location: expr["location"] });
    }
    if (pyTruthy((kind === "MethodCallExpression"))) {
        return data("MethodCallExpression", { receiver: rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type), name: expr["name"], arguments: Array.from(expr["arguments"]).map((arg) => rewrite_expression(arg, local_names, name_rewriter, constructor_type)), location: expr["location"] });
    }
    if (pyTruthy((kind === "WithExpression"))) {
        return data("WithExpression", { receiver: rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type), fields: rewrite_data_fields(expr["fields"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((kind === "LambdaExpression"))) {
        return data("LambdaExpression", { parameters: expr["parameters"], body: rewrite_expression(expr["body"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((pyIn(kind, ["ListLiteral", "SetLiteral", "TupleLiteral"])))) {
        return data(kind, { values: Array.from(expr["values"]).map((value) => rewrite_expression(value, local_names, name_rewriter, constructor_type)), location: expr["location"] });
    }
    if (pyTruthy((kind === "DictLiteral"))) {
        return data("DictLiteral", { entries: Array.from(expr["entries"]).map((entry) => data("DictEntry", { key: rewrite_expression(entry["key"], local_names, name_rewriter, constructor_type), value: rewrite_expression(entry["value"], local_names, name_rewriter, constructor_type), location: entry["location"] })), location: expr["location"] });
    }
    if (pyTruthy((kind === "IndexExpression"))) {
        return data("IndexExpression", { receiver: rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type), index: rewrite_expression(expr["index"], local_names, name_rewriter, constructor_type), endIndex: rewrite_expression(expr["endIndex"], local_names, name_rewriter, constructor_type), hasEndIndex: expr["hasEndIndex"], location: expr["location"] });
    }
    if (pyTruthy((kind === "DataLiteral"))) {
        var type_name_value = expr["typeName"];
        if (pyTruthy(((constructor_type !== null) && (type_name_value === "*")))) {
            var type_name_value = ("__capy_raw|" + constructor_type);
        }
        return data("DataLiteral", { typeName: type_name_value, fields: rewrite_data_fields(expr["fields"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((kind === "FieldAccessExpression"))) {
        return data("FieldAccessExpression", { receiver: rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type), name: expr["name"], location: expr["location"] });
    }
    if (pyTruthy((kind === "ReduceExpression"))) {
        return data("ReduceExpression", { receiver: rewrite_expression(expr["receiver"], local_names, name_rewriter, constructor_type), initial: rewrite_expression(expr["initial"], local_names, name_rewriter, constructor_type), accumulatorName: expr["accumulatorName"], keyName: expr["keyName"], valueName: expr["valueName"], body: rewrite_expression(expr["body"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    if (pyTruthy((kind === "MatchExpression"))) {
        return data("MatchExpression", { value: rewrite_expression(expr["value"], local_names, name_rewriter, constructor_type), cases: Array.from(expr["cases"]).map((matchCaseValue) => data("MatchCase", { typeName: matchCaseValue["typeName"], bindings: matchCaseValue["bindings"], bindsWholeValue: matchCaseValue["bindsWholeValue"], literal: rewrite_expression(matchCaseValue["literal"], local_names, name_rewriter, constructor_type), hasLiteral: matchCaseValue["hasLiteral"], wildcard: matchCaseValue["wildcard"], guard: rewrite_expression(matchCaseValue["guard"], local_names, name_rewriter, constructor_type), hasGuard: matchCaseValue["hasGuard"], body: rewrite_expression(matchCaseValue["body"], local_names, name_rewriter, constructor_type), location: matchCaseValue["location"] })), location: expr["location"] });
    }
    if (pyTruthy((kind === "BlockExpression"))) {
        return data("BlockExpression", { bindings: Array.from(expr["bindings"]).map((binding) => data("LetBinding", { name: binding["name"], typeReference: binding["typeReference"], operator: binding["operator"], value: rewrite_expression(binding["value"], local_names, name_rewriter, constructor_type), location: binding["location"] })), result: rewrite_expression(expr["result"], local_names, name_rewriter, constructor_type), location: expr["location"] });
    }
    return expr;
}

function rewrite_data_fields(fields, local_names, name_rewriter, constructor_type) {
    return Array.from(fields).map((field) => data("DataField", { name: field["name"], value: rewrite_expression(field["value"], local_names, name_rewriter, constructor_type), spread: field["spread"], location: field["location"] }));
}

function expression(ctx) {
    var bindings = Array.from(ctx_list(ctx, "letExpression")).map((letValue) => let_binding(letValue));
    var result = expression_no_let(ctx_call(ctx, "expressionNoLet"));
    if (pyTruthy(!pyTruthy(bindings))) {
        return result;
    }
    return data("BlockExpression", { bindings: bindings, result: result, location: source_location(ctx) });
}

function let_binding(ctx) {
    return data("LetBinding", { name: text(ctx_call(ctx, "identifier")), typeReference: (ctx_has(ctx, "type") ? parse_type_reference(text(ctx_call(ctx, "type"))) : missing_type()), operator: text(ctx_call(ctx, "letBindingOperator")), value: expression_no_let(ctx_call(ctx, "expressionNoLet")), location: source_location(ctx) });
}

function expression_no_let(ctx) {
    if (pyTruthy(ctx_has(ctx, "ifExpression"))) {
        return if_expression(ctx_call(ctx, "ifExpression"));
    }
    if (pyTruthy(ctx_has(ctx, "lambdaExpression"))) {
        return lambda_expression(ctx_call(ctx, "lambdaExpression"));
    }
    if (pyTruthy(ctx_has(ctx, "functionCall"))) {
        return function_call(ctx_call(ctx, "functionCall"));
    }
    if (pyTruthy(ctx_has(ctx, "functionReference"))) {
        var reference = ctx_call(ctx, "functionReference");
        return data("FunctionReferenceExpression", { name: text(ctx_call(reference, "identifier")), location: source_location(reference) });
    }
    if (pyTruthy(ctx_has(ctx, "placeholder"))) {
        return unsupported(ctx_call(ctx, "placeholder"));
    }
    if (pyTruthy(ctx_has(ctx, "new_list"))) {
        return list_literal(ctx_call(ctx, "new_list"));
    }
    if (pyTruthy(ctx_has(ctx, "new_dict"))) {
        return dict_literal(ctx_call(ctx, "new_dict"));
    }
    if (pyTruthy(ctx_has(ctx, "tupleLiteral"))) {
        return tuple_literal(ctx_call(ctx, "tupleLiteral"));
    }
    if (pyTruthy(ctx_has(ctx, "expression"))) {
        return expression(ctx_call(ctx, "expression"));
    }
    if (pyTruthy(ctx_has(ctx, "new_set"))) {
        return set_literal(ctx_call(ctx, "new_set"));
    }
    if (pyTruthy(ctx_has(ctx, "newData"))) {
        return data_literal(ctx_call(ctx, "newData"));
    }
    if (pyTruthy(ctx_has(ctx, "constructorData"))) {
        return constructor_data_literal(ctx_call(ctx, "constructorData"));
    }
    if (pyTruthy(ctx_has(ctx, "matchExpression"))) {
        return match_expression(ctx_call(ctx, "matchExpression"));
    }
    if (pyTruthy(ctx_has(ctx, "value"))) {
        return value(ctx_call(ctx, "value"));
    }
    var children = ctx_list(ctx, "expressionNoLet");
    if (pyTruthy((ctx_has(ctx, "infixMethodLiteral") && (pyLen(children) === 2)))) {
        return binary_expression(text(ctx_call(ctx, "infixMethodLiteral")), expression_no_let(children[0]), expression_no_let(children[1]), source_location(ctx), is_grouped(children[0]));
    }
    if (pyTruthy((ctx_has(ctx, "infixOperator") && (pyLen(children) === 2)))) {
        var operator = text(ctx_call(ctx, "infixOperator"));
        var right_reduce = ((children[1] !== null) ? ctx_call(children[1], "reduceExpression") : null);
        if (pyTruthy(((operator === "|>") && (right_reduce !== null)))) {
            return reduce_expression(expression_no_let(children[0]), right_reduce, source_location(ctx));
        }
        return binary_expression(operator, expression_no_let(children[0]), expression_no_let(children[1]), source_location(ctx), is_grouped(children[0]));
    }
    if (pyTruthy((ctx_has(ctx, "identifier") && (pyLen(children) === 1) && has_child(ctx, ".")))) {
        return data("FieldAccessExpression", { receiver: expression_no_let(children[0]), name: text(ctx_call(ctx, "identifier")), location: source_location(ctx) });
    }
    if (pyTruthy((ctx_has(ctx, "methodIdentifier") && (pyLen(children) === 1)))) {
        return method_call_expression(expression_no_let(children[0]), text(ctx_call(ctx, "methodIdentifier")), ctx_call(ctx, "methodArgumentList"), source_location(ctx));
    }
    if (pyTruthy(is_invocation_expression(ctx))) {
        return invocation_expression(expression_no_let(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx));
    }
    if (pyTruthy(is_slice_expression(ctx))) {
        return slice_expression(expression_no_let(children[0]), source_location(ctx), ctx);
    }
    if (pyTruthy(is_index_expression(ctx))) {
        return index_expression(expression_no_let(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx), ctx);
    }
    if (pyTruthy(is_unary(ctx))) {
        var operator = first_child_text(ctx);
        return data("UnaryExpression", { operator: operator, expression: unary_operand(operator, children[0]), location: source_location(ctx) });
    }
    return unsupported(ctx);
}

function expression_no_pipe(ctx) {
    if (pyTruthy(ctx_list(ctx, "letExpressionNoPipe"))) {
        return unsupported(ctx);
    }
    return expression_no_let_no_pipe(ctx_call(ctx, "expressionNoLetNoPipe"));
}

function expression_no_let_no_pipe(ctx) {
    if (pyTruthy(ctx_has(ctx, "ifExpression"))) {
        return if_expression(ctx_call(ctx, "ifExpression"));
    }
    if (pyTruthy(ctx_has(ctx, "lambdaExpression"))) {
        return lambda_expression(ctx_call(ctx, "lambdaExpression"));
    }
    if (pyTruthy(ctx_has(ctx, "functionCall"))) {
        return function_call(ctx_call(ctx, "functionCall"));
    }
    if (pyTruthy(ctx_has(ctx, "functionReference"))) {
        var reference = ctx_call(ctx, "functionReference");
        return data("FunctionReferenceExpression", { name: text(ctx_call(reference, "identifier")), location: source_location(reference) });
    }
    if (pyTruthy(ctx_has(ctx, "placeholder"))) {
        return unsupported(ctx_call(ctx, "placeholder"));
    }
    if (pyTruthy(ctx_has(ctx, "new_list"))) {
        return list_literal(ctx_call(ctx, "new_list"));
    }
    if (pyTruthy(ctx_has(ctx, "new_dict"))) {
        return dict_literal(ctx_call(ctx, "new_dict"));
    }
    if (pyTruthy(ctx_has(ctx, "tupleLiteral"))) {
        return tuple_literal(ctx_call(ctx, "tupleLiteral"));
    }
    if (pyTruthy(ctx_has(ctx, "expression"))) {
        return expression(ctx_call(ctx, "expression"));
    }
    if (pyTruthy(ctx_has(ctx, "new_set"))) {
        return set_literal(ctx_call(ctx, "new_set"));
    }
    if (pyTruthy(ctx_has(ctx, "newData"))) {
        return data_literal(ctx_call(ctx, "newData"));
    }
    if (pyTruthy(ctx_has(ctx, "constructorData"))) {
        return constructor_data_literal(ctx_call(ctx, "constructorData"));
    }
    if (pyTruthy(ctx_has(ctx, "matchExpressionNoPipe"))) {
        return match_expression_no_pipe(ctx_call(ctx, "matchExpressionNoPipe"));
    }
    if (pyTruthy(ctx_has(ctx, "value"))) {
        return value(ctx_call(ctx, "value"));
    }
    var children = ctx_list(ctx, "expressionNoLetNoPipe");
    if (pyTruthy((ctx_has(ctx, "infixMethodLiteral") && (pyLen(children) === 2)))) {
        return binary_expression(text(ctx_call(ctx, "infixMethodLiteral")), expression_no_let_no_pipe(children[0]), expression_no_let_no_pipe(children[1]), source_location(ctx), is_grouped(children[0]));
    }
    if (pyTruthy((ctx_has(ctx, "infixOperatorNoPipe") && (pyLen(children) === 2)))) {
        return binary_expression(text(ctx_call(ctx, "infixOperatorNoPipe")), expression_no_let_no_pipe(children[0]), expression_no_let_no_pipe(children[1]), source_location(ctx), is_grouped(children[0]));
    }
    if (pyTruthy((ctx_has(ctx, "identifier") && (pyLen(children) === 1) && has_child(ctx, ".")))) {
        return data("FieldAccessExpression", { receiver: expression_no_let_no_pipe(children[0]), name: text(ctx_call(ctx, "identifier")), location: source_location(ctx) });
    }
    if (pyTruthy((ctx_has(ctx, "methodIdentifier") && (pyLen(children) === 1)))) {
        return method_call_expression(expression_no_let_no_pipe(children[0]), text(ctx_call(ctx, "methodIdentifier")), ctx_call(ctx, "methodArgumentList"), source_location(ctx));
    }
    if (pyTruthy(is_invocation_expression_no_pipe(ctx))) {
        return invocation_expression(expression_no_let_no_pipe(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx));
    }
    if (pyTruthy(is_slice_expression_no_pipe(ctx))) {
        return slice_expression(expression_no_let_no_pipe(children[0]), source_location(ctx), ctx);
    }
    if (pyTruthy((ctx_has(ctx, "argumentList") && (pyLen(children) === 1) && has_child(ctx, "[")))) {
        return index_expression(expression_no_let_no_pipe(children[0]), ctx_call(ctx, "argumentList"), source_location(ctx), ctx);
    }
    if (pyTruthy(is_unary_no_pipe(ctx))) {
        var operator = first_child_text(ctx);
        return data("UnaryExpression", { operator: operator, expression: unary_no_pipe_operand(operator, children[0]), location: source_location(ctx) });
    }
    return unsupported(ctx);
}

function unary_operand(operator, operand) {
    if (pyTruthy((operator === "-"))) {
        var min_literal = negatable_min_literal(ctx_call(operand, "value"), source_location(operand));
        if (pyTruthy((min_literal !== null))) {
            return min_literal;
        }
    }
    return expression_no_let(operand);
}

function unary_no_pipe_operand(operator, operand) {
    if (pyTruthy((operator === "-"))) {
        var min_literal = negatable_min_literal(ctx_call(operand, "value"), source_location(operand));
        if (pyTruthy((min_literal !== null))) {
            return min_literal;
        }
    }
    return expression_no_let_no_pipe(operand);
}

function negatable_min_literal(value_ctx, location) {
    var literal = ((value_ctx !== null) ? ctx_call(value_ctx, "literal") : null);
    if (pyTruthy((literal === null))) {
        return null;
    }
    var source = text(literal);
    if (pyTruthy((ctx_has(literal, "INT_LITERAL") && (clean_number(source) === "2147483648")))) {
        return data("IntLiteral", { value: -2147483648, source: source, location: location });
    }
    if (pyTruthy((ctx_has(literal, "LONG_LITERAL") && (clean_number(pySlice(source, null, -1)) === "9223372036854775808")))) {
        return data("LongLiteral", { value: -9223372036854775808, source: source, location: location });
    }
    return null;
}

function if_expression(ctx) {
    return data("IfExpression", { condition: expression(ctx_item(ctx, "expression", 0)), thenBranch: expression(ctx_item(ctx, "expression", 1)), elseBranch: expression(ctx_item(ctx, "expression", 2)), location: source_location(ctx) });
}

function lambda_expression(ctx) {
    return data("LambdaExpression", { parameters: Array.from(ctx_list(ctx, "lambdaArgument")).map((parameter) => text(parameter)), body: expression_no_pipe(ctx_call(ctx, "expressionNoPipe")), location: source_location(ctx) });
}

function reduce_expression(receiver, ctx, location) {
    var argumentValues = ctx_list(ctx, "lambdaArgument");
    if (pyTruthy((pyLen(argumentValues) < 2))) {
        return unsupported_expr(text(ctx), location);
    }
    var key_name = ((pyLen(argumentValues) > 2) ? text(argumentValues[1]) : "");
    var value_name = ((pyLen(argumentValues) > 2) ? text(argumentValues[2]) : text(argumentValues[1]));
    return data("ReduceExpression", { receiver: receiver, initial: expression_no_let_no_pipe(ctx_call(ctx, "expressionNoLetNoPipe")), accumulatorName: text(argumentValues[0]), keyName: key_name, valueName: value_name, body: expression_no_pipe(ctx_call(ctx, "expressionNoPipe")), location: location });
}

function function_call(ctx) {
    var name = ((ctx_has(ctx, "TYPE") && ctx_has(ctx, "identifier")) ? ((text(ctx_call(ctx, "TYPE")) + ".") + text(ctx_call(ctx, "identifier"))) : ctx.getChild(0).getText());
    return data("FunctionCallExpression", { name: name, arguments: argumentValues(ctx_call(ctx, "argumentList")), location: source_location(ctx) });
}

function method_call_expression(receiver, name, argument_list, location) {
    if (pyTruthy((name === "with"))) {
        return data("WithExpression", { receiver: receiver, fields: with_fields(argument_list), location: location });
    }
    var reduce = method_reduce_expression(receiver, name, argument_list, location);
    if (pyTruthy((reduce !== null))) {
        return reduce;
    }
    return data("MethodCallExpression", { receiver: receiver, name: name, arguments: method_arguments(argument_list), location: location });
}

function method_reduce_expression(receiver, name, argument_list, location) {
    if (pyTruthy(((!pyIn(name, ["reduce", "reduce_left"])) || (argument_list === null) || (pyLen(ctx_list(argument_list, "methodArgument")) !== 1)))) {
        return null;
    }
    var argument = ctx_list(argument_list, "methodArgument")[0];
    var argument_expression = ctx_call(argument, "expression");
    if (pyTruthy((argument_expression === null))) {
        return null;
    }
    var reduce_ctx = ctx_call(ctx_call(argument_expression, "expressionNoLet"), "reduceExpression");
    if (pyTruthy((reduce_ctx === null))) {
        return null;
    }
    return reduce_expression(receiver, reduce_ctx, location);
}

function invocation_expression(receiver, argument_list, location) {
    if (pyTruthy((type_name(receiver) === "VariableExpression"))) {
        return data("FunctionCallExpression", { name: receiver["name"], arguments: argumentValues(argument_list), location: location });
    }
    return data("MethodCallExpression", { receiver: receiver, name: "__capy_call", arguments: argumentValues(argument_list), location: location });
}

function argumentValues(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "expression")).map((argument) => expression(argument));
}

function method_arguments(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    var result = [];
    for (const argument of ctx_list(ctx, "methodArgument")) {
        if (pyTruthy(ctx_has(argument, "expression"))) {
            result.push(expression(ctx_call(argument, "expression")));
        } else {
            result.push(unsupported(argument));
        }
    }
    return result;
}

function with_fields(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    var fields = [];
    for (const argument of ctx_list(ctx, "methodArgument")) {
        if (pyTruthy(ctx_has(argument, "namedMethodArgument"))) {
            var named = ctx_call(argument, "namedMethodArgument");
            fields.push(data("DataField", { name: text(ctx_call(named, "identifier")), value: expression(ctx_call(named, "expression")), spread: false, location: source_location(named) }));
        } else {
            fields.push(data("DataField", { name: "$unsupported", value: unsupported(argument), spread: false, location: source_location(argument) }));
        }
    }
    return fields;
}

function list_literal(ctx) {
    return data("ListLiteral", { values: Array.from(ctx_list(ctx, "expression")).map((value_ctx) => expression(value_ctx)), location: source_location(ctx) });
}

function set_literal(ctx) {
    return data("SetLiteral", { values: Array.from(ctx_list(ctx, "expression")).map((value_ctx) => expression(value_ctx)), location: source_location(ctx) });
}

function dict_literal(ctx) {
    return data("DictLiteral", { entries: Array.from(ctx_list(ctx, "dict_entry")).map((entry) => data("DictEntry", { key: expression(ctx_item(entry, "expression", 0)), value: expression(ctx_item(entry, "expression", 1)), location: source_location(entry) })), location: source_location(ctx) });
}

function tuple_literal(ctx) {
    return data("TupleLiteral", { values: Array.from(ctx_list(ctx, "expression")).map((value_ctx) => expression(value_ctx)), location: source_location(ctx) });
}

function index_expression(receiver, argument_list, location, fallback) {
    var expressions = ((argument_list !== null) ? ctx_list(argument_list, "expression") : []);
    if (pyTruthy(!pyTruthy(expressions))) {
        return data("IndexExpression", { receiver: receiver, index: unsupported(fallback), endIndex: unsupported(fallback), hasEndIndex: false, location: location });
    }
    var index = expression(expressions[0]);
    if (pyTruthy((pyLen(expressions) > 1))) {
        return data("IndexExpression", { receiver: receiver, index: index, endIndex: expression(expressions[1]), hasEndIndex: true, location: location });
    }
    return data("IndexExpression", { receiver: receiver, index: index, endIndex: unsupported(fallback), hasEndIndex: false, location: location });
}

function slice_expression(receiver, location, ctx) {
    var start = data("IntLiteral", { value: 0, source: "__capy_slice_start__", location: location });
    var end = data("IntLiteral", { value: 0, source: "__capy_slice_end__", location: location });
    var before_colon = true;
    for (var index = 0; index < ctx.getChildCount(); index++) {
        var child = ctx.getChild(index);
        var child_text = child.getText();
        if (pyTruthy((child_text === ":"))) {
            var before_colon = false;
        } else if (pyTruthy((pyIn(child.constructor.name, ["SliceIndexLiteralContext", "SliceIndexNoPipeLiteralContext"])))) {
            var literal = data("IntLiteral", { value: Number.parseInt(clean_number(child_text), 10), source: child_text, location: source_location(child) });
            if (pyTruthy(before_colon)) {
                var start = literal;
            } else {
                var end = literal;
            }
        }
    }
    return data("IndexExpression", { receiver: receiver, index: start, endIndex: end, hasEndIndex: true, location: location });
}

function data_literal(ctx) {
    var type_name_value = text(ctx_call(ctx, "type"));
    if (pyTruthy(ctx_has(ctx, "BANG"))) {
        var type_name_value = ("__capy_raw|" + type_name_value);
    }
    return data("DataLiteral", { typeName: type_name_value, fields: data_fields(ctx_call(ctx, "fieldAssignmentList")), location: source_location(ctx) });
}

function constructor_data_literal(ctx) {
    return data("DataLiteral", { typeName: "*", fields: data_fields(ctx_call(ctx, "fieldAssignmentList")), location: source_location(ctx) });
}

function data_fields(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    var fields = [];
    var positional_index = 0;
    for (const assignment of ctx_list(ctx, "fieldAssignment")) {
        if (pyTruthy(ctx_has(assignment, "namedFieldAssignment"))) {
            fields.push(named_data_field(ctx_call(assignment, "namedFieldAssignment")));
            positional_index += 1;
        } else if (pyTruthy(ctx_has(assignment, "spreadFieldAssignment"))) {
            fields.push(data("DataField", { name: "", value: expression(ctx_call(ctx_call(assignment, "spreadFieldAssignment"), "expression")), spread: true, location: source_location(assignment) }));
        } else if (pyTruthy(ctx_has(assignment, "positionalFieldAssignment"))) {
            fields.push(data("DataField", { name: ("$" + String(positional_index)), value: expression(ctx_call(ctx_call(assignment, "positionalFieldAssignment"), "expression")), spread: false, location: source_location(assignment) }));
            positional_index += 1;
        } else {
            fields.push(data("DataField", { name: "$unsupported", value: unsupported(assignment), spread: false, location: source_location(assignment) }));
            positional_index += 1;
        }
    }
    return fields;
}

function named_data_field(ctx) {
    var name = (ctx_has(ctx, "identifier") ? text(ctx_call(ctx, "identifier")) : unquote(text(ctx_call(ctx, "STRING_LITERAL"))));
    return data("DataField", { name: name, value: expression(ctx_call(ctx, "expression")), spread: false, location: source_location(ctx) });
}

function match_expression(ctx) {
    var cases = [];
    for (const case_list of ctx_list(ctx, "matchCaseList")) {
        for (const match_case_ctx of ctx_list(case_list, "matchCase")) {
            cases.push(...match_cases(match_case_ctx));
        }
    }
    return data("MatchExpression", { value: expression(ctx_call(ctx, "expression")), cases: cases, location: source_location(ctx) });
}

function match_expression_no_pipe(ctx) {
    var cases = [];
    for (const case_list of ctx_list(ctx, "matchCaseNoPipeList")) {
        for (const match_case_ctx of ctx_list(case_list, "matchCaseNoPipe")) {
            cases.push(...match_cases_no_pipe(match_case_ctx));
        }
    }
    return data("MatchExpression", { value: expression_no_pipe(ctx_call(ctx, "expressionNoPipe")), cases: cases, location: source_location(ctx) });
}

function match_cases(ctx) {
    var patterns = ctx_list(ctx, "pattern");
    if (pyTruthy(!pyTruthy(patterns))) {
        return [match_case(ctx, null)];
    }
    return Array.from(patterns).map((pattern) => match_case(ctx, pattern));
}

function match_case(ctx, pattern) {
    var guard = ((ctx) == null ? null : ((ctx)["guard"] ?? null));
    var body = ((ctx) == null ? null : ((ctx)["body"] ?? null));
    return data("MatchCase", { typeName: pattern_type_name(pattern), bindings: pattern_bindings(pattern), bindsWholeValue: pattern_binds_whole_value(pattern), literal: pattern_literal(pattern), hasLiteral: pattern_has_literal(pattern), wildcard: pattern_wildcard(pattern), guard: ((guard === null) ? unsupported(ctx) : expression(guard)), hasGuard: (guard !== null), body: expression(body), location: source_location(ctx) });
}

function match_cases_no_pipe(ctx) {
    var patterns = ctx_list(ctx, "pattern");
    if (pyTruthy(!pyTruthy(patterns))) {
        return [match_case_no_pipe(ctx, null)];
    }
    return Array.from(patterns).map((pattern) => match_case_no_pipe(ctx, pattern));
}

function match_case_no_pipe(ctx, pattern) {
    var guard = ((ctx) == null ? null : ((ctx)["guard"] ?? null));
    var body = ((ctx) == null ? null : ((ctx)["body"] ?? null));
    return data("MatchCase", { typeName: pattern_type_name(pattern), bindings: pattern_bindings(pattern), bindsWholeValue: pattern_binds_whole_value(pattern), literal: pattern_literal(pattern), hasLiteral: pattern_has_literal(pattern), wildcard: pattern_wildcard(pattern), guard: ((guard === null) ? unsupported(ctx) : expression_no_pipe(guard)), hasGuard: (guard !== null), body: expression_no_pipe(body), location: source_location(ctx) });
}

function pattern_type_name(ctx) {
    if (pyTruthy((ctx === null))) {
        return "";
    }
    if (pyTruthy(ctx_has(ctx, "constructorPattern"))) {
        return text(ctx_call(ctx_call(ctx, "constructorPattern"), "TYPE"));
    }
    if (pyTruthy(ctx_has(ctx, "typedPattern"))) {
        return text(ctx_call(ctx_call(ctx, "typedPattern"), "patternType"));
    }
    if (pyTruthy(ctx_has(ctx, "TYPE"))) {
        return text(ctx_call(ctx, "TYPE"));
    }
    return "";
}

function pattern_bindings(ctx, include_lower_qualified_field) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    if (pyTruthy((ctx_has(ctx, "typedPattern") && ctx_has(ctx_call(ctx, "typedPattern"), "NAME")))) {
        return [text(ctx_call(ctx_call(ctx, "typedPattern"), "NAME"))];
    }
    if (pyTruthy((ctx_has(ctx, "wildcardPattern") && ctx_has(ctx_call(ctx, "wildcardPattern"), "NAME")))) {
        return [text(ctx_call(ctx_call(ctx, "wildcardPattern"), "NAME"))];
    }
    var constructor = ctx_call(ctx, "constructorPattern");
    var field_list = ((constructor !== null) ? ctx_call(constructor, "fieldPatternList") : null);
    if (pyTruthy((field_list === null))) {
        return [];
    }
    var bindings = [];
    var __index_14 = 0;
    for (const field_pattern of ctx_list(field_list, "pattern")) {
        const index = __index_14 + 0;
        __index_14++;
        if (pyTruthy(ctx_has(field_pattern, "identifier"))) {
            bindings.push(text(ctx_call(field_pattern, "identifier")));
        } else if (pyTruthy((include_lower_qualified_field && ctx_has(field_pattern, "patternType") && ctx_has(ctx_call(field_pattern, "patternType"), "lowerQualifiedType")))) {
            bindings.push(text(ctx_call(ctx_call(field_pattern, "patternType"), "lowerQualifiedType")));
        } else if (pyTruthy((ctx_has(field_pattern, "wildcardPattern") && ctx_has(ctx_call(field_pattern, "wildcardPattern"), "NAME")))) {
            bindings.push(text(ctx_call(ctx_call(field_pattern, "wildcardPattern"), "NAME")));
        } else if (pyTruthy(ctx_has(field_pattern, "wildcardPattern"))) {
            bindings.push(("__capy_ignore_" + String(index)));
        }
    }
    return bindings;
}

function pattern_wildcard(ctx) {
    return ((ctx !== null) && ctx_has(ctx, "wildcardPattern"));
}

function pattern_binds_whole_value(ctx) {
    return ((ctx !== null) && (ctx_has(ctx, "typedPattern") || ctx_has(ctx, "wildcardPattern")));
}

function pattern_has_literal(ctx) {
    return ((ctx !== null) && Array.from(["INT_LITERAL", "LONG_LITERAL", "BOOL_LITERAL", "STRING_LITERAL", "FLOAT_LITERAL"]).some((name) => ctx_has(ctx, name)));
}

function pattern_literal(ctx) {
    if (pyTruthy((ctx === null))) {
        return unsupported_literal();
    }
    var location = source_location(ctx);
    if (pyTruthy(ctx_has(ctx, "INT_LITERAL"))) {
        var source = text(ctx_call(ctx, "INT_LITERAL"));
        return data("IntLiteral", { value: Number.parseInt(clean_number(source), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "LONG_LITERAL"))) {
        var source = text(ctx_call(ctx, "LONG_LITERAL"));
        return data("LongLiteral", { value: Number.parseInt(clean_number(pySlice(source, null, -1)), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "FLOAT_LITERAL"))) {
        var source = text(ctx_call(ctx, "FLOAT_LITERAL"));
        return data("FloatLiteral", { value: Number.parseFloat(clean_float(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "BOOL_LITERAL"))) {
        var source = text(ctx_call(ctx, "BOOL_LITERAL"));
        return data("BoolLiteral", { value: bool_value(source), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "STRING_LITERAL"))) {
        return string_literal_expression(text(ctx_call(ctx, "STRING_LITERAL")), location);
    }
    return unsupported_literal();
}

function value(ctx) {
    if (pyTruthy(ctx_has(ctx, "identifier"))) {
        return data("VariableExpression", { name: text(ctx_call(ctx, "identifier")), location: source_location(ctx) });
    }
    if (pyTruthy(ctx_has(ctx, "qualifiedType"))) {
        return data("VariableExpression", { name: text(ctx_call(ctx, "qualifiedType")), location: source_location(ctx) });
    }
    var literal = ctx_call(ctx, "literal");
    var source = text(literal);
    try {
        if (pyTruthy(ctx_has(literal, "BYTE_LITERAL"))) {
            return data("IntLiteral", { value: Number.parseInt(clean_number(source), 0), source: source, location: source_location(ctx) });
        }
        if (pyTruthy(ctx_has(literal, "INT_LITERAL"))) {
            return data("IntLiteral", { value: Number.parseInt(clean_number(source), 10), source: source, location: source_location(ctx) });
        }
        if (pyTruthy(ctx_has(literal, "LONG_LITERAL"))) {
            return data("LongLiteral", { value: Number.parseInt(clean_number(pySlice(source, null, -1)), 10), source: source, location: source_location(ctx) });
        }
    } catch (_) {
        return unsupported(ctx);
    }
    if (pyTruthy(ctx_has(literal, "FLOAT_LITERAL"))) {
        return data("FloatLiteral", { value: Number.parseFloat(clean_float(source)), source: source, location: source_location(ctx) });
    }
    if (pyTruthy(ctx_has(literal, "DOUBLE_LITERAL"))) {
        return data("DoubleLiteral", { value: Number.parseFloat(clean_double(source)), source: source, location: source_location(ctx) });
    }
    if (pyTruthy(ctx_has(literal, "BOOL_LITERAL"))) {
        return data("BoolLiteral", { value: bool_value(source), source: source, location: source_location(ctx) });
    }
    if (pyTruthy(ctx_has(literal, "STRING_LITERAL"))) {
        return string_literal_expression(source, source_location(ctx));
    }
    if (pyTruthy(ctx_has(literal, "REGEX_LITERAL"))) {
        return regex_literal_expression(source, source_location(ctx));
    }
    return unsupported(ctx);
}

function regex_literal_expression(source, location) {
    var closing_slash = regex_literal_closing_slash(source);
    if (pyTruthy((closing_slash < pyLen("regex/")))) {
        return unsupported_expr(source, location);
    }
    var pattern = unescape_regex_content(pySlice(source, pyLen("regex/"), closing_slash));
    var flags = pySlice(source, (closing_slash + 1), null);
    return data("DataLiteral", { typeName: "Regex", fields: [data("DataField", { name: "pattern", value: data("StringLiteral", { value: pattern, source: quote(pattern), location: location }), spread: false, location: location }), data("DataField", { name: "flags", value: data("StringLiteral", { value: flags, source: quote(flags), location: location }), spread: false, location: location })], location: location });
}

function regex_literal_closing_slash(source) {
    var index = (pyLen(source) - 1);
    while (pyTruthy(((index >= 0) && (pyIn(source[index], "ims"))))) {
        index -= 1;
    }
    return index;
}

function unescape_regex_content(value) {
    var result = [];
    var index = 0;
    while (pyTruthy((index < pyLen(value)))) {
        var current = value[index];
        if (pyTruthy(((current === "\\") && ((index + 1) < pyLen(value)) && (pyIn(value[(index + 1)], ["/", "\\"]))))) {
            result.push(value[(index + 1)]);
            index += 2;
        } else {
            result.push(current);
            index += 1;
        }
    }
    return result.join("");
}

function string_literal_expression(source, location) {
    if (pyTruthy(!pyTruthy(source.startsWith("\"")))) {
        return data("StringLiteral", { value: unquote(source), source: source, location: location });
    }
    return interpolated_string_literal_expression(source, location);
}

function interpolated_string_literal_expression(source, location) {
    var content = pySlice(source, 1, -1);
    var parts = [];
    var segment = [];
    var segment_start = 0;
    var found_interpolation = false;
    var index = 0;
    while (pyTruthy((index < pyLen(content)))) {
        var current = content[index];
        if (pyTruthy(((current === "\\") && ((index + 1) < pyLen(content))))) {
            if (pyTruthy(((content[(index + 1)] === "{") && ((index + 2) < pyLen(content)) && (content[(index + 2)] === "{")))) {
                segment.push("{");
                index += 2;
            } else {
                segment.push(...[current, content[(index + 1)]]);
                index += 2;
            }
            continue;
        }
        if (pyTruthy((current === "{"))) {
            var end = interpolation_end(content, (index + 1));
            var expression_source = ((end >= 0) ? pySlice(content, (index + 1), end) : "");
            if (pyTruthy(((end >= 0) && (expression_source.trim() === expression_source) && expression_source))) {
                if (pyTruthy((!pyTruthy(parts) && !pyTruthy(segment)))) {
                    parts.push(string_segment("", location, 0));
                } else {
                    add_string_segment(parts, segment, location, segment_start);
                }
                var segment = [];
                parts.push(interpolation_expression(expression_source, string_content_location(location, (index + 1))));
                var found_interpolation = true;
                var index = (end + 1);
                var segment_start = index;
                continue;
            }
        }
        segment.push(current);
        index += 1;
    }
    if (pyTruthy(!pyTruthy(found_interpolation))) {
        return data("StringLiteral", { value: unquote(source), source: source, location: location });
    }
    add_string_segment(parts, segment, location, segment_start);
    return concatenated(parts, location);
}

function interpolation_end(content, start) {
    var depth = 0;
    var quote_char = null;
    var index = start;
    while (pyTruthy((index < pyLen(content)))) {
        var current = content[index];
        if (pyTruthy((current === "\\"))) {
            index += 2;
            continue;
        }
        if (pyTruthy((quote_char !== null))) {
            if (pyTruthy((current === quote_char))) {
                var quote_char = null;
            }
        } else if (pyTruthy((pyIn(current, ["\"", "'"])))) {
            var quote_char = current;
        } else if (pyTruthy((current === "{"))) {
            depth += 1;
        } else if (pyTruthy((current === "}"))) {
            if (pyTruthy((depth === 0))) {
                return index;
            }
            depth -= 1;
        }
        index += 1;
    }
    return -1;
}

function add_string_segment(parts, segment, literal_location, segment_start) {
    if (pyTruthy(segment)) {
        parts.push(string_segment(segment.join(""), literal_location, segment_start));
    }
}

function string_segment(raw_segment, literal_location, segment_start) {
    var value = unescape_string_content(raw_segment);
    return data("StringLiteral", { value: value, source: quote(value), location: string_content_location(literal_location, segment_start) });
}

function string_content_location(literal_location, content_offset) {
    return loc(literal_location["line"], ((literal_location["column"] + 1) + content_offset));
}

function concatenated(parts, location) {
    if (pyTruthy(!pyTruthy(parts))) {
        return data("StringLiteral", { value: "", source: "\"\"", location: location });
    }
    var result = parts[0];
    for (const part of pySlice(parts, 1, null)) {
        var result = data("BinaryExpression", { operator: "+", left: result, right: part, location: location });
    }
    return result;
}

function interpolation_expression(source, location) {
    var lexer = new FunctionalLexer(new antlr4.InputStream(source));
    var tokens = new antlr4.CommonTokenStream(lexer);
    var parser = new FunctionalParser(tokens);
    var errors = new SyntaxErrorCollector();
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    lexer.addErrorListener(errors);
    parser.addErrorListener(errors);
    var parsed_expression = parser.expression();
    if (pyTruthy((tokens.LA(1) !== antlr4.Token.EOF))) {
        var token_index = (typeof tokens.index === "number" ? tokens.index : tokens.index());
        errors.errors.push([1, (tokens[token_index] ?? undefined).column, "extraneous input"]);
    }
    if (pyTruthy(errors.errors)) {
        return unsupported_expr(source, location);
    }
    return offset_expression(expression(parsed_expression), (location["line"] - 1), location["column"]);
}

function offset_expression(expr, line_offset, column_offset) {
    var kind = type_name(expr);
    var result = Object.assign({}, expr);
    if (pyTruthy((pyIn("location", result)))) {
        result["location"] = offset_location(result["location"], line_offset, column_offset);
    }
    if (pyTruthy((kind === "IfExpression"))) {
        result["condition"] = offset_expression(expr["condition"], line_offset, column_offset);
        result["thenBranch"] = offset_expression(expr["thenBranch"], line_offset, column_offset);
        result["elseBranch"] = offset_expression(expr["elseBranch"], line_offset, column_offset);
    } else if (pyTruthy((pyIn(kind, ["BinaryExpression"])))) {
        result["left"] = offset_expression(expr["left"], line_offset, column_offset);
        result["right"] = offset_expression(expr["right"], line_offset, column_offset);
    } else if (pyTruthy((kind === "UnaryExpression"))) {
        result["expression"] = offset_expression(expr["expression"], line_offset, column_offset);
    } else if (pyTruthy((pyIn(kind, ["FunctionCallExpression", "MethodCallExpression"])))) {
        if (pyTruthy((pyIn("receiver", result)))) {
            result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset);
        }
        result["arguments"] = Array.from(expr["arguments"]).map((arg) => offset_expression(arg, line_offset, column_offset));
    } else if (pyTruthy((kind === "WithExpression"))) {
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset);
        result["fields"] = offset_data_fields(expr["fields"], line_offset, column_offset);
    } else if (pyTruthy((kind === "LambdaExpression"))) {
        result["body"] = offset_expression(expr["body"], line_offset, column_offset);
    } else if (pyTruthy((pyIn(kind, ["ListLiteral", "SetLiteral", "TupleLiteral"])))) {
        result["values"] = Array.from(expr["values"]).map((value_ctx) => offset_expression(value_ctx, line_offset, column_offset));
    } else if (pyTruthy((kind === "DictLiteral"))) {
        result["entries"] = Array.from(expr["entries"]).map((entry) => data("DictEntry", { key: offset_expression(entry["key"], line_offset, column_offset), value: offset_expression(entry["value"], line_offset, column_offset), location: offset_location(entry["location"], line_offset, column_offset) }));
    } else if (pyTruthy((kind === "IndexExpression"))) {
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset);
        result["index"] = offset_expression(expr["index"], line_offset, column_offset);
        result["endIndex"] = offset_expression(expr["endIndex"], line_offset, column_offset);
    } else if (pyTruthy((kind === "DataLiteral"))) {
        result["fields"] = offset_data_fields(expr["fields"], line_offset, column_offset);
    } else if (pyTruthy((kind === "FieldAccessExpression"))) {
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset);
    } else if (pyTruthy((kind === "ReduceExpression"))) {
        result["receiver"] = offset_expression(expr["receiver"], line_offset, column_offset);
        result["initial"] = offset_expression(expr["initial"], line_offset, column_offset);
        result["body"] = offset_expression(expr["body"], line_offset, column_offset);
    } else if (pyTruthy((kind === "MatchExpression"))) {
        result["value"] = offset_expression(expr["value"], line_offset, column_offset);
        result["cases"] = Array.from(expr["cases"]).map((matchCaseValue) => data("MatchCase", { typeName: matchCaseValue["typeName"], bindings: matchCaseValue["bindings"], bindsWholeValue: matchCaseValue["bindsWholeValue"], literal: offset_expression(matchCaseValue["literal"], line_offset, column_offset), hasLiteral: matchCaseValue["hasLiteral"], wildcard: matchCaseValue["wildcard"], guard: offset_expression(matchCaseValue["guard"], line_offset, column_offset), hasGuard: matchCaseValue["hasGuard"], body: offset_expression(matchCaseValue["body"], line_offset, column_offset), location: offset_location(matchCaseValue["location"], line_offset, column_offset) }));
    } else if (pyTruthy((kind === "BlockExpression"))) {
        result["bindings"] = Array.from(expr["bindings"]).map((binding) => data("LetBinding", { name: binding["name"], typeReference: binding["typeReference"], operator: binding["operator"], value: offset_expression(binding["value"], line_offset, column_offset), location: offset_location(binding["location"], line_offset, column_offset) }));
        result["result"] = offset_expression(expr["result"], line_offset, column_offset);
    }
    return result;
}

function offset_data_fields(fields, line_offset, column_offset) {
    return Array.from(fields).map((field) => data("DataField", { name: field["name"], value: offset_expression(field["value"], line_offset, column_offset), spread: field["spread"], location: offset_location(field["location"], line_offset, column_offset) }));
}

function offset_location(location, line_offset, column_offset) {
    var line = (location["line"] + line_offset);
    var column = ((location["line"] === 1) ? (location["column"] + column_offset) : location["column"]);
    return loc(line, column);
}

function parse_type_reference(type_text) {
    if (pyTruthy(((type_text === null) || (type_text === "")))) {
        return missing_type();
    }
    if (pyTruthy(has_top_level_function_arrow(type_text))) {
        return type_ref(type_text, []);
    }
    if (pyTruthy(type_text.endsWith("[]"))) {
        return type_ref("array", [parse_type_reference(pySlice(type_text, null, -2))]);
    }
    var bracket = top_level_generic_bracket(type_text);
    if (pyTruthy((bracket < 0))) {
        return type_ref(type_text, []);
    }
    return type_ref(pySlice(type_text, null, bracket), Array.from(split_type_arguments(pySlice(type_text, (bracket + 1), -1))).map((arg) => parse_type_reference(arg)));
}

function has_top_level_function_arrow(type_text) {
    var bracket_depth = 0;
    var paren_depth = 0;
    for (var index = 0; index < (pyLen(type_text) - 1); index++) {
        var char = type_text[index];
        if (pyTruthy((char === "["))) {
            bracket_depth += 1;
        } else if (pyTruthy((char === "]"))) {
            bracket_depth -= 1;
        } else if (pyTruthy((char === "("))) {
            paren_depth += 1;
        } else if (pyTruthy((char === ")"))) {
            paren_depth -= 1;
        } else if (pyTruthy(((char === "=") && (type_text[(index + 1)] === ">") && (bracket_depth === 0) && (paren_depth === 0)))) {
            return true;
        }
    }
    return false;
}

function top_level_generic_bracket(type_text) {
    var paren_depth = 0;
    var __index_16 = 0;
    for (const char of type_text) {
        const index = __index_16 + 0;
        __index_16++;
        if (pyTruthy((char === "("))) {
            paren_depth += 1;
        } else if (pyTruthy((char === ")"))) {
            paren_depth -= 1;
        } else if (pyTruthy(((char === "[") && (paren_depth === 0)))) {
            return index;
        }
    }
    return -1;
}

function split_type_arguments(type_text) {
    var args = [];
    var depth = 0;
    var start = 0;
    var __index_18 = 0;
    for (const char of type_text) {
        const index = __index_18 + 0;
        __index_18++;
        if (pyTruthy((char === "["))) {
            depth += 1;
        } else if (pyTruthy((char === "]"))) {
            depth -= 1;
        } else if (pyTruthy(((char === ",") && (depth === 0)))) {
            args.push(pySlice(type_text, start, index).trim());
            var start = (index + 1);
        }
    }
    var tail = pySlice(type_text, start, null).trim();
    if (pyTruthy(tail)) {
        args.push(tail);
    }
    return args;
}

function is_index_expression(ctx) {
    return (ctx_has(ctx, "argumentList") && (pyLen(ctx_list(ctx, "expressionNoLet")) === 1) && has_child(ctx, "["));
}

function is_invocation_expression(ctx) {
    return ((pyLen(ctx_list(ctx, "expressionNoLet")) === 1) && has_child(ctx, "("));
}

function is_slice_expression(ctx) {
    return ((pyLen(ctx_list(ctx, "expressionNoLet")) === 1) && has_child(ctx, "[") && has_child(ctx, ":"));
}

function is_slice_expression_no_pipe(ctx) {
    return ((pyLen(ctx_list(ctx, "expressionNoLetNoPipe")) === 1) && has_child(ctx, "[") && has_child(ctx, ":"));
}

function is_invocation_expression_no_pipe(ctx) {
    return ((pyLen(ctx_list(ctx, "expressionNoLetNoPipe")) === 1) && has_child(ctx, "("));
}

function is_unary(ctx) {
    return ((pyLen(ctx_list(ctx, "expressionNoLet")) === 1) && (pyIn(first_child_text(ctx), ["!", "-", ".not."])));
}

function is_unary_no_pipe(ctx) {
    return ((pyLen(ctx_list(ctx, "expressionNoLetNoPipe")) === 1) && (pyIn(first_child_text(ctx), ["!", "-", ".not."])));
}

function unsupported(ctx) {
    return unsupported_expr(text(ctx), source_location(ctx));
}

function object_expression(ctx) {
    var bindings = Array.from(ctx_list(ctx, "letExpression")).map((letValue) => object_let_expression_binding(letValue));
    var result = object_expression_no_let(ctx_call(ctx, "expressionNoLet"));
    if (pyTruthy(!pyTruthy(bindings))) {
        return result;
    }
    return data("BlockExpression", { bindings: bindings, result: result, location: source_location(ctx) });
}

function object_let_expression_binding(ctx) {
    return data("LetBinding", { name: text(ctx_call(ctx, "identifier")), typeReference: (ctx_has(ctx, "type") ? parse_type_reference(text(ctx_call(ctx, "type"))) : missing_type()), operator: text(ctx_call(ctx, "letBindingOperator")), value: object_expression_no_let(ctx_call(ctx, "expressionNoLet")), location: source_location(ctx) });
}

function object_expression_no_let(ctx) {
    if (pyTruthy(ctx_has(ctx, "ifExpression"))) {
        var if_ctx = ctx_call(ctx, "ifExpression");
        return data("IfExpression", { condition: object_expression(ctx_item(if_ctx, "expression", 0)), thenBranch: object_expression(ctx_item(if_ctx, "expression", 1)), elseBranch: object_expression(ctx_item(if_ctx, "expression", 2)), location: source_location(if_ctx) });
    }
    if (pyTruthy(ctx_has(ctx, "newData"))) {
        return object_data_literal(ctx_call(ctx, "newData"));
    }
    if (pyTruthy(ctx_has(ctx, "constructorData"))) {
        return object_constructor_data_literal(ctx_call(ctx, "constructorData"));
    }
    if (pyTruthy(ctx_has(ctx, "matchExpression"))) {
        return object_match_expression(ctx_call(ctx, "matchExpression"));
    }
    if (pyTruthy(ctx_has(ctx, "value"))) {
        return object_value(ctx_call(ctx, "value"));
    }
    if (pyTruthy(ctx_has(ctx, "expression"))) {
        return object_expression(ctx_call(ctx, "expression"));
    }
    var children = ctx_list(ctx, "expressionNoLet");
    if (pyTruthy(((pyLen(children) === 2) && ctx_has(ctx, "infixOperator")))) {
        return data("BinaryExpression", { operator: text(ctx_call(ctx, "infixOperator")), left: object_expression_no_let(children[0]), right: object_expression_no_let(children[1]), location: source_location(ctx) });
    }
    if (pyTruthy(((pyLen(children) === 1) && ctx_has(ctx, "methodIdentifier")))) {
        return data("MethodCallExpression", { receiver: object_expression_no_let(children[0]), name: text(ctx_call(ctx, "methodIdentifier")), arguments: object_method_arguments(ctx_call(ctx, "methodArgumentList")), location: source_location(ctx) });
    }
    if (pyTruthy(((pyLen(children) === 1) && ctx_has(ctx, "identifier") && !pyTruthy(ctx_has(ctx, "methodIdentifier"))))) {
        return data("FieldAccessExpression", { receiver: object_expression_no_let(children[0]), name: text(ctx_call(ctx, "identifier")), location: source_location(ctx) });
    }
    if (pyTruthy(ctx_has(ctx, "functionCall"))) {
        return object_function_call(ctx_call(ctx, "functionCall"));
    }
    if (pyTruthy(ctx_has(ctx, "thisExpression"))) {
        return data("VariableExpression", { name: "this", location: source_location(ctx) });
    }
    return unsupported(ctx);
}

function object_data_literal(ctx) {
    var type_name_value = text(ctx_call(ctx, "type"));
    if (pyTruthy(ctx_has(ctx, "BANG"))) {
        var type_name_value = ("__capy_raw|" + type_name_value);
    }
    return data("DataLiteral", { typeName: type_name_value, fields: object_data_fields(ctx_call(ctx, "fieldAssignmentList")), location: source_location(ctx) });
}

function object_constructor_data_literal(ctx) {
    return data("DataLiteral", { typeName: "*", fields: object_data_fields(ctx_call(ctx, "fieldAssignmentList")), location: source_location(ctx) });
}

function object_data_fields(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    var fields = [];
    var positional_index = 0;
    for (const assignment of ctx_list(ctx, "fieldAssignment")) {
        if (pyTruthy(ctx_has(assignment, "namedFieldAssignment"))) {
            fields.push(object_named_data_field(ctx_call(assignment, "namedFieldAssignment")));
            positional_index += 1;
        } else if (pyTruthy(ctx_has(assignment, "spreadFieldAssignment"))) {
            fields.push(data("DataField", { name: "", value: object_expression(ctx_call(ctx_call(assignment, "spreadFieldAssignment"), "expression")), spread: true, location: source_location(assignment) }));
        } else if (pyTruthy(ctx_has(assignment, "positionalFieldAssignment"))) {
            fields.push(data("DataField", { name: ("$" + String(positional_index)), value: object_expression(ctx_call(ctx_call(assignment, "positionalFieldAssignment"), "expression")), spread: false, location: source_location(assignment) }));
            positional_index += 1;
        } else {
            fields.push(data("DataField", { name: "$unsupported", value: unsupported(assignment), spread: false, location: source_location(assignment) }));
            positional_index += 1;
        }
    }
    return fields;
}

function object_named_data_field(ctx) {
    var name = (ctx_has(ctx, "identifier") ? text(ctx_call(ctx, "identifier")) : unquote(text(ctx_call(ctx, "STRING_LITERAL"))));
    return data("DataField", { name: name, value: object_expression(ctx_call(ctx, "expression")), spread: false, location: source_location(ctx) });
}

function object_match_expression(ctx) {
    var cases = [];
    for (const case_list of ctx_list(ctx, "matchCaseList")) {
        for (const match_case_ctx of ctx_list(case_list, "matchCase")) {
            cases.push(...object_match_cases(match_case_ctx));
        }
    }
    return data("MatchExpression", { value: object_expression(ctx_call(ctx, "expression")), cases: cases, location: source_location(ctx) });
}

function object_match_cases(ctx) {
    var patterns = ctx_list(ctx, "pattern");
    if (pyTruthy(!pyTruthy(patterns))) {
        return [object_match_case(ctx, null)];
    }
    return Array.from(patterns).map((pattern) => object_match_case(ctx, pattern));
}

function object_match_case(ctx, pattern) {
    var guard = ((ctx) == null ? null : ((ctx)["guard"] ?? null));
    var body = ((ctx) == null ? null : ((ctx)["body"] ?? null));
    return data("MatchCase", { typeName: object_pattern_type_name(pattern), bindings: object_pattern_bindings(pattern), bindsWholeValue: object_pattern_binds_whole_value(pattern), literal: object_pattern_literal(pattern), hasLiteral: object_pattern_has_literal(pattern), wildcard: object_pattern_wildcard(pattern), guard: ((guard === null) ? unsupported(ctx) : object_expression(guard)), hasGuard: (guard !== null), body: object_expression(body), location: source_location(ctx) });
}

function object_pattern_type_name(ctx) {
    return pattern_type_name(ctx);
}

function object_pattern_bindings(ctx) {
    return pattern_bindings(ctx, true);
}

function object_pattern_wildcard(ctx) {
    return pattern_wildcard(ctx);
}

function object_pattern_binds_whole_value(ctx) {
    return pattern_binds_whole_value(ctx);
}

function object_pattern_has_literal(ctx) {
    return pattern_has_literal(ctx);
}

function object_pattern_literal(ctx) {
    return pattern_literal(ctx);
}

function object_call_expression(ctx) {
    if (pyTruthy(ctx_has(ctx, "functionCall"))) {
        return object_function_call(ctx_call(ctx, "functionCall"));
    }
    if (pyTruthy(ctx_has(ctx, "methodIdentifier"))) {
        if (pyTruthy(ctx_has(ctx, "thisExpression"))) {
            var receiver = data("VariableExpression", { name: "this", location: source_location(ctx_call(ctx, "thisExpression")) });
        } else if (pyTruthy(ctx_has(ctx, "value"))) {
            var receiver = object_value(ctx_call(ctx, "value"));
        } else if (pyTruthy(ctx_has(ctx, "callExpression"))) {
            var receiver = object_call_expression(ctx_call(ctx, "callExpression"));
        } else {
            var receiver = unsupported(ctx);
        }
        return data("MethodCallExpression", { receiver: receiver, name: text(ctx_call(ctx, "methodIdentifier")), arguments: object_method_arguments(ctx_call(ctx, "methodArgumentList")), location: source_location(ctx) });
    }
    if (pyTruthy((ctx_has(ctx, "callExpression") && ctx_has(ctx, "argumentList")))) {
        var receiver = object_call_expression(ctx_call(ctx, "callExpression"));
        if (pyTruthy((type_name(receiver) === "VariableExpression"))) {
            return data("FunctionCallExpression", { name: receiver["name"], arguments: object_arguments(ctx_call(ctx, "argumentList")), location: source_location(ctx) });
        }
        return data("MethodCallExpression", { receiver: receiver, name: "__capy_call", arguments: object_arguments(ctx_call(ctx, "argumentList")), location: source_location(ctx) });
    }
    return unsupported(ctx);
}

function object_function_call(ctx) {
    return data("FunctionCallExpression", { name: function_call_name(text(ctx)), arguments: object_arguments(ctx_call(ctx, "argumentList")), location: source_location(ctx) });
}

function function_call_name(source) {
    var arguments_start = source.indexOf("(");
    return ((arguments_start < 0) ? source : pySlice(source, null, arguments_start));
}

function object_arguments(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    return Array.from(ctx_list(ctx, "expression")).map((argument) => object_expression(argument));
}

function object_method_arguments(ctx) {
    if (pyTruthy((ctx === null))) {
        return [];
    }
    var result = [];
    for (const argument of ctx_list(ctx, "methodArgument")) {
        if (pyTruthy(ctx_has(argument, "namedMethodArgument"))) {
            result.push(object_expression(ctx_call(ctx_call(argument, "namedMethodArgument"), "expression")));
        } else {
            result.push(object_expression(ctx_call(argument, "expression")));
        }
    }
    return result;
}

function object_value(ctx) {
    if (pyTruthy(ctx_has(ctx, "literal"))) {
        return object_literal(ctx_call(ctx, "literal"));
    }
    if (pyTruthy(ctx_has(ctx, "identifier"))) {
        return data("VariableExpression", { name: text(ctx_call(ctx, "identifier")), location: source_location(ctx) });
    }
    if (pyTruthy(ctx_has(ctx, "qualifiedType"))) {
        return data("VariableExpression", { name: text(ctx_call(ctx, "qualifiedType")), location: source_location(ctx) });
    }
    return unsupported(ctx);
}

function object_literal(ctx) {
    var source = text(ctx);
    var location = source_location(ctx);
    if (pyTruthy(ctx_has(ctx, "INT_LITERAL"))) {
        return data("IntLiteral", { value: Number.parseInt(clean_number(source), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "LONG_LITERAL"))) {
        return data("LongLiteral", { value: Number.parseInt(clean_number(pySlice(source, null, -1)), 10), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "FLOAT_LITERAL"))) {
        return data("FloatLiteral", { value: Number.parseFloat(clean_float(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "DOUBLE_LITERAL"))) {
        return data("DoubleLiteral", { value: Number.parseFloat(clean_double(source)), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "BOOL_LITERAL"))) {
        return data("BoolLiteral", { value: bool_value(source), source: source, location: location });
    }
    if (pyTruthy(ctx_has(ctx, "STRING_LITERAL"))) {
        return data("StringLiteral", { value: unquote(source), source: source, location: location });
    }
    return unsupported(ctx);
}

module.exports = { NativeCapybaraParser };
