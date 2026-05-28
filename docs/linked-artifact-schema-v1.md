# Capybara Linked Artifact Schema v1

This document defines the transition contract for linked compiler artifacts used
between `compile` and `generate`. The contract is language-neutral and must not
treat Java class names, Jackson default typing metadata, Java collection class
names, record names, or package names as public schema.

## Versioning

Linked artifacts are JSON objects with:

- `format`: one of `capybara.linked.program` or `capybara.linked.module`.
- `schemaVersion`: integer schema version. Version `1` is the only supported
  schema version.
- `program` or `module`: the payload.

Readers must reject unknown `format` values and unsupported `schemaVersion`
values with deterministic diagnostics. Transition readers must also read legacy
Java/Jackson artifacts that do not contain `format`.

## Current Serialized Java Inventory

Legacy artifacts may contain values shaped from these Java types:

- Program and modules: `CompiledProgram`, `CompiledModule`,
  `CompiledModule.StaticImport`.
- Declarations and types: `CompiledFunction`,
  `CompiledFunction.CompiledFunctionParameter`, `GenericDataType`,
  `CompiledDataType`, `CompiledDataType.CompiledField`,
  `CompiledDataParentType`, `CompiledPrimitiveBackedType`,
  `CompiledObjectType`, `CompiledObjectKind`, `CompiledObjectMethod`,
  `CompiledObjectMethodParameter`, `CompiledType`, `PrimitiveLinkedType`,
  `CollectionLinkedType.CompiledList`, `CollectionLinkedType.CompiledSet`,
  `CollectionLinkedType.CompiledDict`, `CompiledTupleType`,
  `CompiledFunctionType`, `CompiledGenericTypeParameter`.
- Compiled expressions:
  `CompiledBooleanValue`, `CompiledByteValue`, `CompiledDoubleValue`,
  `CompiledEffectBindExpression`, `CompiledEffectExpression`,
  `CompiledFieldAccess`, `CompiledFloatValue`, `CompiledFunctionCall`,
  `CompiledFunctionInvoke`, `CompiledIfExpression`,
  `CompiledIndexExpression`, `CompiledInfixExpression`, `CompiledIntValue`,
  `CompiledLambdaExpression`, `CompiledLetExpression`, `CompiledLongValue`,
  `CompiledMatchExpression` and its pattern records/enums,
  `CompiledNothingValue`, `CompiledNumericWidening`,
  `CompiledObjectConstruction`, `CompiledPipeExpression`,
  `CompiledPipeFilterOutExpression`, `CompiledPipeFlatMapExpression`,
  `CompiledPipeReduceExpression`, `CompiledReflectionValue`,
  `CompiledReflectionValue.Field`, `CompiledSliceExpression`,
  `CompiledStringValue`, `CompiledTupleExpression`,
  `CompiledUnwrapExpression`, `CompiledNewData`,
  `CompiledNewData.FieldAssignment`, `CompiledNewDict`,
  `CompiledNewDict.Entry`, `CompiledNewList`, `CompiledNewSet`,
  `CompiledVariable`.
- Annotations and parser metadata embedded in declarations:
  `CompiledAnnotation`, `CompiledAnnotationArgument`,
  `CompiledAnnotationValue`, parser `AnnotationDeclaration`,
  `AnnotationTarget`, `AnnotationFieldDeclaration`, `AnnotationUsage`,
  `AnnotationArgument`, `AnnotationValue`, `DeriverDeclaration`,
  `DeriverDeclaration.DeriverMethod`, parser `Type` variants, parser
  `Expression` variants, `SourcePosition`, `Visibility`.
- OO module metadata: `ObjectOrientedModule`, `ObjectOriented`,
  all `ObjectOriented.TypeDeclaration`, `MemberDeclaration`, `MethodBody`,
  `Statement`, `CatchClause`, `Parameter`, `TypeReference`, and
  `NativeProviderDeclaration` variants.
- Native provider metadata: `NativeProviderManifest`,
  `NativeProviderBinding`, `NativeProviderBackendBinding`,
  `NativeProviderBackend`, `NativeProviderCatalog`,
  `CompiledNativeProviderDeclaration`, `CompiledNativeProviderBinding`.
- Generated metadata: CLI `build-info.json` contains
  `capybara_compiler_version` and ordered `source_modules` entries.

Public contract fields are the language concepts in those objects: names, paths,
types, parameters, expressions, annotations, source positions, visibility,
native provider backend metadata, and generated source-module refs. Java
implementation details are not public: Jackson `@class`, Java collection type
wrappers, Java sealed subtype names, Java enum implementation names beyond the
documented symbolic values, and record package names.

## Polymorphic Values

Schema v1 uses logical wrapper keys for polymorphic values. Examples:

```json
{ "primitive": "INT" }
{ "list": { "elementType": { "primitive": "STRING" } } }
{ "int": { "intValue": "7" } }
{ "function-call": { "name": "answer", "arguments": [], "returnType": { "primitive": "INT" } } }
```

The logical keys are stable contract names. They map to compiler concepts, not
Java classes.

## Program

`capybara.linked.program` payload:

- `modules`: linked functional modules.
- `objectOrientedModules`: linked OO module metadata.
- `nativeProviders`: manifest bindings requested by the build.
- `nativeProviderCatalog`: provider declarations and resolved backend bindings.

## Module

`capybara.linked.module` payload:

- `name`: module name without extension.
- `path`: slash-normalized module path without a leading slash.
- `types`: exported and visible data/object/type declarations keyed by symbol.
- `visiblePrimitiveBackedTypes`: primitive-backed helper types visible to the
  module.
- `functions`: compiled functions.
- `derivers`: parser-level deriver declarations keyed by name.
- `annotations`: annotation declarations keyed by name.
- `staticImports`: backend static imports needed by Java generation.

## Diagnostics

Compiler diagnostics use:

- `file`: normalized source file path.
- `line`: one-based line number, or `0` when unavailable.
- `column`: zero-based column, or `0` when unavailable.
- `message`: stable diagnostic text.

Diagnostics are ordered by `file`, `line`, `column`, then `message`.

## Native Provider Manifest and Catalog

Manifest bindings use the existing public JSON names:

- `interface`: stable interface id.
- `qualifier`: provider qualifier, defaulting to the empty string.
- `java.className`, `java.factory`.
- `javascript.module`, `javascript.export`, `javascript.factory`.
- `python.module`, `python.className`, `python.factory`.

Catalog entries preserve the same backend fields plus declaration metadata:
provider function name, source module path/name, target type name, interface id,
qualifier, and source file. These fields are needed by Java, JavaScript, and
Python generators.

## Generated Program Metadata

`build-info.json` remains a transition metadata file. Its public fields are:

- `capybara_compiler_version`.
- `source_modules`: objects with `name` and slash-normalized `path`.

Generators use `source_modules` to filter linked library modules from generated
output. Missing `build-info.json` means all linked modules are generation input.

## Ordering Rules

Writers must produce deterministic output:

- Modules are ordered by `path`, then `name`.
- OO modules are ordered by `path`, then `name`.
- Module maps are ordered by key.
- Functions are ordered by name, arity, parameter type descriptor, then
  parameter name.
- Static imports are ordered by class name, member name, then enum flag.
- Native provider declarations and bindings are ordered by interface id,
  qualifier, source module, provider name, target type, source file, and backend
  binding metadata.
- Manifest providers are ordered by interface id, qualifier, and backend binding
  metadata.
- Generated file manifests are ordered by slash-normalized relative path.
- Duplicate errors and diagnostics are ordered by file, line, column, message.

`Dict[T]` remains string-keyed in schema v1. Future compound keys must either be
normalized to stable string keys or represented as an explicit ordered-map
abstraction.

## Compatibility

Transition readers must:

- Read schema v1 artifacts.
- Read old Java-produced linked artifacts with Jackson default typing.
- Reject unsupported schema versions before semantic compilation/generation.

Transition writers write schema v1 artifacts. Legacy Java default typing remains
an input compatibility path only.
