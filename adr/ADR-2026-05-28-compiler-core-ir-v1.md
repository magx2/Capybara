# ADR: Compiler Core IR in Capybara

## Status

Accepted for migration scaffolding. This task adds Capybara-owned model definitions and adapters only; it does not cut production compiler passes over to the new IR.

## Context

Semantic compiler passes are still Java-owned. Before porting those passes, the compiler needs an immutable Capybara model surface for parsed modules, linked/compiled modules, diagnostics, native provider catalogs, and generated output.

The migration rule is:

- Java records and other product/value models become Capybara `data`.
- Java sealed/interface variant hierarchies become Capybara `union`.
- Java enum-like closed sets become Capybara `enum`.
- Java behavior classes and host-resource holders stay on the Java island, or cross the language boundary through native provider declarations/catalogs. Host resources are not stored in `.cfun data`.

## Mapping Table

| Java model family | Java model types | Capybara IR shape |
| --- | --- | --- |
| Module identity and sources | `RawModule`, `SourceKind`, `SourcePosition`, parser facade `SourceModuleDto` | `data ModuleId`, `data SourceDescriptor`, `enum SourceKind`, `data SourcePosition` |
| Imports | `ImportDeclaration`, parser facade import DTOs | `data Import` plus import-line/extraction DTOs at the Java facade boundary |
| Functional program/module products | `Program`, parser `Module`, `Functional` | `data FunctionalProgram`, `data FunctionalModule` |
| Functional declaration variants | `Definition` sealed interface; `Function`, `DataDeclaration`, `TypeDeclaration`, `PrimitiveBackedTypeDeclaration`, `EnumDeclaration`, `DeriverDeclaration`, `AnnotationDeclaration` | `union FunctionalDeclaration` with `data FunctionDeclaration`, `DataDeclaration`, `UnionDeclaration`, `PrimitiveBackedTypeDeclaration`, `EnumDeclaration`, `DeriverDeclaration`, `AnnotationDeclaration` |
| Function payloads | `Parameter`, `DeriveDirective`, `DataDeclaration.DataField`, `DeriverDeclaration.DeriverMethod` | `data Parameter`, `data DeriveDirective`, `data DataField`, `data DeriverMethod` |
| Parsed types | `Type` sealed interface; `PrimitiveType`, `DataType`, `CollectionType.ListType`, `CollectionType.SetType`, `CollectionType.DictType`, `FunctionType`, `TupleType` | `union ParsedType` with primitive, named data, collection, function, and tuple variants; `enum PrimitiveType` |
| Parsed expressions | `Expression` sealed interface; literal values, `Value`, `FunctionCall`, `FunctionInvoke`, `FunctionReference`, `IfExpression`, `InfixExpression`, `FieldAccess`, `LambdaExpression`, `LetExpression`, `ReduceExpression`, `IndexExpression`, `SliceExpression`, `MatchExpression`, `NewData`, `ConstructorData`, `WithExpression`, collection literals, tuple, unwrap, placeholder | `union ParsedExpression` with one `data` alternative per expression variant; `enum InfixOperator`; `enum LetKind` |
| Parsed match patterns | `MatchExpression.Pattern` sealed interface; literal, typed, variable, wildcard, wildcard-binding, constructor patterns | `union ParsedPattern` plus `data` payloads and singleton `data ParsedWildcardPattern` |
| Annotation declarations/usages | `AnnotationDeclaration`, `AnnotationTarget`, `AnnotationFieldDeclaration`, `AnnotationUsage`, `AnnotationArgument`, `AnnotationValue` sealed variants | `data AnnotationDeclaration`, `data AnnotationTarget`, `data AnnotationFieldDeclaration`, `data AnnotationUsage`, `data AnnotationArgument`, `union AnnotationValue` |
| Object-oriented modules | `ObjectOrientedModule`, `ObjectOriented` | `data ObjectOrientedModule` |
| Object-oriented declaration variants | `ObjectOriented.TypeDeclaration` sealed interface; `ClassDeclaration`, `TraitDeclaration`, `InterfaceDeclaration` | `union OoDeclaration` with class, trait, and interface data alternatives |
| Object-oriented member variants | `ObjectOriented.MemberDeclaration` sealed interface; `FieldDeclaration`, `MethodDeclaration`, `InitBlock` | `union OoMemberDeclaration` |
| Object-oriented method bodies/statements | `ObjectOriented.MethodBody`, `ObjectOriented.Statement` sealed interfaces and all statement records | `union OoMethodBody`, `union OoStatement`, plus product data for catch clauses, parameters, and references |
| Compiled program/module products | `CompiledProgram`, `CompiledModule`, `CompiledModule.StaticImport` | `data CompiledProgram`, `data CompiledModule`, `data StaticImport`, `data CompiledTypeEntry` |
| Compiled function products | `CompiledFunction`, `CompiledFunction.CompiledFunctionParameter` | `data CompiledFunction`, `data CompiledFunctionParameter` |
| Compiled type hierarchy | `CompiledType` sealed interface; `GenericDataType`, `CollectionLinkedType`, `PrimitiveLinkedType`, `CompiledGenericTypeParameter`, `CompiledFunctionType`, `CompiledTupleType`; `CompiledDataType`, `CompiledDataParentType`, `CompiledObjectType`, `CompiledPrimitiveBackedType` | `union CompiledType`, `union GenericCompiledType`, `union CompiledCollectionType`, `enum PrimitiveType`, `enum CompiledObjectKind`, and data alternatives for each product payload |
| Compiled fields/object methods | `CompiledDataType.CompiledField`, `CompiledObjectMethod`, `CompiledObjectMethodParameter` | `data CompiledField`, `data CompiledObjectMethod`, `data CompiledObjectMethodParameter`, `data BackendMethodName` |
| Compiled expressions | `CompiledExpression` sealed interface and all `Compiled*Expression`/literal/value records/enums | `union CompiledExpression` with one data alternative per expression variant |
| Compiled match patterns | `CompiledMatchExpression.Pattern` sealed interface and all pattern records/enums | `union CompiledPattern` |
| Compiled annotations | `CompiledAnnotation`, `CompiledAnnotationArgument`, `CompiledAnnotationValue` sealed variants | `data CompiledAnnotation`, `data CompiledAnnotationArgument`, `union CompiledAnnotationValue` |
| Diagnostics/results | parser syntax diagnostics, `Result.Success`, `Result.Error`, `Result.Error.SingleError` | `union IrResult[T]`, `data IrSuccess[T]`, `data IrFailure`, `union Diagnostic`, `data SyntaxDiagnostic`, `data CompilationDiagnostic` |
| Native provider declarations/catalogs | parser OO native provider declarations; `NativeProviderManifest`, `NativeProviderCatalog`, `NativeProviderBinding`, `NativeProviderBackendBinding`, `CompiledNativeProviderDeclaration`, `CompiledNativeProviderBinding`, `NativeProviderBackend` | `data OoNativeProviderDeclaration`, `data NativeProviderManifest`, `data NativeProviderCatalog`, `data CompiledNativeProviderDeclaration`, `data NativeProviderBinding`, `union NativeProviderBackendBinding`, `enum NativeProviderBackend` |
| Generated output | `GeneratedModule`, `GeneratedProgram`, `OutputType` | `data GeneratedModule`, `data GeneratedProgram`, `enum OutputType` |
| Behavior and host-resource classes | `CapybaraParser`, `ObjectOrientedParser`, parser facades, `ParserImportPreprocessor`, `CapybaraCompiler`, `CapybaraTypeCompiler`, `CapybaraExpressionCompiler`, `CapybaraTypeFinder`, `ObjectOrientedValidator`, `NativeImplementationScanner`, `NativeAnnotations`, code generators, Java AST builder/evaluator helpers, result collectors, scopes | Remain Java behavior for now. Future `.coo` shell objects or native provider boundaries may expose behavior without storing host resources in `.cfun data`. |

## Decision

Add `compiler/src/main/capybara/dev/capylang/compiler/ir/CoreIr.cfun` as the Capybara-owned model source. The generated Java classes are treated as an internal implementation detail. Java tests and compiler code interact through package-private adapters and stable DTOs, not by promoting generated names to public API.

The first adapter surface supports representative round trips:

1. legacy parsed functional/OO models -> Capybara IR -> existing parser facade DTOs -> legacy parsed models;
2. legacy compiled/native/generated models -> Capybara IR -> stable core IR DTOs -> legacy models;
3. no-op pass-through functions over parsed, compiled, and generated IR.

## Consequences

- Compiler model migration can proceed pass-by-pass without changing production behavior in this task.
- Capybara IR definitions make expression/type/declaration/provider variants explicit and keep invalid variant combinations out of product data where practical.
- Java-only behavior remains outside `.cfun data`; only serializable compiler state crosses this boundary.
