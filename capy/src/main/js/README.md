# JavaScript Native Sources

The Capybara JavaScript backend emits CommonJS modules, so JavaScript native
providers in this tree are CommonJS too.

Generated Capybara JavaScript modules use relative CommonJS `require(...)`
imports for other Capybara modules. Native provider shims that replace generated
`<native>` providers therefore use the same module path as the generated
provider module and are copied into the prepared runtime tree at that path. For
example, `dev/capylang/compiler/parser/CapybaraParserProvider.js` exports
`parser()` and loads `NativeCapybaraParser` next to it.

ANTLR 4.13 generates JavaScript parser files as ESM. The Gradle
`generateJavaScriptAntlr` task runs `antlr4 -Dlanguage=JavaScript` and converts
those generated files to CommonJS before they are placed on `NODE_PATH` for the
native parser provider.
