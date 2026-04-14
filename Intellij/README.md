# IntelliJ ❤️ Capy

This folder contains a TextMate grammar for Capybara (`*.cfun`, `*.coo`) that can be imported into IntelliJ IDEA.

## Install in IntelliJ

1. Open `Settings` -> `Editor` -> `TextMate Bundles`.
2. Click `+` and select this folder: `Intellij ❤️ Capy`.
3. Apply changes and reopen any `.cfun` or `.coo` file.

## Included

- `package.json`: language metadata and grammar registration.
- `syntaxes/capybara.tmLanguage.json`: syntax highlighting rules.

## Notes

- The grammar is intentionally lightweight and focuses on readability.
- It highlights keywords, types, functions, comments (`//`, `///`, `/* ... */`), numbers, strings (`"` and `'`), and operators.

Alternative import path (recommended for IntelliJ TextMate): Intellij/Capybara.tmBundle
