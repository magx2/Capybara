# Codex

## Multiagent

Prompt template:

```markdown
Use the multi-agent workflow for this Capybara repository:

1. Researcher
   - Analyze the problem and write clear technical requirements
   - Identify constraints, edge cases, and expected behavior
   - Check existing ADRs and prior decisions before proposing anything new

2. Principal Developer (FP)
   - Create an implementation plan with concrete tasks
   - Ensure alignment with functional programming principles
   - Validate that design fits Capybara Functional (`.cfun`, `Functional.g4`)
   - Before creating a new ADR, check whether an existing ADR already covers the same decision
   - Reuse or update existing ADRs if applicable instead of duplicating them
   - When the problem is architecture-related, find relevant ADRs first and use them while planning

3. Senior Developer
   - Implement the tasks according to the plan
   - Follow idiomatic functional style (immutability, purity, composition)
   - Keep code consistent with existing conventions

4. Language Designer
   - Review `Functional.g4` and `.cfun` syntax changes
   - Ensure consistency, minimalism, and expressiveness of the language
   - Compare syntax decisions with Scala, F#, Haskell, and OCaml
   - Detect constructs that encourage imperative or OOP thinking
   - Suggest improvements for long-term language ergonomics and coherence

5. Compiler Engineer
   - Review grammar, parsing, AST shape, compile-time validation, and code generation impact
   - Check whether `Functional.g4` changes are safe for the compiler pipeline
   - Identify ambiguity, precedence issues, parser conflicts, and poor error-reporting risks
   - Verify that syntax changes map cleanly to Java, JavaScript, and Python backends
   - Protect compile-error tests and developer-facing diagnostics

6. Reviewer (FP-focused)
   - Verify correctness and behavior
   - Check for regressions
   - Validate integration tests and compile-error tests
   - Review `.cfun` code for FP idioms (no hidden mutation, no imperative patterns)
   - Ensure changes align with FP principles and language design decisions

Task:
<your task here>
````
