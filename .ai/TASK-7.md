# TASK-7: Verify and commit #174 documentation changes

Goal: review the documentation/task changes and create one scoped commit for issue #174.

Steps:

1. Review the diff:

   ```bash
   git diff -- .ai/native.md .ai/TASK-*.md
   git status --short
   ```

2. If only documentation and task files changed, no Gradle test is required. Record this as:

   ```text
   Tests not run; documentation/task-only change.
   ```

3. Stage the intended files:

   ```bash
   git add .ai/native.md .ai/TASK-*.md
   ```

4. If `.ai` files are intentionally ignored by the repository, decide whether they should remain local task files or be force-added. Do not use `git add -f` unless these task files should be part of the commit.

5. Commit with the issue number:

   ```bash
   git commit -m "docs(#174): describe native host wiring"
   ```

Expected result:

- One documentation commit exists on the issue branch.
- The commit message follows the repository convention and includes `#174`.
