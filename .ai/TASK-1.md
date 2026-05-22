# TASK-1: Create the issue branch for #174

Goal: start issue #174 work from the current base branch.

Steps:

1. Check the current state:

   ```bash
   git status --short
   git branch --show-current
   ```

2. If the worktree contains unrelated user changes, leave them untouched and note them before continuing.

3. Create a branch that includes the issue number:

   ```bash
   git switch -c docs/174-native-host-wiring
   ```

4. Confirm the branch:

   ```bash
   git branch --show-current
   ```

Expected result:

- The active branch is `docs/174-native-host-wiring`.
- Future commits can use the repository convention `docs(#174): ...`.
