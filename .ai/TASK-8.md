# TASK-8: Create the pull request with gh

Goal: push the issue branch and open a pull request for #174 using the GitHub CLI.

Preconditions:

- The current branch is `docs/174-native-host-wiring`.
- The issue #174 documentation commit exists locally.
- `gh auth status` succeeds.

Steps:

1. Push the branch:

   ```bash
   git push -u origin docs/174-native-host-wiring
   ```

2. Create the pull request:

   ```bash
   gh pr create \
     --title "docs(#174): describe native host wiring" \
     --body "## What changed
- Documented a design for Capybara OO native host interoperability.
- Covered compile-time provider registration and dependency injection from Java, JavaScript, and Python.
- Added validation rules, runtime boundary rules, and backend-specific wiring notes.

## Impacted modules
- .ai documentation only.

## Tests
- Not run; documentation/task-only change.

Closes #174"
   ```

3. Capture the PR URL from `gh pr create` output.

Expected result:

- A PR is open for issue #174.
- The PR body includes what changed, impacted modules, and commands/tests run.
