# Capybara E2E Test Conventions

Use Capybara test companions for migrated e2e behavior so the same assertions run on Java, JavaScript, and Python generated backends.

## Functional Tests

Place functional companions under `src/e2e-cfun/capybara/dev/capylang/test` and name them `SourceModuleCapyTest.cfun`. Import the behavior module and expose a zero-argument producer named `tests` returning `Effect[TestFile]`.

```cfun
from /capy/test/Assert import { * }
from /capy/test/CapyTest import { * }
from /capy/lang/Effect import { * }
from E2eTestSupport import { e2e_test_file }
from /dev/capylang/test/SourceModule import { * }

fun tests(): Effect[TestFile] =
    e2e_test_file("SourceModule.cfun", [
        test("should describe expected behavior", () => should_describe_expected_behavior()),
    ])

fun should_describe_expected_behavior(): Assert =
    assert_that(actual()).is_equal_to(expected)
```

## Object-Oriented Tests

Place object-oriented companions under `src/e2e-coo/capybara/dev/capylang/test` and name them `SourceModuleCapyTest.cfun`. Use `E2eObjectTestSupport.e2e_object_test_file` and point the source name at the behavior file being tested, whether that file is `.cfun` or `.coo`.

## Rules

- Keep test assertions in Capybara and avoid Java, Node, or Python host APIs.
- Use `test_file` names for the behavior source, not the companion test file.
- Keep migrated companion names ending in `CapyTest.cfun` until the old host e2e tests are removed.
- Prefer `assert_that` and `assert_all` from `/capy/test/Assert`; add shared assertions there only when they are generally useful and covered by `:lib:capybara-lib:testCapybara`.
