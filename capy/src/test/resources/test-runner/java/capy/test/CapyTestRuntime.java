package capy.test;

import java.util.List;

public final class CapyTestRuntime {
    private CapyTestRuntime() {
    }

    public static List<Object> gatherTests() {
        return List.of(new CapyTest.TestFile(
                "/sample/JavaRunnerTest.cfun",
                List.of(CapyTest.test(
                        "passes from packaged Java runner",
                        () -> Assert.assertThat__int(1).isEqualTo(1)
                )),
                0L
        ));
    }
}
