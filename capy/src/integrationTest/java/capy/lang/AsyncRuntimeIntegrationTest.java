package capy.lang;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class AsyncRuntimeIntegrationTest {
    @Test
    void runsWithTheCompilerEffectRuntime() {
        var results = Async.all(List.of(Async.start(Effect.pure("done")))).unsafeRun();

        assertThat(results).hasSize(1);
        assertThat(results.getFirst()).isInstanceOfSatisfying(Map.class, result -> {
            assertThat(result.get("__type")).isEqualTo("Success");
            assertThat(result.get("value")).isEqualTo("done");
        });
    }
}
