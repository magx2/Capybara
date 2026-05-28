package dev.capylang.cli.bootstrap;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class CliSelfHostingSmokeTest {
    @Test
    void generatedCliSmokeClassIsCallable() {
        assertThat(SelfHostingBootstrapSmoke.selfHostingBootstrapSmoke()).isEqualTo(184);
    }
}
