package dev.capylang.compiler.bootstrap;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class CompilerSelfHostingSmokeTest {
    @Test
    void generatedCompilerSmokeClassIsCallable() {
        assertThat(SelfHostingBootstrapSmoke.selfHostingBootstrapSmoke()).isEqualTo(184);
    }
}
