package dev.capylang.test;

import capy.lang.Effect;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class EffectBindTest {
    @Test
    void bindsEffectsLazily() {
        assertThat(EffectBind.addEffects(2, 3).unsafeRun()).isEqualTo(5);
        assertThat(EffectBind.nestedEffect(7).unsafeRun()).isEqualTo("v=7");
    }

    @Test
    void constructingEffectDoesNotRunThunk() {
        assertThat(EffectBind.constructDelayedDivideByZero()).isTrue();

        var delayedFailure = EffectBind.delayedDivideByZero();
        assertThatThrownBy(delayedFailure::unsafeRun)
                .isInstanceOf(ArithmeticException.class);
    }

    @Test
    void effectIsLazyAndNonMemoized() {
        var counter = new AtomicInteger();
        var effect = Effect.delay(counter::incrementAndGet);

        assertThat(counter).hasValue(0);
        assertThat(effect.unsafeRun()).isEqualTo(1);
        assertThat(effect.unsafeRun()).isEqualTo(2);
    }

    @Test
    void clockNowReturnsCurrentDateTimeEffect() {
        assertThat(EffectBind.clockNowIso().unsafeRun()).contains("T");
    }
}
