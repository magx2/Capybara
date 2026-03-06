package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ImportsTest {
    @Test
    void importsFunctionFromOtherModule() {
        assertThat(pl.grzeslowski.capybara.test.imports.Main.calculator(2, 3, "+"))
                .isEqualTo(5);
        assertThat(pl.grzeslowski.capybara.test.imports.Main.calculator(2, 3, "-"))
                .isEqualTo(-1);
        assertThat(pl.grzeslowski.capybara.test.imports.Main.calculator(2, 3, "*"))
                .isEqualTo(6);
        assertThat(pl.grzeslowski.capybara.test.imports.Main.calculator(8, 2, "/"))
                .isEqualTo(4);
        assertThat(pl.grzeslowski.capybara.test.imports.Main.calculator(2, 3, "%"))
                .isEqualTo(1);
        assertThat(pl.grzeslowski.capybara.test.imports.Main.calculator(15, 0, "p3"))
                .isEqualTo(100);
    }
}
