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
        assertThat(pl.grzeslowski.capybara.test.imports.Main.x(2, 3, "+"))
                .isEqualTo("Day as usual");
    }

    @Test
    void supportsQualifiedImportedTypes() {
        var value = pl.grzeslowski.capybara.test.imports.Main.t1(7);
        assertThat(value).isInstanceOf(pl.grzeslowski.capybara.test.imports.Types2.TD1.class);
    }

    @Test
    void supportsQualifiedImportedDataTypeConstruction() {
        var value = pl.grzeslowski.capybara.test.imports.Main.xyz(1, 2, 3);
        assertThat(value).isInstanceOf(pl.grzeslowski.capybara.test.imports.Types2.DataX.class);
        assertThat(value.xyz()).isEqualTo("123");
    }

    @Test
    void supportsFullyQualifiedTypesAndDataConstructors() {
        var positive = pl.grzeslowski.capybara.test.imports.Main.type5(5, 2);
        assertThat(positive).isInstanceOf(pl.grzeslowski.capybara.test.imports2.Types3.D51.class);
        assertThat(((pl.grzeslowski.capybara.test.imports2.Types3.D51) positive).x()).isEqualTo(5);

        var nonPositive = pl.grzeslowski.capybara.test.imports.Main.type5(0, 7);
        assertThat(nonPositive).isInstanceOf(pl.grzeslowski.capybara.test.imports2.Types3.D52.class);
        assertThat(((pl.grzeslowski.capybara.test.imports2.Types3.D52) nonPositive).y()).isEqualTo(7);
    }
}
