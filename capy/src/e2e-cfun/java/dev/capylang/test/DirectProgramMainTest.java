package dev.capylang.test;

import capy.lang.Program;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class DirectProgramMainTest {
    @Test
    void directProgramMainIsOrdinaryFunctionOnly() throws Exception {
        var mainMethod = DirectProgramMain.class.getMethod("main", List.class);

        assertThat(mainMethod.invoke(null, List.of())).isInstanceOf(Program.Success.class);
        assertThatThrownBy(() -> DirectProgramMain.class.getMethod("main", String[].class))
                .isInstanceOf(NoSuchMethodException.class);
    }
}
