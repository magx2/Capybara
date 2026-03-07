package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;

class MainProgramTest {
    @Test
    void generatedMainHasJavaEntryPointSignature() throws Exception {
        var mainMethod = MainProgram.class.getMethod("main", String[].class);

        assertThat(Modifier.isPublic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isStatic(mainMethod.getModifiers())).isTrue();
        assertThat(mainMethod.getReturnType()).isEqualTo(void.class);
        assertThat(mainMethod.isVarArgs()).isFalse();
    }

    @Test
    void generatedMainPrintsSuccessResults() throws Exception {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));
            MainProgram.class.getMethod("main", String[].class).invoke(null, (Object) new String[]{"ok"});
        } finally {
            System.setOut(originalOut);
        }

        var lines = Arrays.stream(out.toString().split(System.lineSeparator()))
                .filter(line -> !line.isBlank())
                .toList();
        assertThat(lines).containsExactly("ok");
    }
}
