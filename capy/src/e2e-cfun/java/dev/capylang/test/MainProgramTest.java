package dev.capylang.test;

import capy.lang.Effect;
import capy.lang.Program;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.ResourceLock;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@ResourceLock("java.lang.System.out")
class MainProgramTest {
    @Test
    void generatedMainHasJavaEntryPointSignature() throws Exception {
        var mainMethod = MainProgram.class.getMethod("main", String[].class);

        assertThat(Modifier.isPublic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isStatic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isFinal(mainMethod.getModifiers())).isTrue();
        assertThat(mainMethod.getReturnType()).isEqualTo(void.class);
        assertThat(mainMethod.isVarArgs()).isTrue();
    }

    @Test
    void generatedMainKeepsCapybaraMainFunction() throws Exception {
        var mainMethod = MainProgram.class.getMethod("main", List.class);

        assertThat(Modifier.isPublic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isStatic(mainMethod.getModifiers())).isTrue();
        assertThat(mainMethod.getReturnType()).isEqualTo(Effect.class);

        var program = ((Effect<?>) mainMethod.invoke(null, List.of("ok"))).unsafeRun();

        assertThat(program).isInstanceOf(Program.Success.class);
    }

    @Test
    void generatedMainKeepsFailedProgramExitCode() throws Exception {
        var mainMethod = MainProgram.class.getMethod("main", List.class);

        var program = ((Effect<?>) mainMethod.invoke(null, List.of())).unsafeRun();

        assertThat(program).isInstanceOf(Program.Failed.class);
        assertThat(((Program.Failed) program).exit_code()).isEqualTo(1);
    }

    @Test
    void generatedMainDoesNotPrintSuccessProgram() throws Exception {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));
            MainProgram.class.getMethod("main", String[].class).invoke(null, (Object) new String[]{"ok"});
        } finally {
            System.setOut(originalOut);
        }

        assertThat(out.toString()).isBlank();
    }

    @Test
    void generatedMainExitsWithFailedProgramCode() throws Exception {
        var process = new ProcessBuilder(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "-cp",
                System.getProperty("java.class.path"),
                MainProgram.class.getName()
        ).start();

        var stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var stderr = new String(process.getErrorStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        var exit = process.waitFor();

        assertThat(exit).isEqualTo(1);
        assertThat(stdout).isBlank();
        assertThat(stderr).isBlank();
    }
}
