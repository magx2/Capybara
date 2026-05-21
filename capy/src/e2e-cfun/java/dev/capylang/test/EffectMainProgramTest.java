package dev.capylang.test;

import capy.lang.Effect;
import capy.lang.Program;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.ResourceLock;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Modifier;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@ResourceLock("java.lang.System.out")
class EffectMainProgramTest {
    @Test
    void generatedEffectMainHasJavaEntryPointSignature() throws Exception {
        var mainMethod = EffectMainProgram.class.getMethod("main", String[].class);

        assertThat(Modifier.isPublic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isStatic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isFinal(mainMethod.getModifiers())).isTrue();
        assertThat(mainMethod.getReturnType()).isEqualTo(void.class);
        assertThat(mainMethod.isVarArgs()).isTrue();
    }

    @Test
    void generatedEffectMainKeepsCapybaraMainFunction() throws Exception {
        var mainMethod = EffectMainProgram.class.getMethod("main", List.class);

        assertThat(Modifier.isPublic(mainMethod.getModifiers())).isTrue();
        assertThat(Modifier.isStatic(mainMethod.getModifiers())).isTrue();
        assertThat(mainMethod.getReturnType()).isEqualTo(Effect.class);

        var program = ((Effect<?>) mainMethod.invoke(null, List.of("one", "two"))).unsafeRun();

        assertThat(program).isInstanceOf(Program.Success.class);
    }

    @Test
    void generatedEffectMainDoesNotPrintSuccessProgram() throws Exception {
        var originalOut = System.out;
        var out = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out));
            EffectMainProgram.class.getMethod("main", String[].class).invoke(null, (Object) new String[]{"one", "two"});
        } finally {
            System.setOut(originalOut);
        }

        assertThat(out.toString()).isBlank();
    }
}
