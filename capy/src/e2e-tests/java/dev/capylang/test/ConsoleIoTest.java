package dev.capylang.test;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.ResourceLock;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@ResourceLock("java.lang.System.out")
@ResourceLock("java.lang.System.err")
@ResourceLock("java.lang.System.in")
class ConsoleIoTest {
    @Test
    void generatedConsoleEffectsWriteToStdoutAndStderr() throws Exception {
        var originalOut = System.out;
        var originalErr = System.err;
        var out = new ByteArrayOutputStream();
        var err = new ByteArrayOutputStream();
        try {
            System.setOut(new PrintStream(out, true, StandardCharsets.UTF_8));
            System.setErr(new PrintStream(err, true, StandardCharsets.UTF_8));

            assertThat(ConsoleIo.emitAll().unsafeRun()).isEqualTo(" line");
            assertThat(ConsoleIo.emitPrimitives((byte) 7).unsafeRun()).isFalse();
            assertThat(ConsoleIo.emitByteLists(List.of((byte) 65, (byte) 90)).unsafeRun())
                    .containsExactly((byte) 65, (byte) 90);
        } finally {
            System.setOut(originalOut);
            System.setErr(originalErr);
        }

        assertThat(out.toString(StandardCharsets.UTF_8).replace("\r\n", "\n"))
                .isEqualTo("out line\n7|8|9|1.5|2.5|true\n7\n8\n9\n1.5\n2.5\nfalse\nAZAZ\n");
        assertThat(err.toString(StandardCharsets.UTF_8).replace("\r\n", "\n"))
                .isEqualTo("err line\n7|8|9|1.5|2.5|true\n7\n8\n9\n1.5\n2.5\nfalse\nAZAZ\n");
    }

    @Test
    void generatedConsoleEffectsReadFromStdin() {
        var originalIn = System.in;
        try {
            System.setIn(input("Capy\n"));

            assertThat(ConsoleIo.readOne().unsafeRun()).isEqualTo("Capy");
        } finally {
            System.setIn(originalIn);
        }
    }

    @Test
    void generatedConsoleEffectsReturnNoneAtEndOfInput() {
        var originalIn = System.in;
        try {
            System.setIn(input(""));

            assertThat(ConsoleIo.readEofMarker().unsafeRun()).isEqualTo("EOF");
        } finally {
            System.setIn(originalIn);
        }
    }

    private static InputStream input(String value) {
        return new ByteArrayInputStream(value.getBytes(StandardCharsets.UTF_8));
    }
}
