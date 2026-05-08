package dev.capylang.test;

import capy.io.PathModule;
import capy.lang.Result;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class FileIoTest {
    @TempDir
    Path tempDir;

    @Test
    void generatedFileIoEffectsReadAndWriteTextLinesAndBytes() {
        var textFile = capyPath(tempDir.resolve("text.txt"));
        var linesFile = capyPath(tempDir.resolve("lines.txt"));
        var bytesFile = capyPath(tempDir.resolve("bytes.bin"));

        assertThat(successValue(FileIo.writeTextFile(textFile, "hello").unsafeRun())).isEqualTo("hello");
        assertThat(successValue(FileIo.appendTextFile(textFile, " world").unsafeRun())).isEqualTo(" world");
        assertThat(successValue(FileIo.readTextFile(textFile).unsafeRun())).isEqualTo("hello world");

        assertThat(successValue(FileIo.writeLinesFile(linesFile, List.of("alpha")).unsafeRun())).containsExactly("alpha");
        assertThat(successValue(FileIo.appendLinesFile(linesFile, List.of("beta")).unsafeRun())).containsExactly("beta");
        assertThat(successValue(FileIo.readLinesFile(linesFile).unsafeRun())).containsExactly("alpha", "beta");

        assertThat(successValue(FileIo.writeBytesFile(bytesFile, List.of((byte) 65, (byte) 66)).unsafeRun())).containsExactly((byte) 65, (byte) 66);
        assertThat(successValue(FileIo.appendBytesFile(bytesFile, List.of((byte) 67)).unsafeRun())).containsExactly((byte) 67);
        assertThat(successValue(FileIo.readBytesFile(bytesFile).unsafeRun())).containsExactly((byte) 65, (byte) 66, (byte) 67);
        assertThat(successValue(FileIo.fileSize(bytesFile).unsafeRun())).isEqualTo(3L);

        assertThat(FileIo.existsPath(bytesFile).unsafeRun()).isTrue();
        assertThat(FileIo.isFilePath(bytesFile).unsafeRun()).isTrue();
        assertThat(FileIo.isDirectoryPath(bytesFile).unsafeRun()).isFalse();
    }

    @Test
    void generatedFileIoEffectsManageDirectoriesAndDeleteFiles() {
        var dir = capyPath(tempDir.resolve("nested").resolve("dir"));
        var child = capyPath(tempDir.resolve("nested").resolve("dir").resolve("child.txt"));
        var deleteTarget = capyPath(tempDir.resolve("delete-me.txt"));

        assertThat(successValue(FileIo.createDirectoriesAt(dir).unsafeRun())).isNotNull();
        assertThat(FileIo.isDirectoryPath(dir).unsafeRun()).isTrue();
        assertThat(successValue(FileIo.writeTextFile(child, "x").unsafeRun())).isEqualTo("x");
        assertThat(successValue(FileIo.listDirectory(dir).unsafeRun())).hasSize(1);

        assertThat(successValue(FileIo.createFileAt(deleteTarget).unsafeRun())).isNotNull();
        assertThat(FileIo.isFilePath(deleteTarget).unsafeRun()).isTrue();
        assertThat(successValue(FileIo.deletePath(deleteTarget).unsafeRun())).isTrue();
        assertThat(FileIo.existsPath(deleteTarget).unsafeRun()).isFalse();
    }

    @Test
    void generatedFileIoEffectsCopyAndMoveFiles() {
        var source = capyPath(tempDir.resolve("source.txt"));
        var copied = capyPath(tempDir.resolve("copied.txt"));
        var moved = capyPath(tempDir.resolve("moved.txt"));

        assertThat(successValue(FileIo.writeTextFile(source, "payload").unsafeRun())).isEqualTo("payload");
        assertThat(successValue(FileIo.copyPath(source, copied).unsafeRun())).isNotNull();
        assertThat(successValue(FileIo.readTextFile(copied).unsafeRun())).isEqualTo("payload");
        assertThat(successValue(FileIo.movePath(copied, moved).unsafeRun())).isNotNull();

        assertThat(FileIo.existsPath(copied).unsafeRun()).isFalse();
        assertThat(successValue(FileIo.readTextFile(moved).unsafeRun())).isEqualTo("payload");
    }

    @Test
    void generatedFileIoEffectsReplaceExistingCopyAndMoveTargets() {
        var source = capyPath(tempDir.resolve("replace-source.txt"));
        var copied = capyPath(tempDir.resolve("replace-copied.txt"));
        var moved = capyPath(tempDir.resolve("replace-moved.txt"));

        assertThat(successValue(FileIo.writeTextFile(source, "new").unsafeRun())).isEqualTo("new");
        assertThat(successValue(FileIo.writeTextFile(copied, "old").unsafeRun())).isEqualTo("old");
        assertThat(successValue(FileIo.copyReplacePath(source, copied).unsafeRun())).isNotNull();
        assertThat(successValue(FileIo.writeTextFile(moved, "old").unsafeRun())).isEqualTo("old");
        assertThat(successValue(FileIo.moveReplacePath(copied, moved).unsafeRun())).isNotNull();

        assertThat(successValue(FileIo.readTextFile(moved).unsafeRun())).isEqualTo("new");
    }

    @Test
    void generatedFileIoEffectsReturnErrorsForFailedOperations() {
        var missing = capyPath(tempDir.resolve("missing.txt"));

        assertError(FileIo.readTextFile(missing).unsafeRun()).contains("read_text failed");
        assertError(FileIo.fileSize(missing).unsafeRun()).contains("size failed");
        assertThat(FileIo.existsPath(missing).unsafeRun()).isFalse();
    }

    private static capy.io.Path capyPath(Path path) {
        return PathModule.fromString(path.toString());
    }

    private static <T> T successValue(Result<T> result) {
        assertThat(result).isInstanceOf(Result.Success.class);
        return ((Result.Success<T>) result).value();
    }

    private static org.assertj.core.api.AbstractStringAssert<?> assertError(Result<?> result) {
        assertThat(result).isInstanceOf(Result.Error.class);
        return assertThat(((Result.Error<?>) result).ex().getMessage());
    }
}
