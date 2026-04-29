package dev.capylang;

import capy.io.PathRoot;
import org.junit.jupiter.api.Test;

import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PathUtilTest {

    @Test
    void shouldRoundTripAbsolutePathWithNativeRootPrefix() {
        var nativeRoot = Paths.get("").toAbsolutePath().getRoot();
        var javaPath = nativeRoot.resolve("capybara-pathutil-test").resolve("nested").normalize();

        var capyPath = PathUtil.fromJavaPath(javaPath);

        assertEquals(PathRoot.ABSOLUTE, capyPath.root());
        assertEquals(Optional.of(nativeRoot.toString()), capyPath.prefix());
        assertEquals(List.of("capybara-pathutil-test", "nested"), capyPath.segments());
        assertEquals(javaPath, PathUtil.toJavaPath(capyPath));
    }

    @Test
    void shouldRoundTripHomePathWithoutNativeRootPrefix() {
        var homePath = Paths.get(System.getProperty("user.home")).normalize();
        var javaPath = homePath.resolve("capybara-pathutil-test").resolve("nested").normalize();

        var capyPath = PathUtil.fromJavaPath(javaPath);

        assertEquals(PathRoot.HOME, capyPath.root());
        assertEquals(Optional.empty(), capyPath.prefix());
        assertEquals(List.of("capybara-pathutil-test", "nested"), capyPath.segments());
        assertEquals(javaPath, PathUtil.toJavaPath(capyPath));
    }

    @Test
    void shouldRoundTripRelativePathWithoutNativeRootPrefix() {
        var javaPath = Paths.get("reports", "TEST-capy.lang.MathTest.xml").normalize();

        var capyPath = PathUtil.fromJavaPath(javaPath);

        assertEquals(PathRoot.RELATIVE, capyPath.root());
        assertEquals(Optional.empty(), capyPath.prefix());
        assertEquals(List.of("reports", "TEST-capy.lang.MathTest.xml"), capyPath.segments());
        assertEquals(javaPath, PathUtil.toJavaPath(capyPath));
    }
}
