package dev.capylang.bootstrap;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.zip.ZipFile;

public final class SelfHostingBootstrapRunner {
    private static final Pattern BUILD_DATE_TIME = Pattern.compile("(?m)^build_date_time:.*$");

    private SelfHostingBootstrapRunner() {
    }

    public static void main(String[] args) throws Exception {
        if (args.length != 4) {
            System.err.println("Usage: SelfHostingBootstrapRunner <repo-root> <compiler-source-dir> <invalid-fixture-dir> <stage-output-dir>");
            System.exit(1);
        }

        var repoRoot = Path.of(args[0]).toAbsolutePath().normalize();
        var compilerSourceDir = Path.of(args[1]).toAbsolutePath().normalize();
        var invalidFixtureDir = Path.of(args[2]).toAbsolutePath().normalize();
        var stageOutputDir = Path.of(args[3]).toAbsolutePath().normalize();

        deleteDirectory(stageOutputDir);
        Files.createDirectories(stageOutputDir);

        var runner = new SelfHostingBootstrapRunner();
        runner.runCompilerBootstrap(repoRoot, compilerSourceDir, stageOutputDir);
        runner.runInvalidDiagnostics(repoRoot, invalidFixtureDir, stageOutputDir);
        runner.runCliContract(repoRoot, stageOutputDir);
    }

    private void runCompilerBootstrap(Path repoRoot, Path compilerSourceDir, Path stageOutputDir) throws Exception {
        for (var backend : Backend.values()) {
            var backendDir = stageOutputDir.resolve("bootstrap").resolve(backend.directoryName());
            var generatedDir = backendDir.resolve("generated");
            var linkedDir = backendDir.resolve("linked");
            var args = new java.util.ArrayList<>(List.of(
                    "compile-generate",
                    backend.cliName(),
                    "-i", compilerSourceDir.toString(),
                    "-o", generatedDir.toString(),
                    "--linked-output", linkedDir.toString(),
                    "--log", "WARN"
            ));
            if (backend == Backend.JAVA) {
                args.add("--skip-java-lib");
            }
            runRequired(repoRoot, stageOutputDir, "bootstrap-compile-generate-" + backend.directoryName(), args, 0);
        }
    }

    private void runInvalidDiagnostics(Path repoRoot, Path invalidFixtureDir, Path stageOutputDir) throws Exception {
        var diagnosticsDir = stageOutputDir.resolve("diagnostics");
        runRequired(
                repoRoot,
                stageOutputDir,
                "invalid-compile",
                List.of(
                        "compile",
                        "-i", invalidFixtureDir.toString(),
                        "-o", diagnosticsDir.resolve("invalid-linked").toString(),
                        "--log", "WARN"
                ),
                100
        );
    }

    private void runCliContract(Path repoRoot, Path stageOutputDir) throws Exception {
        var fixtureRoot = stageOutputDir.resolve("cli-contract").resolve("fixtures");
        var observedRoot = stageOutputDir.resolve("cli-contract").resolve("observed");
        var simpleSource = fixtureRoot.resolve("simple-source");
        writeSimpleFixture(simpleSource);

        var compileLinked = observedRoot.resolve("compile").resolve("linked");
        runRequired(repoRoot, stageOutputDir, "cli-compile", List.of(
                "compile",
                "-i", simpleSource.toString(),
                "-o", compileLinked.toString(),
                "--log", "WARN"
        ), 0);

        runRequired(repoRoot, stageOutputDir, "cli-generate-java", List.of(
                "generate",
                "java",
                "-i", compileLinked.toString(),
                "-o", observedRoot.resolve("generate").resolve("java").toString(),
                "--skip-java-lib",
                "--log", "WARN"
        ), 0);

        runRequired(repoRoot, stageOutputDir, "cli-compile-generate-java", List.of(
                "compile-generate",
                "java",
                "-i", simpleSource.toString(),
                "-o", observedRoot.resolve("compile-generate").resolve("java").resolve("generated").toString(),
                "--linked-output", observedRoot.resolve("compile-generate").resolve("java").resolve("linked").toString(),
                "--skip-java-lib",
                "--log", "WARN"
        ), 0);

        var packageModule = fixtureRoot.resolve("package").resolve("capy.yml");
        Files.createDirectories(packageModule.getParent());
        Files.writeString(packageModule, "name: bootstrap-contract\nversion: 1.0.0\n", StandardCharsets.UTF_8);
        runRequired(repoRoot, stageOutputDir, "cli-package", List.of(
                "package",
                "--compiled-input", compileLinked.toString(),
                "--module", packageModule.toString(),
                "--capy.description", "self-hosted bootstrap contract",
                "--log", "WARN"
        ), 0);
        writePackageSummary(packageModule.resolveSibling("capy.cbin"), observedRoot.resolve("package").resolve("summary.txt"));

        var nativeSource = fixtureRoot.resolve("native-source");
        writeNativeFixture(nativeSource);
        for (var backend : Backend.values()) {
            var nativeBackend = observedRoot.resolve("native").resolve(backend.directoryName());
            var args = new java.util.ArrayList<>(List.of(
                    "compile-generate",
                    backend.cliName(),
                    "-i", nativeSource.toString(),
                    "-o", nativeBackend.resolve("generated").toString(),
                    "--linked-output", nativeBackend.resolve("linked").toString(),
                    "--log", "WARN"
            ));
            if (backend == Backend.JAVA) {
                args.add("--skip-java-lib");
            }
            runRequired(repoRoot, stageOutputDir, "native-compile-generate-" + backend.directoryName(), args, 0);
            runRequired(repoRoot, stageOutputDir, "native-generate-" + backend.directoryName(), List.of(
                    "generate",
                    backend.cliName(),
                    "-i", nativeBackend.resolve("linked").toString(),
                    "-o", nativeBackend.resolve("regenerated").toString(),
                    "--log", "WARN"
            ), 0);
        }
    }

    private void writeSimpleFixture(Path sourceDir) throws IOException {
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), """
                fun answer(): int = 42
                """, StandardCharsets.UTF_8);
    }

    private void writeNativeFixture(Path sourceDir) throws IOException {
        var testDir = sourceDir.resolve("dev").resolve("capylang").resolve("test");
        var fixtureRoot = sourceDir.getParent();
        var javaNativeDir = fixtureRoot.resolve("java").resolve("dev").resolve("capylang").resolve("test").resolve("nativeinterop");
        var jsNativeDir = fixtureRoot.resolve("js").resolve("dev").resolve("capylang").resolve("test").resolve("nativeinterop");
        var pyNativeDir = fixtureRoot.resolve("py").resolve("dev").resolve("capylang").resolve("test").resolve("nativeinterop");
        Files.createDirectories(testDir);
        Files.createDirectories(javaNativeDir);
        Files.createDirectories(jsNativeDir);
        Files.createDirectories(pyNativeDir);
        Files.createDirectories(sourceDir.resolve("foo"));
        Files.writeString(sourceDir.resolve("foo").resolve("Main.cfun"), "fun answer(): int = 9\n", StandardCharsets.UTF_8);
        Files.writeString(testDir.resolve("Clock.coo"), """
                interface Clock {
                    def now(): String
                }
                """, StandardCharsets.UTF_8);
        Files.writeString(testDir.resolve("ClockProvider.cfun"), """
                from /capy/lang/Effect import { Effect }
                from /capy/meta_prog/NativeProvider import { NativeProvider }
                from Clock import { Clock }

                @NativeProvider(qualifier: "system")
                fun system_clock(): Effect[Clock] = <native>
                """, StandardCharsets.UTF_8);
        Files.writeString(javaNativeDir.resolve("SystemClock.java"), """
                package dev.capylang.test.nativeinterop;

                import dev.capylang.NativeImplementation;
                import dev.capylang.test.Clock;

                @NativeImplementation(qualifier = "system")
                public final class SystemClock implements Clock {
                    @Override
                    public String now() {
                        return "now";
                    }
                }
                """, StandardCharsets.UTF_8);
        Files.writeString(jsNativeDir.resolve("SystemClock.js"), """
                'use strict';

                const { Clock } = require('../Clock.js');

                @NativeImplementation("system")
                class SystemClock extends Clock {
                    now() {
                        return 'now';
                    }
                }

                module.exports = { SystemClock };
                """, StandardCharsets.UTF_8);
        Files.writeString(pyNativeDir.resolve("SystemClock.py"), """
                from dev.capylang.capybara import NativeImplementation
                from dev.capylang.test.Clock import Clock

                @NativeImplementation(qualifier="system")
                class SystemClock(Clock):
                    def now(self):
                        return "now"
                """, StandardCharsets.UTF_8);
    }

    private void runRequired(
            Path repoRoot,
            Path stageOutputDir,
            String name,
            List<String> args,
            int expectedExitCode
    ) throws Exception {
        var result = runCli(args);
        writeCommandResult(repoRoot, stageOutputDir, name, args, result);
        if (result.exitCode() != expectedExitCode) {
            throw new IllegalStateException("Command `" + name + "` exited with " + result.exitCode()
                    + ", expected " + expectedExitCode + "." + System.lineSeparator() + result.stderr());
        }
    }

    private CommandResult runCli(List<String> args) throws Exception {
        var out = new ByteArrayOutputStream();
        var err = new ByteArrayOutputStream();
        var originalOut = System.out;
        var originalErr = System.err;
        try {
            System.setOut(new PrintStream(out, true, StandardCharsets.UTF_8));
            System.setErr(new PrintStream(err, true, StandardCharsets.UTF_8));
            var capyClass = Class.forName("dev.capylang.Capy");
            var execute = capyClass.getMethod("execute", List.class);
            var effect = execute.invoke(null, args);
            var program = effect.getClass().getMethod("unsafeRun").invoke(effect);
            return new CommandResult(exitCode(program), out.toString(StandardCharsets.UTF_8), err.toString(StandardCharsets.UTF_8));
        } catch (InvocationTargetException e) {
            var cause = e.getCause();
            throw (cause instanceof Exception exception ? exception : e);
        } finally {
            System.setOut(originalOut);
            System.setErr(originalErr);
        }
    }

    private int exitCode(Object program) throws ReflectiveOperationException {
        if (!Objects.equals(program.getClass().getSimpleName(), "Failed")) {
            return 0;
        }
        var value = program.getClass().getMethod("exit_code").invoke(program);
        return value instanceof Number number ? number.intValue() : Integer.parseInt(String.valueOf(value));
    }

    private void writeCommandResult(
            Path repoRoot,
            Path stageOutputDir,
            String name,
            List<String> args,
            CommandResult result
    ) throws IOException {
        var commandDir = stageOutputDir.resolve("cli-contract").resolve("observed").resolve("commands");
        if (name.startsWith("bootstrap-") || name.startsWith("invalid-")) {
            commandDir = stageOutputDir.resolve(name.startsWith("invalid-") ? "diagnostics" : "bootstrap").resolve("commands");
        }
        Files.createDirectories(commandDir);
        var normalizedArgs = args.stream()
                .map(arg -> normalizeText(repoRoot, stageOutputDir, arg))
                .map(SelfHostingBootstrapRunner::jsonString)
                .toList();
        var json = "{\n"
                + "  \"args\": [" + String.join(", ", normalizedArgs) + "],\n"
                + "  \"exitCode\": " + result.exitCode() + ",\n"
                + "  \"stdout\": " + jsonString(normalizeText(repoRoot, stageOutputDir, result.stdout())) + ",\n"
                + "  \"stderr\": " + jsonString(normalizeText(repoRoot, stageOutputDir, result.stderr())) + "\n"
                + "}\n";
        Files.writeString(commandDir.resolve(name + ".json"), json, StandardCharsets.UTF_8);
    }

    private void writePackageSummary(Path packageFile, Path summaryFile) throws IOException {
        Files.createDirectories(summaryFile.getParent());
        var lines = new java.util.ArrayList<String>();
        try (var zip = new ZipFile(packageFile.toFile(), StandardCharsets.UTF_8)) {
            zip.stream()
                    .sorted(Comparator.comparing(java.util.zip.ZipEntry::getName))
                    .forEach(entry -> {
                        lines.add(entry.getName());
                        if ("capy.yml".equals(entry.getName())) {
                            try (var input = zip.getInputStream(entry)) {
                                var content = new String(input.readAllBytes(), StandardCharsets.UTF_8);
                                lines.add(BUILD_DATE_TIME.matcher(content).replaceAll("build_date_time: <normalized>"));
                            } catch (IOException e) {
                                throw new java.io.UncheckedIOException(e);
                            }
                        }
                    });
        }
        Files.writeString(summaryFile, String.join(System.lineSeparator(), lines) + System.lineSeparator(), StandardCharsets.UTF_8);
    }

    private static String normalizeText(Path repoRoot, Path stageOutputDir, String value) {
        return value
                .replace(stageOutputDir.toString(), "<stage>")
                .replace(repoRoot.toString(), "<repo>")
                .replace("\\", "/")
                .replace("\r\n", "\n");
    }

    private static String jsonString(String value) {
        var escaped = value
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
        return "\"" + escaped + "\"";
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (Files.notExists(directory)) {
            return;
        }
        try (var stream = Files.walk(directory)) {
            for (var path : stream.sorted(Comparator.reverseOrder()).toList()) {
                Files.delete(path);
            }
        }
    }

    private enum Backend {
        JAVA("java", "java"),
        JAVASCRIPT("javascript", "js"),
        PYTHON("python", "python");

        private final String cliName;
        private final String directoryName;

        Backend(String cliName, String directoryName) {
            this.cliName = cliName;
            this.directoryName = directoryName;
        }

        String cliName() {
            return cliName;
        }

        String directoryName() {
            return directoryName;
        }
    }

    private record CommandResult(int exitCode, String stdout, String stderr) {
    }
}
