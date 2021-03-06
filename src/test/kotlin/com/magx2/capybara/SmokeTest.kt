package com.magx2.capybara

import org.junit.jupiter.api.Test
import java.io.File
import java.nio.file.Paths
import java.util.stream.Collectors


class SmokeTest {
    @Test
    fun `should parse everything in one compilation unit`() {
        val loader = Thread.currentThread().contextClassLoader
        val url = loader.getResource("integration_test")!!
        val outputDir = outputDir()
        File(outputDir).mkdirs()
        val files = File(url.path).walkTopDown()
                .filter { it.isFile }
                .filter { it.extension == "cb" }
                .map { it.absolutePath }
                .toList()
                .stream()
                .collect(Collectors.joining(","))
        main(arrayOf(
                "-o",
                outputDir,
                "-f",
                files,
                "--clear-output",
                "--debug",
        ))
    }

    private fun outputDir() =
            Paths.get(System.getProperty("java.io.tmpdir"), "capybara")
                    .toAbsolutePath()
                    .toFile()
                    .absolutePath
}
