package com.magx2.capybara

import org.junit.jupiter.api.Test
import java.io.File
import java.util.*


class SmokeTest {
    @Test
    fun `should parse everything in one compilation unit`() {
        val loader = Thread.currentThread().contextClassLoader
        val url = loader.getResource("capybara")!!
        val outputDir = outputDir()
        outputDir.map { File(it) }.forEach { it.mkdirs() }
        val args = outputDir + File(url.path).walkTopDown()
                .filter { it.isFile }
                .map { it.absolutePath }
                .toList()
        main(args.toTypedArray())
    }

    private fun outputDir() = listOf(System.getProperty("java.io.tmpdir") + "capybara-" + UUID.randomUUID().toString())
}
