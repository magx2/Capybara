package com.magx2.capybara

import org.junit.jupiter.api.Test
import java.io.File


class SmokeTest {
    @Test
    fun `should parse everything in one compilation unit`() {
        val loader = Thread.currentThread().contextClassLoader
        val url = loader.getResource("capybara")!!
        val files = File(url.path).walkTopDown()
                .filter { it.isFile }
                .map { it.absolutePath }
                .toList()
        main(files.toTypedArray())
    }
}
