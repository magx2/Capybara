package com.magx2.capybara

import org.apache.commons.cli.*
import org.slf4j.LoggerFactory
import java.io.File
import java.util.stream.Stream
import kotlin.streams.asStream
import kotlin.streams.toList


fun buildOptions(): Options {
    val options = Options()
    options.addOption("o", "output", true, "Output directory where compiled files should be placed")
    options.addOption("f", "files", true, "Coma separated Capybara files to compile (either files or dirs)")
    options.addOption("da", "disable_asserts", false, "Should assertions be disabled")
    options.addOption("h", "help", false, "Help")
    options.addOption(
            Option.builder()
                    .longOpt("debug")
                    .hasArg(false)
                    .desc("Turns on debugging messages")
                    .build())
    options.addOption(
            Option.builder()
                    .longOpt("clear-output")
                    .hasArg(false)
                    .desc("Clears output directory before compiling files")
                    .build())
//    options.addOption("", "", true, "")
    return options
}

fun parseCommandLine(args: Array<String>): CommandLineOptions {
    val parser: CommandLineParser = DefaultParser()
    val parse = parser.parse(buildOptions(), args)
    return CommandLineOptions(
            parse.hasOption("h"),
            findOutputDir(parse),
            parseFilesToCompile(parse.getOptionValue("f")),
            parse.hasOption("da"),
            parse.hasOption("clear-output"),
            parse.hasOption("debug"))
}

private fun findOutputDir(parse: CommandLine): String? {
    val outputDir = parse.getOptionValue("o")
    if (outputDir != null) {
        File(outputDir).also {
            log.info("Output dir `${it.absolutePath}`")
            if (it.exists().not()) {
                throw IllegalArgumentException("Output dir `$outputDir` does not exists")
            }
            if (it.isDirectory.not()) {
                throw IllegalArgumentException("Output dir `$outputDir` is not a directory")
            }
            if (it.canWrite().not()) {
                throw IllegalArgumentException("Output dir `$outputDir` is not writable")
            }
            if ((it.list() ?: emptyArray<String>()).isNotEmpty()) {
                log.warn("Output dir `$outputDir` is not empty")
            }
        }
    }
    return outputDir
}

@Suppress("IfThenToElvis")
fun parseFilesToCompile(files: String?): Set<String> =
        if (files != null) {
            files.split(",")
                    .stream()
                    .map { File(it) }
                    .flatMap { file ->
                        if (file.isDirectory) {
                            file.walkTopDown()
                                    .filter { it.isFile }
                                    .filter { it.extension == "cb" }
                                    .asStream()
                        } else {
                            Stream.of(file)
                        }
                    }
                    .map { it.absolutePath }
                    .distinct()
                    .toList()
                    .toSet()
        } else {
            setOf()
        }

private val log = LoggerFactory.getLogger(CommandLineOptions::class.java)

data class CommandLineOptions(
        val help: Boolean,
        val outputDir: String?,
        val filesToCompile: Set<String>,
        val disableAssertions: Boolean,
        val clearOutput: Boolean,
        val debug: Boolean
)