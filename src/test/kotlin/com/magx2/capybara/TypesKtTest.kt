package com.magx2.capybara

import org.assertj.core.api.Assertions.assertThat
import org.assertj.core.api.Assertions.assertThatThrownBy
import org.assertj.core.api.ThrowableAssert
import org.junit.jupiter.api.Test

internal class TypesKtTest {
    private val types = setOf<Type>()

    @Test
    fun `should parse fully qualified type`() {
        // given
        val rawType = "/x/y/z/Foo"

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).isEmpty()
    }

    @Test
    fun `should parse type with default package`() {
        // given
        val rawType = "Foo"
        val packageName = "/x/y/z"
        val types = setOf(Type(packageName, rawType))

        // when
        val type = parseType(CodeMetainfo(
                "/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).isEmpty()
    }

    @Test
    fun `should throw exception if there is no package in type and default type is null`() {
        // given
        val rawType = "Foo"

        // when
        val `when` = ThrowableAssert.ThrowingCallable {
            parseType(
                    CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                    rawType,
                    types)
        }

        // then
        assertThatThrownBy(`when`)
                .isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should parse fully qualified type with generic type`() {
        // given
        val rawType = "/x/y/z/Foo[/a/b/c/Boo]"

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).hasSize(1)
        assertThat(type.genericTypes[0].name).isEqualTo("Boo")
        assertThat(type.genericTypes[0].packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse 3 generic type`() {
        // given
        val rawType = "/x/y/z/Foo[/a/b/c/Boo, /a/b/c/Bar, /a/b/c/Baz]"

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).hasSize(3)
        assertThat(type.genericTypes[0].name).isEqualTo("Boo")
        assertThat(type.genericTypes[0].packageName).isEqualTo("/a/b/c")
        assertThat(type.genericTypes[1].name).isEqualTo("Bar")
        assertThat(type.genericTypes[1].packageName).isEqualTo("/a/b/c")
        assertThat(type.genericTypes[2].name).isEqualTo("Baz")
        assertThat(type.genericTypes[2].packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse type with default package with generic type`() {
        // given
        val rawType = "Foo[/a/b/c/Boo]"
        val packageName = "/x/y/z"
        val types = setOf(Type(packageName, "Foo"))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).hasSize(1)
        assertThat(type.genericTypes[0].name).isEqualTo("Boo")
        assertThat(type.genericTypes[0].packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse type with default package with 2 generic types`() {
        // given
        val rawType = "Foo[/a/b/c/Boo[/q/w/e/Bar]]"
        val packageName = "/x/y/z"
        val types = setOf(Type(packageName, "Foo"))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).hasSize(1)
        assertThat(type.genericTypes[0].name).isEqualTo("Boo")
        assertThat(type.genericTypes[0].packageName).isEqualTo("/a/b/c")
        assertThat(type.genericTypes[0].genericTypes).hasSize(1)
        assertThat(type.genericTypes[0].genericTypes[0].name).isEqualTo("Bar")
        assertThat(type.genericTypes[0].genericTypes).hasSize(1)
        assertThat(type.genericTypes[0].genericTypes[0].packageName).isEqualTo("/q/w/e")
    }

    @Test
    fun `should parse type from localStructs`() {
        // given
        val packageName = "/x/y/z"
        val rawType = "Foo"
        val types = setOf(Type(packageName, rawType))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).isEmpty()
    }

    @Test
    fun `should parse type from importedStructsWithImports - structs`() {
        // given
        val packageName = "/x/y/z"
        val rawType = "Foo"
        val types = setOf(Type(packageName, rawType))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).isEmpty()
    }


    @Test
    fun `should parse type from importedStructsWithImports - structs even if localStructs has same type`() {
        // given
        val packageName1 = "/x/y/z"
        val packageName2 = "/a/b/c"
        val rawType = "Foo"
        val types = setOf(Type(packageName1, rawType), Type(packageName2, rawType))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).isEmpty()
    }

    @Test
    fun `should parse type from importedStructsWithImports - importStructs even if localStructs has same type`() {
        // given
        val packageName1 = "/x/y/z"
        val packageName2 = "/a/b/c"
        val rawType = "Foo"
        val types = setOf(Type(packageName1, rawType), Type(packageName2, rawType))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                types)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericTypes).isEmpty()
    }
}
