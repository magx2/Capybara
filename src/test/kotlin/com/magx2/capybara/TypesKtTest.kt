package com.magx2.capybara

import org.assertj.core.api.Assertions.assertThat
import org.assertj.core.api.Assertions.assertThatThrownBy
import org.assertj.core.api.ThrowableAssert
import org.junit.jupiter.api.Test

internal class TypesKtTest {
    @Test
    fun `should parse fully qualified type`() {
        // given
        val rawType = "/x/y/z/Foo"

        // when
        val type = parseType(rawType)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }

    @Test
    fun `should parse type with default package`() {
        // given
        val defaultPackage = "/x/y/z"
        val rawType = "Foo"

        // when
        val type = parseType(rawType, defaultPackage)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }

    @Test
    fun `should throw exception if there is no package in type and default type is null`() {
        // given
        val defaultPackage = null
        val rawType = "Foo"

        // when
        val `when` = ThrowableAssert.ThrowingCallable { parseType(rawType, defaultPackage) }

        // then
        assertThatThrownBy(`when`)
                .isInstanceOf(CompilationException::class.java)
    }

    @Test
    fun `should parse fully qualified type with generic type`() {
        // given
        val rawType = "/x/y/z/Foo[/a/b/c/Boo]"

        // when
        val type = parseType(rawType)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType?.name).isEqualTo("Boo")
        assertThat(type.genericType?.packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse type with default package with generic type`() {
        // given
        val defaultPackage = "/x/y/z"
        val rawType = "Foo[/a/b/c/Boo]"

        // when
        val type = parseType(rawType, defaultPackage)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType?.name).isEqualTo("Boo")
        assertThat(type.genericType?.packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse type with default package with 2 generic types`() {
        // given
        val defaultPackage = "/x/y/z"
        val rawType = "Foo[/a/b/c/Boo[/q/w/e/Bar]]"

        // when
        val type = parseType(rawType, defaultPackage)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType?.name).isEqualTo("Boo")
        assertThat(type.genericType?.packageName).isEqualTo("/a/b/c")
        assertThat(type.genericType?.genericType?.name).isEqualTo("Bar")
        assertThat(type.genericType?.genericType?.packageName).isEqualTo("/q/w/e")
    }
}