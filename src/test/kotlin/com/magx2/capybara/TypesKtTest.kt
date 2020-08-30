package com.magx2.capybara

import org.assertj.core.api.Assertions.assertThat
import org.assertj.core.api.Assertions.assertThatThrownBy
import org.assertj.core.api.ThrowableAssert
import org.junit.jupiter.api.Test

internal class TypesKtTest {
    private val localStructs = setOf<BaseStruct>()
    private val importedStructs = listOf<BaseStruct>()

    @Test
    fun `should parse fully qualified type`() {
        // given
        val rawType = "/x/y/z/Foo"

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }

    @Test
    fun `should parse type with default package`() {
        // given
        val rawType = "Foo"
        val packageName = "/x/y/z"
        val localStructs = setOf(FlatStruct(packageName, rawType, listOf()))

        // when
        val type = parseType(CodeMetainfo(
                "/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
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
                    localStructs,
                    importedStructs)
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
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType?.name).isEqualTo("Boo")
        assertThat(type.genericType?.packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse type with default package with generic type`() {
        // given
        val rawType = "Foo[/a/b/c/Boo]"
        val packageName = "/x/y/z"
        val localStructs = setOf(FlatStruct(packageName, "Foo", listOf()))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType?.name).isEqualTo("Boo")
        assertThat(type.genericType?.packageName).isEqualTo("/a/b/c")
    }

    @Test
    fun `should parse type with default package with 2 generic types`() {
        // given
        val rawType = "Foo[/a/b/c/Boo[/q/w/e/Bar]]"
        val packageName = "/x/y/z"
        val localStructs = setOf(FlatStruct(packageName, "Foo", listOf()))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType?.name).isEqualTo("Boo")
        assertThat(type.genericType?.packageName).isEqualTo("/a/b/c")
        assertThat(type.genericType?.genericType?.name).isEqualTo("Bar")
        assertThat(type.genericType?.genericType?.packageName).isEqualTo("/q/w/e")
    }

    @Test
    fun `should parse type from localStructs`() {
        // given
        val packageName = "/x/y/z"
        val rawType = "Foo"
        val localStructs = setOf(FlatStruct(packageName, rawType, listOf()))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }

    @Test
    fun `should parse type from importedStructsWithImports - structs`() {
        // given
        val packageName = "/x/y/z"
        val rawType = "Foo"
        val importedStructs = listOf(FlatStruct(packageName, rawType, listOf()))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }


    @Test
    fun `should parse type from importedStructsWithImports - structs even if localStructs has same type`() {
        // given
        val packageName1 = "/x/y/z"
        val packageName2 = "/a/b/c"
        val rawType = "Foo"
        val importedStructs = listOf(FlatStruct(packageName1, rawType, listOf()))
        val localStructs = setOf(FlatStruct(packageName2, rawType, listOf()))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }

    @Test
    fun `should parse type from importedStructsWithImports - importStructs even if localStructs has same type`() {
        // given
        val packageName1 = "/x/y/z"
        val packageName2 = "/a/b/c"
        val rawType = "Foo"
        val localStructs = setOf(FlatStruct(packageName2, rawType, listOf()))
        val importedStructs = listOf(FlatStruct(packageName1, rawType, listOf()))

        // when
        val type = parseType(
                CodeMetainfo("/home/capybara/xyz.cb", 1, 2),
                rawType,
                localStructs,
                importedStructs)

        // then
        assertThat(type.name).isEqualTo("Foo")
        assertThat(type.packageName).isEqualTo("/x/y/z")
        assertThat(type.genericType).isNull()
    }
}
