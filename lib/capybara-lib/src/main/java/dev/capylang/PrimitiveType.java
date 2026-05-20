package dev.capylang;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a Java type use or declaration as representing a Capybara primitive-backed type.
 *
 * <p>The annotated Java type is the erased backing type; {@link #cfunType()} preserves the
 * fully qualified Capybara type name used by reflection and interop metadata.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({
        ElementType.TYPE_USE,
        ElementType.METHOD,
        ElementType.PARAMETER,
        ElementType.FIELD
})
public @interface PrimitiveType {
    /**
     * Fully qualified Capybara primitive-backed type name, for example {@code /capy/lang/String.char}.
     */
    String cfunType();
}
