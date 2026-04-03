package dev.capylang.test;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ExtensionTest {
    @Test
    void newPoint3dFromScalars() {
        var result = Extension.newPoint3d(1.0f, 2.0f, 3.0f);

        assertThat(result.x()).isEqualTo(1.0f);
        assertThat(result.y()).isEqualTo(2.0f);
        assertThat(result.z()).isEqualTo(3.0f);
    }

    @Test
    void newPoint3dFromPointSpread() {
        var point = new Extension.Point(1.0f, 2.0f);

        var result = Extension.newPoint3d(point, 3.0f);

        assertThat(result).isEqualTo(new Extension.Point3D(1.0f, 2.0f, 3.0f));
    }

    @Test
    void lengthUsesFieldAccess() {
        assertThat(Extension.length(new Extension.Point(3.0f, 4.0f))).isEqualTo(5.0f);
        assertThat(Extension.length(new Extension.Point3D(1.0f, 2.0f, 2.0f))).isEqualTo(3.0f);
    }

    @Test
    void acceptsPoint3dWherePointIsRequired() {
        assertThat(Extension.onlyPoint2d(10, 20)).isEqualTo(3.0f);
    }
}
