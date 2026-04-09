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
    void newPoint3dWithNamedFields() {
        var result = Extension.newPoint3dVer2(4.0f, 5.0f, 6.0f);

        assertThat(result).isEqualTo(new Extension.Point3D(4.0f, 5.0f, 6.0f));
    }

    @Test
    void lengthUsesMostConcreteOverload() {
        assertThat(Extension.length(new Extension.Point(3.0f, 4.0f))).isEqualTo(5.0f);
        assertThat(Extension.length(new Extension.Point3D(1.0f, 2.0f, 2.0f))).isEqualTo(3.0f);
    }

    @Test
    void acceptsPoint3dWherePointIsRequired() {
        assertThat(Extension.onlyPoint2d()).isEqualTo(3.0f);
        assertThat(Extension.testSubstitution(new Extension.Point3D(3.0f, 4.0f, 100.0f))).isEqualTo(5.0f);
    }

    @Test
    void supportsMultipleDataExtensions() {
        var point = new Extension.WeightedLabeledPoint3D(
                1.0f,
                2.0f,
                3.0f,
                "capy",
                4.0f,
                "v1"
        );

        assertThat(Extension.weightedLabeledPointScore(point)).isEqualTo(10.0f);
        assertThat(Extension.weightedLabeledPointLabel(point)).isEqualTo("capy:v1");
        assertThat(Extension.overloadPrefersMostConcrete(point)).isEqualTo((float) Math.sqrt(14.0));
    }

    @Test
    void createsMultiExtendedDataByNamedFields() {
        var result = Extension.newLabeledPoint(1.0f, 2.0f, "origin");

        assertThat(result).isEqualTo(new Extension.LabeledPoint(1.0f, 2.0f, "origin"));
    }

    @Test
    void prefersConcreteOverAnyWhenCoercionsTie() {
        var value = Extension.overloadPrefersConcreteOverAny(new Extension.Point3D(1.0f, 2.0f, 3.0f));

        assertThat(value).isEqualTo("point");
    }
}
