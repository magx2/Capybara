package dev.capylang.test;

import org.junit.jupiter.api.Test;

import java.lang.reflect.ParameterizedType;
import java.util.List;
import java.util.function.Supplier;

import static org.assertj.core.api.Assertions.assertThat;

class LambdaValuesTest {
    @Test
    void returnsStandaloneSupplier() {
        assertThat(LambdaValues.makeSupplier().get()).isEqualTo("hello");
    }

    @Test
    void invokesStandaloneSupplier() {
        assertThat(LambdaValues.invokeSupplier()).isEqualTo("hello");
    }

    @Test
    void invokesSupplierStoredInDataField() {
        assertThat(LambdaValues.invokeFieldSupplier()).isEqualTo("hello");
    }

    @Test
    void invokesPartiallyBoundFunction() {
        assertThat(LambdaValues.add10().apply(5)).isEqualTo(15);
        assertThat(LambdaValues.invokePartialAdd()).isEqualTo(15);
    }

    @Test
    void invokesPartiallyBoundFunctionWithMultiplePlaceholders() {
        assertThat(LambdaValues.invokePartialConcat()).isEqualTo("A-B");
        assertThat(LambdaValues.invokePartialCombine()).isEqualTo(159);
    }

    @Test
    void treatsEachPlaceholderAsSeparateParameter() {
        assertThat(LambdaValues.invokePartialPair()).isEqualTo("1:2");
    }

    @Test
    void invokesPartiallyBoundFunctionWithFixedOuterArguments() {
        var isAgeValid = LambdaValues.isAgeValid();
        assertThat(isAgeValid.apply(42)).isTrue();
        assertThat(isAgeValid.apply(-1)).isFalse();
        assertThat(isAgeValid.apply(200)).isFalse();
    }

    @Test
    void generatesSupplierReturnType() throws NoSuchMethodException {
        var returnType = (ParameterizedType) LambdaValues.class.getMethod("makeSupplier").getGenericReturnType();
        assertThat(returnType.getRawType().getTypeName()).isEqualTo(Supplier.class.getTypeName());
        assertThat(returnType.getActualTypeArguments()[0].getTypeName()).isEqualTo(String.class.getTypeName());
    }

    @Test
    void generatesSupplierFieldTypes() throws NoSuchMethodException {
        var supplierType = (ParameterizedType) LambdaValues.Holder.class.getMethod("supplier").getGenericReturnType();
        assertThat(supplierType.getRawType().getTypeName()).isEqualTo(Supplier.class.getTypeName());
        assertThat(supplierType.getActualTypeArguments()[0].getTypeName()).isEqualTo(String.class.getTypeName());

        var suppliersType = (ParameterizedType) LambdaValues.Holder.class.getMethod("suppliers").getGenericReturnType();
        assertThat(suppliersType.getRawType().getTypeName()).isEqualTo(List.class.getTypeName());
        assertThat(suppliersType.getActualTypeArguments()[0]).isInstanceOf(ParameterizedType.class);
        var elementType = (ParameterizedType) suppliersType.getActualTypeArguments()[0];
        assertThat(elementType.getRawType().getTypeName()).isEqualTo(Supplier.class.getTypeName());
        assertThat(elementType.getActualTypeArguments()[0].getTypeName()).isEqualTo(String.class.getTypeName());
    }
}
