package pl.grzeslowski.capybara.test;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;

import static org.assertj.core.api.Assertions.assertThat;

class FunctionTypeInferenceTest {
    @Test
    void simpleNoParamIn() {
        assertThat(FunctionTypeInference.simpleNoParamInt()).isExactlyInstanceOf(Integer.class);
    }

    @Test
    void simple() {
        assertThat(FunctionTypeInference.simple(1)).isExactlyInstanceOf(Integer.class);
    }

    @Test
    void infixOperator() {
        assertThat(FunctionTypeInference.infixOperator(1, 2)).isExactlyInstanceOf(Integer.class);
    }

    @Test
    void ifOperator() {
        assertThat(FunctionTypeInference.ifOperator(1, 2)).isExactlyInstanceOf(String.class);
    }

    @Test
    void typeSum() {
        assertThat(FunctionTypeInference.typeSum(1)).isExactlyInstanceOf(String.class);
    }

    @Test
    void listOfInts() throws NoSuchMethodException {
        Method method = FunctionTypeInference.class.getMethod("listOfInts", int.class);
        assertThat(method.getGenericReturnType()).isInstanceOf(ParameterizedType.class);
        var generic = (ParameterizedType) method.getGenericReturnType();
        assertThat(generic.getRawType().getTypeName()).isEqualTo("java.util.List");
        assertThat(generic.getActualTypeArguments()[0].getTypeName()).isEqualTo("java.lang.Integer");
    }

    @Test
    void emptyList() throws NoSuchMethodException {
        Method method = FunctionTypeInference.class.getMethod("emptyList");
        assertThat(method.getGenericReturnType()).isInstanceOf(ParameterizedType.class);
        var generic = (ParameterizedType) method.getGenericReturnType();
        assertThat(generic.getRawType().getTypeName()).isEqualTo("java.util.List");
        assertThat(generic.getActualTypeArguments()[0].getTypeName()).isEqualTo("java.lang.Object");
    }

    @Test
    void setOfInts() throws NoSuchMethodException {
        Method method = FunctionTypeInference.class.getMethod("setOfInts", int.class);
        assertThat(method.getGenericReturnType()).isInstanceOf(ParameterizedType.class);
        var generic = (ParameterizedType) method.getGenericReturnType();
        assertThat(generic.getRawType().getTypeName()).isEqualTo("java.util.Set");
        assertThat(generic.getActualTypeArguments()[0].getTypeName()).isEqualTo("java.lang.Integer");
    }

    @Test
    void emptySet() throws NoSuchMethodException {
        Method method = FunctionTypeInference.class.getMethod("emptySet");
        assertThat(method.getGenericReturnType()).isInstanceOf(ParameterizedType.class);
        var generic = (ParameterizedType) method.getGenericReturnType();
        assertThat(generic.getRawType().getTypeName()).isEqualTo("java.util.Set");
        assertThat(generic.getActualTypeArguments()[0].getTypeName()).isEqualTo("java.lang.Object");
    }

    @Test
    void dictOfInts() throws NoSuchMethodException {
        Method method = FunctionTypeInference.class.getMethod("dictOfInts");
        assertThat(method.getGenericReturnType()).isInstanceOf(ParameterizedType.class);
        var generic = (ParameterizedType) method.getGenericReturnType();
        assertThat(generic.getRawType().getTypeName()).isEqualTo("java.util.Map");
        assertThat(generic.getActualTypeArguments()[0].getTypeName()).isEqualTo("java.lang.String");
        assertThat(generic.getActualTypeArguments()[1].getTypeName()).isEqualTo("java.lang.Integer");
    }
}
