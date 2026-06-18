package dev.capylang.compiler;

import dev.capylang.compiler.CollectionLinkedType.CompiledList;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class CompiledTypeSignatureTest {
    @Test
    void shouldUseShallowKeysForRecursiveNamedTypes() {
        var fields = new ArrayList<CompiledDataType.CompiledField>();
        var tree = new CompiledDataParentType("Tree", fields, List.of(), List.of());
        fields.add(new CompiledDataType.CompiledField("children", new CompiledList(tree)));

        var treeKey = CompiledTypeSignature.typeKey(tree);
        var listTreeKey = CompiledTypeSignature.typeKey(new CompiledList(tree));

        assertThat(treeKey).isEqualTo("Tree");
        assertThat(listTreeKey).isEqualTo("CompiledList[elementType=Tree]");
        assertThat(treeKey).isNotEqualTo(listTreeKey);
        assertThat(treeKey).doesNotContain("children");
        assertThat(listTreeKey).doesNotContain("children");
    }

    @Test
    void shouldKeepCollectionElementTypesDistinct() {
        assertThat(CompiledTypeSignature.typeKey(new CompiledList(PrimitiveLinkedType.INT)))
                .isNotEqualTo(CompiledTypeSignature.typeKey(new CompiledList(PrimitiveLinkedType.LONG)));
    }
}
