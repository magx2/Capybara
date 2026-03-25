package dev.capylang.test;

import org.junit.jupiter.api.Test;
import dev.capylang.test.localVisibility.PackageLocalSupport;
import dev.capylang.test.localVisibility.PackagePeer;
import dev.capylang.test.localVisibility.child.PackageChild;

import static org.assertj.core.api.Assertions.assertThat;

class LocalVisibilityTest {
    @Test
    void samePackageModuleCanUseItsOwnLocalMembers() {
        assertThat(PackageLocalSupport.ownPackageValue(1)).isEqualTo(6);
        assertThat(PackageLocalSupport.ownPackageValue(3)).isEqualTo(8);
    }

    @Test
    void peerModuleCanUseLocalMembersFromSamePackage() {
        assertThat(PackagePeer.peerCanUseLocalMembers(1)).isEqualTo(6);
        assertThat(PackagePeer.peerCanUseLocalMembers(3)).isEqualTo(8);
    }

    @Test
    void childPackageCanUseLocalMembersFromParentPackage() {
        assertThat(PackageChild.childCanUseLocalMembers(1)).isEqualTo(6);
        assertThat(PackageChild.childCanUseLocalMembers(3)).isEqualTo(8);
    }
}
