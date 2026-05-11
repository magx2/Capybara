const test = require('node:test');
const { assert, generatedModule } = require('./_support');

test('LocalVisibility', () => {
    const packageLocal = generatedModule('dev/capylang/test/localVisibility/PackageLocalSupport.js');
    const packagePeer = generatedModule('dev/capylang/test/localVisibility/PackagePeer.js');
    const packageChild = generatedModule('dev/capylang/test/localVisibility/child/PackageChild.js');
    assert.equal(packageLocal.ownPackageValue(1), 6);
    assert.equal(packagePeer.peerCanUseLocalMembers(3), 8);
    assert.equal(packageChild.childCanUseLocalMembers(1), 6);
});
