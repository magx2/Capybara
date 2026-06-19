'use strict';

const { NativeHasher } = require('../nativeinterop/NativeHasher.js');

@NativeImplementation("stable")
class StableHasher extends NativeHasher {
    stable_hash(value) {
        return `stable-${String(value).replace(/:/g, '-')}`;
    }
}

module.exports = { StableHasher };
