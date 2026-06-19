'use strict';

const { NativeClock } = require('../nativeinterop/NativeClock.js');

@NativeImplementation("fixed")
class FixedClock extends NativeClock {
    fixed_millis() {
        return 4242;
    }
}

module.exports = { FixedClock };
