'use strict';

const { NativeClock } = require('../NativeClock.js');

@NativeImplementation("system")
class SystemClock extends NativeClock {
    now_millis() {
        return 12345;
    }
}

module.exports = { SystemClock };
