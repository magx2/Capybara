'use strict';

const { Clock } = require('../Clock.js');

/** @NativeImplementation(qualifier: "system") */
class SystemClock extends Clock {
    now_millis() {
        return 12345;
    }
}

module.exports = { SystemClock };
