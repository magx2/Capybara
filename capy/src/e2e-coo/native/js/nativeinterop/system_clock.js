'use strict';

class SystemClock {
    constructor() {
        this.base = 1700000000000n;
    }

    now_millis() {
        return this.base + 42n;
    }
}

module.exports = { SystemClock };
