'use strict';

const { Effect } = require('capy/test/CapyTestRuntime');
const { NativeCapybaraParser } = require('./NativeCapybaraParser');

function parser() {
    return Effect.delay(() => new NativeCapybaraParser());
}

module.exports = { parser };
