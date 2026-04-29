package dev.capylang.test;

import capy.test.CapyTest;

public final class TestLog {
    private static final ThreadLocal<CapyTest.LogType> CURRENT_LOG_TYPE = new ThreadLocal<>();

    private TestLog() {
    }

    public static CapyTest.LogType currentLogType() {
        var logType = CURRENT_LOG_TYPE.get();
        return logType == null ? CapyTest.LogType.NONE : logType;
    }

    public static void setLogType(CapyTest.LogType logType) {
        if (logType == null || logType == CapyTest.LogType.NONE) {
            CURRENT_LOG_TYPE.remove();
        } else {
            CURRENT_LOG_TYPE.set(logType);
        }
    }
}
