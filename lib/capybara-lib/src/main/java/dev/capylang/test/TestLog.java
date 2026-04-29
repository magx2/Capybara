package dev.capylang.test;

import capy.test.CapyTest;

public final class TestLog {
    private static final ThreadLocal<CapyTest.LogType> CURRENT_LOG_TYPE =
            ThreadLocal.withInitial(() -> CapyTest.LogType.NONE);

    private TestLog() {
    }

    public static CapyTest.LogType currentLogType() {
        return CURRENT_LOG_TYPE.get();
    }

    public static void setLogType(CapyTest.LogType logType) {
        CURRENT_LOG_TYPE.set(logType == null ? CapyTest.LogType.NONE : logType);
    }
}
