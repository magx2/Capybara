package dev.capylang.test;

import capy.test.CapyTest;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class TestLogTest {
    @Test
    void shouldClearThreadLocalLogTypeWhenResettingToDefault() throws Exception {
        var currentLogType = currentLogTypeThreadLocal();

        try {
            TestLog.setLogType(CapyTest.LogType.LOG);
            assertEquals(CapyTest.LogType.LOG, currentLogType.get());

            TestLog.setLogType(CapyTest.LogType.NONE);

            assertNull(currentLogType.get());
            assertEquals(CapyTest.LogType.NONE, TestLog.currentLogType());
        } finally {
            currentLogType.remove();
        }
    }

    @Test
    void shouldClearThreadLocalLogTypeWhenResettingToNull() throws Exception {
        var currentLogType = currentLogTypeThreadLocal();

        try {
            TestLog.setLogType(CapyTest.LogType.LOG);
            assertEquals(CapyTest.LogType.LOG, currentLogType.get());

            TestLog.setLogType(null);

            assertNull(currentLogType.get());
            assertEquals(CapyTest.LogType.NONE, TestLog.currentLogType());
        } finally {
            currentLogType.remove();
        }
    }

    @SuppressWarnings("unchecked")
    private static ThreadLocal<CapyTest.LogType> currentLogTypeThreadLocal()
            throws NoSuchFieldException, IllegalAccessException {
        Field currentLogType = TestLog.class.getDeclaredField("CURRENT_LOG_TYPE");
        currentLogType.setAccessible(true);
        return (ThreadLocal<CapyTest.LogType>) currentLogType.get(null);
    }
}
