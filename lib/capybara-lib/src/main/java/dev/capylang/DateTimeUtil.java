package dev.capylang;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Optional;

public final class DateTimeUtil {
    private static final int SECONDS_IN_MINUTE = 60;
    private static final int MINUTES_IN_HOUR = 60;
    private static final int JAVA_MAX_OFFSET_MINUTES = 18 * MINUTES_IN_HOUR;

    private DateTimeUtil() {
    }

    public static LocalDate toJavaLocalDate(Object date) {
        return LocalDate.of(intField(date, "year"), intField(date, "month"), intField(date, "day"));
    }

    public static Object fromJavaLocalDate(LocalDate date) {
        return data("Date", Map.of(
                "day", date.getDayOfMonth(),
                "month", date.getMonthValue(),
                "year", date.getYear()
        ));
    }

    public static LocalTime toJavaLocalTime(Object time) {
        return LocalTime.of(intField(time, "hour"), intField(time, "minute"), intField(time, "second"));
    }

    public static Object fromJavaLocalTime(LocalTime time) {
        var truncated = time.truncatedTo(ChronoUnit.SECONDS);
        return data("Time", Map.of(
                "hour", truncated.getHour(),
                "minute", truncated.getMinute(),
                "second", truncated.getSecond(),
                "offset_minutes", Optional.empty()
        ));
    }

    public static OffsetTime toJavaOffsetTime(Object time) {
        var offset = offsetMinutes(time)
                .map(DateTimeUtil::toJavaZoneOffset)
                .orElseGet(() -> ZoneOffset.UTC);
        return OffsetTime.of(toJavaLocalTime(time), offset);
    }

    public static Object fromJavaOffsetTime(OffsetTime time) {
        var totalSeconds = time.getOffset().getTotalSeconds();
        if (Math.floorMod(totalSeconds, SECONDS_IN_MINUTE) != 0) {
            throw new IllegalArgumentException(
                    "Java offset with second precision is unsupported: " + time.getOffset()
            );
        }
        var offsetMinutes = Math.floorDiv(totalSeconds, SECONDS_IN_MINUTE);
        var truncated = time.toLocalTime().truncatedTo(ChronoUnit.SECONDS);
        return data("Time", Map.of(
                "hour", truncated.getHour(),
                "minute", truncated.getMinute(),
                "second", truncated.getSecond(),
                "offset_minutes", Optional.of(offsetMinutes)
        ));
    }

    public static LocalDateTime toJavaLocalDateTime(Object dateTime) {
        return LocalDateTime.of(toJavaLocalDate(field(dateTime, "date")), toJavaLocalTime(field(dateTime, "time")));
    }

    public static Object fromJavaLocalDateTime(LocalDateTime dateTime) {
        return data("DateTime", Map.of(
                "date", fromJavaLocalDate(dateTime.toLocalDate()),
                "time", fromJavaLocalTime(dateTime.toLocalTime())
        ));
    }

    public static OffsetDateTime toJavaOffsetDateTime(Object dateTime) {
        return OffsetDateTime.of(toJavaLocalDateTime(dateTime), toJavaOffsetTime(field(dateTime, "time")).getOffset());
    }

    public static Object fromJavaOffsetDateTime(OffsetDateTime dateTime) {
        return data("DateTime", Map.of(
                "date", fromJavaLocalDate(dateTime.toLocalDate()),
                "time", fromJavaOffsetTime(dateTime.toOffsetTime())
        ));
    }

    private static ZoneOffset toJavaZoneOffset(int minutes) {
        if (minutes < -JAVA_MAX_OFFSET_MINUTES || minutes > JAVA_MAX_OFFSET_MINUTES) {
            throw new IllegalArgumentException("Capy offset out of Java ZoneOffset range: " + minutes + " minutes");
        }
        return ZoneOffset.ofTotalSeconds(Math.multiplyExact(minutes, SECONDS_IN_MINUTE));
    }

    private static Optional<Integer> offsetMinutes(Object time) {
        return OptionUtil.<Integer>toJavaOptional(field(time, "offset_minutes"));
    }

    @SuppressWarnings("unchecked")
    private static Object field(Object value, String name) {
        return ((Map<String, Object>) value).get(name);
    }

    private static int intField(Object value, String name) {
        var raw = field(value, name);
        if (raw instanceof Number number) {
            return number.intValue();
        }
        if (raw instanceof Map<?, ?> map && map.get("value") instanceof Number number) {
            return number.intValue();
        }
        return Integer.parseInt(String.valueOf(raw));
    }

    private static Object data(String type, Map<String, Object> fields) {
        var result = new java.util.LinkedHashMap<String, Object>();
        result.put("__type", type);
        result.putAll(fields);
        return Map.copyOf(result);
    }
}
