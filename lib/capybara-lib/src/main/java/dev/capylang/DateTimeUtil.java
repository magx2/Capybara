package dev.capylang;

import capy.dateTime.Date;
import capy.dateTime.DateTime;
import capy.dateTime.Time;
import capy.dateTime.TimeModule;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneOffset;
import java.util.Optional;

public final class DateTimeUtil {
    private static final int JAVA_MAX_OFFSET_MINUTES = 18 * TimeModule.mINUTESINHOUR;

    private DateTimeUtil() {
    }

    public static LocalDate toJavaLocalDate(Date date) {
        return LocalDate.of(date.year(), date.month(), date.day());
    }

    public static Date fromJavaLocalDate(LocalDate date) {
        return new Date(date.getDayOfMonth(), date.getMonthValue(), date.getYear());
    }

    /**
     * Converts Capy {@link Time} to Java {@link LocalTime}.
     * <p>
     * Note: this conversion intentionally drops Capy offset information.
     */
    public static LocalTime toJavaLocalTime(Time time) {
        return LocalTime.of(time.hour(), time.minute(), time.second());
    }

    public static Time fromJavaLocalTime(LocalTime time) {
        return new Time(time.getHour(), time.getMinute(), time.getSecond(), Optional.empty());
    }

    public static OffsetTime toJavaOffsetTime(Time time) {
        var offset = offsetMinutes(time)
                .map(DateTimeUtil::toJavaZoneOffset)
                .orElseGet(() -> ZoneOffset.UTC);
        return OffsetTime.of(toJavaLocalTime(time), offset);
    }

    public static Time fromJavaOffsetTime(OffsetTime time) {
        var totalSeconds = time.getOffset().getTotalSeconds();
        if (Math.floorMod(totalSeconds, TimeModule.sECONDSINMINUTE) != 0) {
            throw new IllegalArgumentException(
                    "Java offset with second precision is unsupported: " + time.getOffset()
            );
        }
        var offsetMinutes = Math.floorDiv(totalSeconds, TimeModule.sECONDSINMINUTE);
        return new Time(time.getHour(), time.getMinute(), time.getSecond(), Optional.of(offsetMinutes));
    }

    public static LocalDateTime toJavaLocalDateTime(DateTime dateTime) {
        return LocalDateTime.of(toJavaLocalDate(dateTime.date()), toJavaLocalTime(dateTime.time()));
    }

    public static DateTime fromJavaLocalDateTime(LocalDateTime dateTime) {
        return new DateTime(
                fromJavaLocalDate(dateTime.toLocalDate()),
                fromJavaLocalTime(dateTime.toLocalTime())
        );
    }

    public static OffsetDateTime toJavaOffsetDateTime(DateTime dateTime) {
        return OffsetDateTime.of(toJavaLocalDateTime(dateTime), toJavaOffsetTime(dateTime.time()).getOffset());
    }

    public static DateTime fromJavaOffsetDateTime(OffsetDateTime dateTime) {
        return new DateTime(
                fromJavaLocalDate(dateTime.toLocalDate()),
                fromJavaOffsetTime(dateTime.toOffsetTime())
        );
    }

    private static ZoneOffset toJavaZoneOffset(int minutes) {
        if (minutes < -JAVA_MAX_OFFSET_MINUTES || minutes > JAVA_MAX_OFFSET_MINUTES) {
            throw new IllegalArgumentException("Capy offset out of Java ZoneOffset range: " + minutes + " minutes");
        }
        return ZoneOffset.ofTotalSeconds(Math.multiplyExact(minutes, TimeModule.sECONDSINMINUTE));
    }

    private static Optional<Integer> offsetMinutes(Time time) {
        return OptionUtil.toJavaOptional(OptionUtil.toCapyOption(time.offset_minutes()))
                .map(Integer.class::cast);
    }
}
