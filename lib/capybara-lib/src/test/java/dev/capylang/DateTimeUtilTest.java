package dev.capylang;

import capy.dateTime.Date;
import capy.dateTime.DateTime;
import capy.dateTime.Time;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneOffset;
import java.util.Optional;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

class DateTimeUtilTest {
    @ParameterizedTest
    @MethodSource("dateCases")
    void shouldConvertDateToAndFromJavaLocalDate(Date capyDate, LocalDate javaDate) {
        assertThat(DateTimeUtil.toJavaLocalDate(capyDate)).isEqualTo(javaDate);
        assertThat(DateTimeUtil.fromJavaLocalDate(javaDate)).isEqualTo(capyDate);
    }

    @ParameterizedTest
    @MethodSource("localTimeCases")
    void shouldConvertTimeToAndFromJavaLocalTime(Time capyTime, LocalTime javaTime) {
        assertThat(DateTimeUtil.toJavaLocalTime(capyTime)).isEqualTo(javaTime);
        assertThat(DateTimeUtil.fromJavaLocalTime(javaTime)).isEqualTo(capyTime);
    }

    @ParameterizedTest
    @MethodSource("offsetTimeCases")
    void shouldConvertTimeToAndFromJavaOffsetTime(Time capyTime, OffsetTime javaTime) {
        assertThat(DateTimeUtil.toJavaOffsetTime(capyTime)).isEqualTo(javaTime);
        assertThat(DateTimeUtil.fromJavaOffsetTime(javaTime)).isEqualTo(capyTime);
    }

    @ParameterizedTest
    @MethodSource("dateTimeCases")
    void shouldConvertDateTimeToAndFromJavaLocalDateTime(DateTime capyDateTime, LocalDateTime javaDateTime) {
        assertThat(DateTimeUtil.toJavaLocalDateTime(capyDateTime)).isEqualTo(javaDateTime);
        assertThat(DateTimeUtil.fromJavaLocalDateTime(javaDateTime)).isEqualTo(capyDateTime);
    }

    @ParameterizedTest
    @MethodSource("offsetDateTimeCases")
    void shouldConvertDateTimeToAndFromJavaOffsetDateTime(DateTime capyDateTime, OffsetDateTime javaDateTime) {
        assertThat(DateTimeUtil.toJavaOffsetDateTime(capyDateTime)).isEqualTo(javaDateTime);
        assertThat(DateTimeUtil.fromJavaOffsetDateTime(javaDateTime)).isEqualTo(capyDateTime);
    }

    @ParameterizedTest
    @MethodSource("localTimeCases")
    void shouldUseUtcWhenConvertingTimeWithoutOffsetToJavaOffsetTime(Time capyTime, LocalTime ignored) {
        var timeWithoutOffset = new Time(capyTime.hour(), capyTime.minute(), capyTime.second(), Optional.empty());

        assertThat(DateTimeUtil.toJavaOffsetTime(timeWithoutOffset).getOffset()).isEqualTo(ZoneOffset.UTC);
    }

    private static Stream<Arguments> dateCases() {
        return Stream.of(
                Arguments.of(new Date(21, 4, 2026), LocalDate.of(2026, 4, 21)),
                Arguments.of(new Date(1, 1, 1970), LocalDate.of(1970, 1, 1))
        );
    }

    private static Stream<Arguments> localTimeCases() {
        return Stream.of(
                Arguments.of(new Time(13, 15, 45, Optional.empty()), LocalTime.of(13, 15, 45)),
                Arguments.of(new Time(0, 0, 0, Optional.empty()), LocalTime.MIDNIGHT)
        );
    }

    private static Stream<Arguments> offsetTimeCases() {
        return Stream.of(
                Arguments.of(
                        new Time(13, 15, 45, Optional.of(330)),
                        OffsetTime.of(13, 15, 45, 0, ZoneOffset.ofHoursMinutes(5, 30))
                ),
                Arguments.of(
                        new Time(8, 0, 1, Optional.of(-240)),
                        OffsetTime.of(8, 0, 1, 0, ZoneOffset.ofHours(-4))
                )
        );
    }

    private static Stream<Arguments> dateTimeCases() {
        return Stream.of(
                Arguments.of(
                        new DateTime(new Date(21, 4, 2026), new Time(13, 15, 45, Optional.empty())),
                        LocalDateTime.of(2026, 4, 21, 13, 15, 45)
                ),
                Arguments.of(
                        new DateTime(new Date(1, 1, 1970), new Time(0, 0, 0, Optional.empty())),
                        LocalDateTime.of(1970, 1, 1, 0, 0, 0)
                )
        );
    }

    private static Stream<Arguments> offsetDateTimeCases() {
        return Stream.of(
                Arguments.of(
                        new DateTime(new Date(21, 4, 2026), new Time(13, 15, 45, Optional.of(-240))),
                        OffsetDateTime.of(2026, 4, 21, 13, 15, 45, 0, ZoneOffset.ofHours(-4))
                ),
                Arguments.of(
                        new DateTime(new Date(21, 4, 2026), new Time(13, 15, 45, Optional.of(0))),
                        OffsetDateTime.of(2026, 4, 21, 13, 15, 45, 0, ZoneOffset.UTC)
                )
        );
    }
}
