package dev.capylang.test;

public final class CtrfJson {
    private CtrfJson() {
    }

    public static String escape(String value) {
        var escaped = new StringBuilder(value.length());
        for (var i = 0; i < value.length(); ) {
            var codePoint = value.codePointAt(i);
            appendEscaped(escaped, codePoint);
            i += Character.charCount(codePoint);
        }
        return escaped.toString();
    }

    private static void appendEscaped(StringBuilder escaped, int codePoint) {
        switch (codePoint) {
            case '"' -> escaped.append("\\\"");
            case '\\' -> escaped.append("\\\\");
            case '\b' -> escaped.append("\\b");
            case '\f' -> escaped.append("\\f");
            case '\n' -> escaped.append("\\n");
            case '\r' -> escaped.append("\\r");
            case '\t' -> escaped.append("\\t");
            default -> {
                if (codePoint < 0x20) {
                    appendUnicodeEscape(escaped, codePoint);
                } else {
                    escaped.appendCodePoint(codePoint);
                }
            }
        }
    }

    private static void appendUnicodeEscape(StringBuilder escaped, int codePoint) {
        escaped.append("\\u00");
        escaped.append(Character.forDigit(codePoint / 0x10, 16));
        escaped.append(Character.forDigit(codePoint % 0x10, 16));
    }
}
