package dev.capylang.compiler;

/** A compiler diagnostic with a stable machine-readable code and source location. */
public record CompilerError(
        String code,
        String message,
        String moduleName,
        int line,
        int column
) {
    @Override
    public String toString() {
        return "CompilerError { \"code\": " + dev.capylang.CapybaraToStringUtil.toStringValue(code)
                + ", \"message\": " + dev.capylang.CapybaraToStringUtil.toStringValue(message)
                + ", \"moduleName\": " + dev.capylang.CapybaraToStringUtil.toStringValue(moduleName)
                + ", \"line\": " + line + ", \"column\": " + column + " }";
    }
}
