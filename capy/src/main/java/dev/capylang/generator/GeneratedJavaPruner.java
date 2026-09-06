package dev.capylang.generator;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/** Removes generated Java support members that are unreachable from module declarations. */
final class GeneratedJavaPruner {
    private static final Pattern HELPER_METHOD = Pattern.compile(
            "\\bprivate\\s+static\\b[^{};=]*?\\b(__capy_[A-Za-z0-9_$]*|unsupported)\\s*\\("
    );
    private static final Pattern FUNCTION_INTERFACE = Pattern.compile(
            "\\bprivate\\s+interface\\s+(__CapyFunction[34])\\b"
    );
    private static final Pattern TOP_LEVEL_TYPE = Pattern.compile(
            "\\bpublic\\s+(?:final\\s+)?(?:class|interface)\\b"
    );

    private GeneratedJavaPruner() {
    }

    static String prune(String source) {
        var members = topLevelMembers(source);
        var candidates = members.stream()
                .filter(member -> member.helperName() != null)
                .toList();
        if (candidates.isEmpty()) {
            return source;
        }

        var reachableSource = new StringBuilder(source.length());
        var cursor = 0;
        for (var candidate : candidates) {
            reachableSource.append(source, cursor, candidate.start());
            cursor = candidate.end();
        }
        reachableSource.append(source, cursor, source.length());

        var reachable = new HashSet<String>();
        var changed = true;
        while (changed) {
            changed = false;
            for (var candidate : candidates) {
                if (!reachable.contains(candidate.helperName())
                        && containsCodeIdentifier(reachableSource, candidate.helperName())) {
                    reachable.add(candidate.helperName());
                    reachableSource.append(candidate.source());
                    changed = true;
                }
            }
        }

        var result = new StringBuilder(source.length());
        cursor = 0;
        for (var candidate : candidates) {
            if (reachable.contains(candidate.helperName())) {
                continue;
            }
            result.append(source, cursor, candidate.start());
            cursor = candidate.end();
        }
        result.append(source, cursor, source.length());
        return result.toString();
    }

    private static boolean containsCodeIdentifier(CharSequence source, String identifier) {
        var text = source.toString();
        var state = new ScanState();
        for (var index = 0; index < text.length(); index++) {
            if (!state.accept(text, index) || !text.startsWith(identifier, index)) {
                continue;
            }
            var before = index == 0 ? '\0' : text.charAt(index - 1);
            var end = index + identifier.length();
            var after = end == text.length() ? '\0' : text.charAt(end);
            if (!Character.isJavaIdentifierPart(before) && !Character.isJavaIdentifierPart(after)) {
                return true;
            }
        }
        return false;
    }

    private static List<Member> topLevelMembers(String source) {
        var type = TOP_LEVEL_TYPE.matcher(source);
        if (!type.find()) {
            return List.of();
        }
        var classBody = firstCodeCharacter(source, '{', type.end());
        if (classBody < 0) {
            return List.of();
        }

        var members = new ArrayList<Member>();
        var state = new ScanState();
        var depth = 1;
        var parentheses = 0;
        var memberStart = classBody + 1;
        var blockStart = -1;
        for (var index = classBody + 1; index < source.length(); index++) {
            var character = source.charAt(index);
            if (!state.accept(source, index)) {
                continue;
            }
            if (character == '{') {
                if (depth == 1 && parentheses == 0) {
                    blockStart = index;
                }
                depth++;
            } else if (character == '}') {
                depth--;
                if (depth == 1 && blockStart >= 0) {
                    members.add(member(source, memberStart, index + 1, blockStart));
                    memberStart = index + 1;
                    blockStart = -1;
                } else if (depth == 0) {
                    break;
                }
            } else if (character == ';' && depth == 1) {
                memberStart = index + 1;
            } else if (character == '(' && depth == 1) {
                parentheses++;
            } else if (character == ')' && depth == 1 && parentheses > 0) {
                parentheses--;
            }
        }
        return members;
    }

    private static Member member(String source, int start, int end, int blockStart) {
        var header = source.substring(start, blockStart);
        var method = HELPER_METHOD.matcher(header);
        if (method.find()) {
            return new Member(start, end, method.group(1), source.substring(start, end));
        }
        var functionInterface = FUNCTION_INTERFACE.matcher(header);
        if (functionInterface.find()) {
            return new Member(start, end, functionInterface.group(1), source.substring(start, end));
        }
        return new Member(start, end, null, source.substring(start, end));
    }

    private static int firstCodeCharacter(String source, char expected, int offset) {
        var state = new ScanState();
        for (var index = offset; index < source.length(); index++) {
            if (state.accept(source, index) && source.charAt(index) == expected) {
                return index;
            }
        }
        return -1;
    }

    private record Member(int start, int end, String helperName, String source) {
    }

    private static final class ScanState {
        private boolean string;
        private boolean character;
        private boolean lineComment;
        private boolean blockComment;
        private boolean escaped;

        boolean accept(String source, int index) {
            var current = source.charAt(index);
            var next = index + 1 < source.length() ? source.charAt(index + 1) : '\0';
            if (lineComment) {
                if (current == '\n') {
                    lineComment = false;
                }
                return false;
            }
            if (blockComment) {
                if (current == '*' && next == '/') {
                    blockComment = false;
                }
                return false;
            }
            if (string || character) {
                if (escaped) {
                    escaped = false;
                } else if (current == '\\') {
                    escaped = true;
                } else if (string && current == '"') {
                    string = false;
                } else if (character && current == '\'') {
                    character = false;
                }
                return false;
            }
            if (current == '/' && next == '/') {
                lineComment = true;
                return false;
            }
            if (current == '/' && next == '*') {
                blockComment = true;
                return false;
            }
            if (current == '"') {
                string = true;
                return false;
            }
            if (current == '\'') {
                character = true;
                return false;
            }
            return true;
        }
    }
}
