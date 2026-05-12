#!/usr/bin/env bash

CAPYBARA_BRANCH_PATTERN='^((feature|bug|chore)/#[0-9]+.+|release/[0-9]+\.[0-9]+\.x)$'
CAPYBARA_COMMIT_PATTERN='^(feat|fix|bug|chore|test|docs|refactor|style|perf|build|ci|release|merge|revert)(\(#?[0-9]+\))?: .+'

capybara_default_branch_name() {
    local default_ref=""

    default_ref="$(git symbolic-ref --quiet --short refs/remotes/origin/HEAD 2>/dev/null || true)"

    if [[ "${default_ref}" == */* ]]; then
        printf '%s\n' "${default_ref#*/}"
        return 0
    fi

    printf 'master\n'
}

capybara_print_branch_policy() {
    local default_branch

    default_branch="$(capybara_default_branch_name)"

    {
        cat <<'EOF'
Allowed branch names:
EOF
        cat <<EOF
- ${default_branch}
EOF
        cat <<'EOF'
- {feature|bug|chore}/#{issue-number}{description}
- release/{major}.{minor}.x
EOF
    } >&2
}

capybara_print_commit_policy() {
    cat >&2 <<'EOF'
Commit subjects must start with a conventional type, optionally followed by an issue number.
Examples:
- feat: add parser support
- feat(#111): add parser support
EOF
}

capybara_is_zero_oid() {
    [[ "${1:-}" =~ ^0+$ ]]
}

capybara_is_valid_branch_name() {
    local branch_name="${1:-}"

    [[ "${branch_name}" == "$(capybara_default_branch_name)" || "${branch_name}" =~ ${CAPYBARA_BRANCH_PATTERN} ]]
}

capybara_validate_branch_name() {
    local branch_name="${1:-}"

    if capybara_is_valid_branch_name "${branch_name}"; then
        return 0
    fi

    echo "Invalid branch name: ${branch_name}" >&2
    capybara_print_branch_policy
    return 1
}

capybara_commit_subject_from_file() {
    local message_file="$1"
    local subject=""

    IFS= read -r subject < "${message_file}" || true
    subject="${subject%$'\r'}"
    printf '%s\n' "${subject}"
}

capybara_is_valid_commit_subject() {
    [[ "${1:-}" =~ ${CAPYBARA_COMMIT_PATTERN} ]]
}

capybara_validate_commit_subject() {
    local subject="${1:-}"
    local source="${2:-commit}"

    if capybara_is_valid_commit_subject "${subject}"; then
        return 0
    fi

    echo "Invalid ${source} subject: ${subject}" >&2
    capybara_print_commit_policy
    return 1
}

capybara_validate_commit_message_file() {
    local message_file="$1"
    local subject

    subject="$(capybara_commit_subject_from_file "${message_file}")"
    capybara_validate_commit_subject "${subject}" "commit message"
}

capybara_validate_commit_oid() {
    local oid="$1"
    local subject

    subject="$(git log -1 --format=%s "${oid}")"
    capybara_validate_commit_subject "${subject}" "commit ${oid:0:12}"
}

capybara_pushed_commits() {
    local local_oid="$1"
    local remote_oid="$2"

    if capybara_is_zero_oid "${local_oid}"; then
        return 0
    fi

    if capybara_is_zero_oid "${remote_oid}"; then
        git rev-list --reverse "${local_oid}" --not --remotes
    else
        git rev-list --reverse "${remote_oid}..${local_oid}"
    fi
}

capybara_validate_pushed_commits() {
    local local_oid="$1"
    local remote_oid="$2"
    local pushed_commits
    local failed=0
    local oid

    if ! pushed_commits="$(capybara_pushed_commits "${local_oid}" "${remote_oid}")"; then
        echo "Unable to determine pushed commits for ${local_oid:0:12} against ${remote_oid:0:12}." >&2
        return 1
    fi

    while IFS= read -r oid; do
        if [[ -z "${oid}" ]]; then
            continue
        fi

        if ! capybara_validate_commit_oid "${oid}"; then
            failed=1
        fi
    done <<< "${pushed_commits}"

    return "${failed}"
}
