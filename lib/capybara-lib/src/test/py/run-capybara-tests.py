#!/usr/bin/env python3
import argparse
import html
import importlib
import json
import pathlib
import sys
import time
import traceback


def parse_args(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--generated-dir", required=True)
    parser.add_argument("--output-dir")
    parser.add_argument("--report-type", default="JUNIT_CTRF_JEST")
    parser.add_argument("--log", default="NONE")
    parser.add_argument("--tests", action="append", default=[])
    parser.add_argument("--available-tests", action="store_true")
    args = parser.parse_args(argv)
    args.report_type = args.report_type.upper()
    args.log = args.log.upper()
    if not args.available_tests and not args.output_dir:
        parser.error("--output-dir is required unless --available-tests is set")
    return args


def unsafe_run(value):
    while value is not None:
        runner = getattr(value, "unsafe_run", None) or getattr(value, "unsafeRun", None)
        if not callable(runner):
            return value
        value = runner()
    return value


def flatten_test_values(value):
    value = unsafe_run(value)
    if value is None:
        return []
    if isinstance(value, list):
        flattened = []
        for item in value:
            flattened.extend(flatten_test_values(item))
        return flattened
    return [value]


def field(value, *names, default=None):
    for name in names:
        if isinstance(value, dict) and name in value:
            return value[name]
        if hasattr(value, name):
            return getattr(value, name)
    return default


def test_file_name(test_file):
    return field(test_file, "file_name", "fileName", "path")


def test_cases(test_file):
    return flatten_test_values(field(test_file, "test_cases", "testCases", default=[]))


def normalized_test_file_name(file_name):
    file_name = str(file_name)
    return file_name[:-5] if file_name.endswith(".cfun") else file_name


def escape_test_name(test_name):
    return str(test_name).replace("\\", "\\\\").replace('"', '\\"')


def unescape_test_name(test_name):
    result = []
    escaping = False
    for ch in test_name:
        if escaping:
            result.append(ch)
            escaping = False
        elif ch == "\\":
            escaping = True
        else:
            result.append(ch)
    if escaping:
        result.append("\\")
    return "".join(result)


def test_id(test_file, test_case):
    return f'{normalized_test_file_name(test_file_name(test_file))}."{escape_test_name(field(test_case, "name"))}"'


def parse_selector(raw):
    selector = raw.strip()
    separator = selector.find('."')
    if separator < 0:
        return {"raw": raw, "file_name": normalized_test_file_name(selector), "test_name": None}
    if not selector.endswith('"'):
        raise ValueError(f"Invalid test selector `{raw}`")
    return {
        "raw": raw,
        "file_name": normalized_test_file_name(selector[:separator]),
        "test_name": unescape_test_name(selector[separator + 2:-1]),
    }


def selector_matches_file(selector, file_name):
    return selector["file_name"] == normalized_test_file_name(file_name)


def selector_matches(selector, file_name, test_name):
    return selector_matches_file(selector, file_name) and (
        selector["test_name"] is None or selector["test_name"] == test_name
    )


def with_test_cases(test_file, cases):
    if isinstance(test_file, dict):
        filtered = dict(test_file)
    else:
        filtered = {
            "file_name": test_file_name(test_file),
            "fileName": test_file_name(test_file),
            "path": test_file_name(test_file),
        }
    filtered["test_cases"] = cases
    filtered["testCases"] = cases
    return filtered


def filter_test_files(files, raw_selectors):
    if len(raw_selectors) == 0:
        return files
    selectors = [parse_selector(raw) for raw in raw_selectors]
    matched = [False] * len(selectors)
    filtered = []

    for test_file in files:
        file_name = test_file_name(test_file)
        for index, selector in enumerate(selectors):
            if selector["test_name"] is None and selector_matches_file(selector, file_name):
                matched[index] = True

        cases = []
        for test_case in test_cases(test_file):
            test_name = field(test_case, "name")
            include = False
            for index, selector in enumerate(selectors):
                if selector_matches(selector, file_name, test_name):
                    matched[index] = True
                    include = True
            if include:
                cases.append(test_case)

        if len(cases) > 0:
            filtered.append(with_test_cases(test_file, cases))

    missing = [selector["raw"] for index, selector in enumerate(selectors) if not matched[index]]
    if len(missing) == 1:
        raise ValueError(f"Test selector `{missing[0]}` did not match any test")
    if len(missing) > 1:
        quoted = ", ".join(f"`{selector}`" for selector in missing)
        raise ValueError(f"Test selectors did not match any test: {quoted}")
    return filtered


def assertion_value(value):
    value = unsafe_run(value)
    if callable(value):
        value = unsafe_run(value())
    return value


def assertions_of(value):
    value = unsafe_run(value)
    if value is None:
        return [{"result": False, "message": "Test returned no assertions", "type": "TestRunner"}]
    if isinstance(value, list):
        assertions = []
        for item in value:
            assertions.extend(assertions_of(item))
        return assertions
    assertions = field(value, "assertions")
    if isinstance(assertions, list):
        return [assertion_value(supplier) for supplier in assertions]
    if callable(getattr(value, "succeeded", None)) or field(value, "result") is not None:
        return [value]
    return [{"result": False, "message": f"Unsupported assertion result: {value}", "type": "TestRunner"}]


def assertion_result(assertion):
    assertion = assertion_value(assertion)
    return {
        "result": bool(field(assertion, "result", default=False)),
        "message": str(field(assertion, "message", default="")),
        "type": str(field(assertion, "type", default="Assertion")),
    }


def run_case(test_case):
    start = int(time.time() * 1000)
    started = time.perf_counter()
    try:
        body = field(test_case, "body")
        result = body() if callable(body) else field(test_case, "result")
        assertions = [assertion_result(assertion) for assertion in assertions_of(result)]
        failed = next((assertion for assertion in assertions if not assertion["result"]), None)
        duration_ms = max(0, int(round((time.perf_counter() - started) * 1000)))
        return {
            "name": field(test_case, "name"),
            "assertions": assertions,
            "failed": failed,
            "time": duration_ms / 1000,
            "durationMs": duration_ms,
            "start": start,
            "stop": start + duration_ms,
        }
    except Exception:
        duration_ms = max(0, int(round((time.perf_counter() - started) * 1000)))
        message = traceback.format_exc()
        return {
            "name": field(test_case, "name"),
            "assertions": [],
            "failed": {"result": False, "message": message, "type": "Error"},
            "time": duration_ms / 1000,
            "durationMs": duration_ms,
            "start": start,
            "stop": start + duration_ms,
        }


def teamcity_escape(value):
    return str(value).replace("|", "||").replace("'", "|'").replace("\n", "|n").replace("\r", "|r").replace("[", "|[").replace("]", "|]")


def log_teamcity(log_type, event, **attrs):
    if log_type not in ("TC", "TEAM_CITY"):
        return
    payload = " ".join(f"{key}='{teamcity_escape(value)}'" for key, value in attrs.items())
    print(f"##teamcity[{event} {payload}]")


def report_path(output_dir, file_name):
    normalized = str(file_name).lstrip("/").replace("/", ".")
    return pathlib.Path(output_dir, f"TEST-{normalized}.xml")


def suite_report(file_name, results):
    failures = [result for result in results if result["failed"]]
    assertions = sum(len(result["assertions"]) for result in results)
    elapsed = sum(result["time"] for result in results)
    cases = []
    for result in results:
        base = (
            f'<testcase name="{html.escape(str(result["name"]), quote=True)}" '
            f'classname="{html.escape(str(file_name), quote=True)}" '
            f'assertions="{len(result["assertions"])}" time="{result["time"]}" '
            f'file="{html.escape(str(file_name), quote=True)}" line="0">'
        )
        if result["failed"]:
            message = html.escape(str(result["failed"]["message"]), quote=True)
            failure_type = html.escape(str(result["failed"]["type"]), quote=True)
            cases.append(f'{base}<failure message="{message}" type="{failure_type}">{message}</failure></testcase>')
        else:
            cases.append(f"{base}</testcase>")
    return (
        f'<testsuite name="{html.escape(str(file_name), quote=True)}" tests="{len(results)}" '
        f'failures="{len(failures)}" errors="0" skipped="0" assertions="{assertions}" '
        f'time="{elapsed}" timestamp="{time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}">'
        f'{"".join(cases)}</testsuite>'
    )


def write_reports(output_dir, report_type, suites, start_ms, stop_ms):
    output_dir = pathlib.Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    parts = set(report_type.split("_"))
    if "JUNIT" in parts:
        for suite in suites:
            report_path(output_dir, suite["fileName"]).write_text(suite_report(suite["fileName"], suite["results"]))
    if "CTRF" in parts:
        tests = []
        for suite in suites:
            for result in suite["results"]:
                test = {
                    "name": result["name"],
                    "status": "failed" if result["failed"] else "passed",
                    "duration": result["durationMs"],
                    "start": result["start"],
                    "stop": result["stop"],
                    "suite": [suite["fileName"]],
                    "filePath": suite["fileName"],
                }
                if result["failed"]:
                    test["message"] = result["failed"]["message"]
                    test["trace"] = result["failed"]["message"]
                    test["extra"] = {"failureType": result["failed"]["type"]}
                tests.append(test)
        failed = sum(1 for test in tests if test["status"] == "failed")
        output_dir.joinpath("ctrf-report.json").write_text(json.dumps({
            "reportFormat": "CTRF",
            "specVersion": "0.0.0",
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(stop_ms / 1000)),
            "generatedBy": "Capybara",
            "results": {
                "tool": {"name": "Capybara"},
                "summary": {
                    "tests": len(tests), "passed": len(tests) - failed, "failed": failed,
                    "skipped": 0, "pending": 0, "other": 0, "suites": len(suites),
                    "start": start_ms, "stop": stop_ms, "duration": max(0, stop_ms - start_ms),
                },
                "tests": tests,
            },
        }, indent=2) + "\n")
    if "JEST" in parts:
        results = [result for suite in suites for result in suite["results"]]
        failed = sum(1 for result in results if result["failed"])
        failed_suites = sum(1 for suite in suites if any(result["failed"] for result in suite["results"]))
        output_dir.joinpath("jest-report.json").write_text(json.dumps({
            "success": failed == 0,
            "numTotalTestSuites": len(suites),
            "numPassedTestSuites": len(suites) - failed_suites,
            "numFailedTestSuites": failed_suites,
            "numPendingTestSuites": 0,
            "numTotalTests": len(results),
            "numPassedTests": len(results) - failed,
            "numFailedTests": failed,
            "numPendingTests": 0,
            "numTodoTests": 0,
            "startTime": start_ms,
            "testResults": [{
                "name": suite["fileName"],
                "status": "failed" if any(result["failed"] for result in suite["results"]) else "passed",
                "startTime": suite["results"][0]["start"] if suite["results"] else start_ms,
                "endTime": max((result["stop"] for result in suite["results"]), default=start_ms),
                "assertionResults": [{
                    "ancestorTitles": [suite["fileName"]],
                    "title": result["name"],
                    "status": "failed" if result["failed"] else "passed",
                    "duration": result["durationMs"],
                    "failureMessages": [result["failed"]["message"]] if result["failed"] else [],
                    "location": None,
                } for result in suite["results"]],
            } for suite in suites],
        }, indent=2) + "\n")


def main(argv):
    args = parse_args(argv)
    generated_dir = pathlib.Path(args.generated_dir).resolve()
    sys.path.insert(0, str(generated_dir))
    runtime = importlib.import_module("capy.test.CapyTestRuntime")
    files = flatten_test_values(runtime.gatherTests())

    if args.available_tests:
        for test_file in files:
            for test_case in test_cases(test_file):
                print(test_id(test_file, test_case))
        return 0

    files = filter_test_files(files, args.tests)

    failed = False
    suites = []
    start_ms = int(time.time() * 1000)
    for test_file in files:
        file_name = test_file_name(test_file)
        log_teamcity(args.log, "testSuiteStarted", name=file_name)
        results = []
        for test_case in test_cases(test_file):
            case_name = field(test_case, "name")
            log_teamcity(args.log, "testStarted", name=case_name)
            result = run_case(test_case)
            if result["failed"]:
                failed = True
                log_teamcity(args.log, "testFailed", name=case_name, message=result["failed"]["message"])
            log_teamcity(args.log, "testFinished", name=case_name)
            results.append(result)
        log_teamcity(args.log, "testSuiteFinished", name=file_name)
        suites.append({"testFile": test_file, "fileName": file_name, "results": results})
    write_reports(args.output_dir, args.report_type, suites, start_ms, int(time.time() * 1000))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
