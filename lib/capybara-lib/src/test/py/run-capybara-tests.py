#!/usr/bin/env python3
import argparse
import html
import importlib
import json
import math
import pathlib
import sys
import time
import traceback

SUPPORTED_REPORT_TYPES = ("JUNIT", "CTRF", "JEST")


def parse_args(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--generated-dir", required=True)
    parser.add_argument("--output-dir")
    parser.add_argument("--report-type", default="JUNIT", type=str.upper, choices=SUPPORTED_REPORT_TYPES)
    parser.add_argument("--log", default="NONE")
    parser.add_argument("--tests", action="append", default=[])
    parser.add_argument("--available-tests", action="store_true")
    args = parser.parse_args(argv)
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
        file_selectors = [
            (index, selector)
            for index, selector in enumerate(selectors)
            if selector_matches_file(selector, file_name)
        ]
        if len(file_selectors) == 0:
            continue

        for index, selector in file_selectors:
            if selector["test_name"] is None and selector_matches_file(selector, file_name):
                matched[index] = True

        cases = []
        for test_case in test_cases(test_file):
            test_name = field(test_case, "name")
            include = False
            for index, selector in file_selectors:
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


def linked_dir_for_generated_dir(generated_dir):
    if generated_dir.parent.name != "python":
        return None
    return generated_dir.parent.parent / "linked-python" / generated_dir.name


def load_linked_modules(generated_dir):
    linked_dir = linked_dir_for_generated_dir(generated_dir)
    if linked_dir is None or not linked_dir.exists():
        return {}
    modules = {}
    for path in linked_dir.rglob("*.json"):
        try:
            data = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        module_name = ".".join(path.relative_to(linked_dir).with_suffix("").parts)
        modules[module_name] = data
    return modules


def capy_data_fields(value):
    value = unsafe_run(value)
    if isinstance(value, dict):
        return {key: field for key, field in value.items() if key != "__type"}
    if hasattr(value, "__dict__"):
        return dict(value.__dict__)
    try:
        return dict(value)
    except (TypeError, ValueError):
        return {}


def patch_runtime_data_spread(runtime):
    original = getattr(runtime, "__capy_data", None)
    if not callable(original) or getattr(runtime, "_capy_runner_data_spread_patch", False):
        return

    def capy_data(type_name, fields):
        expanded = {}
        for key, value in dict(fields or {}).items():
            if key == "":
                expanded.update(capy_data_fields(value))
            else:
                expanded[key] = value
        return original(type_name, expanded)

    setattr(runtime, "__capy_data", capy_data)
    setattr(runtime, "_capy_runner_data_spread_patch", True)


def patch_runtime_math_exports(runtime):
    if not hasattr(runtime, "sqrt"):
        setattr(runtime, "sqrt", math.sqrt)
    exported = getattr(runtime, "__all__", None)
    if isinstance(exported, list) and "sqrt" not in exported:
        exported.append("sqrt")


def selected_test_module_names(raw_selectors):
    names = []
    for raw in raw_selectors:
        file_name = parse_selector(raw)["file_name"].lstrip("/")
        if not file_name:
            continue
        module_name = file_name.replace("/", ".") + "CapyTest"
        if module_name not in names:
            names.append(module_name)
    return names


def gather_test_module(module_name):
    try:
        module = importlib.import_module(module_name)
    except ModuleNotFoundError as exc:
        if exc.name == module_name:
            return []
        raise
    tests = getattr(module, "tests", None)
    if not callable(tests):
        return []
    return flatten_test_values(tests())


def gather_selected_test_files(raw_selectors):
    files = []
    for module_name in selected_test_module_names(raw_selectors):
        files.extend(gather_test_module(module_name))
    return files


def merge_test_files(primary, extra):
    merged = list(primary)
    seen = {normalized_test_file_name(test_file_name(test_file)) for test_file in primary}
    for test_file in extra:
        file_name = normalized_test_file_name(test_file_name(test_file))
        if file_name not in seen:
            merged.append(test_file)
            seen.add(file_name)
    return merged


def python_identifier(name):
    result = []
    for char in str(name):
        if char == "_" or char.isalpha() or char.isdigit():
            result.append(char)
        else:
            result.append("_")
    value = "".join(result) or "_"
    if value[0] == "_" or value[0].isalpha():
        return value
    return "_" + value


def generated_function_name(function):
    location = function.get("location", {})
    return f"{python_identifier(function.get('name', ''))}__{location.get('line', 0)}_{location.get('column', 0)}"


def simple_type_name(type_name):
    type_name = str(type_name or "")
    if type_name.startswith("__capy_raw|"):
        type_name = type_name[len("__capy_raw|"):]
    if "[" in type_name:
        type_name = type_name.split("[", 1)[0]
    if "/" in type_name:
        type_name = type_name.rsplit("/", 1)[1]
    if "." in type_name:
        type_name = type_name.rsplit(".", 1)[1]
    return type_name


def linked_data_parents(linked_modules):
    parents = {}
    for module in linked_modules.values():
        for function in module.get("functions", []):
            name = str(function.get("name", ""))
            if not name.startswith("__capy_schema_parent|"):
                continue
            parts = name.split("|")
            if len(parts) < 3:
                continue
            parent = simple_type_name(field(function.get("body", {}), "value", default=""))
            parents.setdefault(simple_type_name(parts[1]), []).append(parent)
    return parents


def value_type_name(value):
    value = unsafe_run(value)
    if isinstance(value, dict) and "__type" in value:
        return simple_type_name(value.get("__type"))
    if isinstance(value, bool):
        return "bool"
    if isinstance(value, int):
        return "int"
    if isinstance(value, float):
        return "float"
    if isinstance(value, str):
        return "String"
    if isinstance(value, list):
        return "List"
    if isinstance(value, set):
        return "Set"
    if isinstance(value, dict):
        return "Dict"
    return "any"


def inheritance_distance(actual_type, expected_type, parents, depth=1, seen=None):
    seen = set(seen or ())
    actual_type = simple_type_name(actual_type)
    expected_type = simple_type_name(expected_type)
    if actual_type in seen or depth > 16:
        return None
    seen.add(actual_type)
    best = None
    for parent in parents.get(actual_type, []):
        parent = simple_type_name(parent)
        distance = depth if parent == expected_type else inheritance_distance(parent, expected_type, parents, depth + 1, seen)
        if distance is not None and (best is None or distance < best):
            best = distance
    return best


def argument_type_score(actual_type, expected_type, parents):
    actual_type = simple_type_name(actual_type)
    expected_type = simple_type_name(expected_type)
    if expected_type == "any":
        return 1
    if actual_type == expected_type:
        return 64
    if actual_type == "any":
        return -1
    distance = inheritance_distance(actual_type, expected_type, parents)
    if distance is None:
        return -1
    return 64 - distance


def overload_score(candidate, args, parents):
    parameters = candidate["parameters"]
    if len(parameters) != len(args):
        return -1
    total = 0
    for parameter, arg in zip(parameters, args):
        expected = field(parameter.get("typeReference", {}), "name", default="any")
        score = argument_type_score(value_type_name(arg), expected, parents)
        if score < 0:
            return -1
        total += score
    return total


def make_overload_dispatcher(candidates, parents, fallback):
    def dispatcher(*args):
        best = None
        best_score = -1
        for candidate in candidates:
            score = overload_score(candidate, args, parents)
            if score > best_score:
                best = candidate
                best_score = score
        if best is None:
            return fallback(*args)
        return best["callable"](*args)

    return dispatcher


def dispatchable_function(function):
    visibility = str(function.get("visibility", ""))
    name = str(function.get("name", ""))
    return (
        not name.startswith("__capy_schema_")
        and visibility != "private"
        and not visibility.startswith("const:")
    )


def patch_overload_dispatchers(generated_dir):
    linked_modules = load_linked_modules(generated_dir)
    parents = linked_data_parents(linked_modules)
    for module_name, linked_module in linked_modules.items():
        module = sys.modules.get(module_name)
        if module is None:
            continue
        groups = {}
        for function in linked_module.get("functions", []):
            if not dispatchable_function(function):
                continue
            py_name = generated_function_name(function)
            py_function = getattr(module, py_name, None)
            if not callable(py_function):
                continue
            groups.setdefault(function.get("name", ""), []).append({
                "python_name": py_name,
                "parameters": function.get("parameters", []),
                "callable": py_function,
            })
        for candidates in groups.values():
            if len(candidates) < 2:
                continue
            dispatcher = make_overload_dispatcher(candidates, parents, candidates[0]["callable"])
            for candidate in candidates:
                setattr(module, candidate["python_name"], dispatcher)


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
    if report_type == "JUNIT":
        for suite in suites:
            report_path(output_dir, suite["fileName"]).write_text(suite_report(suite["fileName"], suite["results"]))
    if report_type == "CTRF":
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
    if report_type == "JEST":
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
    patch_runtime_math_exports(runtime)
    patch_runtime_data_spread(runtime)
    files = flatten_test_values(runtime.gatherTests())
    if args.tests:
        files = merge_test_files(files, gather_selected_test_files(args.tests))
    patch_overload_dispatchers(generated_dir)

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
