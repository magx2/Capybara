import os
import importlib
from pathlib import Path

parser_provider = importlib.import_module("dev.capylang.compiler.parser.CapybaraParserProvider")


def source_kind(name):
    return {"__type": name}


def raw_module(name, path, source, kind):
    return {
        "__type": "RawModule",
        "name": name,
        "path": path,
        "input": source,
        "sourceKind": source_kind(kind),
    }


def assert_equal(actual, expected, label):
    if actual != expected:
        raise AssertionError(f"{label}: expected {expected!r}, got {actual!r}")


def generated_parser():
    parser = getattr(parser_provider, "parser", None)
    if parser is not None:
        return parser

    parser_functions = [
        getattr(parser_provider, name)
        for name in dir(parser_provider)
        if name.startswith("parser__") and callable(getattr(parser_provider, name))
    ]
    if len(parser_functions) != 1:
        raise AssertionError(f"expected one generated parser function, got {len(parser_functions)}")
    return parser_functions[0]


def main():
    parser_impl = generated_parser()().unsafe_run()

    parsed = parser_impl.parse(
        [
            raw_module(
                "ImportedFunctional",
                "dev/capylang/smoke",
                'from /capy/lang/String import { String }\n'
                'data User { name: String }\n'
                'fun greet(user: User): String = "hello {user.name}"\n',
                "FUNCTIONAL",
            ),
            raw_module(
                "Clock",
                "dev/capylang/smoke",
                "interface Clock {\n"
                "  def now(): int\n"
                "}\n"
                "class FixedClock(value: int): Clock {\n"
                '  field name: String = "fixed"\n'
                "  def now(): int = value\n"
                "}\n",
                "OBJECT_ORIENTED",
            ),
        ]
    )

    assert_equal(parsed["__type"], "ParsedProgram", "parsed type")
    assert_equal(len(parsed["modules"]), 2, "parsed module count")
    assert_equal(parsed["modules"][0]["imports"][0]["modulePath"], "/capy/lang/String", "import module")
    assert_equal(parsed["modules"][0]["definitions"][0]["__type"], "DataDeclaration", "first functional definition")
    assert_equal(parsed["modules"][1]["objectOriented"]["interfaces"][0]["name"], "Clock", "object interface")
    assert_equal(parsed["modules"][1]["objectOriented"]["classes"][0]["name"], "FixedClock", "object class")

    output = os.environ.get("PYTHON_NATIVE_PARSER_SMOKE_OUTPUT")
    if output:
        Path(output, "native-parser-smoke.txt").write_text("ok\n", encoding="utf-8")


if __name__ == "__main__":
    main()
