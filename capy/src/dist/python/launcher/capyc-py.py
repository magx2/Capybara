#!/usr/bin/env python3
import sys

from capy.test.CapyTestRuntime import _unsafe_run
from dev.capylang.cli.Capy import main


def data_field(value, name, fallback=None):
    if isinstance(value, dict):
        return value.get(name, fallback)
    return getattr(value, name, fallback)


def exit_code_value(value):
    raw = data_field(value, "value", value)
    try:
        return int(raw)
    except (TypeError, ValueError):
        return 1


def program_exit_code(program):
    result = _unsafe_run(program)
    result_type = str(data_field(result, "__type", ""))
    if result_type == "Failed" or result_type.endswith(".Failed") or result_type.endswith("/Failed"):
        return exit_code_value(data_field(result, "exit_code", data_field(result, "exitCode", data_field(result, "value", 1))))
    return 0


sys.exit(program_exit_code(main(sys.argv[1:])))
