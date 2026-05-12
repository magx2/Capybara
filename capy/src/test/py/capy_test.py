import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import textwrap
import unittest


def run_capy(args):
    classpath = os.environ["CAPY_CLI_CLASSPATH"]
    return subprocess.run(
        ["java", "-cp", classpath, "dev.capylang.Capy", *args],
        text=True,
        capture_output=True,
        check=False,
    )


def run_python(args, cwd=None):
    return subprocess.run(
        [sys.executable, *args],
        cwd=cwd,
        text=True,
        capture_output=True,
        check=False,
    )


class CapyPythonCliTest(unittest.TestCase):
    def temp_project(self):
        return pathlib.Path(tempfile.mkdtemp(prefix="capy-py-test-"))

    def test_compile_generate_python_writes_linked_json_and_runnable_output(self):
        root = self.temp_project()
        source_dir = root / "src"
        generated_dir = root / "generated"
        linked_dir = root / "linked"
        (source_dir / "foo").mkdir(parents=True)
        (source_dir / "foo" / "Main.cfun").write_text(textwrap.dedent("""
            from /capy/lang/Option import { * }
            from /capy/collection/List import { * }

            fun answer(): int = [10, 20, 12].reduce(0, (acc, value) => acc + value)

            fun maybe(flag: bool): Option[String] =
                if flag then Some { "ok" } else None {}

            fun read(flag: bool): String =
                match maybe(flag) with
                case Some { value } -> value
                case None -> "none"
        """))

        result = run_capy([
            "compile-generate",
            "PY",
            "-i", str(source_dir),
            "-o", str(generated_dir),
            "--linked-output", str(linked_dir),
        ])

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), "")
        self.assertTrue((linked_dir / "program.json").exists())
        self.assertTrue((linked_dir / "foo" / "Main.json").exists())
        self.assertTrue((generated_dir / "foo" / "Main.py").exists())
        self.assertTrue((generated_dir / "dev" / "capylang" / "capybara.py").exists())

        python = run_python([
            "-c",
            "import foo.Main as m; print('|'.join([str(m.answer()), m.read(True), m.read(False)]))",
        ], cwd=generated_dir)
        self.assertEqual(python.returncode, 0, python.stderr)
        self.assertEqual(python.stdout.strip(), "42|ok|none")

    def test_generate_python_from_linked_input(self):
        root = self.temp_project()
        source_dir = root / "src"
        linked_dir = root / "linked"
        generated_dir = root / "generated"
        (source_dir / "foo").mkdir(parents=True)
        (source_dir / "foo" / "Main.cfun").write_text("fun answer(): int = 7\n")

        compile_result = run_capy(["compile", "-i", str(source_dir), "-o", str(linked_dir)])
        self.assertEqual(compile_result.returncode, 0, compile_result.stderr)

        generate = run_capy(["generate", "python", "-i", str(linked_dir), "-o", str(generated_dir)])
        self.assertEqual(generate.returncode, 0, generate.stderr)
        self.assertTrue((generated_dir / "foo" / "Main.py").exists())

        python = run_python(["-c", "import foo.Main as m; print(m.answer())"], cwd=generated_dir)
        self.assertEqual(python.returncode, 0, python.stderr)
        self.assertEqual(python.stdout.strip(), "7")

    def test_compile_generate_python_accepts_object_oriented_modules(self):
        root = self.temp_project()
        source_dir = root / "src"
        generated_dir = root / "generated"
        (source_dir / "foo").mkdir(parents=True)
        (source_dir / "foo" / "Main.coo").write_text(textwrap.dedent("""
            class Main {
                def main(args: List[String]): int = args.size()
            }
        """))

        result = run_capy(["compile-generate", "py", "-i", str(source_dir), "-o", str(generated_dir)])

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue((generated_dir / "foo" / "Main.py").exists())

        python = run_python([str(generated_dir / "foo" / "Main.py"), "one", "two"])
        self.assertEqual(python.returncode, 0, python.stderr)
        self.assertEqual(python.stdout.strip(), "2")

    def test_generated_python_output_is_pruned_by_manifest_on_reuse(self):
        root = self.temp_project()
        source_dir = root / "src"
        generated_dir = root / "generated"
        (source_dir / "foo").mkdir(parents=True)
        (generated_dir / "stale").mkdir(parents=True)
        (source_dir / "foo" / "Main.cfun").write_text("fun answer(): int = 1\n")
        (generated_dir / "stale" / "Old.py").write_text("x = 1\n")

        result = run_capy(["compile-generate", "PYTHON", "-i", str(source_dir), "-o", str(generated_dir)])

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue((generated_dir / "foo" / "Main.py").exists())
        self.assertFalse((generated_dir / "stale" / "Old.py").exists())
        manifest = (generated_dir / ".capy-output-manifest").read_text()
        self.assertIn("foo/Main.py", manifest)

    def tearDown(self):
        # Tests use mkdtemp because subprocesses may still have files open briefly on Windows.
        pass
