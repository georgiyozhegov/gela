import os
import sys
import subprocess as sp
from dataclasses import dataclass

EXAMPLES_PATH = "../examples"
EXEC_PATH = "./target/debug/gelac"
ERR_PREFIX = "err:"

NO_COLOR = "--no-color" in sys.argv
IGNORE_ERR = "--ignore-err" in sys.argv
SHOW_OUTPUT = "--show-output" in sys.argv

RESET = "\033[0m"
red = lambda text: f"\033[91m{text}{RESET}"
green = lambda text: f"\033[92m{text}{RESET}"
gray = lambda text: f"\033[90m{text}{RESET}"

if NO_COLOR:
    red = lambda text: text
    green = lambda text: text
    gray = lambda text: text


def hprint(name):
    size = os.get_terminal_size()
    print(f" {name} ".center(size.columns, "="))


def fprint(color, name, value=""):
    print(f"{color('[' + name + ']')}{'' if value == '' else ' '}{value}")


@dataclass
class TestResult:
    example: str
    stdout: str
    stderr: str
    should_fail: bool
    has_err: bool
    code: int


def test(example):
    should_fail = example.startswith(ERR_PREFIX)
    process = sp.run(
        [EXEC_PATH, example],
        capture_output=True,
    )
    stdout = process.stdout.decode()
    stderr = process.stderr.decode()
    has_err = "Err" in stdout
    return TestResult(example, stdout, stderr, should_fail, has_err, process.returncode)


examples = sorted(os.listdir(EXAMPLES_PATH), key=lambda file: file.lstrip(ERR_PREFIX))


def main():
    process = sp.run(
        ["cargo", "build"],
        capture_output=True,
    )
    code = process.returncode
    if code != 0:
        fprint(gray, "build", f"{red('failed')} (exit code: {code})")
        exit(1)
    stderr = process.stderr.decode()
    warnings = stderr.count("warning:")

    hprint("TESTING")
    results = []
    for example in examples:
        fprint(gray, "test", f">> {example}")
        result = test(example)
        results.append(result)
        if SHOW_OUTPUT:
            fprint(gray, "stdout", "no output" if result.stdout == "" else "")
            if result.stdout != "":
                print(result.stdout, end="" if result.stdout.endswith("\n") else "\n")
            fprint(gray, "stderr", "no output" if result.stderr == "" else "")
            if result.stderr != "":
                print(result.stderr, end="" if result.stderr.endswith("\n") else "\n")
        fprint(gray, "should_fail", result.should_fail)
        fprint(gray, "has_err", result.has_err)
        fprint(gray, "code", result.code)
        fprint(gray, "test", f"<< {example}")
        print()

    hprint("SUMMARY")
    passed = 0
    total = len(results)
    for result in results:
        if result.should_fail:
            if result.has_err or result.code != 0:
                passed += 1
                if result.code != 0:
                    fprint(
                        green,
                        "failed",
                        f"{result.example} (should_fail, exit code: {result.code})",
                    )
                else:
                    fprint(green, "failed", f"{result.example} (should_fail, has_err)")
            else:
                fprint(red, "passed", f"{result.example} (should_fail)")
        else:
            if not result.has_err and result.code == 0:
                passed += 1
                fprint(green, "passed", result.example)
            else:
                if result.code != 0:
                    fprint(
                        red, "failed", f"{result.example} (exit code: {result.code})"
                    )
                else:
                    fprint(red, "failed", f"{result.example} (has_err)")

    print()
    warnings = f" (found {warnings} warnings)" if warnings != 0 else ""
    fprint(gray, "build", f"ok{warnings}")
    score = (passed / total) * 100
    fprint(gray, "score", f"{passed}/{total}, {score:.2f}% passed")
    color, message = (green, "passed") if passed == total else (red, "failed")
    fprint(gray, "result", color(message))
    if message != "passed":
        exit(1)


if __name__ == "__main__":
    main()
