import os
import sys
import subprocess

examples = os.listdir("../examples")

FULL = "--full" in sys.argv
NO_COLORS = "--bw" in sys.argv  # "bw" stands for black & white
SUCCESSFUL_CODES = [0]
WIDTH = 80
DO_NOT_DETECT_ERR = "--no-err" in sys.argv
SHOULD_FAIL_PREFIX = "err:"

RED = "\033[91m"
GREEN = "\033[92m"
GRAY = "\033[90m"
RESET = "\033[0m"

if NO_COLORS:
    RED = ""
    GREEN = ""
    GRAY = ""
    RESET = ""

results = []

print(" TESTING ".center(WIDTH, "="))
for example in examples:
    print(f"{GRAY}[test]{RESET} >> {example}")
    should_fail = example.startswith(SHOULD_FAIL_PREFIX)
    process = subprocess.run(
        ["cargo", "run", example, "-Awarnings"],
        capture_output=True,
    )
    if FULL:
        if process.returncode not in SUCCESSFUL_CODES and process.stderr:
            print(f"{GRAY}[stderr]{RESET}")
            print(process.stderr.decode())
        if process.stdout:
            print(f"{GRAY}[stdout]{RESET}")
            print(process.stdout.decode())
    if not DO_NOT_DETECT_ERR:
        has_err = "Err" in process.stdout.decode()
    else:
        has_err = False
    print(f"{GRAY}[code]{RESET} {process.returncode}")
    print(f"{GRAY}[has_err]{RESET} {has_err}")
    print(f"{GRAY}[should_fail]{RESET} {should_fail}")
    print(f"{GRAY}[test]{RESET} << {example}")
    print()
    results.append((example, process.returncode, has_err, should_fail))

print(" SUMMARY ".center(WIDTH, "="))
passed = 0
total = len(results)
for example, code, has_err, should_fail in results:
    passed = code in SUCCESSFUL_CODES and not has_err
    if passed:
        if should_fail:
            print(f"{RED}[passed]{RESET} {example} (should_fail)")
        else:
            passed += 1
            print(f"{GREEN}[passed]{RESET} {example}")
    else:
        if should_fail:
            if has_err:
                print(f"{GREEN}[failed]{RESET} {example} (has_err, should_fail)")
            else:
                print(f"{GREEN}[failed]{RESET} {example} (exit code: {code}, should_fail)")
            passed += 1
        else:
            if has_err:
                ignored = ": ignored" if DO_NOT_DETECT_ERR else ""
                print(f"{RED}[failed]{RESET} {example} (has_err{ignored})")
            else:
                print(f"{RED}[failed]{RESET} {example} (exit code: {code})")

print()
if DO_NOT_DETECT_ERR:
    print(f"{GRAY}[note]{RESET} has_err ignored")
print(f"{GRAY}[score]{RESET} {passed}/{total} passed")

(status, color) = ("passed", GREEN) if passed == total else ("failed", RED)
print(f"{GRAY}[result]{RESET} {color}{status}{RESET}")

if status == "passed":
    exit(0)
else:
    exit(1)
