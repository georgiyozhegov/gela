import os
import sys
import subprocess

examples = os.listdir("../examples")

FULL = "--full" in sys.argv
NO_COLORS = "--bw" in sys.argv # "bw" stands for black & white
SUCCESSFUL_CODES = [0]
WIDTH = 80

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
    print(f"{GRAY}[code]{RESET} {process.returncode}")
    print(f"{GRAY}[test]{RESET} << {example}")
    print()
    results.append((example, process.returncode))

print(" SUMMARY ".center(WIDTH, "="))
passed = 0
total = len(results)
for example, code in results:
    if code in SUCCESSFUL_CODES:
        passed += 1
        print(f"{GREEN}[passed]{RESET} {example}")
    else:
        print(f"{RED}[failed]{RESET} {example} (exit code: {code})")

print()
print(f"{GRAY}[result]{RESET} {passed}/{total} tests passed")

(status, color) = ("passed", GREEN) if passed == total else ("failed", RED)
print(f"{GRAY}[result]{RESET} {color}{status}{RESET}")
