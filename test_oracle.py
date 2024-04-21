import subprocess
import tempfile
import sys
from pathlib import Path
from typing import List

SAT = "s SATISFIABLE"
UNSAT = "s UNSATISFIABLE"


class Bcolors:
    OKGREEN = "\033[92m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"


def count_vars(lines: List[str]) -> int:
    res = 0
    for line in lines:
        if line.startswith("v"):
            vars = line.strip().split()[1:]
            res += len(vars)
            if vars[-1] == "0":
                res -= 1
    return res


def test_with_oracle(in_file: Path) -> None:
    file_path = Path(in_file).absolute().resolve()

    result = subprocess.run(
        [
            "dotnet",
            "run",
            "--project",
            "SomeSAT/SomeSAT.fsproj",
            file_path,
        ],
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        print("Error occurred:")
        print(result.stderr)
        sys.exit(1)

    with tempfile.NamedTemporaryFile(mode="w+") as tmp:
        tmp.write(result.stdout)
        tmp.seek(0)
        status = tmp.readline().strip()

        if status == UNSAT:
            picosat = subprocess.run(
                ["picosat", file_path],
                capture_output=True,
                text=True,
            )

            if picosat.stdout.strip() == UNSAT:
                print(f"{in_file.name}: {Bcolors.OKGREEN}PASSED{Bcolors.ENDC}")
            else:
                print(f"{in_file.name}: {Bcolors.FAIL}FAILED{Bcolors.ENDC}")
                sys.exit(1)
        else:
            model = tmp.readlines()
            additional_clauses = count_vars(model)

            with open(file_path, "r") as src, tempfile.NamedTemporaryFile(
                mode="w+"
            ) as buff:
                for src_line in src:
                    if src_line.startswith("c") or src_line.startswith("s"):
                        continue
                    if src_line.startswith("p"):
                        ltw = src_line.strip().split()
                        ltw[3] = str(int(ltw[3]) + additional_clauses)
                        buff.write(" ".join(ltw))
                        buff.write("\n")
                    else:
                        buff.write(src_line)

                for ans_line in model:
                    if ans_line.startswith("v"):
                        vars = ans_line.strip().split()[1:]

                        if vars[-1] == "0":
                            vars = vars[:-1]

                        for var in vars:
                            buff.write(var + " " + "0\n")

                buff.write("\n")
                buff.flush()

                picosat = subprocess.run(
                    ["picosat", Path(buff.name).absolute().resolve()],
                    capture_output=True,
                    text=True,
                )

                if picosat.stdout.split("\n")[0].strip() == SAT:
                    print(f"{in_file.name}: {Bcolors.OKGREEN}PASSED{Bcolors.ENDC}")
                else:
                    print(f"{in_file.name}: {Bcolors.FAIL}FAILED{Bcolors.ENDC}")
                    sys.exit(1)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 test.py <filePath>")
        sys.exit(1)
    test_with_oracle(Path(sys.argv[1]))
    sys.exit(0)
