from test_oracle import test_with_oracle
from pathlib import Path
import sys


def main() -> None:
    all_files = Path("examples").iterdir()
    for file in all_files:
        test_with_oracle(file.absolute().resolve())


if __name__ == "__main__":
    main()
    sys.exit(0)
