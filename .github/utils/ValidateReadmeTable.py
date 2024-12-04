import os
import re
import sys
from typing import Final

CHECK_ID_PATTERN: Final[str] = r"(PWR\d+|PWD\d+|RMK\d+)"
CHECKS_DIR: Final[str] = "Checks/"
README_FILE: Final[str] = "README.md"
TABLE_MARKER: Final[str] = "|"


def extractIdsFromTable(readmeFile: str) -> set[str]:
    with open(readmeFile, "r") as file:
        return {
            match.group(1)
            for line in file.readlines()
            if line.startswith(TABLE_MARKER)
            and (match := re.search(CHECK_ID_PATTERN, line))
        }


def extractIdsFromChecksDirectory(cheksDirectory: str) -> set[str]:
    return {entry.name for entry in os.scandir(cheksDirectory) if entry.is_dir()}


def main():
    tableIds: Final[set[str]] = extractIdsFromTable(README_FILE)
    checksDirIds: Final[set[str]] = extractIdsFromChecksDirectory(CHECKS_DIR)

    missingFromTable: Final[set[str]] = checksDirIds - tableIds
    extraInTable: Final[set[str]] = tableIds - checksDirIds

    if missingFromTable:
        print(
            f"[Error] The following IDs are missing from the README table: "
            f"{', '.join(missingFromTable)}"
        )
    if extraInTable:
        print(
            f"[Error] The following IDs are listed in the README table but do "
            f"not exist in the checks directory: {', '.join(extraInTable)}"
        )

    if missingFromTable or extraInTable:
        sys.exit(1)
    else:
        print("README table is up-to-date!")
        sys.exit(0)


if __name__ == "__main__":
    main()
