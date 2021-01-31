#!/usr/bin/python3

import sys
from colorama import Fore, Back, Style


def main():
    print("--- Running commit-msg hook ---")
    new_commit_message = []
    with open(sys.argv[1], "r") as fp:
        lines = fp.readlines()
        if not check_convention(lines[0]):
            print(Fore.RED + Style.BRIGHT +
                  "fatal: commit message does not follow convention." + Style.RESET_ALL)
            sys.exit(1)
        for line in lines:
            if line == lines[-1]:
                new_commit_message.append(co_authors(line))
            else:
                new_commit_message.append(line)

    with open(sys.argv[1], "w") as fp:
        fp.writelines(new_commit_message)
    sys.exit(0)


def check_convention(first_line):
    # located in the README
    types = ["feat", "fix", "style", "refactor", "test", "docs", "maintenance"]
    return any([first_line.startswith(x) for x in types])


def co_authors(last_line):
    authors = {"A": "Co-authored-by: R, Alex <alex.richardson19@imperial.ac.uk>\n",
               "D": "Co-authored-by: S, Devam <devam.savjani19@imperial.ac.uk>\n",
               "J": "Co-authored-by: P, Jaimi <jaimi.patel19@imperial.ac.uk>\n",
               "T": "Co-authored-by: D, Tyrell <tyrell.duku19@imperial.ac.uk>\n"}
    line = last_line.strip("\n").split(" ")
    if line[0] != "W/":
        return last_line
    print(Fore.GREEN + Style.BRIGHT +
          "Replacing name(s) with co-author(s)" + Style.RESET_ALL)
    for i in range(1, len(line)):
        line[i] = authors[line[i]]
    return "\n".join(line[1:])


if __name__ == "__main__":
    main()
