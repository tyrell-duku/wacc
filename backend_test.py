#!/usr/bin/python3

import sys
import os
import filecmp


def getAllFiles(dirName):
    rootDirFiles = os.listdir(dirName)
    allFiles = list()

    for file in rootDirFiles:
        fullPath = os.path.join(dirName, file)
        if os.path.isdir(fullPath):
            allFiles = allFiles + getAllFiles(fullPath)
        else:
            allFiles.append(fullPath)

    return allFiles


def main():
    print("--- Running back-end tests ---")
    failed_tests = []
    files = getAllFiles("wacc_expected")
    os.system("make")
    for file in files:
        waccToTest = "wacc_examples/valid" + file[13:-4] + ".wacc"
        os.system("./compile " + waccToTest + "> output.txt")
        passedTest = filecmp.cmp("output.txt", file, False)
        if passedTest:
            print("PASSED: " + waccToTest)
        else:
            print("FAILED: " + waccToTest)
            failed_tests.append(waccToTest)
    print("-------------------------------")
    sys.exit(0)


if __name__ == "__main__":
    main()
