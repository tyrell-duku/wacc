#!/usr/bin/python3

import sys
import os
import filecmp
from colorama import Fore, Back, Style


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


def test_report(msg, colour, num, total):
    print(colour + Style.BRIGHT + msg + " " + str(num) + "/" + str(total))


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
            print(Fore.GREEN + Style.BRIGHT + "PASSED: " + waccToTest)
        else:
            print(Fore.RED + Style.BRIGHT + "FAILED: " + waccToTest)
            failed_tests.append(waccToTest)
    print(Style.RESET_ALL + "-------------------------------")
    numTotal = len(files)
    numFailed = len(failed_tests)
    numPassed = numTotal - numFailed

    test_report("PASSED", Fore.GREEN, numPassed, numTotal)

    if numFailed > 0:
        test_report("FAILURE", Fore.RED, numFailed, numTotal)


if __name__ == "__main__":
    main()
