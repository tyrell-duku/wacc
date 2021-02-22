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
            allFiles.append((fullPath, file))

    return allFiles


def test_report(msg, colour, num, total):
    print(colour + Style.BRIGHT + msg + " " + str(num) + "/" + str(total))


def check_error_code(path):
    os.system("echo $? > exitcode.txt")
    exitCodeExpectedFile = "wacc_exitcodes" + path[13:]
    return filecmp.cmp("exitcode.txt", exitCodeExpectedFile, False)

def check_expected_output(waccToTest, path, file):
    os.system("./compile " + waccToTest)
    fileName = file[:-4]
    os.system(
        "arm-linux-gnueabi-gcc -o " +
        fileName +
        " -mcpu=arm1176jzf-s -mtune=arm1176jzf-s " +
        fileName +
        ".s")
    os.system(
        "qemu-arm -L /usr/arm-linux-gnueabi/ " +
        fileName +
        " > output.txt")
    return (fileName, filecmp.cmp("output.txt", path, False))


def test_all_files(files):
    failed_tests = []
    for (path, file) in files:
        waccToTest = "wacc_examples/valid" + path[13:-4] + ".wacc"

        (fileName, expected) = check_expected_output(waccToTest, path, file)
        errorCodeCheck = check_error_code(path)

        passedTest = expected and errorCodeCheck

        os.system("rm " + fileName + " " + fileName + ".s")
        if passedTest:
            print(Fore.GREEN + Style.BRIGHT + "PASSED: " + waccToTest)
        else:
            print(Fore.RED + Style.BRIGHT + "FAILED: " + waccToTest)
            failed_tests.append(waccToTest)

    os.system("rm exitcode.txt output.txt")
    return failed_tests


def main():
    print("--- Running back-end tests ---")

    files = getAllFiles("wacc_expected")
    os.system("make")
    failed_tests = test_all_files(files)

    print(Style.RESET_ALL + "-------------------------------")
    numTotal = len(files)
    numFailed = len(failed_tests)
    numPassed = numTotal - numFailed

    test_report("PASSED", Fore.GREEN, numPassed, numTotal)

    if numFailed > 0:
        test_report("FAILURE", Fore.RED, numFailed, numTotal)


if __name__ == "__main__":
    main()
