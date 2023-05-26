'''
Description: Ascii file comparer, compares numbers (with tolerance) and text
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os
import logging
import sys
if sys.version_info.major == 2:
    from string import maketrans, translate
import numpy as np
import re
from sys import float_info
from src.Config.TestCaseConfig import TestCaseFailure
from src.Config.FileCheck import Parameter
from src.Utils.ComparisonResult import ComparisonResult


# Compares files on all floats that it contains.
class NumberTextComparer:
    __left = 0
    __right = 1
    __lineNumber = [0, 0]
    __columnNumber = [0, 0]
    __words = [[], []]
    __startTag = [None, None]
    __is_number = [0, 0]

    # floats are separated by different characters.
    # Typical nefis.tkl line:
    # -1.154398e-001 -1.061831e-001 -9.516812e-002 -8.049383e-002 -5.755048e-002
    # An easy and fast (http://stackoverflow.com/questions/1059559/python-strings-split-with-multiple-separators)
    # way to filter out the floats:
    # - Replace all separator characters by spaces by using translate
    # - split()
    # separators:
    # - comma (,) must be added for netCDF
    # - don't add the dot (.) because that is the decimal delimiter (ASSUMPTION!)
    __separators = ",()"
    if sys.version_info.major == 2:
        __sepToSpace = maketrans({c: f" {c} " for c in __separators})
    else:
        __sepToSpace = str.maketrans({c: f" {c} " for c in __separators})

    # compare left and right file
    # input: left path, right path, FileCheck instance, logfilename (optional), startTag (optional)
    # output: boolean
    def compare(self, leftpath, rightpath, file_check, testcase_name, startTag=None):
        self.__lineNumber = [0, 0]
        self.__columnNumber = [0, 0]
        self.__words = [[], []]
        self.__startTag = [startTag, startTag]

        min_ref_value = sys.float_info.max
        max_ref_value = -1.0 * min_ref_value

        nCompared = 0
        parameter = Parameter()
        result = ComparisonResult()

        try:
            filename = file_check.getName()
            with open(os.path.join(leftpath, filename), "r") as leftFile:
                with open(os.path.join(rightpath, filename), "r") as rightFile:
                    while True:
                        skiplines = file_check.getSkipLines()
                        leftData = self.__parseText__(leftFile, skiplines, self.__left, self.__is_number)
                        rightData = self.__parseText__(rightFile, skiplines, self.__right, self.__is_number)

                        if leftData is None and rightData is None:
                            # Finished comparing the files
                            logging.debug("Absolute Tolerance: " + str(parameter.getToleranceAbsolute()))
                            logging.debug("Relative Tolerance: " + str(parameter.getToleranceRelative()))
                            logging.debug("Compared %d floats", nCompared)
                            break
                        elif leftData is None and rightData != None:
                            raise TestCaseFailure(
                                "Current file does not match with reference file. Found in current file: %s; value=%s line=%s token=%s" % (
                                    filename, str(rightData[0]), str(rightData[1]), str(rightData[2])))
                        elif leftData != None and rightData == None:
                            raise TestCaseFailure(
                                "Reference file does not match with current file. Found in reference file: %s; value=%s line=%s token=%s" % (
                                    filename, str(leftData[0]), str(leftData[2]), str(leftData[3])))

                        if self.__is_number[0] == self.__is_number[1] == True:
                            # number found
                            # Assuming that leftData is the data from the reference run.
                            max_ref_value = max(leftData[0], max_ref_value)
                            min_ref_value = min(leftData[0], min_ref_value)

                            nCompared += 1
                            diff = abs(leftData[0] - rightData[0])
                            if diff > 2 * float_info.epsilon and diff > result.maxAbsDiff:
                                result.maxAbsDiff = diff
                                result.maxAbsDiffCoordinates = (nCompared,)
                                result.maxAbsDiffValues = (leftData[0], rightData[0])
                        else:
                            #  Word found
                            nCompared += 1
                            if leftData[0] == rightData[0]:
                                # leftData is equal to rightData, read next word/number
                                a = 1
                                continue
                            else:
                                # leftData is different from rightData, exit comparer
                                parameter.setLocation(leftData[2])
                                parameter.setName(leftData[0])
                                result.maxRelDiff = np.nan
                                result.maxAbsDiff = np.nan
                                result.maxAbsDiffCoordinates = (leftData[2], rightData[2])
                                result.result = "NOK"
                                break

            # Make the absolute difference in maxDiff relative, by dividing by (max_ref_value-min_ref_value).
            if result.maxAbsDiff < 2 * sys.float_info.epsilon:
                # No difference found, so relative difference is set to 0.
                result.maxRelDiff = 0.0
            elif max_ref_value - min_ref_value < 2 * sys.float_info.epsilon:
                # Difference found, but denominator will be very small, so set relative difference to maximum.
                result.maxRelDiff = 1.0
            else:
                # Difference found, make the difference relative. Maximise relative difference to 1.0.
                result.maxRelDiff = min(1.0, result.maxAbsDiff / (max_ref_value - min_ref_value))

        except Exception as e:
            logging.exception(e)
            result.error = True
        finally:
            result.isToleranceExceeded(parameter.getToleranceAbsolute(), parameter.getToleranceRelative())

            # Even though only one result is generated, the caller expects a list of results.
        results = [(testcase_name, file_check, parameter, result)]
        return results

    # parse text to find next text/number to compare
    # input: file object(fileHandle), index(ilr) denoting whether this is the left or the right file using the enumerators:
    #        self.__left == 0
    #        self.__right == 1
    # output: None if no float found (anymore in file
    #         [float, lineNumber, columnNumber]
    # Notes: - lines are skipped until startTag is found
    #        - float_left and float_right may be located on different lines (when using ncdump)
    #        - the following self.parameters are lists containing a left and a right value (using ilr to access it):
    #          __linenumber[ilr]  : counts the number of lines read from file
    #          __words[ilr]       : containing the words read from line. This itself is a list
    #          __columnNumber[ilr]: counts the number of words already "popped" from __words[ilr]
    #          __startTag[ilr]    : Initially, startTag[left]==startTag[right]. startTag is set to None when found.
    #                               This may happen a-synchronous!
    #        - __parseText__ used to read a full file, returning a list of number/word (with their position indices).
    #          But this list can be that huge that Python runs out of memory.
    #          Therefore __parseText__ now returns only a single number/word (with its position indices).
    def __parseText__(self, fileHandle, skiplines, ilr, is_number):
        result = None
        while True:
            if len(self.__words[ilr]) == 0:
                self.__columnNumber[ilr] = 0
                line = fileHandle.readline()
                if line == "":
                    # Finished reading file. No float to return
                    break
                self.__lineNumber[ilr] += 1
                # r = [[i for i in d[x]] for x in d.keys()]
                skip_this_line = 0
                for item in skiplines:
                    for skip in skiplines[item]:
                        skip_word = skip.getName()
                        m = re.search(skip_word, line)
                        if m is not None:
                            skip_this_line = 1
                            break
                if skip_this_line == 1:
                    continue
                if self.__startTag[ilr] != None:
                    if str(line).find(self.__startTag[ilr]) > -1:
                        # startTag found: start parsing, set startTag to None
                        self.__startTag[ilr] = None
                    else:
                        # startTag not found yet: skip this line
                        continue
                # Replace the separator characters into spaces and split
                if sys.version_info.major == 2:
                    self.__words[ilr] = translate(line, self.__sepToSpace).split()
                else:
                    self.__words[ilr] = str.translate(line, self.__sepToSpace).split()

                if len(self.__words[ilr]) == 0:
                    # This line does not contain words. Continue with reading next line
                    continue
            aWord = self.__words[ilr].pop(0)
            self.__columnNumber[ilr] += 1
            if str(aWord).lower().find("#inf") > -1 or str(aWord).lower().find("#ind") > -1 or str(aWord).lower().find(
                    "#nan") > -1:
                raise TestCaseFailure("Error during comparing: NaN found (" + str(aWord) + ") in file \"" +
                                      str(fileHandle.name) + "\", line " + str(self.__lineNumber[ilr]) + " word " + str(
                    self.__columnNumber[ilr]))
            try:
                aFloat = float(aWord)
                is_number[ilr] = True
                result = [aFloat, is_number, self.__lineNumber[ilr], self.__columnNumber[ilr]]
                # Yes, we have a float. Break out of while loop
                break
            except:
                # Not able to convert this word to a float. Continue with the next word
                is_number[ilr] = False
                result = [aWord, is_number, self.__lineNumber[ilr], self.__columnNumber[ilr]]
                break
        return result
