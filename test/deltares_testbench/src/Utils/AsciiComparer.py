'''
Description: Ascii file comparer
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os
import logging
import sys
if sys.version_info.major == 2:
    from string import maketrans, translate
import array
from sys import float_info
from src.Config.TestCaseConfig import TestCaseFailure
from src.Config.FileCheck import Parameter
from src.Utils.ComparisonResult import ComparisonResult


# Compares files on all floats that it contains.
class AsciiComparer:
    __left = 0
    __right = 1
    __lineNumber = [0, 0]
    __columnNumber = [0, 0]
    __words = [[], []]
    __startTag = [None, None]

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
    def compare(self, leftpath, rightpath, file_check, testcase_name, startTag=None, startColumn=0, endColumn=0):
        self.__lineNumber = [0, 0]
        self.__columnNumber = [0, 0]
        self.__words = [[], []]
        self.__startTag = [startTag, startTag]

        results = []
        local_error = False

        nCompared = 0
        for parameters in file_check.getParameters().values():
            for parameter in parameters:
                logging.debug("Checking parameter: " + str(parameter.getName()))
                result = ComparisonResult(error=local_error)

                min_ref_value = sys.float_info.max
                max_ref_value = -1.0 * min_ref_value

                result.lineNumber = sys.maxsize

                try:
                    filename = file_check.getName()
                    with open(os.path.join(leftpath, filename), "r") as leftFile:
                        with open(os.path.join(rightpath, filename), "r") as rightFile:
                            while True:
                                leftData = self.__parseText__(leftFile, self.__left)
                                rightData = self.__parseText__(rightFile, self.__right)

                                if leftData is None and rightData is None:
                                    # Finished comparing the files
                                    logging.debug("Absolute Tolerance: " + str(parameter.getToleranceAbsolute()))
                                    logging.debug("Relative Tolerance: " + str(parameter.getToleranceRelative()))
                                    logging.debug("Compared %d floats", nCompared)
                                    break
                                elif leftData is None and rightData is not None:
                                    raise TestCaseFailure("Still floats found in right file after comparing %i floats in both files: value=%f line=%i column=%i" % (nCompared, rightData[0], rightData[1], rightData[2]))
                                elif leftData is not None and rightData is None:
                                    raise TestCaseFailure("Still floats found in left file after comparing %i floats in both files: value=%f line=%i column=%i" % (nCompared, leftData[0], leftData[1], leftData[2]))

                                if startColumn > 0 and endColumn > 0 and (leftData[2] < startColumn or leftData[2] > endColumn):
                                    continue
                                result.lineNumber = min(result.lineNumber, leftData[1])

                                # Assuming that leftData is the data from the reference run.
                                max_ref_value = max(leftData[0], max_ref_value)
                                min_ref_value = min(leftData[0], min_ref_value)

                                nCompared += 1
                                diff = abs(leftData[0] - rightData[0])
                                if diff > 2 * float_info.epsilon and diff > result.maxAbsDiff:
                                    result.maxAbsDiff = diff
                                    result.maxAbsDiffCoordinates = (nCompared,)
                                    result.maxAbsDiffValues = (leftData[0], rightData[0])
                                    result.columnNumber = leftData[2]

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

                result.isToleranceExceeded(parameter.getToleranceAbsolute(), parameter.getToleranceRelative())

        # Even though only one result is generated, the caller expects a list of results.
                results.append((testcase_name, file_check, parameter, result))
        return results

    # parse text to find next float to compare
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
    #        - __parseText__ used to read a full file, returning a list of reals (with their position indices).
    #          But this list can be that huge that Python runs out of memory.
    #          Therefore __parseText__ now returns only a single real (with its position indices).
    def __parseText__(self, fileHandle, ilr):
        result = None
        while True:
            if len(self.__words[ilr]) == 0:
                self.__columnNumber[ilr] = 0
                line = fileHandle.readline()
                if line == "":
                    # Finished reading file. No float to return
                    break
                self.__lineNumber[ilr] += 1
                if self.__startTag[ilr] is not None:
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
            if str(aWord).lower().find("#inf") > -1 or str(aWord).lower().find("#ind") > -1 or str(aWord).lower().find("#nan") > -1:
                raise TestCaseFailure("Error during comparing: NaN found (" + str(aWord) + ") in file \"" +
                                      str(fileHandle.name) + "\", line " + str(self.__lineNumber[ilr]) + " word " + str(self.__columnNumber[ilr]))
            try:
                aFloat = float(aWord)
                result = [aFloat, self.__lineNumber[ilr], self.__columnNumber[ilr]]
                # Yes, we have a float. Break out of while loop
                break
            except:
                # Not able to convert this word to a float. Continue with the next word
                continue
        return result
