'''
Description: Raw File Content comparer
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os, logging
from src.Config.FileCheck import Parameter
from src.Config.TestCaseConfig import TestCaseFailure
from src.Utils.ComparisonResult import ComparisonResult
import filecmp

# compare files on content equality
class ContentComparer(object):
   
   # compare left and right file
   # input: left path, right path, FileCheck instance
   # output: list of (file_check, parameter, ResultComparison) tuples
   def compare(self, leftpath, rightpath, file_check, testcase_name) :
      
      logging.debug ("Checking: " + file_check.getName())

      parameter = Parameter()
      comparison_result = ComparisonResult()

      filename = file_check.getName()
      leftfilename = os.path.join(leftpath, filename)
      rightfilename = os.path.join(rightpath, filename)
      try : 
         equal = filecmp.cmp (leftfilename, rightfilename)
         if equal : 
            comparison_result.passed = True
            comparison_result.result = "OK" 
         else : 
            comparison_result.passed = False
            comparison_result.maxAbsDiff = 1.0
            comparison_result.maxRelDiff = 1.0
            comparison_result.result = "NOK"
      except Exception as e :
         comparison_result.passed = False
         comparison_result.error = True
         comparison_result.result = "ERROR"

      return [ (testcase_name, file_check, parameter, comparison_result) ]

