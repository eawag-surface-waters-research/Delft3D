import logging
import abc
import os
import datetime
from distutils import file_util, dir_util
from src.Suite.TestSetRunner import TestSetRunner
from src.Config.Type import PathType
from src.Utils.Paths import Paths
from src.Utils.HandlerFactory import HandlerFactory
from src.Utils.Common import attachFileLogger, detachFileLogger
import settings as settings

class ReferenceRunner(TestSetRunner):
   

   def post_process(self, testCaseConfig):

      # Get the reference networkPath of the testcase
      refNetworkPath = None
      creds = None
      for aNetworkPath in testCaseConfig.getLocations():
         if aNetworkPath.getType() == PathType.REFERENCE:
            refNetworkPath = aNetworkPath
            creds = aNetworkPath.getCredentials()

      # Make sure the reference folders are in sync. 
      netloc = Paths().mergeFullPath(refNetworkPath.getRoot(), refNetworkPath.getFrom(), testCaseConfig.getPath())
      if refNetworkPath :                
         HandlerFactory().prepare_upload(testCaseConfig.getAbsoluteTestCaseReferencePath(), netloc, creds)
              
      logging.debug("Overwrite (local) reference")
      if not os.path.exists(os.path.join(testCaseConfig.getAbsoluteTestCasePath(), "_tb3_char.run")):
         raise OSError(-1, "Could not locate _tb3_char.run", testCaseConfig.getAbsoluteTestCasePath())
      if not os.path.exists(testCaseConfig.getAbsoluteTestCaseReferencePath()):
         os.makedirs(testCaseConfig.getAbsoluteTestCaseReferencePath())
      files = self.__findCharacteristicsChangedFiles__(os.path.join(testCaseConfig.getAbsoluteTestCasePath(), "_tb3_char.run"))
      for f in files:
         fl = os.path.join(testCaseConfig.getAbsoluteTestCasePath(), f)
         fr = os.path.join(testCaseConfig.getAbsoluteTestCaseReferencePath(), f)
         if os.path.isfile(fl):
            file_util.copy_file(fl, fr)
         if os.path.isdir(fl):
            dir_util.copy_tree(fl, fr)
      file_util.copy_file(os.path.join(testCaseConfig.getAbsoluteTestCasePath(), "_tb3_char.run"), os.path.join(testCaseConfig.getAbsoluteTestCaseReferencePath(), "_tb3_char.run"))
                   
      # Upload (or prepare upload) of new results of reference run. 
      if refNetworkPath:
         # Build the path to upload to: Root+From+testcasePath:
         # Root: https://repos.deltares.nl/repos/DSCTestbench/references
         # From: trunk/win32_hp
         # testcasePath: e01_d3dflow\f01_general\c03-f34
         logging.debug("\tPreparing reference data for upload...\n")
         HandlerFactory().upload(testCaseConfig.getAbsoluteTestCaseReferencePath(), netloc, creds, settings.autocommit)
      else:
         logging.warning("Could not find reference network path for case to upload to")

   def show_summary (self) :
      logging.info ("SUMMARY of the reference run")
      logging.info ("%-40s (%7s %7s) %-6s" % ("Test case name", "Runtime", "Ratio", "Result"))
      for test_case_config in settings.configs : 
         if len(test_case_config.getErrors()) > 0 : 
            result = "ERROR"
         else : 
            result = "OK"

         logging.info ("%-40s (%7.2f %7.2f) %-6s" % 
                       (test_case_config.getName()[:40], 
                        test_case_config.getRunTime(),
                        test_case_config.getRunTime() / test_case_config.getRefRunTime(), 
                        result))

   # retrieve changed and added files from _tb3_char.run file
   # input: path to _tb3_char.run
   # output: changed and added file names
   def __findCharacteristicsChangedFiles__(self, filename):
      with open(filename) as f:
         lines = []
         for line in f:
            if "Output_" in line:
               _,value = line.split(':')
               lines.append(value.strip())
         return lines

   def add_error_result(self, testCaseConfig) :
      # the result of a reference run is only determined by whether an error was detected or not. 
      # This is reflected in the testCaseConfig (not to be confused with testCase, which also tracks errors).  
      testCaseConfig.setErrors([0])
