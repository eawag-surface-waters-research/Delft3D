'''
Description: Local Paths Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

# contains locations to given root directories
class LocalPaths(object):

   def __init__(self):
      self.__casespath = "cases"
      self.__enginespath = "engines"
      self.__referencepath = "references"
   
   def getCasesPath(self):
      return self.__casespath
   def setCasesPath(self, value):
      self.__casespath = value
   
   def getEnginesPath(self):
      return self.__enginespath
   def setEnginesPath(self, value):
      self.__enginespath = value
      
   def setReferencePath(self, value):
      self.__referencepath = value
   def getReferencePath(self):
      return self.__referencepath
