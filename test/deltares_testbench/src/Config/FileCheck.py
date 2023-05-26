'''
Description: File Check Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

from src.Config.Type import FileType, PresenceType

# Engine configuration
class FileCheck(object):
   
   # constructor: always initialize variables
   def __init__(self):
      self.__name = ""
      self.__type = FileType.NONE
      self.__parameters = {}
      self.__skiplines = {}
      self.__ignore = False
      self.__presence = PresenceType.NONE
      
   def getName(self):
      return self.__name
   def setName(self, value):
      self.__name = value

   def getType(self):
      return self.__type
   def setType(self, value):
      self.__type = value
   
   def getParameters(self):
      return self.__parameters
   def setParameters(self, value):
      self.__parameters = value

   def getSkipLines(self):
      return self.__skiplines
   def setSkipLines(self, value):
      self.__skiplines = value

   def setIgnore(self, value):
      self.__ignore = value
   def ignore(self):
      return self.__ignore

   def getPresence(self):
      return self.__presence
   def setPresence(self, value):
      self.__presence = value

# parameter storage object
class Parameter(object):
   
   def __init__(self):
      self.__name = ""
      self.__location = None
      self.__toleranceabs = None
      self.__tolerancerel = None
      self.__ignore = False
      
   def getName(self):
      return self.__name
   def setName(self, value):
      self.__name = value
   
   def getLocation(self):
      return self.__location
   def setLocation(self, value):
      self.__location = value
   
   def setTolerance(self, value):
      self.__toleranceabs = value
      self.__tolerancerel = value

   def getToleranceAbsolute(self):
      return self.__toleranceabs
   def setToleranceAbsolute(self, value):
      self.__toleranceabs = value

   def getToleranceRelative(self):
      return self.__tolerancerel
   def setToleranceRelative(self, value):
      self.__tolerancerel = value
   def setIgnore(self, value):
      self.__ignore = value
   def ignore(self):
      return self.__ignore
   def presence(self):
      return self.__presence
# parameter storage object
class SkipLine(object):

   def __init__(self):
      self.__name = ""

   def getName(self):
      return self.__name

   def setName(self, value):
      self.__name = value
