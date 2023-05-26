'''
Description: Network Path Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

from src.Config.Type import PathType
from src.Config.Credentials import Credentials

# Network path configuration
class Location(object):

   # constructor: always initialize variables
   def __init__(self):
      self.__name = ""
      self.__type = PathType.NONE
      self.__credentials = Credentials()
      self.__root = ""
      self.__from = ""
      self.__to = ""
      self.__version = None
   
   # name of path
   def getName(self):
      return self.__name   
   def setName(self, value):
      self.__name = value
   
   # type object definition for network (check, reference or input)
   def getType(self):
      return self.__type
   def setType(self, value):
      self.__type = value

   # credentials object
   def getCredentials(self):
      return self.__credentials
   def setCredentials(self, value):
      self.__credentials = value

   # root of the network path (http(s), net, disk, svn)
   def getRoot(self):
      return self.__root
   def setRoot(self, value):
      self.__root = value
   
   # from subpath including trailing escape character (e.g. /)
   def getFrom(self):
      return self.__from
   def setFrom(self, value):
      self.__from = value
   
   # path the root + from is copied to, sub directory of specified local path
   def getTo(self):
      if (self.__to == ""):
         return self.__from
      return self.__to
   def setTo(self, value):
      self.__to = value
   
   # version of application (mainly used for subversion)
   def getVersion(self):
      return self.__version
   def setVersion(self, value):
      self.__version = value
