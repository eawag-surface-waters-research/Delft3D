'''
Description: Program configuration Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

# Program configuration
class ProgramConfig(object):
   
   # constructor: always initialize variables
   def __init__(self):
      self.__shell = None
      self.__shellArguments = ""
      self.__programRemoveQuotes = False
      self.__shellRemoveQuotes = False
      self.__ignoreStandardError = False
      self.__ignoreReturnValue = False
      self.__logOutputToFile = False
      self.__name = ""
      self.__locations = []
      self.__path = ""
      self.__searchPaths = []
      self.__addSearchPaths = False
      self.__excludeSearchPathsContaining = ""
      self.__environmentvars = {}
      self.__modules = []
      self.__environment = None
      self.__absBinPath = ""
      self.__workingDirectory = None
      self.__arguments = []
      self.__sequence = 0
      self.__delay = 0
      self.__maxRunTime = 0
      self.__storeOutput = False
   
   # optional shell statement
   def getShell(self):
      return self.__shell
   def setShell(self, value):
      self.__shell = value
   
   # optional program remove quotes statement
   def getProgramRemoveQuotes(self):
      return self.__programRemoveQuotes
   def setProgramRemoveQuotes(self, value):
      self.__programRemoveQuotes = value
   
   # optional shell remove quotes statement
   def getShellRemoveQuotes(self):
      return self.__shellRemoveQuotes
   def setShellRemoveQuotes(self, value):
      self.__shellRemoveQuotes = value

   # optional ignore standard error
   def getIgnoreStandardError(self):
      return self.__ignoreStandardError
   def setIgnoreStandardError(self, value):
      self.__ignoreStandardError = value

   # optional ignore return value
   def getIgnoreReturnValue(self):
      return self.__ignoreReturnValue
   def setIgnoreReturnValue(self, value):
      self.__ignoreReturnValue = value

   # optional log output to file
   def getLogOutputToFile(self):
      return self.__logOutputToFile
   def setLogOutputToFile(self, value):
      self.__logOutputToFile = value

   # program name
   def getName(self):
      return self.__name
   def setName(self, value):
      self.__name = value

   # network paths for program (reference and current)
   def getLocations(self):
      return self.__locations
   def setLocations(self, value):
      self.__locations = value

   # path for program
   def getPath(self):
      return self.__path
   def setPath(self, value):
      self.__path = value

   # get absolute search paths
   def getSearchPaths(self):
      return self.__searchPaths
   def setSearchPaths(self, value):
      self.__searchPaths = value

   def getAddSearchPaths(self):
      return self.__addSearchPaths
   def setAddSearchPaths(self,value):
      self.__addSearchPaths = value

   def getExcludeSearchPathsContaining(self):
      return self.__excludeSearchPathsContaining
   def setExcludeSearchPathsContaining(self,value):
      self.__excludeSearchPathsContaining = value

   # get the environment variables for the program
   def getEnvironmentVariables(self):
      return self.__environmentvars
   def setEnvironmentVariables(self, value):
      self.__environmentvars = value
 
   # get the operating environment
   def getEnvironment(self):
      return self.__environment
   def setEnvironment(self, value):
      self.__environment = value

  # get the modules for the environment
   def getModules(self):
      return self.__modules
   def setModules(self, value):
      self.__modules = value

   # absolute system path to binary
   def getAbsoluteBinPath(self):
      return self.__absBinPath
   def setAbsoluteBinPath(self, value):
      self.__absBinPath = value
      
   # set the working directory
   def getWorkingDirectory(self):
      return self.__workingDirectory
   def setWorkingDirectory(self, value):
      self.__workingDirectory = value
 
   # arguments to pass to program
   def getArguments(self):
      return self.__arguments
   def setArguments(self, value):
      self.__arguments = value
   
   # sequence group identifier to pass to program
   def getSequence(self):
      return self.__sequence
   def setSequence(self, value):
      self.__sequence = value
   
   # given delay before start
   def getDelay(self):
      return self.__delay
   def setDelay(self, value):
      self.__delay = value

   # given maximum runtime
   def getMaxRunTime(self):
      return self.__maxRunTime
   def setMaxRunTime(self, value):
      self.__maxRunTime = value

   # Flag that output of this program should be stored by RunTimeData or not
   def getStoreOutput(self):
      return self.__storeOutput
   def setStoreOutput(self, value):
      self.__storeOutput = value

   # Single string of Shell Arguments to be passed on to the shell
   def getShellArguments(self):
      return self.__shellArguments
   def setShellArguments(self, value):
      self.__shellArguments = value
