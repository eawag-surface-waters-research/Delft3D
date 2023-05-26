'''
Description: Test Config Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''


# Test case configuration
class TestCaseConfig(object):
    # constructor: initialize variables
    def __init__(self):
        self.__name = ""
        self.__path = ""
        self.__locations = []
        self.__searchPaths = []
        self.__environmentvars = {}
        self.__shell = None
        self.__shellArguments = []
        self.__programConfigs = []
        self.__errors = []
        self.__checks = []
        self.__maxRunTime = 0
        self.__refRunTime = -1
        self.__runTime = 0
        self.__OverruleRefMaxRunTime = False
        self.__absTestCasePath = ""
        self.__absTestCaseRefPath = ""
        self.__maxdiff = -1
        self.__runfile = None
        self.__ignore = False

    # name of the test case
    def getName(self):
        return self.__name

    def setName(self, value):
        self.__name = value

    # relative paths for test case
    def getPath(self):
        return self.__path

    def setPath(self, value):
        self.__path = value

    # network paths for test case (reference and input)
    def getLocations(self):
        return self.__locations

    def setLocations(self, value):
        self.__locations = value

    # get absolute search paths
    def getSearchPaths(self):
        return self.__searchPaths

    def setSearchPaths(self, value):
        self.__searchPaths = value

    # get the environment variables for the program
    def getEnvironmentVariables(self):
        return self.__environmentvars

    def setEnvironmentVariables(self, value):
        self.__environmentvars = value

    # absolute file system path to test case
    def getAbsoluteTestCasePath(self):
        return self.__absTestCasePath

    def setAbsoluteTestCasePath(self, value):
        self.__absTestCasePath = value

    # absolute file system path to reference data for test case
    def getAbsoluteTestCaseReferencePath(self):
        return self.__absTestCaseRefPath

    def setAbsoluteTestCaseReferencePath(self, value):
        self.__absTestCaseRefPath = value

    # maximum run time of the test case
    def getMaxRunTime(self):
        return self.__maxRunTime

    def setMaxRunTime(self, value):
        self.__maxRunTime = value

    # maximum run time of the test case as specified in the reference _tb3_char.run file
    def getRefRunTime(self):
        return self.__refRunTime

    def setRefRunTime(self, value):
        self.__refRunTime = value

    # actual run time of the test case
    def getRunTime(self):
        return self.__runTime

    def setRunTime(self, value):
        self.__runTime = value

    # Default: maxRunTime in the config.xml is overruled by maxRunTime in reference/_tb3_char.run
    # This can be overruled using this flag
    def getOverruleRefMaxRunTime(self):
        return self.__OverruleRefMaxRunTime

    def setOverruleRefMaxRunTime(self, value):
        self.__OverruleRefMaxRunTime = value

    # programs (list) used in test
    def getProgramConfigs(self):
        return self.__programConfigs

    def setProgramConfigs(self, value):
        self.__programConfigs = value

    # expected errors in test
    def getErrors(self):
        return self.__errors

    def setErrors(self, value):
        self.__errors = value

    # specific shell for this case configuration
    def getShell(self):
        return self.__shell

    def setShell(self, value):
        self.__shell = value

    # arguments passed to the shell in which the program is running
    def getShellArguments(self):
        return self.__shellArguments

    def setShellArguments(self, value):
        self.__shellArguments = value

    # files to check
    def getChecks(self):
        return self.__checks

    def setChecks(self, value):
        self.__checks = value

    def getMaxDiff(self):
        return self.__maxdiff

    def setMaxDiff(self, value):
        self.__maxdiff = value

    # Name (including path) of runfile
    def getRunFileName(self):
        return self.__runfile

    def setRunFileName(self, value):
        self.__runfile = value

    # files to check
    def getPrograms(self):
        return self.__programConfigs

    # ignore tescase
    def getIgnore(self):
        return self.__ignore

    def setIgnore(self, value):
        self.__ignore = value


# Custom error for test failures
class TestCaseFailure(Exception):
    def __init__(self, value):
        self.__value = value

    def __str__(self):
        return repr(self.__value)
