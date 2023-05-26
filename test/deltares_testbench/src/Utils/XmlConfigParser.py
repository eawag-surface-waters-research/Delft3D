'''
Description: Xml Configuration parser
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import copy, re

import sys
import pprint as pp
import traceback
import logging

from lxml import etree
from src.Config.Credentials import Credentials
from src.Config.LocalPaths import LocalPaths
from src.Config.Location import Location
from src.Config.ProgramConfig import ProgramConfig
from src.Config.Type import PathType, FileType, PresenceType
from src.Config.TestCaseConfig import TestCaseConfig
from src.Config.FileCheck import FileCheck
from src.Config.FileCheck import Parameter
from src.Config.FileCheck import SkipLine


def loop(dict, key):
    if (key in dict):
        if type(dict[key]) == list:
            return dict[key]
        elif type(dict[key]) == dict:
            return list(dict[key].values())
        else:
            return [dict[key]]
    else:
        return []


def branch(xmltree, prefix):
    newtree = {"txt": xmltree.text}
    for ch in xmltree.getchildren():
        if (type(ch.tag) == str):
            branchname = ch.tag.replace(prefix, "")
            if branchname not in newtree:
                newtree[branchname] = []
            newtree[branchname].append(branch(ch, prefix))
    for key, val in xmltree.attrib.items():
        newtree[key.replace(prefix, "")] = [val]
    return (newtree)


# Parse the xml configuration file
class XmlConfigParser(object):
    # load the config file
    # input: path (relative or absolute), overwritten roots (if any)
    #        cred: credential from the command line
    # ouput: loglevel, local paths, program configs and test case configs
    def load(self, path, rstr, cred):
        self.__path = path
        self.__rstr = rstr
        self.__validate__()
        self.__initialize__()
        self.__credentials.append(cred)
        return self.__parse__()

    def __maketree__(self, path):
        global schema
        mytree = etree.parse(path)
        mytree.xinclude()
        myroot = mytree.getroot()
        schema = myroot.nsmap[None]
        prefix = "{%s}" % schema
        rtname = myroot.tag.replace(prefix, "")
        mytree = branch(myroot, prefix)
        return (mytree, schema, rtname)

    # validate Xml file format
    def __validate__(self):
        # parser = make_parser()
        # parser.setContentHandler(ContentHandler())
        # parser.parse(self.__path)
        pass

        # initialize defaults

    def __initialize__(self):
        self.__credentials = []
        self.__locations = []
        self.__programconfigs = []
        self.__defaultcases = []

    # parse the xml file
    # output: loglevel, local paths, program configs and test case configs
    def __parse__(self):
        xmldoc, schema, rootname = self.__maketree__(self.__path)
        loglevel = None
        lcpaths = LocalPaths()
        tbconfigs = xmldoc["config"]
        for tbconfig in tbconfigs:
            #
            # The following for-loop should be deleted (when preparations are finished)
            for creds in loop(tbconfig, "credentials"):
                for cred in creds["credential"]:
                    c = Credentials()
                    c.setName(str(cred["name"][0]))
                    c.setUsername(str(cred["username"][0]["txt"]))
                    c.setPassword(str(cred["password"][0]["txt"]))
                    self.__credentials.append(c)
            for lcl in loop(tbconfig, "localPaths"):
                lcpaths.setCasesPath(str(lcl["testCasesDir"][0]["txt"]))
                lcpaths.setEnginesPath(str(lcl["enginesDir"][0]["txt"]))
                lcpaths.setReferencePath(str(lcl["referenceDir"][0]["txt"]))
            for nwps in loop(tbconfig, "locations"):
                for nwp in nwps["location"]:
                    self.__locations.append(self.__fillLocation__(nwp))
        for programs in loop(xmldoc, "programs"):
            if rootname == "deltaresTestbench_v3":
                for program in programs["program"]:
                    programInstance = self.__fillProgram__(program)
                    if programInstance is not None:
                        self.__programconfigs.append(programInstance)
        for defaultCases in loop(xmldoc, "defaultTestCases"):
            for case in defaultCases["testCase"]:
                self.__defaultcases.append(self.__fillCase__(case))
        result = []
        caseNr = -1
        for cases in loop(xmldoc, "testCases"):
            for case in loop(cases, "testCase"):
                caseNr = caseNr + 1
                try:
                    self.__defaultcases.append(self.__fillCase__(case))
                    result.append(self.__fillCase__(case))
                except:
                    logging.warning("Something is wrong with test case: " + str(cases['testCase'][caseNr]['path'][0]['txt']) + ", test case will be ignored")
        return lcpaths, self.__programconfigs, result

    # fill network path (including default)
    # input: XmlElement
    # output: Location
    def __fillLocation__(self, element):
        if not "ref" in element and not "name" in element:
            return None
        if not "ref" in element:
            nwp = Location()
            nwp.setName(str(element["name"][0]))
            if "credential" in element:
                c = self.__getCredentials__(str(element["credential"][0]["ref"][0]))
                if not c:
                    raise XmlError("invalid credential reference value in " + nwp.getName())
                nwp.setCredentials(c)
            # overwrite roots if specified
            newroot = self.__getOverwritePaths__(self.__rstr, nwp.getName(), "root")
            if newroot:
                nwp.setRoot(newroot)
            else:
                nwp.setRoot(str(element["root"][0]["txt"].strip()))
        else:
            nwp = copy.deepcopy(self.__getLocations__(element["ref"][0]))
            if not nwp:
                raise XmlError("invalid network path reference value in " + element["ref"][0])
        if "type" in element:
            if str(element["type"][0]).lower() == "input":
                nwp.setType(PathType.INPUT)
            if str(element["type"][0]).lower() == "check":
                nwp.setType(PathType.CHECK)
            if str(element["type"][0]).lower() == "reference":
                nwp.setType(PathType.REFERENCE)
        if "path" in element:
            #  overwrite paths if specified
            newpath = self.__getOverwritePaths__(self.__rstr, nwp.getName(), "path")
            if newpath:
                nwp.setPath(newpath)
            else:
                nwp.setPath(str(element["path"][0]["txt"]))
        if "version" in element:
            nwp.setVersion(str(element["version"][0]["txt"]))
        if "from" in element:
            # overwrite from if specified
            newfrom = self.__getOverwritePaths__(self.__rstr, nwp.getName(), "from")
            if newfrom:
                nwp.setFrom(newfrom)
            else:
                # Remove leading/trailing slashes; they mess up the building of the path
                nwp.setFrom(str(element["from"][0]["txt"]).strip('/\\'))
        if "to" in element:
            # overwrite to if specified
            newto = self.__getOverwritePaths__(self.__rstr, nwp.getName(), "to")
            if newto:
                nwp.setTo(newto)
            else:
                nwp.setTo(str(element["to"][0]["txt"]))
        return nwp

    # fill program (including defaults)
    # input: XmlElement
    # output: Program
    def __fillProgram__(self, element):
        p = ProgramConfig()
        if "ignore" in element:  # ignore program for this case [RL666]
            if (element["ignore"][0].lower() == "true"):
                return None
        if "name" in element:
            p.setName(str(element["name"][0]))
        if "programStringRemoveQuotes" in element and str(element["programStringRemoveQuotes"][0]).lower() == "true":
            p.setProgramRemoveQuotes(True)
        if "shellStringRemoveQuotes" in element and str(element["shellStringRemoveQuotes"][0]).lower() == "true":
            p.setShellRemoveQuotes(True)
        if "ignoreStandardError" in element and str(element["ignoreStandardError"][0]).lower() == "true":
            p.setIgnoreStandardError(True)
        if "ignoreReturnValue" in element and str(element["ignoreReturnValue"][0]).lower() == "true":
            p.setIgnoreReturnValue(True)
        if "logOutputToFile" in element and str(element["logOutputToFile"][0]).lower() == "true":
            p.setLogOutputToFile(True)
        if "storeOutput" in element and str(element["storeOutput"][0]).lower() == "true":
            p.setStoreOutput(True)
        if "addSearchPaths" in element and str(element["addSearchPaths"][0]).lower() == "true":
            p.setAddSearchPaths(True)
        if "excludeSearchPathsContaining" in element:
            p.setExcludeSearchPathsContaining(str(element["excludeSearchPathsContaining"][0]))
        if "ref" in element:
            p.setName(str(element["ref"][0]))
        if "seq" in element:
            p.setSequence(int(element["seq"][0]))
        if "delay" in element:
            p.setDelay(float(element["delay"][0]))
        if "maxRunTime" in element:
            p.setMaxRunTime(float(element["maxRunTime"][0]["txt"]))
        if "path" in element:
            # overwrite path if specified
            newpath = self.__getOverwritePaths__(self.__rstr, p.getName(), "path")
            if newpath:
                p.setPath(newpath)
            else:
                p.setPath(str(element["path"][0]["txt"]))
        if "workingDirectory" in element:
            p.setWorkingDirectory(str(element["workingDirectory"][0]["txt"]))
            # logging.debug (p.getWorkingDirectory())
        for e in loop(element, "location"):
            nwp = self.__fillLocation__(e)
            if nwp:
                nwpExists = False
                for enp in p.getLocations():
                    if enp.getName() == nwp.getName() and enp.getType() == nwp.getType():
                        enp = nwp
                        nwpExists = True
                if not nwpExists:
                    p.getLocations().append(nwp)
        for el in loop(element, "shell"):
            shellProgram = self.__getPrograms__(str(el["ref"][0]))
            if shellProgram == None:
                raise XmlError("Can not find shell program '" + str(el["ref"][0]) + \
                               "'. Is this program defined in the config.xml file before being used as shell?")
            else:
                p.setShell(shellProgram)
        for el in loop(element, "arguments"):
            for package in el["argument"]:
                p.getArguments().append(str(package["txt"]))
        for el in loop(element, "modules"):
            for module in el["module"]:
                p.getModules().append(str(module["txt"]))
        for el in loop(element, "environments"):
            for env in el["environment"]:
                # append search paths if necessary
                if str(env["name"][0]).lower() == "%path%":
                    p.getSearchPaths().append(str(env["txt"]))
                else:
                    p.getEnvironmentVariables()[str(env["name"][0])] = [str(env["type"][0]), str(env["txt"])]
        return p

    # fill file checks
    # input: XmlElement
    # output: list of FileCheck
    def __fillFileCheck__(self, element):
        defined_file_types = {"ascii": FileType.ASCII,
                              "nefis": FileType.NEFIS,
                              "his": FileType.HIS,
                              "netcdf": FileType.NETCDF,
                              "numbertext": FileType.NUMBERTEXT,
                              "dseriesregression": FileType.DSERIESREGRESSION,
                              "dseriesverification": FileType.DSERIESVERIFICATION,
                              "timeseries_pi": FileType.PITIMESERIES,
                              "timeseries_csv": FileType.CSVTIMESERIES}
        defined_presence_types = {"present": PresenceType.PRESENT,
                              "absent": PresenceType.ABSENT}

        fc = FileCheck()
        skiplines = {}
        skipline = []
        i = -1

        fc.setName(str(element["name"][0]))
        if "ignore" in element and str(element["ignore"][0]).lower() == "true":
            fc.setIgnore(True)
        else:
            if "type" in element:
                typename = str(element["type"][0]).lower()
                if typename in defined_file_types:
                    fc.setType(defined_file_types[typename])
                else:
                    fc.setType(FileType.NONE)

            if "presence" in element:
                presencename = str(element["presence"][0]).lower()
                if presencename in defined_presence_types:
                    fc.setPresence(defined_presence_types[presencename])
                else:
                    fc.setPresence(PresenceType.NONE)

        parameters = {}
        for el in loop(element, "parameters"):
            params = []
            name = ""
            if "name" in el:
                name = str(el["name"][0])
            for param in loop(el,"parameter"):
                # parameters MUST have a name when this is a nefis file, otherwise it is optional.
                # But name is not allowed to be empty!
                if name == "":
                    if fc.getType() == FileType.NEFIS:
                        raise OSError(
                            "In config file, checkfile " + fc.getName() + " has type nefis but field <parameters> has no name attribute")
                    name = "parameters"
                p = Parameter()
                p.setName(str(param["name"][0]))
                if "location" in param:
                    p.setLocation(str(param["location"][0]))
                if "tolerance" in param:
                    p.setTolerance(float(param["tolerance"][0]))
                if "toleranceAbsolute" in param:
                    p.setToleranceAbsolute(float(param["toleranceAbsolute"][0]))
                if "toleranceRelative" in param:
                    p.setToleranceRelative(float(param["toleranceRelative"][0]))
                if "ignore" in param and str(param["ignore"][0]).lower() == "true":
                    p.setIgnore(True)
                params.append(p)
            parameters[name] = params
        for el in loop(element, "skipline"):
            i = i + 1
            p = SkipLine()
            p.setName(str(element["skipline"][i]["txt"]))
            skipline.append(p)
            skiplines['skipline'] = skipline

        fc.setParameters(parameters)
        fc.setSkipLines(skiplines)
        return fc

    # #######################################################################################################################################

    # fill cases (including default)
    # input: XmlElement
    # output: TestCaseConfig
    def __fillCase__(self, element):
        if not "ref" in element:
            c = TestCaseConfig()
            if "maxRunTime" not in element:
                raise XmlError("no maximum run time specified for " + c.getName())
        else:
            c = copy.deepcopy(self.__getCase__(str(element["ref"][0])))
            if "programs" in element:
                c.setProgramConfigs([])
        c.setName(str(element["name"][0]))
        if "ignore" in element:
            if str(element["ignore"][0]).lower() == "true":
                c.setIgnore(True)
        for e in loop(element, "location"):
            nwp = self.__fillLocation__(e)
            if nwp:
                nwpExists = False
                for enp in c.getLocations():
                    if enp.getType() == nwp.getType():
                        enp = nwp
                        nwpExists = True
                if not nwpExists:
                    c.getLocations().append(nwp)
        # add case path
        if "path" in element:
            # overwrite path if specified
            newpath = self.__getOverwritePaths__(self.__rstr, c.getName(), "path")
            if newpath:
                c.setPath(newpath)
            else:
                c.setPath(str(element["path"][0]["txt"]))
        if "maxRunTime" in element:
            c.setMaxRunTime(float(element["maxRunTime"][0]["txt"]))
            for el in element["maxRunTime"]:
                if "OverruleRefMaxRunTime" in el and str(el["OverruleRefMaxRunTime"][0]).lower() == "true":
                    c.setOverruleRefMaxRunTime(True)
        for el in loop(element, "programs"):
            for program in loop(el, "program"):
                programInstance = self.__fillProgram__(program)
                if programInstance is not None:
                    c.getProgramConfigs().append(programInstance)
        for el in loop(element, "errors"):
            for error in el["error"]:
                c.getErrors().append(str(error["txt"]))
        for el in loop(element, "checks"):
            for check in el["file"]:
                c.getChecks().append(self.__fillFileCheck__(check))
        for el in loop(element, "shellarguments"):
            for shellargument in el["shellargument"]:
                c.getShellArguments().append(str(shellargument["txt"]))
        if "shell" in element:
            localShellName = str(element["shell"][0]["txt"])
            c.setShell(self.__getPrograms__(localShellName))
        return c

        # parse roots

    def __getOverwritePaths__(self, rstr, who, what):
        if rstr == None or rstr == "":
            return None
        pathparts = rstr.split(",")
        for part in pathparts:
            actual = PathParts()
            call, path = part.split("=")
            # only check calls for my name (in config)
            name = re.findall(r'(?<=\[)(.*?)(?=\])', call)[0]
            if not who == name:
                continue
            # only check calls with correct name (root, form, to or path)
            if not str(call).startswith(what):
                continue
            # found it
            return path
        # found nothing
        return None

    # get reference value if exists
    def __getCredentials__(self, name):
        for credential in self.__credentials:
            if credential.getName() == name:
                return credential
        return None

    # get reference value if exists
    def __getLocations__(self, name):
        for location in self.__locations:
            if location.getName() == name:
                return location
        return None

    # get reference value if exists
    def __getPrograms__(self, name):
        for program in self.__programconfigs:
            if program.getName() == name:
                return program
        return None

    # get reference value if exists
    def __getCase__(self, name):
        for case in self.__defaultcases:
            if case.getName() == name:
                return case
        return None


# Parsable for paths overrides
class PathParts:
    def __init__(self):
        self.__name = None
        self.__root = None
        self.__from = None
        self.__to = None
        self.__path = None

    def getName(self):
        return self.__name

    def setName(self, name):
        self.__name = name

    def getRoot(self):
        return self.__root

    def setRoot(self, root):
        self.__root = root

    def getFrom(self):
        return self.__from

    def setFrom(self, _from):
        self.__from = _from

    def getTo(self):
        return self.__to

    def setTo(self, to):
        self.__to = to

    def getPath(self):
        return self.__path

    def setPath(self, path):
        self.__path = path


# custom error for Xml handler
class XmlError(Exception):
    def __init__(self, value):
        self.__value = value

    def __str__(self):
        return repr(self.__value)
