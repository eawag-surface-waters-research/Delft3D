'''
Description: Manager for running test case sets
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os
import logging
import sys
import abc
from src.Config.Type import PathType
from src.Config.Mode import Mode
from src.Config.TestCaseConfig import TestCaseFailure
from src.Suite.RunTimeData import RunTimeData
from src.Suite.TestCase import TestCase
from src.Suite.Program import Program
from src.Utils.Paths import Paths
from src.Utils.HandlerFactory import HandlerFactory
from src.Utils.Common import stripEscapeCharacters
from src.Utils.ResolveHandler import Handler, ResolveHandler
import settings as settings


# Run test cases in reference or compare mode 
class TestSetRunner(object):
    __metaclass__ = abc.ABCMeta

    # Prepare/update programs and testcases. Typically, download/update cases and programs from SVN.
    def prepare_cases_and_programs(self):
        try:
            self.__updatePrograms__()
        except Exception:
            if settings.teamcity:
                sys.stderr.write("##teamcity[testStarted name='Update programs']\n")
                sys.stderr.write("##teamcity[testFailed name='Update programs' message='Exception occurred']\n")

        self.__updateTestCases__()

    # run test cases to generate reference data
    def run(self):
        skip_testcase = False  # No check defined still running (so no regression test, test aqainst measurements or other numerical package)
        skip_postprocessing = True  # No check defined still running and do not perform the standard postprocessing

        self.prepare_cases_and_programs()
        n_testcases = len(settings.configs)
        for i_testcase, testCaseConfig in enumerate(settings.configs):

            logging.info("\n================================================================================\n")
            logging.info("Testcase " + str(i_testcase + 1) + " of " + str(n_testcases) + ": " + testCaseConfig.getName() + " ...\n")

            if testCaseConfig.getChecks().__len__() > 0:
                skip_testcase = True
                skip_postprocessing = True
                for fc in testCaseConfig.getChecks():
                    if not fc.ignore():
                        skip_testcase = False
                        skip_postprocessing = False
                if not skip_testcase:
                    if testCaseConfig.getIgnore():
                        skip_testcase = True
                        skip_postprocessing = True

            if settings.teamcity:
                if not skip_testcase:
                    sys.stderr.write("##teamcity[testStarted name='%s']\n" % stripEscapeCharacters(testCaseConfig.getName()))
                else:
                    sys.stderr.write("##teamcity[testIgnored name='%s']\n" % stripEscapeCharacters(testCaseConfig.getName()))

            try:
                # Run testcase
                testcase = TestCase(testCaseConfig)
                if settings.only_post:
                    logging.info("Skipping execution of testcase (postprocess only)...\n")
                else:
                    if not skip_testcase:
                        logging.info("Execute testcase...\n")
                        testcase.run()
                    else:
                        logging.info("Testcase not executed (ignored)...\n")

                # Check for errors during execution of testcase
                if len(testcase.getErrors()) > 0:
                    errstr = "\n"
                    for error in testcase.getErrors():
                        errstr = errstr + str(error) + "\n"
                    logging.error("Errors during testcase: " + errstr)
                    raise TestCaseFailure("Errors during testcase: " + errstr)

                # Postprocessing
                if not skip_postprocessing:
                    logging.info("Postprocessing testcase, checking directories...")
                    if not os.path.exists(testCaseConfig.getAbsoluteTestCasePath()):
                        raise TestCaseFailure("Could not locate case data at: " + str(testCaseConfig.getAbsoluteTestCasePath()))

                    # execute concrete method in subclass
                    self.post_process(testCaseConfig)

            except Exception as e:
                logging.exception(e)
                self.add_error_result(testCaseConfig)
                if settings.teamcity:
                    if not skip_testcase:
                        sys.stderr.write("##teamcity[testFailed name='%s' message='Exception occurred' details='%s']\n" % (
                            stripEscapeCharacters(testCaseConfig.getName()), stripEscapeCharacters(e)))
            finally:
                if settings.teamcity:
                    if not skip_testcase:
                        sys.stderr.write("##teamcity[testFinished name='%s' message='Comparison passed']\n" % stripEscapeCharacters(testCaseConfig.getName()))

                # Debug statements:
                # Something goes wrong on Linux with passing through the output of esm_create to
                # setenv(DIO_SHM_ESM)/esm_delete. It only works fine the first time (and the 6th).
                # It seems that when esm_create is executed for the second testcase, the output in
                # RunTimeData is not overwritten.
                # To tackle this problem:
                # - Only the output of esm_create is being stored (introduced: flag storeOutput="true")
                # - At the end of each testcase (here):
                #   - ALL stored output is dumped to screen
                #   - ALL stored output is removed
                RunTimeData().dump()
                RunTimeData().clean()

        if not skip_postprocessing and not skip_testcase:
            self.show_summary()
        logging.info("\n===  End of tests  ===\n")

    @abc.abstractmethod
    def post_process(self, testCaseConfig):
        logging.debug("Reference directory:%s", testCaseConfig.getAbsoluteTestCaseReferencePath())
        logging.debug("Results   directory:%s", testCaseConfig.getAbsoluteTestCasePath())

    @abc.abstractmethod
    def show_summary(self):
        pass

    @abc.abstractmethod
    def add_error_result(self, testCaseConfig):
        pass

    # Update network programs and initialize the stack
    def __updatePrograms__(self):
        logging.info("Updating programs")
        for pcnf in settings.programs:
            logging.info("Updating program: %s", pcnf.getName())

            # Local path to program root folder
            programLocalPath = None

            # Get the program location
            if len(pcnf.getLocations()) > 0:
                for loc in pcnf.getLocations():
                    # check type of program
                    if (settings.run_mode == Mode.REFERENCE and loc.getType() == PathType.REFERENCE) or (
                            settings.run_mode == Mode.COMPARE and loc.getType() == PathType.CHECK):
                        # if the program is local, use the existing location
                        sourceLocation = Paths().mergeFullPath(loc.getRoot(), loc.getFrom())
                        if Paths().isPath(sourceLocation):
                            absLocation = os.path.abspath(Paths().mergeFullPath(sourceLocation, pcnf.getPath()))
                            if ResolveHandler().detect(absLocation, None) == Handler.PATH:
                                if not os.path.exists(absLocation):
                                    logging.warning("could not yet detect specified program %s", absLocation)
#                                   raise SystemExit("Program does not exist")
                                else:
                                    logging.debug("detected local path for program %s, using %s", pcnf.getName(), absLocation)
                                pcnf.setAbsoluteBinPath(absLocation)
                        # else download it from a remote location
                        else:
                            if loc.getVersion():
                                to = loc.getTo() + "_" + loc.getVersion()
                            else:
                                to = loc.getTo()
                            programLocalPath = Paths().rebuildToLocalPath(os.path.join(settings.local_paths.getEnginesPath(), to))
                            # if the program is remote (network or other) and it does not exist locally, download it
                            if not os.path.exists(programLocalPath):
                                logging.debug("Downloading program, %s from %s", pcnf.getName(), sourceLocation)
                                HandlerFactory().download(sourceLocation, programLocalPath, loc.getCredentials(), loc.getVersion())
                            pcnf.setAbsoluteBinPath(os.path.abspath(Paths().mergeFullPath(programLocalPath, pcnf.getPath())))

            # If a program does not have a network path, and path is not a relative or absolute path, we assume the system can find it
            elif not Paths().isPath(pcnf.getPath()):
                pcnf.setAbsoluteBinPath(pcnf.getPath())
            # Otherwise we need to construct the path from the given information
            else:
                # Construct the absolute binary path for the program
                absbinpath = os.path.abspath(Paths().rebuildToLocalPath(pcnf.getPath()))
                if os.path.exists(absbinpath):
                    pcnf.setAbsoluteBinPath(absbinpath)
                # If the local program does not exist, and a network path is not given we are going to crash
                else:
                    raise SystemExit("Could not find " + pcnf.getName() + " at given location " + absbinpath)
            logging.debug("Binary path for program %s: %s", pcnf.getName(), pcnf.getAbsoluteBinPath())

            # Rebuild the environment variables (specified for this program) to local system variables
            # This is the only place containing all relevant information
            # Do not rebuild the environment variable when it contains a keyword surrounded by "[" and "]",
            # they will be replaced later on
            envparams = pcnf.getEnvironmentVariables()
            for envparam in envparams:
                if envparams[envparam][0] == "path" and str(envparams[envparam][1]).find("[") == -1:
                    pp = Paths().rebuildToLocalPath(envparams[envparam][1])
                    if not Paths().isAbsolute(pp):
                        if programLocalPath:
                            pp = os.path.abspath(Paths().mergeFullPath(programLocalPath, pp))
                        envparams[envparam] = [envparams[envparam][0], pp]
                    else:
                        envparams[envparam] = [envparams[envparam][0], envparams[envparam][1]]

            # Add search paths to the program(configure)
            # Search for (the last) win/lnx/linux in AbsoluteBinPath,
            # add all subdirectories from this level downwards to searchPaths
            # It's quite crude, but this way, all Delft3D programs are able to find each other.
            if pcnf.getAddSearchPaths():
                pltIndex = max(pcnf.getAbsoluteBinPath().rfind("win"), pcnf.getAbsoluteBinPath().rfind("lnx"),
                               pcnf.getAbsoluteBinPath().rfind("linux"), pcnf.getAbsoluteBinPath().rfind("x64"))
                if pltIndex > -1:
                    separatorIndex = max(pcnf.getAbsoluteBinPath()[pltIndex:].find("\\"), pcnf.getAbsoluteBinPath()[pltIndex:].find("/"))
                    pltPath = pcnf.getAbsoluteBinPath()[:pltIndex + separatorIndex]
                    logging.debug("Path: " + pltPath)
                    searchPaths = Paths().findAllSubFolders(pltPath, pcnf.getExcludeSearchPathsContaining())
                else:
                    # No win/lnx/linux found in AbsoluteBinPath:
                    # Just add AbsoluteBinPath and its subFolders
                    searchPaths = Paths().findAllSubFolders(pcnf.getAbsoluteBinPath(), pcnf.getExcludeSearchPathsContaining())
                # Add explicitly named searchPaths, rebuild when needed
                for aPath in pcnf.getSearchPaths():
                    aRebuildPath = Paths().rebuildToLocalPath(aPath)
                    if not Paths().isAbsolute(aRebuildPath) and programLocalPath:
                        aRebuildPath = Paths.mergeFullPath(programLocalPath, aRebuildPath)
                    searchPaths.append(aRebuildPath)
                pcnf.setSearchPaths(searchPaths)

            # Initialize the program
            dummyInstance = Program(pcnf)

    # Update test cases
    def __updateTestCases__(self):
        logging.info("Updating cases")
        for testCaseConfig in settings.configs:
            logging.info("Updating case: %s ", testCaseConfig.getName())
            networkPaths = testCaseConfig.getLocations()
            if len(networkPaths) == 0:
                if settings.teamcity:
                    sys.stderr.write("##teamcity[testStarted name='Update testcases']\n")
                    sys.stderr.write("##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n")
                raise SystemExit("Could not update case %s, no network paths given", testCaseConfig.getName())
                continue
            for nwp in networkPaths:
                if nwp.getRoot() == "" or nwp.getFrom() == "":
                    if settings.teamcity:
                        sys.stderr.write("##teamcity[testStarted name='Update testcases']\n")
                        sys.stderr.write("##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n")
                    raise SystemExit("Could not update case %s, invalid network input path part (root:%s, from:%s) given",
                                     testCaseConfig.getName(), nwp.getRoot(), nwp.getFrom())

                # Build the path to download from: Root+From+testcasePath:
                # Root: https://repos.deltares.nl/repos/DSCTestbench/cases
                # From: trunk/win32_hp
                # testcasePath: e01_d3dflow\f01_general\c03-f34
                netloc = Paths().mergeFullPath(nwp.getRoot(), nwp.getFrom(), testCaseConfig.getPath())
                if Paths().isPath(netloc):
                    netloc = os.path.abspath(netloc)
                # Downloading the testcase input/refdata may fail when it is already present and have to be
                # deleted first. This probably has to do with TortoiseSVNCache, accessing that directory.
                # When trying a second time it normally works. Safe side: try 3 times.
                success = False
                attempts = 0
                while attempts < 3 and not success:
                    attempts += 1
                    try:
                        if nwp.getType() == PathType.INPUT:
                            # Build localPath to download to: casesPath+To+testcasePath
                            localPath = Paths().rebuildToLocalPath(
                                Paths().mergeFullPath(settings.local_paths.getCasesPath(), nwp.getTo(), testCaseConfig.getPath()))
                            if settings.only_post:
                                logging.info("Skipping testcase download (postprocess only)")
                            else:
                                logging.debug("Downloading input of case, %s from %s", localPath, netloc)
                                # Download location on local system is always cleaned before start
                                try:
                                    HandlerFactory().download(netloc, localPath, nwp.getCredentials(), nwp.getVersion())
                                except:
                                    # We need always case input data
                                    logging.warning("Could not download from %s", netloc)
                                    if settings.teamcity:
                                        sys.stderr.write("##teamcity[testStarted name='Update testcases']\n")
                                        sys.stderr.write(
                                            "##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n")
                                    raise SystemExit("Could not download from " + netloc)
                            # Add the local path to the downloaded test case to the actual test case configuration
                            testCaseConfig.setAbsoluteTestCasePath(localPath)
                        if nwp.getType() == PathType.REFERENCE:
                            # Build localPath to download to: referencePath+To+testcasePath
                            localPath = Paths().rebuildToLocalPath(
                                Paths().mergeFullPath(settings.local_paths.getReferencePath(), nwp.getTo(), testCaseConfig.getPath()))
                            # Download location on local system is always cleaned before start
                            if settings.only_post:
                                logging.info("Skipping download of reference results (postprocess only)")
                            else:
                                logging.debug("Downloading reference result, %s from %s", localPath, netloc)
                                try:
                                    HandlerFactory().download(netloc, localPath, nwp.getCredentials(), nwp.getVersion(), autocommit=settings.autocommit)
                                except:
                                    logging.warning("Could not download from %s", netloc)
                                    # If in comparison mode, we need reference data
                            #       if settings.run_mode == Mode.COMPARE:
                            #           if settings.teamcity:
                            #               sys.stderr.write("##teamcity[testStarted name='Update testcases']\n")
                            #               sys.stderr.write(
                            #                   "##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n")
                            #           raise SystemExit("Could not download from " + netloc)
                            # Add the local path to the downloaded test case reference to the actual test case configuration
                            testCaseConfig.setAbsoluteTestCaseReferencePath(localPath)
                        success = True
                    except Exception:
                        logging.warning("Unable to download testcase (attempt %s)", attempts + 1)
                        if attempts >= 3:
                            sys.stderr.write("##teamcity[testStarted name='Update testcases']\n")
                            if settings.teamcity:
                                sys.stderr.write("##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n")
