"""
Description: Manager for running test case sets
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import os
import sys
from abc import ABC, abstractmethod

from settings import TestRunSettings
from src.config.location import Location
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_failure import TestCaseFailure
from src.config.types.handler_type import HandlerType
from src.config.types.mode_type import ModeType
from src.config.types.path_type import PathType
from src.suite.program import Program
from src.suite.run_time_data import RunTimeData
from src.suite.test_case import TestCase
from src.utils.common import stripEscapeCharacters
from src.utils.handlers.handler_factory import HandlerFactory
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.paths import Paths


# Run test cases in reference or compare mode
class TestSetRunner(ABC):
    def __init__(self, settings: TestRunSettings) -> None:
        self.__settings = settings

    @property
    def settings(self) -> TestRunSettings:
        """Settings used for running tests

        Returns:
            TestRunSettings: Used test settings
        """
        return self.__settings

    # Prepare/update programs and testcases. Typically, download/update cases and programs from SVN.
    def prepare_cases_and_programs(self):
        try:
            self.__updatePrograms__()
        except Exception:
            if self.__settings.teamcity:
                sys.stderr.write("##teamcity[testStarted name='Update programs']\n")
                sys.stderr.write(
                    "##teamcity[testFailed name='Update programs' message='Exception occurred']\n"
                )

        self.__updateTestCases__()

    # run test cases to generate reference data
    def run(self):
        skip_testcase = False  # No check defined still running (so no regression test, test against measurements or other numerical package)
        skip_postprocessing = True  # No check defined still running and do not perform the standard postprocessing

        self.prepare_cases_and_programs()
        n_testcases = len(self.__settings.configs)
        for i_testcase, testCaseConfig in enumerate(self.__settings.configs):
            logging.info(
                "\n================================================================================\n"
            )
            logging.info(
                "Testcase "
                + str(i_testcase + 1)
                + " of "
                + str(n_testcases)
                + ": "
                + testCaseConfig.name
                + " ...\n"
            )

            if testCaseConfig.checks.__len__() > 0:
                skip_testcase = True
                skip_postprocessing = True
                for fc in testCaseConfig.checks:
                    if not fc.ignore:
                        skip_testcase = False
                        skip_postprocessing = False
                if not skip_testcase:
                    if testCaseConfig.ignore:
                        skip_testcase = True
                        skip_postprocessing = True

            if self.__settings.teamcity:
                if not skip_testcase:
                    sys.stderr.write(
                        "##teamcity[testStarted name='%s']\n"
                        % stripEscapeCharacters(testCaseConfig.name)
                    )
                else:
                    sys.stderr.write(
                        "##teamcity[testIgnored name='%s']\n"
                        % stripEscapeCharacters(testCaseConfig.name)
                    )

            try:
                # Run testcase
                testcase = TestCase(testCaseConfig)
                if self.__settings.only_post:
                    logging.info(
                        "Skipping execution of testcase (postprocess only)...\n"
                    )
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
                    if not os.path.exists(testCaseConfig.absolute_test_case_path):
                        raise TestCaseFailure(
                            "Could not locate case data at: "
                            + str(testCaseConfig.absolute_test_case_path)
                        )

                    # execute concrete method in subclass
                    self.post_process(testCaseConfig)

            except Exception as e:
                logging.exception(e)
                self.add_error_result(testCaseConfig)
                if self.__settings.teamcity:
                    if not skip_testcase:
                        sys.stderr.write(
                            "##teamcity[testFailed name='%s' message='Exception occurred' details='%s']\n"
                            % (
                                stripEscapeCharacters(testCaseConfig.name),
                                stripEscapeCharacters(e),
                            )
                        )
            finally:
                if self.__settings.teamcity:
                    if not skip_testcase:
                        sys.stderr.write(
                            "##teamcity[testFinished name='%s' message='Comparison passed']\n"
                            % stripEscapeCharacters(testCaseConfig.name)
                        )

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

    @abstractmethod
    def post_process(self, testCaseConfig: TestCaseConfig):
        logging.debug(
            "Reference directory:%s", testCaseConfig.absolute_test_case_reference_path
        )
        logging.debug("Results   directory:%s", testCaseConfig.absolute_test_case_path)

    @abstractmethod
    def show_summary(self):
        pass

    @abstractmethod
    def add_error_result(self, testCaseConfig: TestCaseConfig):
        pass

    # Update network programs and initialize the stack
    def __updatePrograms__(self):
        logging.info("Updating programs")
        for pcnf in self.__settings.programs:
            logging.info("Updating program: %s", pcnf.name)

            # Local path to program root folder
            programLocalPath = None

            # Get the program location
            if len(pcnf.locations) > 0:
                for loc in pcnf.locations:
                    # check type of program
                    if (
                        self.__settings.run_mode == ModeType.REFERENCE
                        and loc.type == PathType.REFERENCE
                    ) or (
                        self.__settings.run_mode == ModeType.COMPARE
                        and loc.type == PathType.CHECK
                    ):
                        # if the program is local, use the existing location
                        sourceLocation = Paths().mergeFullPath(loc.root, loc.from_path)
                        if Paths().isPath(sourceLocation):
                            absLocation = os.path.abspath(
                                Paths().mergeFullPath(sourceLocation, pcnf.path)
                            )
                            if (
                                ResolveHandler().detect(absLocation, None)
                                == HandlerType.PATH
                            ):
                                if not os.path.exists(absLocation):
                                    logging.warning(
                                        "could not yet detect specified program %s",
                                        absLocation,
                                    )
                                #                                   raise SystemExit("Program does not exist")
                                else:
                                    logging.debug(
                                        "detected local path for program %s, using %s",
                                        pcnf.name,
                                        absLocation,
                                    )
                                pcnf.absolute_bin_path = absLocation
                        # else download it from a remote location
                        else:
                            if loc.version:
                                to = loc.to_path + "_" + loc.version
                            else:
                                to = loc.to_path
                            programLocalPath = Paths().rebuildToLocalPath(
                                os.path.join(
                                    self.__settings.local_paths.engines_path, to
                                )
                            )

                            # if the program is remote (network or other) and it does not exist locally, download it
                            if not os.path.exists(programLocalPath):
                                logging.debug(
                                    "Downloading program, %s from %s",
                                    pcnf.name,
                                    sourceLocation,
                                )
                                HandlerFactory().download(
                                    sourceLocation,
                                    programLocalPath,
                                    loc.credentials,
                                    loc.version,
                                )
                            pcnf.absolute_bin_path = os.path.abspath(
                                Paths().mergeFullPath(programLocalPath, pcnf.path)
                            )

            # If a program does not have a network path, and path is not a relative or absolute path, we assume the system can find it
            elif not Paths().isPath(pcnf.path):
                pcnf.absolute_bin_path = pcnf.path
            # Otherwise we need to construct the path from the given information
            else:
                # Construct the absolute binary path for the program
                absbinpath = os.path.abspath(Paths().rebuildToLocalPath(pcnf.path))
                if os.path.exists(absbinpath):
                    pcnf.absolute_bin_path = absbinpath
                # If the local program does not exist, and a network path is not given we are going to crash
                else:
                    raise SystemExit(
                        "Could not find "
                        + pcnf.name
                        + " at given location "
                        + absbinpath
                    )
            logging.debug(
                "Binary path for program %s: %s",
                pcnf.name,
                pcnf.absolute_bin_path,
            )

            # Rebuild the environment variables (specified for this program) to local system variables
            # This is the only place containing all relevant information
            # Do not rebuild the environment variable when it contains a keyword surrounded by "[" and "]",
            # they will be replaced later on
            envparams = pcnf.environment_variables
            for envparam in envparams:
                if (
                    envparams[envparam][0] == "path"
                    and str(envparams[envparam][1]).find("[") == -1
                ):
                    pp = Paths().rebuildToLocalPath(envparams[envparam][1])
                    if not Paths().isAbsolute(pp):
                        if programLocalPath:
                            pp = os.path.abspath(
                                Paths().mergeFullPath(programLocalPath, pp)
                            )
                        envparams[envparam] = [envparams[envparam][0], pp]
                    else:
                        envparams[envparam] = [
                            envparams[envparam][0],
                            envparams[envparam][1],
                        ]

            # Add search paths to the program(configure)
            # Search for (the last) win/lnx/linux in AbsoluteBinPath,
            # add all subdirectories from this level downwards to searchPaths
            # It's quite crude, but this way, all Delft3D programs are able to find each other.
            if pcnf.add_search_paths:
                pltIndex = max(
                    pcnf.absolute_bin_path.rfind("win"),
                    pcnf.absolute_bin_path.rfind("lnx"),
                    pcnf.absolute_bin_path.rfind("linux"),
                    pcnf.absolute_bin_path.rfind("x64"),
                )
                if pltIndex > -1:
                    separatorIndex = max(
                        pcnf.absolute_bin_path[pltIndex:].find("\\"),
                        pcnf.absolute_bin_path[pltIndex:].find("/"),
                    )
                    pltPath = pcnf.absolute_bin_path[: pltIndex + separatorIndex]
                    logging.debug("Path: " + pltPath)
                    searchPaths = Paths().findAllSubFolders(
                        pltPath, pcnf.exclude_search_paths_containing
                    )
                else:
                    # No win/lnx/linux found in AbsoluteBinPath:
                    # Just add AbsoluteBinPath and its subFolders
                    searchPaths = Paths().findAllSubFolders(
                        pcnf.absolute_bin_path,
                        pcnf.exclude_search_paths_containing,
                    )
                # Add explicitly named searchPaths, rebuild when needed
                for aPath in pcnf.search_paths:
                    aRebuildPath = Paths().rebuildToLocalPath(aPath)
                    if not Paths().isAbsolute(aRebuildPath) and programLocalPath:
                        aRebuildPath = Paths().mergeFullPath(
                            programLocalPath, aRebuildPath
                        )
                    searchPaths.append(aRebuildPath)
                pcnf.search_paths = searchPaths

            # Initialize the program
            Program(pcnf)

    # Update test cases
    def __updateTestCases__(self):
        logging.info("Updating cases")

        for testCaseConfig in self.__settings.configs:
            logging.info("Updating case: %s ", testCaseConfig.name)
            locations = testCaseConfig.locations

            if len(locations) == 0:
                error_message: str = (
                    f"Could not update case {testCaseConfig.name},"
                    + " no network paths given"
                )
                self.raise_system_error(error_message)

            for location in locations:
                if location.root == "" or location.from_path == "":
                    error_message: str = (
                        f"Could not update case {testCaseConfig.name}"
                        + ", invalid network input path part (root:{location.root},"
                        + " from:{location.from_path}) given"
                    )
                    self.raise_system_error(error_message)

                # Build the path to download from: Root+From+testcasePath:
                # Root: https://repos.deltares.nl/repos/DSCTestbench/cases
                # From: trunk/win32_hp
                # testcasePath: e01_d3dflow\f01_general\c03-f34
                remote_path = Paths().mergeFullPath(
                    location.root, location.from_path, testCaseConfig.path
                )

                if Paths().isPath(remote_path):
                    remote_path = os.path.abspath(remote_path)

                # Downloading the testcase input/refdata may fail when it is already present and have to be
                # deleted first. This probably has to do with TortoiseSVNCache, accessing that directory.
                # When trying a second time it normally works. Safe side: try 3 times.
                success = False
                attempts = 0
                while attempts < 3 and not success:
                    attempts += 1
                    try:
                        destination_dir = None
                        input_description = ""

                        if location.type == PathType.INPUT:
                            destination_dir = self.__settings.local_paths.cases_path
                            input_description = "input of case"

                        if location.type == PathType.REFERENCE:
                            destination_dir = self.__settings.local_paths.reference_path
                            input_description = "reference result"

                        if destination_dir is not None:
                            # Build localPath to download to: To+testcasePath
                            localPath = Paths().rebuildToLocalPath(
                                Paths().mergeFullPath(
                                    destination_dir,
                                    location.to_path,
                                    testCaseConfig.path,
                                )
                            )
                            self.download_file(
                                location, remote_path, localPath, input_description
                            )

                            if location.type == PathType.INPUT:
                                testCaseConfig.absolute_test_case_path = localPath

                            if location.type == PathType.REFERENCE:
                                testCaseConfig.absolute_test_case_reference_path = (
                                    localPath
                                )

                        success = True

                    except Exception:
                        logging.warning(
                            "Unable to download testcase (attempt %s)", attempts + 1
                        )

                        if attempts >= 3:
                            sys.stderr.write(
                                "##teamcity[testStarted name='Update testcases']\n"
                            )
                            if self.__settings.teamcity:
                                sys.stderr.write(
                                    "##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n"
                                )

    def download_file(
        self,
        location_data: Location,
        remote_path: str,
        localPath: str,
        location_description: str,
    ):
        if self.__settings.only_post:
            logging.info("Skipping testcase download (postprocess only)")
        else:
            logging.debug(
                f"Downloading {location_description}, {localPath} from {remote_path}"
            )

            # Download location on local system is always cleaned before start
            try:
                HandlerFactory().download(
                    remote_path,
                    localPath,
                    location_data.credentials,
                    location_data.version,
                )
            except Exception:
                # We need always case input data
                logging.warning("Could not download from %s", remote_path)
                self.raise_system_error("Could not download from " + remote_path)

    def raise_system_error(self, message: str):
        if self.__settings.teamcity:
            sys.stderr.write("##teamcity[testStarted name='Update testcases']\n")
            sys.stderr.write(
                "##teamcity[testFailed name='Update testcases' message='Download exception occurred']\n"
            )

        raise SystemExit(message)
