import logging
import os
from distutils import dir_util, file_util

from src.config.test_case_config import TestCaseConfig
from src.config.types.path_type import PathType
from src.suite.test_set_runner import TestSetRunner
from src.utils.handlers.handler_factory import HandlerFactory
from src.utils.paths import Paths


class ReferenceRunner(TestSetRunner):
    def post_process(self, testCaseConfig: TestCaseConfig):
        # Get the reference networkPath of the testcase
        refNetworkPath = None
        credentials = None
        for aNetworkPath in testCaseConfig.locations:
            if aNetworkPath.type == PathType.REFERENCE:
                refNetworkPath = aNetworkPath
                credentials = aNetworkPath.credentials

        # Make sure the reference folders are in sync.
        netloc = Paths().mergeFullPath(
            refNetworkPath.root, refNetworkPath.from_path, testCaseConfig.path
        )
        if refNetworkPath:
            HandlerFactory().prepare_upload(
                testCaseConfig.absolute_test_case_reference_path, netloc, credentials
            )

        logging.debug("Overwrite (local) reference")
        if not os.path.exists(
            os.path.join(testCaseConfig.absolute_test_case_path, "_tb3_char.run")
        ):
            raise OSError(
                -1,
                "Could not locate _tb3_char.run",
                testCaseConfig.absolute_test_case_path,
            )
        if not os.path.exists(testCaseConfig.absolute_test_case_reference_path):
            os.makedirs(testCaseConfig.absolute_test_case_reference_path)
        files = self.__findCharacteristicsChangedFiles__(
            os.path.join(testCaseConfig.absolute_test_case_path, "_tb3_char.run")
        )
        for f in files:
            fl = os.path.join(testCaseConfig.absolute_test_case_path, f)
            fr = os.path.join(testCaseConfig.absolute_test_case_reference_path, f)
            if os.path.isfile(fl):
                file_util.copy_file(fl, fr)
            if os.path.isdir(fl):
                dir_util.copy_tree(fl, fr)
        file_util.copy_file(
            os.path.join(testCaseConfig.absolute_test_case_path, "_tb3_char.run"),
            os.path.join(
                testCaseConfig.absolute_test_case_reference_path, "_tb3_char.run"
            ),
        )

        # Upload (or prepare upload) of new results of reference run.
        if refNetworkPath:
            # Build the path to upload to: Root+From+testcasePath:
            # Root: https://repos.deltares.nl/repos/DSCTestbench/references
            # From: trunk/win32_hp
            # testcasePath: e01_d3dflow\f01_general\c03-f34
            logging.debug("\tPreparing reference data for upload...\n")
            HandlerFactory().upload(
                testCaseConfig.absolute_test_case_reference_path,
                netloc,
                credentials,
                super().settings.autocommit,
            )
        else:
            logging.warning(
                "Could not find reference network path for case to upload to"
            )

    def show_summary(self):
        logging.info("SUMMARY of the reference run")
        logging.info(
            "%-40s (%7s %7s) %-6s" % ("Test case name", "Runtime", "Ratio", "Result")
        )
        for test_case_config in super().settings.configs:
            if len(test_case_config.errors) > 0:
                result = "ERROR"
            else:
                result = "OK"

            logging.info(
                "%-40s (%7.2f %7.2f) %-6s"
                % (
                    test_case_config.name[:40],
                    test_case_config.run_time,
                    test_case_config.run_time / test_case_config.ref_run_time,
                    result,
                )
            )

    # retrieve changed and added files from _tb3_char.run file
    # input: path to _tb3_char.run
    # output: changed and added file names
    def __findCharacteristicsChangedFiles__(self, filename):
        with open(filename) as f:
            lines = []
            for line in f:
                if "Output_" in line:
                    _, value = line.split(":")
                    lines.append(value.strip())
            return lines

    def add_error_result(self, testCaseConfig):
        # the result of a reference run is only determined by whether an error was detected or not.
        # This is reflected in the testCaseConfig (not to be confused with testCase, which also tracks errors).
        testCaseConfig.errors = [""]
