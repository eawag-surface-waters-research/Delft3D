import logging
import os
import sys
from typing import List, Tuple

from src.config.file_check import FileCheck
from src.config.parameter import Parameter
from src.config.test_case_config import TestCaseConfig
from src.config.types.presence_type import PresenceType
from src.suite.test_set_runner import TestSetRunner
from src.utils.common import (attachFileLogger, detachFileLogger,
                              stripEscapeCharacters)
from src.utils.comparers.comparer_factory import ComparerFactory
from src.utils.comparers.comparison_result import ComparisonResult
from src.utils.paths import Paths


class ComparisonRunner(TestSetRunner):
    __results: List[Tuple[str, FileCheck, Parameter, ComparisonResult]] = []

    def post_process(self, testCaseConfig: TestCaseConfig):
        comparers = []
        file_checks: List[FileCheck] = []
        for fc in testCaseConfig.checks:
            comparers.append(ComparerFactory().selectComparer(fc))
            file_checks.append(fc)

        skip_report = True
        for fc in testCaseConfig.checks:
            if not fc.ignore:
                skip_report = False
        if not skip_report:
            if testCaseConfig.ignore:
                skip_report = True

        if not skip_report:
            logging.info("Comparing results with reference")
            # Step 1: check if all files in the reference also exist in the compare run.
            allfilesref = Paths().findAllSubFiles(
                testCaseConfig.absolute_test_case_reference_path
            )
            allfilesres = Paths().findAllSubFiles(
                testCaseConfig.absolute_test_case_path
            )
            if len(allfilesref) == 0:
                raise OSError(
                    -1,
                    "Could not locate reference data",
                    testCaseConfig.absolute_test_case_reference_path,
                )
            for fl in allfilesref:
                # ignore files with these extensions
                if not fl in allfilesres and os.path.splitext(fl)[1] not in (
                    ".log",
                    ".tmp",
                ):
                    ignore = False
                    fllocal = Paths().rebuildToLocalPath(fl)
                    # To check whether a missing file can be ignored: use "config.getChecks()" instead of "comparers.keys()"
                    # Otherwise files without checks (but with the "ignore" sign) won't be used
                    for fc in testCaseConfig.checks:
                        fclocal = Paths().rebuildToLocalPath(fc.name)
                        if fclocal == fllocal:
                            ignore = fc.ignore
                            break
                    if ignore:
                        logging.debug("File %s is missing but marked as 'ignore'", fl)
                    else:
                        logging.warning(
                            "File %s part of reference result but not part of case result",
                            os.path.basename(fl),
                        )

        # Step 2: content comparison
        if skip_report or len(comparers) == 0 or testCaseConfig.ignore:
            logging.warning("No checks performed for this testcase (ignored)")
            skip_report = True
        else:
            for j in range(len(comparers)):
                file_check = file_checks[j]
                comparer = comparers[j]

                if file_check.ignore or testCaseConfig.ignore:
                    continue
                # ignore files with extension .log
                in_results = (
                    Paths().rebuildToLocalPath(file_check.name) in allfilesres
                )
                if (
                    not in_results
                    and os.path.splitext(file_check.name)[1] != ".log"
                    and file_check.presence == PresenceType.NONE
                ):
                    raise Exception(
                        "Could not check %s, file not found in result data"
                        % file_check.name
                    )
                else:
                    logging.debug("checking file %s" % file_check.name)
                    absolute_file_name = os.path.join(
                        testCaseConfig.absolute_test_case_path, file_check.name
                    )
                    file_exists = os.path.exists(absolute_file_name)
                    if file_check.presence == PresenceType.NONE:
                        new_results = comparer.compare(
                            testCaseConfig.absolute_test_case_reference_path,
                            testCaseConfig.absolute_test_case_path,
                            file_check,
                            testCaseConfig.name,
                        )
                    else:
                        parameter = Parameter()
                        parameter.name = file_check.name
                        result = ComparisonResult()
                        if file_check.presence == PresenceType.ABSENT:
                            if file_exists:
                                result.result = "NOK"
                            else:
                                result.result = "OK"

                        if file_check.presence == PresenceType.PRESENT:
                            if file_exists:
                                result.result = "OK"
                            else:
                                result.result = "NOK"


                        new_results = [
                            (testCaseConfig.name, file_check, parameter, result)
                        ]

                    # hack to support NEFIS comparison, self.__results should contain list of tuples. No lists of lists allowed.
                    if new_results == [] or new_results is None:
                        continue

                        self.__results.extend(new_results)
                    else:
                        for i in range(len(new_results)):
                            for item in new_results[i]:
                                if isinstance(item, tuple):
                                    self.__results.extend(new_results[i])
                    del file_check

        # Step 3: Write the results to a .txt file in the test case directory.
        if len(self.__results) > 0:
            testcasename_length = -1
            filename_length = -1
            parameter_length = -1
            location_length = -1
            for testcase_name, file_check, parameter, cr in self.__results:
                testcasename_length = max(
                    len(testcase_name), len("Test case name"), testcasename_length
                )
                filename_length = max(
                    len(file_check.name), len("Filename"), filename_length
                )
                parameter_length = max(
                    len(str(parameter.name)), len("Parameter"), parameter_length
                )
                location_length = max(
                    len(str(parameter.location)), len("Location"), location_length
                )

            log_file = os.path.join(
                testCaseConfig.absolute_test_case_path, "result.txt"
            )
            logging.info(
                "Detailed comparison results will be written to: %s" % log_file
            )
            attachFileLogger(testCaseConfig.name, log_file, propagate=True)
            logging.getLogger(testCaseConfig.name).debug(
                "RESULTS of the comparison run"
            )
            format_template = (
                "%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s   %%-10s   %%-10s"
                % (
                    testcasename_length,
                    filename_length,
                    parameter_length,
                    location_length,
                )
            )
            logging.getLogger(testCaseConfig.name).debug(
                format_template
                % (
                    "Test case name",
                    "Filename",
                    "Parameter",
                    "Location",
                    "Result",
                    "MaxAbsDiff",
                    "MaxRelDiff",
                )
            )
            if not self.__results:
                empty = True
                failed = False
                error = False
            else:
                empty = False
                failed = False
                error = False

                error_variables = []
                for testcase_name, file_check, parameter, cr in self.__results:
                    if testcase_name == testCaseConfig.name:
                        if cr.result != "ERROR":
                            format_template = (
                                "%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s    %%.3e    %%.3e"
                                % (
                                    testcasename_length,
                                    filename_length,
                                    parameter_length,
                                    location_length,
                                )
                            )
                            logging.getLogger(testCaseConfig.name).debug(
                                format_template
                                % (
                                    testcase_name[:testcasename_length],
                                    file_check.name[:filename_length],
                                    str(parameter.name)[:parameter_length],
                                    str(parameter.location)[:location_length],
                                    cr.result,
                                    cr.maxAbsDiff,
                                    cr.maxRelDiff,
                                )
                            )
                        else:
                            format_template = (
                                "%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s    %%-7s      %%-7s"
                                % (
                                    testcasename_length,
                                    filename_length,
                                    parameter_length,
                                    location_length,
                                )
                            )
                            logging.getLogger(testCaseConfig.name).debug(
                                format_template
                                % (
                                    testcase_name[:testcasename_length],
                                    file_check.name[:filename_length],
                                    str(parameter.name)[:parameter_length],
                                    str(parameter.location)[:location_length],
                                    "<error>",
                                    "<error>",
                                    "<error>",
                                )
                            )
                            error = True
                            error_variables.append(str(parameter.name))

                        if cr.result == "NOK":
                            failed = True

            if skip_report:
                format_template = (
                    "%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s    %%-7s      %%-7s"
                    % (
                        testcasename_length,
                        filename_length,
                        parameter_length,
                        location_length,
                    )
                )
                logging.getLogger(testCaseConfig.name).debug(
                    format_template
                    % (
                        testcase_name[:testcasename_length],
                        "<ignored>",
                        "<ignored>",
                        "<ignored>",
                        "<ignored>",
                        "<ignored>",
                        "<ignored>",
                    )
                )
                # fill self__results for the ignored test
                fc = FileCheck()
                fc.name = "ignored"
                p = Parameter()
                p.name = "ignored"
                r = ComparisonResult(error=False)
                r.maxAbsDiff = 0.0
                r.maxRelDiff = 0.0
                r.result = "IGNORED"
                new_results = [(testCaseConfig.name, fc, p, r)]
                self.__results.extend(new_results)

            if self.settings.teamcity and empty:
                sys.stderr.write(
                    "##teamcity[testFailed name='%s' message='Comparison: empty result']\n"
                    % stripEscapeCharacters(testCaseConfig.name)
                )
            elif self.settings.teamcity and error:
                sys.stderr.write(
                    "##teamcity[testFailed name='%s' message='Comparison: Error occurred while comparing %s']\n"
                    % (
                        stripEscapeCharacters(testCaseConfig.name),
                        ", ".join(error_variables),
                    )
                )
            elif self.settings.teamcity and failed:
                sys.stderr.write(
                    "##teamcity[testFailed name='%s' message='Comparison: differences above tolerance']\n"
                    % stripEscapeCharacters(testCaseConfig.name)
                )

            detachFileLogger(testCaseConfig.name)
        else:
            logging.warning("No results to display")

    def show_summary(self):
        # Write a summary of the result to the logging module (which typically redirects to standard error).
        test_case_names = (
            []
        )  # Will be the set of test case names, in the original order.
        testcasename_length = -1
        for tuple in self.__results:
            test_case_name = tuple[0]
            testcasename_length = max(
                len(test_case_name), len("Test case name"), testcasename_length
            )
            if len(test_case_names) == 0 or test_case_names[-1] != test_case_name:
                test_case_names.append(test_case_name)

        logging.info(
            "\n================================================================================\n"
        )
        logging.info("SUMMARY of the compare run")
        format_template = (
            "%%-%ds   %%7s %%7s    %%-6s    %%-10s   %%-10s   (filename : parameter)"
            % testcasename_length
        )
        logging.info(
            format_template
            % (
                "Test case name",
                "Runtime",
                "Ratio",
                "Result",
                "MaxAbsDiff",
                "MaxRelDiff",
            )
        )
        for test_case_name in test_case_names:
            # Get the config and the results for this test case.
            test_case_config = [
                x for x in super().settings.configs if x.name == test_case_name
            ][0]
            tc_results = [x for x in self.__results if x[0] == test_case_name]

            # For this test case, find the 'worst' result. This has to take into account that a lower maxAbsDiff for a NOK is worse than a higher maxAbsDiff for an OK.
            # That is: the result always has priority over the maxAbsDiff.
            # We make use here of the fact that 'ERROR' < 'NOK' < 'OK', lexicographically.
            maxAbsDiff_worst = 0.0
            result_worst = "OK"
            i_worst = -1
            for i, (_, _, _, comparison_result) in enumerate(tc_results):
                if comparison_result.result < result_worst or (
                    comparison_result.result == result_worst
                    and comparison_result.maxAbsDiff > maxAbsDiff_worst
                ):
                    maxAbsDiff_worst = comparison_result.maxAbsDiff
                    result_worst = comparison_result.result
                    i_worst = i

            # Local variables now contain the 'worst' scores for that test case. This one will be written in the summary
            worst_result = tc_results[i_worst]
            _, w_filename, w_parameter, w_cr = worst_result
            if w_cr.result == "NOK":
                format_template = (
                    "%%-%ds  (%%7.2f %%7.2f)   %%-7s    %%.3e    %%.3e   (%%s : %%s at coordinates %%s)"
                    % testcasename_length
                )
                logging.info(
                    format_template
                    % (
                        test_case_name[:testcasename_length],
                        test_case_config.run_time,
                        test_case_config.run_time / test_case_config.ref_run_time,
                        w_cr.result,
                        w_cr.maxAbsDiff,
                        w_cr.maxRelDiff,
                        w_filename.name,
                        w_parameter.name,
                        str(w_cr.maxAbsDiffCoordinates),
                    )
                )
            elif w_cr.result == "IGNORED":
                format_template = (
                    "%%-%ds  (%%7s %%7s)   %%-7s    %%9s    %%9s " % testcasename_length
                )
                logging.info(
                    format_template
                    % (
                        test_case_name[:testcasename_length],
                        "---",
                        "---",
                        w_cr.result,
                        "---",
                        "---",
                    )
                )
            else:
                format_template = (
                    "%%-%ds  (%%7.2f %%7.2f)   %%-7s    %%.3e    %%.3e"
                    % testcasename_length
                )
                logging.info(
                    format_template
                    % (
                        test_case_name[:testcasename_length],
                        test_case_config.run_time,
                        test_case_config.run_time / test_case_config.ref_run_time,
                        w_cr.result,
                        w_cr.maxAbsDiff,
                        w_cr.maxRelDiff,
                    )
                )

    def add_error_result(self, test_case_config: TestCaseConfig):
        cr = ComparisonResult()
        cr.maxAbsDiff = 0.0
        cr.maxRelDiff = 0.0
        cr.passed = False
        cr.error = True
        cr.result = "ERROR"
        self.__results.append((test_case_config.name, FileCheck(), Parameter(), cr))
