import logging
import os
import settings
import sys
from src.Suite.TestSetRunner import TestSetRunner
from src.Utils.Paths import Paths
from src.Config.FileCheck import FileCheck, Parameter, PresenceType
from src.Utils.ComparisonResult import ComparisonResult
from src.Utils.ComparerFactory import ComparerFactory
from src.Utils.Common import attachFileLogger, detachFileLogger, stripEscapeCharacters


class ComparisonRunner(TestSetRunner):
    __results = []

    def post_process(self, testCaseConfig):
        comparers = []
        file_checks = []
        for fc in testCaseConfig.getChecks():
            comparers.append(ComparerFactory().selectComparer(fc))
            file_checks.append(fc)

        skip_report = True
        for fc in testCaseConfig.getChecks():
            if not fc.ignore():
                skip_report = False
        if not skip_report:
            if testCaseConfig.getIgnore():
                skip_report = True

        if not skip_report:
            logging.info("Comparing results with reference")
            # Step 1: check if all files in the reference also exist in the compare run.
            allfilesref = Paths().findAllSubFiles(testCaseConfig.getAbsoluteTestCaseReferencePath())
            allfilesres = Paths().findAllSubFiles(testCaseConfig.getAbsoluteTestCasePath())
            if len(allfilesref) == 0:
                raise OSError(-1, "Could not locate reference data", testCaseConfig.getAbsoluteTestCaseReferencePath())
            for fl in allfilesref:
                # ignore files with these extensions
                if not fl in allfilesres and os.path.splitext(fl)[1] not in (".log", ".tmp"):
                    ignore = False
                    fllocal = Paths().rebuildToLocalPath(fl)
                    # To check whether a missing file can be ignored: use "config.getChecks()" instead of "comparers.keys()"
                    # Otherwise files without checks (but with the "ignore" sign) won't be used
                    for fc in testCaseConfig.getChecks():
                        fclocal = Paths().rebuildToLocalPath(fc.getName())
                        if fclocal == fllocal:
                            ignore = fc.ignore()
                            break
                    if ignore:
                        logging.debug("File %s is missing but marked as 'ignore'", fl)
                    else:
                        logging.warning("File %s part of reference result but not part of case result",
                                        os.path.basename(fl))

        # Step 2: content comparison
        if skip_report or len(comparers) == 0 or testCaseConfig.getIgnore():
            logging.warning("No checks performed for this testcase (ignored)")
            skip_report = True
        else:
            for j in range(len(comparers)):
                file_check = file_checks[j]
                comparer = comparers[j]

                if file_check.ignore() or testCaseConfig.getIgnore():
                    continue
                # ignore files with extension .log
                in_results = Paths().rebuildToLocalPath(file_check.getName()) in allfilesres
                if not in_results and os.path.splitext(file_check.getName())[1] != ".log" and \
                        file_check.getPresence() == PresenceType.NONE:
                    raise Exception("Could not check %s, file not found in result data" % file_check.getName())
                else:
                    logging.debug('checking file %s' % file_check.getName())
                    absolute_file_name = os.path.join(testCaseConfig.getAbsoluteTestCasePath(), file_check.getName())
                    file_exists = os.path.exists(absolute_file_name)
                    if file_check.getPresence() == PresenceType.NONE:
                        new_results = comparer.compare(testCaseConfig.getAbsoluteTestCaseReferencePath(),
                                                       testCaseConfig.getAbsoluteTestCasePath(),
                                                       file_check, testCaseConfig.getName())
                    else:
                        parameter = Parameter()
                        parameter.setName(file_check.getName())
                        result = ComparisonResult()
                        if file_check.getPresence() == PresenceType.ABSENT:
                            if file_exists:
                                result.result = "NOK"
                            else:
                                result.result = "OK"

                        if file_check.getPresence() == PresenceType.PRESENT:
                            if file_exists:
                                result.result = "OK"
                            else:
                                result.result = "NOK"

                        new_results = [(testCaseConfig.getName(), file_check, parameter, result)]

                    # hack to support NEFIS comparison, self.__results should contain list of tuples. No lists of lists allowed.
                    if new_results == [] or new_results is None:
                        continue
                    if isinstance(new_results[0], tuple):
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
            for (testcase_name, file_check, parameter, cr) in self.__results:
                testcasename_length = max(len(testcase_name), len("Test case name"), testcasename_length)
                filename_length = max(len(file_check.getName()), len("Filename"), filename_length)
                parameter_length = max(len(str(parameter.getName())), len("Parameter"), parameter_length)
                location_length = max(len(str(parameter.getLocation())), len("Location"), location_length)

            log_file = os.path.join(testCaseConfig.getAbsoluteTestCasePath(), "result.txt")
            logging.info('Detailed comparison results will be written to: %s' % log_file)
            attachFileLogger(testCaseConfig.getName(), log_file, propagate=True)
            logging.getLogger(testCaseConfig.getName()).debug("RESULTS of the comparison run")
            format_template = '%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s   %%-10s   %%-10s' % (
                testcasename_length, filename_length, parameter_length, location_length)
            logging.getLogger(testCaseConfig.getName()).debug(format_template % ("Test case name",
                                                                                 "Filename",
                                                                                 "Parameter",
                                                                                 "Location",
                                                                                 "Result",
                                                                                 "MaxAbsDiff",
                                                                                 "MaxRelDiff"))
            if not self.__results:
                empty = True
                failed = False
                error = False
            else:
                empty = False
                failed = False
                error = False

                error_variables = []
                for (testcase_name, file_check, parameter, cr) in self.__results:
                    if testcase_name == testCaseConfig.getName():
                        if cr.result != 'ERROR':
                            format_template = '%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s    %%.3e    %%.3e' % (
                                testcasename_length, filename_length, parameter_length, location_length)
                            logging.getLogger(testCaseConfig.getName()).debug(
                                format_template % (testcase_name[:testcasename_length],
                                                   file_check.getName()[:filename_length],
                                                   str(parameter.getName())[:parameter_length],
                                                   str(parameter.getLocation())[:location_length],
                                                   cr.result,
                                                   cr.maxAbsDiff,
                                                   cr.maxRelDiff))
                        else:
                            format_template = '%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s    %%-7s      %%-7s' % (
                                testcasename_length, filename_length, parameter_length, location_length)
                            logging.getLogger(testCaseConfig.getName()).debug(
                                format_template % (testcase_name[:testcasename_length],
                                                   file_check.getName()[:filename_length],
                                                   str(parameter.getName())[:parameter_length],
                                                   str(parameter.getLocation())[:location_length],
                                                   '<error>',
                                                   '<error>',
                                                   '<error>'))
                            error = True
                            error_variables.append(str(parameter.getName()))

                        if cr.result == "NOK":
                            failed = True

            if skip_report:
                format_template = '%%-%ds  %%-%ds  %%-%ds  %%-%ds  %%-6s    %%-7s      %%-7s' % (
                    testcasename_length, filename_length, parameter_length, location_length)
                logging.getLogger(testCaseConfig.getName()).debug(
                    format_template % (testcase_name[:testcasename_length],
                                       '<ignored>',
                                       '<ignored>',
                                       '<ignored>',
                                       '<ignored>',
                                       '<ignored>',
                                       '<ignored>'))
                # fill self__results for the ignored test
                fc = FileCheck()
                fc.setName('jan')
                p = Parameter()
                p.setName('mooiman')
                r = ComparisonResult(error=False)
                r.maxAbsDiff = 0.0
                r.maxRelDiff = 0.0
                r.result = 'IGNORED'
                new_results = [(testCaseConfig.getName(), fc, p, r)]
                self.__results.extend(new_results)

            if settings.teamcity and empty:
                sys.stderr.write(
                    "##teamcity[testFailed name='%s' message='Comparison: empty result']\n" %
                    stripEscapeCharacters(testCaseConfig.getName()))
            elif settings.teamcity and error:
                sys.stderr.write(
                    "##teamcity[testFailed name='%s' message='Comparison: Error occurred while comparing %s']\n" %
                    (stripEscapeCharacters(testCaseConfig.getName()), ', '.join(error_variables)))
            elif settings.teamcity and failed:
                sys.stderr.write(
                    "##teamcity[testFailed name='%s' message='Comparison: differences above tolerance']\n" %
                    stripEscapeCharacters(testCaseConfig.getName()))

            detachFileLogger(testCaseConfig.getName())
        else:
            logging.warning('No results to display')

    def show_summary(self):
        # Write a summary of the result to the logging module (which typically redirects to standard error).
        test_case_names = []  # Will be the set of test case names, in the original order.
        testcasename_length = -1
        for tuple in self.__results:
            test_case_name = tuple[0]
            testcasename_length = max(len(test_case_name), len("Test case name"), testcasename_length)
            if len(test_case_names) == 0 or test_case_names[-1] != test_case_name:
                test_case_names.append(test_case_name)

        logging.info("\n================================================================================\n")
        logging.info("SUMMARY of the compare run")
        format_template = '%%-%ds   %%7s %%7s    %%-6s    %%-10s   %%-10s   (filename : parameter)' % testcasename_length
        logging.info(format_template % ("Test case name", "Runtime", "Ratio", "Result", "MaxAbsDiff", "MaxRelDiff"))
        for test_case_name in test_case_names:

            # Get the config and the results for this test case.
            test_case_config = [x for x in settings.configs if x.getName() == test_case_name][0]
            tc_results = [x for x in self.__results if x[0] == test_case_name]

            # For this test case, find the 'worst' result. This has to take into account that a lower maxAbsDiff for a NOK is worse than a higher maxAbsDiff for an OK.
            # That is: the result always has priority over the maxAbsDiff.
            # We make use here of the fact that 'ERROR' < 'NOK' < 'OK', lexicographically.
            maxAbsDiff_worst = 0.0
            result_worst = 'OK'
            i_worst = -1
            for i, (_, _, _, comparison_result) in enumerate(tc_results):
                if comparison_result.result < result_worst or \
                        (comparison_result.result == result_worst and comparison_result.maxAbsDiff > maxAbsDiff_worst):
                    maxAbsDiff_worst = comparison_result.maxAbsDiff
                    result_worst = comparison_result.result
                    i_worst = i

            # Local variables now contain the 'worst' scores for that test case. This one will be written in the summary
            worst_result = tc_results[i_worst]
            _, w_filename, w_parameter, w_cr = worst_result
            if w_cr.result == 'NOK':
                format_template = '%%-%ds  (%%7.2f %%7.2f)   %%-7s    %%.3e    %%.3e   (%%s : %%s at coordinates %%s)' % testcasename_length
                logging.info(format_template %
                             (test_case_name[:testcasename_length],
                              test_case_config.getRunTime(),
                              test_case_config.getRunTime() / test_case_config.getRefRunTime(),
                              w_cr.result,
                              w_cr.maxAbsDiff,
                              w_cr.maxRelDiff,
                              w_filename.getName(),
                              w_parameter.getName(),
                              str(w_cr.maxAbsDiffCoordinates)))
            elif w_cr.result == 'IGNORED':
                format_template = '%%-%ds  (%%7s %%7s)   %%-7s    %%9s    %%9s ' % testcasename_length
                logging.info(format_template %
                             (test_case_name[:testcasename_length],
                              '---',
                              '---',
                              w_cr.result,
                              '---',
                              '---'))
            else:
                format_template = '%%-%ds  (%%7.2f %%7.2f)   %%-7s    %%.3e    %%.3e' % testcasename_length
                logging.info(format_template %
                             (test_case_name[:testcasename_length],
                              test_case_config.getRunTime(),
                              test_case_config.getRunTime() / test_case_config.getRefRunTime(),
                              w_cr.result,
                              w_cr.maxAbsDiff,
                              w_cr.maxRelDiff))

    def add_error_result(self, test_case_config):
        cr = ComparisonResult()
        cr.maxAbsDiff = 0.0
        cr.maxRelDiff = 0.0
        cr.passed = False
        cr.error = True
        cr.result = "ERROR"
        self.__results.append((test_case_config.getName(), FileCheck(), Parameter(), cr))
