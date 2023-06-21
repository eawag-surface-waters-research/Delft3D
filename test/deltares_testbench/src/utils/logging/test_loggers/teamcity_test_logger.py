"""
Description: test case logger for teamcity
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import datetime
import sys
from typing import List, Optional

from src.utils.common import stripEscapeCharacters
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.i_test_logger import ITestLogger
from src.utils.logging.test_loggers.test_result_type import TestResultType


class TeamcityTestLogger(ITestLogger):
    """Logs test case information for teamcity"""

    def __init__(self, test_name: str) -> None:
        self.__test_name = test_name
        self.__flow_id = test_name

    def error(self, message: str):
        self.log(message, LogLevel.ERROR)

    def warning(self, message: str):
        self.log(message, LogLevel.WARNING)

    def info(self, message: str):
        self.log(message, LogLevel.INFO)

    def debug(self, message: str):
        self.log(message, LogLevel.DEBUG)

    def log(self, message: str, log_level: LogLevel):
        status = self.__get_status(log_level)

        self.write_tc_message(
            "message",
            f"{str(log_level)} : {message}",
            extra_tags=[f"status='{status}'"],
        )

    def test_started(self):
        self.write_tc_message("testStarted")

    def test_ignored(self):
        self.write_tc_message("testIgnored")

    def test_finished(self):
        self.write_tc_message("testFinished")

    def test_Result(
        self,
        result_type: TestResultType,
        error_message: Optional[str] = None,
    ):
        if result_type == TestResultType.Empty:
            self.write_tc_message("testFailed", "Comparison: empty result")
        elif result_type == TestResultType.Error:
            self.write_tc_message(
                "testFailed",
                f"Comparison: Error occurred while comparing {error_message}",
            )
        elif result_type == TestResultType.Differences:
            self.write_tc_message(
                "testFailed", "Comparison: differences above tolerance"
            )
        elif result_type == TestResultType.Exception:
            self.write_tc_message(
                "testFailed",
                "Exception occurred",
                extra_tags=[f"details='{error_message}'"],
            )
        elif result_type == TestResultType.Passed:
            self.write_tc_message("testFinished", "Comparison passed")

    def write_tc_message(
        self,
        command: str,
        message: Optional[str] = None,
        extra_tags: Optional[List[str]] = None,
    ):
        time_str = datetime.datetime.now().isoformat(timespec="milliseconds")
        tc_message = (
            f"##teamcity[{command} "
            + f"name='{self.__test_name}' "
            + f"flowId='{self.__flow_id}' "
            + f"timestamp='{time_str}' "
        )

        if extra_tags:
            tc_message += " ".join(extra_tags) + " "

        if message:
            escaped_message = stripEscapeCharacters(message)
            tc_message += f"text='{escaped_message}' "

        tc_message += "]"

        sys.stdout.write(tc_message)

    def __get_status(self, level: LogLevel) -> str:
        if level == LogLevel.ERROR or level == LogLevel.CRITICAL:
            return "ERROR"
        if level == LogLevel.FATAL:
            return "FAILURE"
        if level == LogLevel.WARNING:
            return "WARNING"

        return "NORMAL"
