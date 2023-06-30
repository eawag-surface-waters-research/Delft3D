"""
Description: logger for console
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import logging
import sys

from src.utils.logging.i_main_logger import IMainLogger
from src.utils.logging.log_level import LogLevel
from src.utils.logging.test_loggers.file_test_logger import FileTestLogger
from src.utils.logging.test_loggers.i_test_logger import ITestLogger


class ConsoleLogger(IMainLogger):
    def __init__(self, log_level: LogLevel) -> None:
        self.__logger = self.__create_logger(log_level)

    def error(self, message: str):
        self.__base_log_message(message, logging.ERROR)
        sys.stderr.write(message)

    def warning(self, message: str):
        self.__base_log_message(message, logging.WARNING)

    def info(self, message: str):
        self.__base_log_message(message, logging.INFO)

    def debug(self, message: str):
        self.__base_log_message(message, logging.DEBUG)

    def log(self, message: str, log_level: LogLevel):
        log_level = self.__get_internal_log_level(log_level)
        self.__base_log_message(message, log_level)

    def create_test_case_logger(self, test_case_id: str) -> ITestLogger:
        return FileTestLogger(test_case_id)

    def __base_log_message(self, message: str, log_level: int):
        self.__logger.log(log_level, message)

    def __get_internal_log_level(self, log_level: LogLevel) -> int:
        return log_level

    def __create_logger(self, log_level: LogLevel) -> logging.Logger:
        level = self.__get_internal_log_level(log_level)
        logger = logging.getLogger("")
        logger.setLevel(level)
        logger.isEnabledFor(level)

        logger.handlers.clear()
        logger.handlers.append(self.__create_console_handler(level))
        return logger

    def __create_console_handler(self, log_level: int) -> logging.Handler:
        if log_level <= logging.DEBUG:
            format_str = (
                "%(asctime)s [%(levelname)-7s] : %(message)s (%(module)s.%(funcName)s)"
            )
        else:
            format_str = "%(asctime)s [%(levelname)-7s] %(message)s"

        handler = logging.StreamHandler()
        handler.setLevel(log_level)
        handler.setFormatter(logging.Formatter(format_str))

        return handler
