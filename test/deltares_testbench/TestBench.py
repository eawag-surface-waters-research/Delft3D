"""
Description: Main Application for running Tests
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""


import getpass
import logging
import operator
import os
import sys
from argparse import ArgumentParser, Namespace
from typing import Any, Optional

from settings import TestRunSettings
from src.config.credentials import Credentials
from src.config.types.mode_type import ModeType
from src.suite.comparison_runner import ComparisonRunner
from src.suite.globals import Globals
from src.suite.reference_runner import ReferenceRunner
from src.utils.common import getLogLevel, setRawLogHandler, setTcLogHandler
from src.utils.xml_config_parser import XmlConfigParser


class TestBench:
    """Testbench instance"""

    def __init__(self, args: Namespace) -> None:
        self.settings = self.__parse_arguments(args)

    def run(self):
        """runs the testbench"""
        runner = None
        # Create instance of Manager based on command line arguments.
        if self.settings.run_mode == ModeType.COMPARE:
            runner = ComparisonRunner(self.settings)
        else:
            runner = ReferenceRunner(self.settings)
        # Run the testbench
        try:
            runner.run()
            logging.info("Testbench run finished normally\n")

        except Exception as e:
            logging.error(e)

    def __parse_arguments(self, args: Namespace) -> TestRunSettings:
        settings = TestRunSettings()

        # Store path of Testbench.py into os environment

        scriptpath, scriptname = os.path.split(os.path.abspath(__file__))

        Globals().add("TestBenchRoot", scriptpath)
        Globals().add("TestBenchScriptName", scriptname)
        Globals().add("TestBenchStartUpDir", os.getcwd())

        credentials = self.__get_credentials(args)

        # Parse the xml file.
        xmlcp = XmlConfigParser()

        config_file = "config.xml"

        if args.__dict__["config"] != "":
            config_file = args.__dict__["config"]

        settings.local_paths, settings.programs, settings.configs = xmlcp.load(
            config_file, args.__dict__["or_paths"], credentials
        )

        # Filter the testcases to be run
        if args.__dict__["filter"] != "":
            settings.configs = self.__filter_configs__(
                settings.configs, args.__dict__["filter"]
            )

        # Loglevel from config.xml can be overruled by loglevel from arguments
        if args.__dict__["loglevel"] != "":
            settings.log_level = getLogLevel(args.__dict__["loglevel"])

        # Do not run the model, only run the post-processing (comparison).
        settings.only_post = self.__get_argument_value("only_post", args) or False

        # automatically commit reference run if succesfull
        settings.autocommit = self.__get_argument_value("autocommit", args) or False

        # If option is used, all logging is decorated with TeamCity messages.
        # Additionally, extra TeamCity messages will be produced.
        settings.teamcity = self.__get_argument_value("teamcity", args) or False
        if settings.teamcity:
            setTcLogHandler(settings.log_level)
        else:
            setRawLogHandler(settings.log_level)

        # Determine type of run
        settings.run_mode = self.__get_argument_value("run_mode", args) or ModeType.LIST

        if settings.run_mode == ModeType.LIST:
            self.__print_filter_usage_and_exit__()

        if settings.run_mode == ModeType.TEST_CASE_LIST:
            # Only return the list of test cases to be run.
            self.__print_test_case_list_and_exit__()

        logging.info(
            "\n================================================================================\n"
        )
        logging.info("Arguments:")
        logging.info("Mode     : %s", str(args.__dict__["run_mode"]))
        logging.info("Config   : %s", str(args.__dict__["config"]))
        logging.info("Filter   : %s", str(args.__dict__["filter"]))
        logging.info("LogLevel : %s", str(args.__dict__["loglevel"]))
        logging.info("username : %s", str(args.__dict__["username"]))
        logging.info(
            "\n================================================================================\n"
        )

        return settings

    def __get_argument_value(
        self,
        name: str,
        args: Namespace,
        is_interactive: bool = False,
        secret_value: bool = False,
    ) -> Optional[Any]:
        return_value = None

        if hasattr(args, name):
            return_value = getattr(args, name)

        if not return_value and is_interactive:
            if secret_value:
                return getpass.getpass(f"{name}")

            return input(f"{name}")

        return return_value

    def __get_credentials(self, args: Namespace) -> Credentials:
        credentials = Credentials()
        credentials.name = "commandline"

        is_interactive = self.__get_argument_value("interactive", args) or False
        credentials.username = (
            self.__get_argument_value("username", args, is_interactive) or ""
        )
        if credentials.username == "":
            print(
                'No username on commandline. add "-i True" to enable interactive input'
            )

        credentials.password = (
            self.__get_argument_value("password", args, is_interactive, True) or ""
        )
        if credentials.password == "":
            print(
                'No password on commandline. add "-i True" to enable interactive input'
            )

        return credentials

    # check which filters to apply to configuration
    def __filter_configs__(self, configs, args):
        filtered = []
        filters = args.split(":")
        program_filter = None
        test_case_filter = None
        max_runtime = None
        operator = None
        start_at_filter = None

        for fltr in filters:
            con, arg = fltr.split("=")
            con = con.lower()
            if con == "program":
                program_filter = arg.lower()
            elif con == "testcase":
                test_case_filter = arg.lower()
            elif con == "maxruntime":
                max_runtime = float(arg[1:])
                operator = arg[:1]
            elif con == "startat":
                start_at_filter = arg.lower()
            else:
                sys.stderr.write('ERROR: Filter keyword "' + con + '" not recognised\n')
                self.__print_filter_usage_and_exit__()
        # For each testcase (p, t, mrt filters):
        for config in configs:
            c = self.__find_characteristics__(
                config, program_filter, test_case_filter, max_runtime, operator
            )
            if c:
                filtered.append(c)

        # StartAt filter:
        if start_at_filter:
            starti = len(filtered)
            for i, config in enumerate(filtered):
                if start_at_filter in config.getName():
                    starti = i
                    break
            filtered[:] = filtered[starti:]
        return filtered

    # check if a test case matches given characteristics
    def __find_characteristics__(self, config, program, testcase, mrt, op):
        found_program = None
        found_testcase = None
        found_max_runtime = None

        if program:
            # Program is a string, containing names of programs, comma separated
            programs = program.split(",")
            for aprog in programs:
                found_program = any(
                    aprog in e.getName().lower() for e in config.getPrograms()
                )
                if found_program:
                    break
        if testcase:
            # testcase is a string, containing (parts of) testcase names, comma separated
            testcases = testcase.split(",")
            for atestcase in testcases:
                found_testcase = atestcase in config.getName().lower()
                if found_testcase:
                    break
        if mrt:
            mappings = {">": operator.gt, "<": operator.lt, "=": operator.eq}
            found_max_runtime = mappings[op](config.getMaxRunTime(), mrt)

        if (
            ((not program and not found_program) or (program and found_program))
            and ((not testcase and not found_testcase) or (testcase and found_testcase))
            and ((not mrt and not found_max_runtime) or (mrt and found_max_runtime))
        ):
            return config
        return None

    def __print_filter_usage_and_exit__(self):
        sys.stderr.write("A filter pattern must be formatted as follows:\n\n")
        sys.stderr.write(
            '--filter "program=waq,dflowfm:testcase=aaa,bbb:maxruntime=<10:startat=ccc"\n'
        )
        sys.stderr.write("\tprogram    (program name   = at least one substring)\n")
        sys.stderr.write("\ttestcase   (test case name = at least one substring)\n")
        sys.stderr.write(
            "\tmaxruntime (test run time  = float, larger, smaller, equals)\n"
        )
        sys.stderr.write(
            "\tstartat    (test case name = substring is the first testcase)\n"
        )
        sys.exit(1)

    def __print_test_case_list_and_exit__(self):
        for testcase_config in self.settings.configs:
            sys.stderr.write(testcase_config.name + "\n")
        exit()


def create_argument_parser() -> ArgumentParser:
    parser = ArgumentParser(
        description="Test Bench Version 3, test runner for black box tests.",
        add_help=True,
    )

    # Compulsory arguments
    run_mode_group = parser.add_mutually_exclusive_group(required=True)
    run_mode_group.add_argument(
        "-r",
        "--reference",
        help="Execute a reference run",
        dest="run_mode",
        action="store_const",
        const=ModeType.REFERENCE,
    )
    run_mode_group.add_argument(
        "-c",
        "--compare",
        help="Execute a compare run",
        dest="run_mode",
        action="store_const",
        const=ModeType.COMPARE,
    )
    run_mode_group.add_argument(
        "-l",
        "--list",
        help="Only list parameters for filtering",
        dest="run_mode",
        action="store_const",
        const=ModeType.LIST,
    )
    run_mode_group.add_argument(
        "-t",
        "--testcaselist",
        help="Only list test cases to be run",
        dest="run_mode",
        action="store_const",
        const=ModeType.TEST_CASE_LIST,
    )

    # Optional arguments
    parser.add_argument(
        "--config",
        default="",
        help="Path to config file, if empty default config file is used",
        dest="config",
    )
    parser.add_argument(
        "--filter",
        default="",
        help="Specify what tests to run based on filter (--list for options)",
        dest="filter",
    )
    parser.add_argument(
        "--log-level",
        default="",
        help="CRITICAL, ERROR, WARNING, INFO or DEBUG",
        dest="loglevel",
    )
    parser.add_argument(
        "--override-paths",
        default="",
        help="root[name]=some path,from[name]=some path,to[name]=some path,path[name]=some path,....",
        dest="or_paths",
    )
    parser.add_argument(
        "--only-post-process",
        help="Skip running",
        action="store_true",
        dest="only_post",
    )
    parser.add_argument(
        "--autocommit",
        action="store_true",
        help="Turns on autocommit when doing a reference run.",
        dest="autocommit",
    )
    parser.add_argument(
        "--teamcity",
        action="store_true",
        help="Turns on specific TeamCity logging.",
        dest="teamcity",
    )
    parser.add_argument(
        "--username",
        help="Subversion username.",
        default=None,
        # required=True,
        dest="username",
    )
    parser.add_argument(
        "--password",
        help="Subversion password.",
        default=None,
        # required=True,
        dest="password",
    )
    parser.add_argument(
        "-i",
        "--interactive",
        action="store_true",
        help="Must be True to enable username/password via keyboard.",
        dest="interactive",
    )

    return parser


if __name__ == "__main__":
    logging.getLogger("matplotlib").setLevel(level=logging.CRITICAL)
    logging.info(sys.version)

    parser = create_argument_parser()
    parsed_args: Namespace = parser.parse_args()

    TestBench(parsed_args).run()
