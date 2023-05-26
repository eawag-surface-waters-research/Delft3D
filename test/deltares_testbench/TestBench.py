'''
Description: Main Application for running Tests
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import logging
import sys
import os
import argparse
import operator
if sys.version_info>(3,0,0):
    import getpass
from src.Utils.XmlConfigParser import XmlConfigParser
from src.Suite.ComparisonRunner import ComparisonRunner
from src.Suite.ReferenceRunner import ReferenceRunner
from src.Suite.Globals import Globals
from src.Config.Credentials import Credentials
from src.Config.Mode import Mode
from src.Utils.Common import setTcLogHandler, setRawLogHandler, getLogLevel
import settings as settings
import matplotlib


class TestBench(object):
    def run(self):

        # Parse command line arguments.
        self.parse_arguments()
        logging.info("%s" % sys.version)

        runner = None
        # Create instance of Manager based on command line arguments.
        if settings.run_mode == Mode.COMPARE:
            runner = ComparisonRunner()
        else:
            runner = ReferenceRunner()
        # Run the testbench
        try:
            runner.run()
            logging.info("Testbench run finished normally\n")
        except Exception as e:
            logging.error(e)

    def parse_arguments(self):

        parser = argparse.ArgumentParser(description='Test Bench Version 3, test runner for black box tests.')

        # Compulsory arguments
        run_mode_group = parser.add_mutually_exclusive_group(required=True)
        run_mode_group.add_argument("-r", "--reference",
                                    help="Execute a reference run",
                                    dest="run_mode",
                                    action="store_const",
                                    const="reference")
        run_mode_group.add_argument("-c", "--compare",
                                    help="Execute a compare run",
                                    dest="run_mode",
                                    action="store_const",
                                    const="compare")
        run_mode_group.add_argument("-l", "--list",
                                    help="Only list parameters for filtering",
                                    dest="run_mode",
                                    action="store_const",
                                    const="list")
        run_mode_group.add_argument("-t", "--testcaselist",
                                    help="Only list test cases to be run",
                                    dest="run_mode",
                                    action="store_const",
                                    const="testcaselist")

        # Optional arguments
        parser.add_argument("--config",
                            default="",
                            help="Path to config file, if empty default config file is used",
                            dest="config")
        parser.add_argument("--filter",
                            default="",
                            help="Specify what tests to run based on filter (--list for options)",
                            dest="filter")
        parser.add_argument("--log-level",
                            default="",
                            help="CRITICAL, ERROR, WARNING, INFO or DEBUG",
                            dest="loglevel")
        parser.add_argument("--override-paths",
                            default="",
                            help="root[name]=some path,from[name]=some path,to[name]=some path,path[name]=some path,....",
                            dest="or_paths")
        parser.add_argument("--only-post-process",
                            help="Skip running",
                            action="store_true",
                            dest="only_post")
        parser.add_argument("--autocommit",
                            action="store_true",
                            help="Turns on autocommit when doing a reference run.",
                            dest="autocommit")
        parser.add_argument("--teamcity",
                            action="store_true",
                            help="Turns on specific TeamCity logging.",
                            dest="teamcity")
        parser.add_argument("--username",
                            help="Subversion username.",
                            default=None,
                            #required=True,
                            dest="username")
        parser.add_argument("--password",
                            help="Subversion password.",
                            default=None,
                            #required=True,
                            dest="password")
        parser.add_argument('-i', '--interactive',
                            help="Must be True to enable username/password via keyboard.",
                            default=None,
                            dest='interactive')

        # Start parsing
        args = parser.parse_args()

        # Store path of Testbench.py into os environment
        
        scriptpath, scriptname = os.path.split(os.path.abspath(__file__))
        Globals().add("TestBenchRoot", scriptpath)
        Globals().add("TestBenchScriptName", scriptname)
        Globals().add("TestBenchStartUpDir", os.getcwd())

        c = Credentials()
        c.setName("commandline")
        if args.__dict__["username"] != None:
            c.setUsername(args.__dict__["username"])
        else:
            if args.__dict__["interactive"] != None and args.__dict__["interactive"]:
                if sys.version_info<(3,0,0):
                    iin = raw_input('Username for TeamCity access:')
                    c.setUsername(iin)
                else:
                    c.setUsername(input('Username for TeamCity access:'))
            else:
                print('No username on commandline. add "-i True" to enable interactive input')
        if args.__dict__["password"] != None:
            c.setPassword(args.__dict__["password"])
        else:
            if args.__dict__["interactive"] != None and args.__dict__["interactive"]:
                if sys.version_info<(3,0,0):
                    iin = raw_input('pwd:')
                    c.setPassword(iin)
                    iin = None
                else:
                    c.setPassword(getpass.getpass())
            else:
                print('No password on commandline. add "-i True" to enable interactive input')

        # Parse the xml file.
        xmlcp = XmlConfigParser()
        if args.__dict__["config"] != "":
            settings.local_paths, settings.programs, settings.configs = xmlcp.load(args.__dict__["config"], args.__dict__["or_paths"],c)
        else:
            settings.local_paths, settings.programs, settings.configs = xmlcp.load("config.xml", args.__dict__["or_paths"],c)

        # Filter the testcases to be run
        if args.__dict__["filter"] != "":
            settings.configs = self.__filter_configs__(settings.configs, args.__dict__["filter"])

        # Loglevel from config.xml can be overruled by loglevel from arguments
        if args.__dict__["loglevel"] != "":
            settings.log_level = getLogLevel(args.__dict__["loglevel"])

        # Do not run the model, only run the post-processing (comparison).
        settings.only_post = args.__dict__["only_post"]

        # automatically commit reference run if succesfull
        settings.autocommit = args.__dict__["autocommit"]

        # If option is used, all logging is decorated with TeamCity messages.
        # Additionally, extra TeamCity messages will be produced.
        settings.teamcity = args.__dict__["teamcity"]
        if settings.teamcity:
            setTcLogHandler(settings.log_level)
        else:
            setRawLogHandler(settings.log_level)

        # Determine type of run
        if args.__dict__["run_mode"] == "reference":
            settings.run_mode = Mode.REFERENCE
        elif args.__dict__["run_mode"] == "compare":
            settings.run_mode = Mode.COMPARE
        elif args.__dict__["run_mode"] == "list":
            self.__print_filter_usage_and_exit__()
        # Only return the list of test cases to be run.
        elif args.__dict__["run_mode"] == "testcaselist":
            self.__print_test_case_list_and_exit__()
        logging.info("\n================================================================================\n")
        logging.info("Arguments:")
        logging.info("Mode     : %s", str(args.__dict__["run_mode"]))
        logging.info("Config   : %s", str(args.__dict__["config"]))
        logging.info("Filter   : %s", str(args.__dict__["filter"]))
        logging.info("LogLevel : %s", str(args.__dict__["loglevel"]))
        logging.info("username : %s", str(args.__dict__["username"]))
        logging.info("\n================================================================================\n")

    # check which filters to apply to configuration
    def __filter_configs__(self, configs, args):
        filtered = []
        filters = args.split(":")
        p = None
        t = None
        mrt = None
        op = None
        s = None
        for fltr in filters:
            con, arg = fltr.split("=")
            con = con.lower()
            if con == "program":
                p = arg.lower()
            elif con == "testcase":
                t = arg.lower()
            elif con == "maxruntime":
                mrt = float(arg[1:])
                op = arg[:1]
            elif con == "startat":
                s = arg.lower()
            else:
                sys.stderr.write("ERROR: Filter keyword \"" + con + "\" not recognised\n")
                self.__print_filter_usage_and_exit__()
        # For each testcase (p, t, mrt filters):
        for config in configs:
            c = self.__find_characteristics__(config, p, t, mrt, op)
            if c:
                filtered.append(c)
        # StartAt filter:
        if s:
            starti = len(filtered)
            for i, config in enumerate(filtered):
                if s in config.getName():
                    starti = i
                    break
            filtered[:] = filtered[starti:]
        return filtered

    # check if a test case matches given characteristics
    def __find_characteristics__(self, config, program, testcase, mrt, op):
        fp = None
        ft = None
        fm = None
        if program:
            # Program is a string, containing names of programs, comma separated
            programs = program.split(",")
            for aprog in programs:
                fp = any(aprog in e.getName().lower() for e in config.getPrograms())
                if fp:
                    break
        if testcase:
            # testcase is a string, containing (parts of) testcase names, comma separated
            testcases = testcase.split(",")
            for atestcase in testcases:
                ft = atestcase in config.getName().lower()
                if ft:
                    break
        if mrt:
            mappings = {'>': operator.gt, '<': operator.lt, '=': operator.eq}
            fm = mappings[op](config.getMaxRunTime(), mrt)
        if (((not program and not fp) or (program and fp)) and
                ((not testcase and not ft) or (testcase and ft)) and
                ((not mrt and not fm) or (mrt and fm))):
            return config
        return None

    def __print_filter_usage_and_exit__(self):
        sys.stderr.write("A filter pattern must be formatted as follows:\n\n")
        sys.stderr.write("--filter \"program=waq,dflowfm:testcase=aaa,bbb:maxruntime=<10:startat=ccc\"\n")
        sys.stderr.write("\tprogram    (program name   = at least one substring)\n")
        sys.stderr.write("\ttestcase   (test case name = at least one substring)\n")
        sys.stderr.write("\tmaxruntime (test run time  = float, larger, smaller, equals)\n")
        sys.stderr.write("\tstartat    (test case name = substring is the first testcase)\n")
        sys.exit(1)

    def __print_test_case_list_and_exit__(self):
        for testcase_config in settings.configs:
            sys.stderr.write(testcase_config.getName() + '\n')
        exit()


if __name__ == "__main__":
    logging.getLogger('matplotlib').setLevel(level=logging.CRITICAL)
    TestBench().run()
    # On Linux, the testbench hangs after writing "Testbench run finished normally"
    # The sys.exit() below does not help
    # May be the os._exit(0) helps
    sys.exit()
    os._exit(0)
