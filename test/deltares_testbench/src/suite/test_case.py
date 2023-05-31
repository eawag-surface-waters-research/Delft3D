"""
Description: Test Case Handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import copy
import logging
import os
import threading
import time
from itertools import groupby

from src.config.test_case_config import TestCaseConfig
from src.suite.program import Programs
from src.utils.paths import Paths


# Test case handler (compare or reference)
class TestCase(object):
    __errors = []
    __threads = None

    # constructor
    # input: test case configuration
    def __init__(self, config: TestCaseConfig):
        self.__config = config
        self.__maxRunTime: float = self.__config.max_run_time

        logging.debug(
            "Initializing test case (%s), max runtime : %s",
            self.__config.name,
            str(self.__maxRunTime),
        )

        self.__config.run_file_name = os.path.join(
            self.__config.absolute_test_case_path, "_tb3_char.run"
        )
        refrunfile = os.path.join(
            config.absolute_test_case_reference_path, "_tb3_char.run"
        )

        if os.path.exists(refrunfile):
            refruntime = self.__findCharacteristicsRunTime__(refrunfile)
            if refruntime:
                self.__config.ref_run_time = refruntime
                if not self.__config.overrule_ref_max_run_time:
                    # set maxRunTime to 1.5 * reference runtime and add a few seconds (some systems start slow)
                    # The variation in runtimes vary a lot (different machines, other processes)
                    self.__maxRunTime = refruntime * 1.5 + 10.0
                    logging.info(
                        "Overwriting max run time via reference _tb3_char.run (%s)",
                        str(self.__maxRunTime),
                    )

        self.__maxRunTime = max(self.__maxRunTime, 120.0) * 5.0 + 300.0
        logging.debug("maxRunTime increased to %s", str(self.__maxRunTime))

    # execute a Test Case
    # [remark] execution does not throw errors, these can be retrieved from getErrors()
    def run(self):
        # prepare the programs for running
        self.__initializeProgramList__()
        # create thread for handling runners
        logging.debug("Starting test case thread")
        thread = threading.Thread(target=self.__execute__)
        thread.start()
        # set thread join on time-out
        logging.debug("Waiting %s for join", str(self.__maxRunTime))
        thread.join(self.__maxRunTime)
        # if the thread is still alive, runners are still active
        if thread.is_alive():
            # if the thread is still running, terminate all runners
            logging.warning(
                "Test case thread still alive after maximum run time, terminating processes"
            )
            for rt in list(self.__threads):
                # implicit call to Program().terminate, __keepsync__ cleans up lingering threads
                if rt.is_alive():
                    self.__threads[rt].terminate()
            raise RuntimeError(
                "Execution of test timed out (s) > " + str(self.__maxRunTime)
            )

    # get errors from Test Case
    # output: list of Errors (type), can be None
    def getErrors(self):
        return self.__errors

        # prepare programs from configuration

    def __initializeProgramList__(self):
        self.__programs = []
        # programs are loaded by the manager
        shellArguments = " ".join(self.__config.shell_arguments)
        shell = self.__config.shell
        for pcnf in self.__config.program_configs:
            # get the copy of the original program
            p = copy.deepcopy(Programs().get(pcnf.name))
            # Combine the program workdir with the testcase workdir
            if pcnf.working_directory:
                pcnf.working_directory = Paths().mergePathElements(
                    self.__config.absolute_test_case_path,
                    pcnf.working_directory,
                )
            else:
                pcnf.working_directory = self.__config.absolute_test_case_path
            # overwrite run configuration with given overrides
            pcnf.shell_arguments = shellArguments
            pcnf.shell = shell
            p.overwriteConfiguration(pcnf)
            # add runner sequence number and runner configuration to local storage
            self.__programs.append([pcnf.sequence, p])

            # execute the runners

    def __execute__(self):
        # prepare presets for testbench run file
        inputfiles = {}
        size = 0
        # collect all initial files in the working directory before running
        for infile in os.listdir(self.__config.absolute_test_case_path):
            inputfiles[infile] = os.path.getmtime(
                os.path.join(self.__config.absolute_test_case_path, infile)
            )
            size = size + os.path.getsize(
                os.path.join(self.__config.absolute_test_case_path, infile)
            )
        start_time = time.time()
        logging.debug("Test case start time %s", str(time.ctime(int(start_time))))
        # execute all programs, subprocess
        errors = self.__keepsync__()
        # create testbench run file
        elapsed_time = time.time() - start_time
        self.__config.run_time = elapsed_time
        logging.debug("Test case elapsed time %s", str(elapsed_time))
        logging.debug("Creating _tb3_char.run for test case")
        with open(self.__config.run_file_name, "w") as runfile:
            runfile.write("Start_size:" + str(size) + "\n")
            runfile.write("Runtime:" + str(elapsed_time) + "\n")
            for error in errors:
                runfile.write("Error:" + str(error) + "\n")
            for allfile in os.listdir(self.__config.absolute_test_case_path):
                # collect all added and changed files in the working directory (after running, compare to initial list)
                if allfile not in {}.fromkeys(inputfiles, 0):
                    runfile.write("Output_added:" + str(allfile) + "\n")
                    size = size + os.path.getsize(
                        os.path.join(self.__config.absolute_test_case_path, allfile)
                    )
                else:
                    ftime = os.path.getmtime(
                        os.path.join(self.__config.absolute_test_case_path, allfile)
                    )
                    if ftime != inputfiles[allfile]:
                        runfile.write("Output_changed:" + str(allfile) + "\n")
            runfile.write("End_size:" + str(size) + "\n")

        if len(errors) > 0:
            self.__errors = errors

    # keep threads of runners synchronized, ordered by optional sequence and optional delayed start
    def __keepsync__(self):
        errors = []
        # group the runners by sorted sequence number
        for k, g in groupby(
            sorted(self.__programs, key=lambda p: p[0]), lambda x: x[0]
        ):
            if len(errors) > 0:
                for error in errors:
                    logging.error("Error occurred: %s", error)
                break
            # dictionary needed for calling terminate
            logging.debug("Creating thread group %s", str(k))
            self.__threads = {}
            # start threads in specific group
            for program in g:
                logging.debug("Adding thread for %s", program[1].getName())
                thread = threading.Thread(target=program[1].run)
                self.__threads[thread] = program[1]
                thread.start()
            # wait for threads to finish, threads are removed from list when finished
            while len(self.__threads) != 0:
                for thread in list(self.__threads):
                    if not thread.is_alive():
                        # add errors in thread to return variable
                        thread.join()
                        if self.__threads[thread].getError():
                            errors.append(self.__threads[thread].getError())
                        # remove kv-pair from dictionary
                        logging.debug("Removing thread")
                        self.__threads.pop(thread)
        return errors

    # retrieve runtime or none from _tb3_char.run file
    # input: path to _tb3_char.run file
    # output: actual runtime value (float)
    def __findCharacteristicsRunTime__(self, filename):
        with open(filename) as f:
            retval = None
            for line in f:
                if "Runtime:" in line:
                    _, value = line.split(":")
                    retval = float(value)
                    break
            return retval
