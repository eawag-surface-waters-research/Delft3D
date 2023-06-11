"""
Description: Process runner for test suite
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import ctypes
import logging
import os
import platform
import re
import subprocess
import threading
import time
from datetime import datetime
from typing import Optional

from src.config.program_config import ProgramConfig
from src.suite.globals import Globals
from src.suite.run_time_data import RunTimeData
from src.utils.common import (
    Singleton,
    addSearchPath,
    attachFileLogger,
    detachFileLogger,
    stripPassword,
)
from src.utils.paths import Paths


# Process runner that runs a program (part of a test case)
class Program(object):
    # global variables
    __shell = None
    __process = None
    __error = None
    __arguments = []
    __stop = False

    # constructor
    # input: program configuration
    def __init__(self, pcnf: ProgramConfig):
        if not pcnf:
            raise RuntimeError("Cannot instantiate a program without a configuration")
        self.__pcnf = pcnf
        # register this application in the programs store
        Programs().add(self.__pcnf.name, self)

    # start a thread to run the program
    def run(self):
        # reset thread flags
        self.__stop = False
        thread = threading.Thread(target=self.__execute__)
        thread.start()
        # set thread join on time-out if one is given
        if self.__pcnf.max_run_time == 0:
            thread.join()
        else:
            logging.debug("Waiting %s for join", str(self.__pcnf.max_run_time))
            thread.join(self.__pcnf.max_run_time)
            # if the thread is still alive, runners are still active
            if thread.is_alive():
                # if the thread is still running, terminate all runners
                logging.warning(
                    "%s thread still alive after maximum run time, terminating processes",
                    self.__pcnf.name,
                )
                self.terminate()
                # end the thread
                thread.join()
                self.__error = RuntimeError(
                    "Execution of "
                    + self.__pcnf.name
                    + " timed out (s) > "
                    + str(self.__pcnf.max_run_time)
                )

    # [optional]
    # overwrite program configuration settings
    # input: (partial) program configuration
    def overwriteConfiguration(self, pcnf: ProgramConfig):
        if pcnf:
            # overwrite name if one is given
            if pcnf.name != "":
                self.__pcnf.name = pcnf.name
            # overwrite a shell if one is given
            if pcnf.shell:
                if pcnf.shell.working_directory:
                    pcnf.shell.working_directory = Paths().rebuildToLocalPath(
                        pcnf.shell.working_directory
                    )
                else:
                    pcnf.shell.working_directory = self.__pcnf.working_directory

                self.__pcnf.shell = pcnf.shell
            # overwrite arguments if they are given
            if len(pcnf.arguments) > 0:
                self.__pcnf.arguments = pcnf.arguments
            # overwrite shell arguments if they are given
            if len(pcnf.shell_arguments) > 0:
                self.__pcnf.shell_arguments = pcnf.shell_arguments
            # overwrite working directory if it is given
            if pcnf.working_directory:
                self.__pcnf.working_directory = pcnf.working_directory
            # add environment settings
            if len(pcnf.environment_variables) > 0:
                for ev in pcnf.environment_variables:
                    self.__pcnf.environment_variables[ev] = pcnf.environment_variables[
                        ev
                    ]
            if len(pcnf.search_paths) > 0:
                for sp in pcnf.search_paths:
                    self.__pcnf.search_paths.append(sp)
            if len(pcnf.modules) > 0:
                for mod in pcnf.modules:
                    self.__pcnf.modules.append(mod)
            # overwrite max run time if one is given
            if pcnf.max_run_time > 0:
                self.__pcnf.max_run_time = pcnf.max_run_time
            # overwrite delay if one is given
            if pcnf.delay > 0:
                self.__pcnf.delay = pcnf.delay
            # overwrite flags
            if pcnf.ignore_standard_error:
                self.__pcnf.ignore_standard_error = True
            if pcnf.ignore_return_value:
                self.__pcnf.ignore_return_value = True
            if pcnf.program_remove_quotes:
                self.__pcnf.program_remove_quotes = True
            if pcnf.shell_remove_quotes:
                self.__pcnf.shell_remove_quotes = True

    # get the name of the Program
    # output: string
    def getName(self):
        return self.__pcnf.name

    # get the process id if one exists
    # output: PID or None
    def getPid(self):
        if self.__process and self.__process.poll():
            return self.__process.pid
        return None

    # return sub process errors if any
    # output: Error or None
    def getError(self):
        return self.__error

    # terminate the run
    def terminate(self):
        if not self.__stop:
            self.__stop = True
            #
            # It is important that all process activities are stopped
            # Checking whether the process still exists/ is active does
            # not seems to be necessary on Windows. So try it the rough way:
            # if self.__process and self.__process.poll():
            logging.debug("Regular process termination")
            try:
                self.__process.terminate()
            except:
                logging.debug("Termination generated error")
            # if self.__process and self.__process.poll():
            logging.debug("Regular process killing")
            try:
                self.__process.kill()
            except:
                logging.debug("Process kill generated error")

    # run given configuration
    def __execute__(self):
        try:
            logging.debug(
                "Starting %s with delay %s",
                self.__pcnf.absolute_bin_path,
                str(self.__pcnf.delay),
            )
            time.sleep(self.__pcnf.delay)
            execmd = self.__buildExeCommand__()
            # Don't display the Windows GPF dialog if the invoked program dies.
            if platform.system() == "Windows":
                # http://msdn.microsoft.com/en-us/library/windows/desktop/ms680621(v=vs.85).aspx
                # 1 : SEM_FAILCRITICALERRORS = 0x0001
                # 2 : SEM_NOGPFAULTERRORBOX = 0x0002
                # 3 : SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX = 0x0003
                # 4 : SEM_NOALIGNMENTFAULTEXCEPT = 0x0004
                # 8 : SEM_NOOPENFILEERRORBOX = 0x8000
                # !! NOTE : that we are not able to suppress debug build errors of the runtime, only release builds
                ctypes.windll.kernel32.SetErrorMode(0x8003)  # @UndefinedVariable
            if self.__stop:
                logging.debug("Received terminate, stopped before starting process.")
            else:
                logging.info("Executing :: %s", str(stripPassword(execmd)))
                logging.debug(
                    "Executing (in separate thread)::%s::in directory::%s",
                    str(stripPassword(execmd)),
                    self.__pcnf.working_directory,
                )

                program_env = self.__pcnf.environment
                tb_root = Globals().get("TestBenchRoot")
                if tb_root is not None:
                    program_env["TestBenchRoot"] = tb_root
                self.__process = subprocess.Popen(
                    execmd,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    cwd=self.__pcnf.working_directory,
                    env=program_env,
                )
                logging.debug("Subprocess created")
                # clear previous errors if they exist and create output variable
                self.__error = None
                out = ""
                # write output while executing, optionally create a log file
                unique_name = "%s-%s-%s" % (
                    self.__pcnf.name,
                    datetime.now().strftime("%y%m%d_%H%M%S"),
                    str(self.__process.pid),
                )
                log_file = os.path.abspath(
                    os.path.join(
                        str(self.__pcnf.working_directory), ("%s.log" % (unique_name))
                    )
                )
                if self.__pcnf.log_output_to_file:
                    logging.debug("Program output will be written to: %s" % log_file)
                    attachFileLogger(unique_name, log_file)
                # needed for showing output while running
                while not self.__stop:
                    line = self.__process.stdout.readline().decode("utf-8")
                    if line == "" and self.__process.poll() != None:
                        break
                    elif line != "":
                        out += line
                        logging.getLogger(unique_name).debug(line)
                if self.__pcnf.log_output_to_file:
                    detachFileLogger(unique_name)
                # catch errors
                (_, stderr) = self.__process.communicate()
                # store output
                if self.__pcnf.store_output:
                    RunTimeData().addOutput(self, out)
                # check results
                if self.__process.returncode != 0:
                    if self.__pcnf.ignore_return_value:
                        logging.debug(
                            "%s generated non-null error code %s, but ignoring it",
                            str(self.__pcnf.path),
                            str(self.__process.returncode),
                        )
                    else:
                        logging.warning(
                            "%s generated non-null error code %s",
                            str(self.__pcnf.path),
                            str(self.__process.returncode),
                        )
                        self.__error = subprocess.CalledProcessError(
                            self.__process.returncode,
                            self.__pcnf.path,
                            "process raised non null error code",
                        )
                if stderr:
                    if self.__pcnf.ignore_standard_error:
                        logging.debug(
                            "%s contained error message - %s, but ignoring it",
                            str(self.__pcnf.path),
                            str(stderr.rstrip()).replace("'", ""),
                        )
                    else:
                        logging.warning(
                            "%s contained error message - %s",
                            str(self.__pcnf.path),
                            str(stderr).rstrip().replace("'", ""),
                        )
                        self.__error = subprocess.CalledProcessError(
                            -1, self.__pcnf.path, str(stderr).rstrip()
                        )
                self.__process.stdout.close()
                if platform.system() == "Windows":
                    # clean up lingering subprocess
                    self.terminate()
        except Exception as e:
            self.__error = str(e)

    # create a command string for either windows or linux
    def __buildExeCommand__(self):
        logging.debug("Building command to be executed")
        if platform.system() != "Windows":
            if len(self.__pcnf.modules) > 0 or (
                self.__pcnf.shell and len(self.__pcnf.shell.modules) > 0
            ):
                logging.info(
                    "found modules to load, and on a non-windows system, trying to load module shell"
                )
                if not "MODULEPATH" in os.environ:
                    f = open(os.environ["MODULESHOME"] + "/init/.modulespath", "r")
                    path = []
                    for line in f.readlines():
                        line = re.sub("#.*$", "", line)
                        if line != "":
                            path.append(line)
                    os.environ["MODULEPATH"] = ":".join(path)
                if not "LOADEDMODULES" in os.environ:
                    os.environ["LOADEDMODULES"] = ""
                # initialize the modules environment
                if self.__pcnf.shell:
                    logging.info("found shell modules, loading them first")
                    for mod in self.__pcnf.shell.modules:
                        logging.debug("loading %s" % mod)
                        (output, error) = subprocess.Popen(
                            ["/usr/bin/modulecmd", "python", "load", mod],
                            stdout=subprocess.PIPE,
                        ).communicate()
                        exec(output)
                        if error:
                            logging.error(error)
                logging.debug("loading program modules")
                for mod in self.__pcnf.modules:
                    logging.debug("loading %s" % mod)
                    (output, error) = subprocess.Popen(
                        ["/usr/bin/modulecmd", "python", "load", mod],
                        stdout=subprocess.PIPE,
                    ).communicate()
                    exec(output)
                    if error:
                        logging.error(error)
                logging.debug("done loading modules")
        # create a new working environment
        self.__pcnf.environment = os.environ.copy()
        if "LD_LIBRARY_PATH" in self.__pcnf.environment.keys():
            sep = ":"
            if platform.system() == "Windows":
                sep = ";"
            self.__pcnf.environment["LD_LIBRARY_PATH"] = "%s%s." % (
                self.__pcnf.environment["LD_LIBRARY_PATH"],
                sep,
            )
        else:
            self.__pcnf.environment["LD_LIBRARY_PATH"] = "."
        logging.debug(
            "LD_LIBRARY_PATH set to %s", self.__pcnf.environment["LD_LIBRARY_PATH"]
        )
        # add environment variables
        for ev in self.__pcnf.environment_variables:
            logging.debug(
                "Adding environment variable %s : %s",
                ev,
                self.__pcnf.environment_variables[ev][1],
            )
            self.__pcnf.environment[ev] = self.__insertOutputVariable__(
                self.__pcnf.environment_variables[ev][1]
            )
        for sp in self.__pcnf.search_paths:
            addSearchPath(self.__pcnf.environment, sp)
        # start with the binary path
        cmdAndArgs = str(self.__pcnf.absolute_bin_path)
        if platform.system() == "Windows":
            # Needed when the path contains spaces
            cmdAndArgs = '"' + cmdAndArgs + '"'
        # add the given arguments
        for arg in self.__pcnf.arguments:
            a = str(arg)
            # Add quotes around the argument when it is a path,
            # unless a flag is used to switch this off
            if Paths().isPath(arg):
                a = Paths().rebuildToLocalPath(a)
                if (
                    not self.__pcnf.shell and not self.__pcnf.program_remove_quotes
                ) or (self.__pcnf.shell and self.__pcnf.shell.shell_remove_quotes):
                    a = '"' + a + '"'
            cmdAndArgs += " " + a
        # replace argument variables containing [output()] variable and others with actual value
        cmdAndArgs = self.__insertOutputVariable__(cmdAndArgs)
        # if a shell has been specified we need to reformat the command string
        if self.__pcnf.shell:
            # add shell environment variables to the working environment
            for ev in self.__pcnf.shell.environment_variables:
                logging.debug(
                    "adding shell environment variable %s : %s",
                    ev,
                    self.__pcnf.shell.environment_variables[ev][1],
                )
                self.__pcnf.environment[ev] = self.__insertOutputVariable__(
                    self.__pcnf.shell.environment_variables[ev][1]
                )
            for sp in self.__pcnf.shell.search_paths:
                addSearchPath(self.__pcnf.environment, sp)
            # format shell command
            shlAndArgs = str(self.__pcnf.shell.absolute_bin_path)
            LocalShellArgument = self.__pcnf.shell_arguments
            if LocalShellArgument == "":
                for arg in self.__pcnf.shell.arguments:
                    a = str(arg)
                    if (
                        Paths().isPath(arg)
                        and not self.__pcnf.shell.shell_remove_quotes
                    ):
                        a = '"' + a + '"'
                    shlAndArgs += " " + a
            else:
                shlAndArgs += " " + LocalShellArgument
            shlAndArgs = self.__insertOutputVariable__(shlAndArgs)
            if platform.system() == "Windows":
                if self.__pcnf.shell.shell_remove_quotes:
                    # Example: 'cmd \c vs <infile'
                    return str(shlAndArgs + " " + cmdAndArgs)
                # Example: 'cmd \c "vs <infile"'
                return str(shlAndArgs + ' "' + cmdAndArgs + '"')
            else:
                execmd = shlAndArgs.strip().split()
                if self.__pcnf.shell.shell_remove_quotes:
                    # Example: ['bash', '-c', 'vs', '<infile']
                    execmd.extend(cmdAndArgs.split(" "))
                else:
                    # Example: ['bash', '-c', 'vs <infile']
                    execmd.append(cmdAndArgs)
                return execmd
        else:
            if platform.system() == "Windows":
                # Example: 'svn checkout -r head url targetdir'
                return cmdAndArgs
            else:
                # Example: ['svn', 'checkout', '-r', 'head', 'url', 'targetdir']
                return cmdAndArgs.split(" ")

    # replace an output variable and other with actual value
    # input: string to search in
    # return value: modified string
    def __insertOutputVariable__(self, original: str):
        # replace [output(some_name)] result from previous run some_name in command string
        retval = original
        if "[output(" in retval:
            sr = re.search(
                r"(?<=(\[output\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE
            )
            if sr and len(sr.groups()) > 0:
                k = retval.find("[output(")
                outstr = RunTimeData().getOutputByName(sr.group(0))
                logging.debug("substituted: %s", outstr)
                # retval = re.sub(r"\[output\((.*?)\)\]", outstr.encode('string_escape'), retval)
                outstr = os.path.join(outstr, retval[:k])
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]) :]
                retval = retval[3:]  # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logging.warning("Could not match %s as [output(var)]", original)
        if "[programpath(" in retval:
            sr = re.search(
                r"(?<=(\[programpath\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE
            )
            if sr and len(sr.groups()) > 0:
                k = retval.find("[programpath(")
                outstr = os.path.dirname(
                    Programs().get(sr.group(0)).__pcnf.absolute_bin_path
                )
                logging.debug("substituted: %s", outstr)
                outstr = retval[:k] + outstr
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]) :]
                retval = retval[3:]  # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logging.warning("Could not match %s as [programpath(var)]", original)
        if "[programGlobals(" in retval:
            sr = re.search(
                r"(?<=(\[programGlobals\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE
            )
            if sr and len(sr.groups()) > 0:
                k = retval.find("[programGlobals(")
                outstr = os.path.normpath(Globals().get(sr.group(0)))
                logging.debug("substituted: %s", outstr)
                # retval = re.sub(r"\[programGlobals\((.*?)\)\]", outstr.encode('string_escape'), retval)
                outstr = os.path.join(outstr, retval[:k])
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]) :]
                retval = retval[3:]  # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logging.warning("Could not match %s as [root(var)]", original)

        # call this routine again if there is still something to replace
        if "[programpath(" in retval:
            retval = self.__insertOutputVariable__(retval)
        return retval


# Collection of all registered programs
class Programs(object):
    # set as singleton
    __metaclass__ = Singleton

    __programs = {}
    __lock = threading.Lock()

    # add a program
    def add(self, name: str, program: Program):
        self.__lock.acquire()
        try:
            self.__programs[name] = program
        finally:
            self.__lock.release()

    # find a program by name
    def get(self, name: str) -> Optional[Program]:
        rtn = None
        self.__lock.acquire()
        try:
            rtn = self.__programs[name]
        finally:
            self.__lock.release()
        return rtn
