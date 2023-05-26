'''
Description: Process runner for test suite
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os
import subprocess
import threading
import ctypes
import time
import logging
import platform
import re
from datetime import datetime
from src.Utils.Paths import Paths
from src.Suite.RunTimeData import RunTimeData
from src.Suite.Globals import Globals
from src.Utils.Common import Singleton, addSearchPath, attachFileLogger, detachFileLogger, stripPassword


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
    def __init__(self, pcnf):
        if not pcnf:
            raise RuntimeError("Cannot instantiate a program without a configuration")
        self.__pcnf = pcnf
        # register this application in the programs store
        Programs().add(self.__pcnf.getName(), self)

    # start a thread to run the program
    def run(self):
        # reset thread flags
        self.__stop = False
        thread = threading.Thread(target=self.__execute__)
        thread.start()
        # set thread join on time-out if one is given
        if self.__pcnf.getMaxRunTime() == 0:
            thread.join()
        else:
            logging.debug("Waiting %s for join", str(self.__pcnf.getMaxRunTime()))
            thread.join(self.__pcnf.getMaxRunTime())
            # if the thread is still alive, runners are still active
            if thread.is_alive():
                # if the thread is still running, terminate all runners
                logging.warning("%s thread still alive after maximum run time, terminating processes", self.__pcnf.getName())
                self.terminate()
                # end the thread
                thread.join()
                self.__error = RuntimeError( "Execution of " + self.__pcnf.getName() + " timed out (s) > " + str(self.__pcnf.getMaxRunTime()))

    # [optional]
    # overwrite program configuration settings
    # input: (partial) program configuration
    def overwriteConfiguration(self, pcnf):
        if pcnf:
            # overwrite name if one is given
            if pcnf.getName() != "":
                self.__pcnf.setName(pcnf.getName())
            # overwrite a shell if one is given
            if pcnf.getShell():
                if pcnf.getShell().getWorkingDirectory():
                    pcnf.getShell().setWorkingDirectory(Paths().rebuildToLocalPath(pcnf.getShell().getWorkingDirectory()))
                else:
                    pcnf.getShell().setWorkingDirectory(self.__pcnf.getWorkingDirectory())
                self.__pcnf.setShell(pcnf.getShell())
            # overwrite arguments if they are given
            if len(pcnf.getArguments()) > 0:
                self.__pcnf.setArguments(pcnf.getArguments())
            # overwrite shell arguments if they are given
            if len(pcnf.getShellArguments()) > 0:
                self.__pcnf.setShellArguments(pcnf.getShellArguments())
            # overwrite working directory if it is given
            if pcnf.getWorkingDirectory():
                self.__pcnf.setWorkingDirectory(pcnf.getWorkingDirectory())
            # add environment settings
            if len(pcnf.getEnvironmentVariables()) > 0:
                for ev in pcnf.getEnvironmentVariables():
                    self.__pcnf.getEnvironmentVariables()[ev] = pcnf.getEnvironmentVariables()[ev]
            if len(pcnf.getSearchPaths()) > 0:
                for sp in pcnf.getSearchPaths():
                    self.__pcnf.getSearchPaths().append(sp)
            if len(pcnf.getModules()) > 0:
                for mod in pcnf.getModules():
                    self.__pcnf.getModules().append(mod)
            # overwrite max run time if one is given
            if pcnf.getMaxRunTime() > 0:
                self.__pcnf.setMaxRunTime(pcnf.getMaxRunTime())
            # overwrite delay if one is given
            if pcnf.getDelay() > 0:
                self.__pcnf.setDelay(pcnf.getDelay())
            # overwrite flags
            if pcnf.getIgnoreStandardError():
                self.__pcnf.setIgnoreStandardError(True)
            if pcnf.getIgnoreReturnValue():
                self.__pcnf.setIgnoreReturnValue(True)
            if pcnf.getProgramRemoveQuotes():
                self.__pcnf.setProgramRemoveQuotes(True)
            if pcnf.getShellRemoveQuotes():
                self.__pcnf.setShellRemoveQuotes(True)

    # get the name of the Program
    # output: string
    def getName(self):
        return self.__pcnf.getName()

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
            logging.debug("Starting %s with delay %s", self.__pcnf.getAbsoluteBinPath(), str(self.__pcnf.getDelay()))
            time.sleep(self.__pcnf.getDelay())
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
                logging.debug("Executing (in separate thread)::%s::in directory::%s", str(stripPassword(execmd)), self.__pcnf.getWorkingDirectory())

                program_env = self.__pcnf.getEnvironment()
                tb_root = Globals().get("TestBenchRoot")
                if tb_root is not None:
                    program_env["TestBenchRoot"] = tb_root
                self.__process = subprocess.Popen(execmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=self.__pcnf.getWorkingDirectory(), env=program_env)
                logging.debug("Subprocess created")
                # clear previous errors if they exist and create output variable
                self.__error = None
                out = ""
                # write output while executing, optionally create a log file
                unique_name = "%s-%s-%s" % (self.__pcnf.getName(), datetime.now().strftime("%y%m%d_%H%M%S"), str(self.__process.pid))
                log_file = os.path.abspath(os.path.join(self.__pcnf.getWorkingDirectory(), ("%s.log" % (unique_name))))
                if self.__pcnf.getLogOutputToFile():
                    logging.debug('Program output will be written to: %s' % log_file)
                    attachFileLogger(unique_name, log_file)
                # needed for showing output while running
                while not self.__stop:
                    line = self.__process.stdout.readline().decode('utf-8')
                    if line == "" and self.__process.poll() != None:
                        break
                    elif line != "":
                        out += line
                        logging.getLogger(unique_name).debug(line)
                if self.__pcnf.getLogOutputToFile():
                    detachFileLogger(unique_name)
                # catch errors
                (_, stderr) = self.__process.communicate()
                # store output
                if self.__pcnf.getStoreOutput():
                    RunTimeData().addOutput(self, out)
                # check results
                if self.__process.returncode != 0:
                    if self.__pcnf.getIgnoreReturnValue():
                        logging.debug("%s generated non-null error code %s, but ignoring it", str(self.__pcnf.getPath()), str(self.__process.returncode))
                    else:
                        logging.warning("%s generated non-null error code %s", str(self.__pcnf.getPath()), str(self.__process.returncode))
                        self.__error = subprocess.CalledProcessError(self.__process.returncode, self.__pcnf.getPath(), "process raised non null error code")
                if stderr:
                    if self.__pcnf.getIgnoreStandardError():
                        logging.debug("%s contained error message - %s, but ignoring it", str(self.__pcnf.getPath()), str(stderr.rstrip()).replace("'", ""))
                    else:
                        logging.warning("%s contained error message - %s", str(self.__pcnf.getPath()), str(stderr).rstrip().replace("'", ""))
                        self.__error = subprocess.CalledProcessError(-1, self.__pcnf.getPath(), str(stderr).rstrip())
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
            if len(self.__pcnf.getModules()) > 0 or (self.__pcnf.getShell() and len(self.__pcnf.getShell().getModules()) > 0):
                logging.info('found modules to load, and on a non-windows system, trying to load module shell')
                if not 'MODULEPATH' in os.environ:
                    f = open(os.environ['MODULESHOME'] + "/init/.modulespath", "r")
                    path = []
                    for line in f.readlines():
                        line = re.sub("#.*$", '', line)
                        if line != '':
                            path.append(line)
                    os.environ['MODULEPATH'] = ':'.join(path)
                if not 'LOADEDMODULES' in os.environ:
                    os.environ['LOADEDMODULES'] = ''
                # initialize the modules environment
                if self.__pcnf.getShell():
                    logging.info('found shell modules, loading them first')
                    for mod in self.__pcnf.getShell().getModules():
                        logging.debug('loading %s' % mod)
                        (output, error) = subprocess.Popen(['/usr/bin/modulecmd', 'python', 'load', mod], stdout=subprocess.PIPE).communicate()
                        exec(output)
                        if error:
                            logging.error(error)
                logging.debug('loading program modules')
                for mod in self.__pcnf.getModules():
                    logging.debug('loading %s' % mod)
                    (output, error) = subprocess.Popen(['/usr/bin/modulecmd', 'python', 'load', mod], stdout=subprocess.PIPE).communicate()
                    exec(output)
                    if error:
                        logging.error(error)
                logging.debug('done loading modules')
        # create a new working environment
        self.__pcnf.setEnvironment(os.environ.copy())
        if "LD_LIBRARY_PATH" in self.__pcnf.getEnvironment().keys():
            sep = ':'
            if platform.system() == "Windows":
                sep = ';'
            self.__pcnf.getEnvironment()["LD_LIBRARY_PATH"] = "%s%s." % (self.__pcnf.getEnvironment()["LD_LIBRARY_PATH"], sep)
        else:
            self.__pcnf.getEnvironment()["LD_LIBRARY_PATH"] = "."
        logging.debug('LD_LIBRARY_PATH set to %s', self.__pcnf.getEnvironment()["LD_LIBRARY_PATH"])	
        # add environment variables
        for ev in self.__pcnf.getEnvironmentVariables():
            logging.debug("Adding environment variable %s : %s", ev, self.__pcnf.getEnvironmentVariables()[ev][1])
            self.__pcnf.getEnvironment()[ev] = self.__insertOutputVariable__(self.__pcnf.getEnvironmentVariables()[ev][1])
        for sp in self.__pcnf.getSearchPaths():
            addSearchPath(self.__pcnf.getEnvironment(), sp)
        # start with the binary path
        cmdAndArgs = str(self.__pcnf.getAbsoluteBinPath())
        if platform.system() == "Windows":
            # Needed when the path contains spaces
            cmdAndArgs = '"' + cmdAndArgs + '"'
        # add the given arguments
        for arg in self.__pcnf.getArguments():
            a = str(arg)
            # Add quotes around the argument when it is a path,
            # unless a flag is used to switch this off
            if Paths().isPath(arg):
                a = Paths().rebuildToLocalPath(a)
                if (not self.__pcnf.getShell() and not self.__pcnf.getProgramRemoveQuotes()) \
                        or (self.__pcnf.getShell() and self.__pcnf.getShell().getShellRemoveQuotes()):
                    a = "\"" + a + "\""
            cmdAndArgs += " " + a
        # replace argument variables containing [output()] variable and others with actual value
        cmdAndArgs = self.__insertOutputVariable__(cmdAndArgs)
        # if a shell has been specified we need to reformat the command string
        if self.__pcnf.getShell():
            # add shell environment variables to the working environment
            for ev in self.__pcnf.getShell().getEnvironmentVariables():
                logging.debug("adding shell environment variable %s : %s", ev, self.__pcnf.getShell().getEnvironmentVariables()[ev][1])
                self.__pcnf.getEnvironment()[ev] = self.__insertOutputVariable__(self.__pcnf.getShell().getEnvironmentVariables()[ev][1])
            for sp in self.__pcnf.getShell().getSearchPaths():
                addSearchPath(self.__pcnf.getEnvironment(), sp)
            # format shell command
            shlAndArgs = str(self.__pcnf.getShell().getAbsoluteBinPath())
            LocalShellArgument = self.__pcnf.getShellArguments()
            if (LocalShellArgument == ""):
                for arg in self.__pcnf.getShell().getArguments():
                    a = str(arg)
                    if Paths().isPath(arg) and not self.__pcnf.getShell().getShellRemoveQuotes():
                        a = "\"" + a + "\""
                    shlAndArgs += " " + a
            else:
                shlAndArgs += " " + LocalShellArgument
            shlAndArgs = self.__insertOutputVariable__(shlAndArgs)
            if platform.system() == "Windows":
                if self.__pcnf.getShell().getShellRemoveQuotes():
                    # Example: 'cmd \c vs <infile'
                    return str(shlAndArgs + " " + cmdAndArgs)
                # Example: 'cmd \c "vs <infile"'
                return str(shlAndArgs + " \"" + cmdAndArgs + "\"")
            else:
                execmd = shlAndArgs.strip().split()
                if self.__pcnf.getShell().getShellRemoveQuotes():
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
    def __insertOutputVariable__(self, original):
        # replace [output(some_name)] result from previous run some_name in command string
        retval = original
        if "[output(" in retval:
            sr = re.search(r"(?<=(\[output\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE)
            if sr and len(sr.groups()) > 0:
                k = retval.find("[output(")
                outstr = RunTimeData().getOutputByName(sr.group(0))
                logging.debug("substituted: %s", outstr)
                # retval = re.sub(r"\[output\((.*?)\)\]", outstr.encode('string_escape'), retval)
                outstr = os.path.join(outstr, retval[:k])
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]):]
                retval = retval[3:] # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logging.warning("Could not match %s as [output(var)]", original)
        if "[programpath(" in retval:
            sr = re.search(r"(?<=(\[programpath\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE)
            if sr and len(sr.groups()) > 0:
                k = retval.find("[programpath(")
                outstr = os.path.dirname(Programs().get(sr.group(0)).__pcnf.getAbsoluteBinPath())
                logging.debug("substituted: %s", outstr)
                outstr = retval[:k] + outstr
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]):]
                retval = retval[3:] # ')]/'
                retval = os.path.join(outstr, retval)
            else:
                logging.warning("Could not match %s as [programpath(var)]", original)
        if "[programGlobals(" in retval:
            sr = re.search(r"(?<=(\[programGlobals\())(.*?)(?=\)\])", retval, flags=re.IGNORECASE)
            if sr and len(sr.groups()) > 0:
                k = retval.find("[programGlobals(")
                outstr = os.path.normpath(Globals().get(sr.group(0)))
                logging.debug("substituted: %s", outstr)
                # retval = re.sub(r"\[programGlobals\((.*?)\)\]", outstr.encode('string_escape'), retval)
                outstr = os.path.join(outstr, retval[:k])
                retval = retval[k:]
                for i in range(0, len(sr.groups())):
                    retval = retval[len(sr.groups()[i]):]
                retval = retval[3:] # ')]/'
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
    def add(self, name, program):
        self.__lock.acquire()
        try:
            self.__programs[name] = program
        finally:
            self.__lock.release()

    # find a program by name
    def get(self, name):
        rtn = None
        self.__lock.acquire()
        try:
            rtn = self.__programs[name]
        finally:
            self.__lock.release()
        return rtn
