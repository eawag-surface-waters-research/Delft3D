"""
Description: Common static functions
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import logging.handlers as handlers
import os
import platform
import string
import subprocess
import tempfile
from os import path as p

from src.config.credentials import Credentials
from src.utils.paths import Paths

if platform.system() == "Windows":
    try:
        from ctypes import windll
    except:
        logging.warning("Unable to import windll")

# initialize the root logger and set default logging level
root_logger = logging.getLogger("")
root_logger.setLevel(logging.DEBUG)


# Formatter around the default logging.Formatter,
# especially for TeamCity messages
# It mainly uses the default logging.Formatter,
# and additional performs a stripEscapeCharacters call on the msg string and all string arguments
class LogFormatter(logging.Formatter):
    __defaultFormatter = None  # Referencing the default logging.Formatter

    def __init__(self, formatString):
        # Call the default logging.Formatter constructor and store the instance in self.__defaultFormatter
        self.__defaultFormatter = logging.Formatter(formatString)

    def format(self, record):
        # Replace msg by stripEscapeCharacters(msg)
        record.msg = stripEscapeCharacters(record.msg).strip()
        # args is a tuple and can not be changed element by element
        # So, create a list, newArgs, containing all elements of args,
        # where all string elements are modified by stripEscapeCharacters
        # The replace args by the list newArgs, converted to tuple
        newArgs = []
        for i in range(0, len(record.args)):
            if isinstance(record.args[i], str):
                newArgs.append(stripEscapeCharacters(record.args[i]))
            else:
                newArgs.append(record.args[i])
        record.args = tuple(newArgs)
        # Finally, call the defaultFormatter.format with the changed record
        return self.__defaultFormatter.format(record)


# add search path to environment
# input: environment to add search path to, path
def addSearchPath(environment, sp):
    # Return immediately when ".svn" is in the path
    if str(sp).find(".svn") != -1:
        return
    spth = Paths().rebuildToLocalPath(sp)
    if platform.system() == "Windows":
        logging.debug("Adding windows search path %s", spth)
        environment["PATH"] = spth + ";" + environment["PATH"]
    else:
        logging.debug("Adding linux search path %s", spth)
        environment["LD_LIBRARY_PATH"] = spth + ":" + environment["LD_LIBRARY_PATH"]
        environment["PATH"] = spth + ":" + environment["PATH"]


# strip all characters from a string that need to be escaped for TeamCity
def stripEscapeCharacters(cstr):
    if not cstr or cstr == "":
        return ""
    vstr = (
        str(cstr)
        .replace("\r", "")
        .replace("\n", "")
        .replace("'", "")
        .replace('"', "")
        .replace("|", "")
        .replace("[", "")
        .replace("]", "")
    )
    # The decode method messes up the string completely on Linux
    # vstr = vstr.decode('utf-8')
    return vstr


# Replace a word by "***" when it follows a word containing the string "password"
# Needed when logging a command containing a password
def stripPassword(cstr):
    if not cstr or cstr == "":
        return ""
    splitchar = " "
    words = str(cstr).split(splitchar)
    i = 0
    for aword in words:
        if aword.find("password") > -1:
            words[i + 1] = "******"
        i += 1
    vstr = splitchar.join(words)
    # The decode method messes up the string completely on Linux
    # vstr = vstr.decode('utf-8')
    return vstr


# parse log level string to enum
def getLogLevel(level):
    if not level or str(level) == "":
        return logging.DEBUG
    if "info" in str(level).lower():
        return logging.INFO
    if "warn" in str(level).lower():
        return logging.WARNING
    if "err" in str(level).lower():
        return logging.ERROR
    return logging.DEBUG


# create a system logger (log to file)
def setLogFileHandler():
    testbench_folder = p.join(p.dirname(p.realpath(__file__)), "..", "..")
    log_folder = p.join(testbench_folder, "logs")

    if not p.exists(log_folder):
        os.mkdir(log_folder)

    handler = handlers.RotatingFileHandler(
        p.join(log_folder, "testbench.log"), backupCount=10
    )
    handler.doRollover()
    handler.setLevel(logging.DEBUG)
    formatter = LogFormatter(
        "%(asctime)s [%(levelname)-7s] %(module)s.%(funcName)s : %(message)s"
    )
    handler.setFormatter(formatter)
    root_logger.addHandler(handler)


# set default logging output to TeamCity logging
def setTcLogHandler(level):
    handlers = root_logger.handlers
    for handler in handlers:
        root_logger.removeHandler(handler)
    ch = logging.StreamHandler()
    ch.setLevel(level)
    formatter = LogFormatter(
        "##teamcity[message text='%(levelname)s - %(module)s.%(funcName)s : %(message)s']"
    )
    ch.setFormatter(formatter)
    root_logger.addHandler(ch)
    root_logger.isEnabledFor(level)
    setLogFileHandler()


# set default logging output to raw message logging
def setRawLogHandler(level):
    handlers = root_logger.handlers
    for handler in handlers:
        root_logger.removeHandler(handler)
    ch = logging.StreamHandler()
    ch.setLevel(level)
    formatter = LogFormatter(
        "%(asctime)s [%(levelname)-7s] %(module)s.%(funcName)s : %(message)s"
    )
    ch.setFormatter(formatter)
    root_logger.addHandler(ch)
    setLogFileHandler()


# attach a logger that appends output to a log file, specified by path
def attachFileLogger(name, path, level=logging.DEBUG, propagate=False):
    instance_logger = logging.getLogger(name)
    instance_logger.propagate = propagate
    file_handler = logging.FileHandler(path)
    file_handler.setLevel(level)
    formatter = LogFormatter("%(message)s")
    file_handler.setFormatter(formatter)
    instance_logger.addHandler(file_handler)


# detach a file logger that has been attached
def detachFileLogger(name):
    for handler in logging.getLogger(name).handlers:
        logging.getLogger(name).removeHandler(handler)


# mount a network drive in linux or windows
# input: server name, folder name, optional credentials
# output: mount point, boolean specifying if we created it or if it already exists
def mountNetworkDrive(server, folder, credentials: Credentials):
    if platform.system() == "Windows":
        pmp = checkIfAlreadyMounted(server, folder)
        if pmp != "":
            return pmp, False
        dl = getAvailableWindowsDriveLetter()
        if not dl:
            raise OSError("no drive letters available")
        mountpoint = dl + ":"
        cmd = "net use " + mountpoint + " \\\\" + server + "\\" + folder
        if credentials:
            cmd = cmd + " /user:" + credentials.username + " " + credentials.password
    else:
        mountpoint = tempfile.mkdtemp()
        cmd = "mount -t smbfs //" + server + "/" + folder + " " + mountpoint
        if credentials:
            cmd = (
                cmd
                + " -o username="
                + credentials.username
                + ",password="
                + credentials.password
            )
    subprocess.check_call(cmd, shell=True)
    return mountpoint, True


# unmount a network drive in linux or windows
def unmountNetworkDrive(mountpoint):
    if platform.system() == "Windows":
        cmd = "net use " + mountpoint + " /DELETE /YES"
    else:
        cmd = "umount -l " + mountpoint
    subprocess.check_call(cmd, shell=True)


# detect available windows drive letter for mounting
def getAvailableWindowsDriveLetter():
    drives = []
    bitmask = windll.kernel32.GetLogicalDrives()
    for letter in string.uppercase:
        if bitmask & 1:
            drives.append(letter.lower())
        bitmask >>= 1
    for i in range(1, 26):
        val = string.lowercase[:26][-i]
        if not val in drives:
            return val
    return None


# check if a drive with folder is already mounted
def checkIfAlreadyMounted(server, folder):
    if platform.system() == "Windows":
        process = subprocess.Popen(["net", "use"], stdout=subprocess.PIPE)
        while True:
            line = process.stdout.readline()
            if line == "" and process.poll() != None:
                break
            if " \\\\" + server + "\\" + folder in line:
                return line[line.find(":") - 1 : line.find(":") + 1]
        (_, stderr) = process.communicate()
        if stderr:
            logging.error(stderr)
        process.stdout.close()
    return ""


# meta class for singleton type
class Singleton(type):
    _instances = {}

    # singleton constructor instance
    def __call__(self, *args, **kwargs):
        if self not in self._instances:
            self._instances[self] = super(Singleton, self).__call__(*args, **kwargs)
        return self._instances[self]
