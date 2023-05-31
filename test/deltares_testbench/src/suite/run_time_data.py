"""
Description: Run time input and output resolver
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import threading

from src.utils.common import Singleton


# Test runner that runs a (part of) references or comparisons
class RunTimeData(object):
    # set as singleton
    __metaclass__ = Singleton

    __lock = threading.Lock()
    __outputs = {}

    def addOutput(self, program, outp):
        self.__lock.acquire()
        try:
            self.__outputs[program] = outp
        finally:
            self.__lock.release()

    def getOutput(self, program):
        rtn = None
        self.__lock.acquire()
        try:
            rtn = self.__outputs[program]
        finally:
            self.__lock.release()
        return rtn

    def getOutputByName(self, name):
        rtn = None
        self.__lock.acquire()
        try:
            for program in self.__outputs:
                if program.getName() == name:
                    rtn = self.__outputs[program]
                    break
        finally:
            self.__lock.release()
        return rtn

    def clean(self):
        rtn = None
        self.__lock.acquire()
        try:
            self.__outputs = {}
        finally:
            self.__lock.release()
        return rtn

    def dump(self):
        rtn = None
        self.__lock.acquire()
        try:
            for program in self.__outputs:
                logging.debug(
                    "\n\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                )
                logging.debug(
                    "RunTimeData Dump of program " + str(program.getName()) + " :\n"
                )
                logging.debug(str(self.__outputs[program]))
                logging.debug(
                    "\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n\n"
                )
        finally:
            self.__lock.release()
        return rtn
