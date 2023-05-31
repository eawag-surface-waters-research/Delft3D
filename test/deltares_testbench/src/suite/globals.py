"""
Description: Global variables
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import threading

from src.utils.common import Singleton


# Collection of all registered programs
class Globals(object):
    # set as singleton
    __metaclass__ = Singleton
    __tbglobals = dict()
    __lock = threading.Lock()

    def __init__(self):
        pass

    def add(self, name, globalstring):
        self.__lock.acquire()
        try:
            self.__tbglobals[name] = globalstring
        finally:
            self.__lock.release()

    # find a program by name
    def get(self, name):
        rtn = None
        self.__lock.acquire()
        try:
            rtn = self.__tbglobals[name]
        finally:
            self.__lock.release()
        return rtn
