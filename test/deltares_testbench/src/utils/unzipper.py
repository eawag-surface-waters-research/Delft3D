"""
Description: file unzipper
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import fnmatch
import logging
import os
import zipfile


class Unzipper(object):
    """compare files on ASCII content equality

    Args:
       object (_type_): _description_
    """

    def __unzip__(self, zip_file_path: str):
        """unzip file to path

        Args:
            zfp (str): full path to zip file name
        """
        fh = open(zip_file_path, "rb")
        z = zipfile.ZipFile(fh)
        z.extractall(os.path.dirname(zip_file_path))
        fh.close()

    def recursive(self, path: str):
        """unzip all zip files in directory (recursive)

        Args:
            path (str): path to zip file
        """
        matches = []
        for dirpath, _, filenames in os.walk(path):
            for f in fnmatch.filter(filenames, "*.zip"):
                matches.append(os.path.abspath(os.path.join(dirpath, f)))
        for m in matches:
            logging.debug("unzipping %s", m)
            self.__unzip__(m)
