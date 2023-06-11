"""
Description: FTP handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import os
import urllib.parse as parse
from ftplib import FTP, error_perm

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler


# Upload and download for ftp paths
class FTPHandler(IHandler):
    def prepare_upload(
        self, from_path: str, to_path: str, credentials: Credentials
    ) -> None:
        pass

    # Upload data to location
    # input: from, to and credentials
    def upload(self, from_path: str, to_path: str, credentials: Credentials) -> None:
        logging.debug("setting up connection to FTP: %s", to_path)
        url = parse.urlparse(to_path)
        ftp = FTP(url.netloc)
        if credentials:
            ftp.login(credentials.username, credentials.password)
            logging.debug("connecting as: %s", credentials.username)
        else:
            ftp.login()
        ftp.cwd(url.path)
        logging.debug("going to root: %s", url.path)
        self.__traverseDirectoryUpload__(ftp, from_path, "", url.path, "")
        ftp.close()

    # Download data from location
    # input: from, to and credentials
    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: str
    ):
        logging.debug("setting up connection to FTP: %s", from_path)
        url = parse.urlparse(from_path)
        ftp = FTP(url.netloc)
        if credentials:
            ftp.login(credentials.username, credentials.password)
            logging.debug("connecting as: %s", credentials.username)
        else:
            ftp.login()
        ftp.cwd(url.path)
        logging.debug("going to root: %s", url.path)
        # create root on filesystem
        if not os.path.exists(to_path):
            os.makedirs(to_path)
        logging.debug("analysing directory structure on ftp")
        self.__traverseDirectoryDownload__(ftp, "/", to_path)
        ftp.close()

    # recursive traverse, fills ftpdirs array
    # cwd is the local working directory
    # frompath is relative dir of local system
    # destination is abs dir of ftp
    # topath is relative dir of ftp
    def __traverseDirectoryUpload__(self, ftp, cwd, frompath, destination, topath):
        try:
            ftp.cwd(destination)
            if topath:
                ftp.cwd(topath)
            ndir = os.path.basename(os.path.normpath(frompath))
            ftp.mkd(ndir)
            logging.debug("built ftp frompath : %s", frompath.replace(os.sep, "/"))
        except error_perm:
            # invalid entry (ensure input form: "/dir/folder/something/")
            logging.debug("frompath %s already exists", frompath.replace(os.sep, "/"))
        # list children:
        filelist = os.listdir(os.path.join(cwd, frompath))
        for locfile in filelist:
            if os.path.isdir(os.path.join(cwd, frompath, locfile)):
                topath = "/"
                if frompath:
                    parts = os.path.split(frompath)
                    if parts:
                        for part in parts:
                            topath = topath + part + "/"
                self.__traverseDirectoryUpload__(
                    ftp, cwd, os.path.join(frompath, locfile), destination, topath
                )
            else:
                ftp.cwd("/" + destination + frompath.replace(os.sep, "/"))
                with open(os.path.join(cwd, frompath, locfile)) as lf:
                    ftp.storbinary("STOR " + locfile, lf)

                logging.debug("uploaded %s", locfile)
        return

    # recursive traverse download files
    # frompath is str of the form "/dir/folder/something/"
    # frompath should be the abs frompath to the root FOLDER of the file tree to download
    def __traverseDirectoryDownload__(self, ftp, path, destination):
        topath = destination + path.replace("/", os.sep)
        try:
            ftp.cwd(path)
            # clone path to destination
            os.makedirs(topath)
            logging.debug("built path : %s", topath)
        except OSError:
            # folder already exists at destination
            pass
        except error_perm:
            # invalid entry (ensure input form: "/dir/folder/something/")
            raise OSError("error: could not change to %s", path)
        # list children:
        filelist = ftp.nlst()
        for ftpfile in filelist:
            try:
                # this will check if ftpfile is folder:
                ftp.cwd(path + ftpfile + "/")
                # if so, explore it:
                self.__traverseDirectoryDownload__(
                    ftp, path + ftpfile + "/", destination
                )
            except error_perm:
                # possibly need a permission exception catch:
                with open(os.path.join(topath, ftpfile), "wb") as ff:
                    ftp.retrbinary("RETR " + ftpfile, ff.write)

                logging.debug("downloaded %s", ftpfile)
        return
