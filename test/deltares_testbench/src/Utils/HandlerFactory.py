'''
Description: Handler Factory
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os, logging
from distutils import dir_util
from src.Utils.Paths import Paths
from src.Config.Handler import Handler
from src.Utils.ResolveHandler import ResolveHandler
from src.Utils.FtpHandler import FTPHandler
from src.Utils.SvnHandler import SvnHandler
from src.Utils.HttpHandler import HTTPHandler
from src.Utils.LocalNetHandler import LocalNetHandler
from src.Utils.Unzipper import Unzipper


# Chooses which type of handler is used for upload and download actions
class HandlerFactory(object):

    def prepare_upload(self, frompath, topath, credentials=None):
        handler = ResolveHandler().detect(topath, credentials)
        rfp = Paths().rebuildToLocalPath(frompath)
        hi = None
        if handler == Handler.WEB:
            logging.debug("using HTTP handler for %s", topath)
            hi = HTTPHandler()
        if handler == Handler.SVN:
            logging.debug("using SVN handler for %s", topath)
            hi = SvnHandler(False)
        if handler == Handler.FTP:
            logging.debug("using FTP handler for %s", topath)
            hi = FTPHandler()
        if handler == Handler.NET or handler == Handler.PATH:
            logging.debug("using LocalNet handler for %s", topath)
            hi = LocalNetHandler()
        if handler == Handler.NONE:
            raise AttributeError("upload :: no type specified")
        if hi:
            hi.prepare_upload(rfp, topath, credentials)

    # Upload data to location
    # input: from, to (assumes this is network) and optional credentials
    def upload(self, frompath, topath, credentials=None, autocommit=False):
        handler = ResolveHandler().detect(topath, credentials)
        rfp = Paths().rebuildToLocalPath(frompath)
        hi = None
        if handler == Handler.WEB:
            logging.debug("using HTTP handler for %s", topath)
            hi = HTTPHandler()
        if handler == Handler.SVN:
            logging.debug("using SVN handler for %s", topath)
            hi = SvnHandler(autocommit)
        if handler == Handler.FTP:
            logging.debug("using FTP handler for %s", topath)
            hi = FTPHandler()
        if handler == Handler.NET or handler == Handler.PATH:
            logging.debug("using LocalNet handler for %s", topath)
            hi = LocalNetHandler()
        if handler == Handler.NONE:
            raise AttributeError("upload :: no type specified")
        if hi:
            hi.upload(rfp, topath, credentials)

    # Download data from location
    # input: from, to and optional credentials
    def download(self, frompath, topath, credentials=None, version=None, unzip=False, autocommit=False):
        handler = ResolveHandler().detect(frompath, credentials)
        rtp = Paths().rebuildToLocalPath(topath)
        if os.path.exists(rtp):
            dir_util.remove_tree(rtp)
        os.makedirs(rtp)
        hi = None
        if handler == Handler.WEB:
            logging.debug("using HTTP handler for %s", frompath)
            hi = HTTPHandler()
        if handler == Handler.SVN:
            logging.debug("using SVN handler for %s", frompath)
            hi = SvnHandler(autocommit)
        if handler == Handler.FTP:
            logging.debug("using FTP handler for %s", frompath)
            hi = FTPHandler()
        if handler == Handler.NET or handler == Handler.PATH:
            logging.debug("using LocalNet handler for %s", frompath)
            hi = LocalNetHandler()
        if handler == Handler.NONE:
            raise AttributeError("download :: no type specified")
        if hi:
            try:
                hi.download(frompath, rtp, credentials, version)
            except Exception as e:
                raise e
        if unzip:
            Unzipper().recursive(rtp)
