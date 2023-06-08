"""
Description: Resolve Handler helper
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import re
import ssl
import urllib.parse as parse
import urllib.request as url_lib
from urllib.error import HTTPError, URLError

from src.config.credentials import Credentials
from src.config.types.handler_type import HandlerType


# Detect type of handler behind a path
class ResolveHandler(object):
    # detect which protocol handler is needed for the path
    # input: path to analyze
    # output: Handler Type
    def detect(self, path, credentials=None):
        logging.debug("detecting handler for %s", path)
        # assume network path starts with either [//] or [\\]
        match = re.search(r"^\\(\\)?[A-Za-z0-9]+|^\/\/[A-Za-z0-9]+", path)
        if match:
            return HandlerType.NET
        # assume local path handler [X:\] or [/]
        match = re.search(r"[A-Za-z]{1}\:\\|^\/{1}[A-Za-z0-9]|\.\.", path)
        if match:
            return HandlerType.PATH
        # identify ftp handler
        match = re.search(r"^ftp(s)?://", path)
        if match:
            return HandlerType.FTP
        r = self.__tryopen__(path, credentials)
        if r:
            return r
        return HandlerType.NONE

    # Try to open http connections to detect protocol header (recursive analysis to root of path)
    # input: network path, credentials
    # output: Handler type
    def __tryopen__(self, path, credentials: Credentials):
        if credentials:
            password_mgr = url_lib.HTTPPasswordMgrWithDefaultRealm()
            scheme, netloc, _, _, _, _ = parse.urlparse(path)
            password_mgr.add_password(
                None,
                scheme + "://" + netloc + "/",
                credentials.username,
                credentials.password,
            )
            handler = url_lib.HTTPBasicAuthHandler(password_mgr)
            # due to failing SSL certificate (some ICT updates) we disable certificate validation
            if hasattr(ssl, "create_default_context"):
                ctx = ssl.create_default_context()
                ctx.check_hostname = False
                ctx.verify_mode = ssl.CERT_NONE
                ssl_handler = url_lib.HTTPSHandler(context=ctx)
                opener = url_lib.build_opener(handler, ssl_handler)
            else:
                opener = url_lib.build_opener(handler)

            url_lib.install_opener(opener)
        try:
            logging.debug("Trying to urlopen %s", path)
            response = url_lib.urlopen(path)
            data = response.read().decode("utf-8")
            if "<!doctype svn" in data.lower():
                return HandlerType.SVN
            if "<!doctype html" in data.lower():
                return HandlerType.WEB
        except HTTPError as e:
            if e.code == 401:
                logging.error("Authentication error")
                if credentials:
                    logging.error("Credentials missing!")
            else:
                logging.warning(
                    "The server could not fulfill the request (%s). Error code: %s",
                    path,
                    e.code,
                )
                if path.count("/") > 2:
                    newpath = path[: path.rfind("/")]
                    return self.__tryopen__(newpath, credentials)
        except URLError as e:
            logging.error("Failed to reach server (%s). Reason: %s", path, e.reason)
        except Exception as e:
            logging.debug(e)
