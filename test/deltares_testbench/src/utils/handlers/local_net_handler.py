"""
Description: Local and Network handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging
import os
from distutils import dir_util

from src.config.credentials import Credentials
from src.config.types.handler_type import HandlerType
from src.utils.common import mountNetworkDrive, unmountNetworkDrive
from src.utils.handlers.i_handler import IHandler
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.paths import Paths


# Upload and download for local and network paths
class LocalNetHandler(IHandler):
    def prepare_upload(
        self, from_path: str, to_path: str, credentials: Credentials
    ) -> None:
        pass

    # Upload data to location
    # input: from, to (assumes this is network) and optional credentials
    def upload(self, from_path: str, to_path: str, credentials: Credentials) -> None:
        handler = ResolveHandler().detect(to_path, credentials)
        rfp = Paths().rebuildToLocalPath(from_path)
        if not os.path.exists(rfp):
            raise IOError("cannot upload from non-existent path %s", rfp)
        if handler == HandlerType.PATH:
            rtp = Paths().rebuildToLocalPath(to_path)
            logging.debug(
                "copying locally from %s to %s",
                os.path.abspath(rfp),
                os.path.abspath(rtp),
            )
            dir_util.copy_tree(os.path.abspath(rfp), os.path.abspath(rtp))
        if handler == HandlerType.NET:
            server, folder, rest = Paths().splitNetworkPath(to_path)
            mp, nm = mountNetworkDrive(server, folder, credentials)
            logging.debug("mounted share to %s", mp)
            e = None
            try:
                net_path = os.path.join(mp + os.sep, rest)
                logging.debug(
                    "copying to net from %s to %s",
                    os.path.abspath(rfp),
                    os.path.abspath(net_path),
                )
                dir_util.copy_tree(os.path.abspath(rfp), net_path)
            except Exception as e:
                logging.error(
                    "exception during network transfer %s", str(e).replace("'", "")
                )
            finally:
                if nm:
                    unmountNetworkDrive(mp)
                    logging.debug("unmounted share")
                    if e:
                        raise e

    # Download data from location
    # input: from, to and optional credentials
    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: str
    ):
        handler = ResolveHandler().detect(from_path, credentials)
        rtp = Paths().rebuildToLocalPath(to_path)
        if handler == HandlerType.PATH:
            rfp = Paths().rebuildToLocalPath(from_path)
            logging.debug(
                "copying locally from %s to %s",
                os.path.abspath(rfp),
                os.path.abspath(rtp),
            )
            dir_util.copy_tree(os.path.abspath(rfp), os.path.abspath(rtp))
        if handler == HandlerType.NET:
            server, folder, rest = Paths().splitNetworkPath(from_path)
            mp, nm = mountNetworkDrive(server, folder, credentials)
            logging.debug("mounted share to %s", mp)
            e = None
            try:
                netpath = os.path.join(mp + os.sep, rest)
                logging.debug(
                    "copying from net from %s to %s",
                    os.path.abspath(netpath),
                    os.path.abspath(rtp),
                )
                dir_util.copy_tree(netpath, os.path.abspath(rtp))
            except Exception as e:
                logging.error(
                    "exception during network transfer %s", str(e).replace("'", "")
                )
            finally:
                if nm:
                    unmountNetworkDrive(mp)
                    logging.debug("unmounted share")
                if e:
                    raise e
