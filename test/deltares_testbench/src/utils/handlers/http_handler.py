"""
Description: HTTP handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import os
import urllib.parse as parse
import urllib.request as url_lib

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler


# Upload and download for http(s) paths
class HTTPHandler(IHandler):
    def prepare_upload(self, from_path: str, to_path: str, credentials: Credentials):
        pass

    # Upload data to location
    # input: from, to (assumes this is network) and optional credentials
    # output: Not Implemented Error
    def upload(self, from_path: str, to_path: str, credentials: Credentials):
        raise NotImplementedError("cannot upload to websites")

    # Download data from location
    # input: from (assumes this is network), to and optional credentials
    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: str
    ):
        fn = from_path.split("/")[-1]
        if not os.path.exists(to_path):
            os.makedirs(to_path)
        if credentials:
            password_mgr = url_lib.HTTPPasswordMgrWithDefaultRealm()
            scheme, netloc, _, _, _, _ = parse.urlparse(from_path)
            password_mgr.add_password(
                None,
                scheme + "://" + netloc + "/",
                credentials.username,
                credentials.password,
            )
            handler = url_lib.HTTPBasicAuthHandler(password_mgr)
            opener = url_lib.build_opener(handler)
            url_lib.install_opener(opener)
        f = url_lib.urlopen(from_path)
        with open(os.path.join(to_path, fn), "wb") as local_file:
            local_file.write(f.read())

        f.close()
