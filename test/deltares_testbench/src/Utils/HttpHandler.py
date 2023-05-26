'''
Description: HTTP handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os
import sys

if sys.version_info.major == 2:
    import urlparse as parse
    import urllib2 as url_lib
else:
    import urllib.parse as parse
    import urllib.request as url_lib


# Upload and download for http(s) paths
class HTTPHandler(object):

    def prepare_upload(self, frompath, topath, credentials):
        pass

    # Upload data to location
    # input: from, to (assumes this is network) and optional credentials
    # output: Not Implemented Error
    def upload(self, frompath, topath, credentials):
        raise NotImplementedError("cannot upload to websites")

    # Download data from location
    # input: from (assumes this is network), to and optional credentials
    def download(self, frompath, topath, credentials, version):
        fn = frompath.split('/')[-1]
        if not os.path.exists(topath):
            os.makedirs(topath)
        if credentials:
            password_mgr = url_lib.HTTPPasswordMgrWithDefaultRealm()
            scheme, netloc, _, _, _, _ = parse.urlparse(frompath)
            password_mgr.add_password(None, scheme + "://" + netloc + "/", credentials.getUsername(),
                                      credentials.getPassword())
            handler = url_lib.HTTPBasicAuthHandler(password_mgr)
            opener = url_lib.build_opener(handler)
            url_lib.install_opener(opener)
        f = url_lib.urlopen(frompath)
        with open(os.path.join(topath, fn), "wb") as local_file:
            local_file.write(f.read())

        f.close()
