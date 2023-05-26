'''
Description: Local and Network handler
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os, logging
from src.Utils.Common import unmountNetworkDrive, mountNetworkDrive
from distutils import dir_util
from src.Config.Handler import Handler
from src.Utils.Paths import Paths
from src.Utils.ResolveHandler import ResolveHandler

# Upload and download for local and network paths
class LocalNetHandler(object):

   def prepare_upload (self, frompath, topath, credentials) :
      pass
      
   # Upload data to location
   # input: from, to (assumes this is network) and optional credentials
   def upload(self, fromlocalpath, topath, credentials):
      handler = ResolveHandler().detect(topath, credentials)
      rfp = Paths().rebuildToLocalPath(fromlocalpath)
      if not os.path.exists(rfp):
         raise IOError("cannot upload from non-existent path %s", rfp)
      if handler == Handler.PATH:
         rtp = Paths().rebuildToLocalPath(topath)
         logging.debug("copying locally from %s to %s", os.path.abspath(rfp), os.path.abspath(rtp))
         dir_util.copy_tree(os.path.abspath(rfp), os.path.abspath(rtp))
      if handler == Handler.NET:
         server, folder, rest = Paths().splitNetworkPath(topath)
         mp, nm = mountNetworkDrive(server, folder, credentials)
         logging.debug("mounted share to %s", mp)
         e = None
         try:
            netpath = os.path.join(mp + os.sep, rest)
            logging.debug("copying to net from %s to %s", os.path.abspath(rfp), os.path.abspath(netpath))
            dir_util.copy_tree(os.path.abspath(rfp), netpath)
         except Exception as e:
            logging.error("exception during network transfer %s", str(e).replace("'", ""))
         finally:
            if nm:
               unmountNetworkDrive(mp)
               logging.debug("unmounted share")
               if e:
                  raise e
         
   # Download data from location
   # input: from, to and optional credentials
   def download(self, frompath, tolocalpath, credentials, version):
      handler = ResolveHandler().detect(frompath, credentials)
      rtp = Paths().rebuildToLocalPath(tolocalpath)
      if handler == Handler.PATH:
         rfp = Paths().rebuildToLocalPath(frompath)
         logging.debug("copying locally from %s to %s", os.path.abspath(rfp), os.path.abspath(rtp))
         dir_util.copy_tree(os.path.abspath(rfp), os.path.abspath(rtp))
      if handler == Handler.NET:
         server, folder, rest = Paths().splitNetworkPath(frompath)
         mp, nm = mountNetworkDrive(server, folder, credentials)
         logging.debug("mounted share to %s", mp)
         e = None
         try:
            netpath = os.path.join(mp + os.sep, rest)
            logging.debug("copying from net from %s to %s", os.path.abspath(netpath), os.path.abspath(rtp))
            dir_util.copy_tree(netpath, os.path.abspath(rtp))
         except Exception as e:
            logging.error("exception during network transfer %s", str(e).replace("'", ""))
         finally:
            if nm:
               unmountNetworkDrive(mp)
               logging.debug("unmounted share")
            if e:
               raise e
